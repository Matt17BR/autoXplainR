#!/usr/bin/env python3
"""Publication-only tests; synthetic process lines plus the existing public JSON."""
from copy import deepcopy
from pathlib import Path
import json
import os
import stat
import subprocess
import sys
import tempfile
import unittest

import public_results_sanitizer as sanitizer

SENTINEL = "UNRELATED_BROWSER_SECRET_SENTINEL"


def fixture():
    return {"scope": "synthetic publication fixture", "runs": [{
        "case": "covertype", "variant": "ranger", "cohort": "synthetic",
        "process_elapsed_seconds": 374.829887, "peak_rss_kib": 8632284,
        "source_sha256": "a" * 64, "calibration": {"loss": 0.1357952468},
        "shared_host_work": {"declared_threads_each": 4, "address_space_limit_each_bytes": 24 * 1024**3,
            "diagnostic_observed_process_record": {"threads": 4, "freeze_sha256": "b" * 64,
                sanitizer.RAW_FIELD: [" PID PPID COMMAND COMMAND", " 10 1 R /usr/lib/R/bin/exec/R --file=/bench/run.R",
                    " 11 1 chrome chrome --unrelated=" + SENTINEL]}}}]}


def encoded(value):
    return (json.dumps(value, indent=2) + "\n").encode()


def snapshot(document):
    return document["runs"][0]["shared_host_work"]["diagnostic_observed_process_record"]


def without_publication_changes(document):
    result = deepcopy(document)
    result.pop("publication_sanitization", None)
    for run in result["runs"]:
        observed = run.get("shared_host_work", {}).get("diagnostic_observed_process_record", {})
        observed.pop(sanitizer.RAW_FIELD, None)
        observed.pop(sanitizer.SUMMARY_FIELD, None)
    return result


def collector_fixture(base):
    """Synthetic metadata only: never point the collector at the real cache."""
    cache = base / "synthetic-cache"
    run = cache / "runs" / "synthetic" / "covertype" / "ranger"; run.mkdir(parents=True)
    process = {"case": "covertype", "variant": "ranger", "phase": "acceptance", "cohort": "synthetic",
        "threads": 4, "stage": "fit-only", "wall_limit_seconds": 7200,
        "address_space_limit_bytes": 24 * 1024**3, "started_at": "synthetic", "scripts": {},
        "partitions_sha256": "c" * 64, "process_status": "ok", "process_elapsed_seconds": 3.125,
        "peak_rss_kib": 123456, "exit_code": 0}
    (run / "process.json").write_bytes(encoded(process))
    (run / "shared-host-work.json").write_bytes(encoded(fixture()["runs"][0]["shared_host_work"]))
    (cache / "raw").mkdir(); (cache / "raw" / "sources.json").write_text("{}\n")
    (cache / "partitions.json").write_text('{"scope":"synthetic metadata only"}\n')
    return cache


def run_private_collector(source, staging, cache):
    staging.mkdir(mode=0o700)
    collector = staging / "collect.py"; collector.write_text(source)
    (staging / "public_results_sanitizer.py").write_bytes(Path(sanitizer.__file__).read_bytes())
    subprocess.run([sys.executable, "-B", str(collector)], cwd=staging,
        env=dict(os.environ, AXR_TABULAR_DIR=str(cache)), check=True, capture_output=True, timeout=15)
    return (staging / "native-training-results.json").read_bytes()


class PublicResultsSanitizerTests(unittest.TestCase):
    def test_no_unrelated_arguments_and_correct_counts(self):
        data = encoded(fixture())
        output, receipt = sanitizer.sanitize_native_training(data, "private-original.json")
        self.assertNotIn(SENTINEL.encode(), output)
        summary = snapshot(json.loads(output))[sanitizer.SUMMARY_FIELD]
        self.assertEqual((summary["process_count"], summary["r_process_count"], summary["header_lines_excluded"]), (2, 1, 1))
        self.assertEqual(receipt["original_sha256"], sanitizer.sha256(data))
        self.assertEqual(summary["original_text_sha256"], sanitizer.sha256(("\n".join(snapshot(fixture())[sanitizer.RAW_FIELD]) + "\n").encode()))

    def test_all_other_numbers_hashes_controls_and_evidence_preserved(self):
        original = fixture()
        output, _ = sanitizer.sanitize_native_training(encoded(original), "private-original.json")
        self.assertEqual(without_publication_changes(original), without_publication_changes(json.loads(output)))

    def test_malformed_snapshots_rejected(self):
        valid = snapshot(fixture())[sanitizer.RAW_FIELD]
        for invalid in [None, [], {}, ["header"], ["PID PPID COMMAND ARGS", valid[1]],
                        [valid[0], "not a process"], [valid[0], valid[1], valid[1]],
                        [valid[0], 10], [valid[0], valid[1] + "\nsecond line"]]:
            with self.subTest(invalid_type=type(invalid).__name__):
                original = fixture(); snapshot(original)[sanitizer.RAW_FIELD] = invalid
                with self.assertRaises(ValueError):
                    sanitizer.sanitize_native_training(encoded(original), "private-original.json")

    def test_unexpected_snapshot_location_rejected(self):
        original = fixture(); original["extra"] = {sanitizer.RAW_FIELD: snapshot(original)[sanitizer.RAW_FIELD]}
        with self.assertRaises(ValueError):
            sanitizer.sanitize_native_training(encoded(original), "private-original.json")

    def test_renamed_raw_header_rejected(self):
        with self.assertRaises(ValueError):
            sanitizer.assert_public_safe({"renamed": ["PID PPID COMMAND COMMAND", "1 0 chrome " + SENTINEL]})

    def test_second_pass_is_byte_identical(self):
        first, _ = sanitizer.sanitize_native_training(encoded(fixture()), "private-original.json")
        second, receipt = sanitizer.sanitize_native_training(first, "unused-second-name.json")
        self.assertEqual(first, second)
        self.assertFalse(receipt["changed"])

    def test_private_archive_preserves_exact_bytes_and_permissions(self):
        with tempfile.TemporaryDirectory() as temporary:
            data = encoded(fixture()); path = sanitizer.archive_original(data, temporary)
            self.assertEqual(path.read_bytes(), data)
            self.assertEqual(stat.S_IMODE(path.parent.stat().st_mode), 0o700)
            self.assertEqual(stat.S_IMODE(path.stat().st_mode), 0o600)
            self.assertEqual(sanitizer.archive_original(data, temporary), path)

    def test_existing_archive_permissions_restored(self):
        with tempfile.TemporaryDirectory() as temporary:
            data = encoded(fixture()); path = sanitizer.archive_original(data, temporary)
            path.chmod(0o644)
            self.assertEqual(sanitizer.archive_original(data, temporary).read_bytes(), data)
            self.assertEqual(stat.S_IMODE(path.stat().st_mode), 0o600)

    def test_symlink_archive_file_and_directory_rejected(self):
        with tempfile.TemporaryDirectory() as temporary:
            cache = Path(temporary) / "cache"; data = encoded(fixture())
            path = sanitizer.archive_original(data, cache); path.unlink()
            target = Path(temporary) / "original.json"; target.write_bytes(data); path.symlink_to(target)
            with self.assertRaises(ValueError):
                sanitizer.archive_original(data, cache)
            self.assertEqual(target.read_bytes(), data)
        with tempfile.TemporaryDirectory() as temporary:
            cache = Path(temporary) / "cache"; cache.mkdir()
            outside = Path(temporary) / "outside"; outside.mkdir()
            (cache / "publication-originals").symlink_to(outside, target_is_directory=True)
            with self.assertRaises(ValueError):
                sanitizer.archive_original(encoded(fixture()), cache)
            self.assertEqual(list(outside.iterdir()), [])

    def test_cli_malformed_input_retains_original_bytes_and_hash(self):
        with tempfile.TemporaryDirectory() as temporary:
            source = Path(temporary) / "public.json"; original = fixture()
            snapshot(original)[sanitizer.RAW_FIELD] = ["wrong header"]
            data = encoded(original); source.write_bytes(data)
            result = subprocess.run([sys.executable, "-B", str(Path(sanitizer.__file__)),
                "--input", str(source), "--cache", str(Path(temporary) / "cache"), "--in-place"],
                capture_output=True, timeout=10)
            self.assertNotEqual(result.returncode, 0)
            self.assertEqual(source.read_bytes(), data)
            self.assertEqual(sanitizer.sha256(source.read_bytes()), sanitizer.sha256(data))
            self.assertFalse((Path(temporary) / "cache").exists())

    def test_collector_entry_point_publishes_only_sanitized_data(self):
        with tempfile.TemporaryDirectory() as temporary:
            destination = Path(temporary) / "public.json"
            receipt = sanitizer.publish_native_training(fixture(), destination, Path(temporary) / "cache")
            self.assertTrue(receipt["changed"])
            self.assertNotIn(SENTINEL.encode(), destination.read_bytes())
            self.assertIn(SENTINEL.encode(), Path(receipt["private_original"]).read_bytes())
            self.assertEqual(without_publication_changes(fixture()), without_publication_changes(json.loads(destination.read_bytes())))

    def test_malformed_input_does_not_replace_existing_public_file(self):
        with tempfile.TemporaryDirectory() as temporary:
            destination = Path(temporary) / "public.json"; destination.write_bytes(b"previous public bytes")
            original = fixture(); snapshot(original)[sanitizer.RAW_FIELD] = ["bad header"]
            with self.assertRaises(ValueError):
                sanitizer.publish_native_training(original, destination, Path(temporary) / "cache")
            self.assertEqual(destination.read_bytes(), b"previous public bytes")

    def test_real_collector_rejects_unsafe_publication_mutation(self):
        directory = Path(__file__).resolve().parent
        source = (directory / "collect.py").read_text()
        import_line = "from public_results_sanitizer import publish_native_training\n"
        old_start = '(destination / "native-training-results.json").write_text(json.dumps({'
        new_start = 'publish_native_training({'
        old_end = '"runs": native_training}, indent=2) + "\\n")'
        new_end = '"runs": native_training}, destination / "native-training-results.json", cache)'
        self.assertIn(import_line, source); self.assertIn(new_start, source); self.assertIn(new_end, source)
        patched = source
        original = source.replace(import_line, "", 1).replace(new_start, old_start, 1).replace(new_end, old_end, 1)
        self.assertNotEqual(original, patched)
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary); cache = collector_fixture(base)
            outputs = {}
            for name, code in [("original", original), ("patched", patched)]:
                outputs[name] = run_private_collector(code, base / name, cache)
            self.assertIn(SENTINEL.encode(), outputs["original"], "Negative control must expose the unsafe original publication.")
            self.assertNotIn(SENTINEL.encode(), outputs["patched"])
            before, after = json.loads(outputs["original"]), json.loads(outputs["patched"])
            self.assertEqual(without_publication_changes(before), without_publication_changes(after))
            private = cache / after["publication_sanitization"]["private_original_cache_relative_path"]
            self.assertEqual(private.read_bytes(), outputs["original"])
            self.assertEqual(after["publication_sanitization"]["original_document_sha256"], sanitizer.sha256(outputs["original"]))
            for filename in ["development-results.json", "sources.json", "partitions.json", "DEVELOPMENT.md"]:
                self.assertEqual((base / "original" / filename).read_bytes(), (base / "patched" / filename).read_bytes())

    def test_current_public_json_preserves_evidence_and_counts(self):
        original = (Path(__file__).resolve().parent / "native-training-results.json").read_bytes()
        output, _ = sanitizer.sanitize_native_training(original, "test-only-not-published.json")
        before, after = json.loads(original), json.loads(output)
        self.assertEqual(without_publication_changes(before), without_publication_changes(after))
        affected = [r for r in after["runs"] if sanitizer.SUMMARY_FIELD in r.get("shared_host_work", {}).get("diagnostic_observed_process_record", {})]
        self.assertEqual(len(affected), 1)
        run = affected[0]
        self.assertEqual((run["case"], run["variant"], run["cohort"]), ("covertype", "ranger", "native-fixed500-full-fit-4t-v1"))
        summary = run["shared_host_work"]["diagnostic_observed_process_record"][sanitizer.SUMMARY_FIELD]
        self.assertEqual((summary["process_count"], summary["r_process_count"]), (594, 1))
        sanitizer.assert_public_safe(after)


class ActualCollectorIntegrationGate(unittest.TestCase):
    def test_unmodified_actual_collector_never_publishes_snapshot(self):
        """Exercise unmodified repository source in an isolated synthetic cache."""
        source = (Path(__file__).resolve().parent / "collect.py").read_text()
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary); cache = collector_fixture(base)
            output = run_private_collector(source, base / "actual", cache)
            self.assertNotIn(SENTINEL.encode(), output,
                "The actual collector publishes unrelated process arguments.")
            document = json.loads(output)
            sanitizer.assert_public_safe(document)
            counts = snapshot(document)[sanitizer.SUMMARY_FIELD]
            self.assertEqual((counts["process_count"], counts["r_process_count"]), (2, 1))


if __name__ == "__main__":
    # Keep sanitizer/mutation checks distinct from the actual-source CI gate.
    if len(sys.argv) > 1 and sys.argv[1:] != ["--integration-gate"]:
        raise SystemExit("Usage: test-public-results-sanitizer.py [--integration-gate]")
    case = ActualCollectorIntegrationGate if len(sys.argv) > 1 else PublicResultsSanitizerTests
    result = unittest.TextTestRunner().run(unittest.defaultTestLoader.loadTestsFromTestCase(case))
    raise SystemExit(0 if result.wasSuccessful() else 1)
