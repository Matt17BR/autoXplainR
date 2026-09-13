"""Exercise the actual source downloader using small offline ZIP fixtures."""
import argparse
import gzip
import hashlib
import io
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
import zipfile

parser = argparse.ArgumentParser(add_help=False)
parser.add_argument("--script", type=Path, default=Path(__file__).with_name("download.py"))
options, unittest_args = parser.parse_known_args()
SCRIPT = options.script.resolve()
SOURCES = {
    "yearprediction": ("https://archive.ics.uci.edu/static/public/203/yearpredictionmsd.zip", "YearPredictionMSD.txt"),
    "covertype": ("https://archive.ics.uci.edu/static/public/31/covertype.zip", "covtype.data.gz"),
    "bank": ("https://archive.ics.uci.edu/static/public/222/bank+marketing.zip", "bank-additional.zip"),
}
CSV = b"x;y\n1;yes\n2;no\n"
DICTIONARY = b"Synthetic fixture. Contains no UCI observations.\n"


def digest(data):
    return hashlib.sha256(data).hexdigest()


def archive_bytes(members):
    output = io.BytesIO()
    with zipfile.ZipFile(output, "w", compression=zipfile.ZIP_DEFLATED) as archive:
        for name, data in members.items():
            archive.writestr(name, data)
    return output.getvalue()


class DownloaderTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.base = Path(self.temporary.name)
        self.cache = self.base / "synthetic-cache"
        self.raw = self.cache / "raw"
        self.raw.mkdir(parents=True)
        self.guard = self.base / "network-guard"
        self.guard.mkdir()
        (self.guard / "sitecustomize.py").write_text(
            "import urllib.request\n"
            "def no_network(*args, **kwargs):\n"
            "    raise RuntimeError('Network access is forbidden in this fixture')\n"
            "urllib.request.urlopen = no_network\n"
        )
        self.members = {
            "yearprediction": b"2000,1,2\n1999,2,3\n",
            "covertype": gzip.compress(b"1,2,3\n2,3,4\n", mtime=0),
            "bank": archive_bytes({
                "bank-additional/bank-additional-full.csv": CSV,
                "bank-additional/bank-additional-names.txt": DICTIONARY,
            }),
        }
        for name, (_, member) in SOURCES.items():
            (self.raw / (name + ".zip")).write_bytes(archive_bytes({member: self.members[name]}))

    def run_downloader(self):
        environment = dict(os.environ, AXR_TABULAR_DIR=str(self.cache), PYTHONPATH=str(self.guard))
        return subprocess.run(
            [sys.executable, "-B", "-O", str(SCRIPT)], cwd=self.base,
            env=environment, text=True, capture_output=True, timeout=20,
        )

    def require_success(self):
        result = self.run_downloader()
        self.assertEqual(result.returncode, 0, result.stderr)
        return json.loads((self.raw / "sources.json").read_text())

    def write_legacy_cache(self):
        manifest = {}
        for name, (url, member) in SOURCES.items():
            data = self.members[name]
            (self.raw / member).write_bytes(data)
            archive = (self.raw / (name + ".zip")).read_bytes()
            manifest[name] = {
                "url": url, "archive_sha256": digest(archive), "archive_bytes": len(archive),
                "member": member, "member_sha256": digest(data), "member_bytes": len(data),
                "license": "CC BY 4.0",
            }
        (self.raw / "bank-additional-full.csv").write_bytes(CSV)
        (self.raw / "bank-additional-names.txt").write_bytes(DICTIONARY)
        (self.raw / "sources.json").write_text(json.dumps(manifest, indent=2) + "\n")

    def files(self):
        return {str(path.relative_to(self.cache)): path.read_bytes()
                for path in self.cache.rglob("*") if path.is_file()}

    def test_new_extraction_records_exact_data_and_dictionary(self):
        manifest = self.require_success()
        expected = {
            "bank-additional-full.csv": {
                "member": "bank-additional/bank-additional-full.csv", "sha256": digest(CSV), "bytes": len(CSV)},
            "bank-additional-names.txt": {
                "member": "bank-additional/bank-additional-names.txt", "sha256": digest(DICTIONARY), "bytes": len(DICTIONARY)},
        }
        self.assertEqual(manifest["bank"]["nested_members"], expected)
        self.assertEqual((self.raw / "bank-additional-full.csv").read_bytes(), CSV)
        self.assertEqual((self.raw / "bank-additional-names.txt").read_bytes(), DICTIONARY)
        self.assertFalse(list(self.raw.glob("*.partial")))
        for name, (_, member) in SOURCES.items():
            self.assertEqual(manifest[name]["member_sha256"], digest(self.members[name]))
            self.assertEqual((self.raw / member).read_bytes(), self.members[name])

    def test_verified_reuse_preserves_files_and_metadata(self):
        self.require_success()
        before = self.files()
        files = [self.raw / name for name in ("bank-additional-full.csv", "bank-additional-names.txt")]
        timestamps = [path.stat().st_mtime_ns for path in files]
        self.require_success()
        self.assertEqual(self.files(), before)
        self.assertEqual([path.stat().st_mtime_ns for path in files], timestamps)

    def test_same_size_changed_csv_is_rejected(self):
        self.write_legacy_cache()
        changed = CSV.replace(b"yes", b"bad")
        self.assertEqual(len(CSV), len(changed))
        (self.raw / "bank-additional-full.csv").write_bytes(changed)
        before = self.files()
        result = self.run_downloader()
        self.assertNotEqual(result.returncode, 0, "The downloader accepted or silently replaced changed CSV bytes.")
        self.assertIn("Cached extracted source changed: bank/bank-additional-full.csv", result.stderr)
        self.assertEqual(self.files(), before)

    def test_changed_dictionary_is_rejected(self):
        self.write_legacy_cache()
        (self.raw / "bank-additional-names.txt").write_bytes(DICTIONARY.replace(b"UCI", b"XYZ"))
        before = self.files()
        result = self.run_downloader()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Cached extracted source changed: bank/bank-additional-names.txt", result.stderr)
        self.assertEqual(self.files(), before)

    def test_valid_legacy_cache_gets_verified_nested_metadata(self):
        self.write_legacy_cache()
        before = self.files()
        manifest = self.require_success()
        self.assertEqual(manifest["bank"]["nested_members"]["bank-additional-full.csv"]["sha256"], digest(CSV))
        after = self.files()
        del before["raw/sources.json"], after["raw/sources.json"]
        self.assertEqual(after, before)

    def test_changed_outer_archive_is_still_rejected(self):
        self.write_legacy_cache()
        archive = self.raw / "bank.zip"
        archive.write_bytes(archive.read_bytes() + b"changed")
        before = self.files()
        result = self.run_downloader()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Cached archive changed: bank", result.stderr)
        self.assertEqual(self.files(), before)

    def test_prepared_cache_is_refused_before_any_source_write(self):
        self.write_legacy_cache()
        (self.cache / "partitions.json").write_text('{"scope":"synthetic prepared marker"}\n')
        before = self.files()
        result = self.run_downloader()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Prepared partitions already exist", result.stderr)
        self.assertIn("new AXR_TABULAR_DIR", result.stderr)
        self.assertEqual(self.files(), before)

    def test_network_is_forbidden_in_fixture(self):
        (self.raw / "yearprediction.zip").unlink()
        result = self.run_downloader()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Network access is forbidden in this fixture", result.stderr)


if __name__ == "__main__":
    unittest.main(argv=[sys.argv[0], *unittest_args])
