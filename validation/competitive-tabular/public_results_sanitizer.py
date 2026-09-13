"""Remove host-wide process arguments from public benchmark summaries.

Raw diagnostic records remain in the private cache. Counts describe a single
process-list observation; they are not measurements of CPU utilization or load.
"""
from copy import deepcopy
from pathlib import Path
import argparse
import hashlib
import json
import os
import re
import tempfile

RAW_FIELD = "shared_host_processes_before"
SUMMARY_FIELD = "shared_host_process_snapshot"
HEADER = ["PID", "PPID", "COMMAND", "COMMAND"]


def sha256(data):
    return hashlib.sha256(data).hexdigest()


def process_counts(lines):
    if not isinstance(lines, list) or len(lines) < 2 or not all(isinstance(line, str) for line in lines):
        raise ValueError("A host process snapshot must be a header and process lines.")
    if any("\n" in line or "\r" in line for line in lines) or lines[0].split() != HEADER:
        raise ValueError("Unexpected host process snapshot header or embedded newline.")
    processes = []
    for line in lines[1:]:
        match = re.fullmatch(r"\s*(\d+)\s+(\d+)\s+(\S+)\s+(.+)", line)
        if not match or int(match[1]) < 1:
            raise ValueError("Malformed host process snapshot row.")
        processes.append((int(match[1]), match[3]))
    if len({pid for pid, _ in processes}) != len(processes):
        raise ValueError("Duplicate process ID in host snapshot.")
    return {
        "process_count": len(processes),
        "r_process_count": sum(command == "R" for _, command in processes),
        "header_lines_excluded": 1,
        "original_text_sha256": sha256(("\n".join(lines) + "\n").encode("utf-8")),
        "digest_encoding": "UTF-8 lines joined with LF and one trailing LF",
        "scope": "Processes present before this diagnostic launched; R means comm exactly R. No process arguments are published. These counts do not measure CPU use, runnable load, or sustained concurrency.",
    }


def assert_public_safe(value):
    """Fail closed for misplaced snapshots, including a renamed raw text list."""
    if isinstance(value, dict):
        if RAW_FIELD in value:
            raise ValueError("Unrecognized raw host snapshot location; publication refused.")
        for item in value.values():
            assert_public_safe(item)
    elif isinstance(value, list):
        for item in value:
            assert_public_safe(item)
    elif isinstance(value, str) and any(line.split() == HEADER for line in value.splitlines()):
        raise ValueError("Raw host process-list header remains; publication refused.")


def sanitize_native_training(data, private_archive_name):
    document = json.loads(data)
    if not isinstance(document, dict) or not isinstance(document.get("runs"), list):
        raise ValueError("Expected a native-training result object with runs.")
    public = deepcopy(document)
    affected = []
    for run in public["runs"]:
        if not isinstance(run, dict):
            raise ValueError("Invalid native-training run.")
        shared = run.get("shared_host_work", {})
        observed = shared.get("diagnostic_observed_process_record", {})
        if RAW_FIELD not in observed:
            continue
        if SUMMARY_FIELD in observed:
            raise ValueError("Conflicting raw and public process snapshots.")
        counts = process_counts(observed.pop(RAW_FIELD))
        observed[SUMMARY_FIELD] = counts
        affected.append({key: run[key] for key in ("case", "variant", "cohort")})
    assert_public_safe(public)
    if not affected:
        return data, {"changed": False, "original_sha256": sha256(data), "affected_runs": []}
    if "publication_sanitization" in public:
        raise ValueError("Mixed original and already sanitized publication metadata.")
    public["publication_sanitization"] = {
        "schema_version": 1,
        "original_document_sha256": sha256(data),
        "private_original_cache_relative_path": private_archive_name,
        "affected_runs": affected,
        "scope": "Publication-only removal of host-wide process arguments. Original bytes and raw diagnostics remain in the private cache; all other numerical evidence and benchmark-specific controls are retained.",
    }
    output = (json.dumps(public, indent=2, allow_nan=False) + "\n").encode("utf-8")
    return output, {"changed": True, "original_sha256": sha256(data), "public_sha256": sha256(output), "affected_runs": affected}


def archive_original(data, cache):
    cache = Path(cache)
    cache.mkdir(parents=True, exist_ok=True)
    directory = cache / "publication-originals" / "native-training-results"
    for private_directory in (directory.parent, directory):
        if private_directory.is_symlink():
            raise ValueError("Private archive directory must not be a symlink.")
        private_directory.mkdir(mode=0o700, exist_ok=True)
        os.chmod(private_directory, 0o700, follow_symlinks=False)
    path = directory / (sha256(data) + ".json")
    try:
        with path.open("xb") as stream:
            os.chmod(path, 0o600)
            stream.write(data)
    except FileExistsError:
        if path.is_symlink() or not path.is_file():
            raise ValueError("Private archive must be a regular file, not a symlink.")
        if path.read_bytes() != data:
            raise ValueError("Private archive hash collision or changed contents.")
        os.chmod(path, 0o600, follow_symlinks=False)
    return path


def atomic_write(path, data):
    path = Path(path)
    with tempfile.NamedTemporaryFile(dir=path.parent, prefix="." + path.name + ".", delete=False) as stream:
        temporary = Path(stream.name)
        stream.write(data)
    try:
        os.replace(temporary, path)
    finally:
        temporary.unlink(missing_ok=True)


def publish_native_training(document, path, cache):
    """Publication entry point for the collector after its frozen run is complete."""
    original = (json.dumps(document, indent=2, allow_nan=False) + "\n").encode("utf-8")
    archive_name = "publication-originals/native-training-results/" + sha256(original) + ".json"
    public, receipt = sanitize_native_training(original, archive_name)
    if receipt["changed"]:
        receipt["private_original"] = str(archive_original(original, cache))
    atomic_write(path, public)
    return receipt


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", type=Path, required=True)
    parser.add_argument("--cache", type=Path, required=True)
    parser.add_argument("--in-place", action="store_true", required=True)
    args = parser.parse_args()
    original = args.input.read_bytes()
    archive_name = "publication-originals/native-training-results/" + sha256(original) + ".json"
    public, receipt = sanitize_native_training(original, archive_name)
    if receipt["changed"]:
        archived = archive_original(original, args.cache)
        if args.input.read_bytes() != original:
            raise ValueError("Input changed before publication; no replacement performed.")
        atomic_write(args.input, public)
        receipt["private_original"] = str(archived)
    print(json.dumps(receipt, indent=2))


if __name__ == "__main__":
    main()
