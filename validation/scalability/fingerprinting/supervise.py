"""Bounded fresh-process fingerprint probe with an independent streaming SHA oracle."""
import argparse
import datetime
import fcntl
import hashlib
import json
import os
from pathlib import Path
import resource
import shutil
import signal
import struct
import subprocess
import time


def verify_bytes(path, measurement):
    original = hashlib.sha256()
    changed = hashlib.sha256()
    with path.open("rb") as stream:
        header = stream.read(14)
        assert header[:2] == b"X\n" and struct.unpack(">i", header[2:6])[0] == 2
        remaining = path.stat().st_size - 14
        while remaining > 8:
            block = stream.read(min(1024 * 1024, remaining - 8))
            assert block
            original.update(block)
            changed.update(block)
            remaining -= len(block)
        tail = stream.read(8)
        assert tail == struct.pack(">d", 0.125) and stream.read(1) == b""
        original.update(tail)
        changed.update(struct.pack(">d", 0.25))
    expected = "axr-" + original.hexdigest()
    expected_changed = "axr-" + changed.hexdigest()
    assert expected == measurement["fingerprint"]
    assert expected_changed == measurement["changed_final_value_fingerprint"]
    return dict(sha256=expected, changed_final_value_sha256=expected_changed,
                bytes=path.stat().st_size, exact_original=True, exact_final_value_change=True,
                serializer="base::serialize(connection, ascii=FALSE, xdr=TRUE, version=2)",
                hash="Python hashlib.sha256 streaming all bytes after the 14-byte binary header")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--source", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--variant", choices=("before", "after"), required=True)
    parser.add_argument("--vector-mib", type=int, default=64)
    parser.add_argument("--memory-mib", type=int, default=512)
    parser.add_argument("--seconds", type=int, default=120)
    parser.add_argument("--cache", type=Path, default=Path.home() / ".cache/autoxplain-scale-0.7.0")
    args = parser.parse_args()
    assert min(args.vector_mib, args.memory_mib, args.seconds) > 0
    args.output.mkdir(parents=True, exist_ok=False)
    frozen = args.output / "source"
    frozen.mkdir()
    implementation = frozen / "evidence_contract.R"
    script = frozen / "probe.R"
    shutil.copy2(args.source, implementation)
    shutil.copy2(Path(__file__).with_name("probe.R"), script)
    command = ["/usr/bin/time", "-v", "-o", str(args.output.resolve() / "time.txt"),
               "Rscript", "--vanilla", str(script.resolve()), str(implementation.resolve()),
               args.variant, str(args.output.resolve()), str(args.vector_mib)]
    temporary = args.cache / "tmp"
    temporary.mkdir(parents=True, exist_ok=True)
    environment = dict(os.environ, TMPDIR=str(temporary.resolve()), OMP_NUM_THREADS="1",
                       OPENBLAS_NUM_THREADS="1", MKL_NUM_THREADS="1", VECLIB_MAXIMUM_THREADS="1")

    def bound_memory():
        limit = args.memory_mib * 1024**2
        resource.setrlimit(resource.RLIMIT_AS, (limit, limit))

    with (args.cache / "measurement.lock").open("w") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        began = datetime.datetime.now(datetime.timezone.utc).isoformat()
        start = time.monotonic()
        timed_out = False
        with (args.output / "run.log").open("w") as log:
            process = subprocess.Popen(command, stdout=log, stderr=subprocess.STDOUT, env=environment,
                                       start_new_session=True, preexec_fn=bound_memory)
            try:
                returncode = process.wait(timeout=args.seconds)
            except subprocess.TimeoutExpired:
                timed_out = True
                os.killpg(process.pid, signal.SIGTERM)
                try:
                    returncode = process.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    os.killpg(process.pid, signal.SIGKILL)
                    returncode = process.wait()
        elapsed = time.monotonic() - start
        time_path = args.output / "time.txt"
        time_text = time_path.read_text() if time_path.exists() else ""
        peaks = [line for line in time_text.splitlines() if "Maximum resident set size" in line]
        oracle = None
        oracle_error = None
        oracle_path = args.output / "oracle-v2.bin"
        try:
            if returncode == 0 and oracle_path.exists():
                measurement = json.loads((args.output / "measurement.json").read_text())
                oracle = verify_bytes(oracle_path, measurement)
        except (AssertionError, OSError, ValueError, KeyError) as error:
            oracle_error = f"{type(error).__name__}: {error}"
        finally:
            if oracle_path.exists():
                oracle_path.unlink()
        verdict = dict(began_at_utc=began, wall_seconds=elapsed, returncode=returncode,
                       timed_out=timed_out, time_limit_seconds=args.seconds,
                       address_space_limit_mib=args.memory_mib,
                       peak_rss_kib=int(peaks[-1].split(":")[-1]) if peaks else None,
                       source_sha256=hashlib.sha256(implementation.read_bytes()).hexdigest(),
                       runner_sha256=hashlib.sha256(script.read_bytes()).hexdigest(),
                       supervisor_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
                       independent_oracle=oracle, independent_oracle_error=oracle_error, command=command)
        (args.output / "supervisor.json").write_text(json.dumps(verdict, indent=2) + "\n")
        print(json.dumps(verdict, indent=2))
        return 124 if timed_out else (1 if oracle_error else returncode)


if __name__ == "__main__":
    raise SystemExit(main())
