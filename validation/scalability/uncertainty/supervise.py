"""Run one bounded, fresh-process paired-bootstrap measurement."""
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
import subprocess
import time


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--library", type=Path, required=True)
    parser.add_argument("--reference", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--variant", choices=("before", "after"), required=True)
    parser.add_argument("--problem", choices=("regression", "multiclass_brier"), required=True)
    parser.add_argument("--rows", type=int, choices=(20000, 200000, 1000000), required=True)
    parser.add_argument("--seconds", type=int, default=180)
    parser.add_argument("--memory-gib", type=int, default=12)
    parser.add_argument("--cache", type=Path, default=Path.home() / ".cache/autoxplain-scale-0.7.0")
    args = parser.parse_args()
    assert args.seconds > 0 and args.memory_gib > 0
    expected = "b0d94d406c57781221428c289f668438b4aa2ada702f64986251dec250fa10eb"
    assert hashlib.sha256(args.reference.read_bytes()).hexdigest() == expected
    args.output.mkdir(parents=True, exist_ok=False)
    source = args.output / "source"
    source.mkdir()
    script = source / "benchmark.R"
    reference = source / "performance_uncertainty-before.R"
    shutil.copy2(Path(__file__).with_name("benchmark.R"), script)
    shutil.copy2(args.reference, reference)
    command = ["/usr/bin/time", "-v", "-o", str(args.output.resolve() / "time.txt"),
               "Rscript", "--vanilla", str(script.resolve()), str(args.library.resolve()),
               args.variant, args.problem, str(args.rows), str(args.output.resolve()), str(reference.resolve())]
    temporary = args.cache / "tmp"
    temporary.mkdir(parents=True, exist_ok=True)
    environment = dict(os.environ, TMPDIR=str(temporary.resolve()), OMP_NUM_THREADS="1",
                       OPENBLAS_NUM_THREADS="1", MKL_NUM_THREADS="1", VECLIB_MAXIMUM_THREADS="1")

    def bound_memory():
        limit = args.memory_gib * 1024**3
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
                    returncode = process.wait(timeout=10)
                except subprocess.TimeoutExpired:
                    os.killpg(process.pid, signal.SIGKILL)
                    returncode = process.wait()
        time_file = args.output / "time.txt"
        time_text = time_file.read_text() if time_file.exists() else ""
        peak_lines = [line for line in time_text.splitlines() if "Maximum resident set size" in line]
        peak = int(peak_lines[-1].split(":")[-1]) if peak_lines else None
        verdict = dict(began_at_utc=began, wall_seconds=time.monotonic() - start,
                       returncode=returncode, timed_out=timed_out, time_limit_seconds=args.seconds,
                       address_space_limit_gib=args.memory_gib, peak_rss_kib=peak,
                       command=command, reference_sha256=expected,
                       runner_sha256=hashlib.sha256(script.read_bytes()).hexdigest(),
                       supervisor_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest())
        (args.output / "supervisor.json").write_text(json.dumps(verdict, indent=2) + "\n")
        print(json.dumps(verdict, indent=2))
        return 124 if timed_out else returncode


if __name__ == "__main__":
    raise SystemExit(main())
