"""Bound one fresh R run by wall time and address space; preserve partial stages."""
import argparse
import datetime
import fcntl
import hashlib
import json
import os
from pathlib import Path
import resource
import signal
import shutil
import subprocess
import time


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--library", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--problem", choices=["regression", "rare_binary", "multiclass", "wide"], required=True)
    parser.add_argument("--rows", type=int, required=True)
    parser.add_argument("--mode", choices=["controlled_fit", "controlled_full", "default_fit", "default_full", "quick_fit", "quick_full", "stronger_fit", "stronger_full"], required=True)
    parser.add_argument("--seconds", type=int, default=180)
    parser.add_argument("--memory-gib", type=int, default=12)
    parser.add_argument("--cache", type=Path, default=Path.home() / ".cache/autoxplain-scale-0.7.0")
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=False)
    args.cache.mkdir(parents=True, exist_ok=True)
    temporary = args.cache / "tmp"
    temporary.mkdir(exist_ok=True)
    harness_directory = Path(__file__).resolve().parent
    frozen_source = args.output / "source"
    frozen_source.mkdir()
    for name in ("run-case.R", "fixtures.R"):
        shutil.copy2(harness_directory / name, frozen_source / name)
    script = frozen_source.resolve() / "run-case.R"
    command = ["/usr/bin/time", "-v", "-o", str(args.output / "time.txt"),
               "Rscript", "--vanilla", str(script), str(args.library.resolve()),
               args.problem, str(args.rows), args.mode, str(args.output.resolve())]
    environment = dict(os.environ)
    environment.update(TMPDIR=str(temporary), OMP_NUM_THREADS="1", OPENBLAS_NUM_THREADS="1",
                       MKL_NUM_THREADS="1", VECLIB_MAXIMUM_THREADS="1")
    limit = args.memory_gib * 1024**3

    def bound_memory():
        resource.setrlimit(resource.RLIMIT_AS, (limit, limit))

    # This lock coordinates runs of this harness. Other agents reserve the same
    # machine separately; avoid unrelated benchmarks when reporting timings.
    with (args.cache / "measurement.lock").open("w") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        began = datetime.datetime.now(datetime.timezone.utc).isoformat()
        start = time.monotonic()
        with (args.output / "run.log").open("w") as log:
            process = subprocess.Popen(command, stdout=log, stderr=subprocess.STDOUT,
                                       env=environment, start_new_session=True, preexec_fn=bound_memory)
            timed_out = False
            try:
                status = process.wait(timeout=args.seconds)
            except subprocess.TimeoutExpired:
                timed_out = True
                os.killpg(process.pid, signal.SIGTERM)
                try:
                    status = process.wait(timeout=10)
                except subprocess.TimeoutExpired:
                    os.killpg(process.pid, signal.SIGKILL)
                    status = process.wait()
        events_path = args.output / "stages.jsonl"
        events = [json.loads(line) for line in events_path.read_text().splitlines() if line.strip()] if events_path.exists() else []
        elapsed = time.monotonic() - start
        summary = {
            "began_at_utc": began, "wall_seconds": elapsed, "returncode": status,
            "timed_out": timed_out, "time_limit_seconds": args.seconds,
            "address_space_limit_gib": args.memory_gib, "command": command,
            "last_stage": events[-1] if events else None,
            "highest_recorded_rss_kib": max((item["peak_rss_kib"] for item in events), default=None),
            "supervisor_sha256": hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
        }
        (args.output / "supervisor.json").write_text(json.dumps(summary, indent=2) + "\n")
        print(json.dumps(summary, indent=2))
        return 124 if timed_out else status


if __name__ == "__main__":
    raise SystemExit(main())
