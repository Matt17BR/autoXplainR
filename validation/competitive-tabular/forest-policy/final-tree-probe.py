#!/usr/bin/env python3
"""Freeze and supervise individual, development-only final-tree diagnostics."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import resource
import shutil
import signal
import subprocess
import time

parser = argparse.ArgumentParser()
parser.add_argument("case", choices=["yearprediction", "covertype"])
parser.add_argument("stage", choices=["prepare", "fit", "replay", "screen", "detail", "summarize"])
parser.add_argument("--model", choices=["forest500", "forest256", "main_model", "simple_baseline"], default="forest256")
parser.add_argument("--cache", default=str(Path.home() / ".cache/autoxplain-tabular-0.8.0"))
parser.add_argument("--run", default="forest-final256-development-v1")
args = parser.parse_args()
cache = Path(args.cache).resolve()
root = cache / args.run
repository = Path(__file__).resolve().parents[3]


def digest(path):
    result = hashlib.sha256()
    with path.open("rb") as source:
        for block in iter(lambda: source.read(1024 * 1024), b""):
            result.update(block)
    return result.hexdigest()


if not root.exists():
    root.mkdir()
    source = root / "source"
    source.mkdir()
    shutil.copytree(repository / "R", source / "R")
    for name in ["DESCRIPTION", "NAMESPACE"]:
        shutil.copy2(repository / name, source / name)
    scripts = root / "scripts"
    scripts.mkdir()
    for name in ["final-tree-probe.R", "final-tree-probe.py", "final-tree-protocol.md"]:
        shutil.copy2(Path(__file__).parent / name, scripts / name)
    shutil.copy2(repository / "validation/competitive-tabular/common.R", scripts / "common.R")
    frozen = {str(p.relative_to(root)): digest(p) for folder in [source, scripts]
              for p in sorted(folder.rglob("*")) if p.is_file()}
    (root / "freeze.json").write_text(json.dumps({
        "declared_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "scope": "Development-only diagnostic; no acceptance data.", "files": frozen,
        "partitions_sha256": digest(cache / "partitions.json")}, indent=2) + "\n")
freeze = json.loads((root / "freeze.json").read_text())
for name, expected in freeze["files"].items():
    if digest(root / name) != expected:
        raise SystemExit(f"Frozen diagnostic file changed: {name}")
if freeze["partitions_sha256"] != digest(cache / "partitions.json"):
    raise SystemExit("Partition manifest changed.")
stage_name = args.stage + ("-" + args.model if args.stage in ["screen", "detail"] else "")
destination = root / args.case / stage_name
destination.mkdir(parents=True, exist_ok=False)
command = ["/usr/bin/time", "-v", "-o", str(destination / "resource-usage.txt"),
           "Rscript", "--vanilla", str(root / "scripts/final-tree-probe.R"),
           args.case, args.stage, args.model, str(root), str(cache)]
record = {"case": args.case, "stage": args.stage, "model": args.model,
          "phase": "development diagnostic", "process_status": "running",
          "threads": 4, "wall_limit_seconds": 1200,
          "address_space_limit_bytes": 24 * 1024**3, "command": command,
          "started_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
          "freeze_sha256": digest(root / "freeze.json"),
          "shared_host_processes_before": subprocess.check_output(
              ["ps", "-eo", "pid,ppid,comm,args"], text=True).splitlines()}


def save_record():
    (destination / "process.json").write_text(json.dumps(record, indent=2) + "\n")


def bounds():
    resource.setrlimit(resource.RLIMIT_AS, (24 * 1024**3, 24 * 1024**3))


environment = dict(os.environ)
environment.update({"OMP_NUM_THREADS": "4", "OPENBLAS_NUM_THREADS": "4", "MKL_NUM_THREADS": "4"})
save_record()
started = time.monotonic()
with (destination / "process.log").open("w") as log:
    process = subprocess.Popen(command, stdout=log, stderr=subprocess.STDOUT,
                               env=environment, preexec_fn=bounds, start_new_session=True)
    record["supervised_pid"] = process.pid
    save_record()
    try:
        code = process.wait(timeout=1200)
        record["process_status"] = "ok" if code == 0 else "failed"
        record["exit_code"] = code
    except subprocess.TimeoutExpired:
        os.killpg(process.pid, signal.SIGKILL)
        process.wait()
        record["process_status"] = "timeout"
record["process_elapsed_seconds"] = time.monotonic() - started
record["ended_at"] = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
save_record()
print(json.dumps({key: record[key] for key in ["case", "stage", "model", "process_status", "process_elapsed_seconds"]}))
raise SystemExit(0 if record["process_status"] == "ok" else 1)
