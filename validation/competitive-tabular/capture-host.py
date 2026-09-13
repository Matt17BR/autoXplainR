#!/usr/bin/env python3
"""Snapshot visible host allocation and benchmark thread controls, without data."""
import argparse
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import platform
import re
import subprocess

parser = argparse.ArgumentParser()
parser.add_argument("--output", required=True, type=Path)
parser.add_argument("runs", nargs="+", type=Path)
args = parser.parse_args()
output = args.output.expanduser().resolve()
if output.exists():
    raise SystemExit("Refusing to replace a host observation.")
if any((run.expanduser().resolve() / "host-resources-link.json").exists() for run in args.runs):
    raise SystemExit("A run already has a host observation; preserve it and use another observation path.")
record = {
    "observed_at": datetime.now(timezone.utc).isoformat(),
    "platform": platform.platform(), "logical_cpu_count": os.cpu_count(),
    "supervisor_affinity_cpus": sorted(os.sched_getaffinity(0)),
    "lscpu": json.loads(subprocess.check_output(["lscpu", "--json"], text=True)),
    "meminfo": Path("/proc/meminfo").read_text(),
    "self_cgroup": Path("/proc/self/cgroup").read_text(),
    "cgroup_ancestry": {}, "runs": {},
    "limitations": "Current visible allocation only. Configured native backend threads are recorded separately from total operating-system threads. Concurrent host workloads affect timing; this observation does not establish historic host equivalence."
}
relative = record["self_cgroup"].strip().split("::", 1)[1]
cgroup = Path("/sys/fs/cgroup") / relative.lstrip("/")
for path in [cgroup, *cgroup.parents]:
    if not str(path).startswith("/sys/fs/cgroup"):
        break
    values = {}
    for name in ["cpu.max", "cpu.stat", "cpu.weight", "cpuset.cpus.effective",
                 "memory.max", "memory.high", "memory.current", "memory.peak",
                 "memory.swap.max", "memory.events", "pids.max"]:
        file = path / name
        if file.exists():
            values[name] = file.read_text().strip()
    record["cgroup_ancestry"][str(path)] = values
for directory in args.runs:
    run = directory.expanduser().resolve()
    process = json.loads((run / "process.json").read_text())
    log = (run / "process.log").read_text()
    entry = {"supervised_pid": process["supervised_pid"],
             "declared_native_threads": process["threads"],
             "address_space_limit_bytes": process["address_space_limit_bytes"],
             "adapter_trace_thread_counts": sorted(set(map(int, re.findall(r'"threads":(\d+)', log)))),
             "actual_processes": []}
    started = run / "started.json"
    if started.exists():
        entry["started_native_threads"] = json.loads(started.read_text()).get("native_threads")
    pending = [process["supervised_pid"]]
    while pending:
        pid = pending.pop()
        proc = Path("/proc") / str(pid)
        try:
            pending.extend(map(int, (proc / f"task/{pid}/children").read_text().split()))
            status = (proc / "status").read_text()
            command = (proc / "cmdline").read_bytes().replace(b"\0", b" ").decode()
            environment = (proc / "environ").read_bytes().split(b"\0")
            limits = (proc / "limits").read_text()
        except (FileNotFoundError, ProcessLookupError):
            continue
        entry["actual_processes"].append({"pid": pid, "command": command,
            "resource_status": [line for line in status.splitlines() if line.split(":")[0] in
                {"Name", "Threads", "Cpus_allowed_list", "Mems_allowed_list", "VmPeak", "VmSize", "VmHWM", "VmRSS"}],
            "address_space_limit": [line for line in limits.splitlines() if line.startswith("Max address space")],
            "backend_thread_environment": [item.decode() for item in environment
                if item.startswith((b"OMP_NUM_THREADS=", b"OPENBLAS_NUM_THREADS=", b"MKL_NUM_THREADS="))]})
    record["runs"][str(run)] = entry
output.parent.mkdir(parents=True, exist_ok=True)
output.write_text(json.dumps(record, indent=2) + "\n")
link = {"path": str(output), "sha256": hashlib.sha256(output.read_bytes()).hexdigest()}
for directory in args.runs:
    target = directory.expanduser().resolve() / "host-resources-link.json"
    target.write_text(json.dumps(link, indent=2) + "\n")
print(json.dumps(link, indent=2))
