#!/usr/bin/env python3
"""Record a verified missing benchmark process without rewriting its evidence."""
import argparse
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import shutil

parser = argparse.ArgumentParser()
parser.add_argument("run", type=Path)
args = parser.parse_args()
run = args.run.expanduser().resolve()
record = json.loads((run / "process.json").read_text())
if record.get("process_status") != "running":
    raise SystemExit("Only an unfinished recorded process can be marked interrupted.")
pid = record.get("supervised_pid")
if not isinstance(pid, int) or Path(f"/proc/{pid}").exists():
    raise SystemExit("Missing-process evidence is required; the recorded PID still exists or is absent from the record.")
matches = []
for process in Path("/proc").glob("[0-9]*"):
    try:
        command = (process / "cmdline").read_bytes().split(b"\0")
        comm = (process / "comm").read_text().strip()
    except (FileNotFoundError, ProcessLookupError, PermissionError):
        continue
    decoded = [part.decode(errors="replace") for part in command if part]
    if (comm in {"R", "Rscript", "time"}
            or (comm.startswith("python") and any(Path(part).name == "run.py" for part in decoded))):
        matches.append({"pid": int(process.name), "comm": comm, "command": decoded})
if matches:
    raise SystemExit("Native or benchmark processes still exist; inspect them before recording interruption: " + json.dumps(matches))
if any((run / name).exists() for name in ("summary.json", "model.rds", "predictions.rds")):
    raise SystemExit("Completion artifacts exist; inspect them manually before recording interruption.")
output = run / "interruption.json"
if output.exists():
    raise SystemExit("Interruption was already recorded; refusing to replace it.")
archive = run / "interruption-originals"
archive.mkdir()
original_hashes = {}
for file in sorted(run.iterdir()):
    if file.is_file():
        digest = hashlib.sha256(file.read_bytes()).hexdigest()
        original_hashes[file.name] = digest
        shutil.copy2(file, archive / file.name)
        assert hashlib.sha256((archive / file.name).read_bytes()).hexdigest() == digest
log = (run / "process.log").read_bytes()
evidence = {
    "status": "interrupted", "observed_at": datetime.now(timezone.utc).isoformat(),
    "reason": "Recorded supervised PID is absent, and an independent /proc scan found no native R, time wrapper, or run.py supervisor. No completion artifacts exist.",
    "supervised_pid": pid, "supervised_proc_exists": False,
    "matching_live_processes": matches, "original_files_sha256": original_hashes,
    "original_process_status": record["process_status"],
    "last_observed_elapsed_seconds": record.get("last_observed_elapsed_seconds"),
    "last_observed_high_water_rss_kib": record.get("sampled_child_high_water_rss_kib"),
    "process_log_bytes": len(log), "process_log_nul_bytes": log.count(b"\0"),
    "process_log_trailing_nul_bytes": len(log) - len(log.rstrip(b"\0")),
    "originals_preserved_in": "interruption-originals",
    "scope": "Interruption detected after the fact. Actual stop time, total duration, exit code and cause are unknown. This is neither a success nor an observed timeout. Original process record and logs remain unchanged."
}
output.write_text(json.dumps(evidence, indent=2) + "\n")
print(json.dumps({"run": str(run), **evidence}, indent=2))
