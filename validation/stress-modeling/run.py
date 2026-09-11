#!/usr/bin/env python3
"""Run every declared challenge serially, retaining process failures and timing."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import time


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--run", default="baseline")
    parser.add_argument("--library", required=True)
    parser.add_argument("--timeout-seconds", type=int, default=600)
    parser.add_argument("--scenarios", nargs="+", default=["friedman_noise", "sparse_wide", "rare_interaction", "bank_marketing"],
        choices=["friedman_noise", "sparse_wide", "rare_interaction", "bank_marketing"])
    parser.add_argument("--variants", nargs="+", default=["core", "stronger", "references"],
        choices=["core", "stronger", "recommended", "references"])
    arguments = parser.parse_args()
    script = Path(__file__).resolve().with_name("run-one.R")
    output = Path(os.environ.get("AXR_STRESS_DIR", "~/.cache/autoxplain-stress-0.6.2/benchmark")).expanduser()
    # Rscript parses expressions while executing. Freeze both sources before
    # spawning children so later working-tree edits cannot corrupt an active run.
    harness = output / arguments.run / "harness"
    harness.mkdir(parents=True, exist_ok=True)
    hashes = {}
    for source in (script, script.with_name("common.R")):
        destination = harness / source.name
        if destination.exists() and destination.read_bytes() != source.read_bytes():
            raise RuntimeError("Harness changed. Use a new --run name instead of mixing script versions.")
        if not destination.exists():
            shutil.copyfile(source, destination)
        hashes[source.name] = hashlib.sha256(destination.read_bytes()).hexdigest()
    (harness / "sha256.json").write_text(json.dumps(hashes, indent=2) + "\n")
    script = harness / script.name
    (output / arguments.run / "run-plan.json").write_text(json.dumps({
        "scenarios": arguments.scenarios, "variants": arguments.variants,
        "library": str(Path(arguments.library).resolve()),
        "timeout_seconds": arguments.timeout_seconds,
        "budget": {"core": 15, "stronger": 15, "recommended": 30},
        "notes": "Recommended uses the public six-family portfolio with its automatic 30-configuration budget. No custom grids or test-driven settings."
    }, indent=2) + "\n")
    environment = dict(os.environ, AXR_STRESS_RUN=arguments.run,
        AXR_STRESS_LIBRARY=str(Path(arguments.library).resolve()),
        OMP_NUM_THREADS="1", OPENBLAS_NUM_THREADS="1", MKL_NUM_THREADS="1")
    records = []
    for scenario in arguments.scenarios:
        for variant in arguments.variants:
            destination = output / arguments.run / scenario / variant
            destination.mkdir(parents=True, exist_ok=True)
            if (destination / "summary.json").exists():
                summary = json.loads((destination / "summary.json").read_text())
                records.append({"scenario": scenario, "variant": variant,
                    "status": "existing", "result_status": summary["status"]})
                print(scenario, variant, "existing", summary["status"], flush=True)
                continue
            started = time.monotonic()
            command = ["Rscript", str(script), scenario, variant]
            with (destination / "process.log").open("w") as logfile:
                try:
                    process = subprocess.run(command, env=environment,
                        stdout=logfile, stderr=subprocess.STDOUT, timeout=arguments.timeout_seconds)
                    record = {"status": "finished", "exit_code": process.returncode}
                except subprocess.TimeoutExpired:
                    record = {"status": "timeout", "exit_code": None}
            record.update(scenario=scenario, variant=variant, command=command,
                elapsed_seconds=time.monotonic() - started)
            records.append(record)
            (output / arguments.run / "processes.json").write_text(json.dumps(records, indent=2) + "\n")
            print(scenario, variant, record, flush=True)
    failed = any(record.get("exit_code", 0) not in (0,) or
        record.get("result_status", "ok") != "ok" for record in records)
    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
