"""Run fixed, single-threaded jobs serially and retain every exit or timeout."""
import json
import os
from pathlib import Path
import shutil
import subprocess
import time

output = Path(os.environ.get("AXR_SEARCH_DIR", "~/.cache/autoxplain-scale-0.7.0/search")).expanduser()
source = Path(__file__).resolve().with_name("profile-one.R")
frozen = output / "profile-one-frozen.R"
shutil.copyfile(source, frozen)
jobs = [(case, solver, k, 912) for case in ("friedman", "bank")
        for k in (8, 10) for solver in ("gam", "bam")]
jobs += [("rare_1pct", solver, 5, seed) for seed in (912, 913)
         for solver in ("gam", "bam", "bam_discrete")]
jobs += [(case, solver, 5, 914) for case in ("regression_10000", "binary_10000")
         for solver in ("gam", "bam", "bam_discrete")]
env = dict(os.environ, OMP_NUM_THREADS="1", OPENBLAS_NUM_THREADS="1", MKL_NUM_THREADS="1")
records = []
for case, solver, k, seed in jobs:
    command = ["Rscript", str(frozen), case, solver, str(k), str(seed)]
    log = output / f"{case}-{seed}-k{k}-{solver}.log"
    started = time.monotonic()
    with log.open("w") as stream:
        try:
            result = subprocess.run(command, env=env, stdout=stream, stderr=subprocess.STDOUT, timeout=120)
            status, exit_code = "finished", result.returncode
        except subprocess.TimeoutExpired:
            status, exit_code = "timeout", None
    record = dict(case=case, solver=solver, k=k, seed=seed, status=status,
                  exit_code=exit_code, process_seconds=time.monotonic()-started,
                  timeout_seconds=120, command=command, log=str(log))
    records.append(record)
    (output / "profile-operations.json").write_text(json.dumps(records, indent=2)+"\n")
    print(case, solver, k, seed, status, exit_code, round(record["process_seconds"], 2), flush=True)

if any(row["status"] != "finished" or row["exit_code"] != 0 for row in records):
    raise SystemExit(1)
