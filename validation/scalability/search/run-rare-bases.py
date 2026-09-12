"""Check higher basis sizes on the same two fixed one-percent-event folds."""
import json
import os
from pathlib import Path
import shutil
import subprocess
import time

output = Path(os.environ.get(
    "AXR_SEARCH_DIR", "~/.cache/autoxplain-scale-0.7.0/search"
)).expanduser()
source = Path(__file__).resolve().with_name("profile-one.R")
frozen = output / "profile-rare-bases-frozen.R"
shutil.copyfile(source, frozen)
environment = dict(os.environ, OMP_NUM_THREADS="1", OPENBLAS_NUM_THREADS="1", MKL_NUM_THREADS="1")
environment["AXR_PROFILE_LIBRARY"] = os.environ.get("AXR_PROFILE_LIBRARY", str(output / "candidate-library"))
environment["AXR_TIMING_CONTEXT"] = "functional check alongside gallery; not comparative timing evidence"
records = []
for seed in (912, 913):
    for k in (8, 10):
        for solver in ("gam", "bam"):
            stem = f"rare_1pct-{seed}-k{k}-{solver}"
            assert not (output / f"{stem}.json").exists(), "Preserve previous measurements"
            command = ["Rscript", str(frozen), "rare_1pct", solver, str(k), str(seed)]
            started = time.monotonic()
            with (output / f"{stem}.log").open("w") as stream:
                try:
                    result = subprocess.run(command, env=environment, stdout=stream,
                                            stderr=subprocess.STDOUT, timeout=120)
                    status, exit_code = "finished", result.returncode
                except subprocess.TimeoutExpired:
                    status, exit_code = "timeout", None
            record = dict(case="rare_1pct", seed=seed, k=k, solver=solver,
                          status=status, exit_code=exit_code,
                          process_seconds=time.monotonic()-started,
                          timeout_seconds=120, command=command,
                          timing_context=environment["AXR_TIMING_CONTEXT"])
            records.append(record)
            (output / "rare-bases-operations.json").write_text(json.dumps(records, indent=2)+"\n")
            print(stem, status, exit_code, round(record["process_seconds"], 2), flush=True)
if any(row["status"] != "finished" or row["exit_code"] != 0 for row in records):
    raise SystemExit(1)
