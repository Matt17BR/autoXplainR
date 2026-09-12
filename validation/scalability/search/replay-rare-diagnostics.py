"""Check one final PIRLS failure, a transient warning, and a clean native fit."""
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import time

cache = Path("~/.cache/autoxplain-scale-0.7.0/search").expanduser()
output = cache / "rare-diagnostic-replay"
output.mkdir(exist_ok=True)
frozen = cache / "profile-rare-diagnostics-frozen.R"
shutil.copyfile(Path(__file__).resolve().with_name("profile-one.R"), frozen)
environment = dict(os.environ, OMP_NUM_THREADS="1", OPENBLAS_NUM_THREADS="1", MKL_NUM_THREADS="1",
                   AXR_SEARCH_DIR=str(output), AXR_PROFILE_LIBRARY="",
                   AXR_TIMING_CONTEXT="functional convergence replay; not comparative timing evidence")
records = []
cases = ((912, 10, "check_convergence"), (912, 8, "ok"), (913, 10, "ok"), (913, 8, "check_convergence"))
if len(sys.argv) > 1:
    assert len(sys.argv) == 3
    cases = tuple(case for case in cases if case[:2] == tuple(map(int, sys.argv[1:])))
    assert len(cases) == 1
record_name = "operations.json" if len(sys.argv) == 1 else f"operations-{sys.argv[1]}-k{sys.argv[2]}.json"
for seed, k, expected_status in cases:
    stem = f"rare_1pct-{seed}-k{k}-bam"
    assert not (output / f"{stem}.json").exists(), "Preserve the previous diagnostic replay"
    command = ["Rscript", str(frozen), "rare_1pct", "bam", str(k), str(seed)]
    started = time.monotonic()
    with (output / f"{stem}.log").open("w") as stream:
        try:
            result = subprocess.run(command, env=environment, stdout=stream,
                                    stderr=subprocess.STDOUT, timeout=120)
            status, exit_code = "finished", result.returncode
        except subprocess.TimeoutExpired:
            status, exit_code = "timeout", None
    summary_path = output / f"{stem}.json"
    actual = json.loads(summary_path.read_text()) if summary_path.exists() else {}
    record = dict(seed=seed, k=k, status=status, exit_code=exit_code,
                  process_seconds=time.monotonic()-started, expected_status=expected_status,
                  actual_status=actual.get("status"), command=command,
                  timing_context=environment["AXR_TIMING_CONTEXT"])
    records.append(record)
    (output / record_name).write_text(json.dumps(records, indent=2)+"\n")
    print(stem, record["actual_status"], "expected", expected_status, flush=True)
assert all(row["status"] == "finished" and row["actual_status"] == row["expected_status"] for row in records)
