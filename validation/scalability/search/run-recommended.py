"""Measure public recommended runs serially with an external ten-minute cap."""
import json
import hashlib
import os
from pathlib import Path
import shutil
import subprocess
import sys
import time

variant = sys.argv[1]
assert variant in {"baseline", "candidate", "candidate_binary_guard", "candidate_fixed_gam"}
cases = sys.argv[2:] or ["friedman_noise", "bank_marketing"]
assert all(case in {"friedman_noise", "bank_marketing"} for case in cases)
output = Path(os.environ.get("AXR_SEARCH_DIR", "~/.cache/autoxplain-scale-0.7.0/search")).expanduser()
source = Path(__file__).resolve().with_name("run-recommended.R")
script_sha256 = hashlib.sha256(source.read_bytes()).hexdigest()
frozen = output / f"run-recommended-{variant}-{script_sha256[:12]}.R"
if frozen.exists():
    assert hashlib.sha256(frozen.read_bytes()).hexdigest() == script_sha256
else:
    shutil.copyfile(source, frozen)
environment = dict(os.environ, OMP_NUM_THREADS="1", OPENBLAS_NUM_THREADS="1", MKL_NUM_THREADS="1")
records = []
library_name = ("baseline-library" if variant == "baseline" else
                "candidate-binary-guard-library" if variant == "candidate_binary_guard" else
                "candidate-library")
library = output / library_name / "AutoXplainR"
installed = {str(path.relative_to(library)): hashlib.sha256(path.read_bytes()).hexdigest()
             for path in sorted(library.rglob("*")) if path.is_file()}
assert installed, "Installed package must be available"
if variant != "baseline":
    manifest = ("candidate-binary-guard-installed-sha256.json" if variant == "candidate_binary_guard"
                else "candidate-final-installed-sha256.json")
    expected = json.loads(source.with_name(manifest).read_text())
    assert installed == expected, "Candidate installation changed after its recorded source snapshot"
(output / f"recommended-{variant}-installed-sha256.json").write_text(
    json.dumps(installed, indent=2)+"\n")
for case in cases:
    destination = output / "recommended" / variant / case
    destination.mkdir(parents=True, exist_ok=True)
    assert not (destination / "summary.json").exists(), "Preserve previous measurements"
    assert not (destination / "fit-timer.json").exists(), "Preserve previous fitting timers"
    command = ["Rscript", str(frozen), case, variant]
    started = time.monotonic()
    with (destination / "run.log").open("w") as stream:
        try:
            result = subprocess.run(command, env=environment, stdout=stream,
                                    stderr=subprocess.STDOUT, timeout=600)
            status, exit_code = "finished", result.returncode
        except subprocess.TimeoutExpired:
            status, exit_code = "timeout", None
    record = dict(case=case, variant=variant, status=status, exit_code=exit_code,
                  process_seconds=time.monotonic()-started, timeout_seconds=600,
                  command=command, script_sha256=script_sha256,
                  native_threads={key: environment[key] for key in
                                  ("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS")})
    records.append(record)
    (output / f"recommended-{variant}-operations.json").write_text(json.dumps(records, indent=2)+"\n")
    print(case, variant, status, exit_code, round(record["process_seconds"], 2), flush=True)
if any(row["status"] != "finished" or row["exit_code"] != 0 for row in records):
    raise SystemExit(1)
