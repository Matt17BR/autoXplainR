#!/usr/bin/env python3
"""Verify staged native fitting when evaluation files do not exist."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess

parser = argparse.ArgumentParser()
parser.add_argument("--name", default="native-staging-check")
args = parser.parse_args()
if args.name != Path(args.name).name or args.name in {".", "..", ""}:
    raise SystemExit("Use a plain check-directory name.")
scripts = Path(__file__).resolve().parent
cache = Path(os.environ.get("AXR_TABULAR_DIR", "~/.cache/autoxplain-tabular-0.8.0")).expanduser().resolve()
shadow = cache / "reproducibility" / args.name
shadow.mkdir(parents=True, exist_ok=False)
shutil.copyfile(cache / "partitions.json", shadow / "partitions.json")
partition = shadow / "cases/rare_interaction/development"
partition.mkdir(parents=True)
original_partition = cache / "cases/rare_interaction/development"
(partition / "training.rds").symlink_to(original_partition / "training.rds")
environment = dict(os.environ, AXR_TABULAR_DIR=str(shadow))


def run(variant, cohort, *extra, expect_success=True):
    command = ["python3", str(scripts / "run.py"), "rare_interaction", variant,
               "--cohort", cohort, *extra]
    result = subprocess.run(command, env=environment, text=True, capture_output=True)
    if (result.returncode == 0) != expect_success:
        raise RuntimeError(result.stdout + result.stderr)
    return shadow / "runs" / cohort / "rare_interaction" / variant, result


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


evidence = {"scope": "Development-only harness verification; no acceptance data are opened.",
            "evaluation_files_absent_during_fitting": True, "engines": {}}
for variant in ["ranger", "xgboost"]:
    assert not list(partition.glob("evaluation*"))
    fit, _ = run(variant, "fit-only", "--stage", "fit-only")
    summary = json.loads((fit / "summary.json").read_text())
    assert summary["status"] == "ok" and summary["evaluation_files_opened"] is False
    assert "metrics" not in summary and not (fit / "predictions.rds").exists()
    assert summary["native_training_rows"] == 2500
    if variant == "ranger":
        assert summary["native_verified_training_rows"] == 2500
    evidence["engines"][variant] = {"fit_process_sha256": digest(fit / "process.json"),
                                    "fit_model_sha256": digest(fit / "model.rds")}

for filename in ["evaluation-features.rds", "evaluation-targets.rds"]:
    (partition / filename).symlink_to(original_partition / filename)
for variant in ["ranger", "xgboost"]:
    fit = shadow / "runs/fit-only/rare_interaction" / variant
    score, _ = run(variant, "score-only", "--stage", "score-only", "--fit-run", str(fit))
    joint, _ = run(variant, "joint")
    assert digest(score / "predictions.rds") == digest(joint / "predictions.rds")
    process = json.loads((score / "process.json").read_text())
    assert process["wall_limit_seconds"] == 1200 - process["fit_process_elapsed_seconds"]
    assert process["combined_process_elapsed_seconds"] < 1200
    subprocess.run(["Rscript", "--vanilla", str(scripts / "replay.R"), str(score), "unused"],
                   env=environment, check=True, capture_output=True)
    evidence["engines"][variant].update({
        "staged_and_joint_prediction_files_identical": True,
        "predictions_sha256": digest(score / "predictions.rds"),
        "cold_replay": json.loads((score / "cold-replay.json").read_text()),
        "combined_process_elapsed_seconds": process["combined_process_elapsed_seconds"],
        "score_process_sha256": digest(score / "process.json")})

fit = shadow / "runs/fit-only/rare_interaction/ranger"
first_score = json.loads((shadow / "runs/score-only/rare_interaction/ranger/process.json").read_text())
retry, _ = run("ranger", "score-retry", "--stage", "score-only", "--fit-run", str(fit))
retry_record = json.loads((retry / "process.json").read_text())
assert retry_record["previous_scoring_process_seconds"] == first_score["process_elapsed_seconds"]
assert retry_record["wall_limit_seconds"] == (1200 - retry_record["fit_process_elapsed_seconds"]
                                             - first_score["process_elapsed_seconds"])
evidence["repeat_scoring_consumes_remaining_budget"] = True

# This explicit metadata fixture tests refusal, not a measured benchmark run.
fixture = shadow / "fixtures/exhausted-budget"
fixture.mkdir(parents=True)
fit = shadow / "runs/fit-only/rare_interaction/ranger"
for filename in ["summary.json", "model.rds"]:
    (fixture / filename).symlink_to(fit / filename)
process = json.loads((fit / "process.json").read_text())
process["process_elapsed_seconds"] = 1200
process["fixture_purpose"] = "Simulated exhausted budget; not a measured run."
(fixture / "process.json").write_text(json.dumps(process))
rejected, result = run("ranger", "rejected-budget", "--stage", "score-only", "--fit-run", str(fixture), expect_success=False)
assert "exhausted its combined" in result.stderr + result.stdout
assert not (rejected / "process.log").exists()
rejected, result = run("ranger", "rejected-acceptance", "--phase", "acceptance",
                       "--stage", "score-only", "--fit-run", str(fit), expect_success=False)
assert "Acceptance is locked" in result.stderr + result.stdout
assert not rejected.exists()
evidence["exhausted_budget_rejected_before_R"] = True
evidence["unfrozen_acceptance_rejected_before_R"] = True
evidence["check_script_sha256"] = digest(Path(__file__))
evidence["status"] = "ok"
(shadow / "verification.json").write_text(json.dumps(evidence, indent=2) + "\n")
(scripts / "native-staging-verification.json").write_text(json.dumps(evidence, indent=2) + "\n")
print("Native staging preserves predictions, requires no evaluation files while fitting, and enforces both locks.")
