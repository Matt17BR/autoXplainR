#!/usr/bin/env python3
"""Development-only fixed-native fit, tamper, staging and replay checks."""
import argparse
from copy import deepcopy
import json
import os
from pathlib import Path
import shutil
import subprocess

from native_reference_plan import digest


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--name", default="fixed-native-reference-check")
args = parser.parse_args()
if args.name != Path(args.name).name or args.name in {"", ".", ".."}:
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
source_relative = Path("runs/native-development-v2/rare_interaction/ranger")
source = shadow / source_relative
source.mkdir(parents=True)
for filename in ("process.json", "summary.json"):
    shutil.copyfile(cache / source_relative / filename, source / filename)
summary = json.loads((source / "summary.json").read_text())
process = json.loads((source / "process.json").read_text())
metric = "log_loss"
selected = min(range(len(summary["calibration"])),
               key=lambda i: summary["calibration"][i]["calibration_metrics"][metric])
entry = {"source_cohort": process["cohort"], "source_phase": "development", "source_variant": "ranger",
         "source_threads": process["threads"], "source_process_sha256": digest(source / "process.json"),
         "source_summary_sha256": digest(source / "summary.json"), "source_selected_configuration": selected + 1,
         "selection_basis": "Development smoke fixture; training-calibration loss only.",
         "calibration_primary_metric": metric,
         "calibration_candidates": [{"parameters": record["parameters"],
                                     "calibration_primary_loss": record["calibration_metrics"][metric]}
                                    for record in summary["calibration"]],
         "parameters": summary["calibration"][selected]["parameters"],
         "threads": 4, "seed": 80711, "full_training_rows": 2500}
plan = {"protocol": "fixed-native-forest-v1", "partitions_sha256": digest(shadow / "partitions.json"),
        "scope": "Development-only verification fixture, never acceptance authorization.",
        "cases": {"rare_interaction": entry}}
plan_path = shadow / "smoke-plan.json"
plan_path.write_text(json.dumps(plan, indent=2) + "\n")
environment = dict(os.environ, AXR_TABULAR_DIR=str(shadow))


def run(cohort, *extra, expect_success=True, case="rare_interaction"):
    command = ["python3", str(scripts / "run.py"), case, "ranger",
               "--cohort", cohort, "--threads", "4", *extra]
    result = subprocess.run(command, env=environment, text=True, capture_output=True)
    if (result.returncode == 0) != expect_success:
        raise RuntimeError(result.stdout + result.stderr)
    return shadow / "runs" / cohort / case / "ranger", result


rejections = []


def reject_plan(label, altered, expected):
    path = shadow / f"{label}.json"
    path.write_text(json.dumps(altered) + "\n")
    rejected, result = run(label, "--stage", "fit-only", "--native-reference-plan", str(path),
                           expect_success=False)
    assert expected in result.stdout + result.stderr, result.stdout + result.stderr
    assert not (rejected / "process.log").exists()
    rejections.append(label)
    return path


for filename in ("process.json", "summary.json"):
    path = source / filename
    original = path.read_bytes()
    path.write_bytes(original + b"\n")
    reject_plan("tampered-source-" + path.stem, plan, f"source {filename} hash changed")
    path.write_bytes(original)

altered = deepcopy(plan)
altered["partitions_sha256"] = "0" * 64
reject_plan("changed-partitions", altered, "partition manifest changed")
altered = deepcopy(plan)
altered["protocol"] = "unapproved-protocol"
reject_plan("changed-protocol", altered, "unsupported protocol")
altered = deepcopy(plan)
altered["cases"]["rare_interaction"]["parameters"] = dict(entry["parameters"], **{"num.trees": 499})
reject_plan("changed-tree-count", altered, "selected parameters differ")
altered = deepcopy(plan)
altered["cases"]["rare_interaction"]["full_training_rows"] = 2499
reject_plan("changed-row-count", altered, "full training row count differs")
altered = deepcopy(plan)
altered["cases"]["rare_interaction"]["calibration_candidates"][0]["calibration_primary_loss"] = 0.0
reject_plan("changed-calibration-loss", altered, "mapped calibration candidates differ")
altered = deepcopy(plan)
loser = 1 - selected
altered["cases"]["rare_interaction"]["source_selected_configuration"] = loser + 1
altered["cases"]["rare_interaction"]["parameters"] = summary["calibration"][loser]["parameters"]
loser_plan_path = reject_plan("nonminimum-selection", altered, "not the minimum training-calibration")

# The R entry point independently repeats validation, even without Python's preflight.
direct = shadow / "direct-R-rejection"
direct.mkdir()
direct_result = subprocess.run(["Rscript", "--vanilla", str(scripts / "run-one.R"),
    "rare_interaction", "ranger", "development", str(direct), "unused", "4", "", "paired",
    "fit-only", "", str(loser_plan_path)], env=environment, text=True, capture_output=True)
assert direct_result.returncode != 0 and "provenance validation failed" in direct_result.stderr
assert not (direct / "model.rds").exists()

assert not list(partition.glob("evaluation*"))
fit, _ = run("fixed-fit", "--stage", "fit-only", "--native-reference-plan", str(plan_path))
fixed_summary = json.loads((fit / "summary.json").read_text())
fixed_process = json.loads((fit / "process.json").read_text())
assert fixed_summary["status"] == "ok" and fixed_summary["evaluation_files_opened"] is False
assert "metrics" not in fixed_summary and not (fit / "predictions.rds").exists()
assert not list(fit.glob("calibration*")) and "NATIVE_CALIBRATION" not in (fit / "process.log").read_text()
assert fixed_summary["native_verified_training_rows"] == fixed_summary["native_training_rows"] == 2500
assert fixed_summary["native_verified_tree_count"] == 500 and fixed_summary["new_calibration_fits"] == 0
assert fixed_summary["selected_parameters"]["parameters"] == entry["parameters"]
assert fixed_summary["native_reference"]["evaluation_labels_used_for_selection"] is False
assert fixed_summary["saved_model_sha256"] == digest(fit / "model.rds")
assert fixed_process["summary_sha256"] == digest(fit / "summary.json")
assert fixed_process["native_reference_plan_sha256"] == digest(fit / "native-reference-plan.json") == digest(plan_path)
assert fixed_process["scripts"]["fixed-native-forest-v1.json"] == digest(scripts / "fixed-native-forest-v1.json")
assert fixed_process["wall_limit_seconds"] == fixed_process["combined_wall_limit_seconds"] == 1200
assert fixed_process["address_space_limit_bytes"] == 24 * 1024**3
assert fixed_process["command"][-1] == str(fit / "native-reference-plan.json")

# Missing-training fixture exercises the 7200-second declaration without a full
# fit or any acceptance file present. Failure must occur before model fitting.
acceptance_plan = json.loads((scripts / "fixed-native-forest-v1.json").read_text())
acceptance_plan["scope"] = "Missing-training metadata fixture only; no acceptance fit or data access."
bank_source_relative = Path("runs") / acceptance_plan["cases"]["bank"]["source_cohort"] / "bank/ranger"
(shadow / bank_source_relative).mkdir(parents=True)
for filename in ("process.json", "summary.json"):
    shutil.copyfile(cache / bank_source_relative / filename, shadow / bank_source_relative / filename)
acceptance_path = shadow / "acceptance-budget-missing-training-fixture.json"
acceptance_path.write_text(json.dumps(acceptance_plan) + "\n")
assert not (shadow / "cases/bank/acceptance").exists()
budget_fixture, _ = run("acceptance-budget-missing-training-fixture", "--phase", "acceptance",
    "--stage", "fit-only", "--native-reference-plan", str(acceptance_path), expect_success=False, case="bank")
budget_process = json.loads((budget_fixture / "process.json").read_text())
assert budget_process["wall_limit_seconds"] == budget_process["combined_wall_limit_seconds"] == 7200
assert budget_process["address_space_limit_bytes"] == 24 * 1024**3
assert "NATIVE_FINAL" not in (budget_fixture / "process.log").read_text()
assert not (budget_fixture / "model.rds").exists()

# The unchanged default still performs its calibration stage with no evaluation files.
default_fit, _ = run("original-fit", "--stage", "fit-only")
assert "NATIVE_CALIBRATION" in (default_fit / "process.log").read_text()
assert (default_fit / "calibration.json").is_file()
assert json.loads((default_fit / "summary.json").read_text())["evaluation_files_opened"] is False

# Metadata-only rejection checks happen before any scoring process can open data.
original = (fit / "summary.json").read_bytes()
(fit / "summary.json").write_bytes(original + b"\n")
rejected, result = run("changed-saved-summary", "--stage", "score-only", "--fit-run", str(fit), expect_success=False)
assert "identity or completion evidence changed" in result.stdout + result.stderr
assert not (rejected / "process.log").exists()
(fit / "summary.json").write_bytes(original)
rejected, result = run("unfrozen-acceptance", "--phase", "acceptance", "--stage", "score-only",
                       "--fit-run", str(fit), expect_success=False)
assert "Acceptance is locked" in result.stdout + result.stderr and not rejected.exists()

# Scoring/replay use this development partition only, after fit-only checks finish.
for filename in ("evaluation-features.rds", "evaluation-targets.rds"):
    (partition / filename).symlink_to(original_partition / filename)
score, _ = run("fixed-score", "--stage", "score-only", "--fit-run", str(fit))
score_process = json.loads((score / "process.json").read_text())
assert score_process["fit_source_process_sha256"] == digest(fit / "process.json")
assert score_process["fit_source_summary_sha256"] == digest(fit / "summary.json")
assert score_process["fit_source_model_sha256"] == digest(fit / "model.rds")
assert score_process["wall_limit_seconds"] == 1200 - fixed_process["process_elapsed_seconds"]
assert score_process["combined_process_elapsed_seconds"] < 1200
subprocess.run(["Rscript", "--vanilla", str(scripts / "replay.R"), str(score), "unused"],
               env=environment, check=True, capture_output=True)
retry, _ = run("fixed-score-retry", "--stage", "score-only", "--fit-run", str(fit))
retry_process = json.loads((retry / "process.json").read_text())
assert retry_process["previous_scoring_process_seconds"] == score_process["process_elapsed_seconds"]
assert retry_process["wall_limit_seconds"] == (1200 - fixed_process["process_elapsed_seconds"]
                                               - score_process["process_elapsed_seconds"])
assert digest(score / "predictions.rds") == digest(retry / "predictions.rds")

evidence = {"status": "ok", "scope": "Development-only smoke, tamper and replay verification; no acceptance files opened.",
    "evaluation_files_absent_during_fitting": True, "new_calibration_fits": 0,
    "verified_training_rows": 2500, "verified_trees": 500, "native_threads": 4,
    "fit_process_sha256": digest(fit / "process.json"), "fit_summary_sha256": digest(fit / "summary.json"),
    "fit_model_sha256": digest(fit / "model.rds"), "plan_sha256": digest(plan_path),
    "rejected_before_R": rejections, "R_independent_provenance_rejection": True,
    "original_workflow_retains_calibration": True, "changed_saved_summary_rejected": True,
    "unfrozen_acceptance_rejected": True, "score_retry_consumes_remaining_combined_budget": True,
    "acceptance_budget_metadata_without_training_files": {"wall_limit_seconds": 7200,
        "address_space_limit_bytes": 24 * 1024**3, "fit_started": False, "acceptance_files_present": False},
    "cold_replay": json.loads((score / "cold-replay.json").read_text()),
    "check_script_sha256": digest(Path(__file__)), "directory": str(shadow)}
(shadow / "verification.json").write_text(json.dumps(evidence, indent=2) + "\n")
(scripts / "fixed-native-reference-verification.json").write_text(json.dumps(evidence, indent=2) + "\n")
print(json.dumps(evidence, indent=2))
