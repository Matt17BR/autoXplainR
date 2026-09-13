#!/usr/bin/env python3
"""Exercise the unscored XGBoost checker on an existing small saved fit only."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--source", default=str(Path.home() / ".cache/autoxplain-tabular-0.8.0/reproducibility/native-staging-retry-check/runs/fit-only/rare_interaction/xgboost"))
parser.add_argument("--output-directory", required=True)
args = parser.parse_args()
source = Path(args.source).expanduser().resolve()
audit = Path(args.output_directory).expanduser().resolve()
audit.mkdir(parents=True, exist_ok=False)
scripts = Path(__file__).resolve().parent
cache = Path(os.environ.get("AXR_TABULAR_DIR", "~/.cache/autoxplain-tabular-0.8.0")).expanduser().resolve()
metadata_cache = audit / "metadata-only-cache"
metadata_cache.mkdir()
shutil.copyfile(cache / "partitions.json", metadata_cache / "partitions.json")
environment = dict(os.environ, AXR_TABULAR_DIR=str(metadata_cache), OMP_NUM_THREADS="1", OPENBLAS_NUM_THREADS="1")


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def inventory(directory):
    return {str(path.relative_to(directory)): digest(path) for path in sorted(directory.rglob("*")) if path.is_file()}


original_inventory = inventory(source)
original_summary = json.loads((source / "summary.json").read_text())
assert original_summary["case"] == "rare_interaction" and original_summary["phase"] == "development"
assert original_summary["training_rows"] == 2500 and original_summary["variant"] == "xgboost"


def run(label, fixture, expected_error=None):
    output = audit / f"{label}.json"
    result = subprocess.run(["Rscript", "--vanilla", str(scripts / "check-unscored-xgboost.R"),
                             str(fixture), str(output), str(metadata_cache / "partitions.json")],
                            env=environment, text=True, capture_output=True)
    record = json.loads(output.read_text())
    assert (result.returncode == 0) == (expected_error is None), result.stdout + result.stderr
    assert record["status"] == ("ok" if expected_error is None else "failed")
    if expected_error is not None:
        assert expected_error in record["error"], record["error"]
    assert record["evaluation_files_opened"] is False and record["training_partition_opened"] is False
    assert record["evaluation_replay_performed"] is False
    return record


good = run("small-fit-verification", source)
assert good["native_verified_boosting_rounds"] == original_summary["native_verified_boosting_rounds"] == 48
assert good["training_rows_metadata"]["declared"] == 2500 and good["training_rows_native_verified"] is False
assert good["selected_rounds"] == 48 and good["selected_configuration"] == 2
assert good["native_verified_encoded_predictors"] == 15
rejections = {}


def fixture(label):
    target = audit / "fixtures" / label
    shutil.copytree(source, target)
    return target


def change_summary(target, update):
    path = target / "summary.json"
    summary = json.loads(path.read_text())
    update(summary)
    path.write_text(json.dumps(summary, indent=2) + "\n")
    # For a new process that binds summary hashes, update this explicit test
    # fixture's hash so semantic checks are reached; no original is altered.
    process_path = target / "process.json"
    process = json.loads(process_path.read_text())
    if "summary_sha256" in process:
        process["summary_sha256"] = digest(path)
        process_path.write_text(json.dumps(process, indent=2) + "\n")
    return summary


target = fixture("changed-summary-rows")
change_summary(target, lambda summary: summary.update(training_rows=2501))
rejections["changed-summary-rows"] = run("changed-summary-rows", target, "Training row, predictor or thread metadata differs")["error"]
target = fixture("changed-native-round-count")
change_summary(target, lambda summary: summary.update(native_verified_boosting_rounds=49))
rejections["changed-native-round-count"] = run("changed-native-round-count", target, "Native boosted round count differs")["error"]
target = fixture("changed-selected-round")


def change_round(summary):
    summary["calibration"][1]["selected_rounds"] = 49
    summary["selected_parameters"]["selected_rounds"] = 49


summary = change_summary(target, change_round)
(target / "calibration.json").write_text(json.dumps(summary["calibration"], indent=2) + "\n")
rejections["changed-selected-round"] = run("changed-selected-round", target, "Selected rounds differ from the CSV minimum")["error"]
target = fixture("added-evaluation-predictions")
(target / "predictions.rds").write_bytes(b"Deliberate rejection fixture; not a prediction object.")
rejections["added-evaluation-predictions"] = run("added-evaluation-predictions", target, "fit has evaluation metrics, predictions")["error"]
target = fixture("changed-frozen-script")
with (target / "scripts/run-one.R").open("a") as handle:
    handle.write("\n# Deliberate source hash rejection fixture.\n")
rejections["changed-frozen-script"] = run("changed-frozen-script", target, "Frozen script hash changed")["error"]
assert inventory(source) == original_inventory
assert not (metadata_cache / "cases").exists()
evidence = {"status": "ok", "source": str(source), "source_files_unchanged": True,
    "source_inventory_sha256": hashlib.sha256(json.dumps(original_inventory, sort_keys=True).encode()).hexdigest(),
    "no_training_or_evaluation_partitions_in_test_cache": True, "new_fits": 0,
    "evaluation_replay_performed": False, "native_boosted_rounds_verified": 48,
    "recorded_training_rows": 2500, "training_rows_native_verified": False,
    "rejected_fixtures": rejections, "successful_check": str(audit / "small-fit-verification.json"),
    "checker_sha256": digest(scripts / "check-unscored-xgboost.R"), "test_sha256": digest(Path(__file__))}
(audit / "test-verification.json").write_text(json.dumps(evidence, indent=2) + "\n")
print(json.dumps(evidence, indent=2))
