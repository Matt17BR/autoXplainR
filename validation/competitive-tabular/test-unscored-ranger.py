#!/usr/bin/env python3
"""Check an existing small ranger fit and reject changed copies; perform no fits or scoring."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--source", default=str(Path.home() / ".cache/autoxplain-tabular-0.8.0/reproducibility/fixed-native-reference-check-v3/runs/original-fit/rare_interaction/ranger"))
parser.add_argument("--output-directory", required=True)
parser.add_argument("--partitions")
args = parser.parse_args()
source = Path(args.source).expanduser().resolve()
audit = Path(args.output_directory).expanduser().resolve()
if audit == source or source in audit.parents:
    raise ValueError("Test output must be new and outside the original fit directory.")
summary = json.loads((source / "summary.json").read_text())
assert summary["phase"] == "development" and summary["variant"] == "ranger"
assert summary["stage"] == "fit-only" and summary["training_rows"] <= 10000
assert summary["saved_model_bytes"] < 100 * 1024**2
assert len(summary["calibration"]) == 2, "Tamper tests require a small two-setting fixture."
audit.mkdir(parents=True, exist_ok=False)
scripts = Path(__file__).resolve().parent
checker = scripts / "check-unscored-ranger.R"
cache = Path(os.environ.get("AXR_TABULAR_DIR", "~/.cache/autoxplain-tabular-0.8.0")).expanduser().resolve()
partitions = Path(args.partitions).expanduser().resolve() if args.partitions else cache / "partitions.json"
metadata_cache = audit / "metadata-only-cache"
metadata_cache.mkdir()
shutil.copyfile(partitions, metadata_cache / "partitions.json")
environment = dict(os.environ, AXR_TABULAR_DIR=str(metadata_cache), OMP_NUM_THREADS="1", OPENBLAS_NUM_THREADS="1", MKL_NUM_THREADS="1")


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def inventory(directory):
    return {str(path.relative_to(directory)): digest(path) for path in sorted(directory.rglob("*")) if path.is_file()}


original_inventory = inventory(source)


def run(label, fixture, expected_error=None):
    output = audit / f"{label}.json"
    result = subprocess.run(["Rscript", "--vanilla", str(checker), str(fixture), str(output), str(metadata_cache / "partitions.json")],
                            env=environment, text=True, capture_output=True)
    (audit / f"{label}.log").write_text(result.stdout + result.stderr)
    record = json.loads(output.read_text())
    assert (result.returncode == 0) == (expected_error is None), result.stdout + result.stderr
    assert record["status"] == ("ok" if expected_error is None else "failed")
    if expected_error is not None:
        assert expected_error in record["error"], record["error"]
    assert record["evaluation_files_opened"] is False and record["training_partition_opened"] is False
    assert record["evaluation_replay_performed"] is False
    return record


good = run("small-fit-verification", source)
assert good["native_verified_tree_count"] == 500
assert good["native_verified_training_rows"] == summary["training_rows"]
assert good["training_rows_native_verified"] is True
assert good["native_verified_mtry"] == summary["selected_parameters"]["parameters"]["mtry"]
assert good["native_verified_predictors"] == summary["predictors"]
assert good["calibration_metrics_recomputed"] is False
rejections = {}


def fixture(label):
    target = audit / "fixtures" / label
    shutil.copytree(source, target)
    return target


def write_json(path, value):
    path.write_text(json.dumps(value, indent=2) + "\n")


def change_summary(target, update):
    path = target / "summary.json"
    value = json.loads(path.read_text())
    update(value)
    write_json(path, value)
    # Rebind explicit test-copy hashes to exercise semantic checks underneath.
    process_path = target / "process.json"
    process = json.loads(process_path.read_text())
    if "summary_sha256" in process:
        process["summary_sha256"] = digest(path)
        write_json(process_path, process)
    return value


def reject(label, target, expected):
    rejections[label] = run(label, target, expected)["error"]


def modern_command(target, cohort=None):
    process_path = target / "process.json"
    process = json.loads(process_path.read_text())
    script_index = next(index for index, item in enumerate(process["command"]) if Path(item).name == "run-one.R")
    # Keep the ten historical arguments, append an empty reference plan, then
    # the exact cohort now supplied by the hardened runner. This is a command
    # compatibility fixture only; the saved model and frozen scripts are intact.
    process["command"] = process["command"][:script_index + 11] + ["", process["cohort"] if cohort is None else cohort]
    write_json(process_path, process)


target = fixture("modern-12-argument-command")
modern_command(target)
modern = run("modern-12-argument-command", target)
assert modern["command_cohort_binding_present"] is True
assert modern["saved_model_sha256"] == good["saved_model_sha256"]
target = fixture("changed-command-cohort")
modern_command(target, "deliberately-wrong-cohort")
reject("changed-command-cohort", target, "saved command cohort differs from the completed process")


target = fixture("changed-summary-rows")
change_summary(target, lambda value: value.update(training_rows=value["training_rows"] + 1))
reject("changed-summary-rows", target, "Training row, predictor or thread metadata differs")

target = fixture("changed-process-budget")
process = json.loads((target / "process.json").read_text())
process["wall_limit_seconds"] = 7200
write_json(target / "process.json", process)
reject("changed-process-budget", target, "original process resource limits")

target = fixture("changed-fit-command")
process = json.loads((target / "process.json").read_text())
script_index = next(index for index, item in enumerate(process["command"]) if Path(item).name == "run-one.R")
process["command"][script_index + 9] = "joint"
write_json(target / "process.json", process)
reject("changed-fit-command", target, "saved command case, phase, threads, stage or reference plan differs")

target = fixture("changed-calibration-parameters")
def changed_parameters(value):
    value["calibration"][0]["parameters"]["sample.fraction"] = .7
    value["selected_parameters"] = value["calibration"][value["selected_configuration"] - 1]
value = change_summary(target, changed_parameters)
write_json(target / "calibration.json", value["calibration"])
reject("changed-calibration-parameters", target, "Calibration parameters differ from the original controls")

target = fixture("changed-selected-configuration")
def wrong_selection(value):
    value["selected_configuration"] = 3 - value["selected_configuration"]
    value["selected_parameters"] = value["calibration"][value["selected_configuration"] - 1]
change_summary(target, wrong_selection)
reject("changed-selected-configuration", target, "selected configuration is not the minimum saved")

target = fixture("changed-calibration-prediction-hash")
with (target / "calibration-1-predictions.rds").open("ab") as handle:
    handle.write(b"Deliberate rejection fixture.")
reject("changed-calibration-prediction-hash", target, "Training-calibration prediction hash changed")

target = fixture("changed-calibration-trace")
path = target / "process.log"
path.write_text(path.read_text().replace("NATIVE_CALIBRATION ranger 1 fit", "NATIVE_CALIBRATION ranger 9 fit"))
reject("changed-calibration-trace", target, "original training-calibration row traces")

target = fixture("added-evaluation-predictions")
(target / "predictions.rds").write_bytes(b"Deliberate rejection fixture; not predictions.")
reject("added-evaluation-predictions", target, "fit has evaluation metrics, predictions")

target = fixture("changed-frozen-script")
with (target / "scripts/run-one.R").open("a") as handle:
    handle.write("\n# Deliberate source hash rejection fixture.\n")
reject("changed-frozen-script", target, "Frozen script hash changed")

# Change serialized native values, then update only each copy's hash metadata.
# Every checker invocation is a separate fresh R session; no model is refitted.
mutations = {
    "changed-native-row-count": ("saved$model$num.samples <- saved$model$num.samples - 1L", "Native saved forest training row count differs"),
    "changed-native-tree-count": ("saved$model$forest$num.trees <- 499L", "Native saved forest tree count differs"),
    "changed-native-mtry": ("saved$model$mtry <- saved$model$mtry + 1L", "Native saved forest mtry"),
    "changed-native-split-rule": ("saved$model$splitrule <- 'extratrees'", "Native saved forest mtry"),
    "changed-blueprint-name": ("names(saved$blueprint)[1L] <- 'changed_predictor'", "Native predictor names/count or saved feature blueprint"),
    "changed-saved-fit-seed": ("saved$model$call$seed <- 80712L", "Saved literal forest fit-call control differs: seed"),
    "changed-saved-sample-fraction": ("saved$model$call$sample.fraction <- .7", "Saved literal forest fit-call control differs: sample.fraction"),
}
for label, (mutation, expected) in mutations.items():
    target = fixture(label)
    model_path = target / "model.rds"
    r_code = "path <- commandArgs(TRUE)[[1L]]\nsaved <- readRDS(path)\n" + mutation + "\nsaveRDS(saved, path, compress = FALSE, version = 3L)\n"
    result = subprocess.run(["Rscript", "--vanilla", "-", str(model_path)], input=r_code,
                            env=environment, text=True, capture_output=True)
    assert result.returncode == 0, result.stdout + result.stderr
    change_summary(target, lambda value: value.update(saved_model_sha256=digest(model_path), saved_model_bytes=model_path.stat().st_size))
    reject(label, target, expected)

# Output protection must reject both an existing output and a symlink into the
# source. Those invocations must not modify any original run or audit evidence.
protected_output = audit / "small-fit-verification.json"
protected_hash = digest(protected_output)
source_alias = audit / "original-fit-alias"
source_alias.symlink_to(source, target_is_directory=True)
for label, output in [("existing-output", protected_output),
                      ("output-in-source", source / "forbidden-verification.json"),
                      ("output-through-symlink", source_alias / "new-directory/forbidden-verification.json")]:
    result = subprocess.run(["Rscript", "--vanilla", str(checker), str(source), str(output), str(metadata_cache / "partitions.json")],
                            env=environment, text=True, capture_output=True)
    assert result.returncode != 0 and "new output outside" in result.stderr, result.stdout + result.stderr
    rejections[label] = "Output guard rejected before writing."
assert digest(protected_output) == protected_hash
assert inventory(source) == original_inventory
assert sorted(path.name for path in metadata_cache.iterdir()) == ["partitions.json"]
evidence = {
    "status": "ok", "source": str(source), "source_files_unchanged": True,
    "source_inventory_sha256": hashlib.sha256(json.dumps(original_inventory, sort_keys=True).encode()).hexdigest(),
    "no_training_or_evaluation_partitions_in_test_cache": True, "new_fits": 0,
    "evaluation_replay_performed": False, "native_tree_count_verified": 500,
    "native_training_rows_verified": summary["training_rows"], "training_rows_native_verified": True,
    "modern_12_argument_command_compatibility_verified": True,
    "modern_command_fixture_changed_only_process_command": True,
    "rejected_fixtures": rejections, "successful_check": str(audit / "small-fit-verification.json"),
    "checker_sha256": digest(checker), "test_sha256": digest(Path(__file__)),
    "verification_limits": ["No training or evaluation partition contents are opened.",
        "Calibration source-row order, prediction structure and hashes are verified; labels and metric recomputation are excluded.",
        "Calibration selection verifies the minimum saved primary loss, not an independently recomputed loss.",
        "Threads, seed, sample fraction and factor handling are literal saved-call provenance.",
        "Native ranger num.samples proves the saved object's row count, not the underlying rows' identities."]}
write_json(audit / "test-verification.json", evidence)
print(json.dumps(evidence, indent=2))
