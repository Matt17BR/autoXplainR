"""Validate fixed native forest plans using recorded calibration evidence only."""
import argparse
import hashlib
import json
import math
from pathlib import Path
import re


PROTOCOL = "fixed-native-forest-v1"


def digest(path):
    result = hashlib.sha256()
    with Path(path).open("rb") as source:
        for block in iter(lambda: source.read(1024 * 1024), b""):
            result.update(block)
    return result.hexdigest()


def require(condition, message):
    if not condition:
        raise ValueError("Invalid native reference plan: " + message)


def validate_plan(plan_path, cache, case, phase):
    """Read manifests and source JSON; never open any evaluation partition."""
    plan_path, cache = Path(plan_path), Path(cache)
    require(plan_path.is_absolute(), "use an absolute JSON plan path.")
    plan = json.loads(plan_path.read_text())
    require(plan.get("protocol") == PROTOCOL, "unsupported protocol.")
    partitions_hash = digest(cache / "partitions.json")
    require(plan.get("partitions_sha256") == partitions_hash, "partition manifest changed.")
    partitions = json.loads((cache / "partitions.json").read_text())
    declared = partitions["cases"].get(f"{case}/{phase}")
    require(declared is not None, "case/phase is not declared.")
    entry = plan.get("cases", {}).get(case)
    require(isinstance(entry, dict), "case is absent from the mapping.")
    require(entry.get("source_phase") == "development" and entry.get("source_variant") == "ranger",
            "source must be development ranger.")
    cohort = entry.get("source_cohort", "")
    require(isinstance(cohort, str) and re.fullmatch(r"[a-zA-Z0-9][a-zA-Z0-9._-]*", cohort),
            "invalid source cohort.")
    require(entry.get("threads") == 4 and entry.get("seed") == 80711,
            "fixed reference requires four threads and seed 80711.")
    require(entry.get("full_training_rows") == declared["n_training"], "full training row count differs.")
    require(declared.get("fit_seed") == entry["seed"], "partition seed differs.")
    source = cache / "runs" / cohort / case / "ranger"
    for filename, key in [("process.json", "source_process_sha256"), ("summary.json", "source_summary_sha256")]:
        require(digest(source / filename) == entry.get(key), f"source {filename} hash changed.")
    process = json.loads((source / "process.json").read_text())
    summary = json.loads((source / "summary.json").read_text())
    expected_process = {"case": case, "phase": "development", "variant": "ranger", "cohort": cohort,
                        "process_status": "ok", "exit_code": 0, "threads": entry.get("source_threads"),
                        "partitions_sha256": partitions_hash}
    require(all(process.get(key) == value for key, value in expected_process.items()),
            "source process is not a successful matching development run.")
    require(entry.get("source_threads") in (1, 4), "unsupported source thread count.")
    require(summary.get("status") == "ok" and summary.get("case") == case
            and summary.get("phase") == "development" and summary.get("variant") == "ranger"
            and summary.get("native_threads") == entry["source_threads"],
            "source summary is not a successful matching development run.")
    source_partition = partitions["cases"][f"{case}/development"]
    require(summary.get("partition_files") == source_partition["files"]
            and summary.get("training_rows") == source_partition["n_training"]
            and summary.get("task") == declared["task"], "source training partition identity differs.")
    metric = "rmse" if declared["task"] == "regression" else "log_loss"
    require(entry.get("calibration_primary_metric") == metric, "wrong training-calibration primary metric.")
    records = summary.get("calibration")
    candidates = entry.get("calibration_candidates")
    require(isinstance(records, list) and len(records) > 0 and isinstance(candidates, list)
            and len(records) == len(candidates), "calibration candidate inventory differs.")
    losses = []
    parameter_keys = {"mtry", "num.trees", "min.node.size", "sample.fraction", "splitrule"}
    for record, candidate in zip(records, candidates):
        parameters = record.get("parameters", {})
        require(set(parameters) == parameter_keys and parameters.get("num.trees") == 500
                and parameters.get("min.node.size") == 5 and parameters.get("sample.fraction") == .8
                and parameters.get("splitrule") == ("variance" if metric == "rmse" else "gini")
                and type(parameters.get("mtry")) is int and 1 <= parameters["mtry"] <= declared["predictors"],
                "source ranger parameters violate the fixed protocol.")
        loss = record.get("calibration_metrics", {}).get(metric)
        require(type(loss) in (int, float) and math.isfinite(loss), "calibration primary loss is absent or nonfinite.")
        require(candidate.get("parameters") == parameters
                and candidate.get("calibration_primary_loss") == loss,
                "mapped calibration candidates differ from source training-calibration evidence.")
        losses.append(loss)
    selected = min(range(len(losses)), key=losses.__getitem__) + 1
    require(entry.get("source_selected_configuration") == selected
            and summary.get("selected_configuration") == selected,
            "selected configuration is not the minimum training-calibration primary loss.")
    require(entry.get("parameters") == records[selected - 1]["parameters"]
            and summary.get("selected_parameters") == records[selected - 1],
            "selected parameters differ from the recorded training-calibration winner.")
    return {"protocol": PROTOCOL, "plan_sha256": digest(plan_path), "partitions_sha256": partitions_hash,
            "source_directory": str(source.resolve()), "case": case, "phase": phase,
            "selection_data": "source training-calibration metrics only",
            "new_calibration_fits": 0, "entry": entry,
            "source_selected_parameters": records[selected - 1],
            "source_backend_versions": summary.get("backend_versions"),
            "source_calibration_verified": True, "evaluation_labels_used_for_selection": False}


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--plan", required=True)
    parser.add_argument("--cache", required=True)
    parser.add_argument("--case", required=True)
    parser.add_argument("--phase", choices=["development", "acceptance"], required=True)
    args = parser.parse_args()
    try:
        print(json.dumps(validate_plan(args.plan, args.cache, args.case, args.phase)))
    except (ValueError, KeyError, TypeError, OSError) as error:
        raise SystemExit(str(error))
