#!/usr/bin/env python3
"""Collect completed development diagnostics without opening acceptance data."""
import argparse
import hashlib
import json
from pathlib import Path
import re
import shutil
import statistics

parser = argparse.ArgumentParser()
parser.add_argument("run", type=Path)
parser.add_argument("--output", type=Path, default=Path(__file__).resolve().parent)
args = parser.parse_args()


def read(path):
    return json.loads(path.read_text())


def digest(path):
    result = hashlib.sha256()
    with path.open("rb") as source:
        for block in iter(lambda: source.read(1024 * 1024), b""):
            result.update(block)
    return result.hexdigest()


records = {}
copies = []
freeze = read(args.run / "freeze.json")
for name, expected in freeze["files"].items():
    if digest(args.run / name) != expected:
        raise SystemExit(f"Frozen diagnostic file changed: {name}")
expected_stages = {"prepare", "fit", "replay", "summarize",
                   "screen-main_model", "screen-simple_baseline",
                   "screen-forest500", "screen-forest256",
                   "detail-forest500", "detail-forest256"}
for case in ["yearprediction", "covertype"]:
    directory = args.run / case
    prepare = read(directory / "prepare/summary.json")
    for name, path in prepare["source_files"].items():
        if digest(Path(path)) != prepare["source_sha256"][name]:
            raise SystemExit(f"Original development evidence changed: {path}")
    fit = read(directory / "fit/summary.json")
    replay = read(directory / "replay/summary.json")
    comparison = read(directory / "summarize/summary.json")
    original = read(Path(prepare["source_files"]["source_summary"]))
    original_forest_fits = [row for row in original["refit"]["attempts"]
                           if row["family"] == "forest" and row["status"] == "ok" and
                           row["fit_seed"] == replay["records"]["forest500"]["native"]["fit_seed"]]
    if len(original_forest_fits) != 1:
        raise SystemExit(f"Original successful forest fit is not uniquely bound: {case}")
    original_forest_fit = original_forest_fits[0]
    stages = {}
    process_paths = sorted(directory.glob("*/process.json"))
    if {path.parent.name for path in process_paths} != expected_stages:
        raise SystemExit(f"Incomplete or unexpected diagnostic stages: {case}")
    for path in process_paths:
        process = read(path)
        if process["process_status"] != "ok" or read(path.parent / "summary.json")["status"] != "ok":
            raise SystemExit(f"Stage did not complete: {path}")
        resources = (path.parent / "resource-usage.txt").read_text()
        match = re.search(r"Maximum resident set size \(kbytes\):\s*(\d+)", resources)
        other_r_processes = [row for row in process["shared_host_processes_before"]
                             if len(row.split(maxsplit=3)) == 4 and row.split(maxsplit=3)[2] == "R"]
        stages[path.parent.name] = {
            "status": process["process_status"],
            "started_at": process["started_at"], "ended_at": process["ended_at"],
            "process_elapsed_seconds": process["process_elapsed_seconds"],
            "peak_rss_kib": int(match.group(1)) if match else None,
            "threads": process["threads"],
            "wall_limit_seconds": process["wall_limit_seconds"],
            "address_space_limit_bytes": process["address_space_limit_bytes"],
            "other_r_processes_at_start": len(other_r_processes),
            "native_reference_shared_host": any("run-one.R" in row and "fit-only" in row
                                                for row in other_r_processes),
            "process_record_sha256": digest(path)}
    forests = {}
    for key in ["forest500", "forest256"]:
        item = replay["records"][key]
        screen = read(directory / f"screen-{key}/summary.json")
        detail = read(directory / f"detail-{key}/summary.json")
        expected_trees = int(key.removeprefix("forest"))
        evidence = item["native"]["fit_evidence"]
        if not (item["native"]["trees"] == expected_trees and
                item["native"]["native_training_rows"] == 50000 and
                item["prediction_rows"] == 20000 and
                item["cold_replay_maximum_absolute_difference"] <= 1e-12 and
                item["native_maximum_absolute_difference"] <= 1e-12 and
                screen["rows"] == detail["rows"] == 5000 and
                screen["repeats"] == 5 and detail["repeats"] == 20 and
                screen["repeat_score_dimensions"] == [item["native"]["effective_predictors"], 5] and
                detail["repeat_score_dimensions"] == [len(detail["detailed_feature_order"]), 20] and
                evidence["requested_parameters"] == evidence["effective_parameters"] == item["native"]["parameters"] and
                evidence["requested_configuration_seed"] == evidence["fit_seed"] == item["native"]["fit_seed"] and
                evidence["threads"] == 4 and evidence["learned"]["oob_computed"] is True and
                screen["reference_rows_sha256"] == detail["reference_rows_sha256"] == replay["reference_rows_sha256"]):
            raise SystemExit(f"Diagnostic structure or replay mismatch: {case}/{key}")
        union = detail["ordinary_report_unions"][key]
        forests[key] = {
            "metrics": item["metrics"], "classification": item["classification"],
            "confusion_matrix_class_order": item["class_names"],
            "tree_count": item["native"]["trees"],
            "native_training_rows": item["native"]["native_training_rows"],
            "effective_predictors": item["native"]["effective_predictors"],
            "parameters": item["native"]["parameters"], "fit_seed": item["native"]["fit_seed"],
            "native_fit_evidence": item["native"]["fit_evidence"],
            "oob_prediction_error": item["native"]["oob_prediction_error"],
            "nodes": item["native"]["nodes"], "model_bytes": item["model_bytes"],
            "model_sha256": digest(directory / f"{key}.rds"),
            "replayed_predictions_sha256": digest(directory / f"{key}-replayed-predictions.rds"),
            "original_fit_seconds": original_forest_fit["elapsed_ms"] / 1000 if key == "forest500" else fit["fit_elapsed_seconds"],
            "cold_replay_maximum_absolute_difference": item["cold_replay_maximum_absolute_difference"],
            "native_maximum_absolute_difference": item["native_maximum_absolute_difference"],
            "prediction_rows": item["prediction_rows"],
            "prediction_timings_5000_seconds": [row["seconds"] for row in item["prediction_timings_5000"]],
            "prediction_median_5000_seconds": statistics.median(row["seconds"] for row in item["prediction_timings_5000"]),
            "screening_seconds": screen["elapsed_seconds"],
            "detailed_importance_seconds": detail["elapsed_seconds"],
            "ordinary_report_feature_union": union,
            "ordinary_report_forest_permutation_calls": 5 * item["native"]["effective_predictors"] + 20 * len(union),
            "detailed_comparison_features": detail["detailed_feature_order"],
            "additional_borderline_features": detail["borderline_inputs"],
            "screening_repeat_dimensions": screen["repeat_score_dimensions"],
            "detailed_repeat_dimensions": detail["repeat_score_dimensions"],
            "reference_rows_sha256": detail["reference_rows_sha256"],
            "screening_repeat_scores_sha256": screen["repeat_scores_sha256"],
            "detailed_repeat_scores_sha256": detail["repeat_scores_sha256"]}
    primary = "rmse" if case == "yearprediction" else "log_loss"
    before, after = forests["forest500"], forests["forest256"]
    records[case] = {
        "raw_predictors": prepare["raw_predictors"],
        "removed_constant_features": prepare["removed_constant_features"],
        "primary_metric": primary,
        "primary_loss_difference_256_minus500": after["metrics"][primary] - before["metrics"][primary],
        "primary_loss_relative_change": after["metrics"][primary] / before["metrics"][primary] - 1,
        "sampled_class_counts": replay.get("sampled_class_counts"),
        "development_training_rows": 50000, "development_assessment_rows": 20000,
        "forests": forests, "importance_comparison": comparison,
        "source_sha256": prepare["source_sha256"], "stages": stages}
    for kind in ["screen", "detail"]:
        copies.append((directory / f"summarize/{kind}-comparison.csv",
                       args.output / f"final-tree-{case}-{kind}-comparison.csv"))

output = {
    "scope": "Development-only fixed-configuration diagnostic. No acceptance data or full-workflow timing.",
    "limitations": [
        "Changing tree count changes the existing production-derived fitting seed; this measures both together.",
        "The previously completed 500-tree fits ran at a different time; fit timings are not controlled speedup estimates.",
        "Native references shared the host during some diagnostic stages; stage records disclose that overlap.",
        "Covertype's development pool is below the automatic final 256-tree trigger; its 256-tree model is explicitly diagnostic.",
        "Shuffle intervals condition on 5000 reference rows and fixed models; they exclude sampling and fitting uncertainty.",
        "Detailed comparisons retain both ordinary model-set unions and additional borderline inputs, so are not complete default report timings."],
    "sample_rows": 5000, "screening_repeats": 5, "detailed_repeats": 20,
    "sample_seed": 80711, "screening_permutation_seed": 80711, "detailed_permutation_seed": 80712,
    "source_freeze_declared_at": freeze["declared_at"],
    "source_freeze_sha256": digest(args.run / "freeze.json"),
    "protocol_sha256": digest(args.run / "scripts/final-tree-protocol.md"),
    "runner_sha256": digest(args.run / "scripts/final-tree-probe.R"),
    "supervisor_sha256": digest(args.run / "scripts/final-tree-probe.py"),
    "collector_sha256": digest(Path(__file__)), "records": records}
for source, destination in copies:
    shutil.copy2(source, destination)
(args.output / "final-tree-results.json").write_text(json.dumps(output, indent=2) + "\n")
print(json.dumps({case: {"loss_difference": value["primary_loss_difference_256_minus500"],
                         "relative_loss_change": value["primary_loss_relative_change"]}
                  for case, value in records.items()}, indent=2))
