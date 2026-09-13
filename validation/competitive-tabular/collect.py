#!/usr/bin/env python3
"""Publish compact development measurements without loading acceptance labels."""
from collections import Counter
from datetime import datetime, timezone
from pathlib import Path
import hashlib
import json
import os
import re

cache = Path(os.environ.get("AXR_TABULAR_DIR", "~/.cache/autoxplain-tabular-0.8.0")).expanduser()
destination = Path(__file__).resolve().parent
rows = []
native_training = []
for process_path in sorted((cache / "runs").glob("*/*/*/process.json")):
    process = json.loads(process_path.read_text())
    interruption = None
    interruption_path = process_path.parent / "interruption.json"
    if interruption_path.exists():
        interruption = json.loads(interruption_path.read_text())
        assert process.get("process_status") == "running"
        assert interruption["status"] == "interrupted"
        for name in ["process.json", "process.log"]:
            assert hashlib.sha256((process_path.parent / name).read_bytes()).hexdigest() == interruption["original_files_sha256"][name]
        process["process_status"] = "interrupted"
    if process["phase"] != "development":
        if process.get("stage") == "fit-only":
            training_record = {key: process[key] for key in ["case", "variant", "phase", "cohort",
                "threads", "stage", "wall_limit_seconds", "address_space_limit_bytes",
                "started_at", "scripts", "partitions_sha256"]}
            for key in ["process_status", "process_elapsed_seconds", "peak_rss_kib",
                        "sampled_child_high_water_rss_kib", "last_observed_elapsed_seconds",
                        "rss_sampling_seconds", "ended_at", "exit_code",
                        "native_reference_protocol", "native_reference_plan_sha256", "native_reference"]:
                if key in process:
                    training_record[key] = process[key]
            training_record.setdefault("process_status", "running")
            training_record["process_record_sha256"] = hashlib.sha256(process_path.read_bytes()).hexdigest()
            if interruption is not None:
                training_record["interruption"] = interruption
            summary_path = process_path.parent / "summary.json"
            if summary_path.exists():
                summary = json.loads(summary_path.read_text())
                assert "metrics" not in summary, "A fit-only source unexpectedly contains evaluation metrics."
                for key in ["status", "error", "training_rows", "predictors", "task", "backend_versions",
                            "native_training_rows", "native_verified_training_rows", "native_verified_boosting_rounds",
                            "native_verified_tree_count", "native_reference_protocol", "native_reference_plan_sha256",
                            "native_reference", "new_calibration_fits", "fit_seed",
                            "calibration", "selected_configuration", "selected_parameters", "refit_elapsed_seconds",
                            "saved_model_bytes", "saved_model_sha256", "evaluation_files_opened", "warnings"]:
                    if key in summary:
                        training_record[key] = summary[key]
                training_record["summary_sha256"] = hashlib.sha256(summary_path.read_bytes()).hexdigest()
            else:
                calibration_path = process_path.parent / "calibration.json"
                if calibration_path.exists():
                    training_record["partial_calibration"] = json.loads(calibration_path.read_text())
            host_link_path = process_path.parent / "host-resources-link.json"
            if host_link_path.exists():
                link = json.loads(host_link_path.read_text())
                host_path = Path(link["path"])
                assert hashlib.sha256(host_path.read_bytes()).hexdigest() == link["sha256"]
                training_record["host_resources"] = json.loads(host_path.read_text())
                training_record["host_resources_sha256"] = link["sha256"]
            shared_host_path = process_path.parent / "shared-host-work.json"
            if shared_host_path.exists():
                training_record["shared_host_work"] = json.loads(shared_host_path.read_text())
            verification_path = process_path.parent / "unscored-model-verification.json"
            if verification_path.exists():
                training_record["unscored_model_verification"] = json.loads(verification_path.read_text())
            xgboost_link = process_path.parent / "unscored-xgboost-verification-link.json"
            if xgboost_link.exists():
                link = json.loads(xgboost_link.read_text())
                verification_path = Path(link["path"])
                assert hashlib.sha256(verification_path.read_bytes()).hexdigest() == link["sha256"]
                training_record["unscored_xgboost_verification"] = json.loads(verification_path.read_text())
                training_record["unscored_xgboost_verification_sha256"] = link["sha256"]
            native_training.append(training_record)
        continue
    run = process_path.parent
    row = {key: process[key] for key in ["case", "variant", "phase", "cohort", "threads",
        "wall_limit_seconds", "address_space_limit_bytes", "started_at", "scripts", "partitions_sha256"]}
    for key in ["process_status", "process_elapsed_seconds", "peak_rss_kib", "sampled_child_high_water_rss_kib",
                "rss_sampling_seconds", "ended_at", "exit_code"]:
        if key in process:
            row[key] = process[key]
    row.setdefault("process_status", "running")
    row["request"] = process.get("request", "paired")
    row["stage"] = process.get("stage", "fit-and-score")
    for key in ["fit_source_process_sha256", "fit_source_model_sha256", "fit_source_summary_sha256",
                "fit_process_elapsed_seconds", "combined_process_elapsed_seconds", "combined_observed_high_water_rss_kib"]:
        if key in process:
            row[key] = process[key]
    row["process_record_sha256"] = hashlib.sha256(process_path.read_bytes()).hexdigest()
    if interruption is not None:
        row["interruption"] = interruption
    recovery_path = run / "recovery-provenance.json"
    if recovery_path.exists():
        row["recovery_provenance"] = json.loads(recovery_path.read_text())
        row["recovery_provenance_sha256"] = hashlib.sha256(recovery_path.read_bytes()).hexdigest()
    for sidecar in ["recovery-verification", "duplicate-row-provenance"]:
        sidecar_path = run / (sidecar + ".json")
        if sidecar_path.exists():
            row[sidecar.replace("-", "_")] = json.loads(sidecar_path.read_text())
    host_link_path = run / "host-resources-link.json"
    if host_link_path.exists():
        link = json.loads(host_link_path.read_text())
        host_path = Path(link["path"])
        assert hashlib.sha256(host_path.read_bytes()).hexdigest() == link["sha256"]
        row["host_resources"] = json.loads(host_path.read_text())
        row["host_resources_sha256"] = link["sha256"]
    if "installed_package_files" in process:
        inventory = process["installed_package_files"]
        row["installed_package_inventory_sha256"] = hashlib.sha256(
            json.dumps(inventory, sort_keys=True, separators=(",", ":")).encode()).hexdigest()
        snapshots = cache / "candidates"
        for snapshot_path in sorted(snapshots.glob("*/snapshot.json")):
            snapshot = json.loads(snapshot_path.read_text())
            if inventory == snapshot.get("installed_package_files"):
                row["candidate_snapshot"] = snapshot["name"]
                row["candidate_source_inventory_sha256"] = snapshot["source_inventory_sha256"]
                break
    summary_path = run / "summary.json"
    if summary_path.exists():
        summary = json.loads(summary_path.read_text())
        for key in ["status", "error", "training_rows", "evaluation_rows", "predictors", "task",
                    "package_version", "backend_versions", "fit_elapsed_seconds", "workflow_elapsed_seconds", "refit_elapsed_seconds",
                    "primary", "final_configuration", "metrics", "calibration", "selected_configuration",
                    "selected_parameters", "warnings", "saved_model_bytes", "predictions_sha256"]:
            if key in summary:
                row[key] = summary[key]
        for key in ["resources", "search", "control", "public_request", "input_policy",
                    "resolved_public_defaults", "report", "explanation_feature_union"]:
            if key in summary:
                row[key] = summary[key]
        if "candidates" in summary:
            row["candidate_status_counts"] = dict(Counter(item["status"] for item in summary["candidates"]))
            row["selected_candidates"] = [item for item in summary["candidates"] if item.get("selected")]
        row["summary_sha256"] = hashlib.sha256(summary_path.read_bytes()).hexdigest()
    calibration_path = run / "calibration.json"
    if "calibration" not in row and calibration_path.exists():
        row["partial_calibration"] = json.loads(calibration_path.read_text())
    replay_path = run / "cold-replay.json"
    if replay_path.exists():
        row["cold_replay"] = json.loads(replay_path.read_text())
    verification_path = run / "evidence-verification.json"
    if verification_path.exists():
        row["evidence_verification"] = json.loads(verification_path.read_text())
    checkpoint_path = run / "observed-memory-checkpoint.json"
    if checkpoint_path.exists():
        row["memory_checkpoint_lower_bound"] = json.loads(checkpoint_path.read_text())
        row["memory_checkpoint_captured_at"] = datetime.fromtimestamp(
            checkpoint_path.stat().st_mtime, timezone.utc).isoformat()
    log_path = run / "process.log"
    if log_path.exists():
        log = log_path.read_text()
        calls = re.findall(r"NATIVE_FIT (\w+) (\d+) rows", log)
        row["native_fit_entries"] = [{"adapter": family, "rows": int(size)} for family, size in calls]
        parameter_entries = re.findall(r"^NATIVE_PARAMETERS (\w+) (.+)$", log, re.MULTILINE)
        if parameter_entries:
            row["native_fit_parameters"] = [{"adapter": family, **json.loads(value)}
                                              for family, value in parameter_entries]
        workflow_events = re.findall(r"^WORKFLOW_STAGE (\w+) (enter|exit) ([0-9.]+)\s*$", log, re.MULTILINE)
        if workflow_events:
            started_stages = {}
            stage_times = []
            for function, event, elapsed in workflow_events:
                if event == "enter":
                    started_stages.setdefault(function, []).append(float(elapsed))
                elif started_stages.get(function):
                    entered = started_stages[function].pop()
                    stage_times.append({"function": function, "entered_process_seconds": entered,
                                        "elapsed_seconds": float(elapsed) - entered})
            row["workflow_stage_timings"] = stage_times
        row["process_log_sha256"] = hashlib.sha256(log_path.read_bytes()).hexdigest()
    rows.append(row)
result = {"scope": "Development only. Acceptance outcomes are not read or summarized by this script.",
          "limitations": ["Concurrent host workloads affect wall-time comparisons; thread counts describe each process.",
                          "The first controlled native-v1 XGBoost smoke used a zero BEGIN endpoint; v2 corrected it to the documented one-based BEGIN. Both independently reproduced calibration losses; use v2 as the reference.",
                          "The first Year baseline supervisor did not sample RSS on timeout. Any memory checkpoint is a lower bound, not an exact peak."],
          "runs": rows}
(destination / "development-results.json").write_text(json.dumps(result, indent=2) + "\n")
(destination / "native-training-results.json").write_text(json.dumps({
    "scope": "Full-training native fit-only records. Evaluation files and quality scores remain unopened by these fitting processes. Calibration metrics use training rows only.",
    "runs": native_training}, indent=2) + "\n")
sources = json.loads((cache / "raw/sources.json").read_text())
(destination / "sources.json").write_text(json.dumps(sources, indent=2) + "\n")
partitions = json.loads((cache / "partitions.json").read_text())
(destination / "partitions.json").write_text(json.dumps(partitions, indent=2) + "\n")
table = ["# Development measurements", "",
         "Generated from the frozen process records by `collect.py`. These are development",
         "comparisons, not locked acceptance results or leaderboard claims. The full",
         "training sets, report workflow and large saved-model usability still require",
         "their separate acceptance checks. Raw predictions and models stay in the cache.", "",
         "Times include the declared workflow and saving its result. Only the",
         "`public-tabular` request includes default explanations and HTML generation;",
         "the paired and tabular comparisons disable them. Jobs shared the host, so times are",
         "observations rather than isolated speed benchmarks. Compare thread counts",
         "explicitly. The public-default row shows its resolved automatic native threads;",
         "its four-thread supervisor ceiling is recorded separately in the JSON evidence.",
         "An unfinished, interrupted or timed-out run has no invented quality score.", "",
         "| Case | Cohort | Learner | Threads | Status | Seconds | Selected-model loss | Forest loss | Cold replay |",
         "| --- | --- | --- | ---: | --- | ---: | ---: | ---: | --- |"]
for row in rows:
    if row["cohort"] == "native-development-v1" and row["variant"] == "xgboost":
        continue
    metric = "rmse" if row.get("task") == "regression" else "log_loss"
    metrics = row.get("metrics", {})
    selected = metrics.get(row.get("primary", row["variant"]), {})
    forest = metrics.get("forest_model", metrics.get("ranger", {}))
    selected_loss = f"{selected[metric]:.6f}" if metric in selected else "pending"
    forest_loss = f"{forest[metric]:.6f}" if metric in forest else "n/a"
    status = row["process_status"]
    if status in {"timeout", "failed", "interrupted"}:
        selected_loss = "no result"
        forest_loss = "no result"
    elapsed = row.get("process_elapsed_seconds")
    seconds = f"{elapsed:.1f}" if elapsed is not None else "pending"
    if status == "interrupted":
        checkpoint = row["interruption"].get("last_observed_elapsed_seconds")
        seconds = f"unknown (last observed {checkpoint:.1f})" if checkpoint is not None else "unknown"
    replay = "all exact" if row.get("cold_replay", {}).get("status") == "ok" else "pending"
    if status in {"timeout", "failed", "interrupted"}:
        replay = "no saved result"
    if row["stage"] != "fit-and-score":
        status += " (" + row["stage"] + ")"
    native_threads = row.get("resolved_public_defaults", {}).get("threads", row["threads"])
    table.append(f"| {row['case']} | {row['cohort']} | {row['variant']} | {native_threads} | {status} | {seconds} | {selected_loss} | {forest_loss} | {replay} |")
table += ["", "Loss is RMSE for YearPredictionMSD and Friedman, and log loss for the",
          "classification cases. Lower is better. Selected means training-resampling",
          "selection for the package and training-calibration selection for native",
          "references. See `development-results.json` for secondary metrics, class recall,",
          "failures, exact controls, parameter settings, source hashes and memory records.", "",
          "Covertype tests same-area row discrimination, not geographic transfer. Bank",
          "excludes call duration but does not establish independent-customer or future-period",
          "performance. The controlled cases were previously inspected. Native references",
          "use two fixed settings and are bounded comparisons, not optimized oracles.", "",
          "The initial native-v1 XGBoost smoke is retained in the JSON record but omitted",
          "from this table: v2 corrected its R prediction interval to one-based BEGIN.", ""]
(destination / "DEVELOPMENT.md").write_text("\n".join(table))
print(f"Wrote {len(rows)} development process records.")
