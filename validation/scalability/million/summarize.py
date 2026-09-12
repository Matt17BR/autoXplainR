"""Build a compact index without discarding failed or interrupted runs."""
import argparse
import csv
import json
from pathlib import Path
import re


parser = argparse.ArgumentParser()
parser.add_argument("directory", type=Path)
args = parser.parse_args()
rows = []
stage_profiles = []
for file in sorted(args.directory.glob("*/supervisor.json")):
    directory = file.parent
    supervisor = json.loads(file.read_text())
    input_path = directory / "input.json"
    source = json.loads(input_path.read_text()) if input_path.exists() else {}
    result_path = directory / "result.json"
    result = json.loads(result_path.read_text()) if result_path.exists() else {}
    checkpoint_path = directory / "public-call.json"
    checkpoint = json.loads(checkpoint_path.read_text()) if checkpoint_path.exists() else {}
    # Keep the original failure, while distinguishing the two established
    # harness defects from a failure in the package under test.
    harness_errors = {
        "No method asJSON S3 class: table": "fixture metadata serialization before the public call",
        "max(abs(rowSums(prediction) - 1)) < 1e-08 is not TRUE":
            "verification tolerance stricter than the public native-probability contract",
    }
    harness_error = harness_errors.get(result.get("message"))
    tuning_path = directory / "tuning.json"
    tuning = json.loads(tuning_path.read_text()) if tuning_path.exists() else {}
    candidates = tuning.get("candidates", [])
    failure_fields = {"configuration_id", "family", "hyperparameters", "folds_completed",
                      "optimization_issues", "status"}
    failed_configurations = [{key: value for key, value in candidate.items() if key in failure_fields}
                             for candidate in candidates if candidate.get("status") != "ok"]
    cold_path = directory / "cold-replay.json"
    cold = json.loads(cold_path.read_text()) if cold_path.exists() else {}
    times_path = directory / "time.txt"
    match = re.search(r"Maximum resident set size \(kbytes\):\s*(\d+)",
                      times_path.read_text() if times_path.exists() else "")
    events_path = directory / "stages.jsonl"
    events = [json.loads(line) for line in events_path.read_text().splitlines() if line.strip()] if events_path.exists() else []
    pending, profiles = {}, {}
    for event in events:
        stage = event["stage"]
        if event["boundary"] == "enter":
            pending.setdefault(stage, []).append(event)
        elif pending.get(stage):
            entered = pending[stage].pop()
            profile = profiles.setdefault(stage, {"calls": 0, "inclusive_seconds": 0.0})
            profile["calls"] += 1
            profile["inclusive_seconds"] += event["elapsed"] - entered["elapsed"]
    stage_profiles.append({"run": directory.name,
        "scope": "Inclusive wall-clock intervals; nested stages must not be summed together.",
        "completed_stages": profiles,
        "unfinished_stages": {name: len(values) for name, values in pending.items() if values}})
    rows.append({
        "run": directory.name, "problem": source.get("problem"),
        "rows": source.get("training_rows"), "evaluation_rows": source.get("evaluation_rows"),
        "mode": source.get("mode"), "package": source.get("package"), "library": source.get("library"),
        "status": "timeout" if supervisor["timed_out"] else result.get("status", "process_failure"),
        "failure_origin": "harness" if harness_error else None,
        "failure_explanation": harness_error,
        "public_call_completed": checkpoint.get("status") == "public_call_returned",
        "public_call_seconds": result.get("public_call_seconds"),
        "wall_seconds": supervisor["wall_seconds"],
        "peak_rss_kib": int(match.group(1)) if match else supervisor.get("highest_recorded_rss_kib"),
        "peak_rss_source": "process peak from /usr/bin/time" if match else "last recorded VmHWM; lower bound if interrupted",
        "models": result.get("models"), "saved_bytes": result.get("saved_bytes"),
        "error": result.get("message"), "last_stage": (supervisor.get("last_stage") or {}).get("stage"),
        "metrics": result.get("metrics"),
        "configurations_attempted": result.get("configurations_attempted", len(candidates) if candidates else None),
        "configurations_failed": result.get("configurations_failed", failed_configurations if candidates else None),
        "families_resampling_failed": result.get("families_resampling_failed"),
        "families_refit_failed": result.get("families_refit_failed"),
        "cold_replay_status": cold.get("status"),
    })
(args.directory / "index.json").write_text(json.dumps(rows, indent=2) + "\n")
(args.directory / "stage-profiles.json").write_text(json.dumps(stage_profiles, indent=2) + "\n")
if rows:
    with (args.directory / "index.csv").open("w") as file:
        fields = [key for key in rows[0] if key not in {"metrics", "configurations_failed", "families_resampling_failed", "families_refit_failed"}]
        writer = csv.DictWriter(file, fields, extrasaction="ignore")
        writer.writeheader()
        writer.writerows(rows)
print(json.dumps(rows, indent=2))
