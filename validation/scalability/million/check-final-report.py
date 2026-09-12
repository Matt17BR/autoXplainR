"""Check a final one-call report against original full-row cold predictions."""
import argparse
import hashlib
import json
import math
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))
from report_payload import decode_data_payload, decode_prediction_payload, read_json_payload

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("folder", type=Path, help="A completed controlled_full evidence directory")
folder = parser.parse_args().folder.resolve()
source = json.loads((folder / "input.json").read_text())
result = json.loads((folder / "result.json").read_text())
cold = json.loads((folder / "cold-replay.json").read_text())
html = (folder / "report.html").read_text()
data = decode_data_payload(read_json_payload(html, "axr-data-payload"))
predictions = decode_prediction_payload(read_json_payload(html, "axr-predictions-payload"))
checks = []


def check(name, passed, evidence=None):
    checks.append({"name": name, "passed": bool(passed), "evidence": evidence})


check("Cold process compares all original complete model prediction vectors",
      cold["full_prediction_vector_check"] == "passed")
check("Full public default report completed",
      result["status"] == "passed" and source["mode"] == "controlled_full")
check("Aggregate data export includes all source rows",
      data["mode"] == "summary" and
      data["manifest"]["full_rows"] == source["training_rows"] + source["evaluation_rows"])
check("Data explorer exports summaries without individual data rows",
      data["manifest"]["individual_records"] == 0 and not data["rows"])

for stage, profile in data["profile"]["stages"].items():
    check(stage + ": complete training/evaluation profile populations",
          profile["training_rows"] == source["training_rows"] and
          profile["evaluation_rows"] == source["evaluation_rows"])
    for column, details in profile["columns"].items():
        for partition in ("training", "evaluation"):
            values = details[partition]
            expected = source[partition + "_rows"]
            check(f"{stage}/{column}/{partition}: every univariate row counted",
                  values["n_total"] == expected and
                  sum(values["counts"]) == values["n_used"] and
                  values["n_used"] + values["n_missing"] + values["n_nonfinite"] == expected,
                  {key: values[key] for key in ("n_total", "n_used", "n_missing", "n_nonfinite")})

models = []
for model in predictions["models"]:
    model_id = model["model_id"]
    n = source["evaluation_rows"]
    regression = model["regression"]
    counts = {
        name: sum(item["n"] for item in regression[name])
        for name in ("density", "residual_histogram", "bias")
    }
    check(model_id + ": complete official prediction and chart-summary populations",
          model["n"] == n and all(count == n for count in counts.values()), counts)
    check(model_id + ": exact full-holdout RMSE in HTML",
          math.isclose(regression["metrics"][0], cold["losses"][model_id],
                       rel_tol=1e-12, abs_tol=1e-12),
          [regression["metrics"][0], cold["losses"][model_id]])
    models.append({
        "model_id": model_id, "evaluation_rows": model["n"],
        "rmse": regression["metrics"][0],
        "embedded_case_rows": len(model.get("cases") or []),
    })

check("Every retained model is present in the report",
      len(models) == result["models"] and
      {model["model_id"] for model in models} == set(cold["losses"]))
check("Default explanation settings and actual sampled rows retained",
      result["explanation_details"]["config"] == {
          "top_features": 8, "n_repeats": 20, "max_models": 5, "explanation_rows": 5000
      } and result["explanation_sampling"]["rows_used"] == 5000)
uncertainty = result["paired_uncertainty"]
check("All full-evaluation bootstrap draws executed",
      len(uncertainty) == 1 and
      uncertainty[0]["bootstrap_draws"] == uncertainty[0]["retained_draws"] == 1000 and
      uncertainty[0]["sampling_units"] == source["evaluation_rows"])

verdict = {
    "passed": all(item["passed"] for item in checks), "checks": checks, "models": models,
    "data_manifest": data["manifest"],
    "data_pair_sampling": data["profile"].get("pair_sampling"),
    "html_sha256": hashlib.sha256((folder / "report.html").read_bytes()).hexdigest(),
    "scope": "Independent Python decoding of the exact one-call HTML, compared with original "
             "full-row cold prediction losses and traced original-process scope. "
             "Browser interaction is checked separately.",
}
(folder / "report-payload-verdict.json").write_text(json.dumps(verdict, indent=2) + "\n")
print(json.dumps({
    "passed": verdict["passed"], "checks": len(checks),
    "failures": [item for item in checks if not item["passed"]],
}, indent=2))
raise SystemExit(0 if verdict["passed"] else 1)
