"""Export every retained score and the paired public-search timings."""
import csv
import json
import os
from pathlib import Path

cache = Path(os.environ.get(
    "AXR_SEARCH_DIR", "~/.cache/autoxplain-scale-0.7.0/search"
)).expanduser()
destination = Path(__file__).resolve().parent
cases = ("friedman_noise", "bank_marketing")
variants = ("baseline", "candidate")
if all((cache / "recommended" / "candidate_fixed_gam" / case /
        "summary.json").exists() for case in cases):
    variants += ("candidate_fixed_gam",)
summaries, models, families, candidates, failures = [], [], [], [], []
for case in cases:
    identity = None
    for variant in variants:
        folder = cache / "recommended" / variant / case
        record = json.loads((folder / "summary.json").read_text())
        checkpoint = json.loads((folder / "fit-timer.json").read_text())
        assert record["status"] == "ok", record
        assert checkpoint["fit_seconds"] == record["fit_seconds"]
        this_identity = (record["case_sha256"], record["seed"])
        if identity is None:
            identity = this_identity
        assert identity == this_identity, "Compared runs must share data, folds and seed"
        primary = record["metrics"][record["primary"]]
        metric = "rmse" if case == "friedman_noise" else "log_loss"
        summaries.append(dict(
            case=case, variant=variant, package_version=record["package_version"],
            fit_seconds=record["fit_seconds"], configurations=record["configuration_count"],
            successful_configurations=record["configuration_status"].get("ok", 0),
            retained_models=record["model_count"],
            selected_configuration=record["selected_configuration"],
            primary_metric=metric, primary_score=primary[metric],
            warnings=json.dumps(record["warnings"]), case_sha256=record["case_sha256"],
        ))
        for model, scores in record["metrics"].items():
            for name, value in scores.items():
                models.append(dict(case=case, variant=variant, model=model,
                                   primary=model == record["primary"], metric=name, score=value))
        for row in record["fold_fit_seconds_by_family"]:
            families.append(dict(case=case, variant=variant, family=row["family"],
                                 fold_fit_seconds=row["x"]))
        candidates.extend(dict(case=case, variant=variant, **row)
                          for row in record["candidates"])
        failures.extend(dict(case=case, variant=variant, **row)
                        for row in record["failed_folds"])

for name, rows in (("recommended-comparison.csv", summaries),
                   ("recommended-retained-scores.csv", models),
                   ("recommended-family-times.csv", families),
                   ("recommended-configurations.csv", candidates),
                   ("recommended-failed-folds.csv", failures)):
    if not rows:
        continue
    with (destination / name).open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]), lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)
    print(name, len(rows), "rows")
