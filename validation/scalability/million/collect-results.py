"""Collect the generated run indexes without dropping failed attempts."""
import argparse
import hashlib
import json
from pathlib import Path


parser = argparse.ArgumentParser()
parser.add_argument("cache", type=Path)
parser.add_argument("output", type=Path)
args = parser.parse_args()
runs = []
sources = []
for series in ["baseline", *sorted(path.name for path in args.cache.glob("candidate-v*"))]:
    index = args.cache / series / "index.json"
    if not index.is_file():
        continue
    manifest = args.cache / f"candidate-source-{series.removeprefix('candidate-')}-manifest.json"
    if manifest.is_file():
        sources.append({"series": series, "source_manifest": manifest.name,
                        "manifest_sha256": hashlib.sha256(manifest.read_bytes()).hexdigest()})
    for item in json.loads(index.read_text()):
        row = dict(item)
        row.pop("library", None)
        row["series"] = series
        row["evidence_directory"] = f"~/.cache/autoxplain-scale-0.7.0/{series}/{row['run']}"
        row["library_role"] = "published0.6.2" if series == "baseline" else series
        runs.append(row)
answer = {
    "scope": "Completed and failed staged measurements. Single Linux host, one native thread, "
             "external process bounds. Missing cases are not implied to have passed.",
    "published_baseline_sha256": "e15ee291f447414a16e10acce0f69cca826d7e58b424f5597c41ea9f56114624",
    "candidate_source_manifests": sources,
    "runs": runs,
}
args.output.write_text(json.dumps(answer, indent=2) + "\n")
print(f"Recorded {len(runs)} attempts, including failures, in {args.output}")
