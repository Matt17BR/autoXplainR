"""Prove the browser gates reject three specific, reintroduced scale defects."""
import argparse
import json
from pathlib import Path
import shutil
import subprocess
import sys

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--folder", type=Path, required=True, help="Root containing candidate/2500 and compatibility fixtures")
args = parser.parse_args()
base = args.folder.resolve()
scripts = Path(__file__).resolve().parent
out = base / "negative-controls"
out.mkdir(parents=True, exist_ok=True)
controls = [
    dict(name="no-checksum", source=base / "candidate/2500", script="check-codec.py", result="codec/checks.json",
         original="verifyZlibChecksum(bytes, compressed);", replacement="/* Deliberately skip checksum verification. */",
         expected=["damaged-checksum"]),
    dict(name="unbounded-categories", source=base / "compatibility", script="check-categories.py", result="category-checks.json",
         original="if (visible.length < 200) visible.push(value);", replacement="visible.push(value);",
         expected=["category control bounds actual option creation"]),
    dict(name="ignored-pair-cap", source=base / "compatibility", script="check-sampled-relationships.py", result="sampled-pair-checks.json",
         original="const sampled = Number.isInteger(limit) && limit > 0 && population.length > limit;",
         replacement="const sampled = false; /* Deliberately ignore the requested cap. */",
         expected=["evaluation association agrees", "browser discloses sample", "sampled joint counts"]),
]
verdicts = []
for control in controls:
    folder = out / control["name"]
    folder.mkdir(exist_ok=True)
    shutil.copyfile(control["source"] / "source.json", folder / "source.json")
    for name in ["report.html", "all-pairs.html"]:
        source = control["source"] / name
        if not source.exists():
            continue
        html = source.read_text()
        assert html.count(control["original"]) == 1, control["name"] + " no longer matches its intended defect"
        (folder / name).write_text(html.replace(control["original"], control["replacement"], 1))
    with (folder / "runner.log").open("w") as log:
        process = subprocess.run([sys.executable, str(scripts / control["script"]), "--folder", str(folder)],
                                 stdout=log, stderr=subprocess.STDOUT, timeout=90)
    report = json.loads((folder / control["result"]).read_text())
    failures = [item["name"] for item in report["checks"] if not item["passed"]]
    correct = process.returncode == 1 and failures and all(
        any(expected in failure for expected in control["expected"]) for failure in failures)
    verdicts.append(dict(name=control["name"], rejected_for_expected_reason=bool(correct), failures=failures))
result = dict(passed=all(item["rejected_for_expected_reason"] for item in verdicts), controls=verdicts)
(out / "verdict.json").write_text(json.dumps(result, indent=2) + "\n")
print(json.dumps(result, indent=2))
raise SystemExit(0 if result["passed"] else 1)
