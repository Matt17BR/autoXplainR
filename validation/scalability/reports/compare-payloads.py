"""Compare old and compact reports made from the same fitted-model result."""
import argparse
import hashlib
import json
from pathlib import Path
import re
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))
from report_payload import decode_data_payload, decode_prediction_payload

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("baseline", type=Path)
parser.add_argument("candidate", type=Path)
parser.add_argument("--output", type=Path, required=True)
args = parser.parse_args()


def read(path, name):
    html = path.read_text()
    return json.loads(re.search(r'<script[^>]*id="' + name + r'"[^>]*>(.*?)</script>', html, re.S)[1])


def digest(value):
    return hashlib.sha256(json.dumps(value, sort_keys=True, separators=(",", ":")).encode()).hexdigest()


before = decode_data_payload(read(args.baseline, "axr-data-payload"))
after = decode_data_payload(read(args.candidate, "axr-data-payload"))
checks = []


def check(name, first, second):
    checks.append(dict(name=name, passed=first == second,
                       baseline_sha256=digest(first), candidate_sha256=digest(second)))


check("Every exported source identity, raw value, processed value and nonfinite flag", before["rows"], after["rows"])
check("Complete export manifest", before["manifest"], after["manifest"])
for stage in before["profile"]["stages"]:
    a = before["profile"]["stages"][stage]["columns"]
    b = after["profile"]["stages"][stage]["columns"]
    for name in a:
        check(f"{stage}/{name}: exact whole-column statistics and bin definitions", a[name], b[name])
del before, after
before = decode_prediction_payload(read(args.baseline, "axr-predictions-payload"))
after = decode_prediction_payload(read(args.candidate, "axr-predictions-payload"))
check("All prediction diagnostics, cutoff counts, linked cases and exact model outputs", before, after)
result = dict(passed=all(item["passed"] for item in checks), checks=checks,
              scope="All exported rows, whole-column summaries and complete prediction payloads. Pair metadata intentionally changed to expose sampling and category replication, so it is tested separately.")
args.output.parent.mkdir(parents=True, exist_ok=True)
args.output.write_text(json.dumps(result, indent=2) + "\n")
print(json.dumps(dict(passed=result["passed"], checks=len(checks), failures=[item for item in checks if not item["passed"]]), indent=2))
raise SystemExit(0 if result["passed"] else 1)
