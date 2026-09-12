"""Keep portable report benchmark measurements; leave large HTML files in the cache."""
import argparse
import hashlib
import json
from pathlib import Path
import re

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--cache", type=Path, required=True)
parser.add_argument("--output", type=Path, required=True)
parser.add_argument("--candidate-dir", type=Path)
args = parser.parse_args()
cache = args.cache.resolve()
measurements = []
for variant, cases, base in [("baseline", ("wide-500", "200000"), cache),
                              ("candidate", ("wide-500", "200000", "1000000"), args.candidate_dir or cache / "final")]:
    for case in cases:
        folder = base / variant / case
        value = json.loads((folder / "measurement.json").read_text())
        source = value.pop("source_md5", None)
        if source:
            # Preserve the exact source, with portable paths within its snapshot.
            normalized = {}
            for name, fingerprint in source.items():
                path = Path(name)
                root = next(parent for parent in path.parents if parent.name.startswith("source-final-"))
                normalized[str(path.relative_to(root))] = fingerprint
            value["source_md5"] = normalized
        report = folder / "report.html"
        value["html_sha256"] = hashlib.sha256(report.read_bytes()).hexdigest()
        log = base / (f"{variant}-{case}.time" if variant == "candidate" else f"{variant}-{case}.log")
        match = re.search(r"Maximum resident set size \(kbytes\):\s*(\d+)", log.read_text())
        value["process_peak_rss_kib"] = int(match[1]) if match else None
        measurements.append(value)

result = dict(
    measurements=measurements,
    scope="Each is one measured run. The wide case reuses an unchanged fitted result; the large-row cases fit on 100 training rows and score every evaluation row. These are reporting measurements, not million-row training benchmarks.",
    explanation_scope="Full rendering uses each version's default explanation policy. Candidate reference explanations use at most 5000 rows; original predictive scores use every evaluation row. Separate preparation/serialization timings isolate data export from explanation work.",
    units="Byte counts are exact; KiB is 1024 bytes. Process peak RSS comes from /usr/bin/time -v and includes loading, source fixture construction, standalone preparation/serialization, and complete rendering.",
)
args.output.parent.mkdir(parents=True, exist_ok=True)
args.output.write_text(json.dumps(result, indent=2) + "\n")
print(json.dumps([dict(case=x["case"], variant=x["variant"], render_seconds=x["render_seconds"],
                       html_bytes=x["html_bytes"], process_peak_rss_kib=x["process_peak_rss_kib"])
                  for x in measurements], indent=2))
