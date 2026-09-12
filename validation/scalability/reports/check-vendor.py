"""Verify the pinned offline decoder and the license embedded in each report."""
import hashlib
import json
from pathlib import Path

root = Path(__file__).resolve().parents[3]
folder = root / "inst/report"
manifest = json.loads((folder / "fflate-source.json").read_text())
asset = (folder / "fflate-0.8.3.js").read_bytes()
license_text = (folder / "fflate-LICENSE.txt").read_text().strip()
header, upstream = asset.split(b"*/\n", 1)
assert manifest["version"] == "0.8.3"
assert hashlib.sha256(upstream).hexdigest() == manifest["upstream_file_sha256"]
assert license_text in header.decode(), "Reports must embed the complete MIT license with the decoder"
assert b"</script" not in asset.lower(), "An embedded asset must not terminate its surrounding script element"
print(json.dumps(dict(passed=True, version=manifest["version"],
                      upstream_sha256=manifest["upstream_file_sha256"], full_license_embedded=True)))
