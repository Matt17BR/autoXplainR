#!/usr/bin/env python3
"""Cache official UCI archives, preserve their hashes, and extract named files."""
from pathlib import Path
import hashlib
import json
import os
import shutil
import urllib.request
import zipfile


CACHE = Path(os.environ.get("AXR_TABULAR_DIR", "~/.cache/autoxplain-tabular-0.8.0")).expanduser()
RAW = CACHE / "raw"
RAW.mkdir(parents=True, exist_ok=True)
SOURCES = {
    "yearprediction": ("https://archive.ics.uci.edu/static/public/203/yearpredictionmsd.zip", "YearPredictionMSD.txt"),
    "covertype": ("https://archive.ics.uci.edu/static/public/31/covertype.zip", "covtype.data.gz"),
    "bank": ("https://archive.ics.uci.edu/static/public/222/bank+marketing.zip", "bank-additional.zip"),
}


def sha256(path):
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1 << 20), b""):
            digest.update(block)
    return digest.hexdigest()


manifest_path = RAW / "sources.json"
previous = json.loads(manifest_path.read_text()) if manifest_path.exists() else {}
manifest = {}
for name, (url, member) in SOURCES.items():
    archive = RAW / f"{name}.zip"
    if not archive.exists():
        # Reuse the previously licensed UCI download for Bank when available.
        bank_cache = Path.home() / ".cache/autoxplain-stress-0.6.2/benchmark/uci-bank/bank-marketing.zip"
        if name == "bank" and bank_cache.exists():
            shutil.copyfile(bank_cache, archive)
        else:
            temporary = archive.with_suffix(".partial")
            print(f"Downloading {name} from {url}", flush=True)
            with urllib.request.urlopen(url, timeout=180) as response, temporary.open("wb") as output:
                shutil.copyfileobj(response, output, length=1 << 20)
            temporary.replace(archive)
    digest = sha256(archive)
    if name in previous and digest != previous[name]["archive_sha256"]:
        raise RuntimeError(f"Cached archive changed: {name}")
    with zipfile.ZipFile(archive) as source:
        if member not in source.namelist():
            raise RuntimeError(f"Expected member {member} is absent: {source.namelist()}")
        extracted = RAW / member
        if extracted.exists() and name in previous:
            if sha256(extracted) != previous[name]["member_sha256"]:
                raise RuntimeError(f"Cached extracted source changed: {name}")
        else:
            temporary = extracted.with_suffix(extracted.suffix + ".partial")
            with source.open(member) as input_stream, temporary.open("wb") as output:
                shutil.copyfileobj(input_stream, output, length=1 << 20)
            temporary.replace(extracted)
    if name == "bank":
        nested_member = "bank-additional/bank-additional-full.csv"
        nested_info = "bank-additional/bank-additional-names.txt"
        with zipfile.ZipFile(extracted) as source:
            for entry in (nested_member, nested_info):
                target = RAW / Path(entry).name
                with source.open(entry) as input_stream, target.open("wb") as output:
                    shutil.copyfileobj(input_stream, output)
    manifest[name] = {
        "url": url, "archive_sha256": digest, "archive_bytes": archive.stat().st_size,
        "member": member, "member_sha256": sha256(extracted), "member_bytes": extracted.stat().st_size,
        "license": "CC BY 4.0",
    }
    manifest_path.write_text(json.dumps(manifest | {k: v for k, v in previous.items() if k not in manifest}, indent=2) + "\n")
    print(f"Verified {name}: {digest}", flush=True)
