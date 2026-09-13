#!/usr/bin/env python3
"""Copy an immutable development source snapshot and install it privately."""
import argparse
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess

parser = argparse.ArgumentParser()
parser.add_argument("name")
args = parser.parse_args()
if not re.fullmatch(r"[a-zA-Z0-9][a-zA-Z0-9._-]*", args.name):
    raise SystemExit("Use a plain snapshot name.")
repository = Path(__file__).resolve().parents[2]
cache = Path(os.environ.get("AXR_TABULAR_DIR", "~/.cache/autoxplain-tabular-0.8.0")).expanduser()
destination = cache / "candidates" / args.name
if destination.exists():
    raise SystemExit(f"Snapshot already exists: {destination}")
source = destination / "source"
library = destination / "library"
source.mkdir(parents=True)
library.mkdir()
rules = [re.compile(line) for line in (repository / ".Rbuildignore").read_text().splitlines() if line]


def ignored(relative):
    prefixes = [str(parent) for parent in Path(relative).parents if str(parent) != "."] + [relative]
    return any(rule.search(prefix) for rule in rules for prefix in prefixes)


def source_files():
    paths = subprocess.check_output(
        ["git", "ls-files", "--cached", "--others", "--exclude-standard", "-z"],
        cwd=repository).decode().split("\0")
    return sorted(set(path for path in paths
                      if path and (repository / path).is_file() and not ignored(path)))


files = source_files()


def inventory(root):
    return {file: hashlib.sha256((root / file).read_bytes()).hexdigest() for file in files}


before = inventory(repository)
for file in files:
    target = source / file
    target.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(repository / file, target)
copied = inventory(source)
after_files = source_files()
after = {file: hashlib.sha256((repository / file).read_bytes()).hexdigest() for file in after_files}
if before != copied or copied != after:
    changed = [file for file in sorted(set(files) | set(after_files))
               if before.get(file) != copied.get(file) or copied.get(file) != after.get(file)]
    (destination / "copy-failed.json").write_text(json.dumps({"changed_files": changed}, indent=2) + "\n")
    raise SystemExit("Source changed during snapshot; use a fresh name and retry: " + ", ".join(changed))
record = {"name": args.name, "created_at": datetime.now(timezone.utc).isoformat(),
          "git_head": subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=repository, text=True).strip(),
          "source_files": copied,
          "source_inventory_sha256": hashlib.sha256(json.dumps(copied, sort_keys=True, separators=(",", ":")).encode()).hexdigest(),
          "source": str(source), "library": str(library), "source_unchanged_during_copy": True,
          "scope": "Development snapshot. Documentation may still describe the previous release; this is not a release artifact."}
(destination / "snapshot.json").write_text(json.dumps(record, indent=2) + "\n")
with (destination / "install.log").open("w") as log:
    installed = subprocess.run(["R", "CMD", "INSTALL", "--no-multiarch", f"--library={library}", str(source)],
                               stdout=log, stderr=subprocess.STDOUT, check=False)
record["install_exit_code"] = installed.returncode
if installed.returncode == 0:
    package = library / "AutoXplainR"
    record["installed_package_files"] = {str(file.relative_to(package)): hashlib.sha256(file.read_bytes()).hexdigest()
                                         for file in sorted(package.rglob("*")) if file.is_file()}
(destination / "snapshot.json").write_text(json.dumps(record, indent=2) + "\n")
print(json.dumps({key: record[key] for key in ["name", "source_inventory_sha256", "source_unchanged_during_copy", "install_exit_code", "library"]}, indent=2))
raise SystemExit(installed.returncode)
