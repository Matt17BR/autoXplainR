"""Freeze a coherent package source, then record its separate installed library."""
import hashlib
import json
import os
from pathlib import Path
import shutil
import sys

mode = sys.argv[1]
assert mode in {"source", "installed"}
repository = Path(__file__).resolve().parents[3]
evidence = Path(__file__).resolve().parent
cache = Path(os.environ.get(
    "AXR_SEARCH_DIR", "~/.cache/autoxplain-scale-0.7.0/search"
)).expanduser()
source = cache / "candidate-source-binary-guard"
library = cache / "candidate-binary-guard-library"
roots = ("DESCRIPTION", "LICENSE", "LICENSE.md", "NAMESPACE", "NEWS.md", "R", "inst", "man")


def inventory(folder, names=None):
    files = []
    for name in names or (".",):
        path = folder / name
        files.extend([path] if path.is_file() else [item for item in path.rglob("*") if item.is_file()])
    return {str(path.relative_to(folder)): hashlib.sha256(path.read_bytes()).hexdigest()
            for path in sorted(files)}


if mode == "source":
    assert not source.exists(), "Preserve previous source snapshots"
    assert not library.exists() or not any(library.iterdir()), "Preserve previous installed libraries"
    expected = inventory(repository, roots)
    source.mkdir(parents=True)
    for name in roots:
        path = repository / name
        if path.is_dir():
            shutil.copytree(path, source / name)
        else:
            shutil.copyfile(path, source / name)
    actual = inventory(source)
    assert expected == actual == inventory(repository, roots), "Source changed during snapshot"
    library.mkdir(exist_ok=True)
    output = evidence / "candidate-binary-guard-source-sha256.json"
else:
    expected = json.loads((evidence / "candidate-binary-guard-source-sha256.json").read_text())
    assert expected == inventory(source), "Frozen source changed"
    actual = inventory(library / "AutoXplainR")
    assert actual, "Install the frozen source before recording its library"
    output = evidence / "candidate-binary-guard-installed-sha256.json"
assert not output.exists(), "Preserve existing provenance"
output.write_text(json.dumps(actual, indent=2) + "\n")
print(mode, len(actual), "files recorded in", output)
