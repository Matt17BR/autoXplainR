#!/usr/bin/env python3
"""Run one declared benchmark in its own frozen, bounded R process."""
import argparse
import hashlib
import json
import math
import os
from pathlib import Path
import resource
import re
import shutil
import signal
import subprocess
import time
from native_reference_plan import validate_plan

parser = argparse.ArgumentParser()
parser.add_argument("case", choices=["yearprediction", "covertype", "bank", "friedman_noise", "rare_interaction"])
parser.add_argument("variant", choices=["package", "xgboost", "ranger"])
parser.add_argument("--cohort", required=True)
parser.add_argument("--phase", choices=["development", "acceptance"], default="development")
parser.add_argument("--library", default=str(Path.home() / ".cache/autoxplain-scale-0.7.0/release-verification/published-release-0.7.0/installed-library"))
parser.add_argument("--threads", type=int, choices=[1, 4], default=1)
parser.add_argument("--request", choices=["paired", "tabular", "public-tabular"], default="paired")
parser.add_argument("--freeze-manifest", default="")
parser.add_argument("--stage", choices=["fit-and-score", "fit-only", "score-only"], default="fit-and-score")
parser.add_argument("--fit-run", default="")
parser.add_argument("--native-reference-plan", default="",
                    help="Absolute fixed-native-forest-v1 JSON plan; ranger fit-only with four threads.")
args = parser.parse_args()
if not re.fullmatch(r"[a-zA-Z0-9][a-zA-Z0-9._-]*", args.cohort):
    raise SystemExit("Use a plain cohort name.")
if args.variant == "package" and args.stage != "fit-and-score":
    raise SystemExit("Separate fitting and scoring are supported only for the fixed native references.")
if bool(args.fit_run) != (args.stage == "score-only"):
    raise SystemExit("Supply --fit-run exactly when using --stage score-only.")
if args.request == "public-tabular" and (args.variant != "package" or args.threads != 4):
    raise SystemExit("The public tabular workflow is a package cohort with the four-thread supervisor ceiling.")
if args.native_reference_plan and (args.variant != "ranger" or args.stage != "fit-only" or args.threads != 4):
    raise SystemExit("A native reference plan requires ranger, --stage fit-only and --threads 4.")
cache = Path(os.environ.get("AXR_TABULAR_DIR", "~/.cache/autoxplain-tabular-0.8.0")).expanduser().resolve()
native_reference = None
if args.native_reference_plan:
    try:
        native_reference = validate_plan(args.native_reference_plan, cache, args.case, args.phase)
    except (ValueError, KeyError, TypeError, OSError) as error:
        raise SystemExit(str(error))
destination = cache / "runs" / args.cohort / args.case / args.variant
if destination.exists():
    raise SystemExit(f"Refusing to overwrite a prior run: {destination}")
if args.phase == "acceptance" and args.stage != "fit-only" and not args.freeze_manifest:
    raise SystemExit("Acceptance is locked until a candidate freeze manifest is explicitly supplied.")
destination.mkdir(parents=True)
script_source = Path(__file__).resolve().parent
script_destination = destination / "scripts"
script_destination.mkdir()
for file in script_source.iterdir():
    if file.is_file() and (file.suffix in {".R", ".py", ".md"}
                           or file.name == "fixed-native-forest-v1.json"):
        shutil.copy2(file, script_destination / file.name)


def digest(path):
    # Native forests can occupy several GiB. Hash without copying an entire
    # model into the supervisor, outside the bounded R process.
    result = hashlib.sha256()
    with path.open("rb") as source:
        for block in iter(lambda: source.read(1024 * 1024), b""):
            result.update(block)
    return result.hexdigest()


def write_process_record(value):
    temporary = destination / "process.json.tmp"
    temporary.write_text(json.dumps(value, indent=2) + "\n")
    temporary.replace(destination / "process.json")


def finite_nonnegative(value, label):
    if (isinstance(value, bool) or not isinstance(value, (int, float))
            or not math.isfinite(value) or value < 0):
        raise SystemExit(f"{label} must be a finite nonnegative number.")
    return value


def observed_rss(record):
    return max([finite_nonnegative(record[key], key)
                for key in ("peak_rss_kib", "sampled_child_high_water_rss_kib") if key in record] or [0])


if native_reference is not None:
    plan_copy = destination / "native-reference-plan.json"
    shutil.copyfile(args.native_reference_plan, plan_copy)
    if digest(plan_copy) != native_reference["plan_sha256"]:
        raise SystemExit("The native reference plan changed while being copied.")
    args.native_reference_plan = str(plan_copy)


combined_limit = 1200 if args.phase == "development" else 7200
limit = combined_limit
fit_record = None
fit_directory = None
previous_scoring_seconds = 0
previous_scoring_peak_rss_kib = 0
if args.stage == "score-only":
    fit_directory = Path(args.fit_run).expanduser().resolve()
    fit_record = json.loads((fit_directory / "process.json").read_text())
    fit_summary = json.loads((fit_directory / "summary.json").read_text())
    expected = {"case": args.case, "variant": args.variant, "phase": args.phase,
                "threads": args.threads, "stage": "fit-only", "process_status": "ok"}
    if any(fit_record.get(key) != value for key, value in expected.items()):
        raise SystemExit("The source must be a successful matching native fit-only process.")
    if fit_summary.get("status") != "ok" or "metrics" in fit_summary:
        raise SystemExit("The source is not a completed, unscored native fit.")
    if fit_record["partitions_sha256"] != digest(cache / "partitions.json"):
        raise SystemExit("The source fit used a different partition manifest.")
    if fit_summary.get("saved_model_sha256") != digest(fit_directory / "model.rds"):
        raise SystemExit("The saved native fit changed after fitting.")
    if fit_record.get("native_reference_protocol"):
        if (fit_record.get("native_reference_protocol") != "fixed-native-forest-v1"
                or fit_record.get("summary_sha256") != digest(fit_directory / "summary.json")
                or fit_record.get("native_reference_plan_sha256") != digest(fit_directory / "native-reference-plan.json")
                or fit_record.get("native_reference_plan_sha256") != fit_summary.get("native_reference_plan_sha256")
                or fit_summary.get("native_reference_protocol") != "fixed-native-forest-v1"
                or fit_summary.get("native_verified_tree_count") != 500
                or fit_summary.get("native_verified_training_rows") != fit_record["native_reference"]["entry"]["full_training_rows"]
                or fit_summary.get("evaluation_files_opened") is not False):
            raise SystemExit("The saved fixed native reference identity or completion evidence changed.")
    fit_record_hash = digest(fit_directory / "process.json")
    fit_elapsed_seconds = finite_nonnegative(fit_record.get("process_elapsed_seconds"),
                                            "Source fit elapsed seconds")
    fit_peak_rss_kib = observed_rss(fit_record)
    for prior_path in (cache / "runs").glob(f"*/{args.case}/{args.variant}/process.json"):
        prior = json.loads(prior_path.read_text())
        if prior.get("stage") != "score-only" or prior.get("fit_source_process_sha256") != fit_record_hash:
            continue
        if prior.get("process_status", "running") == "running":
            raise SystemExit("Another scoring process for this exact native fit is still running.")
        previous_scoring_seconds += finite_nonnegative(prior.get("process_elapsed_seconds"),
                                                      "Earlier scoring process elapsed seconds")
        previous_scoring_peak_rss_kib = max(previous_scoring_peak_rss_kib, observed_rss(prior))
    limit -= fit_elapsed_seconds
    limit -= previous_scoring_seconds
    if limit <= 0:
        raise SystemExit("The native reference exhausted its combined fitting and scoring budget.")
    (destination / "model.rds").symlink_to(os.path.relpath(fit_directory / "model.rds", destination))
record = {"case": args.case, "variant": args.variant, "phase": args.phase,
          "cohort": args.cohort, "threads": args.threads, "request": args.request, "stage": args.stage,
          "wall_limit_seconds": limit, "address_space_limit_bytes": 24 * 1024**3,
          "combined_wall_limit_seconds": combined_limit,
          "started_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
          "scripts": {f.name: digest(f) for f in script_destination.iterdir()},
          "partitions_sha256": digest(cache / "partitions.json"),
          "git_head": subprocess.check_output(["git", "rev-parse", "HEAD"], text=True).strip(),
          "git_diff_sha256": hashlib.sha256(subprocess.check_output(["git", "diff"])).hexdigest()}
if native_reference is not None:
    record["native_reference_protocol"] = native_reference["protocol"]
    record["native_reference_plan_sha256"] = native_reference["plan_sha256"]
    record["native_reference"] = native_reference
if fit_record is not None:
    record["fit_source"] = str(fit_directory)
    record["fit_source_process_sha256"] = digest(fit_directory / "process.json")
    record["fit_source_model_sha256"] = digest(fit_directory / "model.rds")
    record["fit_source_summary_sha256"] = digest(fit_directory / "summary.json")
    record["fit_process_elapsed_seconds"] = fit_record["process_elapsed_seconds"]
    record["previous_scoring_process_seconds"] = previous_scoring_seconds
    for key in ("native_reference_protocol", "native_reference_plan_sha256", "native_reference"):
        if key in fit_record:
            record[key] = fit_record[key]
if args.variant == "package":
    package = Path(args.library) / "AutoXplainR"
    if not (package / "DESCRIPTION").is_file():
        raise SystemExit(f"The requested private package installation is absent: {package}")
    record["installed_package_files"] = {str(f.relative_to(package)): digest(f) for f in sorted(package.rglob("*")) if f.is_file()}
if args.phase == "acceptance" and args.stage != "fit-only":
    frozen = json.loads(Path(args.freeze_manifest).read_text())
    partitions = json.loads((cache / "partitions.json").read_text())
    if (frozen.get("acceptance_authorized") is not True
            or frozen.get("partitions_sha256") != record["partitions_sha256"]
            or frozen.get("protocol_sha256") != partitions["protocol_sha256"]
            or not frozen.get("candidate_source_sha256")):
        raise SystemExit("Acceptance manifest does not match the declared source, protocol and partitions.")
    declared_protocols = frozen.get("protocol_files", {})
    required_protocols = {"README.md", "additional-cohorts.md", "forest-family-acceptance.md",
                          "covertype-multicore.md", "native-staging.md", "staged-scoring-retries.md",
                          "forest-validation-budget-v2.md", "boosting-anchor-amendment-v2.md",
                          "public-one-call.md", "forest-tree-budget-v3.md",
                          "fixed-native-forest-v1.md", "fixed-native-forest-v1.json",
                          "native-bank-original-1t-20260913.md"}
    if not required_protocols.issubset(declared_protocols):
        raise SystemExit("The acceptance freeze must include every required protocol amendment.")
    if declared_protocols["README.md"] != frozen["protocol_sha256"]:
        raise SystemExit("The original protocol changed; record changes as explicit amendments.")
    for name, expected in declared_protocols.items():
        if name != Path(name).name or record["scripts"].get(name) != expected:
            raise SystemExit(f"The frozen protocol or amendment changed: {name}")
    matching = [entry for entry in frozen.get("allowed_runs", [])
                if entry.get("case") == args.case and entry.get("variant") == args.variant
                and entry.get("threads") == args.threads and entry.get("request", "paired") == args.request
                and entry.get("stage") == args.stage and entry.get("cohort") == args.cohort]
    if len(matching) != 1:
        raise SystemExit("This exact acceptance case, learner variant, thread count, request, stage and cohort were not authorized.")
    if args.variant == "package" and matching[0].get("package_files") != record["installed_package_files"]:
        raise SystemExit("The installed package differs from the frozen acceptance candidate.")
    if args.stage == "score-only" and (
            matching[0].get("fit_process_sha256") != record["fit_source_process_sha256"]
            or matching[0].get("fit_model_sha256") != record["fit_source_model_sha256"]
            or matching[0].get("fit_summary_sha256") != record["fit_source_summary_sha256"]):
        raise SystemExit("The saved native fit differs from the frozen acceptance reference.")
    frozen_path = destination / "acceptance-freeze.json"
    shutil.copyfile(args.freeze_manifest, frozen_path)
    record["acceptance_freeze_sha256"] = digest(frozen_path)
    args.freeze_manifest = str(frozen_path)
command = ["/usr/bin/time", "-v", "-o", str(destination / "resource-usage.txt"),
           "Rscript", "--vanilla", str(script_destination / "run-one.R"), args.case,
           args.variant, args.phase, str(destination), args.library, str(args.threads), args.freeze_manifest,
           args.request, args.stage, str(fit_directory) if fit_directory is not None else "",
           args.native_reference_plan, args.cohort]
record["command"] = command
write_process_record(record)
environment = dict(os.environ)
environment.update({"OMP_NUM_THREADS": str(args.threads), "OPENBLAS_NUM_THREADS": str(args.threads),
                    "MKL_NUM_THREADS": str(args.threads), "AXR_TABULAR_DIR": str(cache)})


def bounds():
    resource.setrlimit(resource.RLIMIT_AS, (24 * 1024**3, 24 * 1024**3))


def observed_process_memory(root_pid):
    """Sample the native R child's high-water mark, including on timeout."""
    pending = [root_pid]
    largest_hwm = 0
    while pending:
        pid = pending.pop()
        try:
            children = Path(f"/proc/{pid}/task/{pid}/children").read_text().split()
            pending.extend(int(child) for child in children)
            lines = Path(f"/proc/{pid}/status").read_text().splitlines()
            for line in lines:
                if line.startswith("VmHWM:"):
                    largest_hwm = max(largest_hwm, int(line.split()[1]))
        except (FileNotFoundError, ProcessLookupError):
            pass
    return largest_hwm


started = time.monotonic()
with (destination / "process.log").open("w") as log:
    child = subprocess.Popen(command, stdout=log, stderr=subprocess.STDOUT,
                             env=environment, start_new_session=True, preexec_fn=bounds)
    record["supervised_pid"] = child.pid
    record["process_status"] = "running"
    write_process_record(record)
    sampled_hwm = 0
    last_checkpoint = started
    try:
        while child.poll() is None:
            sampled_hwm = max(sampled_hwm, observed_process_memory(child.pid))
            if time.monotonic() - last_checkpoint >= 10:
                record["sampled_child_high_water_rss_kib"] = sampled_hwm
                record["last_observed_elapsed_seconds"] = time.monotonic() - started
                record["rss_sampling_seconds"] = 1
                write_process_record(record)
                last_checkpoint = time.monotonic()
            remaining = limit - (time.monotonic() - started)
            if remaining <= 0:
                raise subprocess.TimeoutExpired(command, limit)
            try:
                child.wait(timeout=min(1, remaining))
            except subprocess.TimeoutExpired:
                pass
        code = child.returncode
        record["process_status"] = "ok" if code == 0 else "failed"
    except subprocess.TimeoutExpired:
        record["process_status"] = "timeout"
        os.killpg(child.pid, signal.SIGTERM)
        try:
            child.wait(timeout=5)
        except subprocess.TimeoutExpired:
            os.killpg(child.pid, signal.SIGKILL)
            child.wait()
        code = child.returncode
record["exit_code"] = code
record["sampled_child_high_water_rss_kib"] = sampled_hwm
record["rss_sampling_seconds"] = 1
record["process_elapsed_seconds"] = time.monotonic() - started
if fit_record is not None:
    record["combined_process_elapsed_seconds"] = (record["process_elapsed_seconds"]
        + fit_record["process_elapsed_seconds"] + previous_scoring_seconds)
record["ended_at"] = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
if (destination / "summary.json").is_file():
    record["summary_sha256"] = digest(destination / "summary.json")
usage_path = destination / "resource-usage.txt"
if usage_path.exists():
    for line in usage_path.read_text().splitlines():
        if "Maximum resident set size (kbytes):" in line:
            record["peak_rss_kib"] = int(line.split(":")[-1])
if fit_record is not None:
    record["combined_observed_high_water_rss_kib"] = max(
        observed_rss(record), fit_peak_rss_kib, previous_scoring_peak_rss_kib)
write_process_record(record)
print(json.dumps({key: value for key, value in record.items() if key in {
    "case", "variant", "cohort", "process_status", "process_elapsed_seconds", "peak_rss_kib"}}, indent=2), flush=True)
raise SystemExit(0 if record["process_status"] == "ok" else 1)
