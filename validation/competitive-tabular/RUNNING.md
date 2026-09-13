# Reproducing a development comparison

Run from the repository root. Raw data and large outputs stay in the cache;
set `AXR_TABULAR_DIR` to use another location. Preparation intentionally refuses
to replace an existing partition manifest. Install `data.table`, `jsonlite`,
`digest`, `ranger` and the declared XGBoost version first.

On another machine, install the [published 0.7.0 source archive](https://github.com/Matt17BR/autoXplainR/releases/download/v0.7.0/AutoXplainR_0.7.0.tar.gz)
into a private library, with the package's dependencies already installed:

```r
benchmark_cache <- path.expand(Sys.getenv("AXR_TABULAR_DIR", "~/.cache/autoxplain-tabular-0.8.0"))
baseline_library <- file.path(benchmark_cache, "baseline", "library")
dir.create(baseline_library, recursive = TRUE, showWarnings = FALSE)
archive <- file.path(benchmark_cache, "baseline", "AutoXplainR_0.7.0.tar.gz")
download.file("https://github.com/Matt17BR/autoXplainR/releases/download/v0.7.0/AutoXplainR_0.7.0.tar.gz",
  archive, mode = "wb")
stopifnot(digest::digest(file = archive, algo = "sha256") ==
  "bc6ad22bee49a9a3fee2ba7a092d975372815e07629295b3a012871782ac2532")
install.packages(archive, repos = NULL, type = "source", lib = baseline_library)
```

Pass that absolute `baseline_library` path to package runs with `--library`.
The default path in `run.py` points to the archived installation used for the
measurements in this repository; it is not expected to exist on another machine.

```sh
python3 validation/competitive-tabular/download.py
Rscript --vanilla validation/competitive-tabular/prepare.R
Rscript --vanilla validation/competitive-tabular/check-harness.R
python3 validation/competitive-tabular/run.py bank package --cohort baseline-development-v1 --library /absolute/baseline/library
python3 validation/competitive-tabular/run.py bank xgboost --cohort native-development-v2
python3 validation/competitive-tabular/run.py bank ranger --cohort native-development-v2
python3 validation/competitive-tabular/collect.py
```

Use a new cohort name for a repeat; a process directory is never overwritten.
If a recorded process disappears without completion, independently inspect its
PID and the live process inventory before recording an interruption. The
[13 September recovery record](interruption-recovery-20260913.md) describes the
preserved originals and separate evidence sidecars. A stale `running` record
alone does not justify a restart or establish a timeout.
The `package` variant defaults to the archived 0.7.0 private installation,
not the working tree or the default R library. Pass `--library /absolute/path`
for an explicitly installed candidate. Each process copies the protocol and
scripts before starting, records the installed package file hashes, and runs
under wall-time and address-space bounds. A process timeout remains a timeout
even if some candidate folds completed. Native reference settings are in the
protocol, not chosen from the evaluation results.

The selected native model, complete package result and full held-out
predictions are saved. `replay.R` reads only evaluation features and reproduces
every retained prediction in a fresh `Rscript --vanilla` process:

```sh
Rscript --vanilla validation/competitive-tabular/replay.R /absolute/run/directory /absolute/package/library
```

`collect.py` copies compact development records, source hashes and partition
hashes into this directory. It never opens acceptance labels. The controlled
Friedman and rare-interaction cases were previously inspected and are useful
regression checks, not fresh generalization evidence.
Their fixed generators are included in `controlled-cases.R`; no earlier cache
is required. `controlled-reproduction.json` records that regenerating them
reproduced all six frozen partition files byte for byte on the declared R runtime.

Create an isolated development candidate before fitting it:

```sh
python3 validation/competitive-tabular/freeze-candidate.py candidate-v2
python3 validation/competitive-tabular/run.py bank package --cohort candidate-paired-1t-v2 --library /absolute/cache/candidates/candidate-v2/library
python3 validation/competitive-tabular/run.py bank package --cohort candidate-tabular-4t-v2 --library /absolute/cache/candidates/candidate-v2/library --request tabular --threads 4
Rscript --vanilla validation/competitive-tabular/check-evidence.R /absolute/run/directory
```

The snapshot helper checks source contents and the file inventory before and
after copying, installs privately, and records both source and installed-file
hashes. A failed copy or installation remains on disk for diagnosis; use a new
snapshot name after fixing it. A candidate can still carry the previous version
number during development, so identify it by its snapshot and hashes.

The `paired` request uses the published forest/boosting request above. The
separate `tabular` request uses the new regularized/forest/boosting portfolio,
its default 18 settings and implicit selection rule. Both currently supply
the declared five folds and disable explanations to measure fitting. Neither
fit-only request establishes complete one-call report usability. One-thread
and four-thread runs occupy separate cohorts; added threads are not an
algorithmic speedup. See [additional cohorts](additional-cohorts.md).

The separate `public-tabular` request exercises the actual default one-call
workflow, including automatic threads, generated folds, default explanations
and HTML generation. It provides no fitting or explanation overrides. The
four-thread option below sets the supervisor's ceiling; it is not passed to
`autoxplain`. Record the resolved automatic choice from the result.

```sh
python3 validation/competitive-tabular/run.py bank package --cohort public-default-development-v1 --library /absolute/cache/candidates/final/library --request public-tabular --threads 4
```

See [the public-workflow amendment](public-one-call.md). The complete call,
report, predictions and saved result share the same process time limit.

Acceptance fitting remains locked pending an explicit frozen candidate
manifest. The manifest must authorize the exact case, variant, public request,
thread count and installed package. Record every protocol amendment, including
the [separate forest requirement](forest-family-acceptance.md), before opening
acceptance outcomes. A timeout preserves the process log and sampled memory
high-water mark; it does not become a successful run when some fits completed.

Native fitting can run separately from locked evaluation scoring under the
[staging amendment](native-staging.md). Verify the separation with:

```sh
python3 validation/competitive-tabular/check-native-staging.py
```

This creates an isolated development cache with no evaluation files, fits both
native engines, then compares staged scoring with ordinary joint runs. It also
checks fresh-session predictions, an exhausted combined budget, and the locked
acceptance guard. Use a new `--name` to repeat it without replacing evidence.

After the declared resource check and development runs complete, native
full-training references can use `--phase acceptance --stage fit-only`.
This explicit stage reads only training data and needs no evaluation unlock.
Later use a new cohort with `--stage score-only --fit-run /absolute/fit/run`
and `--freeze-manifest /absolute/final-freeze.json`. The matching `allowed_runs`
entry must include `fit_process_sha256`, `fit_summary_sha256` and
`fit_model_sha256`. Scoring receives the time remaining from the original
combined limit. Its model path is a relative symlink to the immutable fit.

`collect.py` publishes these fitting records separately in
`native-training-results.json`. They contain training-calibration evidence and
resource measurements, never locked evaluation scores.

The separately declared [fixed native forest](fixed-native-forest-v1.md) uses
the exact archived development calibration winners in its JSON mapping. With
those source process/summary files present and hash-matched, run one full
fit-only reference at a time:

```sh
python3 validation/competitive-tabular/check-fixed-native-reference.py --name fixed-native-reference-check-v4
python3 validation/competitive-tabular/run.py bank ranger --cohort native-fixed500-full-fit-4t-v1 --phase acceptance --threads 4 --stage fit-only --native-reference-plan /absolute/repository/validation/competitive-tabular/fixed-native-forest-v1.json
python3 validation/competitive-tabular/capture-host.py --output /absolute/cache/native-bank-host.json /absolute/cache/runs/native-fixed500-full-fit-4t-v1/bank/ranger
```

The fixed reference skips new calibration fits, retains 500 trees and every
training row, and keeps the original combined time/memory limits and scoring
freeze. The staging check fits a small development case without evaluation
files, tests source/manifest tampering, and verifies exact saved replay and
shared scoring budgets. On another machine, new calibration records have new
hashes: create and declare a new source mapping before running its fixed
reference, rather than weakening validation of the archived mapping.
