# Reproduce the extended-portfolio walkthrough

This bundle preserves the scripts behind the
[0.6.0 expert walkthrough](../results/extended-walkthrough-0.6.0.md). It fits a
separate synthetic dispatch example or replays its saved result through an
installed package. It does not update the public examples, screenshots or
package source. Keep its output outside the checkout.

Use AutoXplainR 0.6.0 installed from the archive being checked. The original run
used R 4.5.2, mgcv 1.9.4, glmnet 5.0, ranger 0.18.0, xgboost 3.2.1.1, e1071
1.7.17, earth 5.3.6 and kknn 1.4.1. The record gives archive hashes and observed
answers. This fixture expects a selected GAM plus retained boosting and forest
models; changed engine versions or selection need investigation, not adjusted
expected values merely to pass.

Run from the repository root. Choose an existing private library containing the
installed archive and its dependencies, then an empty output directory:

```sh
export AXR_EXTENDED_LIBRARY=/absolute/path/to/checked-library
export AXR_EXTENDED_DIR="$(mktemp -d)"
Rscript validation/extended-walkthrough/generate.R
```

`generate.R` preserves the original seeds, data-generating function, 20-setting
extended search and four-model/two-feature/two-repeat explanation budget. It
writes `source-and-result.rds`, `extended.html` and session information. It
refuses to overwrite a saved result.

For a report-only replay, use the saved RDS and a different output directory.
This starts a fresh R process, predicts with the saved neural model before any
report work, and renders without refitting. The copied RDS must remain byte-for-byte
identical to the supplied original.

```sh
export AXR_EXTENDED_SOURCE="$AXR_EXTENDED_DIR/source-and-result.rds"
export AXR_EXTENDED_DIR="$(mktemp -d)"
Rscript validation/extended-walkthrough/replay.R
```

Install Playwright 1.58.0 in a Python environment and its Chromium browser:

```sh
python -m pip install playwright==1.58.0
python -m playwright install chromium
```

Alternatively, set `BROWSER_EXECUTABLE` to an installed Chrome executable; the
original review used Chrome 152.0.7977.82. All browser scripts open the local
`extended.html`. They collect visible text and screenshots, not model oracles.

```sh
python validation/extended-walkthrough/browser-first.py
python validation/extended-walkthrough/browser-details.py
python validation/extended-walkthrough/browser-folds.py
```

Read the overview, model details, selection evidence, optional-model effects and
linked error record. Inspect the desktop and phone screenshots and write down
answers and friction **before** running the native checks. In particular, check
GAM basis dimensions, the one-SE choice, all eleven cost labels, the deliberately
uncomputed forest effects and the original row behind a boosting error. A script
completing does not establish that those views are useful or legible.

`browser-details.py` also records the exact displayed prediction calls and the
visible GAM curve table. The native checker executes those copied calls on the
original evaluation rows, uses native mgcv/xgboost/ranger predictions, reconstructs
CV arithmetic from OOF predictions and compares the displayed GAM differences
with native predictions independently of ALE centering:

```sh
Rscript validation/extended-walkthrough/check-native.R
```

Review `independent-native-answers.json`, the browser JSON records and the new
session information together. Repeating a saved fit preserves its original
training timings; fitting anew can change timings and numerical results across
environments. Do not compare a regenerated file's hash to an older report as
if timing observations and build environments were identical. The package-wide
fresh-session acceptance guard is separate: [check-reloaded-artifact.R](../check-reloaded-artifact.R).
