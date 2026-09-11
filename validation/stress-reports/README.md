# Reports on harder problems

This review asks whether someone can inspect a difficult fitted analysis, rather
than whether a small demonstration page loads. It separates fitting, explanation
computation, report generation and browser interaction. Output stays outside the
checkout, normally in `~/.cache/autoxplain-stress-0.6.2/reports`.
The compact [observed results](observed-results.json) retain timings, payload
hashes, browser observations and check totals from this run.

The scripts use R 4.5.2 and Playwright 1.58.0 in the recorded local run. Timing
observations are descriptive measurements on one computer, with native fitting
threads restricted to one. They are not promises about other machines.

## Workloads

`generate-wide.R` creates 1,200 rows with 100 or 500 predictors, including a
categorical variable with 80 levels. It fits a small tree search and baseline,
then audits three specified signal variables. This deliberately isolates report
cost from the cost of screening every input. Both reports retain all data
columns; row mode exports all 1,200 observations.

`render-benchmark.R` reuses saved models from the separate modeling benchmark.
The inspected cases were:

| Case | Training / evaluation rows | Inputs | Question for the report |
| --- | ---: | ---: | --- |
| Nonlinear signal among irrelevant inputs | 1,200 / 800 | 30 | Can readers compare boosting, forest and regularized fits and inspect their settings? |
| Many predictors, few observations | 180 / 600 | 240 | Can readers understand failed neural configurations and an unstable linear alternative without losing the useful comparisons? |
| Bank marketing responses | 5,000 / 3,000 | 19 | Can readers inspect probabilities, errors and mixed categorical data when accuracy alone would be misleading? |

These runs screen all inputs for three models, then audit and plot each model's
leading inputs with two permutation repeats and three displayed features per
model. The reports explicitly retain the small repeat budget. Their curves and
shuffle intervals are inspection fixtures, not final inferential results.

## What failed, and what changed

- **Some relationships could not be inspected even with every row embedded.**
  Outside the 512 precomputed pair budget, the browser required a filter before
  it would calculate a pair from exported records. Selecting a pair directly
  now works. Its scope names exported records and gives the available row count
  for each split. Summary-only reports still say when a pair was not computed.
- **Report generation greatly exceeded fitting time.** The 500-input fixture
  fitted in 5.47 seconds, but produced its summary in 26.75 seconds and its
  59.9 MB row report in 121.14 seconds. Separate profiling found repeated small
  table construction, per-row conversions and JSON serialization. The
  performance changes preserve the complete data payload. See the independent
  measurements in [stress-performance](../stress-performance/README.md).
- **One very poor alternative flattened the comparison chart.** The sparse
  example retained a linear model with RMSE 110.5, alongside useful scores
  between 3.25 and 4.24. The table remained useful, but their plotted separation
  was roughly two pixels. An explicit logarithmic score scale now supports
  positive losses. It keeps every model, original score and Pareto step. The
  default remains linear; zero losses and metrics such as accuracy or R-squared
  cannot use the logarithmic score axis.
- **Repeated uncertainty cards buried the useful information.** Unresolved
  shuffle intervals are now grouped into a table containing each model, feature,
  claim and link to its exact evidence row. Different claims and recommendations
  remain present. A single finding keeps its existing presentation.

The browser did not crash on the 59.9 MB report. It loaded in about 1.9 seconds
locally, and choosing a late column took about 0.2 seconds on the inspected
desktop and phone layouts. Downloading or sharing that file remains a real
limitation. Use `report_data_control(columns = ..., max_rows = ...)` when the
recipient needs a smaller set of columns or records. Selecting fewer exported
columns does not change the fitted model or its evaluation scores.

Fresh-process rendering of the unchanged saved fits after the fixes gave:

| Inputs | Export | Before | After | Final HTML size |
| ---: | --- | ---: | ---: | ---: |
| 100 | Summary | 19.80 s | 8.75 s | 17.1 MB |
| 100 | All rows | 39.33 s | 9.53 s | 25.6 MB |
| 500 | Summary | 26.75 s | 11.40 s | 19.4 MB |
| 500 | All rows | 121.14 s | 20.72 s | 59.9 MB |

The embedded data JSON was byte-identical before and after in all four cases,
including the complete raw and processed records. The change makes construction
faster; it does not claim to make those files smaller. The final 500-input row
report loaded in 1.53 to 1.57 seconds locally. Direct pair selection worked at
both viewport sizes, and no browser exception or page-width overflow was found.

Full-feature permutation screening remains a separate cost. In the sparse
example it took 44.14 seconds even with two repeats; profiling attributed most
of that time to repeated model predictions, especially the rank-deficient
linear fit. `top_features` limits the displayed/audited subset after screening,
not the initial search for important inputs. No claim of inexpensive automatic
explanations for arbitrarily wide or expensive models follows from this review.

## Reproduce the bounded regression checks

Run from the package root with dependencies installed. Choose an output
directory and install Playwright 1.58.0 with Chromium, then run:

```sh
export AXR_STRESS_REPORTS="$(mktemp -d)"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1
Rscript validation/stress-reports/generate-pair-cases.R
Rscript validation/stress-reports/render-score-fixture.R
python validation/stress-reports/check-pairs.py
python validation/stress-reports/check-score-scale.py
python validation/stress-reports/check-grouped-findings.py
```

These checks also run in `report-browser` CI. They use 120 rows and 35 inputs,
plus a three-model literal chart, rather than fitting the large stress cases.
They check:

- Exact joint counts and numeric, mixed and categorical associations against
  independently retained source observations, including missing and non-finite
  context values, sampled exports and restricted columns.
- Full-data versus exported-sample scope, unchanged model scores, keyboard
  controls, phone layouts and a deliberately disconnected pair calculation.
- Linear and logarithmic point positions and Pareto steps against literal
  values, visible scale labels, URL restoration, print output and invalid-scale
  guards. Deliberately displaced points and a removed scale label are rejected.
- Every grouped diagnostic record and the actual evidence row reached by each
  link, including keyboard focus and retained model/feature identity.

The published 0.6.1 implementation failed all 12 uncomputed-pair selection tasks
in the original full/sample export fixture. Its other 40 checks passed. This
distinguishes the defect from a broken browser setup or a missing fixture.

## Reproduce the larger walkthrough

`generate-wide.R` creates the saved fits, initial reports and separate numerical
answers. Set `AXR_STRESS_LIBRARY` to an installed package library when measuring
an immutable release; otherwise it loads the checkout. `profile-render.R`
profiles a saved fit and keeps report generation separate from fitting.
`replay-wide.R` renders the same saved results through current source without
refitting. `browser-wide.py` records desktop and phone tasks and screenshots.

`render-benchmark.R` reads the benchmark's `baseline/<case>/<setting>/result.rds`.
Set `AXR_STRESS_CASE` and `AXR_STRESS_SETTING`, then run
`browser-benchmark.py` with `AXR_STRESS_BENCHMARK=<case>-<setting>`. The browser
walkthrough captures every tab, score and family search before conclusions are
drawn from the R objects. The sparse model is also checked by the score-axis
script when its saved report is available.

Inspect the screenshots and recorded text as well as the numerical checks.
This is an adversarial product walkthrough and acceptance automation, not a
study with recruited participants or proof that every future workload will be
usable.
