# AutoXplainR 0.6.0 release evidence

Status: final local product acceptance passed; release artifact and remote CI
acceptance are pending. No 0.6.0 tag or release has been published. The source
revision, archive checksum, remote checks and public-site verification will be
recorded here after they complete.

## What changed and why

The maintainer rejected the sparse 0.5.0 report. Its controls and calculations
could pass tests while leaving the reader unable to investigate the analysis.
The [overhaul record](product-overhaul-0.6.0.md) and four independent audits
separate the original findings, rejected intermediate designs and repairs.

The new report connects seven focused views: comparison, selection, data,
feature effects, predictions, checks and methods. Search evidence explains the
scheduled settings, fold results, numerical selection rule and actual refit.
Data exploration distinguishes original values from processed inputs and
preserves original row positions. Explicit record exports connect mistakes,
model disagreement and data filters. Supporting text uses contextual help and
disclosures; plots use responsive coordinates, readable labels and shared
scales where models are compared.

`evaluate_models()` brings existing fits into the same report without inventing
training history. `benchmark_predictions()` records repeated costs on a common
batch, including raw timing records and reasons for withholding unreliable
measurements. Both remain optional; `autoxplain()` is still the short entry point.

## Defects demonstrated during the review

These were reproduced incorrect behaviors, not hypothetical concerns:

- Changing a fitted coefficient or captured prediction state could combine old
  scores with new predictions or off-grid explanations. Evaluation snapshots,
  captured dependency checks and comparison with recorded predictions now
  reject that combination. Registered custom S3 prediction methods and native
  formula dependencies are included in the checked scope.
- The cutoff slider changed confusion counts but left mistake rows at 0.50.
  Case decisions, ordering, selected record and executable R code now follow
  the same cutoff. A probability exactly equal to the literal 0.57 also exposed
  a floating-point grid mismatch; every decimal boundary is now checked.
- Calibration plots and reported calibration error used different grouping
  rules. They now share the same grouped evidence, plotted at mean probability
  and observed event rate with group size retained.
- Data plots could omit an observed category, draw poorly spaced tick labels,
  overflow on phones or leave inert controls visible without JavaScript.
  Independent source-count and geometry checks now cover those states.
- The relationship cap still allocated all column pairs before truncation.
  Bounded generation retains every target pair and the requested additional
  pairs without that quadratic intermediate allocation.
- A repeated-cost table became illegible in an actual PDF despite screen
  geometry checks passing. The compact table separates costs from exceptional
  measurement details; validation reads the rendered PDF's text sizes.
- Several tests modified sealed model evidence after construction, and one
  numerical simulation silently depended on an old fitting default. Fixtures
  now use valid public constructors or narrowly scoped presentation helpers.
  The simulation explicitly requests its intended fitted linear model and
  preserves all seeds and numerical outputs.
- Generated-report tests could pass while committed website examples were
  stale. The gallery gate checks source and asset hashes before regeneration,
  then opens the committed reports in a browser.

## Final local validation

These checks passed on the frozen local implementation and refreshed reports.
They precede the final archive and remote platform checks; neither is inferred
from the local results.

| Check | Recorded result |
|---|---|
| Full package suite | 397 tests; 3,845 passing assertions; no failures, errors or warnings; two opt-in integrations skipped |
| Statement coverage | 91.84%; a coverage measure, not evidence that every behavior is correct |
| Live H2O minimum-version integration | 95 passing assertions, no test failures, warnings or skips; H2O 3.44.0.3, R 4.5.2, Java 17, two threads and 2 GB |
| PDP/ALE reference comparisons | All 11 passed; largest absolute difference 5.684342e-14 |
| Training-imputation mutation | Correct implementation passed; deliberately leaked fold imputation failed the intended numerical assertions without runtime errors |
| Fixed-linear-model bootstrap experiment | 200 replicates; coverage 0.90 at 20 evaluation rows and 0.96 at 100 rows; Monte Carlo standard errors 0.030 and 0.0196; seeds and numerical CSVs unchanged |
| Actual saved 0.5.0 results | Explicit explanation recomputation rendered regression, binary and multiclass results; incompatible retained audits were rejected |

Two H2O startup notices concerned the deliberately pinned older engine version.
They occurred outside the tests. The local cluster was shut down afterward.
See [the numerical execution record](results/numerical-validation-0.6.0.txt).

## Product acceptance

The final walkthroughs covered the public delivery and churn reports, multiclass
selection, supplied models, grouped and temporal splits, all export modes and
restricted explanation budgets. Readers could trace an error to its original
row, inspect the effect of preprocessing, filter exported data, explain the
selected parameter tuple, investigate failed optimization, compare model curves
and reproduce a selected model's predictions in R. Displayed answers were
checked against the original data and fitted objects after browser inspection.

See the [novice/data walkthrough](audits-0.6.0/data-and-workflows.md#implementer-walkthrough-after-the-repair--2026-09-07),
[statistical expert walkthrough](results/statistical-walkthrough-0.6.0.md),
[structured workflows](results/structured-walkthrough-0.6.0.md) and
[UI browser record](results/ui-browser-0.6.0.md). The
[overhaul disposition](product-overhaul-0.6.0.md) connects the original defects to
these completed tasks and their remaining limits.

| Final browser gate | Result and scope |
|---|---|
| Main report workflows | 1,279 passed; regression, binary, multiclass and quick reports; 320/390/768/1440 px; selection state, chart geometry, links, keyboard, no-JavaScript and selected-view print |
| Independent chart fixture | 63 passed against literal values; also invoked as one aggregate check in the main gate |
| Prediction workflows | 1,923 passed; original-prediction oracles, cutoff/case/R-code consistency, privacy modes, model/class selection and print |
| Literal cutoff fixture | 35 passed; decimal boundaries, first-level positive event, quoted model ID and record links |
| Data exploration | 209 passed; original raw/processed values, distributions, missingness, filtering, source mapping, export scope and narrow layouts |
| Selection evidence | 107 passed; candidate/fold arithmetic, identity, rationale, geometry and selected-family/open-candidate PDF scope |
| Supplied models and repeated costs | 166 passed; single/multiple fits, absent training, raw-block arithmetic, reference exclusions, row links and readable PDF output |
| Committed public gallery | 118 passed; source/asset manifest matched, all three actual public report files opened and their tabs, models and source links exercised at 1440/390 px |
| Deliberate faults | Disconnected model selection and false graphics rejected by the intended assertions, with no JavaScript errors; corrupted missing counts and source keys also rejected |

All eight gallery screenshots were refreshed and visually inspected. Browser
runtime errors were absent from the final main, prediction and gallery records.
The main UI run used Chrome 152.0.7977.82; prediction, data and gallery runs used
Chromium 145.0.7632.6, with Playwright 1.58.0. The detailed local JSONs are under
`/home/mmazzarelli/.cache/autoxplain-overhaul-0.6/` in `final-browser`,
`final-predictions`, `final-data`, `final-selection`, `final-supplied` and
`gallery-checks`. Full-suite results and coverage are in `full-suite-compact`
and `coverage-summary.txt`. Browser counts overlap where one gate invokes
another; they are not a total number of independent user tasks.

## Acceptance still pending

- Check the final source archive and installed-package workflows in a fresh
  library, including the supplied-model bridge and saved-result reuse.
- Complete supported-platform, optional-engine and release-quality CI against
  the release revision.
- Publish the checked archive and website, then verify the downloaded source,
  checksum, installed behavior and public reports.

This is an implementer review with independent agents and numerical oracles,
not a recruited-participant study. It does not establish superiority over other
packages. Aggregates are not anonymization; row exports are explicit. Fixed-fit
bootstrap intervals exclude retraining and model selection. Batch throughput
is not single-request latency. Dynamic external prediction state and temporal
tuning remain outside the supported contracts.
