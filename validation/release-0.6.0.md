# AutoXplainR 0.6.0 release evidence

Status: the repaired local archive, fresh-session installed workflows,
extended-portfolio walkthrough and remote preflight checks passed. PR #3 was
merged and `v0.6.0` tagged at `c0dfd97cada947923855ff67605fa3961c175fe0`.
Its Git tree matches the reviewed `c96234d` revision exactly. The complete
[release workflow](https://github.com/Matt17BR/autoXplainR/actions/runs/34149652943)
passed and [0.6.0 was published](https://github.com/Matt17BR/autoXplainR/releases/tag/v0.6.0)
at 2026-09-07 18:26:01 UTC. The actual published download, fresh-session
workflows, saved eleven-model analysis and live website are verified.
The authorized GitHub release and product-acceptance work are complete.

The accepted local source archive has SHA-256
`6b78b8968f27736d80e9b89e8343ed13030d12e7519b4d85be53607fef3f9e58`.
It matches the packaged files of source revision
`c96234d00925740f2d281aa8036b88cb97162d0c`. Its full `--as-cran` check passed
with zero errors and warnings and two notes: new submission and unavailable
external time verification. The final published archive will be identified
separately; rebuilding an archive need not produce the same bytes.

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

## Remote preflight on the repaired revision

All checks passed on `c96234d00925740f2d281aa8036b88cb97162d0c` before
[PR #3](https://github.com/Matt17BR/autoXplainR/pull/3) was merged:

| Check | Evidence |
|---|---|
| Seven package-check jobs, including Windows, macOS, Linux release/oldrel/devel, R 4.1 and an exact R-devel source archive | [R-CMD-check](https://github.com/Matt17BR/autoXplainR/actions/runs/34148943594) |
| Current Linux/Windows engines and the exact declared minimum versions | [Native engines](https://github.com/Matt17BR/autoXplainR/actions/runs/34148943624) |
| Live H2O | [H2O integration](https://github.com/Matt17BR/autoXplainR/actions/runs/34149009420) |
| Lint and spelling | [Lint](https://github.com/Matt17BR/autoXplainR/actions/runs/34148943659) |
| Numerical references, deliberate leakage fault and simulation | [Statistical validation](https://github.com/Matt17BR/autoXplainR/actions/runs/34148943665) |
| Statement coverage, 91.46% | [Coverage](https://github.com/Matt17BR/autoXplainR/actions/runs/34148943620) |
| All browser workflows and deliberate UI faults | [Browser gates](https://github.com/Matt17BR/autoXplainR/actions/runs/34148943660) |

The downloaded browser artifacts confirm 1,283 main, 217 literal-chart, 1,923
prediction, 209 data, 107 selection, 35 cutoff and 118 gallery checks. Supplied
models passed 165 checks remotely versus 168 locally: the remote run withheld
an unreliable historical timing, replacing six numeric timing checks with three
explicit-withholding checks. The different count is a measured-state branch,
not an omitted workflow. No runtime errors occurred. The tagged
[release workflow](https://github.com/Matt17BR/autoXplainR/actions/runs/34149652943)
repeats its gates and then checks the exact archive it publishes.

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

## Initial local acceptance checkpoint

These checks passed before the additional defects below were discovered.
They document that checkpoint; final archive and remote results are recorded
separately rather than inferred from these results.

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

## Follow-up after CI preflight and the wider-portfolio walkthrough

The tables above describe the earlier local checkpoint, including its 397 tests
and 3,845 passing assertions. Subsequent inspection found additional defects:

- lintr 3.4 rejected compound assignment in an environment helper. Explicit
  local-environment assignments preserve the intended state changes; the focused
  suite passed 101 assertions.
- An R 4.1 offset fixture used a native prediction expression outside its valid
  environment. The corrected fixture supplies that context in the global
  environment and restores it afterward. The focused suite passed 97 assertions
  with all guards retained; a separate old-R method probe also passed. Remote
  R 4.1 CI for the final release revision is pending.
- At 320 pixels, an unbroken R call and font metrics made the multiclass Checks
  context note overflow. Scoped text wrapping and a Checks font-variation gate
  now cover that case.
- The eleven-model [extended walkthrough](results/extended-walkthrough-0.6.0.md)
  found a blank GAM basis parameter and overlapping cost-plot labels. The GAM
  identity now uses effective per-input basis sizes, with native `bs.dim` values
  in model details. Dense cost plots reserve a column for connected model names
  while preserving linear scales; tick spacing also has a minimum separation.
- CI exposed a second chart defect: bold model names in the no-JavaScript SVG
  exceeded their reserved width with a different system font. Static labels now
  reserve room for bold 14-pixel text and wrap long compound names at their
  hyphens. This preserves the full names, point geometry and production fonts.
  The local chart gate passed 217 checks, including 33 new wider-font checks at
  1440, 390 and 320 pixels. The same wider font reproduced clipping in the old
  output before the repair; the clipping and collision assertions were retained.
- A saved tuned-neural model failed prediction in a fresh R session because its
  native nnet S3 method had only been loaded during fitting. Explicit namespace
  imports of the existing nnet and rpart dependencies now register those methods
  when AutoXplainR is loaded. The installed-package smoke test starts a separate
  `Rscript --vanilla` process which reads saved results without fitting or
  attaching either engine. The repaired local installation passed this replay
  for 17 models across five scenarios and rendered four reports. Responses and
  classification decisions matched the outputs saved by the earlier process;
  the recipe scenario also handled missing values and a novel category.

Intermediate checks passed 184 hand-chart assertions, 27 real extended-model
plot combinations and 1,283 main browser assertions. These results precede the
last tick-spacing and font repairs. The subsequent 217-check chart run is in
`static-chart-fonts/checks/chart-fixture-checks.json`; the fresh-session replay is
in `core-namespace-replay/artifact-replay.log`, under the local evidence directory
listed above. These intermediate results preceded the accepted repaired
archive described below; remote acceptance is tracked separately.

The candidate archive with SHA-256
`c585530574482161b144e586af71f24407c780b5ca1e15d7be2653ca6220df4a`
is explicitly superseded. Its package check reported zero errors and warnings,
but the subsequent fresh-session replay demonstrated the missing native method.
It is not the release artifact. The repaired local archive's digest and revision
are recorded at the top of this document; published-artifact results remain
separate obligations.

Remote preflight for commit `cb7d360a93ca0e2029f4e63924a38365f2e6bc48`
passed the [full platform matrix, including Linux R 4.1](https://github.com/Matt17BR/autoXplainR/actions/runs/34148079885),
[current and declared-minimum native engines](https://github.com/Matt17BR/autoXplainR/actions/runs/34148079911),
[live H2O](https://github.com/Matt17BR/autoXplainR/actions/runs/34148080093),
[lint](https://github.com/Matt17BR/autoXplainR/actions/runs/34148079930),
[coverage](https://github.com/Matt17BR/autoXplainR/actions/runs/34148080060) and
[statistical validation](https://github.com/Matt17BR/autoXplainR/actions/runs/34148080021).
The [browser gate failed on static label clipping](https://github.com/Matt17BR/autoXplainR/actions/runs/34148079877),
prompting the font repair above. Those successful runs validate that preflight
revision, not the subsequent font and fresh-session dispatch repairs.

## Published release archive

The published `AutoXplainR_0.6.0.tar.gz` is 2,750,939 bytes. Its SHA-256 is
`902e9a6f122299e836cd73cfe9790b42cdec876e41753cb630d02e8199da7e35`.
The release job built this archive once, checked it on R 4.6.1 with its PDF
manual, examples and vignettes, installed it into a new library and ran both
the parent workflow and separate-process reload guard. R-devel
`2026-09-06 r90498` then checked the same downloaded archive with
`--as-cran --no-manual`. Both checks reported zero errors, zero warnings and
one note: "New submission." Checksums matched before and after these checks
and immediately before publication.

The CI fresh-session process restored 17 models across five saved scenarios,
matched their responses and class decisions, rendered four reports, reapplied
the saved recipe and reused the supplied-model benchmark. Raw job logs are
retained locally in `release-source-package.log` and
`release-final-r-devel.log`; the linked release workflow contains the public
jobs and downloadable check artifacts.

An independent download of GitHub release asset `549210346` matched both the
published checksum and the staged archive byte-for-byte. It was installed into
another new private library. The installed parent workflow and fresh
`Rscript --vanilla` child passed all five scenarios: 17 models, matching raw
responses and class decisions, four regenerated reports, saved preprocessing
and benchmark reuse.

The downloaded package also replayed the unchanged eleven-model saved analysis
through the [portable walkthrough](extended-walkthrough/README.md). Its report
SHA-256 is exactly
`da7834ad6bd8f10ca0f41832a8ec0abc964a9f7a0ed35b9b24f3fac8ea528d2a`, matching
the previously inspected report. Original and copied data/model RDS files both
retain SHA-256
`086f87ec494d8fe386715eb49f8826a6db7a58701e21e83a0d6b19de396f3aa4`.
All 186 packaged source, test and documentation files match the checkout.
Compared with the accepted local archive, differences are confined to packaging
metadata, `build/vignette.rds` and two generated vignette HTML files; there are
no authored-source changes. Downloaded files, installation logs, source
comparison and replay evidence are under `published-package-verification/`
in the local evidence directory.

This completion record and portable review scripts are committed after the
release as validation evidence. They are excluded from the package build and
do not change the immutable release tag, archive or reviewed public gallery.

## Tagged release prerequisites

The release workflow on `c0dfd97` passed every supported-platform job,
current and exact-minimum engine jobs, live H2O, browser tasks and quality
checks before starting the source-package job. The quality job recorded 3,860
passing assertions with zero failures or warnings and two opt-in integration
skips; coverage was 91.86% in that environment. Roxygen synchronization, lint,
spelling, all eleven numerical reference comparisons, the deliberate training
leakage counterexample and the fixed-linear-model simulation passed.

The live H2O job passed 95 assertions on H2O 3.44.0.3 and R 4.6.1. Each
current-engine job passed 694 assertions on Linux and Windows. The tagged
browser artifacts contain 1,283 main, 217 literal-chart, 1,923 prediction,
209 data, 107 selection, 35 cutoff, 168 supplied-model and 118 gallery checks,
with no recorded runtime errors. Both deliberate UI faults were rejected by
the intended checks. These are results from the tag's own jobs, not counts
copied from the earlier PR run.

## Published website verification

The [pkgdown build](https://github.com/Matt17BR/autoXplainR/actions/runs/34149633973)
produced site revision `00097309ef01cb695132b869cd81088574e1ae07` from the
merged source. Its completion initially left the old 0.5.0 reports online:
the separate [Pages deployment](https://github.com/Matt17BR/autoXplainR/actions/runs/34149816152)
was still queued. Acceptance waited until that deployment succeeded and the
canonical URLs served the new bytes.

All three downloaded HTML reports and eight PNG screenshots match the reviewed
repository files exactly. The online walkthrough passed 280 checks across
1440- and 390-pixel layouts, with no browser or console errors. It exercised
the seven tabs, model settings and details, tree rationale and candidate folds,
source-record links and affected-evidence deep links. New API references,
guides and site navigation were also checked. A final in-app-browser reload
showed the actual 0.6.0 selection view and its seven tree configurations.

| Canonical report | SHA-256 |
|---|---|
| [Delivery time](https://matt17br.github.io/autoXplainR/model-report.html) | `39a74e381d749d53c47df56cd3f876e69b30cda1cca936cfde439ddb777d9ab7` |
| [Customer churn](https://matt17br.github.io/autoXplainR/binary-report.html) | `c4af6c7aa0e7c1a9a0508f173cfcdacd901222a738a575d1dfd0f2b347615dea` |
| [Flower species](https://matt17br.github.io/autoXplainR/multiclass-report.html) | `32d66daf22072de056b3d3733a945b593d21fa316c3e5353efcb881703c374a8` |

Downloaded responses, hashes, browser observations and screenshots are in
`public-release-verification/published-final/public-verification.json` and
its adjacent files under the local evidence directory.

## Accepted repaired local archive

The exact `6b78b896…f3f9e58` archive passed 3,860 package assertions with two
opt-in integration skips. It was checked with its manual, examples and
vignettes, installed into a new private library, and exercised through the
installed-workflow script. A separate `Rscript --vanilla` process restored
17 models across five scenarios, reproduced their responses and class decisions,
rendered four reports, reapplied missing/novel-category preprocessing, and
reused a saved benchmark. Neither fitting nor manually attaching the model
engines was used to make that replay succeed.

The independent eleven-model [extended replay](results/extended-walkthrough-0.6.0.md)
used that same archive and the unchanged original saved analysis. Its copied R
calls and report answers match native engines; effective GAM settings, dense
chart labels and original-record navigation were inspected again.

After the final font repair, the main browser gate passed 1,283 checks and its
literal chart fixture passed 217, including 33 wider-font cases. No browser
runtime errors occurred. The final regenerated public gallery passed 118 checks
and its manifest matches 65 sources and 11 assets. The eight recaptured PNGs
are byte-identical to the previously inspected screenshots because the final
chart repair affects the no-JavaScript fallback. Main/chart evidence is in
`final-release-browser/`; gallery evidence is in `final-font-gallery/`, under
the local evidence directory above. The other follow-up gates passed 1,923
prediction, 209 data, 107 selection, 168 supplied-model and 35 literal-cutoff
checks before the final static-font repair; the final remote browser workflow
runs them all again against the release source.

This is an implementer review with independent agents and numerical oracles,
not a recruited-participant study. It does not establish superiority over other
packages. Aggregates are not anonymization; row exports are explicit. Fixed-fit
bootstrap intervals exclude retraining and model selection. Batch throughput
is not single-request latency. Dynamic external prediction state and temporal
tuning remain outside the supported contracts.
