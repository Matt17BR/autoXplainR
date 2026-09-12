# AutoXplainR 0.6.2 validation record

Status: [0.6.2 is published](https://github.com/Matt17BR/autoXplainR/releases/tag/v0.6.2).
All 16 release jobs passed. The actual download, fresh installation, saved-model
reloads and public website are verified. The package source and tag are unchanged
by the later validation-record commit.

## What prompted this release

The smaller examples showed that the workflow ran. They did not establish
whether it fitted useful models on difficult data or kept those analyses
inspectable. The new [modeling comparison](stress-modeling/findings.md) uses a
frozen published 0.6.1 installation, fixed evaluation rows, training-only folds
and independently fitted native references. Failures and poor scores remain
in its output.

The optional regularized and boosted models were already useful. The default
core portfolio was substantially weaker on the nonlinear and sparse wide
problems, across two synthetic replicates. The bank difference was small, and
rare-event ranking and probability accuracy sometimes favored different fits.
This release improves robustness and makes those distinctions visible; it does
not claim a new modeling algorithm or general superiority over other packages.

## Repairs and evidence

| Problem | Accepted behavior and evidence |
| --- | --- |
| Feasible grouped classification folds lacked classes | All 30 published-failure probe seeds now have coverage. An independent oracle also checked 2,500 constructed feasible cases and 2,940 small support multisets. See [grouped validation](stress-grouped/README.md). |
| Large categorical glmnet inputs allocated dense matrices | The measured matrix fell from 46.57 MB to 0.621 MB; native predictions agree. See [adapter probes](stress-adapters/README.md). |
| Extreme units and encoded name collisions broke learners | Scaling preserves finite meaningful variation, and encoded columns retain unique names and their original input mapping. Public SVM and MARS failures have regression tests. |
| Automatic wide GAM search ran for minutes without finishing | A disclosed fold-local capacity policy excludes that case before native work; other families continue. It does not prohibit externally fitted penalized GAMs. |
| An unstable linear fit looked like an ordinary alternative | The fit warning and rank/design dimensions are recorded; rank deficiency is visible beside report settings. Actual predictions and scores remain unchanged. |
| One extreme loss flattened the useful models | An explicit positive-loss logarithmic axis preserves every model and Pareto step. Literal geometry, scale guards, URL state, keyboard use and print are checked. |
| Wide row reports were slow and missed available relationships | The 500-predictor row report fell from 121.14 to 20.72 seconds with byte-identical embedded data. Direct pair exploration uses exported records when no aggregate pair was retained, with its sample scope identified. |
| Repeated findings buried their evidence | Shared findings use a compact model/feature table, preserving each claim and the exact diagnostic link. |

The [report review](stress-reports/README.md) and
[isolated performance measurements](stress-performance/README.md) separate
fitting, explanation computation, serialization and browser costs.

## Task inspection and test quality

Actual reports from the sparse wide, nonlinear and bank problems were inspected
at desktop and phone widths. Tasks included finding the training-selected model,
comparing retained fits, inspecting parameters and failed folds, changing metrics
and models, finding prediction mistakes and exploring source records. The sparse
fit's RMSE 110.5 comparator remains present and visibly rank deficient.

New browser regressions cover 155 pair/source-record checks, 66 score-scale
checks including the actual sparse report, and 10 grouped-evidence link checks.
The existing cost-axis checks also pass. Negative controls deliberately disconnect
pair fallback or distort chart coordinates and must fail for the intended reason.
The grouped allocator's first implementation failed 30 known-feasible cases;
its retry strategy was repaired before acceptance. Tests were not weakened to
match that first implementation.

The complete browser replay also passes: 1,365 explorer checks, with nested
chart, frontier and cost suites reporting 219, 327 and 96 checks. Separate data,
prediction, selection, cutoff and supplied-model runners report 249, 1,923, 223,
35 and 171 checks. These are counts per runner, not a combined unique-test total.
Both deliberate browser defects are rejected for the intended reason.

The refreshed three-report gallery passes its 118 checks and 128 layout/print
checks. The screenshot manifest binds 65 source files and 11 assets. Overview,
feature, prediction and Checks PDF pages were rendered to PNG and visually
inspected for legibility, clipped content and orphaned charts. This is an agent
task walkthrough, not a recruited-participant study.

The final local candidate passes 4,182 assertions, with no failures, errors
or warnings. Live H2O and hosted Gemini are separate opt-in integrations and
were skipped in that run. Lint and spelling pass. Eleven numerical references
agree within 5.68e-14. The imputation mutation check rejects leaked fold medians
through the expected assertion failures, rather than a script error.

An earlier local pass was insufficient. On macOS ARM64, the new subtraction-overflow
test failed because `colMeans()` accumulated huge finite values into an infinite
sum. Linux's extended-precision accumulator concealed the defect. A portable
regression reproduces the double-precision additions, while the original test
continues to run on every platform. Independent review also found that the
standard-deviation fallback overestimated a one-step difference near the numeric
limit by 41.4%. Its regression uses the exact two-point answer, the difference
divided by the square root of two. Neither failure was skipped or accepted as
harmless platform variation. The repaired source passes actual macOS ARM64
R 4.6.1 CI. The [independent numerical review](numerical-review/README.md) also
compares 48 ordinary input encodings and 35 high-precision SD oracles; its
maximum relative SD error is 2.22e-16.

The standalone numerical CI job also omitted its direct `testthat` dependency.
It failed in an empty library and later passed after another job populated the
shared dependency cache. The workflow now installs that dependency explicitly;
the cached pass was not treated as proof that the setup was complete.

## Remaining limits

The largest modeling training sample has 5,000 rows; the widest modeling case
has 240 inputs. The report fixture has 500 inputs and 1,200 records. These do not
establish suitability for millions of rows or arbitrary deployment shifts.

The full recommended search can be expensive. On the nonlinear case it took
532.5 seconds and selected the same boosted model as the 57.7-second explicit
regularized/forest/boosting search. Faster native BAM fitting is a promising
separate experiment, not a shipped solver change. Some extreme-unit native fits
remain unsupported and fail explicitly. Local fitting has no hard elapsed-time
limit; `max_runtime_secs` remains specific to H2O.

Full-feature explanation screening can still dominate runtime. The widest full
row report remains about 60 MB; its faster construction does not reduce its
transfer size. Temporal tuning and CRAN submission are not part of this release.

Local raw evidence is under `~/.cache/autoxplain-stress-0.6.2/`. Compact protocols,
measurements and independent checks are committed alongside this record.

## Publication evidence

[PR #5](https://github.com/Matt17BR/autoXplainR/pull/5) passed all 14 checks on
`86bd6db2ce7378f74ff817a6e018c6318523173b` and was merged as
`f71f7edcbe3f3cfbbd1831ab6c2eb4fd44647167`, with an identical source tree.
The annotated `v0.6.2` tag points to that merge. The release workflow is
[34659828765](https://github.com/Matt17BR/autoXplainR/actions/runs/34659828765).

The accepted local archive has SHA-256
`229e255d49be2dba5b528183db0dc384342b29dd5df65eadf84f5d4019d8facc`.
Its `R CMD check --as-cran` result is zero errors, zero warnings and one NOTE,
including rebuilt vignettes and both manuals. The NOTE records new-submission
status and a findings link that was not yet on main. That link returned HTTP 200
after the merge. All 191 authored packaged files match the accepted source.
Fresh installation and a separate R process verify 17 models across five
workflows, plus eight native models and three additional reports.

Two earlier local archives were superseded despite passing their local checks:
`e191b97c21c5137285441b6a5d72954342541a9372026c6c539c128f881c15eb`
preceded the multiresponse linear-model rank correction;
`8e9969068caaf3b19c5b136d9ca996e158baa8e2c38e6973bf293fea94cfda7d`
preceded the numerical portability fixes. Neither archive was published.

The release was published at 2026-09-12 00:21:57 UTC. The downloaded archive's
SHA-256 is
`e15ee291f447414a16e10acce0f69cca826d7e58b424f5597c41ea9f56114624`,
matching GitHub's asset digest and the downloaded `SHA256SUMS`. Its 191 authored
files match the reviewed commit, allowing R's build metadata in `DESCRIPTION`.
The complete 207-file inventory matches the accepted local archive. The [portable record](results/release-0.6.2.json) retains
source hashes, release-job links and actual public HTTP observations.

The published archive's [R 4.6.1 check](results/release-0.6.2-r-check.txt) and
[R-devel check](results/release-0.6.2-r-devel-check.txt) each passed with zero
errors, zero warnings and one "New submission" note. R-devel used `--no-manual`
and reported unavailable optional engines; both manuals passed under release R,
and the native-engine gates passed separately. The quality gate passed all
4,182 assertions and measured 92.06% statement coverage. Coverage is supporting
evidence, not a correctness guarantee. Live H2O passed 95 assertions with no
failures, warnings or skips. The paid Gemini integration was not run.

A fresh installation of the actual download passed prediction and saved-result
checks across 25 models in nine workflows. Separate R processes reloaded the
models and rendered seven reports, covering ordinary regression/classification,
supplied models, preprocessing, sparse regularized models and extreme-unit SVMs.
The [installed workflow log](results/release-0.6.2-installed-smoke.txt) records
the completed runs. The archive checksum remained unchanged after installation.

After publication, the public site passed all 280 checks: 22 successful HTTP
fetches, all 11 report/image hashes, and desktop/mobile report tasks without
browser errors. All fetched bytes match the accepted main deployment. The
[pkgdown build](https://github.com/Matt17BR/autoXplainR/actions/runs/34659798596)
and [Pages deployment](https://github.com/Matt17BR/autoXplainR/actions/runs/34660017466)
remain current; no separate release-event deployment was observed.
