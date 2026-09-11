# AutoXplainR 0.6.2 validation record

Status: release candidate. Local package and browser checks are in progress;
publication and supported-platform CI have not yet been accepted.

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

The refreshed three-report gallery passes its 118 checks and 128 layout/print
checks. The screenshot manifest binds 65 source files and 11 assets. Overview,
feature, prediction and Checks PDF pages were rendered to PNG and visually
inspected for legibility, clipped content and orphaned charts. This is an agent
task walkthrough, not a recruited-participant study.

The final fresh source suite passes 4,150 assertions, with no failures, errors
or warnings. Live H2O and hosted Gemini are separate opt-in integrations and
were skipped in that run. Lint and spelling pass. Eleven numerical references
agree within 5.68e-14. The imputation mutation check rejects leaked fold medians
through the expected assertion failures, rather than a script error.

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
