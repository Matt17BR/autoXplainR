# Product overhaul after 0.5.0

The maintainer rejected 0.5.0's limited exploration, unexplained search choices
and sparse charts. Passing the previous release checks did not establish that
the report helped readers investigate their models. This record follows the
new implementation and the tasks used to challenge it.

## What must become possible

1. Trace a retained model from its search space through fold results, selection
   rule and final fit. Explain the actual parameter values and why the search
   included them. Distinguish a pragmatic default grid from a theoretically
   optimal setting. Show warnings, failed fits and incomplete optimization.
2. Understand the supplied data before interpreting a model: distributions,
   missingness, target balance, training/evaluation differences and relationships.
   Identify what preprocessing changed. Explore individual observations when
   the report author explicitly includes rows.
3. Compare accuracy, resources and behavior without mentally joining unrelated
   legends or scrolling through oversized figures. Switch models while keeping
   the question, input and class consistent. Compare fitted curves directly.
4. Investigate prediction failures, calibration and classification cutoffs using
   concrete values. Connect observations back to the exported data where allowed.
5. Reproduce the analysis in R and export a report with a clear data scope. A
   summary report must not silently embed individual prediction or input records.

## Working boundaries

- Keep `autoxplain()` as the simple entry point. Advanced options must expose
  substantive choices, not require beginners to assemble the report.
- Use focused tabs and compact analytical layouts. Put explanations at the
  decision they explain; reserve tooltips and disclosures for supporting detail.
- Prefer readable labels, shared scales where comparison requires them, useful
  hover/focus details, and inspectable data over decoration or generic summaries.
- Treat default grids, one-standard-error selection and small timing differences
  as choices requiring evidence and limitations. Do not manufacture a rationale
  from the winning test score.
- Raw data, processed model inputs and evaluation data are different objects.
  Preserve their provenance and row mapping. Aggregation is not anonymization.
- Check important answers independently. Test meaningful incorrect behavior,
  not only markup or an expected answer copied from the implementation.

## Initial findings to resolve

| Finding | Consequence | Required repair |
|---|---|---|
| Four-point cost plots occupy most of a screen and use numbered legends. | The figure consumes attention without helping identify a model. | Compact responsive plots, direct identification and details, sensible axes, comparison beside relevant scores. |
| Search information is behind two disclosures and rationale is mostly absent. | Readers cannot explain why their retained tree or neural network has those settings. | A selection workspace with candidate/fold evidence, grid rationale and explicit selection thresholds. |
| Raw training data are discarded after preprocessing. | Before/after data exploration cannot be reconstructed honestly. | A versioned retained data profile/snapshot with row and partition provenance. |
| HTML already includes per-case predictions without an export mode. | Sharing scope is unclear and a new data tab could make it worse. | One consistent export contract covering all report channels. |
| Changing the resource axis resets table sorting. | Controls discard the reader's current comparison. | Separate metric selection, resource selection and table sort state. |
| Feature switches use independent model defaults and scales. | Side-by-side interpretation requires memorizing values from different views. | Preserve compatible feature/class choices and support direct model comparison. |
| Nonconverged neural fits can have status `ok` and no warning. | The search presents unfinished optimization as an ordinary successful fit. | Retain engine convergence status per fold and refit; define and test its selection policy. |

Four independent audits cover [visual workflows](audits-0.6.0/product-and-visuals.md),
[statistical/search design](audits-0.6.0/statistics-and-selection.md),
[data exploration](audits-0.6.0/data-and-workflows.md) and
[package architecture/tests](audits-0.6.0/software-and-tests.md). They record the
0.5.0 baseline separately from proposed repairs. This file is the living
disposition; earlier release and audit records retain their historical scope.


## First integrated inspection: rejected states

The first component-level checks passed, but the combined report still failed
several user tasks. These are implementation findings, not release acceptance:

| State inspected | Observed failure | Next iteration |
|---|---|---|
| Binary Data tab at 1440px | 3,501px page stacked distribution, relationships and 20 rows. Two-category plot had unused vertical space. | Persistent Distribution / Relationships / Records workspaces; compact category height and ten-row pages. |
| Binary relationship plot | The axis omitted the observed `yes` category while displaying empty overflow bins. | Plot observed category bins and label every displayed category; retain full counts separately. |
| Binary selection tab | 3,139px page stacked all family charts; parameter rationale sat below them. | Choose a family, show its actual parameter tuples and rationale beside its candidate evidence. |
| Classification errors | A replacement table retained correctness but dropped probabilities and confident-error ordering. | Restore both class probabilities and rank wrong rows by probability of the observed class. |
| Calibration | Bin midpoints were used instead of mean predicted probabilities, without a diagonal reference. | Plot mean probability against observed frequency on common 0–1 axes, with bin counts. |
| Fitted-effect summary | An endpoint-only sentence asserted a rising/falling pattern even for non-monotone curves. | Remove the unsupported verbal trend; retain the actual curve, units and support. |
| Existing-model workflow | Expert-owned models could only enter the older audit artifact. | Add explicit `evaluate_models()` with stored prediction contracts and honest unavailable training/selection history. |
| Family-retention test | Required a linear model on separated iris classification data even after optimizer nonconvergence. | Require the requested search and an inspectable reason for every omitted family; do not retain invalid fits for a test. |

The messy-data fixture has independent supplied-data answers: 4 training and 20
evaluation weights are missing before imputation; no weights are missing afterward;
9 evaluation service values are novel. Report filters and original-record links
must recover those facts, not merely reproduce their own embedded payload.

## Implemented repairs and final local acceptance

The integrated reports and public examples were refreshed from the frozen
implementation on 2026-09-07. The final local gates below passed on those files.
The walkthroughs started with questions a reader should be able to answer, then
checked the displayed answers against original data, native predictions and
retained fold records. Package and publication acceptance are recorded separately
in the [release evidence](release-0.6.0.md).

| Finding and repair | Verification and current limit |
|---|---|
| Raw training values and source positions were missing. `capture_data_context()` now retains separate raw/processed partitions, a versioned column registry and source-row mapping. It rejects changed raw snapshots or incompatible processed rows. | [Data contract tests](../tests/testthat/test-data-profile.R) independently check imputation, row removal, save/reload and mutation rejection. External fitted models with no supplied training data show training unavailable; the bridge does not copy evaluation rows into a pretend training set. |
| Data exploration was limited to a small processed-input matrix. Summary mode now profiles every included input and target, missingness, common-bin split distributions, target relationships and pair associations. | The [messy-data generator](render-exploration-fixtures.R) retains original-data answers. [The data browser gate](check-data-explorer.py) checks 4/20 missing weights, 9 novel categories, mapped values, histogram geometry, observed binary event rates and signed association against independent original tables. Aggregate summaries remain descriptive and may reveal small groups. |
| The first Data page stacked unrelated work and omitted a real outcome category. It now has persistent Distribution / Relationships / Records subviews, category-sized charts and ten-row pages. | The final data gate completed 209 checks across summary/rows/none, keyboard tasks and 1440/768/390/320px. It opens secondary charts and checks their bounds and rendered labels. Screen-reader review and participant usability are not established by these checks. |
| Ordinary charts depended on a 760px minimum width even on phones. Data charts now use their actual available width and readable native text; only dense categorical matrices use labeled local scrolling. | Element and plot-label checks replaced page-overflow-only acceptance. Original-row inspection was also repaired to wrap long values. No broad cross-browser or accessibility conformance claim follows from the tested Chromium states. |
| Default HTML disclosed individual predictions. `report_data` now has one summary/rows/none policy across data and prediction views; rows are explicit and sampled with recorded scope. | [Report wiring tests](../tests/testthat/test-report-data-wiring.R), [data gate](check-data-explorer.py) and [prediction gate](check-predictions.py) inspect hidden payloads as well as visible content. Export column choices control explorer values, not feature names, fitted model details or explanation results elsewhere. This is not a general redaction or anonymization tool. |
| Filters and error examples could lose the observation's identity. Exported rows retain source keys and processed positions; Predictions links open the matching Records view and clear filters that hide it. | Browser tasks compare displayed raw and imputed values with original source rows. A deliberately swapped source key is rejected by that task; a separately corrupted missing count is rejected by the independent missingness check. Filtered views describe only the embedded sample and leave official scores unchanged. |
| Search choices lacked rationale and nonconvergence could look successful. Selection now shows scheduled tuples, fold evidence, effective settings, policy arithmetic, final refit and omitted-family reasons. Explicit optimizer failure is excluded by default. | [Selection tests](../tests/testthat/test-tuning-selection.R) and the [hand-calculated browser fixture](render-selection-fixture.R) exercise best/one-SE/final-fit differences and failure paths; the final [selection gate](check-selection.py) completed 107 checks, including chosen-family and open-candidate print scope. Scheduled ranges include failed attempts; small default grids and family priorities are engineering choices, not optimality results. |
| Single timing readings invited unsupported speed comparisons. An opt-in repeated benchmark now records a common batch, warmup, calibration, interleaved repetitions, warnings and raw records. | [Benchmark tests](../tests/testthat/test-prediction-benchmark.R) cover a backend that fails permanently during warmup and a clock with no observed positive step. Identity failures preserve raw failed records and withhold costs; unknown resolution also withholds costs. Batch throughput is not deployment latency or a confidence interval. |
| Existing fits could not enter the main report. `evaluate_models()` now accepts explicit prediction contracts, primary/reference choices and optional training context. | [Bridge tests](../tests/testthat/test-evaluate-models.R) and [supplied-model fixtures](render-supplied-models.R) cover model/source identity and class contracts. A separate local evaluation-only data walkthrough checked absent-training labels and row selection. The final supplied-model gate passed 166 checks across single/multiple fits, absent-training states, record links, timing arithmetic and actual PDF output. |
| Binary error and calibration redesigns lost meaningful numbers. Prediction views now retain observed-class probability, all declared class columns, independent cutoff counts and mean-probability calibration coordinates. | The [prediction gate](check-predictions.py) checks every exported binary cutoff against original predictions and tests a valid-JavaScript wrong-model mutation. Summary mode uses aggregates; case examples require row mode. These displays do not authorize selecting thresholds on a final test set. |
| Tutorials and reference navigation lagged behind the implementation. The temporal example now requests comparison mode, the beginner guide includes raw/processed and export tasks, and the reference index includes the existing-model and benchmark entry points. | Executable vignette chunks are checked against the development package. Static/no-JavaScript and export claims are scoped to what is actually available. The three public examples and eight screenshots were refreshed and inspected; the committed-gallery browser gate passed 118 checks. The final installed-archive quickstart and published-site verification remain release tasks. |

## Reader tasks completed on the final reports

| Task | Verified answer or behavior | Evidence |
|---|---|---|
| Explain a useful delivery comparison and inspect its inputs. | The primary has held-out RMSE 3.00 hours versus 7.565 for the baseline. Five parcel weights are missing in training and none in evaluation; preprocessing removes those missing values. Distance/route-time associations are 0.9931 and 0.9900 on the two raw partitions. | [Novice task walkthrough](audits-0.6.0/data-and-workflows.md#implementer-walkthrough-after-the-repair--2026-09-07), with browser answers recorded before reconstructing the source. |
| Trace a large error and explore a subgroup. | Delivery row 79 displays 24.26 hours observed, 15.07 predicted and 9.19 residual; its link opens the correct original record. Monthly-plan filtering gives 27 of 48 churn evaluation rows without changing the official scores. | The novice walkthrough and original-row/data-filter oracles in [the data gate](check-data-explorer.py). |
| Explain a hyperparameter choice, then examine adverse evidence. | The multiclass six-unit network has the lowest CV loss, but the two-unit final network is inside the stated one-SE limit. The omitted multinomial family shows iteration-limit failure in all five folds; successful trees remain inspectable alternatives outside the primary-selection limit. | [Statistical expert walkthrough](results/statistical-walkthrough-0.6.0.md), with independent fold arithmetic and fitted-parameter checks. |
| Compare behavior and inspect the exact models. | Both curves use the same feature, class and axes. Each model exposes its own fitted settings and model-details link; copied finding links restore the intended model, evidence and record after reload. | [Final UI browser record](results/ui-browser-0.6.0.md), including numerical chart oracles and actual dialog/navigation tasks. |
| Change a decision cutoff and understand its consequences. | Raising the churn cutoff from 0.50 to 0.80 changes false positives from 3 to 0 and false negatives from 8 to 13. Counts, case ordering, selected-record decisions and the R call agree; the official probability score and calibration remain unchanged. | Novice walkthrough; 1,923 final [prediction checks](check-predictions.py); a separate 35-check literal-cutoff fixture covers first-level events and quoted model IDs. |
| Bring existing fits and measured costs into the report. | Supplied models retain explicit prediction and reference contracts; absent training or tuning history is labeled unavailable. Repeated-cost medians and quartiles match the raw blocks on their common batch. | [Supplied-model gate](check-supplied-models.py) and statistical expert walkthrough. Batch measurements are not deployment latency. |
| Use grouped/temporal data and reproduce predictions. | Group membership and chronological gaps match original source units; row exclusions remain mapped. Group intervals use six sites, temporal IID intervals are withheld, and deliberately uncomputed explanations are labeled. All eight copied model-specific R calls match native predictions exactly. | [Structured workflow record](results/structured-walkthrough-0.6.0.md), including 41 original-unit checks. |
| Read and navigate the report in different formats. | Focused tabs, keyboard controls, copied links and narrow layouts work in the inspected states. Print retains the selected model/family and opened evidence with readable chart text. Without JavaScript, static evidence remains available and inert Data controls are hidden. | Final UI, data, prediction, selection and supplied-model gates; refreshed screenshots and 118 committed-gallery checks. |

The final main browser gate passed 1,279 checks across regression, binary,
multiclass and quick workflows. The independent hand-calculated chart fixture
passed 63 checks; it is also invoked as one aggregate main-gate check and should
not be added to that count. Deliberately disconnected model selection and false
chart coordinates failed the intended state/numerical assertions with no
JavaScript errors. Separately corrupted missing counts and source keys were
rejected by the data gate. Leaked fold imputation failed its independent
numerical assertions rather than crashing the test process.

These are implementer walkthroughs with independent agents and source oracles,
not a recruited-participant study or evidence of superiority over other packages.
Automated accessibility checks found no violations in the inspected states;
manual review remains necessary for incomplete contrast/ARIA results. Safari,
Firefox and assistive-technology user testing were not performed. Dense advanced
tables still require local horizontal scrolling on small screens. Those limits
do not defer the completed task-based usability work to a future release.

## Remaining release work

- Check the final source archive and installed-package examples, including the
  existing-model workflow, in a fresh library.
- Complete supported-platform, optional-engine and release-quality CI on the
  release revision.
- Publish the verified archive and refreshed website, then check the downloaded
  package, checksums and public reports. Record the exact revision and evidence
  in the release record.
