# Candidate architecture review

Reviewed the uncommitted adaptive-search work against `292846f` on 2026-09-12.
This is a production-code audit with small counterexamples, not predictive
acceptance evidence. The coordinating agent fixed the initial exception-evidence
issue. Subsequent authorized work by this reviewer added the bounded forest CV
budget and corrected multiclass OOF binding, with new tests described below.
Existing tests and frozen candidate installations were not changed.

## Confirmed issue, now fixed

**P2: a failed fit after successful round calibration reported the wrong
attempted configuration.** Before the correction, `fit_tuning_configuration()`
attached fit counters to exceptions but omitted the updated effective settings
and successful calibration record. The fold scorer and final-refit recorder
therefore fell back to the originally requested round cap and seed.

Counterexample: 120 Gaussian rows generated with seed 818, a 50-round cap, and
patience 3. Native XGBoost selected one round after trying four. Its monitoring
RMSE values were 1.123253, 1.166814, 1.255467, and 1.261573. An injected exception
at the subsequent native fixed-round fit observed these arguments:

| Field | Actual attempted fit | Previously retained evidence |
|---|---:|---:|
| Boosting rounds | 1 | 50 |
| Fit seed | 1758360035 | 875593743 |
| Successful calibration curve | Four observations | Missing |
| Calibration/model fit attempts | 1 / 1 | 1 / 1 |

This affected diagnostics and replay of failed configurations; the counters
were already correct. A full-training refit failure had the same defect.
The correction propagates the actual fit specification and any completed
calibration evidence through exceptions. Current relevant locations are
`R/tuning.R:1373`, `R/tuning.R:1427`, the fold error handler, and
`safely_timed_model_fit()` / `refit_tuned_candidates()` in `R/guided_workflow.R`.

## Validation

The new owned file `tests/testthat/test-adaptive-adversarial-review.R` passes
**29 expectations across five tests**, with zero failures, warnings, or skips:

- Native calibration followed by an injected backend failure preserves the
  actual native arguments and complete monitoring curve.
- An actual search with an injected first full-training failure successfully
  falls back. Failed-attempt settings match intercepted native arguments, and
  total fit accounting matches native calls plus the baseline.
- A failure inside calibration preserves its attempted cap and seed, counts
  one calibration and zero scored-model fits, and invents no selected round.
- Binary and multiclass monitoring log loss matches an independent numerical
  calculation over native per-round probabilities when target factor levels
  are deliberately reordered and nonalphabetical.
- Final-round aggregation includes successful skipped-calibration folds at
  their effective cap and excludes failed fold scores.

Run with:

```r
pkgload::load_all(".", quiet = TRUE)
testthat::test_file("tests/testthat/test-adaptive-adversarial-review.R", reporter = "summary")
```

Additional small probes found no further blocker:

- Total budgets 3, 4, 5, 6, 17, 18, and 19 produced exactly that many unique
  configuration IDs across the three tabular families. Every family received
  at least one setting; allocation differed by at most one setting per family.
- Forest growth scaling and fold-specific predictor clamps were inspected
  against native input semantics. The forest uses original predictors, so
  categorical matrix expansion is not its `mtry` width. Boosting representation
  is planned once from outer-training inputs and fitted fold encodings retain
  their own category levels.
- Three-round calibrated boosters serialized to 10,360 bytes at 100 rows and
  10,431 bytes at 1,000 rows. The retained native attributes contain a symbolic
  call, scalar parameters and round records, rather than a callback closure
  holding training/validation matrices. This is a focused retention check,
  not a peak-memory bound for long runs.
- `git diff --check` passed.

## Evidence limits

Existing native early-stopping tests use direct native fits or explicit
probability calculations and are not vacuous. Tests asserting exact policy
anchors demonstrate reproducibility and bounds; they cannot establish that
those anchors are competitive. The new exception tests inject only the failure
boundary and independently intercept real native arguments; they exercise
failure states omitted by the normal successful-fit accounting tests.

Screening and CV remain selection evidence. Report text identifies their
conditional relationship and keeps final evaluation separate. This audit
found no additional substantiated blocker in budget allocation, class ordering,
round aggregation, input-width policy or retained model state. Predictive
quality, long-run peak memory and report usability still require their separate
benchmark/browser evidence. This result applies to the corrected working tree;
it does not retroactively alter the frozen candidate-v1 installation or runs.

## Multiclass OOF assembly, fixed after the v2 development failure

**P1: a completed multiclass search could fail while combining OOF predictions.**
The frozen Covertype v2 development run completed five CV folds, then failed
before final refitting with `length of 'dimnames' [1] not equal to array extent`.
Its process record retains the failure at 615.586 seconds and 1,299,264 KiB peak
RSS. This is an unsuccessful run, with no final-model quality result.

The cause in `R/tuning.R:combine_tuning_predictions()` was `rbind.data.frame()`
combining `AsIs` probability-matrix columns: ranger's matrices lacked row names,
while boosting's carried source row names. R attempted to install incomplete
matrix dimnames. Unequal fold sizes expose the problem, but equal folds can
also fail. The correction binds scalar case records and probability matrices
separately, clears redundant matrix row names, and requires identical class
column ordering. Explicit `source_row` and `training_row` remain authoritative.

`tests/testthat/test-tuning-multiclass-oof-binding.R` passes 77 expectations.
Its native fixture has 507 rows, seven nonalphabetical target levels, a rare
class of five rows, and fold sizes 104/102/101/100/100. Direct ranger and XGBoost
refits independently reproduce every fold's probability values, class order,
case loss, fold score and source identity. Both OOF-retention settings select
and refit identically. A separate check retains regression's zero-column
probability schema and rejects inconsistent class schemas. Focused existing
suites pass 84 tuning-evidence, 113 tuning-selection, 40 search-execution and
58 competition-metric-integration expectations. The new file has no lint
findings. The frozen v2 source and failed artifacts remain unchanged.

## Full-size capacity assessment

This assessment uses development logs and training-model operation probes,
not locked acceptance outcomes. The acceptance pools contain 463,715 Year rows
with 90 predictors and 464,810 Covertype rows with 54 predictors. The resource
gate remains 7,200 seconds and 24 GiB address space per process. RSS measurements
are useful diagnostics, but are not the address-space limit itself.

**Year remains a serious completion risk; neither full-size fitting nor the
complete default report is demonstrated.** Covertype has substantially cheaper
native forests and looks more plausible, conditional on a successful corrected
development run and full-training timing. The independent forest-family gate
also requires a completed native forest reference, so a reference timeout
cannot be excused by a good boosted model.

Measured development operations, all native references using 50,000 training
rows and two 40,000-row calibration fits plus one full-training refit:

| Operation | Threads | Process seconds | Final-refit seconds | Peak RSS KiB |
|---|---:|---:|---:|---:|
| Year native forest | 4 | 685.228 | 342.987 | 1,640,784 |
| Covertype native forest | 4 | 134.712 | 56.396 | 1,606,448 |
| Covertype native forest | 1 | 328.519 | 154.021 | See development record |
| Covertype native boosting | 4 | 224.434 | 63.630 | 738,812 |
| Covertype native boosting | 1 | 211.036 | 57.673 | See development record |

Sources are the corresponding `native-development-4t-v1` and
`native-development-v2` process/summary records under the local benchmark cache,
with portable summaries in [DEVELOPMENT.md](DEVELOPMENT.md). Shared-host timings
are not isolated scaling experiments. In particular, four threads improved the
observed Covertype forest time 2.44-fold, while boosting did not improve.

### Repeated training is the dominant forest cost

The authorized automatic large-data policy keeps all requested CV folds and
all their training rows, uses 256 trees for CV, and retains 500 trees in each
final all-training fit. Screening remains 128 trees on at most 16,000 rows.
Grid search and explicit grids preserve their exact requested tree counts.
The 97 expectations in `test-forest-validation-budget.R` include real small
native regression/binary/multiclass fits with manual seed parity and failed-fit
evidence. Its 50,000-row orchestration check uses a mock backend and establishes
row allocation only; it is not a native performance result.

For one finalist and five equal folds, normalized row-tree work is:

- Previous CV plus final fit: `5 * 0.8 * 500 + 500 = 2500` per training row.
- Current CV plus final fit: `5 * 0.8 * 256 + 500 = 1524`, a **39.04% reduction**.
- Native reference: `2 * 0.8 * 500 + 500 = 1300` before its different `mtry` costs.

The paired search's five forest screening fits add a constant 10.24 million
row-trees. At full Year size that is approximately 1.45% of the 706.70 million
CV-plus-refit row-trees. These are workload proxies, not timing models: `mtry`,
node size, task, cache behavior and tree depth materially change cost.

Even linear scaling of the native Year total by `463715 / 50000` gives
6,355 seconds, leaving about 845 seconds for all departures from linear scaling.
The package repeats 17.23% more normalized forest work than that reference,
although its larger terminal-node policy helps. A separate 40,000-row native
Year forest with `mtry=30`, node size 20 and 500 trees took 443.796 seconds on
two threads. Assuming ideal doubling from two to four threads and linear row
scaling yields `443.796 / 2 * 1.25 * 3.048 * 9.2743 = 7841` seconds for current
forest CV plus refit alone. Applying the measured 128-tree node20-to-node50
timing ratio `82.906 / 88.379` reduces this illustrative figure to 7,355 seconds.
These unmatched extrapolations are risk indicators, not measured bounds or
proof of failure. The full-size automatic node size also differs.

Boosting adds work beyond one fit per fold: successful early-stopped folds
perform an inner calibration and then a fixed-round fit on the complete outer
fold training rows. With the supplied five-way fold grouping, each calibration
uses roughly 60% of the outer pool for fitting and 20% for monitoring, followed
by the 80% scored fit. Calibration callbacks were independently measured to
scale approximately linearly with rounds. There is no evidence here that a
callback rewrite would eliminate the repeated native training cost.

The intended `portfolio="tabular"` default allocates 18 screening settings
across three families, with three finalists, five-fold validation and final
family refits. The paired benchmark allocates ten settings across two families.
A successful paired fit therefore does not alone establish the complete
one-argument tabular workflow's cost.

### Memory and explanations

The fold-major execution retains OOF predictions and compact fit evidence,
not every fitted fold model. The raw full-Year dense predictor matrix is about
318 MiB; preprocessing subsets, encoded matrices, native training buffers and
final model retention can add multiple copies. One sanitized 40,000-row,
500-tree Year forest occupied 108.54 MB with 3.38 million nodes at node size 20
and `mtry=30`, versus 327.33 MB and 10.21 million nodes at node size 5 and
`mtry=9`. Thus larger nodes help retained model memory, while greater `mtry`
still makes fitting slower. Existing peak RSS and these structures make 24 GiB
plausible, but do not prove the full-size address-space gate. Raw reference RDS
sizes also include retained `do.call` arguments, unlike the sanitized package
adapter, so they are not clean comparisons of forest structure size.

The benchmark harness currently calls `autoxplain(..., explain=FALSE)`.
Complete report preparation remains a separate necessary gate. Exact reuse of
baseline predictions and fingerprints removes repeated work, but default
permutation analysis still performs `5 * p + 20 * U` prediction batches per
model on up to 5,000 rows, where `U` is the union of each model's leading eight
features. With Year `p=90` and an illustrative `U=26`, that is 970 batches per
model, before effects and rendering. The recorded development forest precision
probe measured 374.850 seconds for its five-repeat 90-feature screen alone at
5,000 rows, versus 158.371 seconds at 1,000 rows. Its leading eight features
matched, but two of the leading sixteen differed. One model and one nested
sample pair do not justify claiming unchanged explanation precision generally.

### Bounded next steps

1. Finish corrected development fitting, then use training-only full-size
   reference stages to establish native time and address-space feasibility.
   Keep final locked scoring and the combined resource ceiling unchanged.
2. If Year full-size training confirms the margin problem, predeclare a
   training-only comparison of 128 versus 256 CV trees, retaining every fold
   and the full-row 500-tree final forest. Moving CV from 256 to 128 cuts the
   current row-tree proxy another 33.60%. Existing prefix probes motivate this
   experiment but do not prove seed-changing refit or selection equivalence.
   Do not globally force low `mtry`: on Covertype, native calibration log loss
   was 0.48757 at `mtry=7` versus 0.35515 at `mtry=18`.
3. A bounded automatic thread choice is justified for the large automatic
   tabular route: at this assessment's start the public default was one thread
   in `R/tuning_control.R`, so four-thread evidence did not describe the
   simplest call. Cap automatic use at four and at the effective allocation;
   respect explicit thread counts, CPU affinity, scheduler/environment limits
   and conservative fallback when allocation is unknown. Keep outer fits
   sequential and record requested/resolved counts. Core/grid compatibility
   and explicit one-thread reproducibility can remain intact. This is a
   recommendation was subsequently authorized and implemented below; it is
   not a promise of fourfold speed.
4. Preserve the full final forest and the separate forest quality gate.
   Sampling final fitting rows or accepting only the boosted winner would
   change the declared task rather than resolve this operational gap.

Completion, held-out quality, replay and novice/expert report usefulness still
need their declared direct evidence.

## Subsequent operational update and automatic threads

Year v2 timed out at the 1,200-second development gate during its mandatory
50,000-row, 500-tree forest refit, after all five folds and its primary boosted
refit completed. This supplies a direct completion failure in addition to the
scaling risks above. It remains a failed frozen run, not final quality evidence.

The coordinating agent subsequently authorized a bounded automatic native CPU
policy. `tuning_control(threads=NULL)` resolves once after actual outer-training
rows, predictor count and search mode are known. Large automatic adaptive
forest/boosting searches use `min(4L, parallelly::availableCores())`; other
automatic routes use one. The maintained resolver respects CPU allocation,
including affinity, container/scheduler limits and R check constraints.
Explicit positive counts remain unchanged, including one and four. Plan rows
for other backends remain at one. Requested and resolved counts, allocation,
policy and reason are recorded in control and input policy. Replay emits an
explicit resolved count; hardware is absent from parameter keys and seeds.

`test-thread-policy.R` passes 85 expectations, covering documented allocation
hooks for one/two/22 cores, the actual R check cap, exact/grid/small exclusions,
unclamped explicit counts, real tiny native ranger/XGBoost two-thread parity,
native configuration inspection, and one-time resolution with full-row mock
orchestration. Replayed code is evaluated and works without rediscovering CPUs.
No native test exceeds two threads. Existing tuning-control (125), search
execution (40), forest-budget (97), tuning-evidence (84) and multiclass OOF (77)
checks pass. A separate reviewer found no scope, seed or replay defect. The
frozen v2/v3 cohorts and their explicit four-thread interpretation are unchanged.

Given Year v2's timeout, a further subsequently authorized work tier uses
128 CV trees when outer-training `n*p >= 4e6`, preserving 256 for `[1e6,4e6)`.
This coarse fourfold work step activates on Year development (`4.5e6`) and both
full pools, while keeping 256 on Covertype development (`2.7e6`). It is not an
empirically optimal cutoff. The fivefold CV-plus-final proxy falls from 1524 to
1012, saving approximately 1.024 full 500-tree refits' normalized work. Final
500-tree fitting, every requested fold and every fold-training row stay intact.
The narrower tier avoids extra CV approximation where no completion failure
currently requires it: Bank's prefix log-loss gap from 500 rises from 0.000199
at 256 to 0.001682 at 128; Covertype's rises from 0.000095 to 0.001944. Native
prefixes support bounded exploration of this change, but seed-changing separate
fits and selection can differ. The tier alone does not establish a successful
complete workflow or waive the final forest gate.

The implementation and unchanged acceptance boundaries are declared in
[forest-validation-budget-v2.md](forest-validation-budget-v2.md). A new frozen
candidate rerun is required; neither earlier development failures nor the
ongoing native reference runs are reinterpreted.
