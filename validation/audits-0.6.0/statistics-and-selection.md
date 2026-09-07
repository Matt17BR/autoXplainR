# Statistical and selection audit of 0.5.0

Audited baseline: commit `12e9ba1`. Audit date: 2026-09-07. Source references below
refer to that commit, not moving line numbers in the repaired working tree.
Native reproductions used R 4.5.2 and nnet 7.3-20 on Linux. No external LLM was
called. This is an adversarial code and numerical review, not a study of human
comprehension or an estimate of predictive superiority over other tools.

## 1. Explicit optimizer failure is accepted as a successful tuned fit

**P1, demonstrated correctness failure.** `R/tuning.R:752–812` treats an engine
return value without an R error as successful. `fit_tuning_configuration()` at
`R/tuning.R:1122–1162` saves settings and seeds but never checks optimizer status.
The native neural path fixes `maxit = 500` at `R/tuning.R:1200–1206`.

A synthetic regression with 160 rows, 12 independent normal predictors and an
independent normal outcome, seed 739, and the single neural configuration
`size = 8, decay = 0.001` produced four fits. The three fold fits used 85, 85 and
86 training rows; the final fit used 128. All four returned
`model$convergence == 1`. All fold warnings/errors were empty. Fold RMSEs were
2.109808, 1.861003 and 2.102108; the candidate was `status = "ok"`, selected, and
its full-training refit was also `ok`.

This is an explicit iteration-limit outcome: the official nnet documentation
defines convergence code 1 as reaching the iteration budget. A finite prediction
does not prove successful optimization. Conversely, successful optimization
would not establish good generalization. [nnet documentation](https://stat.ethz.ch/R-manual/R-devel/library/nnet/html/nnet.html).

Baseline reproduction (run with the baseline package loaded):

```r
set.seed(739)
d <- as.data.frame(matrix(rnorm(160 * 12), 160, 12))
d$y <- rnorm(160)
fits <- list()
trace(AutoXplainR:::fit_tuned_neural_network, exit = quote({
  .GlobalEnv$fits[[length(.GlobalEnv$fits) + 1L]] <- list(
    rows = nrow(data), convergence = returnValue()$model$convergence
  )
}), print = FALSE)
r <- autoxplain(
  d, "y", learners = "neural", nfolds = 3, seed = 739, explain = FALSE,
  tuning_control = tuning_control(
    grids = list(neural = list(size = 8L, decay = 0.001)),
    family_budgets = c(neural = 1L)
  )
)
untrace(AutoXplainR:::fit_tuned_neural_network)
fits
r$tuning$fold_scores[c("fold", "score", "warning", "error")]
r$tuning$candidates[c("configuration_id", "status", "selected", "refit_status")]
```

**User impact:** an unsuccessful optimization can determine both the selected
configuration and the report's fitted primary model without any visible caveat.

**Required correction and acceptance:** retain native termination status before
discarding fold models; explicitly decide eligibility; preserve the failure
through refit fallback and reports. An optional retain-with-warning policy must
remain distinguishable from convergence. The fixture above must be excluded by
default or explicitly labeled under an opt-in policy. Check GAM/GLM/glmnet native
status where available, and record unavailable status as unknown. Do not invent
convergence guarantees for fixed-round boosting, forest construction or engines
that do not expose a checked diagnostic.

### Does exclusion remove useful default neural candidates?

A separate, post-repair diagnostic run used the unchanged default core grid,
15 candidates and three folds. It did not increase the iteration budget.

| Data | Valid neural configurations | `neural_05` failed folds | Neural retained? |
|---|---:|---:|---|
| `mtcars`, target `mpg` | 6 / 7 | 3 / 3 | Yes |
| `iris`, target `Species` | 7 / 7 | 0 / 3 | Yes |
| Smooth quadratic regression, n = 180 | 6 / 7 | 2 / 3 | Yes |
| Nonlinear binary classification, n = 180 | 6 / 7 | 1 / 3 | Yes |

The fifth neural preset is exactly `size = 8, decay = 0.001`. The iris unpenalized
multinomial reference also had unsuccessful termination in two folds. This audit
did not establish the cause of that multinomial failure; separation and numerical
conditioning must not be diagnosed from the status code alone.

The two synthetic representative datasets were generated as follows; all calls
used `autoxplain(d, target, nfolds = 3, explain = FALSE)` with its default seed:

```r
set.seed(91)
smooth <- data.frame(x = runif(180, -2, 2), z = rnorm(180))
smooth$y <- smooth$x^2 + 0.4 * smooth$z + rnorm(180, sd = 0.12)
set.seed(41)
binary <- data.frame(x = rnorm(180), z = rnorm(180))
binary$event <- factor(ifelse(binary$x^2 + binary$z > 0.7, "yes", "no"))
```

These four cases show that exclusion does not routinely erase the neural family
in these fixtures. They do not justify universal adequacy of 500 iterations, nor
an unbounded retry policy. The fixed budget and any caller override should be
visible. Increasing iterations does not itself resolve statistical separation.

## 2. A capacity proxy is described as identifying the least flexible setting

**P1, demonstrated semantic overclaim, not a broken numerical comparison.**
`R/tuning.R:218–229` first defines eligibility as
`CV <= best CV + best candidate SE`. `R/tuning.R:580–598` then prefers a family
and minimizes its capacity proxy, with score and configuration ID breaking ties.
`R/tuning.R:1293–1297` calls the outcome the “least-flexible” near-best setting.

The default family preference is:

`linear → regularized → additive → tree → mars → neighbors → forest → kernel → neural → boosting`.

This is a package preference. Neural capacity counts potential weights but omits
weight decay (`R/learner_registry.R:663–665`); tree capacity omits `cp`
(`R/learner_registry.R:659–660`). GAM basis capacity similarly cannot summarize
all smoothing and selection penalties. Equal capacity values therefore do not
prove equal fitted flexibility or regularization.

Hand-verifiable counterexample:

```r
candidates <- data.frame(
  family = "neural", simplicity_rank = 9L, complexity_proxy = 7,
  cv_score = c(1, 1.05),
  configuration_id = c("unregularized", "strong_decay")
)
AutoXplainR:::select_one_se_candidate(candidates, c(TRUE, TRUE))
# 1: the lower-score unregularized candidate wins a capacity tie.
# Both have the same size; imagine their decay values are 0 and 10.
```

This result is consistent with the code. It does not support the prose claim.
There is also no general theorem making the package's family order an ordering
of statistical simplicity. The glmnet one-SE convention is defined along a
regularization path; it does not supply such a theorem across unrelated model
families. [glmnet documentation](https://glmnet.stanford.edu/articles/glmnet.html).

**Acceptance:** state the exact preference and proxy, allow an explicit caller
preference, and do not describe the result as universally least flexible. Show
lowest CV, policy-selected, and final-refit configurations separately.

An independent numeric fixture is `best = 1, SE = .2`, tree `1.1`, linear `1.21`:
the threshold is `1.2`; neural and tree are eligible, linear is not, and the
default family preference picks tree. A second candidate tied at score 1 but
with SE .01 must not change the recorded threshold if the original first
minimum supplied SE .2. Record the decision before sorting candidate rows.

## 3. Exact preset values, coverage and rationale are absent from the report contract

**P2, demonstrated information gap and a bounded design choice.**
`R/learner_registry.R:578–607` contains hand-written tree and neural tuples.
`R/tuning.R:440–470` allocates ordered prefixes round-robin. The default core
budget of 15 gives one linear, seven tree and seven neural configurations,
although the latter two grids each contain 12 tuples.

For example tree `minsplit` is `max(4, round(n * fraction))`; the fractions are
0.20, 0.12, 0.08, 0.05, 0.20, 0.08, 0.05, 0.03, 0.08, 0.05, 0.03, 0.02.
Several controls vary together. A selected maximum depth at a range edge does
not isolate a depth effect because its `cp` and minimum split may differ too.
The official rpart documentation explains those controls, not the optimality of
these package-specific tuples. [rpart controls](https://stat.ethz.ch/R-manual/R-devel/library/rpart/html/rpart.control.html).

There is nothing intrinsically incorrect about a bounded preset search. The
missing explanation prevents users from assessing what was not tried and what
to investigate next. General research supporting random search as a useful
baseline does not justify these exact numbers. [Bergstra and Bengio, 2012](https://www.jmlr.org/papers/v13/bergstra12a.html).

**Acceptance:** retain the grid actually offered and scheduled, counts not
tested, origin (package or caller), parameter meanings, fixed controls, exact
budget and engineering rationale. Show numeric edge positions as possible next
experiments without predicting benefit. Provide an executable custom-grid
control containing the actual tuples; require the original data/preprocessing/
validation context for full replay. Do not silently reconstruct old runs from
new package defaults.

## 4. Family representatives are labeled with the global selection rule

**P2, demonstrated report mismatch.** The primary refit order at
`R/guided_workflow.R:719–745` starts with the global policy choice; alternatives
at `R/guided_workflow.R:765–795` instead use lowest within-family CV, with refit
fallback. `R/report_model_specs.R:237–240` prints the same global rule for every
tuned retained model. Thus a one-SE run incorrectly implies that each alternative
was selected by a within-family one-SE procedure.

**Acceptance:** label global choice, fallback primary and within-family
representatives by their actual selection roles. A candidate may be a family's
minimum-CV configuration without being the global policy choice, and vice versa.

## 5. Useful fold evidence exists but is hidden; learned optimizer values are lost

**P2, demonstrated schema/report gap.** The report's candidate table at
`R/reporting.R:192–204` includes score, SE and requested settings, but no
individual fold scores, fold sizes, effective clamps, seeds or warnings/errors.
Most of these already exist in `fold_scores`. The aggregate export at
`R/evidence_summary.R:32–36` reduces selection to primary ID, method and seed.

Learned values have a second problem: fold fits are discarded without retaining
the actual lambda/path length, encoded input count, effective kernel gamma,
fitted structure, or optimizer termination. For example glmnet learns a path
then maps `path_fraction` to its actual available length
(`R/learner_backends.R:40–68`), while the kernel gamma divides a multiplier by
encoded input count (`R/learner_backends.R:419–450`). The same requested tuple
can therefore describe different numerical models across folds, correctly, but
the report cannot explain that difference.

**Acceptance:** an aggregate selection API and report should expose the existing
fold records plus retained learned values. Distinguish requested settings,
effective adapter settings and learned quantities. Show fold variability as
observed variability, not confidence bars; preserve all failed configurations
and refit attempts. Rendering must not refit or rescore models. Full row-level
OOF evidence should remain separately available under its existing contract.

## 6. Single, timer-resolution-limited prediction timings invite unstable ranking

**P2, demonstrated measurement limitation.** `R/guided_workflow.R:915–917` times
one prediction call. That single number becomes a leaderboard/cost coordinate
at `R/guided_workflow.R:981–985`. Forty repeated calls to the identical fitted
model and evaluation batch in one native audit run produced 20 values of 0 ms,
19 of 1 ms and one of 3 ms. This is evidence of unstable resolution-scale
measurement, not a cross-model speed comparison. Exact counts vary by machine
and load. R documents platform-dependent timer granularity.
[proc.time documentation](https://stat.ethz.ch/R-manual/R-devel/library/base/html/proc.time.html).

**Acceptance:** use warm, repeated batch measurements with enough accumulated
elapsed time; retain repetitions, batch size, timer resolution, quantiles and
seed/context. Report normalized per-row cost only with the measured batch
context. Mark measurements below effective resolution instead of claiming zero
cost. Separate final-fit time, aggregate CV/refit work and prediction cost.
Intervals from repeated timings describe machine measurement variability; they
are not deployment latency guarantees.

## 7. Fold SE and evaluation uncertainty require distinct scopes

**P2, statistical limitation; no demonstrated arithmetic bug.** The row-weighted
loss and pooled RMSE implementation with its delta-method SE is coherent as a
selection heuristic. It is not an ordinary independent-replicate confidence
interval: training folds overlap. General CV variance has important limits;
there is no universal unbiased variance estimator from an ordinary K-fold run.
[Bengio and Grandvalet, 2004](https://www.jmlr.org/papers/v5/grandvalet04a.html).

For folds with RMSE 1 and 3 and validation sizes 1 and 3, pooled RMSE is
`sqrt(7)`, not `mean(c(1, 3))`. Under the implementation's weighted convention,
the corrected squared-loss variance is 32, effective fold count is 8/5 and
the RMSE selection SE is `sqrt(20)/(2*sqrt(7))`. This is a useful independent
acceptance case, without granting an inferential interpretation to the SE.

The existing paired evaluation bootstrap is better suited to a visible
primary-minus-baseline difference, conditional on fixed fitted models. It is
independently tested against stored error draws and group sampling. It omits
fitting/tuning uncertainty, assumes independent observations or independent
groups, is unreliable for small/degenerate samples, and rejects temporal IID
inference. In particular, an interval for the training-selected primary must not
be attached to an alternative chosen because it had the lowest observed holdout
score. Repeatedly selecting on evaluation performance compromises that
evaluation. [Cawley and Talbot, 2010](https://jmlr.org/papers/v11/cawley10a.html).

**Acceptance:** expose the paired fixed-primary/baseline comparison and sampling
unit count; keep small-unit and degenerate-distribution notes near intervals.
Do not infer equivalence from an interval containing zero, or change the
training selection based on these intervals. External data do not prove IID
sampling; that remains an explicit design assumption.

## What withstood scrutiny

- Outer evaluation rows are separated from local candidate tuning. Fold-local
  preprocessing is actually relearned rather than inherited from the holdout.
- Requested/effective parameter keys and stable configuration seeds preserve
  reproducibility when a fold-dependent clamp makes two requests identical.
- OOF row identities, omissions and complete-candidate coverage are checked.
- RMSE aggregates squared losses before taking a square root.
- Refit failure is isolated and does not discard usable other-family models.
- Fixed-model paired bootstrap and permutation Monte Carlo intervals already
  have different, appropriately limited uncertainty interpretations.

These strengths do not establish superiority over established modeling tools
or that the report changes real user decisions. Human usefulness remains an
empirical question requiring a separately executed user study.

## Implementation acceptance evidence after the baseline audit

The baseline findings above remain distinct from this subsequent verification.
The revised selection page has an independent synthetic arithmetic fixture in
`validation/render-selection-fixture.R`. Its declared fold losses 1 and 3 and sample
sizes 10 and 30 give pooled RMSE `sqrt(7)` and SE `sqrt(20)/(2*sqrt(7))`; a lower-capacity
tree is the policy choice and a recorded failed refit makes another tree the
final primary. `validation/check-selection.py` checks the exported source,
rendered cards, every fold and pooled dot against numeric-axis interpolation,
the threshold, requested settings, exact opened candidate, mobile tables,
no-JavaScript fallback and actual PDF font sizes. It does not call package
statistical helpers to construct expected values. The first completed run had
104 checks and no failures at 1440, 390 and 320 px; PDF labels are checked at 8 pt
minimum. Synthetic fixture values are labeled as such and are not model results.

The optional repeated prediction benchmark has deterministic arithmetic, common
batch/RNG, budget, error and identity tests in `test-prediction-benchmark.R`.
Native timing smoke checks assert record consistency rather than a speed ranking.
The benchmark retains warmup, adaptive calibration and interleaved measured
blocks; reports the empirical observed clock step and processed batch scope;
and withholds aggregate costs when resolution is unknown/limited, fewer than
two repetitions finish, prediction fails or final model identity is unverifiable.
A custom predictor that permanently fails only on the sampled rows 2, 3 and 6 retains
its failed measurements even when the later full-data identity probe also fails.
Repeated quartiles describe observed variation and do not establish deployment
latency or a confidence interval. Single recorded fit/prediction timings remain
separate evidence with their original scope.

Scheduled grid bounds include unsuccessful fits. A depth 1 failed / depth 2 valid /
depth 3 failed acceptance case verifies that the mathematically interior scheduled
winner is accompanied by an explicit statement that only one configuration
completed every fold. The report does not imply successful coverage around it.

### Additional integration counterexamples and acceptance checks

An exact decimal cutoff counterexample was reproduced during the final supplied-model
review: `seq(0, 1, by = .01)[58]` exceeds the floating-point value of the literal
`.57`. Consequently, a prediction of `.57` was treated as negative at the displayed
cutoff 0.57, contrary to the documented `prediction >= cutoff` rule. Ten of the
101 displayed decimal boundaries were affected. The report grid now uses
`(0:100) / 100`, and the public diagnostic default uses `(2:18) / 20`; explicitly
supplied numeric cutoffs retain their exact values. An independent test parses
the decimal labels rather than recreating the former sequence. The dedicated
test file completed 205 assertions, including all 101 exact ties. The durable
browser fixture also checks every cutoff and keyboard movement from 0.56 to 0.57.

Reusing an actual 0.5.0 result with explicit explanation recomputation exposed
another demonstrated integration bug: the missing `tuning$selection` partially
matched the older atomic `selection_rule`. Exact lookup now represents the
unrecorded original selection decision as unavailable, preserving candidate
results without reconstructing an invented threshold. Regression, binary and
multiclass 0.5.0 result files all rendered after explicit recomputation; a durable
old-schema omission fixture exercises this behavior with partial-match warnings
enabled. The incompatible retained explanation fingerprint still correctly
requires recomputation.

Benchmark attachment now checks the captured measurement payload as well as model
and evaluation identity. Independent mutation tests alter the reported median,
raw timing, units and sampled-row identity and verify rejection. This detects
accidental inconsistency; it is not a digital signature. An RDS round trip retains
validity. The dedicated benchmark tests completed 58 assertions. The timed scope
includes transformations performed inside a supplied prediction function; the
guided workflow's saved raw-data recipe is applied outside that timed call.

`validation/check-supplied-models.py` independently reads saved raw measurements
and calculates elapsed-time / iterations / batch-size costs, medians and quartiles.
It checks the report table and cost-axis coordinates, unavailable statuses and
warnings, the explicit reference exclusion, absolute prediction distances and
actual exported-row navigation. Summary and no-row exports are checked for the
absence of row links and private benchmark sampling indices. The latest completed
native fixture run passed 166 checks, including the literal-cutoff fixture and
actual PDF text sizes of at least 8 pt. The repeated-measurement table now has five
columns; any incomplete status, reason or warning appears in a separate issues
table. These checks establish consistency of recorded evidence and presentation,
not a benchmark speed ranking or demonstrated user benefit.
