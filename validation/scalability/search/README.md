# Additive fitting and recommended-search cost

This investigation asks whether the recommended search can become faster without
quietly removing difficult models or deciding settings from the final test set.
The test cases include nonlinear interactions, irrelevant predictors, a skewed
numeric predictor, unusual factor labels, and imbalanced classification.

## Full recommended searches and a rejected policy

The fresh paired runs below use the same training rows, five folds, seed and
30-configuration budget, with `explain = FALSE`. Times cover the public
`autoxplain()` call; they exclude HTML generation and this investigation's result
serialization. Every run retained seven models. Scores are computed separately
on the original, untouched evaluation rows.

| Problem | Published 0.6.2 seconds | Initial candidate seconds | Successful configurations, before / after | Selected evaluation score, before / after |
| --- | ---: | ---: | --- | --- |
| Friedman regression | 412.198 | 65.154 | 30 / 30 | RMSE 1.553636 / 1.553636 |
| Bank classification | 209.985 | 131.370 | 30 / 27 | Log loss 0.271408 / 0.271408 |

Friedman's training selection chose `boosting_02`; Bank chose `regularized_01`.
Their evaluation metrics stayed exactly unchanged. All other retained models'
metrics also stayed unchanged except the additive alternatives. The full tables
retain those differences, every configuration and every failed fold. Family
timings include fitting and validation scoring; they are not native-engine-only
measurements.

The initial automatic rule is not accepted as the final default. It routed
moderate-size binary fits to BAM based on coefficient work, and lost three
otherwise usable Bank configurations: `additive_01` failed in folds 2 and 5,
`additive_02` in fold 5, and `additive_03` in fold 1. Each failure was final PIRLS
nonconvergence. Faster fitting and unchanged selection do not justify losing
valid alternatives. These results remain labeled `candidate` in the raw tables;
the revised policy's Bank replay has the distinct name `candidate_binary_guard`.

The revision keeps the coefficient-work shortcut for Gaussian regression and
uses nested GAM for binary problems below 10,000 outer-training rows. It follows
the native algorithm distinction and the training-fold failures, without using
final evaluation scores to select a policy. mgcv routes Gaussian identity models
through `bam.fit`, while binary models use the iteratively reweighted `bgam.fit`
procedure. Its changing smoothing criterion can cycle. See the
[mgcv convergence explanation](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/gam.convergence.html)
and [native implementation](https://github.com/cran/mgcv/blob/master/R/bam.r).

The final binary-policy replay took **202.443 seconds**, compared with the
published baseline's 209.985 seconds. All **30 configurations and 150 folds**
succeeded. All 150 validation scores, the 30 configuration scores and selection
outcomes, and the seven retained models' complete evaluation prediction arrays
are identical to the published run. All five additive configurations were
planned with GAM, and the retained additive model used GAM. These checks are
recorded in `bank-policy-parity.json`; the initial policy's four failed folds
remain in the evidence tables. This restores useful alternatives with a
comparable runtime; one run does not establish a binary speed improvement.

The Gaussian route is unaffected by this policy revision. A refit from the final
installed snapshot reproduced the saved Friedman additive model's native fitted
fields, effective parameters and training predictions exactly. The wrapper's
task and policy-rationale metadata changed intentionally. The 65.154-second
Friedman result remains its original full-search measurement, rather than a
second timing of the final snapshot. `gaussian-policy-parity.json` records the
scope of that separate check.

This is one sequential run per case and version on an Intel Core Ultra 9 185H,
Ubuntu 26.04.1, R 4.5.2 and OpenBLAS 0.3.32, with native thread counts set to one.
Saved models record the same native versions in both runs: mgcv 1.9-4,
XGBoost 3.2.1.1, ranger 0.18.0, glmnet 5.0 and rpart 4.1.27.
`recommended-engine-versions.csv`, per-run session files and installed-file
manifests record that provenance. There are no timing repetitions or universal
speed claims. The older 532.5-second Friedman observation is a separate run;
412.198 seconds is the paired baseline here.

Portable session-text copies use LF line endings and omit trailing whitespace.
The raw session logs in the cache remain unchanged.

All three Bank runs warn about 26 coincident training/evaluation records. Source row
indices are disjoint, but repeated customers cannot be ruled out because customer
identifiers are absent. This contact-row split does not establish independence
between customers or future-period performance. The warning and original split
are preserved rather than changing the benchmark after seeing its results.

## What the first comparison found

`solver-comparison.csv` retains every result from 14 fixed training folds and
three fitting procedures. The GAM reference uses the published 0.6.2 source
archive identified in `baseline-provenance.json`. All native fits produced finite
predictions. The published optimizer reader then failed on every discrete BAM
fit because its convergence flag is a logical value rather than a list. Those
14 integration failures remain recorded, even though predictions were available.

Continuous BAM took 1.1–1.7 seconds on the four noisy Friedman regression folds,
compared with 6.8–8.3 seconds for GAM. It was not uniformly faster: the first Bank
Marketing fold took 21.6 seconds with BAM and 6.3 seconds with GAM. That BAM fit
also emitted a nonconvergence warning, which the original reader did not treat
as failure; its score is diagnostic only. The two methods produced different
predictions. Similar average losses in these examples are evidence about these
folds, not numerical identity or general equivalence.

Default-resolution discrete BAM was faster, but changed an extreme held-out
prediction by 22.9 units in one skewed regression fold. Its RMSE happened to
improve on that fold. An improvement on one sample is not a reason to hide the
approximation. Discretization therefore needs explicit control and recorded
resolution.

The follow-up at 10,000 bins completed all 14 adapter fits with checked
convergence. On the four skewed regression folds, the largest prediction
difference was 0.014 rather than 22.9. The Bank classification predictions still
differed by up to 0.069, which illustrates that resolution alone cannot make
different fitting algorithms identical. `discrete-10000-comparison.csv` retains
all follow-up scores, including those that became worse.

`larger-and-rare-comparison.csv` adds 20 fits, none reaching the 120-second
process limit. Its original reader labeled all fits converged, but a later
warning audit found a nonconvergence warning in the Bank `k = 8` BAM run. That
original label is unreliable; the table preserves it with an explicit warning
annotation. Some representative paired results are below. The score is RMSE
for regression and log loss for classification; lower is better.

| Training problem | GAM seconds | BAM seconds | GAM score | BAM score |
| --- | ---: | ---: | ---: | ---: |
| Friedman, k = 8 | 25.04 | 2.26 | 1.690590 | 1.690580 |
| Friedman, k = 10 | 38.86 | 2.64 | 1.678807 | 1.678795 |
| Bank, k = 8 (BAM score diagnostic only) | 6.97 | 25.93 | 0.266948 | 0.267404 |
| Bank, k = 10 | 10.96 | 2.30 | 0.265733 | 0.266916 |
| Rare event, seed 912 | 1.13 | 3.55 | 0.092736 | 0.096172 |
| Rare event, seed 913 | 2.12 | 3.15 | 0.073123 | 0.072146 |
| 10,000-row regression | 3.38 | 1.43 | 1.018125 | 1.018124 |
| About 10,000-row classification | 2.49 | 2.95 | 0.597217 | 0.597211 |

The rare-event generator sets expected prevalence to 1%. These particular
training samples contain 27 and 18 events, respectively. Both faster and slower
fits, and both better and worse scores, remain in the table. None of the results
establishes that an additive formula is adequate for an interaction problem.

The eight higher-basis rare-event checks in `rare-bases-comparison.csv` cover
`k = 8` and `k = 10`, which crossed the initial task-agnostic work threshold on
these 2,000 outer-training rows. They ran alongside report generation, so their times are
not a solver-speed comparison. Both better and worse validation losses remain
recorded. More seriously, two BAM fits reached their 200-iteration PIRLS limit
and emitted `algorithm did not converge`, while mgcv also returned generic
`converged = TRUE` and `outer.info = "full convergence"` for a separate
smoothing-parameter step. The initial adapter incorrectly accepted those fits.

The adapter now records warning messages with their calls and source stages.
An explicit final nonconvergence warning from `bgam.fit` or `bgam.fitd`
overrides smoothing-step success. Initialization warnings and transient
`fast.REML.fit` warnings do not establish final failure. An externally fitted
BAM that reaches its recorded PIRLS limit without preserved warning history is
marked unverified. The original incorrect labels and final warnings remain in
the first evidence table; `rare-diagnostic-replay.csv` records the corrected
failure and control replays. Their predictions and serialized native fitted
fields are unchanged. The fix changes what the package is prepared to claim
about those fits, and excludes their failed configurations under the default
optimization policy.

## Automatic choice and explicit control

The default plans each configuration's solver from the task and outer-training
inputs. It uses continuous BAM at 10,000 rows. Gaussian regression also uses BAM
when `rows * estimated_coefficients^2` reaches 10 million; binary problems below
10,000 rows retain nested GAM. The coefficient estimate includes smooth
basis columns and observed factor levels. The threshold is an overridable
computational policy informed by these measurements, not a statistically optimal
cutoff, a convergence guarantee, or a universal prediction of runtime. The
quadratic work term follows
mgcv's documented computational structure. Large factor expansions can still
make either solver expensive.

The planned solver stays fixed across validation folds and the final refit.
Each fitting partition still learns its own imputation, levels, feasible smooth
dimensions and smoothing penalties. The final evaluation set and the outcome
values do not choose the computational method. `input_policy$additive` retains
the planning decisions; each fitted model also records its actual method and
the planning size separately from its fitting size.

Pin the solver when reproducibility of a particular fitting procedure matters:

```r
control <- tuning_control(grids = list(additive = list(
  k = 8, gamma = 1, select = TRUE, solver = "gam"
)))
result <- autoxplain(data, "outcome", learners = "additive", max_models = 1,
                    tuning_control = control)
```

Use `solver = "bam"` for continuous BAM, or `solver = "bam_discrete"` with
`discrete_bins` to request discretization. The default resolution for that
explicit option is 10,000. Older grids containing only `k`, `gamma`, and
`select` remain valid and use the automatic policy.

## Problems fixed beyond timing

The adapter handles both forms of mgcv's convergence record. A failed or missing
BAM-specific diagnostic is not overwritten by its generic `converged = TRUE`
flag. Numeric prediction arrays with one dimension are accepted without
weakening matrix-shape checks. Externally fitted GAMs and BAMs keep their additive
identity, smoothing method and effective degrees of freedom in model details.
The native evidence path recognizes these standard models while continuing to
track custom response links and custom subclasses. Custom `Predict.matrix`
methods, including tensor-product margins, also contribute their code and
captured inputs to the model fingerprint. A focused test changes a custom
smooth's behavior away from the evaluation rows while leaving its predictions
on those rows unchanged; the fingerprint still changes. This uses the existing
trust boundary for functions defined in package namespaces.

Basis-size counting now uses a small prefix only when that prefix proves the
necessary distinct-value bound. Otherwise it checks the full column, including
rare values near the end. This removes avoidable large unique-value allocations
without changing the resulting feasible basis size.

## Reproduce the measurements

The input fixtures come from the fixed, versioned plan described in
[`../../stress-modeling/README.md`](../../stress-modeling/README.md). Prepare those
fixtures first. Large data, fitted models and logs stay in
`~/.cache/autoxplain-scale-0.7.0/search`; only small evidence tables belong here.

Install the published 0.6.2 archive into `search/baseline-library`, then run each
command in a separate process from the repository root. Set
`OMP_NUM_THREADS=1`, `OPENBLAS_NUM_THREADS=1` and `MKL_NUM_THREADS=1`. Copy scripts
into the cache before a measured run so a concurrent edit cannot change a run
partway through. Use an external time limit and record timeouts as failures.

1. `Rscript validation/scalability/search/compare-solvers.R` creates the 14-fold
   comparison, retaining each GAM fit and its validation predictions.
2. `Rscript validation/scalability/search/replay-discrete.R` reuses those saved
   folds with the candidate adapter and 10,000 bins. It keeps the first comparison
   unchanged.
3. `Rscript validation/scalability/search/profile-one.R CASE SOLVER K SEED`
   measures one additional configuration. Supported cases are `bank`, `friedman`,
   `rare_1pct`, `regression_10000`, and `binary_10000`; solvers are `gam`, `bam`,
   and `bam_discrete`. `gam` uses the immutable installed baseline; the other
   methods use the working source. Apply a 120-second cap to each process.
4. `python3 validation/scalability/search/run-recommended.py baseline` runs the
   full public recommended search against the immutable baseline. Run the same
   command with `candidate` after installing the candidate in
   `search/candidate-library`. Both use the original fixed training rows, folds,
   seeds and final evaluation set, with the same 30-configuration budget. Each
   process has a 600-second cap. The runner records the `autoxplain()` call time
   before saving models or producing summaries; process time is a separate
   measurement. `candidate_fixed_gam` provides an optional control that pins
   every additive configuration to the former GAM procedure.
5. `python3 validation/scalability/search/summarize-recommended.py` verifies
   matching fixture hashes, seeds and timer checkpoints, then exports the
   comparison, every retained score, every configuration and failed-fold reasons.
6. After installing the revised policy in `search/candidate-binary-guard-library`,
   run `python3 validation/scalability/search/run-recommended.py candidate_binary_guard bank_marketing`.
   This uses a separate directory and manifests. `verify-gaussian-policy.R`
   checks the unaffected Gaussian route against its saved native fit;
   `verify-bank-policy.R` compares all saved validation scores and prediction
   arrays with the published Bank run;
   `summarize-recommended-environment.R` reads the engine versions captured in
   all retained models. Neither script selects settings from evaluation scores.

The full public comparison measures the combined release changes. Both versions
search 30 configurations, but the candidate's automatic additive procedure
differs from the published default. The recommended portfolio does not include
the neural family; its separate iteration-cap change does not affect these
runs. Equal numbers of configurations do not mean equal computational work.
This comparison cannot
attribute an overall time or score difference to streaming alone; the separate
fixed-procedure parity checks address that question. Every configuration status
and every failed fold is exported alongside all retained models' scores.

The driver verifies the installed candidate against
`candidate-final-installed-sha256.json` before fitting and records the worker's
SHA-256. `candidate-final-source-sha256.json` identifies its corresponding source
snapshot. Later changes to the checkout do not alter these installed runs.
The revised binary-policy replay uses `candidate-binary-guard-library` and
separate source/installed manifests, leaving the rejected candidate intact.

Preprocessing learns from each fitting fold. Validation rows score the fit;
none of these scripts opens the final evaluation data to choose the solver.
The simple synthetic interaction problems deliberately violate the automatic
GAM's additive formula, so solver agreement does not establish model adequacy.

The first two full candidate runs finished fitting all 30 configurations and
saved all seven retained models, then failed in this investigation's JSON
summary writer because a contingency table was not converted to a plain list.
Those failures are retained in `recommended-initial-candidate-operations.json`
and the cache's `recommended/candidate-initial-jsonfailure` directory. Their
exact fitting timers were lost. They are excluded from comparative fitting-time
claims. The stable runner completed both paired cases in the table above. A
serialization-only replay
checked the corrected writer before repeating the expensive fits; its timing is
not benchmark evidence.

## A separate neural optimizer diagnosis

The million-row investigation also checks the ordinary default workflow at
10,000 rows. In that published-package run, several useful neural configurations
were excluded because they reached the fixed 500-iteration limit. The probe in
`probe-neural-iterations.R` constructs only the original training rows and
recreates the original fold assignment and effective fitting seeds. It does not
construct or open the final evaluation dataset. Its 500-iteration replay checks
the original convergence statuses and all available reference scores before
trying the predeclared 2,000-iteration cap.

The follow-up covers all five original folds. All 25 fits converged at 2,000
iterations, compared with 6 at 500. The table uses pooled validation RMSE; every
fold contains 2,000 rows. Scores for nonconverged fits are diagnostic only and
remain ineligible for ordinary model selection.

| Hidden units / decay | Converged folds, 500 | Converged folds, 2,000 | RMSE, 500 | RMSE, 2,000 |
| --- | ---: | ---: | ---: | ---: |
| 2 / 0.03 | 5 | 5 | 1.723956 | 1.723956 |
| 4 / 0.01 | 1 | 5 | 1.171120 | 1.170881 |
| 6 / 0.01 | 0 | 5 | 0.780647 | 0.772083 |
| 8 / 0.001 | 0 | 5 | 1.167666 | 1.144923 |
| 4 / 0.001 | 0 | 5 | 1.370362 | 1.370193 |

The 2-unit control's predictions and weights are identical at both caps on the
checked fold. Some individual validation losses become slightly worse with
more iterations. `neural-iterations.csv` retains both caps, the seeds,
convergence codes and training objectives. These runs shared a functional
testing window with report generation, so their elapsed times support no speed
claim. This is one synthetic regression problem, not a general neural-network
benchmark.

The public neural grid now accepts `maxit`, defaults to 2,000, and still accepts
older grids containing only `size` and `decay`. Simple fits can stop earlier.
Different iteration caps keep the same initialization seed for a fixed
architecture and weight penalty. Nonconverged fits remain excluded by default;
there are no hidden retries. The cap and convergence record appear with the
model's settings. Set `maxit = 500` to retain the previous fitting budget.

The native recorded call now uses symbolic `x` and `y`, with literal settings
and instructions for rebuilding the transformed inputs. The fixed-500 replay
in `verify-neural-500.R` compares predictions and every native field except the
recorded call against published fits, separately from the changed default.

An initial probe setup omitted the guided workflow's imputation defaults and
stopped before fitting. Its log is retained in the cache. The corrected replay
uses the published guided preprocessing settings and verifies the original
finite fold scores. As the [nnet documentation](https://stat.ethz.ch/R-manual/R-devel/library/nnet/html/nnet.html)
explains, convergence code 1 means the iteration limit was reached. Increasing
that limit gives the optimizer more time; it does not guarantee convergence or
better predictions.

## Why the methods need separate labels

GAM uses nested optimization. BAM reduces memory by constructing the model
matrix in blocks, and its generalized-model fitting uses a different iteration
scheme. The mgcv documentation explicitly discusses the speed and convergence
tradeoff. Discrete BAM also approximates covariate values at a specified
resolution. The adapter records the requested solver, actual solver, REML/fREML
method, discretization, and the fitting-data size. Checked convergence is a
numerical diagnostic, not a certificate that the model is appropriate.

Sources: [mgcv BAM documentation](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/bam.html),
[GAM convergence](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/gam.convergence.html),
and [mgcv computational cost](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/mgcv-parallel.html).
