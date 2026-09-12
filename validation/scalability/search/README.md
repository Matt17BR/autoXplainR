# Additive fitting and recommended-search cost

This investigation asks whether the recommended search can become faster without
quietly removing difficult models or deciding settings from the final test set.
The test cases include nonlinear interactions, irrelevant predictors, a skewed
numeric predictor, unusual factor labels, and imbalanced classification.

## What the first comparison found

`solver-comparison.csv` retains every result from 14 fixed training folds and
three fitting procedures. The GAM reference uses the published 0.6.2 source
archive identified in `baseline-provenance.json`. All native fits produced finite
predictions. The published optimizer reader then failed on every discrete BAM
fit because its convergence flag is a logical value rather than a list. Those
14 integration failures remain recorded, even though predictions were available.

Continuous BAM took 1.1–1.7 seconds on the four noisy Friedman regression folds,
compared with 6.8–8.3 seconds for GAM. It was not uniformly faster: the first Bank
Marketing fold took 21.6 seconds with BAM and 6.3 seconds with GAM. The two methods
also produced different predictions. Similar average losses in these examples
are evidence about these folds, not numerical identity or general equivalence.

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

`larger-and-rare-comparison.csv` adds 20 fits, all with checked convergence and
none reaching the 120-second process limit. Some representative paired results
are below. The score is RMSE for regression and log loss for classification;
lower is better.

| Training problem | GAM seconds | BAM seconds | GAM score | BAM score |
| --- | ---: | ---: | ---: | ---: |
| Friedman, k = 8 | 25.04 | 2.26 | 1.690590 | 1.690580 |
| Friedman, k = 10 | 38.86 | 2.64 | 1.678807 | 1.678795 |
| Bank, k = 8 | 6.97 | 25.93 | 0.266948 | 0.267404 |
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
`k = 8` and `k = 10`, which cross the automatic work threshold on these 2,000
outer-training rows. They ran alongside report generation, so their times are
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

The default plans each configuration's solver from outer-training inputs. It
uses continuous BAM at 10,000 rows or when `rows * estimated_coefficients^2`
reaches 10 million, and GAM otherwise. The coefficient estimate includes smooth
basis columns and observed factor levels. The threshold is an overridable
computational policy informed by these measurements, not a statistically optimal
cutoff or a universal prediction of runtime. The quadratic work term follows
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
claims and the stable runner reruns both cases. A serialization-only replay
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
