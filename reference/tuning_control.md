# Advanced controls for local model tuning

`tuning_control()` is an optional escape hatch for users who need more
control than the beginner defaults in
[`autoxplain()`](https://matt17br.github.io/autoXplainR/reference/autoxplain.md).
Omitting it uses AutoXplainR's portfolio-aware search, stratified/random
V-folds, default loss, retained out-of-fold predictions, and failure
isolation.

## Usage

``` r
tuning_control(
  grids = NULL,
  family_budgets = NULL,
  fold_ids = NULL,
  metric = c("auto", "rmse", "mae", "rmsle", "log_loss", "brier", "auc"),
  retain_oof = TRUE,
  failure_policy = c("continue", "stop"),
  optimization_policy = c("exclude", "warn"),
  family_priority = NULL,
  search = c("auto", "grid", "adaptive"),
  threads = NULL,
  screening_rows = 20000L,
  finalists_per_family = 1L,
  time_limit = NULL,
  early_stopping = NULL,
  patience = 30L
)
```

## Arguments

- grids:

  Optional named per-family custom grids.

- family_budgets:

  Optional named positive integer counts, one for every requested
  learner family.

- fold_ids:

  Optional atomic vector assigning every training row to one supplied
  V-fold.

- metric:

  Selection score. `"auto"` resolves to RMSE for regression and log loss
  for classification. Regression also supports `"mae"` and `"rmsle"`
  (root mean squared log error, requiring nonnegative outcomes and
  predictions). Classification also supports `"brier"`; binary
  classification supports `"auc"`, recorded as `roc_auc` and maximized.
  All other supported scores are minimized. RMSLE never clips negative
  values to make a fit eligible.

- retain_oof:

  Retain row-level out-of-fold predictions and case losses.

- failure_policy:

  `"continue"` records a failed configuration and keeps searching;
  `"stop"` aborts on the first resampling or refit failure.

- optimization_policy:

  `"exclude"` excludes an explicit unsuccessful optimizer termination
  from selection and refitting. `"warn"` retains it with a recorded
  warning. An unavailable convergence diagnostic is recorded as unknown,
  not treated as proof of convergence.

- family_priority:

  Optional character vector ordering every requested family for the
  one-standard-error policy, from most to least preferred. This is a
  user preference, not a statistical ordering of algorithms.

- search:

  `"auto"` screens settings before full cross-validation for a
  forest/boosting portfolio (optionally with regularized regression), at
  least 200 training rows and at least two settings per requested
  family. Other portfolios, custom grids and exact budgets retain
  `"grid"` search. `"adaptive"` explicitly requests screening with
  package-generated settings; `"grid"` evaluates every scheduled
  configuration on every fold. When outer-training rows times predictor
  count reaches one million, adaptive forests use 256 trees per complete
  CV fit, dropping to 128 at four million rows times predictors.
  Automatic final refits use 500 trees below four million and 256 at or
  above that threshold. The final count is planned from input size
  before fitting; it is not a convergence test. Fewer trees can change
  predictions and explanations. The tree budget does not remove training
  rows, and final fits retain native out-of-bag diagnostics. Explicit
  preprocessing rules can still omit rows. The smaller validation forest
  is an approximation. Screening uses 128 trees. Grid search keeps each
  requested tree count in CV.

- threads:

  Native threads per XGBoost or ranger fit. `NULL` (default) uses up to
  four available cores for automatic adaptive searches when
  outer-training rows times predictors reaches one million. Availability
  is resolved once using
  [`parallelly::availableCores()`](https://parallelly.futureverse.org/reference/availableCores.html),
  respecting process, scheduler and R check limits. Other searches use
  one thread automatically. A positive integer overrides this choice and
  is not clamped. Fits run sequentially; this does not parallelize folds
  or other backends. Reports retain the resolved count, and their replay
  code requests it explicitly.

- screening_rows:

  Maximum total rows used for the common training and assessment split
  during adaptive screening. Full validation and refitting use all their
  training rows.

- finalists_per_family:

  Successful settings per family promoted from screening to full
  cross-validation. Defaults to one; fewer advance if fewer succeed.
  Increasing this costs more complete folds and offers a check on the
  ranking from the smaller screening sample.

- time_limit:

  Optional search scheduling budget in seconds. An in-progress fit is
  allowed to finish. At least one complete cross-validation candidate is
  attempted; unfinished candidates cannot win. Final refits,
  explanations and reporting are outside this budget. This is not a hard
  process timeout. Time-limited searches can choose different models on
  different machines.

- early_stopping:

  Whether to choose boosting rounds on a separate split inside each
  training fold, then fit all fold-training rows at those rounds. `NULL`
  enables this for adaptive searches only. Final refits use the
  rounded-up median of successful folds' round choices. Assessment and
  final evaluation rows never choose the stopping point.

- patience:

  Boosting rounds without improvement before inner calibration stops.
  The requested `nrounds` remains a maximum.

## Value

An `autoxplain_tuning_control` object for the `tuning_control` argument
of
[`autoxplain()`](https://matt17br.github.io/autoXplainR/reference/autoxplain.md).

## Details

Custom grids must be a named list keyed by learner family. Each family
value may be a data frame (one configuration per row), a list of named
parameter lists, or one named parameter list. Parameter names are exact
adapter contracts: `linear` has no parameters; `regularized` uses
`alpha` and `path_fraction`; `additive` uses `k`, `gamma`, and `select`,
with optional `solver` (`"auto"`, `"gam"`, `"bam"`, or `"bam_discrete"`)
and `discrete_bins` (default 10000). `gam` uses nested REML
optimization; `bam` uses fREML and builds its design in blocks;
`bam_discrete` discretizes numeric covariates at the recorded
resolution. The final fit and each training fold record their actual
method. `auto` plans each configuration's solver once from the task and
outer-training inputs: continuous BAM at 10,000 rows; Gaussian
regression also uses BAM when rows times estimated coefficients squared
reaches 10 million. Binary classification below 10,000 rows retains
nested GAM because iteratively weighted BAM fitting can fail to converge
on smaller samples. That solver stays fixed through validation and
refitting; preprocessing and smoothing are still learned separately
within each fitting partition. This is a computational policy, not a
guarantee of faster fitting or equivalent predictions. Discretization is
never automatic. Omitting the new controls preserves valid older custom
grids. `tree` uses `maxdepth`, `cp`, and `minsplit`; `forest` uses
`num.trees`, `mtry`, `min.node.size`, `sample.fraction`, and
`splitrule`; `boosting` uses `nrounds`, `eta`, `max_depth`,
`min_child_weight`, `subsample`, `colsample_bytree`, `reg_alpha`, and
`reg_lambda`, with optional `encoding` (`"auto"`, `"matrix"`, or
`"native"`). Native encoding uses categorical partitions and a quantized
training matrix; it can change predictions. Auto switches when
categorical expansion exceeds twice the input width and 50 million
estimated matrix cells. This choice is planned once from the
outer-training inputs and fixed across folds and refits; category levels
and preprocessing are still learned within each fold. Each fit records
the choice. `neural` uses `size` and `decay`, with optional `maxit`
(default 2,000). This is the optimizer iteration limit, not a promise of
convergence. Changing only this limit preserves the initialization seed;
earlier two-parameter grids remain valid. Set `maxit = 500` to retain
the former iteration budget. `kernel` uses `cost`, `gamma_multiplier`,
and `epsilon` (fixed at `0.1` for classification because that backend
ignores it); `neighbors` uses `k`, `distance`, and `kernel`; and `mars`
uses `degree` and `nprune`.

Family budgets are exact positive configuration counts. Their names must
exactly match the learner families requested from
[`autoxplain()`](https://matt17br.github.io/autoXplainR/reference/autoxplain.md).
When they are supplied, leave `max_models = NULL`; the sum of the
budgets replaces the automatic portfolio budget.

Supplied fold IDs define ordinary V-fold resampling: each row is
validated once after fitting on all rows assigned to every other fold.
They are not a rolling-origin or forward-chaining specification. To
avoid ambiguity after AutoXplainR's automatic holdout split, fold IDs
are accepted only when `test_data` is supplied explicitly to
[`autoxplain()`](https://matt17br.github.io/autoXplainR/reference/autoxplain.md).
Candidate losses and their selection standard-error heuristic are
weighted by the number of validation rows in each fold; RMSE and RMSLE
use pooled squared losses with a delta-method standard error on the
root-loss scale. AUC uses a row-weighted average of within-fold AUC
values, without comparing prediction ranks across different fitted
models. It has no row-level case loss. Overlapping training folds mean
that this heuristic is not a confidence interval or a test of equivalent
model performance.

## Examples

``` r
control <- tuning_control(
  grids = list(tree = data.frame(
    maxdepth = c(2L, 5L), cp = c(0.02, 0.002), minsplit = c(12L, 6L)
  )),
  family_budgets = c(linear = 1L, tree = 2L),
  metric = "mae"
)
control
#> <AutoXplainR tuning control>
#>   metric:     mae
#>   search:     auto
#>   threads:    automatic per native fit
#>   screening:  limits: 20,000 rows; 1 finalist per family (if adaptive)
#>   early stop: automatic for adaptive boosting only
#>   patience:   30 rounds without improvement
#>   time limit: none
#>   OOF rows:   retained
#>   failures:   continue
#>   optimizer:  exclude
#>   grids:      tree
#>   budgets:    linear, tree
#>   fold IDs:   automatic
```
