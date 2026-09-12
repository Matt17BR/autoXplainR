# Advanced controls for local model tuning

`tuning_control()` is an optional escape hatch for users who need more
control than the beginner defaults in
[`autoxplain()`](https://matt17br.github.io/autoXplainR/reference/autoxplain.md).
Omitting it preserves AutoXplainR's existing portfolio-aware search,
stratified/random V-folds, default loss, retained out-of-fold
predictions, and failure isolation.

## Usage

``` r
tuning_control(
  grids = NULL,
  family_budgets = NULL,
  fold_ids = NULL,
  metric = c("auto", "rmse", "mae", "log_loss", "brier"),
  retain_oof = TRUE,
  failure_policy = c("continue", "stop"),
  optimization_policy = c("exclude", "warn"),
  family_priority = NULL
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

  Selection loss. `"auto"` resolves to RMSE for regression and log loss
  for classification. Regression also supports `"mae"`; classification
  also supports `"brier"`.

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
weighted by the number of validation rows in each fold; RMSE uses pooled
squared loss with a delta-method standard error on the RMSE scale.
Overlapping training folds mean that this heuristic is not a confidence
interval or a test of equivalent model performance.

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
#>   OOF rows:   retained
#>   failures:   continue
#>   optimizer:  exclude
#>   grids:      tree
#>   budgets:    linear, tree
#>   fold IDs:   automatic
```
