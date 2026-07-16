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
  failure_policy = c("continue", "stop")
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

## Value

An `autoxplain_tuning_control` object for the `tuning_control` argument
of
[`autoxplain()`](https://matt17br.github.io/autoXplainR/reference/autoxplain.md).

## Details

Custom grids must be a named list keyed by learner family. Each family
value may be a data frame (one configuration per row), a list of named
parameter lists, or one named parameter list. Parameter names are exact
adapter contracts: `linear` has no parameters; `regularized` uses
`alpha` and `path_fraction`; `additive` uses `k`, `gamma`, and `select`;
`tree` uses `maxdepth`, `cp`, and `minsplit`; `forest` uses `num.trees`,
`mtry`, `min.node.size`, `sample.fraction`, and `splitrule`; `boosting`
uses `nrounds`, `eta`, `max_depth`, `min_child_weight`, `subsample`,
`colsample_bytree`, `reg_alpha`, and `reg_lambda`; `neural` uses `size`
and `decay`; `kernel` uses `cost`, `gamma_multiplier`, and `epsilon`
(fixed at `0.1` for classification because that backend ignores it);
`neighbors` uses `k`, `distance`, and `kernel`; and `mars` uses `degree`
and `nprune`.

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
Candidate losses and their one-standard-error uncertainty are weighted
by the number of validation rows in each fold; RMSE uses pooled squared
loss with a delta-method standard error on the RMSE scale.

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
#>   grids:      tree
#>   budgets:    linear, tree
#>   fold IDs:   automatic
```
