# Compare one fitted feature effect across retained models

`compare_model_effects()` evaluates the same feature, prediction target,
evaluation rows, and effect grid for every selected model. It is the
feature-level companion to
[`compare_model_behavior()`](https://matt17br.github.io/autoXplainR/reference/compare_model_behavior.md):
use it when the question is not only whether models predict differently,
but how their fitted response to a named input differs.

## Usage

``` r
compare_model_effects(
  result,
  feature,
  models = NULL,
  method = c("ale", "pdp"),
  class = NULL,
  n_points = 20L,
  quantile_range = c(0.05, 0.95),
  sample_size = 1000L,
  seed = 123L,
  min_support = 0.1,
  performance_tolerance = NULL
)
```

## Arguments

- result:

  An `autoxplain_result` containing at least two retained, non-baseline
  models.

- feature:

  One predictor name shared by the retained explainers.

- models:

  Model IDs, indices, or `NULL`. `NULL` selects retained non-baseline
  models.

- method:

  `"ale"` or `"pdp"`.

- class:

  For multiclass results, the probability class to compare. Defaults to
  the first outcome level.

- n_points:

  Number of ALE bins or PDP grid points.

- quantile_range:

  Numeric length-two range for a numeric PDP grid.

- sample_size:

  Maximum common evaluation rows used for PDP. `NULL` uses all rows.

- seed:

  Reproducible PDP sampling seed.

- min_support:

  Minimum relative empirical support, from zero to one, for a grid point
  to enter pairwise summaries.

- performance_tolerance:

  Optional non-negative relative performance gap from the best selected
  model.

## Value

An `autoxplain_model_effects` object containing aligned `curves`,
pairwise `comparisons`, model performance context, failures, findings,
and interpretation scope. `n_evaluation_rows` counts the common
evaluation set; `n_effect_rows` counts rows that actually entered each
effect estimate after reproducible PDP sampling or ALE missing-value
exclusion. `models` retains every requested model with `effect_status`
and `effect_error`.

## Details

ALE remains the default for numeric features. Categorical features
require `method = "pdp"`. Comparisons are restricted to grid points
meeting `min_support` for both models. PDP dependence diagnostics remain
attached; curves are descriptive properties of fitted prediction
functions, not intervention effects or population confidence bands.

Pairwise columns have deliberately narrow meanings:

- `overlap` is the supported numeric grid span or supported categorical
  levels shared by the pair; `n_overlap` is its number of grid points.

- `mean_absolute_level_gap` is the mean absolute prediction-scale gap
  and therefore includes vertical offsets between curves.

- `centered_shape_rmse` is the root mean squared gap after centering
  each curve on its own supported mean. It describes shape disagreement
  and is undefined with fewer than two shared points.

- `max_absolute_gap` is the largest prediction-scale gap.

- `effect_rank_correlation` is Spearman correlation between paired
  effect values. `direction_agreement` is the fraction of adjacent
  numeric grid intervals in which the two curves change in the same
  direction.

Effect bands are descriptive, conditional on each already-fitted model.
They summarize across-row variation for PDP or propagated within-bin
local difference variation for ALE; they do not include model-fitting
uncertainty.

## Examples

``` r
fit <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 2026)
compare_model_effects(fit, "wt")
#> <AutoXplainR cross-model effect comparison>
#>   feature:   wt (ALE)
#>   target:    predicted value
#>   models:    3 of 3 requested (flexible_tree, small_tree, main_model)
#>   evidence:  6 effect rows from 6 test rows
#>   support:   relative support >= 0.1
#> 
#> Findings
#>   [note] The largest centered curve difference was between `flexible_tree` and `main_model` (RMSE = 1.039) across 6 supported grid points.
#>   [boundary] Pairwise summaries exclude grid points below relative support 0.1 and describe fitted predictions, not causal effects.
#> 
#> Pairwise summary
#>        model A    model B points mean level gap shape RMSE max gap
#>  flexible_tree main_model      6         0.9082      1.039    1.96
#>     small_tree main_model      6         0.9082      1.039    1.96
#>  flexible_tree small_tree      6         0.0000      0.000    0.00
#>  direction agreement
#>                    0
#>                    0
#>                   NA
#>   bands:     Descriptive fixed-model bands propagated from within-bin variation in local prediction differences under an independent-bin approximation; unavailable if a bin has fewer than two rows and not model-fitting uncertainty, population confidence, or causal intervals.
#>   caution:   fitted-curve disagreement is descriptive, not causal inference
```
