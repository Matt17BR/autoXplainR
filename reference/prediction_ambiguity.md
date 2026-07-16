# Find held-out rows where supplied models disagree

`prediction_ambiguity()` compares predictions from at least two retained
models on the same evaluation rows. Regression output reports the range
of predicted values. Classification output reports hard-class
disagreement and the largest pairwise probability distance for each row.

## Usage

``` r
prediction_ambiguity(result, models = NULL, performance_tolerance = NULL)
```

## Arguments

- result:

  An `autoxplain_result` containing at least two comparable retained
  models.

- models:

  Model IDs, indices, or `NULL`. `NULL` selects every retained model not
  labeled as a baseline.

- performance_tolerance:

  Optional non-negative relative gap from the best selected evaluation
  score. `NULL` keeps all selected models.

## Value

An `autoxplain_prediction_ambiguity` object with model performance,
case-level `rows`, and aggregate summaries.

## Details

By default the simple baseline is excluded and every other supplied
model is compared. These models need not form a statistically defined
Rashomon set. Their performance table is retained beside the ambiguity
results so that disagreement from a weak candidate is not mistaken for
near-optimal model uncertainty. Set `performance_tolerance` to keep only
models within a relative performance gap from the best selected model.

## Examples

``` r
fit <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 2026)
prediction_ambiguity(fit)
#> <AutoXplainR prediction ambiguity>
#>   task:       regression
#>   models:     3 (flexible_tree, small_tree, main_model)
#>   rows:       6 (test)
#>   median gap: 2.4701 target units
#>   caution:    supplied-model disagreement is descriptive, not uncertainty coverage
```
