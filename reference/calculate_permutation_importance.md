# Repeated permutation feature importance with uncertainty diagnostics

Measures the change in out-of-sample performance after jointly permuting
one feature (or a user-defined feature group). Unlike a single
permutation bar chart, the result retains every repeat, Monte Carlo
uncertainty, sign stability, and the evaluation contract required for an
explanation audit.

## Usage

``` r
calculate_permutation_importance(
  model,
  data = NULL,
  target_column = NULL,
  metric = "auto",
  n_repeats = 20L,
  seed = 123L,
  features = NULL,
  feature_groups = NULL,
  within = NULL,
  confidence = 0.95,
  predict_function = NULL,
  task = "auto",
  positive = NULL,
  n_permutations = NULL
)
```

## Arguments

- model:

  An `autoxplain_explainer` or a fitted model.

- data:

  Evaluation data. Required for a fitted model; ignored when `model` is
  an explainer unless supplied to replace its evaluation data.

- target_column:

  Outcome column name when `model` is a fitted model.

- metric:

  One of `"auto"`, `"rmse"`, `"mae"`, `"logloss"`, `"accuracy"`, or
  `"auc"`.

- n_repeats:

  Number of independent permutations.

- seed:

  Random seed. The caller's random-number state is restored.

- features:

  Character vector of features to evaluate. Defaults to all.

- feature_groups:

  Optional named list of character vectors. Features in each element are
  permuted together, which is useful for one-hot encodings and
  semantically coupled predictors.

- within:

  Optional character vector naming columns that define strata, or a
  vector/factor with one stratum per row. Permutations occur within
  strata. This is a blocked sensitivity analysis, not a general
  conditional permutation importance estimator.

- confidence:

  Confidence level for Monte Carlo intervals.

- predict_function:

  Optional prediction function for a fitted model.

- task:

  Task type passed to
  [`explain_model()`](https://matt17br.github.io/autoXplainR/reference/explain_model.md).

- positive:

  Positive class passed to
  [`explain_model()`](https://matt17br.github.io/autoXplainR/reference/explain_model.md).

- n_permutations:

  Deprecated alias for `n_repeats`.

## Value

A data frame of class `autoxplain_importance` with repeat-level values
stored in the `repeat_scores` attribute.

## Details

The confidence limits quantify Monte Carlo variation across
permutations. They are not confidence intervals for population-level
variable importance. Use a genuinely held-out evaluation set and a
formal inference method when population claims are required.

## Examples

``` r
train <- mtcars[1:24, ]
test <- mtcars[25:32, ]
fit <- lm(mpg ~ wt + hp + disp, data = train)
x <- explain_model(fit, test, y = "mpg")
calculate_permutation_importance(x, n_repeats = 10)
#> <AutoXplainR permutation importance>
#>   metric: rmse | repeats: 10 | baseline: 2.5436
#>  feature importance std_error   conf_low   conf_high sign_stability
#>       hp  2.4225457 0.3683050  1.5893819  3.25570958            1.0
#>       wt  1.0435820 0.2123997  0.5631005  1.52406342            1.0
#>      cyl  0.0000000 0.0000000  0.0000000  0.00000000            1.0
#>     drat  0.0000000 0.0000000  0.0000000  0.00000000            1.0
#>     qsec  0.0000000 0.0000000  0.0000000  0.00000000            1.0
#>       vs  0.0000000 0.0000000  0.0000000  0.00000000            1.0
#>       am  0.0000000 0.0000000  0.0000000  0.00000000            1.0
#>     gear  0.0000000 0.0000000  0.0000000  0.00000000            1.0
#>     carb  0.0000000 0.0000000  0.0000000  0.00000000            1.0
#>     disp -0.1867363 0.0538506 -0.3085548 -0.06491778            0.9
#>   intervals describe permutation Monte Carlo variation, not population inference
```
