# Estimate a model feature effect

Computes either accumulated local effects (ALE) or partial dependence
(PDP). ALE is the default because it avoids the most direct
extrapolation problem of marginal PDPs when predictors are correlated.
PDP output includes local support and dependence diagnostics so
unsupported curves are not presented as unqualified facts.

## Usage

``` r
explain_effect(
  model,
  data = NULL,
  feature = NULL,
  method = c("ale", "pdp"),
  n_points = 20L,
  quantile_range = c(0.05, 0.95),
  sample_size = 1000L,
  seed = 123L,
  predict_function = NULL,
  task = "auto",
  positive = NULL,
  class = NULL,
  grid_size = NULL,
  return_all_classes = FALSE
)
```

## Arguments

- model:

  An `autoxplain_explainer` or fitted model.

- data:

  Reference data for a fitted model. For an explainer, defaults to its
  stored feature data.

- feature:

  A single feature name.

- method:

  `"ale"` or `"pdp"`.

- n_points:

  Number of quantile bins for ALE or grid points for PDP.

- quantile_range:

  Numeric length-two range used for a numeric PDP grid.

- sample_size:

  Maximum number of reference rows used by PDP. `NULL` uses all rows.
  ALE always uses all rows that fall inside its bins.

- seed:

  Sampling seed; the caller's random-number state is restored.

- predict_function:

  Optional prediction function for a fitted model.

- task:

  Prediction task for a fitted model.

- positive:

  Positive class for binary classification.

- class:

  For multiclass predictions, the class whose probability is explained.
  By default the first prediction column is used.

- grid_size:

  Deprecated alias for `n_points`.

- return_all_classes:

  Retained for compatibility. Multiclass callers should make separate
  class-specific explainers; `TRUE` is not supported by ALE.

## Value

A data frame of class `autoxplain_effect`. Both methods return the
effect estimate, relative empirical support, a descriptive standard
error, and normal-approximation limits. PDP limits summarize across-row
variation in fixed-model predictions at each grid value. ALE limits
propagate within-bin variation in fixed-model local prediction
differences and are unavailable when any bin has fewer than two rows.
Neither is a model-fitting or population confidence interval.

## Examples

``` r
fit <- lm(mpg ~ wt + hp + disp, data = mtcars)
x <- explain_model(fit, mtcars, y = "mpg")
explain_effect(x, feature = "wt")
#> <AutoXplainR ALE effect>
#>   feature: wt | rows: 32 | max association: 0.898
#>   target:  predicted value
#>   bands:   Descriptive fixed-model bands propagated from within-bin variation in local prediction differences under an independent-bin approximation; unavailable if a bin has fewer than two rows and not model-fitting uncertainty, population confidence, or causal intervals.
#>        wt accumulated_effect std_error conf_low conf_high n support
#>  1.624500        6.043000304        NA       NA        NA 2    0.50
#>  1.845750        5.208704821        NA       NA        NA 2    0.50
#>  2.067250        4.359205776        NA       NA        NA 1    0.25
#>  2.264000        3.713054377        NA       NA        NA 2    0.50
#>  2.465125        2.830297539        NA       NA        NA 1    0.25
#>  2.677125        2.101476770        NA       NA        NA 2    0.50
#>  2.816875        1.767948621        NA       NA        NA 1    0.25
#>  3.009375        0.638133895        NA       NA        NA 2    0.50
#>  3.173500        0.520306287        NA       NA        NA 1    0.25
#>  3.257000        0.003385168        NA       NA        NA 2    0.50
#>  3.382500       -0.433717249        NA       NA        NA 1    0.25
#>  3.454500       -0.543943076        NA       NA        NA 4    1.00
#>  3.512000       -0.870819666        NA       NA        NA 1    0.25
#>  3.582500       -1.079868648        NA       NA        NA 2    0.50
#>  3.690000       -1.688011141        NA       NA        NA 1    0.25
#>  3.805875       -1.960725040        NA       NA        NA 2    0.50
#>  3.944625       -2.742758278        NA       NA        NA 1    0.25
#>  4.670125       -7.475817276        NA       NA        NA 2    0.50
#>  5.358375       -7.974684165        NA       NA        NA 2    0.50
```
