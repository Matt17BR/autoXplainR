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

A data frame of class `autoxplain_effect`.

## Examples

``` r
fit <- lm(mpg ~ wt + hp + disp, data = mtcars)
x <- explain_model(fit, mtcars, y = "mpg")
explain_effect(x, feature = "wt")
#> <AutoXplainR ALE effect>
#>   feature: wt | rows: 32 | max association: 0.898
#>        wt accumulated_effect    std_error     conf_low    conf_high n support
#>  1.624500        6.043000304 0.000000e+00  6.043000304  6.043000304 2    0.50
#>  1.845750        5.208704821 0.000000e+00  5.208704821  5.208704821 2    0.50
#>  2.067250        4.359205776 0.000000e+00  4.359205776  4.359205776 1    0.25
#>  2.264000        3.713054377 0.000000e+00  3.713054377  3.713054377 2    0.50
#>  2.465125        2.830297539 0.000000e+00  2.830297539  2.830297539 1    0.25
#>  2.677125        2.101476770 0.000000e+00  2.101476770  2.101476770 2    0.50
#>  2.816875        1.767948621 0.000000e+00  1.767948621  1.767948621 1    0.25
#>  3.009375        0.638133895 0.000000e+00  0.638133895  0.638133895 2    0.50
#>  3.173500        0.520306287 0.000000e+00  0.520306287  0.520306287 1    0.25
#>  3.257000        0.003385168 0.000000e+00  0.003385168  0.003385168 2    0.50
#>  3.382500       -0.433717249 0.000000e+00 -0.433717249 -0.433717249 1    0.25
#>  3.454500       -0.543943076 0.000000e+00 -0.543943076 -0.543943076 4    1.00
#>  3.512000       -0.870819666 0.000000e+00 -0.870819666 -0.870819666 1    0.25
#>  3.582500       -1.079868648 0.000000e+00 -1.079868648 -1.079868648 2    0.50
#>  3.690000       -1.688011141 0.000000e+00 -1.688011141 -1.688011141 1    0.25
#>  3.805875       -1.960725040 0.000000e+00 -1.960725040 -1.960725040 2    0.50
#>  3.944625       -2.742758278 0.000000e+00 -2.742758278 -2.742758278 1    0.25
#>  4.670125       -7.475817276 8.881784e-16 -7.475817276 -7.475817276 2    0.50
#>  5.358375       -7.974684165 8.881784e-16 -7.974684165 -7.974684165 2    0.50
```
