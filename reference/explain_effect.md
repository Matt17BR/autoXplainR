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
  return_all_classes = FALSE,
  max_rows = NULL
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

  Maximum number of empirical quantile bins for ALE or grid points for
  PDP. Ties can reduce the ALE bin count. ALE returns both the first
  lower boundary and each upper boundary.

- quantile_range:

  Numeric length-two range used for a numeric PDP grid.

- sample_size:

  Maximum number of reference rows used by PDP. `NULL` uses all rows
  unless `max_rows` is set. The curve uses the smaller of these limits,
  drawn once from the original reference population. Grid support uses
  the separate `max_rows` sample; both row counts are recorded.

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

- max_rows:

  Optional cap on reference rows for either method. `NULL` preserves the
  full reference data. A finite cap selects a uniform sample without
  replacement before computing bins, support, and effects. The returned
  `sampling` attribute records this scope; descriptive bands do not
  include uncertainty from selecting these rows.

## Value

A data frame of class `autoxplain_effect`. Both methods return the
effect estimate, relative empirical support, a descriptive standard
error, and normal-approximation limits. PDP limits summarize across-row
variation in fixed-model predictions at each grid value. ALE limits
propagate within-bin variation in fixed-model local prediction
differences and are unavailable when any bin has fewer than two rows.
Neither is a model-fitting or population confidence interval.

## Details

ALE uses observed empirical quantiles (type 1), with right-closed bins
and the minimum included in the first bin. Every bin therefore has
observations even when predictor values are tied. Cumulative local
differences are reported at the bin boundaries. The curve is centered by
subtracting the empirical mean of its linearly interpolated values at
the reference observations. The initial boundary has no separate bin
count (`n = NA`); its support is that of the first bin. These are
finite-bin approximations, and wider bins can obscure within-bin
nonlinear behavior.

## Examples

``` r
fit <- lm(mpg ~ wt + hp + disp, data = mtcars)
x <- explain_model(fit, mtcars, y = "mpg")
explain_effect(x, feature = "wt")
#> <AutoXplainR ALE effect>
#>   feature: wt | rows: 32 | max association: 0.898
#>   target:  predicted value
#>   association: Limited pairwise screen: absolute Spearman correlation for numeric pairs, correlation ratio for mixed pairs, and Cramer's V for categorical pairs. Small values do not establish independence or exclude nonlinear or joint dependence. Categorical pairs without repeated categories are unavailable; many rare categories can inflate association.
#>   bands:   Descriptive fixed-model bands propagated from within-bin variation in local prediction differences under an independent-bin approximation; unavailable if a bin has fewer than two rows and not model-fitting uncertainty, population confidence, or causal intervals.
#>     wt accumulated_effect std_error conf_low conf_high  n support
#>  1.513        6.477667775        NA       NA        NA NA    0.50
#>  1.615        6.089976936        NA       NA        NA  2    0.50
#>  1.935        4.873691950        NA       NA        NA  2    0.50
#>  2.140        4.094509380        NA       NA        NA  1    0.25
#>  2.320        3.410349075        NA       NA        NA  2    0.50
#>  2.465        2.859219941        NA       NA        NA  1    0.25
#>  2.770        1.699948313        NA       NA        NA  2    0.50
#>  2.875        1.300854802        NA       NA        NA  2    0.50
#>  3.150        0.255609892        NA       NA        NA  1    0.25
#>  3.190        0.103574268        NA       NA        NA  2    0.50
#>  3.215        0.008552004        NA       NA        NA  1    0.25
#>  3.440       -0.846648377        NA       NA        NA  4    1.00
#>  3.460       -0.922666189        NA       NA        NA  1    0.25
#>  3.570       -1.340764153        NA       NA        NA  3    0.75
#>  3.780       -2.138951175        NA       NA        NA  2    0.50
#>  3.845       -2.386009063        NA       NA        NA  2    0.50
#>  4.070       -3.241209444        NA       NA        NA  1    0.25
#>  5.345       -8.087344937        NA       NA        NA  2    0.50
#>  5.424       -8.387615293        NA       NA        NA  1    0.25
```
