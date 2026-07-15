# Calculate partial dependence data

Backward-compatible PDP entry point. New code should generally call
[`explain_effect()`](https://matt17br.github.io/autoXplainR/reference/explain_effect.md)
and keep its default ALE method.

## Usage

``` r
calculate_partial_dependence(
  model,
  data = NULL,
  feature = NULL,
  n_points = 50L,
  quantile_range = c(0.05, 0.95),
  return_all_classes = FALSE,
  grid_size = NULL,
  sample_size = 1000L,
  seed = 123L,
  predict_function = NULL,
  task = "auto",
  positive = NULL,
  class = NULL
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

- n_points:

  Number of quantile bins for ALE or grid points for PDP.

- quantile_range:

  Numeric length-two range used for a numeric PDP grid.

- return_all_classes:

  Retained for compatibility. Multiclass callers should make separate
  class-specific explainers; `TRUE` is not supported by ALE.

- grid_size:

  Deprecated alias for `n_points`.

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

## Value

A data frame of class `autoxplain_effect`.
