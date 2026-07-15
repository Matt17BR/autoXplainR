# Calculate accumulated local effects data

Calculate accumulated local effects data

## Usage

``` r
calculate_accumulated_local_effects(
  model,
  data = NULL,
  feature = NULL,
  n_points = 20L,
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
