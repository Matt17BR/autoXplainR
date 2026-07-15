# Plot permutation importance with uncertainty

Plot permutation importance with uncertainty

## Usage

``` r
plot_permutation_importance(
  importance_data,
  title = "Permutation Importance Evidence",
  max_features = 15L,
  color = "#237a57",
  negative_color = "#b24747"
)
```

## Arguments

- importance_data:

  Data frame returned by
  [`calculate_permutation_importance()`](https://matt17br.github.io/autoXplainR/reference/calculate_permutation_importance.md).

- title:

  Plot title.

- max_features:

  Maximum features displayed.

- color:

  Positive-importance color.

- negative_color:

  Negative-importance color.

## Value

A Plotly htmlwidget.
