# Plot AutoML model performance

Plot AutoML model performance

## Usage

``` r
plot_model_comparison(
  autoxplain_result,
  performance_metric = NULL,
  complexity_metric = "model_size",
  title = "Model Performance Landscape"
)
```

## Arguments

- autoxplain_result:

  An `autoxplain_result`.

- performance_metric:

  Leaderboard metric; automatically selected when `NULL`.

- complexity_metric:

  Optional leaderboard complexity column. Model rank is used when
  unavailable.

- title:

  Plot title.

## Value

A Plotly scatter plot.
