# Plot model performance and resource trade-offs

Plot model performance and resource trade-offs

## Usage

``` r
plot_model_comparison(
  autoxplain_result,
  performance_metric = NULL,
  complexity_metric = NULL,
  title = "Model Trade-off Landscape"
)
```

## Arguments

- autoxplain_result:

  An `autoxplain_result`.

- performance_metric:

  Leaderboard metric; automatically selected when `NULL`.

- complexity_metric:

  Optional numeric leaderboard or model-metadata column. Model size is
  preferred when `NULL`.

- title:

  Plot title.

## Value

A Plotly scatter plot.
