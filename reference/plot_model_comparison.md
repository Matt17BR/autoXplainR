# Plot model performance and resource trade-offs

Plot model performance and resource trade-offs

## Usage

``` r
plot_model_comparison(
  autoxplain_result,
  performance_metric = NULL,
  complexity_metric = NULL,
  title = "Model performance and cost"
)
```

## Arguments

- autoxplain_result:

  An `autoxplain_result`.

- performance_metric:

  Leaderboard metric; automatically selected when `NULL`.

- complexity_metric:

  Optional numeric leaderboard or model-metadata column. `NULL` prefers
  usable repeated prediction timing, retained fit time, evaluation-batch
  prediction time, then model size.

- title:

  Plot title.

## Value

A Plotly scatter plot.
