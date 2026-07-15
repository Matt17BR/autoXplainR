# Plot a PDP or ALE feature effect

Plot a PDP or ALE feature effect

## Usage

``` r
plot_partial_dependence(
  pdp_data,
  title = NULL,
  color = "#6f3f86",
  height = 320,
  width = NULL
)
```

## Arguments

- pdp_data:

  Data frame returned by
  [`explain_effect()`](https://matt17br.github.io/autoXplainR/reference/explain_effect.md).

- title:

  Optional title.

- color:

  Effect-line color.

- height, width:

  Widget dimensions.

## Value

A Plotly htmlwidget.
