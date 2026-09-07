# Create a compact model metadata report

Deprecated in 0.4.0, with removal no earlier than 0.6.0. Use
[`render_model_report()`](https://matt17br.github.io/autoXplainR/reference/render_model_report.md)
for the maintained analysis report.

## Usage

``` r
create_model_comparison_report(
  model_characteristics,
  output_file = "model-comparison.html",
  include_plots = FALSE
)
```

## Arguments

- model_characteristics:

  Output from
  [`extract_model_characteristics()`](https://matt17br.github.io/autoXplainR/reference/extract_model_characteristics.md).

- output_file:

  Destination HTML file.

- include_plots:

  Retained for compatibility and ignored.

## Value

The normalized output path, invisibly.
