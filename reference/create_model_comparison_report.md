# Create a compact model metadata report

Create a compact model metadata report

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
