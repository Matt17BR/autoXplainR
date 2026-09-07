# Deprecated simple-dashboard compatibility wrapper

Deprecated in 0.4.0; removal will occur no earlier than 0.6.0. Use
[`render_model_report()`](https://matt17br.github.io/autoXplainR/reference/render_model_report.md)
instead. This wrapper warns once per call.

## Usage

``` r
create_simple_dashboard(
  autoxplain_result,
  output_file = "autoxplain-dashboard.html",
  top_features = 8L,
  sample_instances = 3L,
  open_browser = FALSE,
  n_repeats = 10L,
  ...
)
```

## Arguments

- autoxplain_result:

  An `autoxplain_result`.

- output_file:

  Destination HTML file.

- top_features:

  Maximum displayed inputs per model. The audit uses the union of their
  leading inputs, ranked by repeated permutation importance.

- sample_instances:

  Retained for backward compatibility; no longer used.

- open_browser:

  Open the result interactively.

- n_repeats:

  Permutation repeats in the final audit.

- ...:

  Additional arguments forwarded to
  [`generate_dashboard()`](https://matt17br.github.io/autoXplainR/reference/generate_dashboard.md).

## Value

The normalized output path, invisibly.
