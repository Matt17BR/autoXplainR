# Create a lightweight AutoXplainR dashboard

`create_simple_dashboard()` now uses the same dependency-free evidence
report as
[`generate_dashboard()`](https://matt17br.github.io/autoXplainR/reference/generate_dashboard.md).

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

  Maximum number of features included in the audit. The initial ranking
  is obtained from repeated permutation importance on the leading model,
  never from impurity importance.

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
