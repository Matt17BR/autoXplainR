# Generate an explanation evidence dashboard

Compatibility entry point for H2O AutoML results. It converts retained
H2O models to model-agnostic explainers, runs the same evidence audit
available to every model framework, and writes a standalone HTML report.

## Usage

``` r
generate_dashboard(
  autoxplain_result,
  output_file = "autoxplain-dashboard.html",
  top_features = 8L,
  sample_instances = 3L,
  include_llm_report = FALSE,
  open_browser = FALSE,
  performance_weight = NULL,
  n_repeats = 20L,
  max_models = 5L
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

- include_llm_report:

  Whether to request an optional narrative. The evidence report remains
  complete without it.

- open_browser:

  Open the result interactively.

- performance_weight:

  Deprecated and ignored. Performance and explanation reliability are
  reported separately rather than collapsed into an arbitrary weighted
  score.

- n_repeats:

  Permutation repeats in the final audit.

- max_models:

  Maximum retained models to compare.

## Value

The normalized dashboard path, invisibly.
