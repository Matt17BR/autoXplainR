# Extract comparable AutoML model metadata

Extracts stable, compact metadata from retained H2O models and joins it
to the AutoML leaderboard. Native H2O variable importance is labelled as
such; it is not substituted for model-agnostic permutation importance.

## Usage

``` r
extract_model_characteristics(
  autoxplain_result,
  include_hyperparams = TRUE,
  include_performance = TRUE,
  include_varimp = TRUE
)
```

## Arguments

- autoxplain_result:

  An `autoxplain_result`.

- include_hyperparams:

  Include a compact set of configured parameters.

- include_performance:

  Include numeric leaderboard metrics.

- include_varimp:

  Include native H2O importance when available.

## Value

A list of class `autoxplainr_model_characteristics`.
