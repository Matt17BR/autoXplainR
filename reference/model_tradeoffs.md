# Compare supplied models without hiding trade-offs

Builds a two-objective comparison from an
[`autoxplain()`](https://matt17br.github.io/autoXplainR/reference/autoxplain.md)
result. Predictive performance is taken from the evaluation leaderboard
and the secondary axis defaults to approximate model-object size. For
local models this is R's in-memory
[`object.size()`](https://rdrr.io/r/utils/object.size.html) estimate;
H2O uses an engine-reported size when available. Size and runtime are
operational resource proxies, not structural model complexity. A model
is Pareto-efficient when no other supplied model is at least as good on
both dimensions and strictly better on one.

## Usage

``` r
model_tradeoffs(
  result,
  performance_metric = NULL,
  complexity_metric = NULL,
  include_baseline = TRUE
)
```

## Arguments

- result:

  An `autoxplain_result`.

- performance_metric:

  Numeric leaderboard metric. `NULL` selects the task-appropriate
  primary metric.

- complexity_metric:

  Numeric leaderboard or model-metadata column. The argument name is
  retained for compatibility; `NULL` prefers model size, then training
  or prediction time, and the returned object labels the exact metric as
  a resource or structural-complexity proxy.

- include_baseline:

  Include models labeled with the baseline role.

## Value

A data frame of class `autoxplain_model_tradeoffs` with Pareto status
and metric-direction metadata.

## Details

Pareto status is descriptive and candidate-set-relative. It is not a
tuning rule, evidence that every useful model was considered, or
permission to use the held-out evaluation data repeatedly for model
selection.
