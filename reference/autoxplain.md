# Fit a reproducible H2O AutoML candidate set

`autoxplain()` is the optional model-fitting convenience layer. The core
explanation and audit API does not require H2O; users can call
[`explain_model()`](https://matt17br.github.io/autoXplainR/reference/explain_model.md)
for any fitted model.

## Usage

``` r
autoxplain(
  data,
  target_column,
  max_models = 10L,
  max_runtime_secs = 300L,
  seed = 123L,
  test_data = NULL,
  enable_preprocessing = TRUE,
  preprocessing_config = list(),
  task = c("auto", "regression", "binary", "multiclass"),
  nfolds = 5L,
  sort_metric = "AUTO",
  include_algos = NULL,
  exclude_algos = NULL,
  use_test_as_validation = FALSE,
  init_h2o = TRUE,
  h2o_nthreads = -1L,
  h2o_max_mem_size = "2G",
  verbosity = c("quiet", "info")
)
```

## Arguments

- data:

  Training data frame.

- target_column:

  Name of the outcome column.

- max_models:

  Maximum number of base models.

- max_runtime_secs:

  Overall training time budget in seconds.

- seed:

  Reproducible H2O seed.

- test_data:

  Optional held-out evaluation data. It is not used as an H2O validation
  frame unless `use_test_as_validation = TRUE`.

- enable_preprocessing:

  Apply
  [`preprocess_for_h2o()`](https://matt17br.github.io/autoXplainR/reference/preprocess_for_h2o.md).

- preprocessing_config:

  Named overrides for preprocessing. Identifier removal defaults to
  `FALSE`.

- task:

  One of `"auto"`, `"regression"`, `"binary"`, or `"multiclass"`.

- nfolds:

  Number of H2O cross-validation folds. At least two folds are required
  when stacked ensembles are desired.

- sort_metric:

  H2O AutoML leaderboard metric.

- include_algos, exclude_algos:

  Optional H2O algorithm filters. Supply at most one.

- use_test_as_validation:

  Whether to pass `test_data` to H2O as a validation frame. The default
  keeps held-out explanation data independent.

- init_h2o:

  Start a local H2O cluster when no connection is available.

- h2o_nthreads:

  Threads used when starting H2O.

- h2o_max_mem_size:

  Memory used when starting H2O.

- verbosity:

  One of `"quiet"` or `"info"`.

## Value

An `autoxplain_result` containing models, a data-frame leaderboard, task
metadata, preprocessing provenance, and evaluation data.

## Details

Numeric outcomes with exactly two distinct values are treated as binary
classification by default. Potentially destructive preprocessing, such
as identifier removal, is opt-in and recorded in the result.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- autoxplain(mtcars, "mpg", max_models = 3)
explainers <- as_explainers(result)
audit_explanations(explainers)
} # }
```
