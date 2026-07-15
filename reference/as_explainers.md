# Convert an AutoML result to model-agnostic explainers

Convert an AutoML result to model-agnostic explainers

## Usage

``` r
as_explainers(x, data = NULL, models = NULL)
```

## Arguments

- x:

  An `autoxplain_result`.

- data:

  Evaluation data. Defaults to held-out `test_data` when available,
  otherwise training data.

- models:

  Model indices, IDs, or `NULL` for all retained models.

## Value

A named list of `autoxplain_explainer` objects.
