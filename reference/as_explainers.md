# Convert a fitted-model result to model-agnostic explainers

Convert a fitted-model result to model-agnostic explainers

## Usage

``` r
as_explainers(x, data = NULL, models = NULL)
```

## Arguments

- x:

  A result from
  [`autoxplain()`](https://matt17br.github.io/autoXplainR/reference/autoxplain.md)
  or
  [`evaluate_models()`](https://matt17br.github.io/autoXplainR/reference/evaluate_models.md).

- data:

  Optional raw evaluation data. The fitted preprocessing recipe is
  applied automatically. Defaults to the configured `test_data` when
  available, otherwise training data.

- models:

  Model indices, IDs, or `NULL` for all retained models.

## Value

A named list of `autoxplain_explainer` objects.
