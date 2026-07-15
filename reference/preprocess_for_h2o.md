# Prepare a data frame for the H2O adapter

Compatibility alias for
[`preprocess_data()`](https://matt17br.github.io/autoXplainR/reference/preprocess_data.md).
New code should use the engine-neutral name.

## Usage

``` r
preprocess_for_h2o(data, target_column, ...)
```

## Arguments

- data:

  Data frame to prepare.

- target_column:

  Outcome column name.

- ...:

  Additional preprocessing controls passed to
  [`preprocess_data()`](https://matt17br.github.io/autoXplainR/reference/preprocess_data.md).

## Value

The same auditable preprocessing result as
[`preprocess_data()`](https://matt17br.github.io/autoXplainR/reference/preprocess_data.md).
