# Keep groups or future observations out of training

Defines a study boundary for
[`autoxplain()`](https://matt17br.github.io/autoXplainR/reference/autoxplain.md).
Group splitting holds out whole groups, targeting prediction for
previously unseen groups. Temporal splitting holds out the latest
distinct times, targeting prediction after the training period. Neither
design guarantees transport to a different population.

## Usage

``` r
validation_split(group = NULL, time = NULL, gap = 0L)
```

## Arguments

- group:

  Name of a group column (for example patient or site ID).

- time:

  Name of a numeric, `Date`, or `POSIXct` time column. Supply exactly
  one of `group` and `time`. Tied times always stay together.

- gap:

  Number of distinct time values to exclude immediately before the test
  period. Only available with `time`; the units are time values, not
  rows or elapsed days. This does not prevent leakage from incorrectly
  built lagged features or delayed labels.

## Value

A validated `autoxplain_validation` specification. The fitted result's
`validation` component records original row indices for each partition,
excluded gap rows, and group fold membership where applicable.

## Details

The split column is removed from predictors. `test_fraction` in
[`autoxplain()`](https://matt17br.github.io/autoXplainR/reference/autoxplain.md)
refers to groups or distinct time values; row fractions can differ.
`test_data` cannot be combined with this design.

Grouped tuning allocates entire training groups to inner validation
folds, balancing row counts. Each classification fold must contain all
classes; infeasible designs fail with an explanation. Temporal designs
currently support `model_set = "quick"` or `"comparison"`; random-fold
tuning and H2O are rejected. Use an external rolling-origin workflow for
temporal tuning.

## Examples

``` r
data <- transform(mtcars, vehicle_group = rep(1:8, each = 4))
result <- autoxplain(data, "mpg", validation = validation_split(group = "vehicle_group"),
                     explain = FALSE)
```
