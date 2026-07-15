# Compare held-out performance across an explicitly chosen group

`subgroup_performance()` calculates the same prediction metrics
separately for each value of one evaluation-data column. It is intended
to reveal where aggregate performance may be hiding weak spots and to
prompt better data collection or validation.

## Usage

``` r
subgroup_performance(result, by, model = NULL, min_rows = 10L)
```

## Arguments

- result:

  An `autoxplain_result`.

- by:

  Name of one categorical or low-cardinality evaluation column.

- model:

  One model ID or index. `NULL` uses `main_model` when available,
  otherwise the first retained model.

- min_rows:

  Minimum rows used to label a group as large enough for a preliminary
  comparison. Smaller groups remain visible and are flagged.

## Value

An `autoxplain_subgroups` object containing overall metrics and a
group-level `performance` data frame.

## Details

The function never guesses which columns are sensitive and never labels
the result a fairness assessment. Observed differences can reflect small
sample sizes, case mix, measurement quality, or genuine model behavior.
They are descriptive context from the supplied evaluation rows.

## Examples

``` r
cars <- transform(mtcars, transmission = factor(am, labels = c("auto", "manual")))
fit <- autoxplain(cars, "mpg", seed = 2026)
subgroup_performance(fit, by = "transmission", min_rows = 3)
#> <AutoXplainR subgroup performance>
#>   model:       main_model
#>   compared by: transmission (2 groups)
#>   metric:      rmse (lower is better)
#>   largest gap: 3.3003
#>   caution:     descriptive holdout check; not fairness certification
```
