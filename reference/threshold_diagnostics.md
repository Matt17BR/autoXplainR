# Explore binary decision-threshold trade-offs

A binary model returns a probability; a decision threshold turns that
probability into a class label. `threshold_diagnostics()` shows how
observed sensitivity, specificity, precision, accuracy, false positives,
and false negatives change across candidate thresholds on the evaluation
rows.

## Usage

``` r
threshold_diagnostics(
  result,
  thresholds = seq(0.1, 0.9, by = 0.05),
  model = NULL,
  false_positive_cost = 1,
  false_negative_cost = 1
)
```

## Arguments

- result:

  An `autoxplain_result` for binary classification.

- thresholds:

  Numeric candidate thresholds between 0 and 1.

- model:

  One model ID or index. `NULL` uses `main_model` when available,
  otherwise the first retained model.

- false_positive_cost:

  Non-negative relative cost assigned to one false positive.

- false_negative_cost:

  Non-negative relative cost assigned to one false negative.

## Value

An `autoxplain_thresholds` object containing a `performance` data frame.
Expected cost is the average user-supplied relative cost per evaluation
row, not a monetary estimate.

## Details

The default 0.5 threshold is a convention, not a universal optimum. The
output is descriptive and must not be used to choose a threshold and
then report performance on the same rows as if it were independent
validation. Threshold choice requires domain consequences and fresh
validation data.

## Examples

``` r
cars <- transform(mtcars, am = factor(am, labels = c("automatic", "manual")))
fit <- autoxplain(cars, "am", seed = 2026)
threshold_diagnostics(fit, thresholds = c(0.3, 0.5, 0.7))
#> <AutoXplainR decision-threshold check>
#>   model:       main_model
#>   positive:    manual
#>   rows:        7 (test)
#>   thresholds:  3 checked
#>   lowest cost: 0.2857 at 0.3, 0.5, 0.7 (descriptive only)
#>   caution:     validate a chosen threshold on different data
```
