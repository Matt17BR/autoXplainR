# Calculate a relative weighted model score

Min-max combines predictive performance and training time. Because
results depend on the candidate set and the subjective weight, use this
only as a sensitivity analysis; AutoXplainR reports the underlying
dimensions separately in its primary workflow.

## Usage

``` r
calculate_weighted_efficiency(
  performance_scores,
  training_times,
  performance_weight = 0.7,
  higher_is_better = TRUE
)
```

## Arguments

- performance_scores:

  Numeric model scores.

- training_times:

  Numeric training times in seconds.

- performance_weight:

  Weight on predictive performance.

- higher_is_better:

  Whether larger performance is better.

## Value

Numeric relative scores from zero to one.
