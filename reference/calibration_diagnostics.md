# Check whether held-out class probabilities behave literally

`calibration_diagnostics()` groups classification probabilities on the
evaluation rows and compares each group's average probability with what
actually happened. For binary classification the event is the positive
class. For multiclass classification it checks whether the confidence of
the predicted class matches its observed accuracy.

## Usage

``` r
calibration_diagnostics(result, model = NULL, bins = 5L)
```

## Arguments

- result:

  An `autoxplain_result`.

- model:

  One model ID or index. `NULL` uses `main_model` when available,
  otherwise the first retained model.

- bins:

  Maximum number of approximately equal-sized probability groups. Small
  evaluation sets automatically use fewer groups so that each group is
  not presented with false precision.

## Value

An `autoxplain_calibration` object containing the overall binned
calibration error and a `groups` data frame.

## Details

This is a descriptive, binned diagnostic. Its value depends on the
evaluation sample and grouping, so it is context for probability
quality, not a population guarantee or a replacement for log loss and
Brier score.

## Examples

``` r
flowers <- autoxplain(iris, "Species", seed = 2026)
calibration_diagnostics(flowers)
#> <AutoXplainR probability calibration>
#>   model:      main_model
#>   check:      confidence in the predicted class
#>   rows:       30 in 1 probability groups
#>   average:    1 predicted vs 0.967 observed
#>   binned gap: 0.033 (lower is better)
#>   caution:    sample- and grouping-dependent; not a population guarantee
```
