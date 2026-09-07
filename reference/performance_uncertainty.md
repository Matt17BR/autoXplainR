# Estimate evaluation-sample uncertainty with a paired bootstrap

Resamples evaluation observations, using the same sampled rows for the
primary model and designated reference model (the intercept-only
baseline in guided workflows). The models stay fixed. The difference is
primary loss minus reference loss, so negative values favor the primary
model. This estimates evaluation-sample variability conditional on the
fitted models; it does not include fitting, tuning, or feature-selection
uncertainty.

## Usage

``` r
performance_uncertainty(result, n_boot = 1000L, confidence = 0.95, seed = 123L)
```

## Arguments

- result:

  An
  [`autoxplain()`](https://matt17br.github.io/autoXplainR/reference/autoxplain.md)
  or
  [`evaluate_models()`](https://matt17br.github.io/autoXplainR/reference/evaluate_models.md)
  result.

- n_boot:

  Number of bootstrap draws (at least 20). Use at least 1000 for
  analysis; smaller values are useful for examples and software tests.

- confidence:

  Percentile interval level, strictly between zero and one.

- seed:

  Reproducible resampling seed; the calling session's RNG is preserved.

## Value

An `autoxplain_uncertainty` list with `estimates` (primary, baseline,
and paired difference), all `draws`, resampling `unit`, and
interpretation `notes`. Losses are RMSE or MAE for regression, log loss
or Brier for classification, following the fitted result's primary
metric.

## Details

For ordinary data this is an IID row bootstrap. With
`validation_split(group = ...)`, whole evaluation groups are sampled
with replacement and all their rows retained; losses remain
observation-weighted. This assumes independent groups and enough
representative groups. It does not estimate equally weighted group
performance. Temporal designs are rejected because an IID bootstrap
would ignore serial dependence.

Intervals are approximate, can be unreliable in small or degenerate
samples, and are not simultaneous across metrics or model comparisons.
The configured primary loss is used; no model is selected using these
intervals. A validation-set interval does not make that set an
independent test set.

## References

Davison, A. C. and Hinkley, D. V. (1997). Bootstrap Methods and Their
Application. Cambridge University Press. <doi:10.1017/CBO9780511802843>.

## Examples

``` r
result <- autoxplain(mtcars, "mpg", explain = FALSE)
performance_uncertainty(result, n_boot = 50)
#> <AutoXplainR paired evaluation bootstrap>
#>   metric: rmse | units: 6 observations
#>    quantity  estimate      lower     upper
#>     primary  2.455942  0.8680807  3.437006
#>    baseline  6.621187  4.6100140  8.512278
#>  difference -4.165245 -6.0630899 -1.917466
#> Paired percentile intervals conditional on the fitted models; negative differences favor the primary model.
#> These intervals omit fitting and selection uncertainty and assume independent sampling units.
#> Fewer than 20 sampling units: interval endpoints may be very unstable. 
```
