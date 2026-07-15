# Stress-test the evidence behind model explanations

`audit_explanations()` is AutoXplainR's advanced reliability layer. It
evaluates repeated permutation importance, feature dependence, Monte
Carlo stability, prediction disagreement, and explanation disagreement
among near-equivalent models. The output distinguishes robust
descriptive evidence from claims that need more data or a conditional
method.

## Usage

``` r
audit_explanations(
  explainers,
  features = NULL,
  metric = "auto",
  n_repeats = 20L,
  seed = 123L,
  confidence = 0.95,
  performance_tolerance = 0.05,
  dependence_threshold = 0.7
)
```

## Arguments

- explainers:

  An `autoxplain_explainer` or a list of explainers.

- features:

  Features shared by every explainer. Defaults to their intersection.

- metric:

  Performance metric passed to
  [`calculate_permutation_importance()`](https://matt17br.github.io/autoXplainR/reference/calculate_permutation_importance.md).

- n_repeats:

  Number of permutations per model and feature.

- seed:

  Reproducible seed.

- confidence:

  Monte Carlo interval level.

- performance_tolerance:

  Relative tolerance defining the empirical set of near-optimal supplied
  models. For example, `0.05` retains models whose evaluation score is
  within five percent of the best supplied score.

- dependence_threshold:

  Association above which marginal importance and PDP claims receive a
  dependence warning.

## Value

An object of class `autoxplain_audit`.

## Details

This is a diagnostic protocol, not a formal certification or a
substitute for domain review, causal identification, or external
validation.

## Examples

``` r
train <- mtcars[1:24, ]
test <- mtcars[25:32, ]
lm1 <- lm(mpg ~ wt + hp + disp, train)
lm2 <- lm(mpg ~ wt + hp + qsec, train)
e1 <- explain_model(lm1, test, "mpg", label = "model A")
e2 <- explain_model(lm2, test, "mpg", label = "model B")
audit <- audit_explanations(list(e1, e2), n_repeats = 5)
audit
#> <AutoXplainR explanation evidence audit>
#>   grade:              C (diagnostic, not certification)
#>   models:             2 (1 near-optimal)
#>   stable claims:      0.0%
#>   max dependence:     0.929
#>   explanation accord: n/a (one model)
#>   prediction accord:  n/a (one model)
#> 
#> Findings
#>   [warning] 8 feature(s) exceed the dependence threshold.
#>   [warning] 20 model-feature claim(s) are qualified or unsupported.
#>   [note] The permutation budget is suitable for smoke testing, not a final report.
#>   [note] The audit cannot verify that explanation data are independent of model fitting.
```
