# Stress-test the evidence behind model explanations

`audit_explanations()` is AutoXplainR's advanced reliability layer. It
evaluates repeated permutation importance, feature dependence, Monte
Carlo stability, prediction disagreement, and explanation disagreement
among near-equivalent models. The output keeps shuffle variation, a
limited pairwise association screen, evaluation scope, and candidate
disagreement separate. It does not combine these questions into an
evidence grade.

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
  dependence_threshold = 0.7,
  max_rows = NULL
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
  With `"auto"`, explainers carrying an `autoxplain_result` primary
  metric use that same metric for both performance screening and
  permutation importance.

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

  Pairwise association above which marginal importance receives a
  warning. Numeric pairs use absolute Spearman correlation, mixed pairs
  use a correlation ratio, and categorical pairs use Cramer's V. Small
  values do not establish independence or rule out nonlinear or joint
  dependence.

- max_rows:

  Maximum evaluation rows for permutation importance and
  feature-dependence checks. `NULL` uses all rows. Model performance and
  prediction comparisons still use the complete evaluation set. Sampling
  is uniform without replacement and shared across models; its
  uncertainty is not included in shuffle intervals.

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
#>   models:             2 (1 near-optimal)
#>   max association:    0.929
#>   explanation accord: unavailable
#>   prediction accord:  unavailable
#>   scope: Separate descriptive diagnostics; no overall evidence grade. Shuffle intervals omit evaluation-sampling, fitting and selection uncertainty.
#>   association: Limited pairwise screen: absolute Spearman correlation for numeric pairs, correlation ratio for mixed pairs, and Cramer's V for categorical pairs. Small values do not establish independence or exclude nonlinear or joint dependence. Categorical pairs without repeated categories are unavailable; many rare categories can inflate association.
#>   comparison: Fewer than two supplied models meet the performance tolerance.
#> 
#> Findings
#>   [warning] `cyl` exceeds the pairwise association threshold.
#>   [warning] `disp` exceeds the pairwise association threshold.
#>   [warning] `hp` exceeds the pairwise association threshold.
#>   [warning] `wt` exceeds the pairwise association threshold.
#>   [warning] `qsec` exceeds the pairwise association threshold.
#>   [warning] `vs` exceeds the pairwise association threshold.
```
