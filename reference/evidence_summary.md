# Extract a compact, versioned evidence summary

Returns aggregate results suitable for review or an analysis registry.
It excludes raw rows, row names, per-case predictions, group
identifiers, fitted objects, and narrative provider credentials. Target
and feature names, aggregate statistics, and diagnostic messages remain;
review them before sharing.

## Usage

``` r
evidence_summary(result)
```

## Arguments

- result:

  An
  [`autoxplain()`](https://matt17br.github.io/autoXplainR/reference/autoxplain.md)
  or
  [`evaluate_models()`](https://matt17br.github.io/autoXplainR/reference/evaluate_models.md)
  result.

## Value

A plain list with `schema_version`, package version, task, model
selection, evaluation, explanation summaries, and interpretation limits.
Schema 2.0 allows additional fields within a minor package release.
Removing or changing the meaning of a field requires a new schema major
version.

## Examples

``` r
result <- autoxplain(mtcars, "mpg")
evidence <- evidence_summary(result)
evidence$evaluation
#> $role
#> [1] "test"
#> 
#> $split_method
#> [1] "reproducible random holdout"
#> 
#> $rows
#> [1] 6
#> 
#> $primary_metric
#> [1] "rmse"
#> 
#> $metrics
#> $metrics$main_model
#>      rmse       mae r_squared 
#> 2.4559419 1.7778871 0.8544681 
#> 
#> $metrics$tree_model
#>      rmse       mae r_squared 
#> 2.4951620 1.9000000 0.8497828 
#> 
#> $metrics$linear_model
#>      rmse       mae r_squared 
#>  2.441285  2.089462  0.856200 
#> 
#> $metrics$simple_baseline
#>        rmse         mae   r_squared 
#>  6.62118671  5.69358974 -0.05777599 
#> 
#> 
#> $metric_definitions
#>                                                                                                            rmse 
#>                        "Typical prediction error, with larger mistakes weighted more heavily; lower is better." 
#>                                                                                                             mae 
#>                                     "Average absolute prediction error in the target's units; lower is better." 
#>                                                                                                       r_squared 
#> "Share of evaluation-set variation explained relative to predicting the evaluation-set mean; higher is better." 
#> 
#> $improvement_over_baseline
#> [1] 0.6290783
#> 
#> $notes
#>   severity                 code
#> 1  caution small_evaluation_set
#> 2  caution few_rows_per_feature
#>                                              message
#> 1       Only 6 rows were available for test scoring.
#> 2 26 training rows were used with 10 input features.
#>                                                                                                                                                                                                                                            recommendation
#> 1                                                                                                                                                                               Treat the scores as preliminary and validate on more representative rows.
#> 2 Expect unstable unregularized coefficients. Consider fewer justified features, more training data, or `portfolio = "recommended"` for regularized, forest and boosting alternatives. Compare their training-CV results before interpreting the holdout.
#> 
# Optional JSON export:
# jsonlite::write_json(evidence, "evidence.json", auto_unbox = TRUE, pretty = TRUE)
```
