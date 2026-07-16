# Inspect automatic tuning evidence

Returns the complete training-only resampling record created by
`autoxplain(..., model_set = "tuned")`. The outer evaluation rows are
never used to rank configurations.

## Usage

``` r
tuning_results(result)
```

## Arguments

- result:

  An `autoxplain_result` fitted with `model_set = "tuned"`.

## Value

An object of class `autoxplain_tuning` containing the candidate table,
fold-level scores, selected configuration, resampling boundary, and
search settings.

## Examples

``` r
tuned <- autoxplain(iris, "Sepal.Length", model_set = "tuned",
                    max_models = 4, nfolds = 3, seed = 2026)
tuning_results(tuned)
#> <AutoXplainR training-only tuning>
#>   search:     4 configurations across 3 model families
#>   resampling: 3 folds; rmse minimized
#>   rule:       one-standard-error (prefer the simpler near-best configuration)
#>   selected:   linear regression (reference_01)
#>   score:      0.32064 +/- 0.01501 SE
#>   boundary:   Configuration ranking used only resamples of the outer training rows; the held-out evaluation rows were untouched until final scoring.
#>  configuration_id             model
#>      reference_01 linear regression
#>         neural_01    neural network
#>           tree_02     decision tree
#>           tree_01     decision tree
#>                                       hyperparameters  cv_score      cv_se
#>                               default statistical fit 0.3206438 0.01500691
#>                  hidden units = 1, weight decay = 0.1 0.3383003 0.02275242
#>  max depth = 4, pruning cp = 0.01, minimum split = 14 0.3950433 0.01624737
#>  max depth = 2, pruning cp = 0.03, minimum split = 24 0.4084825 0.02560590
#>  complexity_proxy selected status
#>                 5     TRUE     ok
#>                 7    FALSE     ok
#>                 9    FALSE     ok
#>                 4    FALSE     ok
```
