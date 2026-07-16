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
fold-level scores, out-of-fold predictions, selected configuration,
resampling boundary, and search settings.

## Details

`out_of_fold_predictions` stores one row per outer-training case and
configuration. `training_row` is the stable position used for paired
model comparisons, `source_row` preserves the input row name, and
`case_loss` stores squared error or negative log likelihood.
Classification probability vectors are stored compactly as the
matrix-column `probabilities`. If fold-specific preprocessing removes
rows (for example with `missing_value_strategy = "drop_rows"`),
`omitted_rows` records their identity and fold, while `rows_evaluated`
and candidate `evaluated_rows` count only cases that actually received
out-of-fold predictions.

`refit` records every full-training refit attempt. If the
resampling-selected configuration cannot be refitted, AutoXplainR tries
the remaining valid configurations in resampled-score order, records the
first successful one in `final_configuration`, and marks
`fallback_used`. Failed alternative-family refits are diagnosed but do
not discard a usable primary model. Families for which every
configuration failed resampling remain visible in
`families_resampling_failed` and are excluded from paired prediction
evidence.

## Examples

``` r
tuned <- autoxplain(iris, "Sepal.Length", model_set = "tuned",
                    portfolio = "core", max_models = 4,
                    nfolds = 3, seed = 2026)
tuning_results(tuned)
#> <AutoXplainR training-only tuning>
#>   search:     4 configurations across 3 model families
#>   resampling: 3 folds; rmse minimized
#>   evidence:   120/120 outer-training rows predicted
#>   rule:       one-standard-error (prefer the reviewed family priority, then the least-flexible near-best setting within that family)
#>   selected:   linear regression (linear_01)
#>   final fit:  linear_01
#>   score:      0.32064 +/- 0.01501 SE
#>   boundary:   Configuration ranking used only resamples of the outer training rows; the held-out evaluation rows were untouched until final scoring. Every outer-training row received an out-of-fold prediction.
#>   proxy:      family-specific flexibility; values are not comparable across families
#>  configuration_id family    backend             model
#>         linear_01 linear stats/nnet linear regression
#>         neural_01 neural       nnet    neural network
#>           tree_02   tree      rpart     decision tree
#>           tree_01   tree      rpart     decision tree
#>                                       hyperparameters  cv_score      cv_se
#>                               default statistical fit 0.3206438 0.01500691
#>                  hidden units = 1, weight decay = 0.1 0.3379306 0.02288559
#>  max depth = 4, pruning cp = 0.01, minimum split = 14 0.3950433 0.01624737
#>  max depth = 2, pruning cp = 0.03, minimum split = 24 0.4084825 0.02560590
#>  complexity_proxy selected status
#>                 5     TRUE     ok
#>                 7    FALSE     ok
#>                 9    FALSE     ok
#>                 4    FALSE     ok
```
