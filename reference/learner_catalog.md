# Inspect AutoXplainR learner families

Returns the learner-family contract used by training-only tuning.
Families describe the kind of pattern a model can learn; backends
identify the R implementation. Keeping those concepts separate lets
reports explain model behavior without exposing beginners to
backend-specific syntax.

## Usage

``` r
learner_catalog(task = NULL)
```

## Arguments

- task:

  Optional task filter: one of `"regression"`, `"binary"`, or
  `"multiclass"`.

## Value

A data frame with learner capabilities, dependencies, and plain-
language behavior notes.

## Examples

``` r
learner_catalog()
#> <AutoXplainR learner catalog>
#>   families:  10
#>   available: 9
#>       family    backend                supported_tasks available
#>       linear stats/nnet regression, binary, multiclass      TRUE
#>  regularized     glmnet regression, binary, multiclass      TRUE
#>     additive       mgcv             regression, binary      TRUE
#>         tree      rpart regression, binary, multiclass      TRUE
#>       forest     ranger regression, binary, multiclass      TRUE
#>     boosting    xgboost regression, binary, multiclass      TRUE
#>       neural       nnet regression, binary, multiclass      TRUE
#>       kernel      e1071 regression, binary, multiclass      TRUE
#>    neighbors       kknn regression, binary, multiclass     FALSE
#>         mars      earth             regression, binary      TRUE
#>                      nonlinearity
#>   none unless encoded in features
#>   none unless encoded in features
#>   smooth, one predictor at a time
#>                          stepwise
#>      flexible threshold ensembles
#>     sequential threshold ensemble
#>               smooth and flexible
#>         smooth similarity surface
#>               local neighborhoods
#>  piecewise-linear hinge functions
#>                                         interactions
#>                    none unless specified in features
#>                    none unless specified in features
#>                 not in the automatic default formula
#>                           automatic along tree paths
#>                          automatic across tree paths
#>                  automatic across boosted tree paths
#>                       automatic through hidden units
#>  implicit through distances in encoded feature space
#>              implicit through multivariable distance
#>                       optional and bounded by degree
learner_catalog("regression")
#> <AutoXplainR learner catalog>
#>   families:  10
#>   available: 9
#>       family    backend                supported_tasks available
#>       linear stats/nnet regression, binary, multiclass      TRUE
#>  regularized     glmnet regression, binary, multiclass      TRUE
#>     additive       mgcv             regression, binary      TRUE
#>         tree      rpart regression, binary, multiclass      TRUE
#>       forest     ranger regression, binary, multiclass      TRUE
#>     boosting    xgboost regression, binary, multiclass      TRUE
#>       neural       nnet regression, binary, multiclass      TRUE
#>       kernel      e1071 regression, binary, multiclass      TRUE
#>    neighbors       kknn regression, binary, multiclass     FALSE
#>         mars      earth             regression, binary      TRUE
#>                      nonlinearity
#>   none unless encoded in features
#>   none unless encoded in features
#>   smooth, one predictor at a time
#>                          stepwise
#>      flexible threshold ensembles
#>     sequential threshold ensemble
#>               smooth and flexible
#>         smooth similarity surface
#>               local neighborhoods
#>  piecewise-linear hinge functions
#>                                         interactions
#>                    none unless specified in features
#>                    none unless specified in features
#>                 not in the automatic default formula
#>                           automatic along tree paths
#>                          automatic across tree paths
#>                  automatic across boosted tree paths
#>                       automatic through hidden units
#>  implicit through distances in encoded feature space
#>              implicit through multivariable distance
#>                       optional and bounded by degree
```
