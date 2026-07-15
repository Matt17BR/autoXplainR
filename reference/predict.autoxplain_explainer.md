# Predict with an AutoXplainR explainer

Predict with an AutoXplainR explainer

## Usage

``` r
# S3 method for class 'autoxplain_explainer'
predict(object, newdata, ...)
```

## Arguments

- object:

  An `autoxplain_explainer`.

- newdata:

  Data frame with the explainer's feature schema.

- ...:

  Unused.

## Value

A numeric vector for regression or binary classification, or a
probability matrix for multiclass classification.
