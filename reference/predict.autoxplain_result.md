# Predict from an AutoXplainR result

Applies the stored training recipe to raw predictor rows, then predicts
with the pre-specified or training-selected primary model. No model is
refitted. The target column is optional and ignored. Row order and row
count are kept; recipes that drop incomplete rows return `NA` at those
positions.

## Usage

``` r
# S3 method for class 'autoxplain_result'
predict(object, newdata, model = NULL, type = c("response", "class"), ...)
```

## Arguments

- object:

  An
  [`autoxplain()`](https://matt17br.github.io/autoXplainR/reference/autoxplain.md)
  result.

- newdata:

  Data frame of raw predictor rows.

- model:

  One model ID or index. `NULL` uses the primary model.

- type:

  `"response"` returns numeric predictions for regression, positive-
  class probabilities for binary tasks, and a named probability matrix
  for multiclass tasks. `"class"` returns a factor for classification.

- ...:

  Reserved for future use; additional arguments are rejected.

## Value

A vector or matrix with one prediction per input row. Classification
levels follow the training outcome; binary probabilities refer to its
second level. Novel categorical levels follow the recorded recipe
strategy.

## Examples

``` r
result <- autoxplain(mtcars, "mpg", explain = FALSE)
predict(result, mtcars[1:3, c("wt", "hp", "cyl", "disp", "drat",
                             "qsec", "vs", "am", "gear", "carb")])
#> [1] 21.90628 20.94303 27.06011
```
