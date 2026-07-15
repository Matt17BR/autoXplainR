# Create a model-agnostic explainer

`explain_model()` defines the prediction contract used by AutoXplainR.
It deliberately separates model fitting from explanation, so base R
models, H2O models, and models from other frameworks can be audited in
the same way.

## Usage

``` r
explain_model(
  model,
  data,
  y,
  predict_function = NULL,
  task = c("auto", "regression", "binary", "multiclass"),
  label = NULL,
  positive = NULL,
  metadata = list()
)
```

## Arguments

- model:

  A fitted model object.

- data:

  A data frame. It may contain the outcome when `y` is the name of a
  column; the outcome is removed from the feature data stored in the
  explainer.

- y:

  Outcome values, or a single column name in `data`.

- predict_function:

  Optional prediction function. It may have signature
  `function(model, newdata)` or `function(newdata)`. Regression
  functions should return a numeric vector. Classification functions
  should return probabilities (a vector for binary outcomes or a
  matrix/data frame with one column per class).

- task:

  One of `"auto"`, `"regression"`, `"binary"`, or `"multiclass"`.

- label:

  Human-readable model label.

- positive:

  Positive outcome level for binary classification. By default the
  second outcome level is used.

- metadata:

  Optional named list recorded in the explainer provenance.

## Value

An object of class `autoxplain_explainer`.

## Examples

``` r
fit <- lm(mpg ~ wt + hp, data = mtcars)
explainer <- explain_model(fit, mtcars, y = "mpg", label = "linear model")
explainer
#> <AutoXplainR explainer>
#>   model:    linear model
#>   task:     regression
#>   data:     32 rows x 10 features
#>   id:       axr-26a5a153
```
