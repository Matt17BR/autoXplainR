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
  metadata = list(),
  probability_class = NULL
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
  matrix/data frame with one column per class). Hard classification
  labels are retained as factors for accuracy only; they cannot supply
  probability losses or probability effects. An ellipsis after the
  supported arguments is allowed.

- task:

  One of `"auto"`, `"regression"`, `"binary"`, or `"multiclass"`.

- label:

  Human-readable model label.

- positive:

  Positive outcome level for binary classification. By default the
  second outcome level is used.

- metadata:

  Optional named list recorded in the explainer provenance.

- probability_class:

  Event represented by a binary probability vector. For custom functions
  it defaults to `positive`. For native adapters it overrides the
  inferred event; supply it when fitted response levels are unavailable,
  such as a factor GLM fitted with `model = FALSE`. Numeric binomial
  GLMs model event `"1"`, logical GLMs model `"TRUE"`, and factor GLMs
  model their second fitted level. Named probability matrices identify
  their events directly.

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
#>   id:       axr-bd58152b4926c4339009b0638db5e337c254bfe788406eb3eb28962d77b2a046
```
