# Evaluate already fitted models on explicitly supplied observations

Connects an existing modeling workflow to the same prediction,
explanation, data and report tools as
[`autoxplain()`](https://matt17br.github.io/autoXplainR/reference/autoxplain.md).
No model is fitted, selected, or replaced. The default evaluation role
makes no claim that these rows were excluded from training or model
selection. Classification requires numeric probabilities, with factor
outcome levels declaring the complete class set.

## Usage

``` r
evaluate_models(
  models,
  data,
  outcome,
  task = c("auto", "regression", "binary", "multiclass"),
  predict_functions = NULL,
  labels = NULL,
  primary = names(models)[[1L]],
  reference = NULL,
  training_data = NULL,
  evaluation_role = "evaluation",
  positive = NULL,
  features = NULL,
  seed = 123L
)
```

## Arguments

- models:

  A list of fitted objects with unique non-empty model IDs.

- data:

  Evaluation observations, including the outcome.

- outcome:

  Name of the outcome column.

- task:

  Prediction task, or `"auto"` to infer it from the outcome.

- predict_functions:

  Optional named list of custom prediction functions, keyed by model ID.
  Functions take `newdata`, or `model, newdata`, as in
  [`explain_model()`](https://matt17br.github.io/autoXplainR/reference/explain_model.md).
  They receive the declared `features` and own any preprocessing needed
  by their fitted model. No recipe is learned here. Binary numeric
  outputs must refer to `positive`.

- labels:

  Optional named character vector of display labels.

- primary:

  ID of the user-chosen primary model; defaults to the first ID.

- reference:

  Optional ID of an explicitly supplied comparison reference. It must
  differ from `primary`. No intercept-only model is fitted or inferred.

- training_data:

  Optional training observations supplied only as context. Their use in
  fitting cannot be verified. `NULL` records training unavailable.

- evaluation_role:

  User-declared role: `"evaluation"` (neutral default), `"test"`,
  `"validation"`, or `"training"`. The package cannot verify the role.

- positive:

  Binary event label. Defaults to the second declared level.

- features:

  Predictor columns; `NULL` uses every column except `outcome`. Choose
  explicitly to keep identifiers or future-only information out of the
  model-input and explanation views. Other supplied columns remain local
  data context and require explicit `context_columns` in
  [`report_data_control()`](https://matt17br.github.io/autoXplainR/reference/report_data_control.md)
  to enter the report's data explorer.

- seed:

  Seed recorded for subsequent explanation randomization.

## Value

An `autoxplain_result`. Call
[`render_model_report()`](https://matt17br.github.io/autoXplainR/reference/render_model_report.md)
to prepare its explanations and report. Custom prediction functions are
retained for [`predict()`](https://rdrr.io/r/stats/predict.html) and
[`as_explainers()`](https://matt17br.github.io/autoXplainR/reference/as_explainers.md);
required packages and registered prediction methods must remain
available after serialization.

## Details

Reusable evidence requires deterministic predictors with explicit model
and lexical state. Referenced closure inputs, custom S3 prediction
methods and supported native formula, offset, contrast and inverse-link
extensions are checked for changes. Arbitrary dynamic lookup, external
mutable dependencies and stochastic prediction callbacks are
unsupported; this is not a general proof of purity for R code. If a
model or its assessment inputs change, call `evaluate_models()` again to
create a new assessment before reporting.

## Examples

``` r
fitted <- lm(mpg ~ wt + hp, data = mtcars[1:20, ])
evaluated <- evaluate_models(list(linear = fitted), mtcars[21:32, ], "mpg",
  features = c("wt", "hp")
)
predict(evaluated, mtcars[1:2, ])
#> [1] 23.88007 23.02934
```
