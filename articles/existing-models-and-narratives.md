# Bring your own fitted models

If you already have fitted models, use
[`evaluate_models()`](https://matt17br.github.io/autoXplainR/reference/evaluate_models.md)
to open the same report used by
[`autoxplain()`](https://matt17br.github.io/autoXplainR/reference/autoxplain.md).
It evaluates the supplied models without fitting, selecting or replacing
them. The primary model is your choice.

``` r

training <- mtcars[1:20, ]
evaluation <- mtcars[21:32, ]
fits <- list(
  linear = lm(mpg ~ wt + hp, data = training),
  tree = rpart::rpart(mpg ~ wt + hp, data = training),
  reference = lm(mpg ~ 1, data = training)
)
result <- evaluate_models(
  fits, evaluation, "mpg", features = c("wt", "hp"),
  primary = "linear", reference = "reference"
)
result$leaderboard
#>   rank  model_id     model        role family backend     rmse      mae
#> 1    1    linear    linear     primary linear   stats 2.585338 2.144486
#> 2    2      tree      tree alternative   tree   rpart 4.173713 3.176010
#> 3    3 reference reference    baseline linear   stats 5.270474 4.430000
#>       r_squared model_size_kb complexity training_time_ms prediction_time_ms
#> 1  0.7592824193      23.24219          3               NA                 NA
#> 2  0.3726381958      23.06250          2               NA                 NA
#> 3 -0.0003970558      17.82812          1               NA                 NA
#>                                            fit_warning
#> 1 Training provenance was not recorded by AutoXplainR.
#> 2 Training provenance was not recorded by AutoXplainR.
#> 3 Training provenance was not recorded by AutoXplainR.
predict(result, evaluation[1:3, ])
#> [1] 24.96928 19.11718 19.40076
```

The default data role is `"evaluation"`; the package cannot verify
whether these rows were excluded from fitting or earlier choices. This
ordered `mtcars` split is a teaching example. Declare
`evaluation_role = "test"` only when your analysis design supports that
interpretation.

Choose `features` explicitly. By default, every column except the
outcome is treated as an input, including an ID or context column that
your adapter might ignore. The report’s data and feature views use this
declared input set.

``` r

path <- tempfile(fileext = ".html")
render_model_report(result, path, n_repeats = 5)
file.exists(path)
#> [1] TRUE
unlink(path)
```

The report shows scores, fitted behavior, error diagnostics and the
supplied data. Training data are **unavailable** unless you pass
`training_data`; supplied training data add context but do not prove how
a model was fitted. No search history or training time is invented. A
reference is optional and can be any explicitly supplied model. Without
one, baseline improvement and paired reference intervals are
unavailable.

The default report embeds aggregate summaries. Set
`report_data = "rows"` to add individual-record inspection and links
from errors to source rows. With no training table supplied, **Explore
data** shows only evaluation distributions; it does not invent a
training sample. Save the R result with
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) to retain models,
data and custom prediction functions; external dependencies used by
those functions must still be available when you reload it.

## Custom prediction functions

For an unsupported model class, supply a named `predict_functions` list.
Each function takes `newdata`, or `model, newdata`, and returns one
prediction per row. Regression uses a numeric vector; binary
classification uses probabilities of `positive`; multiclass uses a
probability matrix with named class columns. The outcome factor declares
the complete class set. Your adapter owns any preprocessing needed by
the fit;
[`evaluate_models()`](https://matt17br.github.io/autoXplainR/reference/evaluate_models.md)
does not learn a recipe.

``` r

result <- evaluate_models(
  list(my_fit = fitted_object), evaluation_data, "outcome",
  features = c("age", "measurement"),
  predict_functions = list(my_fit = function(model, newdata) {
    predict(model, newdata, type = "response")
  }),
  task = "binary", positive = "yes"
)
```

## Work with one explanation or audit

Use
[`explain_model()`](https://matt17br.github.io/autoXplainR/reference/explain_model.md)
and
[`audit_explanations()`](https://matt17br.github.io/autoXplainR/reference/audit_explanations.md)
when you want a focused audit or an individual estimator rather than the
full model report. This interface also supports hard-label
classification for metrics such as accuracy.

``` r

train <- mtcars[1:24, ]
test <- mtcars[25:32, ]
fit <- lm(mpg ~ wt + hp, data = train)
existing <- explain_model(
  fit, test[c("wt", "hp", "mpg")], y = "mpg", label = "existing linear model",
  predict_function = function(model, newdata) as.numeric(predict(model, newdata)),
  metadata = list(evaluation_role = "test")
)
audit <- audit_explanations(existing, n_repeats = 20)
audit$findings
#>   severity                     code
#> 1     note association_screen_scope
#>                                                                     message
#> 1 The pairwise association screen does not assess every form of dependence.
#>                                                                                                                                                                                                                                                                                                                                               evidence
#> 1 Limited pairwise screen: absolute Spearman correlation for numeric pairs, correlation ratio for mixed pairs, and Cramer's V for categorical pairs. Small values do not establish independence or exclude nonlinear or joint dependence. Categorical pairs without repeated categories are unavailable; many rare categories can inflate association.
#>                                                                                              recommendation
#> 1 Review nonlinear relationships and joint support before interpreting shuffled inputs or marginal effects.
#>   model feature                            scope     entities
#> 1  <NA>    <NA> Limits of the association screen existing....
```

A regression prediction function returns one numeric value per row. A
binary function returns the probability of the declared positive class;
use `positive` to name the requested event and `probability_class` when
a custom vector describes the other event. Hard-label adapters can
support accuracy, but cannot supply probability losses or probability
effects. Multiclass functions return a probability matrix with named
class columns. Preprocessing must match what the fitted model expects;
[`explain_model()`](https://matt17br.github.io/autoXplainR/reference/explain_model.md)
does not learn a new preprocessing recipe. The metadata records the
evaluation role you assert, not independent verification of that role.

For a binomial GLM fitted to numeric 0/1 outcomes, the native response
probability means outcome 1, regardless of evaluation factor order. A
factor GLM fitted with `model = FALSE` no longer retains its original
response levels: set `probability_class` to the event its response
probability represents. This follows [R’s binomial response
conventions](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/family.html).
Recreate a custom explainer and recompute its evidence after changing
external state used by its prediction function. A content check on
observed predictions cannot establish equivalence on every possible
perturbed input.

``` r

path <- tempfile(fileext = ".html")
render_explanation_report(audit, path)
file.exists(path)
#> [1] TRUE
unlink(path)
```

## Narrative from retained evidence

``` r

result <- autoxplain(mtcars, "mpg", seed = 2026)
memo <- generate_natural_language_report(result)
cat(memo)
#> # Model Fit and Evaluation Report
#> 
#> ## Scope
#> Target: `mpg` (regression). Models: 4. Inputs: 10.
#> 
#> ## How did the primary model perform?
#> The tuned decision tree was evaluated on 6 test rows. Its **rmse** was 2.6856.
#> That is a 47.3% improvement over intercept-only baseline (5.0963).
#> 
#> ## What the main metric means
#> **rmse:** Typical prediction error, with larger mistakes weighted more heavily; lower is better.
#> 
#> ## Score cautions
#> - **caution:** Only 6 rows were available for test scoring. Treat the scores as preliminary and validate on more representative rows.
#> - **caution:** 26 training rows were used with 10 input features. Expect unstable unregularized coefficients. Consider fewer justified features, more training data, or `portfolio = "recommended"` for regularized, forest and boosting alternatives. Compare their training-CV results before interpreting the holdout.
#> 
#> ## How automatic tuning selected the model
#> 15 configurations across 3 model families were compared with 5 training-only folds. The selection metric was rmse.
#> The one-standard-error (prefer the documented family priority, then the lowest recorded capacity proxy among eligible settings within that family) rule selected the decision tree with max depth = 3, pruning cp = 0.01, minimum split = 5. Its resampled rmse was 3.03628.
#> The resampling-selected configuration was `tree_05`; the actual final fitted configuration was `tree_05` (decision tree). A recorded refit fallback was not needed.
#> That resampled score selected a configuration; it is not the final performance estimate. The held-out score above evaluated the selected, refitted model on different rows.
#> 
#> ## What did the retained models do differently?
#> This section is computed evidence from common evaluation rows, not a claim inferred from model-family names.
#> The largest average paired prediction difference was between `main_model` and `linear_model`: 2.5416 using absolute difference in predicted target units.
#> Descriptive comparison of supplied fitted models on common evaluation rows; not causal inference, uncertainty coverage, or a deployment rule.
#> 
#> ## Findings
#> - Pairwise association flags affect cyl, hp, disp, drat, wt, qsec, am, gear. Inspect joint support; interpret marginal shuffling as fitted reliance and consider ALE for effects.
#> - Shuffle intervals do not resolve the direction of the mean loss change for tuned neural network alternative / gear, tuned neural network alternative / qsec, tuned neural network alternative / am, tuned neural network alternative / drat, tuned neural network alternative / carb, linear regression reference / vs, linear regression reference / am. Inspect the repeat distribution; more shuffles address Monte Carlo error only.
#> - The pairwise association screen does not assess every form of dependence. Review nonlinear relationships and joint support before interpreting shuffled inputs or marginal effects.
#> 
#> ## Fitted feature evidence for tuned decision tree
#> Retained primary-model audit.
#> Permutation importance is the change in rmse after shuffling an input. These fitted-model summaries should not be read as causal effects.
#> - cyl: 0.6933; shuffle Monte Carlo interval [0.2558, 1.1308]; positive loss change.
#> - hp: 0.4808; shuffle Monte Carlo interval [0.2272, 0.7344]; positive loss change.
#> - disp: 0.00; shuffle Monte Carlo interval [0.00, 0.00]; no observed change.
#> Showing three of 10 retained feature summaries; inspect the full audit for the rest.
#> - ALE for cyl (predicted value): centered effects range from -1.5606 to 3.1213.
#> - ALE for hp (predicted value): centered effects range from -1.7733 to 0.8867.
#> - ALE for disp (predicted value): centered effects range from 0.00 to 0.00.
#> - ALE for drat (predicted value): centered effects range from 0.00 to 0.00.
#> - ALE for wt (predicted value): centered effects range from 0.00 to 0.00.
#> - ALE for qsec (predicted value): centered effects range from 0.00 to 0.00.
#> - ALE for vs (predicted value): centered effects range from 0.00 to 0.00.
#> - ALE for am (predicted value): centered effects range from 0.00 to 0.00.
#> Separate descriptive diagnostics; no overall evidence grade. Shuffle intervals omit evaluation-sampling, fitting and selection uncertainty.
#> - performance uncertainty: not run. 
#> - resources: not run. This optional check has not been computed.
#> - model behavior: not run. This optional check has not been computed.
#> - prediction disagreement: not run. This optional check has not been computed.
#> - decision cutoffs: not applicable. Decision-cutoff comparisons apply to binary classification.
#> - comparison: insufficient evidence. Fewer than two supplied models meet the performance tolerance.
#> 
#> ## What to do next
#> Inspect the held-out errors and explanation evidence, check whether the data represent the intended use, and validate the result on new data before relying on it.
#> 
#> ## Required limitations
#> Shuffle repeats measure randomness in this calculation, not population uncertainty. 
#> Feature explanations do not establish causality, fairness, safety, or deployment readiness. 
#> Use held-out data, domain review, and external validation.
#> 
#> Data disclosure: Aggregate diagnostics only; raw rows, fitted model objects and case-level predictions are omitted. Names and diagnostic text may still contain sensitive information.
```

The local memo combines performance, retained importance, audit
findings, fitted effects and failed checks without refitting or
rerunning explanations. An explicit `audit` replaces only that
component; `importance_data` and `pdp_data` override importance and
effects respectively. Evaluation evidence is preserved. Identified
audits, effects and importance tables must match the fitted model and
ordered evaluation data. Bare importance tables are labeled
user-supplied with unverified identity; set their `metric` attribute
explicitly. A fitting-only result reports that feature evidence was not
supplied.

``` r

# Configure the credentials listed by narrative_providers() first.
memo <- generate_natural_language_report(result, provider = "gemini")
render_model_report(result, "report.html", narrative = memo)
```

Hosted prose requires an explicit provider. The package checks response
format and length and appends interpretation limits; it does not verify
numerical claims or guarantee that the model follows its prompt. Review
claims against the computed output. The default local memo does not send
a request, even when API keys exist. Prompt construction omits raw rows
and case predictions, but feature names, aggregates and diagnostic text
may still be sensitive.

Inspect
[`narrative_providers()`](https://matt17br.github.io/autoXplainR/reference/narrative_providers.md)
and
[`?generate_natural_language_report`](https://matt17br.github.io/autoXplainR/reference/generate_natural_language_report.md)
for adapter configuration. Provider availability and terms can change
independently of the package.

The memo reads diagnostic status from the supplied result without
running new checks. Rendering may compute optional comparisons in a
local copy of that result; it returns their compact status in
`attr(report_path, "diagnostic_status")` and leaves the original result
unchanged. A later memo from the original result therefore still
describes those optional checks as not run unless their status records
were retained.

## Migrating old report calls

Saved 0.5.0 regression, binary and multiclass results were replayed with
explicit explanation recomputation. Their older retained audits are
rejected by the newer identity check. For an unchanged saved fit:

``` r

older <- readRDS("analysis-0.5.0.rds")
render_model_report(older, "updated-report.html", top_features = 2, n_repeats = 3)
```

The small budget illustrates the migration; choose a suitable
explanation budget for the final report. Rendering validates the
available stored predictions and metrics, then recomputes explanation
evidence without refitting. It cannot recover missing raw context or
reconstruct historical external state used by a custom function on
unobserved inputs. Changed models need a new evaluation via
[`evaluate_models()`](https://matt17br.github.io/autoXplainR/reference/evaluate_models.md)
rather than combining their predictions with saved scores.

[`generate_dashboard()`](https://matt17br.github.io/autoXplainR/reference/generate_dashboard.md)
and
[`create_simple_dashboard()`](https://matt17br.github.io/autoXplainR/reference/create_simple_dashboard.md)
were deprecated in 0.4.0 and remain compatibility entry points in 0.6.0.
Replace them with `render_model_report(result, output_file)`. For a
memo, pass `narrative = generate_natural_language_report(result)`. Each
compatibility call emits one migration warning. Use
[`evaluate_models()`](https://matt17br.github.io/autoXplainR/reference/evaluate_models.md)
followed by
[`render_model_report()`](https://matt17br.github.io/autoXplainR/reference/render_model_report.md)
for existing fitted models, or
[`render_explanation_report()`](https://matt17br.github.io/autoXplainR/reference/render_explanation_report.md)
for a standalone explanation audit.
