# Explain an existing model and write a memo

The explanation interface accepts already fitted models. Supply
evaluation predictors, outcomes and a prediction function when the
model’s usual [`predict()`](https://rdrr.io/r/stats/predict.html) method
does not return the required shape. Both `function(newdata, ...)` and
`function(model, newdata, ...)` signatures are supported.

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
#>                                                                                                                                                                                                                                  evidence
#> 1 Limited pairwise screen: absolute Spearman correlation for numeric pairs, correlation ratio for mixed pairs, and Cramer's V for categorical pairs. Small values do not establish independence or exclude nonlinear or joint dependence.
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
#> Target: `mpg` (regression). Models: 2. Inputs: 10.
#> 
#> ## Did the model improve on a simple baseline?
#> The linear regression was evaluated on 6 test rows. Its **rmse** was 3.5529.
#> That is a 30.3% improvement over the intercept-only baseline (5.0963).
#> 
#> ## What the main metric means
#> **rmse:** Typical prediction error, with larger mistakes weighted more heavily; lower is better.
#> 
#> ## Score cautions
#> - **caution:** Only 6 rows were available for test scoring. Treat the scores as preliminary and validate on more representative rows.
#> - **caution:** 26 training rows were used with 10 input features. Use fewer justified features or more training data, and expect unstable coefficients.
#> - **warning:** Evaluation R-squared is negative, so squared error exceeded an evaluation-mean reference. Do not rely on this model for prediction without substantially better validation performance.
#> 
#> ## Findings
#> - Pairwise association flags affect wt, qsec, disp, hp, cyl, am. Inspect joint support; interpret marginal shuffling as fitted reliance and consider ALE for effects.
#> - Shuffle intervals do not resolve the direction of the mean loss change for linear regression / vs. Inspect the repeat distribution; more shuffles address Monte Carlo error only.
#> - The pairwise association screen does not assess every form of dependence. Review nonlinear relationships and joint support before interpreting shuffled inputs or marginal effects.
#> 
#> ## Fitted feature evidence for linear regression
#> Retained primary-model audit.
#> Permutation importance is the change in rmse after shuffling an input. These fitted-model summaries should not be read as causal effects.
#> - wt: 1.5611; shuffle Monte Carlo interval [1.1245, 1.9977]; positive loss change.
#> - qsec: 1.2301; shuffle Monte Carlo interval [0.9354, 1.5249]; positive loss change.
#> - disp: 1.0061; shuffle Monte Carlo interval [0.6047, 1.4076]; positive loss change.
#> Showing three of 8 retained feature summaries; inspect the full audit for the rest.
#> - ALE for wt (predicted value): centered effects range from -5.347 to 3.6053.
#> - ALE for qsec (predicted value): centered effects range from -2.741 to 2.8551.
#> - ALE for disp (predicted value): centered effects range from -3.1662 to 3.2498.
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

[`generate_dashboard()`](https://matt17br.github.io/autoXplainR/reference/generate_dashboard.md)
and
[`create_simple_dashboard()`](https://matt17br.github.io/autoXplainR/reference/create_simple_dashboard.md)
are deprecated in 0.4.0 and will remain callable until at least 0.6.0.
Replace them with `render_model_report(result, output_file)`. For a
memo, pass `narrative = generate_natural_language_report(result)`. Each
compatibility call emits one migration warning. Use
[`render_explanation_report()`](https://matt17br.github.io/autoXplainR/reference/render_explanation_report.md)
for a standalone audit of an existing model.
