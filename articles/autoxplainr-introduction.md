# Your First Model with AutoXplainR

## What AutoXplainR is for

Fitting a model is only one part of a useful analysis. A first-time
modeler also needs to know whether the model works on unseen rows,
whether it improves on a simple guess, which fitted patterns matter, and
how to describe those patterns without claiming more than the analysis
established.

AutoXplainR puts those questions in one workflow. Its safe default is
local and requires neither Java nor an API account. This vignette uses
the small built-in `mtcars` data only so every step is reproducible.

## Fit and evaluate in one call

``` r

result <- autoxplain(
  mtcars,
  target_column = "mpg",
  seed = 2026
)

result
#> <AutoXplainR guided result>
#>   question:   predict `mpg` (regression)
#>   engine:     base
#>   data:       26 training + 6 evaluation rows
#>   models:     2 (primary + baseline)
#>   result:     primary model has rmse = 3.5529
#>   baseline:   30.3% improvement in rmse
#>   next:       use as_explainers() to investigate the fitted patterns
```

`mpg` is numeric with more than two distinct values, so this is
recognized as a regression problem. AutoXplainR reproducibly holds out
20% of the rows, learns preprocessing from the remaining rows, and fits
two models:

- a linear regression using the input features; and
- an intercept-only baseline that ignores all input features.

The baseline matters. It answers whether the fitted relationships
improve on a very simple reference rather than merely producing
predictions.

## Read the held-out metrics

``` r

result$leaderboard
#>   rank        model_id                   model     role     rmse      mae
#> 1    1      main_model       linear regression  primary 3.552918 2.797996
#> 2    2 simple_baseline intercept-only baseline baseline 5.096266 3.907692
#>    r_squared training_time_ms model_size_kb complexity fit_warning
#> 1 -0.1281933                1      44.43750         11            
#> 2 -1.3212249                2      19.82812          1            
#>   prediction_time_ms
#> 1                  1
#> 2                  0
result$evaluation$metric_definitions
#>                                                                                                rmse 
#>            "Typical prediction error, with larger mistakes weighted more heavily; lower is better." 
#>                                                                                                 mae 
#>                         "Average absolute prediction error in the target's units; lower is better." 
#>                                                                                           r_squared 
#> "Share of held-out variation explained relative to predicting the held-out mean; higher is better."
result$evaluation$improvement_over_baseline
#> [1] 0.3028389
```

For regression, RMSE is the primary comparison metric. It is a typical
error in the outcome’s units, with larger errors weighted more heavily.
Lower is better. MAE is the average absolute error. R-squared compares
squared error with a held-out mean reference and can be negative on
genuinely unseen data.

The exact values from a six-row `mtcars` holdout are not a basis for
scientific conclusions; the example demonstrates the contract. Real
analyses need an evaluation set large and representative enough for the
intended use.

## Build the report

``` r

path <- tempfile(fileext = ".html")
render_model_report(result, path, top_features = 4, n_repeats = 5)
file.exists(path)
#> [1] TRUE
unlink(path)
```

The standalone report uses progressive disclosure:

1.  the modeling question and baseline verdict;
2.  every held-out metric with definitions;
3.  input reliance and fitted effect directions;
4.  warnings and concrete next actions; and
5.  a collapsed technical evidence audit and complete provenance.

The report does not need an LLM, Plotly, H2O, or a browser runtime after
it is written.

## Compare a small candidate set

The first call stays intentionally simple. When comparing a few
approachable model shapes would help, use the explicit comparison mode:

``` r

comparison <- autoxplain(
  iris,
  target_column = "Sepal.Length",
  model_set = "comparison",
  test_fraction = 0.4,
  seed = 2026
)

model_tradeoffs(comparison)
#> <AutoXplainR model trade-offs>
#>   performance: rmse (lower is better)
#>   complexity:  model_size_kb (lower is better)
#>   Pareto set:  3 / 4 supplied models
#>         model_id                   model      role      rmse model_size_kb
#>       main_model       linear regression   primary 0.2963068      49.46094
#>       small_tree     small decision tree candidate 0.3744086      36.66406
#>  simple_baseline intercept-only baseline  baseline 0.8691950      32.51562
#>    flexible_tree  flexible decision tree candidate 0.4126230      43.25000
#>  pareto_optimal
#>            TRUE
#>            TRUE
#>            TRUE
#>           FALSE
#>   note: Pareto status compares only the supplied models on the supplied evaluation data; it is not a final model-selection rule.
```

This adds a shallow tree and a more flexible tree to the pre-specified
statistical model and intercept-only baseline. Pareto status asks
whether another supplied model is at least as good on both held-out
performance and model size, with a strict improvement on one.

The comparison is descriptive. AutoXplainR does not silently promote the
holdout winner to primary, because selecting on the holdout and
reporting the same score as final performance would be optimistic. Use
fresh validation or a pre-specified resampling procedure when model
selection itself is the goal.

An interactive view is available when Plotly is installed:

``` r

plot_model_comparison(comparison)
```

## Investigate one fitted pattern

The guided result can be converted to a model-agnostic explainer. Select
the primary model rather than the simple baseline:

``` r

explainer <- as_explainers(result, models = "main_model")$main_model
explainer
#> <AutoXplainR explainer>
#>   model:    main_model
#>   task:     regression
#>   data:     6 rows x 10 features
#>   id:       axr-7221ef31
```

Repeated permutation importance measures how much held-out performance
changes when an input is shuffled:

``` r

importance <- calculate_permutation_importance(
  explainer,
  features = c("wt", "hp", "disp"),
  n_repeats = 10,
  seed = 2026
)

importance
#> <AutoXplainR permutation importance>
#>   metric: rmse | repeats: 10 | baseline: 3.5529
#>  feature importance std_error  conf_low conf_high sign_stability
#>       wt  1.7808278 0.3005901 1.1008457  2.460810            0.9
#>     disp  0.8514659 0.2980729 0.1771782  1.525754            0.9
#>       hp  0.6710299 0.1756528 0.2736757  1.068384            0.9
#>   intervals describe permutation Monte Carlo variation, not population inference
```

The result keeps every repeat, a Monte Carlo interval, and sign
stability. The interval describes randomness from shuffling. It is not a
confidence interval for a population and cannot support a causal claim.

[`explain_effect()`](https://matt17br.github.io/autoXplainR/reference/explain_effect.md)
defaults to accumulated local effects (ALE) for numeric inputs:

``` r

weight_effect <- explain_effect(explainer, feature = "wt")
weight_effect
#> <AutoXplainR ALE effect>
#>   feature: wt | rows: 6 | max association: 0.829
#>        wt accumulated_effect std_error   conf_low  conf_high n support
#>  2.688750          1.0055717         0  1.0055717  1.0055717 1       1
#>  2.826250          1.0055717         0  1.0055717  1.0055717 0       0
#>  2.963750          1.0055717         0  1.0055717  1.0055717 0       0
#>  3.101250          1.0055717         0  1.0055717  1.0055717 0       0
#>  3.206250          0.7587882         0  0.7587882  0.7587882 1       1
#>  3.278750          0.7587882         0  0.7587882  0.7587882 0       0
#>  3.351250          0.7587882         0  0.7587882  0.7587882 0       0
#>  3.423750          0.7587882         0  0.7587882  0.7587882 0       0
#>  3.493750          0.5290243         0  0.5290243  0.5290243 1       1
#>  3.561250          0.5290243         0  0.5290243  0.5290243 0       0
#>  3.628750          0.5290243         0  0.5290243  0.5290243 0       0
#>  3.696250          0.5290243         0  0.5290243  0.5290243 0       0
#>  3.744375          0.4311619         0  0.4311619  0.4311619 1       1
#>  3.773125          0.4311619         0  0.4311619  0.4311619 0       0
#>  3.801875          0.4311619         0  0.4311619  0.4311619 0       0
#>  3.830625          0.4311619         0  0.4311619  0.4311619 0       0
#>  4.020625         -0.7644614         0 -0.7644614 -0.7644614 1       1
#>  4.371875         -0.7644614         0 -0.7644614 -0.7644614 0       0
#>  4.723125         -0.7644614         0 -0.7644614 -0.7644614 0       0
#>  5.074375         -1.9600847         0 -1.9600847 -1.9600847 1       1
```

ALE summarizes local prediction changes inside observed feature bins. It
reduces the most direct extrapolation problem of a marginal
partial-dependence plot under dependent inputs, but it still describes a
fitted prediction function—not the effect of intervening on vehicle
weight.

## Open the reliability layer

The evidence audit is available when more technical review is useful:

``` r

audit <- audit_explanations(
  as_explainers(result),
  features = c("wt", "hp", "disp"),
  n_repeats = 10,
  seed = 2026
)

audit
#> <AutoXplainR explanation evidence audit>
#>   grade:              C (diagnostic, not certification)
#>   models:             2 (1 near-optimal)
#>   stable claims:      16.7%
#>   max dependence:     0.829
#>   explanation accord: n/a (one model)
#>   prediction accord:  n/a (one model)
#> 
#> Findings
#>   [warning] 2 feature(s) exceed the dependence threshold.
#>   [warning] 5 model-feature claim(s) are qualified or unsupported.
#>   [note] The permutation budget is suitable for smoke testing, not a final report.
```

The audit checks computation stability, feature association,
supplied-model performance, and explanation agreement. Because this
example supplies a primary model and a baseline, it can also demonstrate
that only competitively performing supplied models should influence an
explanation comparison.

The A–D grade is a triage heuristic. It is not a p-value, certification,
fairness test, or safety assessment.

## Add a narrative only if it helps communication

The deterministic provider works offline:

``` r

memo <- generate_natural_language_report(result)
cat(substr(memo, 1, 400), "...")
#> # Model Fit and Evaluation Report
#> 
#> ## Scope
#> This is a descriptive summary of a regression task for `mpg`, covering 2 model(s) and 10 feature(s).
#> 
#> ## Did the model improve on a simple baseline?
#> The linear regression was evaluated on 6 held-out test rows. Its **rmse** was 3.5529.
#> That is a 30.3% improvement over the intercept-only baseline (5.0963).
#> 
#> ## What the main metric means
#> **rmse:** Typical p ...
attr(memo, "narrative_provenance")
#> $provider_requested
#> [1] "local"
#> 
#> $provider_used
#> [1] "local"
#> 
#> $model
#> NULL
#> 
#> $remote
#> [1] FALSE
#> 
#> $fallback
#> [1] FALSE
#> 
#> $disclosure
#> [1] "Aggregated diagnostics only. No raw rows, fitted model objects, case-level predictions, or secrets are included."
#> 
#> $error
#> NULL
```

Remote use requires an explicit provider:

``` r

Sys.setenv(GEMINI_API_KEY = "...")
memo <- generate_natural_language_report(result, provider = "gemini")

# Or use a model running locally through Ollama
memo <- generate_natural_language_report(result, provider = "ollama")
```

Only aggregated diagnostics are placed in the prompt. Raw rows, fitted
objects, case-level predictions, and secrets are excluded. Provider and
model details are attached to the returned text so the prose can be
audited. Generated prose still needs human review against the numerical
report.

## Classification uses the same contract

``` r

flowers <- autoxplain(iris, target_column = "Species", seed = 2026)
flowers
#> <AutoXplainR guided result>
#>   question:   predict `Species` (multiclass)
#>   engine:     base
#>   data:       120 training + 30 evaluation rows
#>   models:     2 (primary + baseline)
#>   result:     primary model has log_loss = 1.1513
#>   baseline:   -4.8% improvement in log_loss
#>   next:       use as_explainers() to investigate the fitted patterns
flowers$evaluation$metric_definitions
#>                                                                        log_loss 
#>    "Probability error that penalizes confident wrong answers; lower is better." 
#>                                                                     brier_score 
#>                           "Average squared probability error; lower is better." 
#>                                                                        accuracy 
#>       "Share of held-out rows assigned to the correct class; higher is better." 
#>                                                                    macro_recall 
#> "Recall calculated for each class and then averaged equally; higher is better."
```

Classification splits are stratified. Binary tasks report log loss,
Brier score, accuracy, balanced accuracy, and ROC AUC. Multiclass tasks
report log loss, Brier score, accuracy, and macro recall. Log loss is
primary because it evaluates probability quality and penalizes confident
wrong predictions.

## Bring an existing model

The explanation layer does not depend on the guided fitting engine. Wrap
an ordinary R model directly:

``` r

train <- mtcars[1:24, ]
test <- mtcars[25:32, ]
fit <- lm(mpg ~ wt + hp + disp, data = train)

existing <- explain_model(
  fit,
  test,
  y = "mpg",
  label = "existing linear model",
  metadata = list(evaluation_role = "test")
)
```

Other frameworks can supply a `predict_function`. H2O AutoML remains an
explicit optional fitting engine through
`autoxplain(..., engine = "h2o")`.

## What to say—and not say

Reasonable language is specific to the fitted model and evaluation data:

> On the held-out rows, shuffling `wt` changed the fitted model’s RMSE
> by the reported amount, with the displayed variation across
> permutation repeats.

The following requires a different design and evidence:

> Reducing vehicle weight will cause fuel economy to improve by the
> fitted ALE amount in a target population.

AutoXplainR makes the first statement easier to compute and preserve. It
does not turn it into the second.
