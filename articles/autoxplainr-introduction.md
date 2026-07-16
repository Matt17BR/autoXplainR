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
#>   rank        model_id                   model     role   family backend
#> 1    1      main_model       linear regression  primary   linear   stats
#> 2    2 simple_baseline intercept-only baseline baseline baseline   stats
#>       rmse      mae  r_squared training_time_ms model_size_kb complexity
#> 1 3.552918 2.797996 -0.1281933                1      44.43750         11
#> 2 5.096266 3.907692 -1.3212249                2      19.82812          1
#>   fit_warning prediction_time_ms
#> 1                              1
#> 2                              1
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

## Tune supervised models without opening the final holdout

When model selection is the goal, use the explicit tuned workflow:

``` r

tuned <- autoxplain(
  iris,
  target_column = "Species",
  model_set = "tuned",
  portfolio = "core",
  max_models = 6,
  nfolds = 3,
  seed = 2026
)

tuning <- tuning_results(tuned)
tuning$candidates[, c(
  "model", "hyperparameters", "cv_score", "cv_se", "selected"
)]
#>                             model
#> 1                  neural network
#> 2                  neural network
#> 3 multinomial logistic regression
#> 4                   decision tree
#> 5                   decision tree
#> 6                   decision tree
#>                                         hyperparameters   cv_score      cv_se
#> 1                 hidden units = 2, weight decay = 0.03 0.09446072 0.03629471
#> 2                  hidden units = 1, weight decay = 0.1 0.29339319 0.01038619
#> 3                               default statistical fit 0.37178519 0.20404352
#> 4 max depth = 6, pruning cp = 0.003, minimum split = 10 2.37836688 1.39532699
#> 5  max depth = 2, pruning cp = 0.03, minimum split = 24 2.39466788 1.39241141
#> 6  max depth = 4, pruning cp = 0.01, minimum split = 14 2.39466788 1.39241141
#>   selected
#> 1     TRUE
#> 2    FALSE
#> 3    FALSE
#> 4    FALSE
#> 5    FALSE
#> 6    FALSE
```

This executable vignette uses the dependency-light `core` portfolio: a
statistical reference, decision-tree pruning/depth settings, and scaled
neural-network hidden-unit/weight-decay settings. It learns
preprocessing separately inside each training fold.

For a real model search, the default `recommended` portfolio is broader.
It compares linear, regularized, additive (where supported),
decision-tree, random-forest, and gradient-boosted-tree behavior.
Installation is explicit so the tournament never changes silently with
the packages present on one machine:

``` r

install_model_engines("recommended")

recommended <- autoxplain(
  my_data,
  target_column = "outcome",
  model_set = "tuned",
  portfolio = "recommended", # the default for tuned fits
  nfolds = 5,
  seed = 2026
)

learner_catalog()
compare_model_behavior(recommended)
```

The `extended` portfolio additionally covers neural, radial-kernel,
nearest-neighbor, and multivariate adaptive regression spline behavior
when the task supports them. Advanced users can supply an explicit
`learners` vector and change `max_models`, `nfolds`, and `tuning_rule`.

The default one-standard-error rule first uses the documented family
priority, then chooses the least-flexible eligible setting inside that
family. Its family-specific flexibility proxies are never compared as if
they shared one unit. Use `tuning_rule = "best"` when the explicitly
desired rule is minimum resampled error instead. Both rules and every
fold score remain in the result.

The resampled score answers *which configuration should be refitted?*
The held-out score answers *how did that selected, refitted model
perform on unseen rows?* AutoXplainR keeps the outer evaluation rows out
of tuning and labels the two numbers separately in the report.

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

The same comparison can reveal rows whose prediction depends strongly on
model specification:

``` r

ambiguity <- prediction_ambiguity(comparison)
head(ambiguity$rows)
#>   evaluation_row row_id observed prediction_min prediction_max prediction_range
#> 1              1    121      6.9       6.302632       6.854533        0.5519014
#> 2              2     38      4.9       5.042424       5.150000        0.1075758
#> 3              3     45      5.1       5.042424       5.569338        0.5269134
#> 4              4    111      6.5       6.302632       6.655556        0.3529240
#> 5              5     91      5.5       5.939797       6.500000        0.5602030
#> 6              6    108      7.3       7.143343       7.700000        0.5566568
#>   prediction_sd
#> 1    0.27950628
#> 2    0.05421837
#> 3    0.27157039
#> 4    0.18215693
#> 5    0.28414510
#> 6    0.28013427
```

Regression rows report the range of supplied candidate predictions.
Binary and multiclass rows report hard-class disagreement and
probability distance. This is not uncertainty coverage: the supplied
models can perform very differently, and their held-out scores must
remain beside the disagreement summary.

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
#> $structured_requested
#> [1] TRUE
#> 
#> $structured_used
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

# Or use the Groq or Cloudflare free allowance
memo <- generate_natural_language_report(result, provider = "groq")
memo <- generate_natural_language_report(result, provider = "cloudflare")

# Or use a model running locally through Ollama
memo <- generate_natural_language_report(result, provider = "ollama")
```

Only aggregated diagnostics are placed in the prompt. Raw rows, fitted
objects, case-level predictions, and secrets are excluded. Provider and
model details are attached to the returned text so the prose can be
audited. Generated prose still needs human review against the numerical
report.

Schema-capable providers return five validated fields by default.
AutoXplainR renders them locally and inserts fixed interpretation
boundaries that the model cannot omit. The schema constrains format, not
truth; malformed output falls back to the deterministic report. Gemini
uses its stateless Interactions API with provider storage disabled for
this one-shot request.

See the repository’s narrative provider guide for the dated comparison
of free access, structured-output support, privacy, and reproducibility.
Provider plans and model catalogs can change independently of
AutoXplainR.

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
#>                                                                                                                                                     log_loss 
#>                                                                                 "Probability error that penalizes confident wrong answers; lower is better." 
#>                                                                                                                                                  brier_score 
#>                                                                                                        "Average squared probability error; lower is better." 
#>                                                                                                                                            calibration_error 
#> "Average absolute gap between grouped probabilities and observed frequencies; lower is better, but the value depends on the evaluation sample and grouping." 
#>                                                                                                                                                     accuracy 
#>                                                                                    "Share of held-out rows assigned to the correct class; higher is better." 
#>                                                                                                                                                 macro_recall 
#>                                                                              "Recall calculated for each class and then averaged equally; higher is better."
```

Classification splits are stratified. Binary tasks report log loss,
Brier score, a binned calibration gap, accuracy, balanced accuracy, and
ROC AUC. Multiclass tasks report log loss, Brier score, predicted-class
calibration, accuracy, and macro recall. Log loss is primary because it
evaluates probability quality and penalizes confident wrong predictions.

Calibration asks a more literal question: when the model reports a
probability near 70%, does the corresponding event happen about 70% of
the time on the held-out rows? The report explains this visually, and
the grouped values remain available directly:

``` r

calibration_diagnostics(flowers)
#> <AutoXplainR probability calibration>
#>   model:      main_model
#>   check:      confidence in the predicted class
#>   rows:       30 in 1 probability groups
#>   average:    1 predicted vs 0.967 observed
#>   binned gap: 0.033 (lower is better)
#>   caution:    sample- and grouping-dependent; not a population guarantee
```

The binned gap is descriptive. It changes with the evaluation sample and
the grouping, so it should be read beside log loss and Brier score
rather than as a population guarantee.

For a binary task, the report also makes the 0.5 decision convention
visible. Use
[`threshold_diagnostics()`](https://matt17br.github.io/autoXplainR/reference/threshold_diagnostics.md)
to compare a deliberate set of cutoffs:

``` r

binary_cars <- transform(
  mtcars,
  am = factor(am, labels = c("automatic", "manual"))
)
binary_fit <- autoxplain(binary_cars, "am", seed = 2026)
threshold_diagnostics(binary_fit, thresholds = c(0.3, 0.5, 0.7))
```

Lower thresholds usually find more positive cases while creating more
false positives; higher thresholds usually do the reverse. The relative
consequences belong to the application. A threshold chosen on these
held-out rows must be evaluated again on different representative data.

## Check an explicitly chosen group

Aggregate scores can hide weak spots. If the evaluation data contain a
group that matters to the intended use, ask for the comparison directly:

``` r

cars_with_group <- transform(
  mtcars,
  transmission = factor(am, labels = c("automatic", "manual"))
)
group_fit <- autoxplain(cars_with_group, "mpg", test_fraction = 0.4, seed = 2026)
subgroup_performance(group_fit, by = "transmission", min_rows = 3)
#> <AutoXplainR subgroup performance>
#>   model:       main_model
#>   compared by: transmission (2 groups)
#>   metric:      rmse (lower is better)
#>   largest gap: 0.9029
#>   caution:     descriptive holdout check; not fairness certification
```

The same check can be included in the standalone report with
`subgroup = "transmission"`. AutoXplainR does not guess which column is
sensitive, and it does not call this a fairness assessment. Observed
gaps can come from small samples, different case mix, measurement
quality, or model behavior and require domain-specific investigation.

## Check whether missingness changed

The guided workflow retains per-column missing-value rates before
imputation. This matters because a field that is absent much more often
in evaluation or future use can change what the model is effectively
being asked to do:

``` r

training <- data.frame(x = c(NA, 2:40), y = 1:40)
evaluation <- data.frame(x = c(rep(NA, 5), 46:60), y = 41:60)
missing_fit <- autoxplain(training, "y", test_data = evaluation)
missingness_shift(missing_fit)
#> <AutoXplainR missingness shift>
#>   data:        40 training + 20 evaluation rows
#>   predictors:  1 with any missing values
#>   flagged:     1 model inputs at 5 percentage points
#>   caution:     practical flag; not a statistical test or general drift check
```

The default five-percentage-point flag is an investigation prompt, not a
hypothesis test. The diagnostic does not claim that the complete
predictor distribution is stable, and it reports the rates before the
training-derived imputation was applied.

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
