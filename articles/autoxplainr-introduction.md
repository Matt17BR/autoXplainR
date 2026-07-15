# Auditing Explanation Evidence with AutoXplainR

## From explanation output to evidence

Permutation importance and feature-effect plots are useful descriptions
of a fitted model. They are also easy to over-interpret. A feature can
appear important because of a particular random permutation, correlated
predictors can make marginal perturbations unrealistic, and a different
but equally accurate model can support a different story.

AutoXplainR treats an explanation as a claim that needs a recorded
evidence contract:

1.  Which fitted model and evaluation distribution are being described?
2.  Does the numerical result survive repetition of its random
    computation?
3.  Are feature-dependence assumptions under pressure?
4.  Do similarly performing supplied models support the same ranking?
5.  What language is justified by those diagnostics?

The resulting grade is a transparent triage heuristic. It is not a
p-value, causal result, fairness assessment, or certification.

## Use held-out data

This small example uses a fixed split only to keep the vignette
reproducible. A real analysis should design its evaluation split before
fitting and retain enough observations for stable diagnostics.

``` r

train <- mtcars[1:24, ]
test <- mtcars[25:32, ]

model_a <- lm(mpg ~ wt + hp + disp, data = train)
model_b <- lm(mpg ~ wt + hp + qsec, data = train)
```

[`explain_model()`](https://matt17br.github.io/autoXplainR/reference/explain_model.md)
validates how each model produces predictions. The explainer stores
feature data, outcomes, task metadata, and a lightweight provenance ID.

``` r

explainer_a <- explain_model(
  model_a,
  test,
  y = "mpg",
  label = "displacement model",
  metadata = list(evaluation_role = "test")
)

explainer_b <- explain_model(
  model_b,
  test,
  y = "mpg",
  label = "acceleration model",
  metadata = list(evaluation_role = "test")
)

explainer_a
#> <AutoXplainR explainer>
#>   model:    displacement model
#>   task:     regression
#>   data:     8 rows x 10 features
#>   id:       axr-0c9b3882
```

For an unsupported model class, pass `predict_function`. Regression
functions return numeric predictions; binary functions return the
positive-class probability; multiclass functions return a named
probability matrix.

## Inspect repeated permutation importance

``` r

importance <- calculate_permutation_importance(
  explainer_a,
  features = c("wt", "hp", "disp"),
  n_repeats = 10,
  seed = 2026
)

importance
#> <AutoXplainR permutation importance>
#>   metric: rmse | repeats: 10 | baseline: 2.5436
#>  feature importance  std_error   conf_low  conf_high sign_stability
#>       hp  2.5556753 0.22940656  2.0367217 3.07462904            1.0
#>       wt  0.9892010 0.21483320  0.5032146 1.47518747            0.9
#>     disp -0.1262717 0.05856004 -0.2587437 0.00620031            0.7
#>   intervals describe permutation Monte Carlo variation, not population inference
```

The result deliberately retains negative values. Its interval is a Monte
Carlo interval across permutation repeats. Repeating an algorithm does
not create new independent observations from a population, so this is
not population-level inference.

Feature groups can be permuted jointly:

``` r

calculate_permutation_importance(
  explainer_a,
  feature_groups = list(engine = c("hp", "disp"), weight = "wt"),
  n_repeats = 5,
  seed = 2026
)
#> <AutoXplainR permutation importance>
#>   metric: rmse | repeats: 5 | baseline: 2.5436
#>  feature importance std_error  conf_low conf_high sign_stability
#>   engine   2.872314 0.5134143 1.4468471   4.29778              1
#>   weight   1.382660 0.1783360 0.8875198   1.87780              1
#>   intervals describe permutation Monte Carlo variation, not population inference
```

The optional `within` argument permutes within explicit strata. This is
useful as a blocked sensitivity analysis, but it is not advertised as a
general conditional permutation estimator.

## Prefer ALE for dependent predictors

Partial dependence replaces one feature while leaving all others
unchanged. With dependent predictors, that can ask the model to score
combinations absent from the reference data. Accumulated local effects
(ALE) instead average local prediction differences within observed
feature bins (Apley and Zhu, 2020).

``` r

ale <- explain_effect(explainer_a, feature = "wt")
pdp <- explain_effect(explainer_a, feature = "wt", method = "pdp")

ale
#> <AutoXplainR ALE effect>
#>   feature: wt | rows: 8 | max association: 0.929
#>        wt accumulated_effect std_error   conf_low  conf_high n support
#>  1.586850          1.1613799         0  1.1613799  1.1613799 1       1
#>  1.734550          1.1613799         0  1.1613799  1.1613799 0       0
#>  1.876825          0.7780811         0  0.7780811  0.7780811 1       1
#>  1.981125          0.7780811         0  0.7780811  0.7780811 0       0
#>  2.052875          0.7780811         0  0.7780811  0.7780811 0       0
#>  2.145875          0.4580819         0  0.4580819  0.4580819 1       1
#>  2.313250          0.4580819         0  0.4580819  0.4580819 0       0
#>  2.533750          0.4580819         0  0.4580819  0.4580819 0       0
#>  2.707750          0.1009713         0  0.1009713  0.1009713 1       1
#>  2.773250          0.1009713         0  0.1009713  0.1009713 0       0
#>  2.776750          0.1009713         0  0.1009713  0.1009713 0       0
#>  2.818250         -0.1216977         0 -0.1216977 -0.1216977 1       1
#>  2.926250         -0.1216977         0 -0.1216977 -0.1216977 0       0
#>  3.062750         -0.1216977         0 -0.1216977 -0.1216977 0       0
#>  3.200500         -0.5110183         0 -0.5110183 -0.5110183 1       1
#>  3.340000         -0.5110183         0 -0.5110183 -0.5110183 0       0
#>  3.480000         -0.5110183         0 -0.5110183 -0.5110183 0       0
#>  3.601250         -0.7981073         0 -0.7981073 -0.7981073 1       1
#>  3.700625         -0.7981073         0 -0.7981073 -0.7981073 0       0
#>  3.796875         -1.0676908         0 -1.0676908 -1.0676908 1       1
attr(pdp, "max_association")
#> [1] 0.9285714
attr(pdp, "dependence_warning")
#> [1] TRUE
```

Both effect outputs include relative empirical support. Their
uncertainty columns remain descriptive computation diagnostics.

## Audit several plausible models

``` r

audit <- audit_explanations(
  list(explainer_a, explainer_b),
  features = c("wt", "hp", "disp", "qsec"),
  n_repeats = 10,
  performance_tolerance = 0.10,
  dependence_threshold = 0.70,
  seed = 2026
)

audit
#> <AutoXplainR explanation evidence audit>
#>   grade:              D (diagnostic, not certification)
#>   models:             2 (2 near-optimal)
#>   stable claims:      0.0%
#>   max dependence:     0.929
#>   explanation accord: 0.6
#>   prediction accord:  1
#> 
#> Findings
#>   [warning] 4 feature(s) exceed the dependence threshold.
#>   [warning] 8 model-feature claim(s) are qualified or unsupported.
#>   [critical] Near-optimal models disagree on the feature-importance ranking.
#>   [note] The permutation budget is suitable for smoke testing, not a final report.
```

The near-optimal indicator is relative to the models supplied to this
call. It does not claim to enumerate every near-optimal model in a
hypothesis class. If near-optimal supplied models disagree, the audit
recommends reporting an importance range rather than a single-model
ranking.

The main components are ordinary data structures:

``` r

audit$performance
#>                                 model    score metric near_optimal relative_gap
#> displacement model displacement model 2.543600   rmse         TRUE   0.05433181
#> acceleration model acceleration model 2.412523   rmse         TRUE   0.00000000
audit$dependence
#>   feature max_association associated_feature high_dependence
#> 1      wt       0.9285714               disp            TRUE
#> 2      hp       0.8024096               disp            TRUE
#> 3    disp       0.9285714                 wt            TRUE
#> 4    qsec       0.7784571                 hp            TRUE
audit$explanation_agreement$importance_ranges
#>      feature min_importance max_importance mean_importance
#> wt        wt     0.98920102       2.053720      1.52146039
#> hp        hp     1.70166680       2.555675      2.12867108
#> disp    disp    -0.12627171       0.000000     -0.06313585
#> qsec    qsec    -0.09100309       0.000000     -0.04550155
audit$findings
#>   severity                        code
#> 1  warning          feature_dependence
#> 2  warning limited_importance_evidence
#> 3 critical       rashomon_disagreement
#> 4     note      low_monte_carlo_budget
#>                                                                     message
#> 1                             4 feature(s) exceed the dependence threshold.
#> 2                    8 model-feature claim(s) are qualified or unsupported.
#> 3           Near-optimal models disagree on the feature-importance ranking.
#> 4 The permutation budget is suitable for smoke testing, not a final report.
#>                                                                                          evidence
#> 1                                                          Maximum observed association is 0.929.
#> 2 4 interval(s) include zero; 2 have sign stability below 0.8; 8 exceed the dependence threshold.
#> 3                                                                    Mean Spearman agreement: 0.6
#> 4                                                              10 repeats per model-feature pair.
#>                                                                                                                                recommendation
#> 1                          Prefer ALE for effects; treat marginal PFI as model reliance, and use a conditional method for conditional claims.
#> 2 Inspect repeat distributions and dependence, increase evaluation data when uncertainty is material, and avoid ranking unsupported features.
#> 3                                              Report model-class importance ranges instead of presenting one best model's ranking as unique.
#> 4                                                             Use at least 20 repeats for routine work and more when feature ranks are close.
```

Evidence grades control the wording that the report permits:

- **A:** stable marginal evidence under the configured diagnostics;
- **B:** usable with the stated uncertainty;
- **C:** sensitivity finding only;
- **D:** do not make a feature claim.

These cutoffs are visible in the source and should be configured and
justified for the application. A grade cannot turn an associational
explanation into a causal one.

## Preserve the review artifact

``` r

path <- tempfile(fileext = ".html")
render_explanation_report(audit, path)
file.exists(path)
#> [1] TRUE
unlink(path)
```

The report is a standalone HTML file with findings, action items, model
and feature tables, interpretation boundaries, configuration,
timestamps, and explainer IDs. Store it with the model version,
evaluation-data version, and analysis code.

## Optional H2O AutoML adapter

The explanation core does not require H2O or Java. If H2O is useful for
fitting a candidate set, keep it at the boundary:

``` r

result <- autoxplain(
  training_data,
  target_column = "outcome",
  test_data = held_out_data,
  max_models = 10,
  max_runtime_secs = 300,
  seed = 2026
)

explainers <- as_explainers(result)
audit <- audit_explanations(explainers)
generate_dashboard(result, "automl-evidence.html")
```

Held-out test data are not passed to H2O as a validation frame by
default. Numeric two-level outcomes are classified correctly, and
training preprocessing is applied to evaluation data through a fitted
recipe.

## Optional narratives

``` r

memo <- generate_natural_language_report(audit, use_remote = FALSE)
cat(substr(memo, 1, 300), "...")
#> # Explanation Evidence Report
#> 
#> ## Scope
#> This is a descriptive summary of a unspecified task for `unspecified`, covering 2 model(s) and 4 feature(s).
#> 
#> ## Reliability first
#> The heuristic explanation-evidence grade is **D**; this is a diagnostic, not a certification.
#> The stable-claim rate is 0.0%.
#> 
#> ##  ...
```

The local narrative is deterministic. Remote Gemini generation is an
explicit optional boundary and receives only aggregated diagnostics. The
prompt forbids causal, safety, fairness, and compliance claims, but
generated text still requires human review.

## Literature context

Permutation-based model reliance and model-class reliance are discussed
by Fisher, Rudin, and Dominici (2019). ALE and the extrapolation problem
of PDPs under dependent features are developed by Apley and Zhu (2020).
AutoXplainR operationalizes these concerns as a review workflow; it does
not claim a new formal inferential estimator.
