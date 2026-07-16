# Verified narrative output examples

This page shows what AutoXplainR narratives actually look like. The
local snapshot is regenerated from the current reproducible tuned-model
analysis; the remote snapshot is a versioned live transport capture
rather than marketing copy written to resemble model output.

## Reproduction record

- **Local snapshot refreshed:** 2026-07-16
- **Remote snapshot source:** commit `cb7ed50` on 2026-07-16
- **Data:** the public `iris` data included with R
- **Task:** multiclass prediction of `Species`
- **Tuning:** eight configurations, three model families, five
  training-only folds, one-standard-error selection
- **Final evaluation:** 36 outer held-out rows that did not participate
  in tuning
- **Explanation input:** 12-repeat permutation importance for the
  selected model
- **Remote disclosure:** aggregate metrics, feature names, warnings,
  tuning provenance, and interpretation boundaries only

The analysis was created with:

``` r

fit <- autoxplain(
  iris,
  "Species",
  model_set = "tuned",
  portfolio = "core",
  max_models = 8,
  nfolds = 5,
  test_fraction = 0.25,
  seed = 2026
)

explainer <- as_explainers(fit, models = "main_model")$main_model
importance <- calculate_permutation_importance(
  explainer,
  n_repeats = 12,
  seed = 2026
)
```

## Deterministic local narrative

This is the default. It makes no network request and returns the same
wording for the same computed context.

``` text
# Model Fit and Evaluation Report

## Scope
This is a descriptive summary of a multiclass task for `Species`, covering 4 model(s) and 4 feature(s).

## Did the model improve on a simple baseline?
The tuned neural network was evaluated on 36 held-out test rows. Its **log_loss** was 0.0844.
That is a 92.3% improvement over the intercept-only baseline (1.0986).

## What the main metric means
**log_loss:** Probability error that penalizes confident wrong answers; lower is better.

## Can the probabilities be taken literally?
The descriptive binned calibration gap was 2.4% across 3 groups. Mean reported probability was 98.3% and observed frequency was 97.2%.
This depends on the held-out sample and grouping; it is not a population guarantee. Read it beside log loss and Brier score.

## Score cautions
- **caution:** Only 36 rows were available for held-out evaluation. Treat the scores as preliminary and validate on more representative unseen rows.

## How automatic tuning selected the model
8 configurations across 3 model families were compared with 5 training-only folds. The selection metric was log_loss.
The one-standard-error (prefer the reviewed family priority, then the least-flexible near-best setting within that family) rule selected the neural network with hidden units = 2, weight decay = 0.03. Its resampled log_loss was 0.09475.
The resampling-selected configuration was `neural_02`; the actual final fitted configuration was `neural_02` (neural network). A recorded refit fallback was not needed.
That resampled score selected a configuration; it is not the final performance estimate. The held-out score above evaluated the selected, refitted model on different rows.

## What kinds of models were retained?
The family descriptions below are prior/model-capacity knowledge. They describe what a family can represent, not patterns proven to have been used by these fitted models.
- `main_model`: neural via nnet; can represent smooth and flexible; interaction capacity: automatic through hidden units.
- `linear_model`: linear via nnet; can represent none unless encoded in features; interaction capacity: none unless specified in features.
- `tree_model`: tree via rpart; can represent stepwise; interaction capacity: automatic along tree paths.

## What did the retained models do differently?
This section is computed evidence from common evaluation rows, not a claim inferred from model-family names.
The largest average paired prediction difference was between `tree_model` and `linear_model`: 0.0887 using total-variation distance between class-probability vectors.
Descriptive comparison of supplied fitted models on common evaluation rows; not causal inference, uncertainty coverage, or a deployment rule.

## Computed feature evidence
The leading descriptive permutation-importance features are Petal.Length, Petal.Width, Sepal.Width. They are computed evidence of model reliance on the evaluation data, not family-capacity claims, and should not be read as causal effects.

## What to do next
Inspect the held-out errors and explanation evidence, check whether the data represent the intended use, and validate the result on new data before relying on it.

## Required limitations
Permutation repeats quantify computation-level variability, not population uncertainty.
Feature explanations do not establish causality, fairness, safety, or deployment readiness.
Use held-out data, domain review, and external validation.

Data disclosure: Aggregated diagnostics only. No raw rows, fitted model objects, case-level predictions, or secrets are included.
```

## Archived live Gemini narrative

The live text below was captured on 2026-07-16 against source commit
`cb7ed50`. It verifies authentication, stateless transport, structured
parsing, and locally appended boundaries. Later tuning and reporting
changes altered the exact computed numbers, so this archived wording
must not be used as a current numerical regression fixture. Run the
reproduction code above and generate a new memo when current values
matter.

This request used the explicit Gemini adapter:

``` r

memo <- generate_natural_language_report(
  fit,
  importance_data = importance,
  provider = "gemini",
  temperature = 0.2,
  max_tokens = 4000,
  fallback = FALSE
)
```

The response passed the package’s five-field JSON-schema validation.
Sections through **Practical next steps** came from Gemini’s validated
fields. The **Required interpretation boundaries** and disclosure were
appended locally by AutoXplainR and therefore cannot be removed by the
provider.

``` text
# Evaluation of Tuned Neural Network for Species Classification

## Held-out performance
On the held-out test set, the reference tuned neural network achieved a final log_loss of 0.08206, which is a 92.5% relative improvement over the simple baseline score of 1.09861. Log_loss is a metric of probability error that penalizes confident wrong answers, where lower values are better. This final held-out performance is distinct from the training-resampled selection score of 0.09566, which was calculated using 5 training-only folds during the automatic tuning process.

## Patterns used by the model
- The model's predictions are descriptively associated with the top features Petal.Length, Petal.Width, and Sepal.Width.
- Probability calibration checks show a binned gap of 0.0232 across 3 groups, where the model's mean reported probability was 98.0% and the observed frequency was 97.2%.

## Cautions
- Only 36 rows were available for the held-out evaluation, meaning the performance metrics may not generalize to broader populations.

## Practical next steps
- Treat the current evaluation scores as preliminary and validate the model on a larger, more representative set of unseen rows.
- Verify that future input data maintains a low rate of missingness, as the current check showed 0 model inputs crossing the 5.0% practical rate-change flag.

## Required interpretation boundaries
- This memo summarizes predictive associations in the supplied evaluation; it does not establish causality.
- Schema validation checks the response format, not whether generated statements are true. Verify every statement against the computed report.
- This analysis is not a fairness, safety, compliance, or deployment certification. Validate it on relevant new data with domain review.

Data disclosure: Aggregated diagnostics only. No raw rows, fitted model objects, case-level predictions, or secrets are included.
```

Recorded provenance:

``` text
provider requested: gemini
provider used:      gemini
model:              gemini-3.5-flash
structured used:    true
fallback used:      false
provider storage:   disabled in the request (`store = false`)
```

Generated wording can change between requests and provider model
updates. The computed metrics, tuning evidence, fixed limitations, and
disclosure remain the authoritative parts of the report. Schema
conformance proves the response shape, not the truth of every generated
sentence.
