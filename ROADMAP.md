# Architecture and development plan

This plan distinguishes work included in 0.3.0 from work that needs another
implementation and validation cycle. A feature is not complete merely because
an API or a test file exists. The validation record lists what actually ran.

## Product purpose

AutoXplainR should be the short path from a tabular prediction question to a
reviewable analysis: fit, compare with a baseline, evaluate on an appropriate
partition, inspect fitted behavior, and share the evidence. Beginners should be
able to start with `autoxplain(data, "outcome", report = "report.html")`.
Experienced users should be able to inspect every partition, prediction, loss,
model-selection decision and diagnostic without reverse-engineering an HTML page.

The package should not compete on the number of algorithms or on claims of a
new importance estimator. DALEX, iml, ingredients, modelStudio and modelDown
already address explanations and reporting; tidymodels and mlr3 provide mature
modeling infrastructure; xplainfi addresses importance inference. Our proposed
advantage is the integration and clarity of the workflow. Comparative usability,
runtime and numerical evidence are needed before claiming it does this better.

## Audit of 0.2.0

The repository already included ten learner families, training-only tuning,
ALE/PDP, repeated permutation importance, multiple-model comparisons, calibration,
subgroups, missingness checks, H2O and HTML reports. The baseline local suite
passed 1,391 assertions but skipped 30 tests, primarily optional engines.

Important gaps were still present:

- `autoxplain()` fitted and evaluated models but left explanations to a later
  report call; the name and the result did not describe the same experience.
- There was no public `predict.autoxplain_result()` method to apply the saved
  training recipe to new raw rows.
- Imputation values were learned only where training values were already
  missing, so newly missing evaluation values could fail.
- Explanation validation required every class to occur in the evaluation
  sample even when the training factor declared the full class vocabulary.
- Failed effect calculations were silently omitted.
- Random row splitting and ordinary V-fold IDs were the only native validation
  designs. Grouped and temporal data needed external orchestration.
- Permutation Monte Carlo intervals existed, but evaluation-sample uncertainty
  for the model-versus-baseline comparison did not.
- Long product prose, provider claims and overlapping governance documents
  obscured the first useful call. The provenance document referred to a methods
  vignette that was not present in the source tree.
- Large modules mix orchestration and implementation, and exported legacy
  helpers expand the maintenance surface. Object schemas were implicit.

## Changes in 0.3.0

| Area | Implementation | Acceptance evidence |
|---|---|---|
| One-call analysis | `result_workflow.R` finalizes both engines, records versions, retains explanation evidence, optionally writes HTML | Fitting-only path; retained evidence; byte-identical rerender; explicit recomputation |
| Predictions | `predict.autoxplain_result()` applies the stored recipe and selected model | Regression/binary/multiclass; one/zero rows; omitted targets; serialization; missing/novel values; preserved row positions |
| Preprocessing | Learn imputation values from all training predictors | Complete training followed by incomplete evaluation; fold recipe invariants |
| Evaluation | Respect declared classification levels in sparse evaluation samples | Single-class test data; undefined AUC/macro recall remain unavailable |
| Validation | `validation_split()` with whole-group holdouts and folds; chronological holdouts and gaps | Disjoint units; tied times; gap accounting; shuffled input order; test-label perturbation cannot change training selection |
| Uncertainty | Paired percentile bootstrap for the primary model and baseline; whole-group bootstrap for grouped designs | Independent row/group resampling calculations; retained draws; RNG preservation; temporal rejection |
| Report | Retained evidence reuse, explicit effect failures, validation design, optional uncertainty, print and keyboard styles | Escaping; report content; offline rendering; narrow viewport and print checks |
| Evidence exchange | Aggregate `evidence_summary()` schema 1.0 | Metric agreement; exclusion of raw rows, predictions and group identifiers |
| Methods | Installed statistical-methods vignette and deterministic numerical oracles | Closed-form additive PDP/ALE, null reliance, exact permutation draws, tied-score AUC |
| Repository | Shorter README and product/provider docs, reproducible validation scripts and release records | Examples, package checks, lint, spelling, website and release workflow |

Structured validation is deliberately limited to the base engine. Chronological
tuning is rejected rather than using random folds. Grouped classification fails
when class coverage is infeasible; there is no hidden fallback to row splitting.
The bootstrap conditions on fitted models and does not turn selected-feature
importance into population inference. These limits are part of the API contract.

## Next: validation and estimator interfaces

1. **Rolling-origin resampling.** Replace fold-ID-only orchestration in
   `tuning.R` with explicit analysis/assessment row lists. Add expanding and
   sliding windows, label-availability gaps and skipped-window reasons. Verify
   that preprocessing sees only analysis rows, all assessment times follow the
   analysis window, and lag construction never reaches into the future. Keep
   group and temporal estimands separate.
2. **Nested, cross-fitted evaluation.** Add an outer evaluation-plan object that
   runs the full selection procedure inside each outer analysis partition.
   Retain predictions with original row IDs and prohibit pooling repeat-level
   observations as independent samples. Test the null-target selection-bias
   scenario and compare to tidymodels/rsample on identical partitions.
3. **Conditional importance.** Introduce a sampler interface with `fit()` on
   training data and `sample()` on evaluation rows. Keep marginal, grouped,
   within-stratum and conditional estimands explicitly named. Compare known
   Gaussian conditional samplers against analytic truth and xplainfi; test
   invalid samplers, dependencies and support failures.
4. **Fitting uncertainty.** Add an optional refit bootstrap or repeated outer
   evaluation layer distinct from the fixed-model bootstrap and permutation
   Monte Carlo layer. Use paired fits across models. Measure coverage across
   sample sizes, correlations, signal strengths, imbalance and misspecification.
   Report coverage failures rather than assuming nominal coverage.
5. **Prediction intervals/sets.** Explore split conformal regression and
   classification with a third, independent calibration partition. Test finite
   sample quantiles, marginal coverage, class imbalance, set size and drift.
   Do not use the final evaluation set for calibration or promise conditional
   coverage from an ordinary marginal method.
6. **Model multiplicity.** Define a candidate-set provider interface and coverage
   metadata before adding model reliance ranges or effect envelopes. Separate
   fitted-candidate ranges from statements about an entire Rashomon set.

## Next: internals and performance

- Split `guided_workflow.R` into fit, evaluation and diagnostic modules; split
  `tuning.R` into plans, execution, selection and evidence. Preserve numerical
  equivalence with fixture-based tests before changing behavior.
- Consolidate task/class schemas and prediction normalization across native,
  custom and H2O adapters. Test class reordering, missing probability columns,
  non-finite output and model serialization for every supported task.
- Introduce a result validator and documented schema migration functions before
  promising long-term RDS compatibility. Schema 1.0 for aggregate summaries is
  intentionally narrower than a stable serialized model format.
- Add batched prediction and memory budgets for PDP/permutation workloads; reuse
  baseline predictions. Benchmark prediction-call counts and peak memory on
  wide and tall data. Any parallel backend must preserve deterministic RNG
  streams and propagate failures without dropping records.
- Preserve original row IDs through every recipe operation, including dropped
  rows, and expose input-type and novel-level diagnostics for new predictions.
  Formalize dates, ordered factors and sparse-matrix support before expanding
  supported inputs.
- Keep optional engines optional. Add a tested adapter registration boundary
  instead of adding each future engine as another special case throughout R/.
- Deprecate duplicate dashboard and weighted-efficiency helpers over at least
  two minor releases, with replacement examples and tests of warnings.

## Next: user experience and repository

- Run task-based usability sessions with R beginners and experienced modelers:
  identify the outcome, choose a split, find the positive class, interpret an
  importance interval, and locate a failed diagnostic. Measure completion,
  errors and time against equivalent DALEX/modelStudio and tidymodels workflows.
- Build a small report rendering layer with structured sections and view models
  before expanding the current string-building renderer. Test keyboard order,
  table semantics, contrast, mobile overflow, long names and screen readers.
  Add automated accessibility checks in browser CI; manual checks remain needed.
- Add an annotated example report generated from public synthetic data in CI.
  Never substitute design mockups for output generated by the package.
- Add a JSON Schema, schema fixtures and compatibility tests for registry
  integration. Include provenance hashes only after defining exactly what is
  hashed; a hash is not a signature or proof that data are representative.
- Keep hosted prose opt-in. Test transport contracts with mocks, redact
  credentials, and separate provider availability tests from numerical tests.
  Move provider catalogs and pricing claims out of the main package docs.
- Keep GitHub Actions permissions minimal, bound job runtimes, update actions
  through Dependabot, and attach checked source archives plus checksums to
  releases. Publish only after required checks; do not move version tags.
- Add contribution examples for a bug, new method and learner adapter. Require
  evidence of the statistical target and failure cases rather than test counts
  alone. Keep ownership and maintainer responsibilities explicit.

## Evidence required before a 1.0 claim

A reproducible benchmark corpus must cover regression, binary and multiclass
outcomes; linear and nonlinear effects; interactions; correlated and null
predictors; noise; rare classes; missingness; unseen levels; distribution shift;
clustered data; and temporal dependence. Separate numerical agreement, empirical
coverage, predictive performance, runtime/memory, and user comprehension.

Release criteria are clean package checks on supported R versions and operating
systems, exercised optional adapters, published simulation scripts and results,
resolved accessibility issues, documented compatibility rules, and independent
statistical review. CRAN acceptance and a methods manuscript are separate
milestones; neither is implied by a GitHub release or a passing local test suite.
