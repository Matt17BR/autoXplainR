# AutoXplainR 0.2.0

## Beginner-first workflow

- Added a dependency-light `autoxplain(data, target)` path that creates a
  reproducible holdout, fits a simple baseline and an understandable primary
  model, and evaluates both with task-appropriate metrics and definitions.
- Added local linear, logistic, and multinomial logistic workflows for
  regression, binary classification, and multiclass classification. H2O
  remains available explicitly through `engine = "h2o"`.
- Added `model_set = "tuned"`, `tuning_results()`, and a ten-family learner
  registry. The recommended portfolio compares linear, regularized, additive,
  tree, forest, and boosting models; extended mode adds neural, radial-kernel,
  nearest-neighbor, and MARS models. Every backend shares one serializable
  prediction contract and is tuned with fold-specific preprocessing inside the
  outer training set.
- Added portfolio-aware automatic search budgets, space-filling low-budget
  grids, stable per-configuration seeds, explicit dependency/version status,
  retained family winners, paired out-of-fold predictions, and deterministic
  fallback when a full-data refit fails. Explicit model budgets are not silently
  capped.
- The one-standard-error rule now uses a reviewed family priority followed by a
  family-specific flexibility proxy; raw proxy values are never compared across
  unrelated model families. The final holdout remains untouched until selection
  and full-training refit are complete.
- Added `compare_model_behavior()` to separate reviewed model-capacity cards
  from computed same-row prediction disagreement and optional permutation-
  importance evidence across retained families.
- Added `render_model_report()`, a standalone beginner-first report that leads
  with the modeling question, held-out baseline comparison, and metric
  definitions before revealing feature patterns and a collapsed evidence audit.
- Added evaluation-row predictions, regression error diagnostics,
  classification confusion matrices, Brier scores, held-out probability
  calibration groups, and actionable warnings for sample-size and
  baseline-performance failure modes.
- Added `calibration_diagnostics()` and an adjacent report explanation that
  compares grouped probabilities with observed frequencies while making its
  sample- and grouping-dependence explicit.
- Added `threshold_diagnostics()` and a default binary-report view showing how
  sensitivity, specificity, precision, accuracy, false positives, and false
  negatives change when the 0.5 decision convention moves. The report never
  presents a cutoff optimized on its held-out rows as validated.
- Added opt-in `subgroup_performance()` and `subgroup =` report support for
  comparing held-out metrics across one explicitly chosen column. Small groups
  remain visible and flagged, and the output rejects fairness-certification
  language.
- Added `missingness_shift()` using pre-imputation, per-predictor rates retained
  in preprocessing provenance. Guided results flag practical training-versus-
  evaluation differences without presenting them as a statistical test or a
  general drift check.
- The guided workflow now rejects unsupported or non-finite inputs, removes and
  records constant predictors, and reapplies its fitted preprocessing recipe to
  raw evaluation data supplied later.
- Added `model_set = "comparison"`, a dependency-light candidate set with the
  pre-specified statistical model, two decision-tree complexities, and the
  simple baseline. Held-out candidate ranks remain descriptive rather than
  silently replacing the primary model.
- Added candidate-set-relative Pareto analysis through `model_tradeoffs()` and
  rebuilt `plot_model_comparison()` around explicit performance-versus-size
  trade-offs rather than a subjective composite score.
- Guided reports now include a plain-language multi-model trade-off section and
  dependency-free Pareto SVG whenever more than two candidates were fitted.
- Added `prediction_ambiguity()` and a comparison-report section that finds
  held-out rows where supplied non-baseline candidates predict different values,
  classes, or probability distributions. Candidate performance remains beside
  disagreement so weak-model variation is not mislabeled as uncertainty.
- Numerical fit warnings are captured in model diagnostics and translated into
  an actionable report warning instead of being lost or printed without context.
- Repositioned the model-agnostic explanation evidence audit as an advanced
  reliability layer behind the beginner workflow.
- Added `explain_model()` with a validated prediction contract for regression,
  binary classification, and multiclass classification.
- Added `audit_explanations()` to combine repeated-importance stability,
  feature dependence, near-optimal-model importance ranges, prediction
  agreement, prioritized findings, and explicit permitted-claim labels.
- Added `render_explanation_report()`, a standalone accessible HTML artifact
  with no runtime dependency on H2O, Plotly, Flexdashboard, or an LLM.

## Statistical methods

- Rebuilt permutation importance to retain repeat-level values, sign stability,
  grouped-feature permutations, optional blocked permutations, and Monte Carlo
  intervals whose interpretation is stated explicitly.
- Added accumulated local effects (ALE) as the default feature-effect method.
- Rebuilt PDP estimation with relative support and predictor-dependence
  diagnostics.
- Added supplied-model Rashomon diagnostics: empirical near-optimal membership,
  feature-importance ranges, rank agreement, and prediction ambiguity.
- Numeric two-level outcomes are now detected as binary classification instead
  of silently becoming regression targets.

## Architecture and reliability

- Replaced the hard-coded three-column tuning dispatcher with a learner-family
  registry and generic list-column search plan. `learner_catalog()` now exposes
  the distinction between a model family, its backend, supported tasks, and a
  reviewed plain-language behavior card.
- Tuning seeds are now derived per configuration, so adding or reordering a
  learner cannot silently change the random initialization of another one.
- Held-out and fold-validation rows are never moved into training to repair
  categorical levels. Training recipes instead learn a modal fallback for
  novel predictor levels, record mapping counts, and retain a strict
  `novel_level_strategy = "error"` option.
- H2O AutoML is now an optional fitting adapter; core audits use a lightweight
  runtime dependency set.
- Test data remain held out from H2O validation by default.
- Preprocessing is conservative, returns a reusable recipe, and applies fitted
  levels/imputation values to evaluation data. Identifier removal and guessed
  ordinal conversion are opt-in.
- Added the engine-neutral `preprocess_data()` name; `preprocess_for_h2o()` is
  retained as a compatibility alias.
- Replaced the generated 2,300-line Flexdashboard implementation with the same
  evidence-report pipeline used by model-agnostic callers.
- Replaced fixed, ranking-first LLM prompts with local deterministic reports and
  explicit aggregated-data-only Gemini, Groq, Cloudflare Workers AI, Ollama,
  OpenRouter, and custom OpenAI-compatible adapters. Local deterministic
  reporting is always the default, even when an API key is present, and
  provider/model/fallback details are attached as provenance.
- Added a dated provider decision guide covering current free access, privacy,
  reproducibility, setup, and structured-output capabilities. The Groq default
  now uses schema-capable `openai/gpt-oss-120b`.
- Schema-capable narrative providers now return a validated five-field object
  that AutoXplainR renders locally with fixed causal, fairness, safety, and
  external-validation boundaries. Invalid JSON falls back transparently and
  structured-output use is retained in provenance.
- Migrated Gemini to its current Interactions API with `store = false` and its
  provider-recommended default temperature. OpenRouter schema requests require
  a compatible routed provider; unsupported adapters remain explicitly
  unstructured.
- Added complete, reproducible local and live Gemini output snapshots. A live
  Gemini check exposed reasoning-token exhaustion at the previous response
  budget, so the default is now 4,000 tokens and incomplete interactions return
  targeted recovery guidance.
- Removed bundled third-party datasets whose provenance and redistribution
  terms were not documented.

## Package engineering

- Added CRAN-oriented metadata, automated checks, coverage, pkgdown deployment,
  issue forms, contribution and security policies, citation metadata, and a
  release checklist.
- Enforced an 80% statement-coverage floor while exercising Plotly and H2O
  integration paths; the full local suite currently measures above that floor.
- Split fast model-agnostic unit tests from opt-in H2O integration tests.
- Rebuilt examples and the vignette around held-out evaluation and explicit
  interpretation boundaries.

# AutoXplainR 0.1.2

- Added a min-max weighted model score.
- Added `performance_weight` to the legacy dashboard.

# AutoXplainR 0.1.1

- Expanded the legacy dashboard and Gemini integration.

# AutoXplainR 0.1.0

- Initial experimental H2O AutoML release.
