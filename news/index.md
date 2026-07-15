# Changelog

## AutoXplainR 0.2.0

### Beginner-first workflow

- Added a dependency-light `autoxplain(data, target)` path that creates
  a reproducible holdout, fits a simple baseline and an understandable
  primary model, and evaluates both with task-appropriate metrics and
  definitions.
- Added local linear, logistic, and multinomial logistic workflows for
  regression, binary classification, and multiclass classification. H2O
  remains available explicitly through `engine = "h2o"`.
- Added
  [`render_model_report()`](https://matt17br.github.io/autoXplainR/reference/render_model_report.md),
  a standalone beginner-first report that leads with the modeling
  question, held-out baseline comparison, and metric definitions before
  revealing feature patterns and a collapsed evidence audit.
- Added evaluation-row predictions, regression error diagnostics,
  classification confusion matrices, Brier scores, held-out probability
  calibration groups, and actionable warnings for sample-size and
  baseline-performance failure modes.
- Added
  [`calibration_diagnostics()`](https://matt17br.github.io/autoXplainR/reference/calibration_diagnostics.md)
  and an adjacent report explanation that compares grouped probabilities
  with observed frequencies while making its sample- and
  grouping-dependence explicit.
- Added
  [`threshold_diagnostics()`](https://matt17br.github.io/autoXplainR/reference/threshold_diagnostics.md)
  and a default binary-report view showing how sensitivity, specificity,
  precision, accuracy, false positives, and false negatives change when
  the 0.5 decision convention moves. The report never presents a cutoff
  optimized on its held-out rows as validated.
- Added opt-in
  [`subgroup_performance()`](https://matt17br.github.io/autoXplainR/reference/subgroup_performance.md)
  and `subgroup =` report support for comparing held-out metrics across
  one explicitly chosen column. Small groups remain visible and flagged,
  and the output rejects fairness-certification language.
- Added
  [`missingness_shift()`](https://matt17br.github.io/autoXplainR/reference/missingness_shift.md)
  using pre-imputation, per-predictor rates retained in preprocessing
  provenance. Guided results flag practical training-versus- evaluation
  differences without presenting them as a statistical test or a general
  drift check.
- The guided workflow now rejects unsupported or non-finite inputs,
  removes and records constant predictors, and reapplies its fitted
  preprocessing recipe to raw evaluation data supplied later.
- Added `model_set = "comparison"`, a dependency-light candidate set
  with the pre-specified statistical model, two decision-tree
  complexities, and the simple baseline. Held-out candidate ranks remain
  descriptive rather than silently replacing the primary model.
- Added candidate-set-relative Pareto analysis through
  [`model_tradeoffs()`](https://matt17br.github.io/autoXplainR/reference/model_tradeoffs.md)
  and rebuilt
  [`plot_model_comparison()`](https://matt17br.github.io/autoXplainR/reference/plot_model_comparison.md)
  around explicit performance-versus-size trade-offs rather than a
  subjective composite score.
- Guided reports now include a plain-language multi-model trade-off
  section and dependency-free Pareto SVG whenever more than two
  candidates were fitted.
- Numerical fit warnings are captured in model diagnostics and
  translated into an actionable report warning instead of being lost or
  printed without context.
- Repositioned the model-agnostic explanation evidence audit as an
  advanced reliability layer behind the beginner workflow.
- Added
  [`explain_model()`](https://matt17br.github.io/autoXplainR/reference/explain_model.md)
  with a validated prediction contract for regression, binary
  classification, and multiclass classification.
- Added
  [`audit_explanations()`](https://matt17br.github.io/autoXplainR/reference/audit_explanations.md)
  to combine repeated-importance stability, feature dependence,
  near-optimal-model importance ranges, prediction agreement,
  prioritized findings, and explicit permitted-claim labels.
- Added
  [`render_explanation_report()`](https://matt17br.github.io/autoXplainR/reference/render_explanation_report.md),
  a standalone accessible HTML artifact with no runtime dependency on
  H2O, Plotly, Flexdashboard, or an LLM.

### Statistical methods

- Rebuilt permutation importance to retain repeat-level values, sign
  stability, grouped-feature permutations, optional blocked
  permutations, and Monte Carlo intervals whose interpretation is stated
  explicitly.
- Added accumulated local effects (ALE) as the default feature-effect
  method.
- Rebuilt PDP estimation with relative support and predictor-dependence
  diagnostics.
- Added supplied-model Rashomon diagnostics: empirical near-optimal
  membership, feature-importance ranges, rank agreement, and prediction
  ambiguity.
- Numeric two-level outcomes are now detected as binary classification
  instead of silently becoming regression targets.

### Architecture and reliability

- H2O AutoML is now an optional fitting adapter; core audits use a
  lightweight runtime dependency set.
- Test data remain held out from H2O validation by default.
- Preprocessing is conservative, returns a reusable recipe, and applies
  fitted levels/imputation values to evaluation data. Identifier removal
  and guessed ordinal conversion are opt-in.
- Added the engine-neutral
  [`preprocess_data()`](https://matt17br.github.io/autoXplainR/reference/preprocess_data.md)
  name;
  [`preprocess_for_h2o()`](https://matt17br.github.io/autoXplainR/reference/preprocess_for_h2o.md)
  is retained as a compatibility alias.
- Replaced the generated 2,300-line Flexdashboard implementation with
  the same evidence-report pipeline used by model-agnostic callers.
- Replaced fixed, ranking-first LLM prompts with local deterministic
  reports and explicit aggregated-data-only Gemini, Groq, Cloudflare
  Workers AI, Ollama, OpenRouter, and custom OpenAI-compatible adapters.
  Local deterministic reporting is always the default, even when an API
  key is present, and provider/model/fallback details are attached as
  provenance.
- Added a dated provider decision guide covering current free access,
  privacy, reproducibility, setup, and structured-output capabilities.
  The Groq default now uses schema-capable `openai/gpt-oss-120b`.
- Schema-capable narrative providers now return a validated five-field
  object that AutoXplainR renders locally with fixed causal, fairness,
  safety, and external-validation boundaries. Invalid JSON falls back
  transparently and structured-output use is retained in provenance.
- Migrated Gemini to its current Interactions API with `store = false`
  and its provider-recommended default temperature. OpenRouter schema
  requests require a compatible routed provider; unsupported adapters
  remain explicitly unstructured.
- Removed bundled third-party datasets whose provenance and
  redistribution terms were not documented.

### Package engineering

- Added CRAN-oriented metadata, automated checks, coverage, pkgdown
  deployment, issue forms, contribution and security policies, citation
  metadata, and a release checklist.
- Enforced an 80% statement-coverage floor while exercising Plotly and
  H2O integration paths; the full local suite currently measures above
  that floor.
- Split fast model-agnostic unit tests from opt-in H2O integration
  tests.
- Rebuilt examples and the vignette around held-out evaluation and
  explicit interpretation boundaries.

## AutoXplainR 0.1.2

- Added a min-max weighted model score.
- Added `performance_weight` to the legacy dashboard.

## AutoXplainR 0.1.1

- Expanded the legacy dashboard and Gemini integration.

## AutoXplainR 0.1.0

- Initial experimental H2O AutoML release.
