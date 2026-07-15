# AutoXplainR 0.2.0

## Beginner-first workflow

- Added a dependency-light `autoxplain(data, target)` path that creates a
  reproducible holdout, fits a simple baseline and an understandable primary
  model, and evaluates both with task-appropriate metrics and definitions.
- Added local linear, logistic, and multinomial logistic workflows for
  regression, binary classification, and multiclass classification. H2O
  remains available explicitly through `engine = "h2o"`.
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

- H2O AutoML is now an optional fitting adapter; core audits use a lightweight
  runtime dependency set.
- Test data remain held out from H2O validation by default.
- Preprocessing is conservative, returns a reusable recipe, and applies fitted
  levels/imputation values to evaluation data. Identifier removal and guessed
  ordinal conversion are opt-in.
- Replaced the generated 2,300-line Flexdashboard implementation with the same
  evidence-report pipeline used by model-agnostic callers.
- Replaced fixed, ranking-first LLM prompts with local deterministic reports and
  an optional aggregated-data-only Gemini narrative.
- Removed bundled third-party datasets whose provenance and redistribution
  terms were not documented.

## Package engineering

- Added CRAN-oriented metadata, automated checks, coverage, pkgdown deployment,
  issue forms, contribution and security policies, citation metadata, and a
  release checklist.
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
