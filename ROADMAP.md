# AutoXplainR Roadmap

The 0.2 architecture establishes a beginner-first fit, evaluate, explain, and
communicate workflow backed by a model-agnostic evidence audit. The roadmap is
ordered by user and scientific leverage, not novelty theatre.

## 0.2 — reliable foundation

- [x] Guided base-R fitting and held-out evaluation with no Java requirement.
- [x] Plain-language model report with progressive evidence disclosure.
- [x] Explicit Gemini, Groq, Ollama, OpenRouter, and local narrative providers.
- [x] Beginner evaluation diagnostics: error summaries, confusion tables,
  probability scores, sample-size cautions, and baseline-failure warnings.
- [x] Optional local comparison set with multiple understandable model
  complexities, measured resource dimensions, and a labeled Pareto frontier.
- [x] Training-only local hyperparameter tuning through a ten-family registry:
  linear, regularized, additive, tree, forest, boosting, neural, kernel,
  nearest-neighbor, and MARS, with fold-specific preprocessing, inspectable
  one-standard-error selection, and untouched final evaluation.
- [x] Unified serializable prediction adapters, retained family winners,
  paired out-of-fold evidence, and cross-model prediction/importance comparison.
- [x] Model-agnostic prediction contract and H2O boundary.
- [x] Repeated grouped permutation importance with retained draws.
- [x] ALE, diagnosed PDP, feature association, and support outputs.
- [x] Supplied near-optimal-model explanation and prediction agreement.
- [x] Evidence-first standalone report with provenance.
- [x] Fast core tests, isolated H2O tests, enforced coverage, CI, pkgdown, and
  project policy.
- [x] Version-checked source-package artifacts and draft GitHub releases for
  version tags.

## 0.3 — validation and estimands

- [ ] Observation-level loss deltas with explicitly supported inferential
  procedures and multiplicity correction; benchmark against `xplainfi`.
- [ ] Conditional-sampler interface rather than treating blocked permutations
  as a general conditional estimator.
- [ ] Bootstrap over observations and refits to separate permutation,
  evaluation-sample, and model-fitting variability.
- [ ] Cross-fitted audit orchestration that prevents training/evaluation leakage
  by construction.
- [x] Calibration, subgroup performance, and missingness-shift diagnostics as
  context for explanation claims—not as fairness certification.
- [ ] Simulation corpus with known ground truth, correlated features,
  interactions, null predictors, extrapolation, and label noise.

## 0.4 — model multiplicity

- [x] First-class guided candidate-set mode that shares the ordinary result,
  explainer, audit, visualization, and report contracts.
- [ ] First-class Rashomon-set provider interface for AutoML, resampling,
  regularization paths, and user-defined candidate generators.
- [ ] Rashomon ALE bands and class-reliance ranges with coverage diagnostics.
- [x] Binary decision-threshold sensitivity with explicit false-positive and
  false-negative trade-offs and no holdout-optimized recommendation.
- [x] Case-level prediction ambiguity across supplied candidate models, with
  their performance gaps retained beside every disagreement summary.
- [ ] Case-level feature-attribution disagreement.
- [x] Candidate-relative Pareto selection over held-out performance and model
  size without hiding trade-offs in one weighted score.
- [ ] Extend Pareto objectives to calibration, latency, stability, and
  explanation agreement with missing-objective diagnostics.

## 0.5 — reporting and governance

- [ ] Versioned JSON audit schema and validation package for CI/model registry
  gates.
- [ ] Signed report manifests with model, data, code, and environment hashes.
- [ ] Redaction policy and pluggable local narrative models.
- [ ] WCAG audit, print/PDF stylesheet, and report localization.
- [ ] Reproducible benchmark website comparing runtime and numerical agreement
  with DALEX, ingredients, iml, xplainfi, and reference implementations.

## 1.0 criteria

- Stable public object schemas and a documented deprecation policy.
- Zero errors, warnings, or unexplained notes across CRAN reference platforms.
- Independent review of estimands, simulations, and reporting language.
- Every advertised non-H2O adapter validated across its supported tasks in an
  isolated native-engine integration workflow.
- Published methods/validation manuscript and archived benchmark artifacts.
