# AutoXplainR Roadmap

The 0.2 architecture establishes a beginner-first fit, evaluate,
explain, and communicate workflow backed by a model-agnostic evidence
audit. The roadmap is ordered by user and scientific leverage, not
novelty theatre.

## 0.2 — reliable foundation

Guided base-R fitting and held-out evaluation with no Java requirement.

Plain-language model report with progressive evidence disclosure.

Explicit Gemini, Groq, Ollama, OpenRouter, and local narrative
providers.

Beginner evaluation diagnostics: error summaries, confusion tables,
probability scores, sample-size cautions, and baseline-failure warnings.

Model-agnostic prediction contract and H2O boundary.

Repeated grouped permutation importance with retained draws.

ALE, diagnosed PDP, feature association, and support outputs.

Supplied near-optimal-model explanation and prediction agreement.

Evidence-first standalone report with provenance.

Fast core tests, isolated H2O tests, enforced coverage, CI, pkgdown, and
project policy.

Version-checked source-package artifacts and draft GitHub releases for
version tags.

## 0.3 — validation and estimands

Observation-level loss deltas with explicitly supported inferential
procedures and multiplicity correction; benchmark against `xplainfi`.

Conditional-sampler interface rather than treating blocked permutations
as a general conditional estimator.

Bootstrap over observations and refits to separate permutation,
evaluation-sample, and model-fitting variability.

Cross-fitted audit orchestration that prevents training/evaluation
leakage by construction.

Calibration, subgroup performance, and missingness-shift diagnostics as
context for explanation claims—not as fairness certification.

Simulation corpus with known ground truth, correlated features,
interactions, null predictors, extrapolation, and label noise.

## 0.4 — model multiplicity

First-class Rashomon-set provider interface for AutoML, resampling,
regularization paths, and user-defined candidate generators.

Rashomon ALE bands and class-reliance ranges with coverage diagnostics.

Case-level prediction ambiguity, feature-attribution disagreement, and
decision-threshold sensitivity.

Pareto selection over performance, calibration, latency, stability, and
explanation agreement without hiding trade-offs in one weighted score.

## 0.5 — reporting and governance

Versioned JSON audit schema and validation package for CI/model registry
gates.

Signed report manifests with model, data, code, and environment hashes.

Redaction policy and pluggable local narrative models.

WCAG audit, print/PDF stylesheet, and report localization.

Reproducible benchmark website comparing runtime and numerical agreement
with DALEX, ingredients, iml, xplainfi, and reference implementations.

## 1.0 criteria

- Stable public object schemas and a documented deprecation policy.
- Zero errors, warnings, or unexplained notes across CRAN reference
  platforms.
- Independent review of estimands, simulations, and reporting language.
- At least two non-H2O production adapters validated in integration
  tests.
- Published methods/validation manuscript and archived benchmark
  artifacts.
