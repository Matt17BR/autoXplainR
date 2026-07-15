# AutoXplainR Roadmap

The 0.2 architecture establishes a model-agnostic evidence audit. The roadmap
below is ordered by scientific leverage, not novelty theatre.

## 0.2 — reliable foundation

- [x] Model-agnostic prediction contract and H2O boundary.
- [x] Repeated grouped permutation importance with retained draws.
- [x] ALE, diagnosed PDP, feature association, and support outputs.
- [x] Supplied near-optimal-model explanation and prediction agreement.
- [x] Evidence-first standalone report with provenance.
- [x] Fast core tests, isolated H2O tests, CI, pkgdown, and project policy.

## 0.3 — validation and estimands

- [ ] Observation-level loss deltas with explicitly supported inferential
  procedures and multiplicity correction; benchmark against `xplainfi`.
- [ ] Conditional-sampler interface rather than treating blocked permutations
  as a general conditional estimator.
- [ ] Bootstrap over observations and refits to separate permutation,
  evaluation-sample, and model-fitting variability.
- [ ] Cross-fitted audit orchestration that prevents training/evaluation leakage
  by construction.
- [ ] Calibration, subgroup performance, and missingness-shift diagnostics as
  context for explanation claims—not as fairness certification.
- [ ] Simulation corpus with known ground truth, correlated features,
  interactions, null predictors, extrapolation, and label noise.

## 0.4 — model multiplicity

- [ ] First-class Rashomon-set provider interface for AutoML, resampling,
  regularization paths, and user-defined candidate generators.
- [ ] Rashomon ALE bands and class-reliance ranges with coverage diagnostics.
- [ ] Case-level prediction ambiguity, feature-attribution disagreement, and
  decision-threshold sensitivity.
- [ ] Pareto selection over performance, calibration, latency, stability, and
  explanation agreement without hiding trade-offs in one weighted score.

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
- At least two non-H2O production adapters validated in integration tests.
- Published methods/validation manuscript and archived benchmark artifacts.
