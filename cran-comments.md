## Test environments

- local Ubuntu 26.04 LTS, R 4.5.2
- GitHub Actions matrix: Ubuntu (R devel, release, oldrel-1), Windows
  (release), and macOS (release)

## R CMD check results

- Package-owned results: 0 errors, 0 warnings, and 1 expected NOTE (new
  submission).
- The local check reports one environment WARNING because the external `qpdf`
  executable is unavailable. This is a check-machine dependency rather than a
  package warning; the source package contains no PDF artifacts.
- Dependency-light suite: 1,391 passed expectations, 0 failed, and 30 deliberate
  skips for optional native engines and live-service integrations.
- All-native-engine suite: 1,812 passed expectations, 0 failed, with only the
  opt-in live Gemini and H2O tests skipped.
- The isolated live H2O test passed 35 of 35 assertions across regression,
  binary classification, and multiclass classification.
- Statement coverage is 86.09%.

## Optional software

The package core does not require Java, H2O, Plotly, or network access. Native
adapters for e1071, earth, glmnet, kknn, mgcv, ranger, and xgboost are optional.
H2O integration tests are skipped unless `AUTOXPLAIN_RUN_H2O=true`; hosted
narrative-provider tests are also explicitly opt-in. Remote API examples are not
executed, and ordinary checks never use credentials.

## Submission notes

This is a first CRAN submission. The package contribution is a beginner-first
workflow joining leak-safe tuning across behaviorally diverse model families,
held-out baseline comparison, model-agnostic cross-model explanation,
communication, and a provenance-rich reliability audit. Individual model and
explanation estimators are established methods and are cited in the package
documentation.
