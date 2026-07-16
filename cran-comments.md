## Release

- AutoXplainR 0.2.0, prepared 2026-07-16
- Initial CRAN submission

## Test environments

- local Ubuntu 26.04 LTS, R 4.5.2, with `qpdf` and HTML Tidy
- GitHub Actions matrix: Ubuntu (R devel, release, oldrel-1), Windows
  (release), and macOS (release)

## R CMD check results

- The source archive is built once and that exact archive is checked with
  `R CMD check --as-cran`; PDF and HTML manual checks are enabled and its
  SHA-256 digest is verified before and after checking.
- Local results: 0 errors, 0 warnings, and 2 NOTEs. The first NOTE is expected
  for a new submission. The second says that the check machine was unable to
  verify the current time; it is specific to the local environment.
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
