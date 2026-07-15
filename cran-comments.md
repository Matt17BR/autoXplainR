## Test environments

- local Ubuntu Linux, R 4.5.x
- GitHub Actions: Ubuntu (R devel, release, oldrel-1), Windows (release),
  macOS (release)

## R CMD check results

- Package-owned results: 0 errors, 0 warnings, 1 expected note (new submission).
- The local check reports one environment warning because the system executable
  `qpdf` is unavailable. This is a check-machine dependency, not a package
  warning; the source package contains no PDF artifacts.
- Fast test suite: 262 expectations passed, 0 failed, 1 skipped opt-in H2O
  integration test.
- The isolated real H2O integration test passed locally. Statement coverage is
  83.33% for the dependency-light suite and 87.50% with H2O enabled.

## Optional software

The package core does not require Java, H2O, Plotly, or network access. Optional
H2O integration tests are skipped unless `AUTOXPLAIN_RUN_H2O=true`. Remote API
examples are not executed and ordinary checks never use credentials.

## Submission notes

This is a first CRAN submission. The package contribution is a beginner-first
workflow joining safe local fitting, held-out baseline comparison, explanation,
communication, and an advanced reliability audit. Individual explanation
estimators are established methods and are cited in the package documentation.
