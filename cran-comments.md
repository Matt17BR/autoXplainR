## Test environments

- local Ubuntu Linux, R 4.5.x
- GitHub Actions: Ubuntu (R devel, release, oldrel-1), Windows (release),
  macOS (release)

## R CMD check results

- Package-owned results: 0 errors, 0 warnings, 1 expected note (new submission).
- The local `--as-cran` run additionally reported that the system executable
  `qpdf` was unavailable. This is a check-machine dependency, not a package
  warning; the source package contains no PDF artifacts.
- Fast test suite: 94 passed, 0 failed, 1 skipped opt-in H2O integration test.
- The isolated H2O integration test also passed locally.

## Optional software

The package core does not require Java, H2O, Plotly, or network access. Optional
H2O integration tests are skipped unless `AUTOXPLAIN_RUN_H2O=true`. Remote API
examples are not executed and ordinary checks never use credentials.

## Submission notes

This is a first CRAN submission. The package contribution is an integrated
diagnostic protocol for the reliability of model explanations, not another
wrapper around a single explanation chart.
