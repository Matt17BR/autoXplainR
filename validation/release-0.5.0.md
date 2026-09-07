# AutoXplainR 0.5.0 release record

This release restores model exploration after the maintainer's rejection of the
0.4.0 report. The [product review](product-review-0.4.0.md) records the source
comparison with 0.1, the observed task answers and revisions made during review.
[Version 0.5.0 was published on GitHub](https://github.com/Matt17BR/autoXplainR/releases/tag/v0.5.0)
on 7 September 2026. The [machine-readable record](results/release-0.5.0.json)
identifies the source, checks and downloaded artifact.

## Product acceptance

The report opens with retained models, held-out scores and measured costs.
Six focused tabs expose model comparison, importance and fitted effects, input
relationships, prediction errors, checks and methods. Background explanations
use hover/focus/tap help and expandable details. The novice and expert journeys in [the product walkthrough](product-walkthrough.md)
are mandatory in the contributor guide and release checklist. The README screenshots are
captures of the actual current implementation.

Task checks use regression, binary, multiclass and quick-mode fixtures. Displayed
scores, importance, curve values, errors, relationship methods and prediction
commands are checked against R-generated answers. Model settings, keyboard-accessible
details, class selection and the detail-view PDF are included. The developer also inspected
desktop and phone screenshots and the selected-feature PDF. Specific failures
and repairs are recorded in the product review; these are implementer acceptance
checks, not a recruited participant study.

## Local validation

- Full R suite: 2,872 passed, zero failures/errors/warnings; two opt-in integration
  skips. The separate live H2O suite passed 95 assertions with no skips or failures.
- Eleven numerical references: maximum absolute discrepancy 5.69e-14, covering
  PDP, ALE shapes, analytic boundaries, ties and empirical centering.
- Browser tasks: all 1,325 assertions passed on four generated reports, including
  320/390/768/1440px layouts and 24 regression-tab and 12 model-details axe scans. No JavaScript errors.
  Incomplete axe results are retained separately and are not counted as passes.
  The wider-font 320px case that reproduced a CI overflow is included for every model.
- Negative controls reject a disconnected model selector, falsified graphics
  and leaked fold imputation for their intended assertion failures, without
  execution errors. The [test-quality review](test-quality-0.5.0.md) records the
  old tests that passed these faults, stronger replacements and 14 removals.
- R lint and spelling, Python compilation, JavaScript syntax and whitespace checks
  pass. Generated documentation is synchronized.

The first source check exposed an unqualified `tail()` call and missing local
qpdf/tidy tools. The function now uses `utils::tail()`, and the complete source
check uses the existing local qpdf/tidy installation. Its clock-network probe is
disabled locally; GitHub source checks retain their normal configuration.

## Published artifact and release checks

Tag `v0.5.0` points to `bcc72250968f47079f98e9c056b6cce208db0253`.
The [release workflow](https://github.com/Matt17BR/autoXplainR/actions/runs/34133288982)
built the source once and checked that archive on R 4.6.1 and R-devel
(2026-09-06 r90498). Both checks reported zero errors, zero warnings and one NOTE
for a new submission. The R release check included PDF and HTML manuals;
R-devel used `--no-manual` and omitted unavailable optional dependencies.
The full [R release log](results/release-0.5.0-r-check.txt) and
[R-devel log](results/release-0.5.0-r-devel-check.txt) preserve those details.

The tagged release also passed 2,872 native-suite assertions, 731 assertions with
exactly the declared minimum engine versions, 95 live H2O assertions and all
1,325 browser assertions. Deliberately broken graphics, model selection and fold
imputation were rejected. Statement coverage was 88.81%; the negative controls
provide separate evidence that these particular tests detect wrong behaviour.
Documentation links, lint, spelling and generated-documentation checks passed.
H2O setup was retried after unusually slow Ubuntu package downloads; no package
checks were waived.

All workflows on the tagged commit passed. The separate package-check matrix
covers macOS and Windows with R release, Ubuntu with R 4.1, oldrel-1, release
and devel, plus a source-archive check. Its macOS job was rerun after a gfortran
download returned HTTP 504 during R setup; the retry passed without source or
test changes. Job links are retained in the machine-readable record.

The downloaded `AutoXplainR_0.5.0.tar.gz` matches the checked workflow artifact
byte for byte and passes its published `SHA256SUMS` file:

```text
b4cecefb6ad0fdf9f3553aa0e937b7340f094cd042d991337797d523d35c7052
```

Installed into a fresh R library and exercised outside the source checkout,
the published archive passed regression, binary and multiclass fit, prediction,
HTML report, saved-result reuse, narrative and JSON export checks. Prediction
with missing values and novel categories also passed using the saved recipe.
The [installed-artifact log](results/release-0.5.0-installed-smoke.txt) records
the result on R 4.5.2.

The public regression, binary and multiclass reports match the reviewed files
byte for byte. All six README screenshots load. Their URLs and checksums are
recorded in the [published-site checks](results/published-site-0.5.0.json).
The live report's model-details dialog was also opened and inspected after
deployment. This release record was completed after publication; the release
tag continues to identify the code that was checked and published.

## Explicit limits

- Default explanations cover up to five models and eight displayed inputs per
  model and outcome class; the audit uses their union. The matrix shows at most 12 selected inputs.
- Temporal tuning remains unsupported; chronological workflows must explicitly
  choose quick/comparison mode. Grouped tuning retains whole units.
- Browser automation uses Chromium. Screen-reader, Firefox and Safari behaviour
  are not established by this record. No comparative participant result is claimed.
- CRAN submission: not applicable, GitHub release only. This is not CRAN approval.
