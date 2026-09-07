# AutoXplainR 0.5.0 release record

This release restores model exploration after the maintainer's rejection of the
0.4.0 report. The [product review](product-review-0.4.0.md) records the source
comparison with 0.1, the observed task answers and revisions made during review.
Publication status and artifact identity are recorded below as gates complete.

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

- Full R suite: 2,886 passed, zero failures/errors/warnings; two opt-in integration
  skips. The separate live H2O suite passed 95 assertions with no skips or failures.
- Eleven numerical references: maximum absolute discrepancy 5.69e-14, covering
  PDP, ALE shapes, analytic boundaries, ties and empirical centering.
- Browser tasks: all 1,164 assertions passed on four generated reports, including
  320/390/768/1440px layouts and 24 regression-tab and 12 model-details axe scans. No JavaScript errors.
  Incomplete axe results are retained separately and are not counted as passes.
  The wider-font 320px case that reproduced a CI overflow is included for every model.
- Negative control: deliberately disconnecting model selection causes a failed
  model-switch assertion and exit status 1, without an execution error.
- R lint and spelling, Python compilation, JavaScript syntax and whitespace checks
  pass. Generated documentation is synchronized.

The first source check exposed an unqualified `tail()` call and missing local
qpdf/tidy tools. The function now uses `utils::tail()`, and the complete source
check uses the existing local qpdf/tidy installation. Its clock-network probe is
disabled locally; GitHub source checks retain their normal configuration.

## Release gates

The local source archive passed `R CMD check --as-cran` on R 4.5.2 with
zero errors, zero warnings and one NOTE (new submission), including both manuals.
GitHub platform jobs and publication are still being executed. A passing older release does not satisfy these gates. The final record
will identify the tagged commit, downloaded archive checksum and fresh-library
fit/predict/report check.

## Explicit limits

- Default explanations cover up to five models and eight displayed inputs per
  model and outcome class; the audit uses their union. The matrix shows at most 12 selected inputs.
- Temporal tuning remains unsupported; chronological workflows must explicitly
  choose quick/comparison mode. Grouped tuning retains whole units.
- Browser automation uses Chromium. Screen-reader, Firefox and Safari behaviour
  are not established by this record. No comparative participant result is claimed.
- CRAN submission: not applicable, GitHub release only. This is not CRAN approval.
