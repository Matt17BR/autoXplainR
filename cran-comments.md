## Release candidate

AutoXplainR 0.3.0, prepared 2026-09-07. The package is currently distributed on
GitHub. This file is a preparation record, not evidence of CRAN acceptance or
submission of this version.

## Local validation

- Ubuntu 26.04.1 LTS, R 4.5.2, all declared optional native model engines installed.
- Complete suite: 1,915 passing expectations, no failures or warnings. Two
  opt-in live tests skipped (H2O and Gemini).
- Separate live H2O suite: 93 passing expectations, no failures or skips.
- Statement coverage: 89.28%; ordinary coverage excludes the live H2O path.
- Lint and spelling checks passed; pkgdown site and both vignettes built.
- Independent numerical oracles and iml PDP agreement passed. A small bootstrap
  simulation found 90% coverage with 20 evaluation rows and 96% with 100 rows
  for nominal 95% intervals (100 replicates per setting). Small-sample limits
  are documented; these results do not establish general coverage guarantees.

## Source archive and remote checks

The release pipeline builds a source archive, checks that exact archive with
`R CMD check --as-cran`, includes PDF/HTML manuals, and verifies SHA-256 before
and after checking. It checks the same archive with R-devel as a second gate.
GitHub's main-branch matrix also covers Windows, macOS, current/older R and R 4.1.
The tagged commit passed the complete matrix. The separate R-devel source
check (2026-09-06 r90498) had zero errors/warnings and one new-submission note.
Actual check results and artifact identifiers are recorded in
`validation/release-0.3.0.md` and linked GitHub Actions runs.

The Win-builder URLs previously recorded for 0.2.0 apply only to that older
archive. They are not evidence for this version. A new CRAN submission still
requires a fresh review of the exact proposed archive and maintainer submission.

## Optional software and scope

The core does not require Java, a browser, network access or credentials during
checks. H2O and hosted narrative integrations are explicitly opt-in. Ordinary
examples use local data and models. Existing modeling and explanation methods
are cited in DESCRIPTION and the installed statistical-methods vignette.

This release integrates fitting, study-aware validation, baseline evaluation,
explanation diagnostics, raw-row prediction and reports. The percentile bootstrap
conditions on fitted models; it does not include selection or refitting
uncertainty. Structured H2O validation and temporal tuning are unsupported.
