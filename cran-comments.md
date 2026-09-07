## Release candidate

AutoXplainR 0.4.0, prepared 2026-09-07. The package is distributed on GitHub.
This is a preparation record, not evidence of CRAN submission or acceptance.
The next CRAN submission requires maintainer review of the exact archive.

## Scope of this release

This release repairs the calculation and evidence-contract failures recorded in
`validation/audit-0.3.0.md`. The disposition of all 25 findings is in
`validation/resolution-0.4.0.md`. Binary prediction events, invalid prediction
coercion, stratified permutations, ALE coordinates, subgroup context and stale
attached evidence have dedicated regression tests. Aggregate heuristic grades
are removed. Reports and local narratives use scoped diagnostics.

The core does not require Java, a browser, network access or credentials during
checks. H2O and hosted narrative integrations are explicitly opt-in. Examples
and installed vignettes use local data and models. The five vignettes describe
first use, selection, validation, statistical methods and existing-model reports.

## Validation record

`validation/release-0.4.0.md` records the executed tests, runtime, source checks,
browser checks and final release artifact. Older 0.2.0 Win-builder and 0.3.0
release checks do not establish anything about this archive.

The release pipeline builds once, fully checks that archive with R CMD check
--as-cran including PDF/HTML manuals, checks the same archive under R-devel,
verifies its SHA-256 and publishes that checked archive. Native engines and live
H2O have separate gates. An isolated job installs and tests the exact optional
engine minimum versions declared in DESCRIPTION; current-version jobs remain.

## Interpretation and compatibility

Result and aggregate export schemas are 2.0. Affected analyses from 0.3.0 should
be recomputed. Fitting and explanation methods are described and cited in the
installed documentation. Shuffle intervals describe Monte Carlo variation;
fixed-model evaluation bootstraps omit selection and refitting uncertainty.
Structured H2O validation and temporal tuning remain unsupported. External
learned preprocessing that crosses H2O's internal folds is rejected.

The executed DALEX/modelStudio comparison establishes a bounded same-model
workflow comparison, not a general superiority or human-usability claim.
