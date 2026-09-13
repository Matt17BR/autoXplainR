## GitHub release and CRAN preparation

AutoXplainR 0.8.0 adds a tabular portfolio, adaptive
screening, training-only boosting round selection, size-aware forest budgets,
and reports of the fitted settings and selection evidence. It also adds AUC
and RMSLE selection and preserves actionable failure records.
This is a preparation note, not a CRAN submission or acceptance record.
CRAN submission is not part of this GitHub release preparation.

PR #8 was merged at `3cb78eb`, and the annotated `v0.8.0` tag was pushed.
All 16 tagged release jobs passed, including the 7,583-assertion quality suite
and 110 live H2O assertions. The website serves 0.8.0; its three public reports
and eight gallery images match the reviewed files at their default URLs. The
published source archive matches the checked archive and tagged source files.
Fresh isolated installation and saved-artifact replay from the public download
passed for seven workflows.

Completed full-data comparisons, report and sampled-record checks, measured
tradeoffs and preserved failures are recorded in `validation/release-0.8.0.md`.
Protocols and machine-readable results are under `validation/competitive-tabular/`.
These measurements keep their original frozen benchmark source identity.

Historical 0.7.0 evidence is recorded in `validation/release-0.7.0.md`: all
16 release jobs passed for that release, followed by verification of its actual
download and public site. Its scale measurements remain in
`validation/scalability/findings.md`, and earlier checks remain in
`validation/release-0.6.2.md`. These historical records do not validate the 0.8.0 archive.

Examples and vignettes use local data and models. H2O and hosted narrative
integrations are opt-in. Reports compute in R and need no external service.
Statistical scope and the consequences of feature screening are documented in
the statistical-methods vignette. No new estimator, comparative usability result
or superiority over established R packages is claimed.
