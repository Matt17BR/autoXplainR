## GitHub release and CRAN preparation

The unreleased AutoXplainR 0.8.0 candidate adds a tabular portfolio, adaptive
screening, training-only boosting round selection, size-aware forest budgets,
and reports of the fitted settings and selection evidence. It also adds AUC
and RMSLE selection and preserves actionable failure records.
This is a preparation note, not a CRAN submission or acceptance record.
CRAN submission is not part of this GitHub release preparation.

The 0.8.0 release gates and verification of its published archive and public
site remain pending. Local candidate checks and development measurements do
not establish those outcomes. The comparison protocols and recorded development
runs are under `validation/competitive-tabular/`; they are not an acceptance claim.

Historical 0.7.0 evidence is recorded in `validation/release-0.7.0.md`: all
16 release jobs passed for that release, followed by verification of its actual
download and public site. Its scale measurements remain in
`validation/scalability/findings.md`, and earlier checks remain in
`validation/release-0.6.2.md`. These records do not validate the 0.8.0 candidate
or any future 0.8.0 archive.

Examples and vignettes use local data and models. H2O and hosted narrative
integrations are opt-in. Reports compute in R and need no external service.
Statistical scope and the consequences of feature screening are documented in
the statistical-methods vignette. No new estimator, comparative usability result
or superiority over established R packages is claimed.
