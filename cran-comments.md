## GitHub release and CRAN preparation

AutoXplainR 0.6.2 improves grouped classification validation, sparse regularized
model encoding, numerical failure handling and reports for wide data.
This is a preparation note, not
a CRAN submission or acceptance record. CRAN submission is not part of this
GitHub release.

The exact tested source, CI gates, task walkthrough and remaining limitations
are recorded in `validation/release-0.6.2.md`. Historical checks remain in
`validation/release-0.6.1.md` and do not establish anything about the new archive.

Examples and vignettes use local data and models. H2O and hosted narrative
integrations are opt-in. Reports compute in R and need no external service.
Statistical scope and the consequences of feature screening are documented in
the statistical-methods vignette. No new estimator, comparative usability result
or superiority over established R packages is claimed.
