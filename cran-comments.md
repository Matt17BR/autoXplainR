## GitHub release and CRAN preparation

AutoXplainR 0.7.0 reduces tuning overhead and report size, adds recorded
computation choices for larger models, and separates complete-data evaluation
from bounded explanations and exploration.
This is a preparation note, not
a CRAN submission or acceptance record. CRAN submission is not part of this
GitHub release.

The exact tested source, CI gates, task walkthrough and remaining limitations
will be recorded in `validation/release-0.7.0.md` after release acceptance.
Current scalability measurements and pending checks are in
`validation/scalability/findings.md`. Historical checks remain in
`validation/release-0.6.2.md` and do not establish anything about the new archive.

Examples and vignettes use local data and models. H2O and hosted narrative
integrations are opt-in. Reports compute in R and need no external service.
Statistical scope and the consequences of feature screening are documented in
the statistical-methods vignette. No new estimator, comparative usability result
or superiority over established R packages is claimed.
