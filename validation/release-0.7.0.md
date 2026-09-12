# AutoXplainR 0.7.0 validation record

Status: candidate verification is in progress. No 0.7.0 release has been
published by this work yet. The final archive and public site must pass their
own checks before this record can claim publication.

## What this release addresses

The previous validation established three practical limits: a recommended
search could take almost nine minutes, a wide report occupied about 60 MB,
and no workflow had been accepted with a million training rows. The
[scalability protocol](scalability/README.md) separates fitting, evaluation,
explanations, export and browser work, with fixed problems and explicit budgets.

The changes remove repeated work and redundant retained inputs, and make large
reports cheaper to construct and explore. Some changes are exact computation
repairs. Others change the fitting procedure or explanation population and
therefore require separate evidence. The [findings](scalability/findings.md)
identify those differences and retain failed and slower cases.

## What the review caught

Passing small tests was insufficient. Independent checks found a retained
prediction-column naming defect, misleading BAM convergence records, missing
source identities in sampled explanations, and platform-dependent overlap and
association checks. Actual browser tasks also found a WebKit HTML parsing stall
before report code could run. Each repair has a reproducing case and an
independent comparison, rather than only a test of its implementation.

The first automatic BAM policy made the recommended searches faster, but lost
three previously usable additive configurations on the binary problem. The
revised policy keeps the coefficient-work shortcut for Gaussian regression and
retains GAM below 10,000 binary training rows. That choice follows the native
algorithms and training-fold failures, not the final test scores. Explicit
solver requests remain available, and the solver is fixed before CV starts.
Final acceptance of that revision is still pending.

The larger neural iteration allowance improves selection on the 10,000-row
nonlinear problem but takes longer. Its evaluation is compared with the known
conditional mean given the inputs that remain observable after missingness.
That reference is diagnostic and was never used for selection.

The interval review found avoidable allocations and repeated loss arithmetic.
The faster paired bootstrap keeps every original resample and interval
result; it does not replace the method or reduce its population. Its small-case
exactness checks pass, with larger evaluation workloads still pending.

The wide million-row run also exposed a failure after fitting and scoring: the
final evidence fingerprint tried to allocate another complete serialized copy.
The repair streams that same serialization through temporary storage. Under a
512 MiB address-space cap, the old function failed and the repair succeeded;
independent whole-byte hashing verified the exact identity and a changed final
value. The complete wide workflow still needs to pass with the repair installed.

The installed-package gate caught a separate JSON export failure in classification
audit summaries. Exporting class counts as named values repairs the documented
workflow while preserving the original audit tables, class order and zero counts.

## Product inspection

The [difficult regression walkthrough](scalability/report-walkthrough.md) follows
selection, parameters, convergence, feature effects and a prediction error back
to its original missing input. Separate [large-report checks](scalability/reports/README.md)
exercise all-record lookup and filtered exploration at desktop and phone widths.
These are agent walkthroughs and browser tests, not a recruited-participant study.

## Pending release acceptance

The full final-source platform, native-engine, numerical, browser, documentation
and coverage gates must pass. The checked archive must then be freshly installed
and reloaded, including categorical boosting, automatic BAM, complete evaluation
scores, sampled explanation identities and exact exported values. The
[archive acceptance protocol](scalability/release-acceptance.md) defines that work.

After publication, the actual downloaded archive must match its asset digest,
checksum file and reviewed source inventory. The deployed reports and screenshots
must match the accepted assets and pass public-site tasks. CRAN submission is
outside this release.
