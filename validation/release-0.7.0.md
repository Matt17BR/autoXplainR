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
The final binary replay passed all 30 configurations and 150 folds, with every
retained model's holdout predictions unchanged from 0.6.2. It took 202.443 seconds
versus 209.985 seconds; this small timing difference is a single observation.

The larger neural iteration allowance improves selection on the 10,000-row
nonlinear problem but takes longer. Its evaluation is compared with the known
conditional mean given the inputs that remain observable after missingness.
That reference is diagnostic and was never used for selection.

The interval review found avoidable allocations and repeated loss arithmetic.
The faster paired bootstrap keeps the method, all evaluation rows and 1,000
draws. Complete outputs match exactly through one million regression rows and
200,000 eight-class Brier rows. Million-row regression fell from 135.608 to
28.883 seconds. The old million-row Brier call timed out under the 180-second
process bound; the candidate completed in 47.478 seconds including preparation
and saving. That censored pair has no whole-output parity claim.

The wide million-row run also exposed a failure after fitting and scoring: the
final evidence fingerprint tried to allocate another complete serialized copy.
The repair streams that same serialization through temporary storage. Under a
512 MiB address-space cap, the old function failed and the repair succeeded;
independent whole-byte hashing verified the exact identity and a changed final
value. The complete wide workflow subsequently passed in 303.841 seconds,
including both selected full-training refits and final fingerprinting, under the
unchanged 420-second and 12 GiB bounds. Its uncompressed saved result still
occupies 3.34 GB, so this is not a low-memory workflow.

The installed-package gate caught a separate JSON export failure in classification
audit summaries. Exporting class counts as named values repairs the documented
workflow while preserving the original audit tables, class order and zero counts.

The [mobile accessibility review](scalability/reports/mobile-axe-review.md) found
that the test's two-frame pause did not wait for focus-driven smooth scrolling
to finish. Scans now require stable geometry before and throughout axe, without
changing the scroll destination or hiding open details. The complete local
CI-fixture replay passed 184 checks, and axe still rejected deliberately tiny
adjacent controls. The original intermittent CI violation did not reproduce
locally, so scrolling is a supported explanation rather than a proven cause.

## Product inspection

The [difficult regression walkthrough](scalability/report-walkthrough.md) follows
selection, parameters, convergence, feature effects and a prediction error back
to its original missing input. Separate [large-report checks](scalability/reports/README.md)
exercise all-record lookup and filtered exploration at desktop and phone widths.
These are agent walkthroughs and browser tests, not a recruited-participant study.

The [complete million-row walkthrough](scalability/million/report-walkthrough.md)
uses the actual 3.9 MB report from an 81.055-second public call. It covers model
comparison, search coverage, computation choices, data summaries, sampled
explanations and phone-width model details. The run used all one million training
rows and all 20,000 evaluation rows, with 5,000 explanation rows and 1,000 paired
bootstrap draws. Every original prediction from all three models matched after
a fresh-process reload. Its small fixed grid is separate from the stronger
million-row fit-only result of RMSE 0.7491.

The walkthrough also challenged the model-size comparison. The baseline retains
large R diagnostic arrays, while native XGBoost allocation is hidden behind a
pointer. Chart help and the R documentation now explain that these estimates
are not deployment memory or file sizes. The final report derivative differs
only in those two help passages; all embedded data payloads remain identical.

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
