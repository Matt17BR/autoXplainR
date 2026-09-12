# AutoXplainR 0.7.0 validation record

Status: [0.7.0 is published](https://github.com/Matt17BR/autoXplainR/releases/tag/v0.7.0).
All 16 release jobs passed. The actual download, fresh installation, saved-model
reloads and post-publication public site are verified. The package source and
tag are unchanged by the later validation-record commit.

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
value. The wide fit-only public call subsequently passed in 303.841 seconds,
including both selected full-training refits, full evaluation and final
fingerprinting, under the unchanged 420-second and 12 GiB bounds. It did not
render a report or compute explanations. Its uncompressed saved result still
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

The [review of undecided accessibility items](scalability/reports/accessibility-review.md)
found two genuine naming defects:
comparison and feature-importance containers had labels without grouping roles.
The final repair gives them explicit group semantics and checks their accessible
names and content through actual browser roles. A separate contrast review
covered the undecided chart text and decorative keys; the report styles did not
need changing. Browser evidence now retains the undecided targets for review.

The [actual final PR artifact](results/release-0.7.0-pr-accessibility.json)
exposes all 17 tested group names and has no remaining ARIA items. Its 443
undecided contrast occurrences were reviewed, including 12 importance values
whose containing buttons confused background detection. The
[final tag artifact](results/release-0.7.0-tag-accessibility.json) has 449
contrast occurrences and one additional overlap flag on a cost-chart label.
The actual label remains complete and readable; the scan-time overlap mechanism
was not reproduced. These are scoped reviews, not a claim that every
accessibility requirement has been exhaustively verified.

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
are not deployment memory or file sizes. The final report derivative adds those
two help passages and explicit comparison and importance group roles. All
embedded data payloads remain byte-identical to the original timed report.

## Publication evidence

[PR #6](https://github.com/Matt17BR/autoXplainR/pull/6) contains the scale work.
[PR #7](https://github.com/Matt17BR/autoXplainR/pull/7) passed all 14 checks on
`99de419c82b36fab09d8de6d0171120781c0fc6c` and was merged as
`61aaa8f9ddca445b6938da9183b308fc1d69c02d`, with an identical source tree.
The annotated `v0.7.0` tag points to that merge. All 16 jobs in the
[release workflow](https://github.com/Matt17BR/autoXplainR/actions/runs/34672967338)
passed, and publication completed at 2026-09-12 04:52:22 UTC.

The published archive is `AutoXplainR_0.7.0.tar.gz`, 2,331,983 bytes, with SHA-256
`bc6ad22bee49a9a3fee2ba7a092d975372815e07629295b3a012871782ac2532`.
Its 217 authored files match the reviewed source; R adds build metadata and
generated files to make 233 archive files. The full source-package check passed
with zero errors, zero warnings and one "New submission" note. The same archive
passed R-devel before publication. CI freshly installed it and reloaded all 21
models across seven workflows, including native categorical boosting and
automatic BAM. The [acceptance protocol](scalability/release-acceptance.md)
specifies the original-data, prediction, explanation and export checks.

The final quality gate passed 5,293 assertions with no failures or warnings,
and measured 92.59% statement coverage. The two opt-in tests were skipped in
that run; live H2O passed separately, and the paid hosted Gemini test was not
run. R 4.1 compatibility retains known upstream test warnings and optional
dependency skips, recorded separately from the release archive's check result.

Browser runners passed 1,382 explorer checks, 1,923 prediction checks, 118
gallery checks and 128 layout/print checks. These counts are per runner and
must not be summed as unique tests. The final supplied-model runner passed
181 checks. Its [timing review](results/release-0.7.0-cost-withholding.json)
confirmed a legitimate `resolution_limited` result: all three repeats completed,
but a measured block was too close to the observed clock step. Three applicable
withholding checks replaced six finite-cost checks, explaining the difference
from the earlier 184-check run. The report retains the reason instead of
presenting a numeric cost; the CI artifact does not preserve raw durations.

The [final public walkthrough](results/release-0.7.0-public-walkthrough.md)
uses the served reports after their bytes matched the reviewed gallery. It
covers actual model settings, missing-input records, phone-width cutoff changes
and multiclass comparison curves. The [post-publication site verdict](results/release-0.7.0-public-site.json)
confirms HTTP 200 and exact reviewed bytes for all three reports and eight
screenshots, bound to the completed main deployment.

A fresh private installation of the actual public download passed all seven
workflows, all 21 models and nine compressed export blocks. Separate processes
also reloaded both original final million-row results using that installation.
Each replay checked all three models against all 20,000 original holdout
predictions, within the recorded 1e-12 tolerance. The saved source results
remained unchanged. The [portable release record](results/release-0.7.0.json)
retains the source inventory, actual asset digest, checksum verification,
individual CI jobs and complete acceptance scope.

The original measurement cohorts retain their source identities; later report
help and accessibility repairs do not turn them into new timing measurements.
CRAN submission is outside this GitHub release.
