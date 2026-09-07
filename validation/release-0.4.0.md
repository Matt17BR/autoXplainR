# AutoXplainR 0.4.0 release evidence

Prepared 2026-09-07. This release repairs the [independent 0.3.0 audit](audit-0.3.0.md).
[Every finding has a disposition](resolution-0.4.0.md), with executable acceptance
checks and explicit remaining limits. This record does not claim CRAN acceptance.

## Executed local checks

| Check | Result |
|---|---|
| Runtime | R 4.5.2, Ubuntu 26.04.1, x86_64 |
| Complete installed-engine suite | 2,717 passing assertions; no failures or warnings; two opt-in live skips |
| Live H2O integration | 93 passing assertions on CRAN H2O 3.44.0.3 and Temurin 17; no failures, warnings or skips |
| Statement coverage | 89.66%; [instrumented summary](results/coverage-0.4.0.txt) |
| Minimum-version engine check | All eight declared engine versions match; 694 adapter/geometry/data-contract assertions pass |
| Lint and spelling | Clean; roxygen2 8.1.0 documentation regenerated |
| Documentation | All five vignette code paths execute; pkgdown site builds |
| Numerical references | Eleven PDP/ALE/analytic checks; maximum absolute error below 5.69e-14 |
| Bootstrap experiment | 90% coverage at n=20 and 96% at n=100, for nominal 95%, with 100 replicates per setting |
| Browser gate | 51/51 checks; 320, 390, 768 and 1440px; keyboard disclosures and no page overflow |
| Printed report | Closed/open/no-JavaScript PDFs contain identical 15,532-character normalized text |
| Automated accessibility | No axe A/AA violations in the eight tested width/disclosure states; contrast incompletes retained |
| Comparator workflow | Same fitted objects/data/split with DALEX and modelStudio; six metrics identical; four offline HTML smoke checks pass |

The live Gemini test was not run. Its mocked transport tests do not establish
current provider availability or generated-text accuracy. H2O was exercised
separately on a local Java cluster. The full-suite version record retains the
then-installed vendor H2O 3.46.0.9. The corrected CRAN minimum, 3.44.0.3, was
installed and tested separately with Temurin 17. Native minimum-version evidence
is described in [engine-support.md](engine-support.md); both the selected-engine
installer and live integration entry point use an isolated library.

The first remote checks rejected the vendor-only H2O minimum because CRAN could
not resolve it. The declared minimum and Java runtime were corrected only after
the published CRAN version passed the integration checks.

Coverage measures instrumented execution, not the fraction of statistical
behaviors proven correct. The simulation's small-sample undercoverage remains a
finding. The [workflow comparison](workflow-comparison.md) is a bounded executed
comparison, not evidence of usability or general superiority. The
[user-study protocol](user-study-protocol.md) has no human results.

## Report and source checks

The [browser record](report-browser.md) names the exact fixture hash and tool
versions. It also records a deliberately broken fixture that failed 38 checks,
showing that the gate rejects injected regressions. Four README screenshots were
recaptured from the actual delivery-time report. Automated contrast checks left
manual review cases; Safari, Firefox and screen-reader behavior are unverified.

The first local source check identified non-ASCII punctuation in R strings and
a generated top-level `figure` directory. The punctuation now uses R Unicode
escapes, and the generated directory is excluded from source builds. Both PDF
and HTML manuals built, and tests/examples/vignette rebuilding passed. New article
URLs were not yet deployed at that first check. Final archive and publication
results are recorded below when the release gates finish; the earlier warning
is not counted as a successful release check.

A later remote package build exposed a false evidence mismatch when R retained
source references. Source-file timestamps and caches were entering the model
fingerprint. Those nonstatistical references are now removed while prediction
code and model/evaluation content remain checked. Regressions cover source
metadata changes, compilation and RDS reuse; the exact previously failing
`R_KEEP_PKG_SOURCE=yes NOT_CRAN=true R CMD build --no-manual` command passes.

The R 4.1 job also found optional tests running against its bundled mgcv
1.8.39, below the declared 1.9-4 floor. Test guards now read the loaded package
DESCRIPTION and skip unavailable or unsupported dependencies. Supported versions
remain mandatory in the separate exact-minimum and current-engine jobs.

The [installed-artifact smoke check](check-installed-artifact.R) passed against
the locally built archive in a fresh package library. It exercises regression,
binary and multiclass fits, raw prediction, RDS reuse, narratives, HTML and JSON
export, plus missing-value and novel-category preprocessing. The same script is
required against the downloaded publication artifact below.

## Publication gates

Main-branch checks precede the version tag. The tag workflow blocks publication
on native tests, lint/spelling, documentation synchronization, reference methods,
coverage, live H2O, the exact minimum-engine installer/tests, and the browser gate.
It builds one archive, fully checks it under release R including manuals, then
checks that same archive under R-devel and verifies SHA-256 throughout.

The final tag/commit, workflow links, check notes and published archive checksum
will be added after the gates succeed. No 0.3.0 or older Win-builder result is
reused as evidence for this release. CRAN submission remains a separate
maintainer action.
