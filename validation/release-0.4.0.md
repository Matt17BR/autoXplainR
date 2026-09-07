# AutoXplainR 0.4.0 release evidence

Published [v0.4.0](https://github.com/Matt17BR/autoXplainR/releases/tag/v0.4.0)
on 2026-09-07 at 12:12:31 UTC from commit
[`bc941b63bd308df24f1950579a01b1d1af428c50`](https://github.com/Matt17BR/autoXplainR/commit/bc941b63bd308df24f1950579a01b1d1af428c50).
This release repairs the [independent 0.3.0 audit](audit-0.3.0.md).
[Every finding has a disposition](resolution-0.4.0.md), with executable acceptance
checks and explicit remaining limits. This post-publication record does not
change the tagged package and does not claim CRAN submission or acceptance.

## Published archive

The [release workflow](https://github.com/Matt17BR/autoXplainR/actions/runs/34119029839)
passed every required gate. It built one archive, checked that archive under
release R and R-devel, and published it with its checksum and release notes.
The downloaded public asset matched the checked artifact byte for byte:

```text
d159edd52a892c506237faaf5935f4b9c61db4d007b764e52876815bcd3f0cbd  AutoXplainR_0.4.0.tar.gz
```

| Archive check | Result |
|---|---|
| R 4.6.1, Ubuntu 24.04.4, `--as-cran` | 0 errors, 0 warnings, 1 NOTE: “New submission”; [complete log](results/release-0.4.0-r-check.txt) |
| R-devel 2026-09-06 r90498, same archive, `--as-cran --no-manual` | 0 errors, 0 warnings, 1 NOTE: “New submission”; [complete log](results/release-0.4.0-r-devel-check.txt) |
| Examples, tests and vignette rebuilding | Passed in both archive checks |
| PDF and HTML manuals | Passed in the release-R check |
| Documentation links | URL checker reported all URLs correct |
| Downloaded publication | SHA256SUMS verified; fresh installation and all [installed-artifact smoke cases](results/release-0.4.0-installed-smoke.txt) passed on local R 4.5.2 |

The R-devel job intentionally used a smaller dependency set. It reported nine
unavailable suggested packages; optional native and live H2O coverage comes
from the separate successful engine gates, not from that job. The
[machine-readable record](results/release-0.4.0.json) retains versions, job links,
checksum, dependency scope and the browser summary.

Before tagging, the same commit passed [all seven package-check jobs](https://github.com/Matt17BR/autoXplainR/actions/runs/34118057785),
including Windows, macOS and Linux release R, Linux R 4.1, oldrel and R-devel.
[Current Ubuntu/Windows engines and exact minima](https://github.com/Matt17BR/autoXplainR/actions/runs/34118057771),
[coverage](https://github.com/Matt17BR/autoXplainR/actions/runs/34118057782),
[reference methods](https://github.com/Matt17BR/autoXplainR/actions/runs/34118057765),
[browser checks](https://github.com/Matt17BR/autoXplainR/actions/runs/34118057816),
[lint](https://github.com/Matt17BR/autoXplainR/actions/runs/34118057730) and
[the documentation site](https://github.com/Matt17BR/autoXplainR/actions/runs/34118057750)
also passed. The tag workflow then repeated its publication gates.

## Executed local checks

| Check | Result |
|---|---|
| Runtime | R 4.5.2, Ubuntu 26.04.1, x86_64 |
| Complete installed-engine suite | 2,732 passing assertions; no failures or warnings; two opt-in live skips |
| Live Gemini integration | 12 passing assertions with gemini-3.5-flash, low thinking and a 4,000-token budget; no fallback |
| Live H2O integration | 93 passing assertions on CRAN H2O 3.44.0.3 and Temurin 17; no failures, warnings or skips |
| Statement coverage | 89.67%; [instrumented summary](results/coverage-0.4.0.txt) |
| Minimum-version engine check | All eight declared engine versions match; 694 adapter/geometry/data-contract assertions pass |
| Lint and spelling | Clean; roxygen2 8.1.0 documentation regenerated |
| Documentation | All five vignette code paths execute; pkgdown site builds |
| Numerical references | Eleven PDP/ALE/analytic checks; maximum absolute error below 5.69e-14 |
| Bootstrap experiment | 90% coverage at n=20 and 96% at n=100, for nominal 95%, with 100 replicates per setting |
| Browser gate | 51/51 checks; 320, 390, 768 and 1440px; keyboard disclosures and no page overflow |
| Printed report | Closed/open/no-JavaScript PDFs contain identical 15,532-character normalized text |
| Automated accessibility | No axe A/AA violations in the eight tested width/disclosure states; contrast incompletes retained |
| Comparator workflow | Same fitted objects/data/split with DALEX and modelStudio; six metrics identical; four offline HTML smoke checks pass |

The live Gemini test used aggregate evidence from the public iris example. Its
initial default request exhausted the shared reasoning/response budget. The
shipped model now requests low thinking, and the corrected live test passed in
4.3 seconds. An outdated disclosure assertion was corrected too. This verifies
one provider interaction and its format, not generated-text accuracy or future
availability. Model overrides retain provider defaults; request settings are
recorded on success and fallback. H2O was exercised
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
URLs were not yet deployed at that first check. The published archive's clean
manual and URL checks supersede that preliminary result.

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
the downloaded publication in a fresh package library, with execution outside
the checkout and an assertion on the loaded package path. It exercised
regression, binary and multiclass fits, raw prediction, RDS reuse, narratives,
HTML and JSON export, plus missing-value and novel-category preprocessing.

## Tagged browser evidence

The tag workflow generated a fresh report and passed all 51 checks under
Chromium 145.0.7632.6, Playwright 1.58.0 and axe 4.13.0. Its HTML SHA-256 is
`2b48d99d944e3d3e9eff717aa98b536ca28fcf6b6561badbcd4b0f61c4112444`.
Its closed/open/no-JavaScript PDFs each contain the same 15,703 normalized
characters across 14 pages, with extracted text inside the tested margins.
The first two printed pages were visually inspected for margins and wrapping.
That inspection does not establish complete print legibility or accessibility.

The committed/public example has SHA-256
`697c1454c314e6d0e5b219bf997a71dd71143c42943101572e25776fe289bc54`.
It has its own [local browser record](results/browser-0.4.0.json). Runtime and
measured provenance differ between those executions, so their hashes and print
counts must not be conflated. The tagged browser summary retains unresolved axe
contrast cases for human review. No WCAG conformance or human-usability claim
follows from passing the automated checks.

No 0.3.0 or older Win-builder result is reused as evidence for this release.
Human usability testing and a CRAN submission remain separate future work.
