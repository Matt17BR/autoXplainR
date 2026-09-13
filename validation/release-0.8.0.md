# AutoXplainR 0.8.0 validation record

Status: candidate under review. No 0.8.0 tag or release has been published.
All 14 CI jobs passed on the preceding candidate. Its first full-data run
passed YearPrediction and then stopped on a Covertype screening overflow.
Repairs are being verified in a follow-up candidate; final package checks,
remaining full-data calls and publication remain release gates.

## What changed

`autoxplain(..., portfolio = "tabular")` compares regularized models, random
forests and XGBoost with an automatic 18-setting search. Training-only screening
narrows the candidates before complete cross-validation. Boosting calibrates
its stopping round inside each training fold, and large forests use explicit
tree budgets. Reports distinguish screened settings, complete CV results and
the final model's actual controls. The ordinary report still includes data
exploration, prediction diagnostics and explanations.

The [benchmark protocol](competitive-tabular/README.md) sets the data boundaries,
native references, quality thresholds and process budgets. The
[development results](competitive-tabular/DEVELOPMENT.md) retain failures and
interruptions alongside successes. They do not establish full-data acceptance.
The complete public call must fit, explain, render and save its results within
7,200 seconds and 24 GiB on each declared acceptance case.

## Defects found during review

The early search crashed when binding multiclass probability records from
different engines. Other checks caught progress handlers changing random draws,
all-screening failures hiding their cause, and requested defaults being confused
with learned native settings. Each repair has a reproducing case. The
[independent review](competitive-tabular/release-candidate-review.md) records its
scope and repeated checks.

The report tests also needed correction. One oracle rounded model parameters
to eight digits, which rejected the report's accurate display of a one-third
class prior. Numeric expectations now carry hexadecimal doubles from the fitted
settings. The browser parses displayed values and compares their exact numeric
value, while separate checks retain named-vector labels and parameter identity.
Deliberately rounded, missing and renamed values fail the revised check.

The acceptance harness was challenged separately: verification must enumerate
every retained model, reject missing row metadata, bind reported scores to saved
predictions and charge failed scoring attempts to their original native fit.
Harness tests use development results and synthetic records. Passing them does
not establish acceptance quality.

## Completed local checks before the full-data findings

The tested source snapshot is `candidate-v6`, inventory SHA-256
`afa4e63bfa1506cdce8e51d1b0271aa844f02a868d20123df6b3b07710e438ed`.
The original frozen acceptance installation is `candidate-v9`, inventory SHA-256
`8f8ba04c4560c0274861cfb8559703eb5b5123f6ce311ec05bfb9d77f1044b5f`.
Changes from candidate-v6 through candidate-v9 comprised README wording, two overview screenshot
copies, two test files, the summary-report help described below and its NEWS
entry. Fitting and statistical code, report styles/scripts, help files and
DESCRIPTION were unchanged over that interval. The follow-up repairs below change package code.
The test changes isolate three planning checks from optional engine installation;
they preserve the real dependency guard and native fitting tests. Both affected
files passed 260 assertions with engines installed. A fresh process also passed
the 59 relevant assertions without loading any optional native engine namespace.

A subsequent walkthrough found that summary reports offered only an argument
fragment for exporting individual errors. The working report now provides a
collapsed help panel with a complete command and the exported data scope. The
command was copied from the HTML and executed for eight summary/none paths
across four saved synthetic results. Every export contained linked records and
every retained model's predictions stayed unchanged. All 52 visible-help checks
passed on desktop and phone, including keyboard opening and code scrolling.
The three affected report test files and source lint passed. Candidate-v9
includes this help; its other 252 packaged source files match candidate-v8.

The gallery was regenerated from the same saved fitted examples after that
change. All three HTML files and eight screenshots were byte-identical because
the public examples already export individual records. The refreshed manifest
binds the new source, and all 118 committed-gallery browser checks passed.

| Check | Result | Scope |
| --- | --- | --- |
| Complete R tests | 7,233 assertions passed; no failures or warnings | Two opt-in integrations skipped: live H2O and Gemini |
| Source build and CRAN-style check | No errors or warnings; one note | Tests ran separately; manuals, examples and five vignette scripts passed |
| Documentation, lint and spelling | Passed | Generated help is current |
| Report exploration | 1,382 checks passed; no browser errors | Four example types in Chromium; separate contrast review found no confirmed failure in 355 undecided occurrences |
| Committed gallery | 118 smoke checks passed | All three HTML reports and eight screenshots regenerated and visually inspected |
| Adaptive selection UI | Chromium 609, Firefox 597 and WebKit 597 checks passed | Final CI artifact; independent selection fixture, keyboard, deep links, mobile charts and deliberate overlap rejection |
| Fold-table layout and print | 131 checks passed | Named fold table, complete errors, mobile scrolling and an inspected PDF page |

The candidate-v9 archive checked locally has SHA-256
`878184be2e59ec3459197c5044f0f3de4adfdf937bcbd77b0b9ed509fca6a9f2`
and contains 2,444,156 bytes. It is a candidate-v9 archive, not a published
artifact. Build and check took 50.329 and 198.654 seconds. All 24 example topics,
five vignette scripts and the PDF/HTML manuals passed. The archive's source
entries were verified against the snapshot, with only documented build-generated
fields and vignette outputs added. The note covers new-submission status and two benchmark links whose
files have not reached the public main branch. Those links must be checked again
after publication of the documentation.

Browser tasks and manual inspection are implementer walkthroughs, not a
recruited-participant usability study. Counts describe individual runners and
must not be added as unique tests. Shared-host timings are observations, not
isolated speed measurements.

The [contrast review](competitive-tabular/explorer-contrast-review-20260913.md)
checks actual backgrounds, chart labels and mobile tab visibility. All 99
initially off-screen labels became visible after scrolling; all 14 checked
keyboard states exposed the selected tab. The hollow fold marker is faint but
passes the graphical contrast threshold. This review resolves those recorded
items and does not claim a complete accessibility certification.

The [provenance review](../PROVENANCE.md) covers the six commits from `v0.7.0`
through the candidate head. The bundled decompressor and its retained MIT
license passed the vendor check. Benchmark data sources and attribution are
recorded; their raw observations, complete predictions and fitted models are
not committed or included in the source archive.

The review found a benchmark preparation gap: Bank's ZIP is verified, but its
separately extracted CSV is read without another checksum check. A subsequent
[byte comparison](competitive-tabular/bank-extracted-source-verification-20260913.json)
confirmed that the current CSV and data dictionary match the verified nested
archive exactly. This is a post-preparation check, not a claim about the files
at every earlier instant. The preparation and publication repairs were
integrated only after the original run and report bindings were secured.
The private downloader repair passed eight offline fixture checks. Two
negative controls reproduce the original behavior: silently overwriting an
altered CSV and allowing a source refresh in a cache with prepared partitions.
The new preparation guard also binds each dataset to the exact filename read
by the script. It runs before any dataset reads or partition writes. All 16
synthetic R checks passed after the acceptance controller stopped. A negative
control confirmed that the earlier guard reached the data-read boundary with
Year's required source record missing. The integrated downloader and preparation
tests now run against the actual repository sources in statistics and release CI.
The prepared acceptance inputs and original frozen scripts were not changed.

A native-reference summary had also copied a complete host process list,
including unrelated desktop command lines. The public file now retains process
counts and the original diagnostic hash; all other JSON values are unchanged.
The original bytes remain in the private cache. Fourteen publication checks
passed, including preservation checks and a synthetic browser-argument example.
The integration check rejected the original collector and passes on the repaired
repository source. Both CI workflows run it. Across the source and publication
guards, all 39 bounded checks passed. The public JSON stayed byte-identical
during integration; the original frozen collector is preserved separately.

The first CI round passed current and minimum native engines, numerical
references, lint, and 93.12% statement coverage. The separate
[live H2O run](https://github.com/Matt17BR/autoXplainR/actions/runs/34764504254)
passed on commit `7d0bc1edf604aaea5b1f6f002908ede3e570d3c6`.
The next round passed all seven R-check jobs and all three native-engine jobs,
plus lint, statistics and coverage. Its Firefox chart check failed on text
rectangle overlaps; the screenshots show separate readable labels. The
[CI review](competitive-tabular/ci-review-20260913.md) records that diagnosis
and the completed verification on commit
`23d541d1e2889da710c9fbe09252aa617f5dbf89`. All 14 jobs passed on that head,
including the complete browser workflow; coverage was 93.13%. Firefox's actual
text has a four-pixel gap where decorated client rectangles overlap. A deliberate
15-pixel text overlap is rejected. No package code changed in response to those
CI failures. These report tests use controlled examples and do not establish
the large-data acceptance results.

## Full-data acceptance: first result and failure

All eight native references completed fitting, held-out scoring and independent
verification. Every saved model reproduced all evaluation predictions exactly in
a fresh R session. The one-thread Bank references match the declared automatic
thread policy; the public call's actual resolved controls still need checking.
The candidate, installed package, protocol and exact native artifacts
were frozen before held-out scoring. The final acceptance manifest has SHA-256
`a0ff580cb3a565dd6ccfad7c1532d48d035ffdd2414b17d12160537a7655b510`.
The separately reviewed controller began on 13 September 2026 at 16:11 UTC.

The eight native score/replay/verification sequences and the published-0.7 Bank
comparison passed without retries. The old Bank result's main-model log loss is
0.273553, with 197.918 seconds for fitting and scoring; all three retained
models replayed exactly. The complete YearPredictionMSD public call then passed
quality, full-training, complete-prediction and cold-replay checks for all four
retained models. Its default summary HTML is 5.87 MB. The following Covertype
call failed before screening because class-sample allocation overflowed R's
integer range. The controller stopped; Bank was not attempted. The
[first full-call findings](competitive-tabular/full-call-findings-v9.md) retain
the original timings, hashes, failure and report issues. Repairs and subsequent
attempts must remain distinct from this frozen candidate's results. Replay and
verification costs are recorded separately from each fitting/scoring budget.

## Follow-up repairs and original-report review

Large-class screening and report-row allocation now calculate quotas without
integer overflow. Their 165 focused assertions cover large counts, rare classes,
exact quotas, disjoint folds, ordinary sample identity and RNG preservation.
Native forest progress passed 678 assertions across the affected fitting tests.
Two separate minute-long forest fits confirmed identical trees and OOB predictions
in information/quiet modes, with a live completed-tree update only in information
mode. These checks do not replace the remaining full public calls.

Cost formatting and default-resource selection passed 246 focused assertions,
31 browser checks and saved-result compatibility checks. The implementation
preserves raw measurements and native baseline diagnostics. Actual Year report
inspection additionally identified a long input-policy cell and no-op center/scale
tables. Their replacement passed 201 focused assertions, saved-model parity checks
and 46 final dialog browser checks, including visible training settings on open.

The original Year report passed 12 offline browser task groups against an
independent oracle. All scores, retained native controls and complete CV records
agree. The original summary loaded in 0.384 seconds in this browser run and made
no external requests. Direct desktop inspection covered model choice, forest and
booster settings, a screened-out candidate, data distributions and relationships,
comparative effects, aggregate errors and export instructions before consulting
numerical answers.

A separate compatibility derivative rendered the saved v9 Year result with v12.
All 206,520 predictions replayed exactly. Its 9,502,169-byte rows report contains
4,499 training and 501 evaluation records. Independent checks verified all 910,000
raw/processed data cells, source identities and 2,004 model prediction cases.
Browser checks covered 1,456 displayed source values and all model switches,
error ordering, tooltips and source links at desktop and mobile widths. Direct
inspection followed the largest exported prediction error into its original
source record and compared forest and booster errors.

The first all-cell checker failed at its final allocation calculation because
its own integer product overflowed. A versioned verification-only correction
promoted that product to double precision, with positive and negative allocation
checks. The original failure and unchanged exported report remain preserved.
This is saved-model compatibility evidence, not a new v12 full-data fit.

Candidate-v13 includes the compact cost table headings, adjacent collapsed
chart disclosures and clearer exported-record status discovered during these
walkthroughs. The complete public gallery was regenerated from saved public
example fits and visually inspected. All eight images passed the existing
framing checks without reducing type or cropping required evidence; all 118
gallery browser checks passed. Its source inventory SHA256 is
`24ca0e1672219f601d2f856028ba0632506608abe56320974c9237bf32f8aeb5`.
Its complete R suite passed 7,538 assertions across 687 tests, with no failures
or warnings and the two opt-in H2O/Gemini skips. Lint and 147 source-record browser
checks passed. The archive check completed with no errors or warnings and one
note for new-submission status and two links that will exist on main after merge.

Independent review then found that the new compact disclosure CSS exposed a
closed heading in print. Candidate-v14 restricts those rules to screen display.
Its only package source difference from v13 is that CSS file; R implementation,
tests and gallery PNGs are identical. HTML was regenerated and the gallery
manifest refreshed. Final v14 source inventory:
`6d7e7f8eea29e8168b3665c6923e46f33ce56d4b35d0d4d4384a58ea5cf4f983`.
The final print fix passed 13 independent browser/PDF checks: closed support
is omitted from print, while expanded support and its table remain printable.
The final archive check and three final-candidate public calls are still required. The superseded v13 follow-up plan was checked but never
authorized or run.

## Remaining gates

- Complete the public call on all three full datasets and compare its results
  with the verified native and applicable published-version references.
- Complete final-source verification of the repaired quota arithmetic, native
  progress and report presentation, including an actual bounded rows export.
- Independently check quality, every retained model's full training population,
  full predictions, cold replay, resource use and the actual large reports.
- Require the integrated source and publication guard tests in final CI.
  Preserve the original frozen harness and private acceptance diagnostics.
- Retain the passing candidate platform/browser evidence and require the
  version-tag workflow's platform, engine, H2O and quality gates to pass.
- Build and check the final release archive, then verify its public download,
  fresh installation and deployed report gallery.

CRAN submission is outside this GitHub release.
