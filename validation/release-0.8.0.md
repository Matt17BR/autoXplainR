# AutoXplainR 0.8.0 validation record

Status: candidate under review. No 0.8.0 tag or release has been published.
Full-data acceptance and platform CI remain release gates.

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

## Completed local checks

The tested source snapshot is `candidate-v6`, inventory SHA-256
`afa4e63bfa1506cdce8e51d1b0271aa844f02a868d20123df6b3b07710e438ed`.
The current frozen installation is `candidate-v9`, inventory SHA-256
`8f8ba04c4560c0274861cfb8559703eb5b5123f6ce311ec05bfb9d77f1044b5f`.
Changes after candidate-v6 comprise README wording, two overview screenshot
copies, two test files, the summary-report help described below and its NEWS
entry. Fitting and statistical code, report styles/scripts, help files and
DESCRIPTION are unchanged.
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
| Adaptive selection UI | Chromium 608 and WebKit 596 checks passed | Independent selection fixture, keyboard, deep links, mobile charts and print; Firefox remains a CI gate |
| Fold-table layout and print | 131 checks passed | Named fold table, complete errors, mobile scrolling and an inspected PDF page |

The current candidate archive checked locally has SHA-256
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

The first CI round passed current and minimum native engines, numerical
references, lint, and 93.12% statement coverage. The separate
[live H2O run](https://github.com/Matt17BR/autoXplainR/actions/runs/34764504254)
passed on commit `7d0bc1edf604aaea5b1f6f002908ede3e570d3c6`.
The next round passed all seven R-check jobs and all three native-engine jobs,
plus lint, statistics and coverage. Its Firefox chart check failed on text
rectangle overlaps; the screenshots show separate readable labels. The
[CI review](competitive-tabular/ci-review-20260913.md) records that diagnosis
and the pending cross-browser verification. No package code changed in response
to those CI failures.

## Remaining gates

- Finish and verify the full native references, including references matching
  the public call's automatically resolved thread count.
- Freeze the candidate, protocol and completed reference artifacts before
  reading locked evaluation outcomes. Complete the public call on all three
  full datasets and the applicable published-version comparison.
- Independently check quality, every retained model's full training population,
  full predictions, cold replay, resource use and the actual large reports.
- Complete Windows, macOS, Linux, supported R versions, minimum native engines,
  live H2O, coverage, statistical references and all configured browser gates.
- Build and check the final release archive, then verify its public download,
  fresh installation and deployed report gallery.

CRAN submission is outside this GitHub release.
