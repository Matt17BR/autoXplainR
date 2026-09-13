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
The final intended acceptance installation is `candidate-v7`, inventory SHA-256
`3a1c080bf67f906d29713431d1f4a9fc1a52406c6bbc82fb3a63380b3546413e`.
Only README wording and two overview screenshot copies differ between these
snapshots. R code, report assets, tests, help files and DESCRIPTION are identical.

| Check | Result | Scope |
| --- | --- | --- |
| Complete R tests | 7,233 assertions passed; no failures or warnings | Two opt-in integrations skipped: live H2O and Gemini |
| Source build and CRAN-style check | No errors or warnings; one note | Tests ran separately; manuals, examples and five vignette scripts passed |
| Documentation, lint and spelling | Passed | Generated help is current |
| Report exploration | 1,382 checks passed; no browser errors | Four example types in Chromium; unresolved automated contrast items require separate review |
| Committed gallery | 118 smoke checks passed | All three HTML reports and eight screenshots regenerated and visually inspected |
| Adaptive selection UI | Chromium 608 and WebKit 596 checks passed | Independent selection fixture, keyboard, deep links, mobile charts and print; Firefox remains a CI gate |

The candidate archive checked locally has SHA-256
`c899d2cf0f51403e2650452f3d204769f2e417b611673c9de52e66b4e3211f29`
and contains 2,444,140 bytes. It is a candidate-v6 archive, not a published
artifact. The note covers new-submission status and two benchmark links whose
files have not reached the public main branch. Those links must be checked again
after publication of the documentation.

Browser tasks and manual inspection are implementer walkthroughs, not a
recruited-participant usability study. Counts describe individual runners and
must not be added as unique tests. Shared-host timings are observations, not
isolated speed measurements.

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
