# Validation evidence

The [scalability work](scalability/README.md) separates recommended-search cost,
report storage and browser behavior, and fits with a million training rows.
Each measurement identifies its source and workload; the work record states
which release gates remain open.

The [harder-modeling comparison](stress-modeling/findings.md) tests predictive
quality against independently fitted reference models. The accompanying
[adapter probes](stress-adapters/README.md),
[grouped-fold checks](stress-grouped/README.md) and
[report stress review](stress-reports/README.md) record failures, repairs and
remaining limits. These challenge behavior beyond the smaller teaching examples.

The [product review of the 0.4.0 report](product-review-0.4.0.md) records the
maintainer’s feedback and the regressions from 0.1. Current acceptance tasks are
in the [product walkthrough](product-walkthrough.md) and the 0.6.0 work record.
The [critical 0.3.0 audit](audit-0.3.0.md) records reproduced correctness bugs,
report-design findings and repair acceptance criteria. Its counterexamples
complement the passing release checks below; test totals alone did not cover
these cases. The [0.4.0 repair disposition](resolution-0.4.0.md) tracks every
finding, its acceptance checks and the remaining limits.

The [0.5.0 test-quality review](test-quality-0.5.0.md) records tests that passed
despite wrong behavior, their replacements and the required negative controls.

The [0.6.0 overhaul record](product-overhaul-0.6.0.md) tracks component audits,
rejected report designs and new data, selection, prediction and existing-model
workflows. Its acceptance remains tied to the tested files and source revision.

Run these commands from the repository root. They write small reviewable
artifacts under `validation/results/`. They do not send data to a service.

```sh
Rscript -e 'devtools::test(stop_on_failure = TRUE)'
Rscript validation/run-reference.R
Rscript validation/run-simulation.R
AUTOXPLAIN_RUN_H2O=true Rscript -e 'devtools::test(filter = "h2o", stop_on_failure = TRUE)'
bash .github/scripts/build-check-source.sh /tmp/autoxplain-release /tmp/autoxplain-check
```

The reference script additionally requires `iml`. Its executed comparisons cover
four PDP cases on identical grids and rows, four ALE shapes anchored at the
minimum (the packages use different centering conventions), and three analytic
ALE boundary/empirical-centering cases, including irregular spacing and ties.
The largest absolute error across these 11 comparisons was 5.69e-14; see
[`reference-agreement.csv`](results/reference-agreement.csv) for individual results.
The unit suite separately checks null reliance, permutation draws and pairwise
tied-score AUC. Agreement in these cases does not establish that either package
is preferable.

The [executed workflow comparison](workflow-comparison.md) applies AutoXplainR
and DALEX + modelStudio to identical fitted models and held-out rows, records
numerical agreement, and checks selected HTML controls offline. Immediate
report walkthroughs and task checks are part of release acceptance now. The
separate [participant-study protocol](user-study-protocol.md) has not been
executed; no comparative participant result or superiority is claimed.

The simulation records every replicate of a fixed-model percentile bootstrap
coverage diagnostic. The data-generating distribution gives an analytic
conditional RMSE for each fitted line. Empirical coverage, Monte Carlo standard
error and interval width are reported for two evaluation sizes. There are only
100 replicates per setting and 300 bootstrap draws per replicate: these are
small validation experiments, not a comprehensive inferential study. Undercoverage
is a finding, not a reason to adjust the seed or hide results.

The main suite tests all installed optional engines. A skipped optional test is
not a passed integration. Hosted Gemini requires a separate explicit live test;
its mocked transport tests do not prove current endpoint availability. H2O tests
start an isolated local Java cluster and shut it down after testing.

Published 0.5.0 evidence is in `release-0.5.0.md`. New release evidence must record the tested commit, runtime,
package versions, check status and skipped tests. Do not reuse an older release's
CRAN or Win-builder evidence for a new archive. Checksums identify an artifact;
they do not make it CRAN-approved.

## Report screenshots

The README images are browser captures of `pkgdown/assets/model-report.html`,
generated from public synthetic data by the current R implementation. Refresh
the report and all gallery images together when the report changes:

```sh
Rscript validation/render-explorer-cases.R
python3 -m venv /tmp/autoxplain-screenshots
/tmp/autoxplain-screenshots/bin/pip install playwright==1.58.0
/tmp/autoxplain-screenshots/bin/playwright install chromium
/tmp/autoxplain-screenshots/bin/python validation/capture-screenshots.py \
  --qa-dir /tmp/autoxplain-gallery-review
```

After reviewing all three public reports and the refreshed images, explicitly run
`python validation/check-gallery.py --record` to update `gallery-manifest.json`.
Normal mode checks the exact source and asset hashes; `--browser` also opens the
committed reports and tests tabs, model selection and exported-record links.
The gallery guard also rejects undersized or tall desktop screenshots, including
when recording a new manifest. This landscape-shape check is a framing heuristic;
it cannot establish legibility or usefulness.
CI runs this guard before regenerating reports, so fresh temporary output cannot
conceal a stale published gallery. Never record the manifest automatically in CI.

Alternatively, set `CHROME_PATH` to an installed Chrome executable and omit the
browser download. The capture script uses a real 1280px desktop viewport at 1.5x
resolution. Each image ends after a complete task, with a maximum viewport
height of 1080px. Model selection is scrolled to the family comparison, chosen
search and fold-score plot. The other views retain the report header and their
main controls. Font sizes and report styles are unchanged during capture.

It stages all eight images before replacing files in `man/figures/`, checking
that required controls and evidence fit, text is not cut at the image edges,
the navigation rail spans the viewport and the browser reports no runtime
errors. It also checks page overflow at 390px. `--output-dir` can save a draft
gallery elsewhere for review.

The QA directory contains complete-page captures, the actual PNGs displayed at
a typical GitHub README width of 896px, and `capture-geometry.json`. Review the
896px previews for legibility and the complete pages for evidence the preview
does not show. A well-framed screenshot does not establish that the rest of the
report is usable. The model-comparison image is retained as a compatibility
alias of the opening image; the README embeds it only once. Run
`Rscript validation/render-explorer-cases.R` to refresh the public binary and
multiclass demonstrations together with the regression example.

The report browser workflow also generates messy-data and supplied-model cases.
The data, prediction and selection gates use independent source tables or hand
calculations. Their deliberately corrupted counts, record keys, controls and
plot values must fail the intended assertions without relying on runtime errors.
Review the images visually before committing; the script does not replace
checking text legibility and framing. These Python dependencies are only needed
to refresh screenshots, not to install or use the R package.


## Browser regression gate

The [product walkthrough](product-walkthrough.md) is the release acceptance
process. Browser assertions support it; they do not replace working through the
novice and experienced-user journeys and recording useful answers.

`report-browser` generates regression, binary, multiclass, quick, messy-data and
supplied-model reports, plus a deliberately constructed selection fixture. It
checks controls and displayed answers against original data or independent
calculations, including source rows, missingness, cutoff decisions and plotted
coordinates. Keyboard, narrow-layout, offline and selected-view print checks
cover the tested states.

Before regenerating fixtures, `check-layout-tasks.py` also exercises the committed
public reports. It checks whether a phone viewport reaches useful distribution
evidence, opens data disclosures from the keyboard, and prints every report tab.
Printed chart captions must stay with their axes, Methods must retain model
summaries, and interval and distribution assumptions must survive printing.
Importance values must fit their rows at A4's printable width. A failed-family
warning must open and focus the recorded fold errors in the Selection tab; its
error text must remain readable within the horizontally scrolling table.
The gate rejects a trailing page whose text occupies less than one quarter of
the page height; its PDFs still need visual review. Injected excessive setup
spacing, overflowing values and an actual printed orphan title must fail the
relevant task checks. PDF captions are reconstructed from neighboring lines
within their column, rather than loosely matching words elsewhere on the page.

```sh
python validation/check-layout-tasks.py --output-dir /tmp/report-layout-tasks
```

```sh
Rscript validation/render-explorer-cases.R
Rscript validation/render-exploration-fixtures.R
Rscript validation/render-supplied-models.R
Rscript validation/render-selection-fixture.R
Rscript validation/render-cutoff-fixture.R
Rscript validation/render-cutoff-cases.R
python validation/check-explorer.py \
  --axe-path /path/to/axe-core/axe.min.js --output-dir /tmp/report-browser
python validation/check-data-explorer.py \
  --case-dir /tmp/autoxplain-explorer-cases --output-dir /tmp/report-browser/data \
  --axe-path /path/to/axe-core/axe.min.js
python validation/check-predictions.py \
  --case-dir /tmp/autoxplain-explorer-cases --output-dir /tmp/report-browser/predictions \
  --axe-path /path/to/axe-core/axe.min.js
python validation/check-selection.py --output-dir /tmp/report-browser/selection
python validation/check-supplied-models.py \
  --case-dir /tmp/autoxplain-explorer-cases --output-dir /tmp/report-browser/supplied-models
python validation/check-cutoff-cases.py \
  --case-dir /tmp/autoxplain-explorer-cases --output-dir /tmp/report-browser/cutoff-cases
```

See [the check specification](report-browser.md) for dependencies and scope.
The release workflow requires this gate, numerical tests and exact engine-minimum
checks. Passing automation is only part of acceptance: inspect the actual views
and complete the tasks in the product review before publication.
