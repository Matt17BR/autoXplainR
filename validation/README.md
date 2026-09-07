# Validation evidence

The [product review of the 0.4.0 report](product-review-0.4.0.md) records the
maintainer’s feedback, the regressions from 0.1 and current task acceptance criteria.
The [critical 0.3.0 audit](audit-0.3.0.md) records reproduced correctness bugs,
report-design findings and repair acceptance criteria. Its counterexamples
complement the passing release checks below; test totals alone did not cover
these cases. The [0.4.0 repair disposition](resolution-0.4.0.md) tracks every
finding, its acceptance checks and the remaining limits.

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

Current release evidence belongs in `release-0.5.0.md`; record the tested commit, runtime,
package versions, check status and skipped tests. Do not reuse an older release's
CRAN or Win-builder evidence for a new archive. Checksums identify an artifact;
they do not make it CRAN-approved.

## Report screenshots

The README images are browser captures of `pkgdown/assets/model-report.html`,
generated from public synthetic data by the current R implementation. Refresh
the report and all gallery images together when the report changes:

```sh
Rscript validation/render-example.R
python3 -m venv /tmp/autoxplain-screenshots
/tmp/autoxplain-screenshots/bin/pip install playwright==1.58.0
/tmp/autoxplain-screenshots/bin/playwright install chromium
/tmp/autoxplain-screenshots/bin/python validation/capture-screenshots.py
```

Alternatively, set `CHROME_PATH` to an installed Chrome executable and omit the
browser download. The capture script uses a 1440px desktop viewport at 1.5x
resolution and checks for page overflow at 390px. It saves the model comparison, fitted patterns, relationships, predictions
and checks tabs to `man/figures/`.
It also captures the fitted-model details view. Run
`Rscript validation/render-explorer-cases.R` to refresh the public binary and
multiclass demonstrations together with the regression example.
Review the images visually before committing; the script does not replace
checking text legibility and framing. These Python dependencies are only needed
to refresh screenshots, not to install or use the R package.


## Browser regression gate

The [product walkthrough](product-walkthrough.md) is the release acceptance
process. Browser assertions support it; they do not replace working through the
novice and experienced-user journeys and recording useful answers.

`report-browser` generates four actual reports and compares their controls and
displayed answers with R results. It checks model, metric and feature switching;
relationship inspection; prediction commands; keyboard and tooltip interaction;
narrow layouts; offline rendering; and printing the selected view.

```sh
Rscript validation/render-explorer-cases.R
python validation/check-explorer.py \
  --axe-path /path/to/axe-core/axe.min.js --output-dir /tmp/report-browser
```

See [the check specification](report-browser.md) for dependencies and scope.
The release workflow requires this gate, numerical tests and exact engine-minimum
checks. Passing automation is only part of acceptance: inspect the actual views
and complete the tasks in the product review before publication.
