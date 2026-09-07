# Validation evidence

Run these commands from the repository root. They write small reviewable
artifacts under `validation/results/`. They do not send data to a service.

```sh
Rscript -e 'devtools::test(stop_on_failure = TRUE)'
Rscript validation/run-reference.R
Rscript validation/run-simulation.R
AUTOXPLAIN_RUN_H2O=true Rscript -e 'devtools::test(filter = "h2o", stop_on_failure = TRUE)'
bash .github/scripts/build-check-source.sh /tmp/autoxplain-release /tmp/autoxplain-check
```

The reference script additionally requires `iml`. It compares PDP values on an
identical grid and identical rows for tree fits to additive, interaction,
nonlinear and correlated-feature data. The unit suite separately checks analytic
additive ALE/PDP, null reliance, permutation draws and pairwise tied-score AUC.
Agreement in these cases does not establish that either package is preferable.

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

Release evidence belongs in `release-0.3.0.md`; record the tested commit, runtime,
package versions, check status and skipped tests. Do not reuse an older release's
CRAN or Win-builder evidence for a new archive. Checksums identify an artifact;
they do not make it CRAN-approved.
