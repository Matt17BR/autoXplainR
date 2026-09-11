# Harder modeling problems

These scripts ask whether the fitted models predict useful signals on fresh rows,
not just whether the API returns an object. They compare the published package
with independent native-engine fits and preserve failures as results.

The first run uses the published 0.6.1 source archive, SHA-256
`d5a5e9097cb1c5d9469cee02a80f020df498026548ce801cb10492d8dbb9ca14`.
Install it into a separate library. Do not load a changing source checkout while
collecting baseline evidence.

| Problem | Training / evaluation | What it challenges |
| --- | ---: | --- |
| Friedman response with extra noise inputs | 1,200 / 800 | Curves, interactions, irrelevant predictors |
| Sparse linear signal | 180 / 600 | More predictors than training rows: 240 inputs, five useful |
| Rare nonlinear event | 2,500 / 1,500 | Useful minority predictions, interactions, probability quality |
| UCI Bank Marketing | 5,000 / 3,000 | Mixed categorical and numeric inputs, imbalance, weak signal |

The Friedman response follows the simulation in Friedman's
[Multivariate Adaptive Regression Splines](https://doi.org/10.1214/aos/1176347963).
The other two synthetic functions are fully specified in `prepare.R`. Their
known conditional means or probabilities provide an oracle reference, not a
model fitted by the package.

The real data are [Bank Marketing, UCI](https://archive.ics.uci.edu/dataset/222/bank+marketing),
by Sérgio Moro, Paulo Rita and Paulo Cortez, DOI
[10.24432/C5K306](https://doi.org/10.24432/C5K306), licensed CC BY 4.0.
The download contains `bank-additional-full.csv`, with 41,188 contacts. The script
removes `duration` because it is not available before a call. Literal `unknown`
categories remain. Source row indices and the download checksum are saved.

The bank experiment measures prediction for randomly sampled contact rows. It
does not estimate future-period performance. Customer identifiers are absent,
so repeated customers across splits cannot be ruled out. The source's
chronological ordering is not used in this particular comparison.

## Comparison rules

Every package variant sees the same training and evaluation rows and the same
five training-only folds. The held-out outcome is never used for model selection,
feature selection, threshold choice or stopping. The package uses its default
one-standard-error selection rule.

* `core` uses the default 15-configuration linear, tree and neural tournament.
* `stronger` spends the same 15 configurations on explicitly requested
  regularized, forest and boosting families.
* `references` fits native XGBoost and ranger models with settings declared in
  `benchmark-plan.json` before fitting. A native lasso reference uses its full
  default lambda path and the same training folds, selecting `lambda.1se`.

The native fits are reasonable reference procedures, not an exhaustive competing
AutoML system. Their preprocessing uses a separate training-derived model matrix.
Their budgets differ from the package's tournament, so their timing is not a
comparison of equally expensive searches.

For ordinary package use, the smaller optional portfolio remains a single call
once its engine packages are installed:

```r
result <- autoxplain(
  data, "outcome",
  learners = c("regularized", "forest", "boosting"),
  report = "model-report.html"
)
```

This was effective on the sparse and nonlinear cases here. It does not replace
choosing a validation design appropriate to the data, and it did not beat the
core tree on every rare-event measure.

`explain = FALSE` isolates fitting and prediction costs. Saved result objects are
available for separate report rendering and browser inspection. No report cost
should be inferred from these fit timings.

All retained models are scored by an independent metric implementation using
`predict(result, raw_evaluation_rows)`. The tables distinguish the model selected
by training resampling from the other retained families. A better test score
among those families does not change the selected model. Classification scores
include log loss, Brier score, ROC AUC, average precision, and precision/recall at
the fixed 0.5 cutoff. Accuracy alone is insufficient for the rare-event cases.

## Run

Run from the repository root. Generated data, fitted models and reports stay in
the cache, outside the repository. The comparisons require `glmnet`, `ranger`
and `xgboost` at the versions accepted by `learner_catalog()`, plus `jsonlite`
and `digest`. The public recommended-portfolio check also requires `mgcv`.

```sh
export AXR_STRESS_DIR="$HOME/.cache/autoxplain-stress-0.6.2/benchmark"
export TMPDIR="$HOME/.cache/autoxplain-stress-0.6.2/tmp"
mkdir -p "$TMPDIR" "$AXR_STRESS_DIR/library-baseline"
R CMD INSTALL --library="$AXR_STRESS_DIR/library-baseline" /path/to/AutoXplainR_0.6.1.tar.gz
Rscript validation/stress-modeling/check-metrics.R
Rscript validation/stress-modeling/prepare.R
python3 validation/stress-modeling/run.py --run baseline \
  --library "$AXR_STRESS_DIR/library-baseline"
Rscript validation/stress-modeling/summarize.R
```

The driver freezes and hashes its R scripts, then runs one R process at a time, limits native thread counts to one,
allows ten minutes per condition and records process exits, timeouts, package
warnings, failed configurations and total elapsed time. It resumes completed
conditions rather than replacing their evidence. Use another `--run` name for a
new package installation or a deliberate rerun. Keep the original cases and
comparison plan when testing a fix.

A second synthetic replicate uses `AXR_STRESS_SEED_OFFSET=1000` in a fresh
`AXR_STRESS_DIR`, then runs the same comparisons with
`--scenarios friedman_noise sparse_wide rare_interaction`. This generates new
training and evaluation observations with unchanged functions, dimensions and
settings. Its bank sample is prepared but not run. Both synthetic replicates
are retained, including cases where the more complex portfolio loses.

The actual public portfolio is a separate comparison on the fixed first
Friedman and Bank samples. After installing the candidate into another library:

```sh
python3 validation/stress-modeling/run.py --run candidate-recommended \
  --library /path/to/candidate-library --scenarios friedman_noise bank_marketing \
  --variants recommended
AXR_STRESS_RUN=candidate-recommended Rscript validation/stress-modeling/summarize.R
```

It uses the public `portfolio = "recommended"` default budget of 30
configurations and the same five folds. The six families include additive,
linear and tree models alongside regularized, forest and boosting models.
Its increased search budget is stated separately from the 15-versus-15 test.

`summarize.R` rescans every stored prediction, verifies it against the recorded
score, and writes all models to `scores.csv`. Its paired evaluation-row bootstrap
compares the two selected fits. Those intervals condition on the fitted models;
they do not include training or model-selection variability, and independent
contacts cannot be assumed for the bank sample.

Do not edit a running R script: Rscript can parse later expressions from the
file while earlier expressions are executing. A harness edit during the first
Friedman stronger run caused a syntax error after its fit and predictions were
saved. The evidence records this separately from package failures; the same
condition is repeated from an unchanged script.

These are reproducible challenge cases, not a leaderboard or a claim that the
package handles every data domain. They do not cover images, text, censoring,
survival outcomes, distributed data, or every form of distribution shift.
