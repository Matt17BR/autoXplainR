# What the harder problems showed

The optional modeling path in 0.6.1 is useful. Its regularized and boosted models
were competitive with the native reference fits on these problems. The core
defaults are a much weaker choice when there are many irrelevant predictors or
more predictors than observations. Completing a call and producing a report did
not mean that every retained model was sensible.

These findings come from the published 0.6.1 archive, not the changing working
tree. Each pair uses identical data, five training folds and 15 configurations.
The optional comparison explicitly requests `regularized`, `forest` and
`boosting`. It is not the larger `recommended` portfolio. Settings were declared
before fitting; no model was chosen using its test score.

## Predictive results

Lower loss is better. The regression loss is RMSE; the binary loss is log loss.
The family in parentheses is the model selected using training folds.

| Problem | Core, 15 configurations | Optional, 15 configurations | Native XGBoost, fixed settings |
| --- | ---: | ---: | ---: |
| Friedman, seed 1001 | 2.634 (neural) | 1.554 (boosting) | 1.473 |
| Friedman, seed 2001 | 2.318 (neural) | 1.481 (boosting) | 1.418 |
| Sparse wide data, seed 1002 | 3.253 (tree) | 1.122 (regularized) | 2.003 |
| Sparse wide data, seed 2002 | 3.296 (tree) | 1.078 (regularized) | 1.923 |
| Rare interaction, seed 1003 | 0.1602 (tree) | 0.1664 (forest) | 0.1710 |
| Rare interaction, seed 2003 | 0.1646 (tree) | 0.1613 (boosting) | 0.1731 |
| Bank Marketing, seed 1004 | 0.2755 (logistic) | 0.2714 (regularized) | 0.2736 |

Native XGBoost appears in every row to provide a consistent reference, not
because it won every problem. Native ranger and lasso results, all retained
package models, and the known synthetic functions are in
[the first run](baseline-first-scores.csv) and
[the second synthetic replicate](baseline-replicate-scores.csv).

The sparse-data results are especially clear. Native cross-validated lasso had
RMSE 1.130 in both replicates. The known mean function had RMSE 1.017 and 1.022
because the outcome includes irreducible noise. The package's regularized fits
were close to those references; its core trees were not.

More complex models did not improve every measure. In the first rare-event
sample, the core tree had lower log loss than the optional forest, while average
precision improved from 0.621 to 0.722. In the second, average precision improved
from 0.584 to 0.682. The event prevalences were about 10%. A constant prediction
can exceed 90% accuracy while identifying no events at a 0.5 cutoff. Probability
quality, ranking and the eventual decision threshold answer different questions.

The bank difference is small. The paired fixed-model bootstrap interval for the
log-loss difference includes zero. Its contact rows may include repeated
customers, so even this conditional interval needs care. This experiment is a
check on mixed-data prediction, not evidence that one model will improve a
bank's next campaign.

## Failures that successful calls concealed

All 14 paired package calls completed. That is a useful operational result, but
it is not sufficient evidence of robustness.

On both sparse-data replicates, six neural configurations failed and the package
correctly continued searching. A retained unregularized linear model then had
test RMSE 110.503 and 53.988, respectively, despite the selected models having
RMSE around 3.3. The first linear model's R-squared was approximately -682.
Rank-deficient predictions were warned about. Retaining this evidence is honest;
presenting the fit as an ordinary usable alternative would not be.

That case also challenges the report: a single exploding comparator can flatten
all useful models on shared axes. Its parameters, failed folds, warning and
model-specific diagnostic views must remain inspectable without making every
other chart unreadable. The original result objects were passed to the report
stress review for this purpose.

The bank fit warned about 26 evaluation records whose values matched training
records after call duration was removed. The sampled source row indices are
disjoint. This warning therefore identifies a real ambiguity rather than proving
that rows were copied between splits. Customer identifiers are not available,
so the experiment does not establish independent-customer generalization.

The benchmark itself also needed correction. Its first JSON output lost numeric
metric names. Those summaries were reconstructed from the unchanged saved
predictions. Editing the active Rscript during that correction caused a syntax
error after the first optional Friedman fit had saved successfully. Repeating
the unchanged condition from frozen scripts exited successfully and produced
exactly identical predictions for all four retained models. The driver now
copies and hashes its R scripts before starting children. These harness errors
are not counted as package modeling failures.

## What this supports

The evidence supports making the route to regularized, forest and boosting
models clear, and giving an actionable warning when a default fit is poorly
suited to the data. It does not support silently changing every user's portfolio
based on whichever optional packages happen to be installed.

The public `recommended` route was checked separately because it also includes
linear, additive and tree models, and uses a 30-configuration budget. On the
first Friedman sample it completed all 30 configurations and retained seven
models, but took 532.5 seconds, almost nine minutes. It selected the same boosted
model, with identical held-out predictions, as the 15-configuration optional
comparison that took 57.7 seconds. Its retained GAM had test RMSE 1.786. The
larger portfolio provided more alternatives, but bought no predictive gain for
the selected model on this case.

On Bank Marketing, `recommended` completed in 293.7 seconds with all 30
configurations and seven models. It again selected `regularized_01`, with log
loss 0.271408 and average precision 0.489502. The 15-configuration optional run
took 58.2 seconds and selected the same configuration; primary predictions
differed by at most 7.5e-12. A retained boosted alternative scored 0.268159 on
the holdout. That result is reported as an alternative, not used to replace the
training-selected primary after the fact.

Both public-portfolio runs finished within the fixed ten-minute process limit.
See [all candidate model scores](candidate-recommended-scores.csv),
[operational outcomes](candidate-recommended-operations.json),
[selected-model comparisons](candidate-recommended-comparison.json), and
[the installed-library and harness hashes](candidate-recommended-provenance.json).

The candidate's sparse-data replay preserved the useful regularized fit: RMSE
1.122410583173817. The maximum change in its held-out predictions was
1.60e-14; the other three retained models had identical predictions. See
[the replay evidence](candidate-sparse-verification.json).

The separate candidate comparison must not be presented as another equal-budget
baseline result. Runtime measurements here use one native thread and include
training resampling. They are single runs on this machine, not a general latency
guarantee.

The useful report questions are concrete: which model was selected before
opening the holdout, whether it beats a simple reference, which fits failed,
which inputs it relies on, and what minority-class mistakes it makes. The
report should make an unstable alternative easy to recognize and inspect.

## Where the GAM time went

The default additive fits were profiled on the first training fold, containing
960 fitting rows, 240 validation rows and 30 numeric inputs. The outer evaluation
sample was not used. Increasing the smooth basis size made the fit much more
expensive on this fold:

| Default configuration | Coefficients | Fit time | Training-fold RMSE |
| --- | ---: | ---: | ---: |
| k = 5, gamma = 1, selection on | 121 | 9.04 s | 1.687910 |
| k = 8, gamma = 1, selection on | 211 | 25.83 s | 1.690590 |
| k = 10, gamma = 1.4, selection on | 271 | 43.21 s | 1.678806 |

An exploratory native `mgcv::bam` fit with fREML and discretization took 1.12 s
for the last configuration, with fold RMSE 1.678795. Its maximum prediction
difference from the package's `mgcv::gam` REML fit was 0.000304. Continuous BAM
with fREML took 2.86 s and gave a similarly close prediction. For k = 5,
discrete BAM took 1.05 s, but the maximum prediction difference was larger,
0.030. These are observations from one fold, not an equivalence guarantee.

This is a promising computational direction. It is deliberately separate from
the validated package changes: no production adapter was switched to BAM, and
no faster-engine score replaced an inconvenient original score. A supported
implementation needs regression and classification checks, categorical inputs,
convergence handling, prediction parity and clear method provenance. The
three-family workflow is usable now while that broader work remains open.

The experiment is reproducible with `profile-additive.R` and `profile-bam.R`.
Its [recorded timings and prediction differences](additive-profile.json) include
all completed configurations. The first native replay needed an explicit mgcv
namespace load so its saved GAM used `predict.gam`; the profiler now does this
before prediction. That setup error was not a failure of the package's adapter.

## Scope and reproducibility

The largest training set here has 5,000 rows. The widest has 240 inputs. These
cases do not establish performance on millions of rows, very high-cardinality
features, images, text or arbitrary distribution shifts. The independent report
and adapter stress checks cover additional failure modes.

All scores are rescored from raw held-out predictions. The benchmark's metric
oracle was checked using hand-computable constant, perfect, reversed and tied
predictions. No performance threshold is used to delete a bad result or turn a
weak fit into a passing test. The two synthetic replicates are descriptive
evidence, not a broad benchmark leaderboard.

See [the protocol](README.md), [data and artifact provenance](baseline-provenance.json),
[first-run operational outcomes](baseline-first-operations.json) and
[replicate operational outcomes](baseline-replicate-operations.json).
Large fitted objects, raw data, process logs and reports are kept under
`~/.cache/autoxplain-stress-0.6.2/`.
