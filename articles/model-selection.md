# Compare and tune models

The default, `model_set = "tuned", portfolio = "core"`, schedules 15
settings across linear, tree and neural families using five
training-only folds. It retains a representative from each successful
family and a baseline for the report. The selected model is evaluated on
rows kept outside that search.

`model_set = "quick"` fits a pre-specified linear, logistic or
multinomial model and a baseline. `"comparison"` adds two pre-specified
trees but keeps the same primary model. Its evaluation ranks are
descriptive. Use `"tuned"` when the analysis calls for model selection:
candidates are compared using training-only cross-validation before the
selected model is scored on the outer evaluation set.

## Start with regularized models, forests and XGBoost

The unreleased 0.8.0 candidate adds `portfolio = "tabular"` for this
combination. It requires R 4.3 or newer for the supported XGBoost
backend; the core package supports R 4.1 or newer. Install the three
optional engines once, then read your labeled CSV and name its outcome
column:

``` r

install_model_engines("tabular")
training <- read.csv("train.csv", check.names = FALSE)
result <- autoxplain(training, "outcome", portfolio = "tabular", report = "report.html")
```

Replace `"outcome"` with the target column’s name. Before fitting,
identify its task: numeric outcomes with exactly two values select
binary classification; other numeric outcomes select regression. Numeric
category codes with three or more values, such as `0, 1, 2`, need
`task = "multiclass"` or conversion to a factor. Use
`task = "regression"` for a numeric two-value regression outcome. For a
binary classification example, set
`training$outcome <- factor(training$outcome, levels = c("no", "yes"))`
before fitting to make `"yes"` the probability target.

`test_data` must contain observed outcomes. Keep a competition’s
unlabeled test CSV for prediction after fitting:

``` r

competition_test <- read.csv("test.csv", check.names = FALSE)
predictions <- predict(result, competition_test)
```

The output is a numeric vector for regression, a vector of
positive-class probabilities for binary classification, or a matrix with
one probability column per multiclass outcome.
`predict(result, competition_test, type = "class")` returns class labels
for classification.

This starts with 18 candidate settings. On at least 200 outer-training
rows, it screens them on a shared sample and takes the best successful
setting from each family into five-fold cross-validation. It selects the
best CV finalist, refits the retained settings on all training rows and
evaluates them on a separate holdout. The report includes those models
and an intercept-only baseline. Regression, binary classification and
multiclass classification use the same call.

Small datasets skip screening and cross-validate each scheduled preset.
The [0.8.0 validation
record](https://github.com/Matt17BR/autoXplainR/blob/main/validation/release-0.8.0.md)
reports completed Covertype, Bank and YearPrediction comparisons,
including forest quality tradeoffs, measured costs and remaining report
and release checks. These results do not establish competition-level
performance. Inspect the fitted results before deciding what additional
modeling work the data need.

## Choose a portfolio

``` r

as.data.frame(learner_catalog())[, c("family", "backend", "supported_tasks", "portfolios")]
#>         family    backend                supported_tasks
#> 1       linear stats/nnet regression, binary, multiclass
#> 2  regularized     glmnet regression, binary, multiclass
#> 3     additive       mgcv             regression, binary
#> 4         tree      rpart regression, binary, multiclass
#> 5       forest     ranger regression, binary, multiclass
#> 6     boosting    xgboost regression, binary, multiclass
#> 7       neural       nnet regression, binary, multiclass
#> 8       kernel      e1071 regression, binary, multiclass
#> 9    neighbors       kknn regression, binary, multiclass
#> 10        mars      earth             regression, binary
#>                        portfolios
#> 1     core, recommended, extended
#> 2  recommended, extended, tabular
#> 3           recommended, extended
#> 4     core, recommended, extended
#> 5  recommended, extended, tabular
#> 6  recommended, extended, tabular
#> 7                  core, extended
#> 8                        extended
#> 9                        extended
#> 10                       extended
```

| Portfolio | Included families | Dependencies |
|----|----|----|
| `core` | Linear, tree, neural | Ordinary package dependencies |
| `tabular` | Regularized, forest, boosting | `glmnet`, `ranger`, `xgboost` |
| `recommended` | Linear, regularized, additive, tree, forest, boosting | Optional engines installed explicitly |
| `extended` | All ten, including kernel, nearest-neighbor and MARS | Additional optional engines |

Additive and MARS learners support regression and binary tasks. The
requested portfolio does not change silently with installed packages; a
missing engine produces an installation instruction.

``` r

tuned <- autoxplain(iris, "Species", model_set = "tuned", portfolio = "core",
                    max_models = 6, nfolds = 3, seed = 2026)
tuning <- tuning_results(tuned)
tuning$candidates[, c("model", "hyperparameters", "cv_score", "cv_se", "selected")]
#>                             model
#> 1                  neural network
#> 2                  neural network
#> 3                   decision tree
#> 4                   decision tree
#> 5                   decision tree
#> 6 multinomial logistic regression
#>                                                 hyperparameters   cv_score
#> 1 hidden units = 2, weight decay = 0.03, iteration limit = 2000 0.09445857
#> 2  hidden units = 1, weight decay = 0.1, iteration limit = 2000 0.29456937
#> 3         max depth = 6, pruning cp = 0.003, minimum split = 10 2.37836688
#> 4          max depth = 2, pruning cp = 0.03, minimum split = 24 2.39466788
#> 5          max depth = 4, pruning cp = 0.01, minimum split = 14 2.39466788
#> 6                                       default statistical fit         NA
#>        cv_se selected
#> 1 0.03609199     TRUE
#> 2 0.01072168    FALSE
#> 3 1.40932486    FALSE
#> 4 1.40612488    FALSE
#> 5 1.40612488    FALSE
#> 6         NA    FALSE
```

Every fold learns its preprocessing from that fold’s training rows. The
core portfolio’s default one-standard-error rule favors the first
eligible family in the documented priority, then its smallest recorded
within-family capacity proxy. Some tuning dimensions are not ordered by
that proxy; for example, a neural weight-count proxy does not order
weight decay. Cross-family priority is a package policy, not a
statistical ordering of model families. Fold-score standard errors are a
selection heuristic, not independent-test confidence intervals. Use
`tuning_rule = "best"` for the best resampled score instead. This is
already the default for `portfolio = "tabular"` when `learners` and
`tuning_rule` are not supplied. An explicit `learners` list retains the
usual one-standard-error default; choose `tuning_rule = "best"` when
that is the intended policy.

``` r

evidence <- tuning_evidence(tuned)
evidence$selection[c("best_configuration", "selected_configuration", "threshold")]
#> $best_configuration
#> [1] "neural_02"
#> 
#> $selected_configuration
#> [1] "neural_02"
#> 
#> $threshold
#> [1] 0.1305506
```

The report’s **Model selection** view connects those decisions to the
parameter values, effective fold settings and retained fit. The default
grid is a small, versioned set of contrasting controls. It does not
follow a theorem that makes those numbers optimal for your data. A
winning setting on the edge of a searched range is a reason to
investigate that range, not evidence that it should always be extended.
The range includes failed attempts: being inside it does not imply that
neighboring settings produced usable fits. Inspect the successful count
and failure reasons. Candidate comparisons stay inside training data.

Explicit optimizer nonconvergence excludes a configuration by default.
Fold and refit records retain the status and reason.
`optimization_policy = "warn"` in
[`tuning_control()`](https://matt17br.github.io/autoXplainR/reference/tuning_control.md)
deliberately keeps such fits with a warning; an unreported optimizer
status remains unknown. Successful family representatives use their best
valid within-family CV score, while the primary follows the configured
global selection rule. These are different choices.

``` r

install_model_engines("recommended")
result <- autoxplain(my_data, "outcome", model_set = "tuned", portfolio = "recommended")
```

[`tuning_control()`](https://matt17br.github.io/autoXplainR/reference/tuning_control.md)
accepts explicit parameter grids, fold IDs, selection metrics and
failure policies. Inspect its help and
[`tuning_results()`](https://matt17br.github.io/autoXplainR/reference/tuning_results.md)
before changing these defaults. Chronological tuning is not implemented;
temporal model selection requires an explicit rolling-origin design
outside this workflow.

## How the tabular search spends its budget

The first stage compares varied settings on one common
training/validation sample of at most 20,000 outer-training rows, with
up to 128 forest trees and 600 boosting rounds. The best successful
setting in each family receives complete cross-validation. Large
adaptive searches, with at least one million training rows times
predictors, use 256 trees per forest validation fit. At four million
rows times predictors, they use 128. The final all-training forest has
256 trees in that largest tier and 500 below it. Smaller searches retain
500 trees for both CV and the final fit. Every requested fold and all of
its training rows are retained; the smaller validation forests’ scores
approximate the final forest’s performance. These tree counts are
computation defaults, not convergence tests. Boosting allows up to 2,000
rounds and stops after 30 rounds without improvement on its inner score.
Screening scores are shown separately because they describe a different
workload.

`search = "auto"` also chooses this adaptive path for an explicit
combination of `regularized`, `forest` and `boosting` that includes a
forest or booster, has at least 200 outer-training rows and has a budget
of at least two settings per family. Custom grids or exact family
budgets use `search = "grid"`. Choose that mode explicitly to give every
scheduled configuration full CV.

Boosting chooses its round count on an approximately 80/20 split made
inside each fitting fold, before preprocessing. The inner training rows
alone learn that split’s recipe. After stopping, the model is refitted
on the whole fitting fold for scoring. The final model uses all
outer-training rows and the rounded-up median of the successful folds’
round counts. If an inner split is skipped, the fit keeps the requested
round budget and records the reason. The final evaluation rows never
select rounds.

Screening uses training outcomes to choose which settings reach full CV.
Those CV scores are therefore **selection evidence conditional on
screening**, not an unbiased accuracy estimate for the whole search
procedure. Keep a final evaluation set separate before choosing the
search. Repeated decisions based on its scores turn it into development
data.

Supply `test_data` when you have a separate labeled evaluation set;
otherwise the default holdout comes from the labeled input table.

``` r

result <- autoxplain(
  training, "outcome", test_data = final_test, evaluation_role = "test",
  portfolio = "tabular", explain = FALSE, seed = 2026,
  tuning_control = tuning_control(threads = 4, finalists_per_family = 2)
)
search <- tuning_results(result)
search$screening$promotion
search$candidates
result$leaderboard
```

This example takes up to two successful settings per family into full
CV, instead of the default one. It explicitly requests four CPU threads
in each native forest or boosting fit. Automatic adaptive searches with
at least one million training rows times predictors use up to four
available cores; smaller and exact searches use one. The automatic
allocation respects process and job limits through
[`parallelly::availableCores()`](https://parallelly.futureverse.org/reference/availableCores.html).
An explicit count overrides it. Candidate fits still run sequentially,
and replay records the resolved count. Increasing finalists spends more
time checking the screening decision. Increasing `max_models` instead
proposes more settings to screen.

Tuned forest and boosting workflows show their current stage, setting
and fold automatically from 200 input rows. During longer importance
calculations, updates count successful shuffles at most once every 30
seconds. An active native fit or prediction finishes before an update
can appear. Set `verbosity = "quiet"` to silence progress or
`verbosity = "info"` to enable it on smaller inputs.

Use `tuning_control(time_limit = 600)` to stop scheduling additional
search work after ten minutes. The initial screening round and the first
usable candidate are allowed to complete, and an active fit is not
interrupted. Final refits, explanations and report generation are
outside this budget. The record distinguishes a failed fit, a
screened-out setting and a setting skipped because the budget was used.
`time_limit` is a scheduling control, not a hard timeout.

The report’s **Model selection** tab shows promotion decisions, actual
settings, fold scores, round choices and failure reasons. In R, inspect
`search$screening`, `search$fold_scores` and `search$refit`. Settings
cover plausible alternatives; they are not an exhaustive search or a
guarantee that the best model was found. The [current comparison
protocol](https://github.com/Matt17BR/autoXplainR/blob/main/validation/competitive-tabular/README.md)
tests the development workflow against native forests and XGBoost on
separate development and acceptance data.

## Choose the score that matches the problem

Set the metric before fitting. It governs candidate selection and, when
enabled, the stopping decision for boosting. It also becomes the main
held-out report score.

| Task | `tuning_control(metric = ...)` | Interpretation |
|----|----|----|
| Regression | `"rmse"` (default), `"mae"` | Prediction error in the outcome’s units; lower is better |
| Nonnegative regression | `"rmsle"` | Root mean squared error after [`log1p()`](https://rdrr.io/r/base/Log.html); lower is better |
| Binary or multiclass | `"log_loss"` (default), `"brier"` | Probability error; lower is better |
| Binary | `"auc"` | Positive-negative ranking, with half credit for ties; higher is better |

``` r

result <- autoxplain(training, "outcome", portfolio = "tabular",
                     tuning_control = tuning_control(metric = "auc"))
```

Binary probabilities refer to the second outcome factor level. Set the
levels explicitly, for example
`factor(outcome, levels = c("no", "yes"))`. AUC is recorded as
`roc_auc`. Its CV score averages within-fold AUC, weighted by each
fold’s evaluated row count. It does not rank predictions from different
fold models together. AUC has no additive per-row loss, so its
out-of-fold `case_loss` is `NA`; probabilities and class labels remain
available. The fold SE is only a selection heuristic, not a confidence
interval for AUC.

RMSLE requires nonnegative outcomes and predictions. It does not
transform the training target or clip negative predictions. A candidate
with a negative validation prediction cannot receive a valid RMSLE
score. If a shuffle produces a negative prediction, that feature’s
importance is unavailable and the report explains why; the other
features can still be inspected.

Failed configurations remain in the search record. A completed model can
still be unsuitable: compare its held-out score with the baseline and
inspect its mistakes. AUC assesses ranking; log loss and calibration
assess probability quality. A good result on one does not imply a good
result on the others.

## Budget explanations and the report separately

Fitting, explaining and exporting are different costs. `explain = FALSE`
lets you inspect the search first. Request a small report explicitly,
then increase its explanation budget when the fitted models deserve
closer investigation:

``` r

render_model_report(result, "first-look.html", top_features = 3, n_repeats = 5,
                    explanation_rows = 2000, report_data = "summary",
                    uncertainty = FALSE)
```

Even a small feature audit first screens inputs; expensive prediction
functions and hundreds of columns can take time. Five permutations are a
first look, not precise importance estimates. The report records the
budget and warns about unstable Monte Carlo estimates.

This first report also omits paired score intervals. By default, those
intervals use 1,000 bootstrap resamples of the complete evaluation set,
independently of the explanation and export limits.
`uncertainty = FALSE` skips that calculation; model scores still use the
complete evaluation set. Omit this argument to include the intervals
when the validation design supports them.

Rendering returns the HTML path and leaves `result` unchanged. If the
result was fitted with `explain = FALSE`, repeat the explanation
settings in later renders: the first report does not save its evidence
or budget back into `result`. Omitting those settings computes default
explanations unless the result already contains retained explanations.

## Larger data

The rows used to fit and score models are distinct from the rows used to
explain them or included in HTML.
[`autoxplain()`](https://matt17br.github.io/autoXplainR/reference/autoxplain.md)
fits on the complete processed training partition and scores the
complete processed evaluation partition. Rows removed by an explicit
preprocessing rule remain recorded as omissions.

The report’s default importance and effect calculations use at most
5,000 evaluation rows. Pairwise plots and associations use at most
10,000 rows per partition. Unfiltered distributions and missing counts
use all available rows. Filtered charts and their counts describe only
matching exported records; sidebar counts keep the full population.
Official model scores always use the complete evaluation partition and
are unchanged by filters. Each sampled view gives its own denominator. A
rare class or small cluster can be absent from a uniform sample even
when its full count is visible elsewhere. Increasing permutation repeats
does not resolve that absence, and shuffle intervals do not measure
row-sampling uncertainty.

``` r

result <- autoxplain(
  training, "outcome", test_data = final_test,
  learners = c("regularized", "boosting"), max_models = 12, nfolds = 3,
  tuning_control = tuning_control(retain_oof = FALSE),
  explanation_rows = 5000, report = "model-report.html"
)

# Include a bounded sample for record inspection and browser filters.
render_model_report(
  result, "records.html",
  report_data = report_data_control(mode = "rows", max_rows = 20000, max_pair_rows = 10000)
)
```

Choose the search and validation design before looking at the final test
scores. The example budget is a starting point, not a claim that twelve
settings are enough for a difficult problem. `retain_oof = FALSE` saves
memory by omitting case-level CV predictions; fold scores, candidate
settings and selection remain available. Use the default `TRUE` when you
need to inspect individual CV errors. This control does not reduce
fitting rows or final evaluation rows.

The 0.7.0 million-row regression control used a smaller fixed search:
one regularized and one boosting configuration, two CV folds and
`retain_oof = FALSE`. One call fitted all million training rows, scored
all 20,000 independent evaluation rows and wrote a 3,891,782-byte
summary report in 81.055 seconds. Default explanations used 5,000 rows;
all 1,000 paired bootstrap draws used the full evaluation set. The
shallow selected model had RMSE 1.6835. A separate three-configuration
search selected 600 boosting rounds at depth 6 and reached RMSE 0.7491
in 109.012 seconds with explanations disabled. These single-host results
measure different workloads; see the
[measurements](https://github.com/Matt17BR/autoXplainR/blob/main/validation/scalability/findings.md).

Large result checks write complete model and data state to a temporary
file, then remove it. The temporary file system needs space for that
serialization. If you need disk-backed temporary storage, set `TMPDIR`
to an existing, writable directory on disk before starting R. Changing
it after R starts does not move the session’s temporary directory.

Neural configurations now allow up to 2,000 optimizer iterations, and
stop earlier when they converge. The former 500-iteration ceiling
excluded wider networks on the nonlinear stress problem even when more
iterations produced useful fits. `maxit` is an explicit neural grid
setting, so you can retain the former budget or choose another
allowance. Inspect convergence and failed configurations alongside
scores. A larger allowance can improve model selection but also
increases fitting time; it does not guarantee convergence.

Use `explanation_rows = NULL` to remove the report explanation cap and
`report_data_control(max_pair_rows = NULL)` for all-row pair summaries.
The lower-level
[`calculate_permutation_importance()`](https://matt17br.github.io/autoXplainR/reference/calculate_permutation_importance.md),
[`audit_explanations()`](https://matt17br.github.io/autoXplainR/reference/audit_explanations.md)
and
[`explain_effect()`](https://matt17br.github.io/autoXplainR/reference/explain_effect.md)
have no shared row cap by default and accept `max_rows` explicitly. PDP
has its separate `sample_size` control: its curve and support
distribution can use different row counts, both recorded in the result.

Large reports store compressed column values and unpack them offline as
needed. Compression does not remove exported values. The Data tab’s
linked scatter shows at most 1,500 points; filters and record lookup use
all exported rows. Exporting every row can still create a large file and
considerable browser memory use; leave `report_data = "summary"` unless
individual records serve a concrete purpose.

For large categorical expansions, automatic boosting encoding uses
native categorical splits instead of a numeric contrast matrix. The
threshold is an estimated matrix with more than 50 million cells and
more than twice the input width. The representation is fixed for a
search from outer-training inputs; category mappings and preprocessing
stay inside each fold. The two representations are different models, not
interchangeable numerical shortcuts. Set `encoding = "matrix"` or
`"native"` in an explicit boosting grid to control this choice. The
search record and report show what was used and why.

Use
[`report_data_control()`](https://matt17br.github.io/autoXplainR/reference/report_data_control.md)
to restrict exported columns or sample records if you need row
filtering. Full records for hundreds of columns make a large HTML file.
Aggregate mode keeps full univariate summaries and bounded pair
summaries but does not allow row filtering. `max_models` bounds the
candidate pool, not elapsed time. Adaptive screening and early-stopping
calibration can fit a candidate more than once. `max_runtime_secs` is an
H2O setting and does not impose a local-engine timeout.

## Compare fitted behavior

``` r

comparison <- autoxplain(iris, "Sepal.Length", model_set = "comparison", seed = 2026)
compare_model_behavior(comparison)
#> <AutoXplainR model behavior comparison>
#>   models:      3 (main_model, small_tree, flexible_tree)
#>   evidence:    30 test rows
#>   performance: rmse (lower is better)
#>   trade-off:   model_size_kb (resource proxy)
#>   distance:    absolute difference in predicted target units
#>   feature check: not computed
#> 
#> Models at a glance
#>          model family backend   rmse relative_gap model_size_kb
#>   main_model * linear   stats 0.2736         0.0%         58.60
#>     small_tree   tree   rpart 0.4490        64.1%         40.81
#>  flexible_tree   tree   rpart 0.4103        49.9%         46.84
#>   * best supplied evaluation score; rankings remain descriptive
#> 
#> What differs
#>   - main_model has the best supplied rmse score (0.2736).
#>   - main_model and small_tree differ most on average (0.29 using absolute difference in predicted target units).
#>   - Before considering this dataset, main_model allows none unless encoded in features with none unless specified in features; small_tree allows stepwise with automatic along tree paths.
#> 
#> Evidence key
#>   behavior cards = prior knowledge about model capacity
#>   metrics, prediction gaps, and optional permutation importance = computed evidence
#>   caution: descriptive comparison, not causal or uncertainty coverage
head(prediction_ambiguity(comparison)$rows)
#>   evaluation_row row_id observed prediction_min prediction_max prediction_range
#> 1              1    121      6.9       6.322807       6.746404      0.423597456
#> 2              2     38      4.9       4.800000       5.111729      0.311729049
#> 3              3     45      5.1       5.037500       5.553309      0.515809353
#> 4              4    111      6.5       6.320000       6.323176      0.003176116
#> 5              5     91      5.5       6.009130       6.385714      0.376584687
#> 6              6    108      7.3       7.238864       7.716667      0.477802181
#>   prediction_sd
#> 1   0.225104561
#> 2   0.162834886
#> 3   0.258093469
#> 4   0.001737014
#> 5   0.201728726
#> 6   0.275859218
```

Family descriptions state what an algorithm can represent. The scores
and prediction differences describe what these particular fitted models
did on common evaluation rows. Disagreement does not supply prediction
intervals or identify the correct prediction.

``` r

effects <- compare_model_effects(comparison, "Petal.Length", method = "ale", n_points = 8)
plot(effects)
```

![](model-selection_files/figure-html/effects-1.png)

Effects are evaluated on a common grid. Their differences cover the
supplied fits, not every competitive model or uncertainty from
refitting. For multiclass outcomes, request a named class with
`class = "virginica"`.

[`model_tradeoffs()`](https://matt17br.github.io/autoXplainR/reference/model_tradeoffs.md)
offers optional performance/resource comparisons. Its default cost uses
a recorded repeated prediction benchmark when available, then retained
fit time, evaluation-batch prediction time, and R object size. It
requires at least two finite score/cost pairs. Fit time excludes the
search; R size includes retained diagnostics and excludes native
allocations. Use these measurements for a concrete resource constraint;
a favorable Pareto position is not a model-selection rule.

For repeated prediction measurements on the same rows:

``` r

bench <- benchmark_predictions(comparison)
render_model_report(comparison, "report.html", benchmark = bench)
```

The benchmark records a common batch, warmup, repeated blocks, warnings
and timing limits. A per-row batch cost is not single-request latency;
quartiles describe the repeated measurements, not a confidence interval.
The benchmark excludes fitting and the guided workflow’s raw-data
preprocessing. Transformations performed inside a custom prediction
function are timed with that function.

## Optional H2O search

H2O is a separate engine requiring Java, the `h2o` package and a running
process. It does not support this package’s grouped or temporal
validation plans. With H2O internal cross-validation, learned external
imputation, missingness-based column removal and automatic ordinal
coercion are rejected because they would be learned before H2O creates
its folds. Use the default keep-missing path, the base engine for
fold-local preprocessing, or explicitly designed validation with
`nfolds = 0`. H2O’s own transformations remain engine responsibilities.

``` r

holdout <- seq(5L, nrow(iris), by = 5L)
h2o_result <- autoxplain(
  iris[-holdout, ], "Species", test_data = iris[holdout, ],
  engine = "h2o", evaluation_role = "test", max_models = 10,
  max_runtime_secs = 0, exclude_algos = "DeepLearning", seed = 2026
)
render_model_report(h2o_result, "h2o-report.html")
```

This keeps supplied evaluation rows out of H2O selection. A fixed model
budget, no wall-clock limit and exclusion of Deep Learning reduce known
sources of H2O variation; consult the recorded provenance for remaining
limits. See the [statistical
methods](https://matt17br.github.io/autoXplainR/articles/statistical-methods.md)
for evaluation assumptions.
