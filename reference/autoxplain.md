# Fit and evaluate a model through a guided workflow

`autoxplain()` is the beginner-first entry point. By default it creates
a reproducible held-out split, compares linear, tree and neural model
families using training-only cross-validation, then evaluates the
retained models and an intercept-only baseline on held-out rows. It
stores predictions and explanations for an interactive offline report.
No optional model engines, Java or cloud account are needed. Use
`model_set = "quick"` for a fast pre-specified reference model and
baseline.

## Usage

``` r
autoxplain(
  data,
  target_column,
  max_models = NULL,
  max_runtime_secs = 300L,
  seed = 123L,
  test_data = NULL,
  test_fraction = 0.2,
  engine = c("auto", "base", "h2o"),
  model_set = c("tuned", "quick", "comparison"),
  portfolio = c("core", "recommended", "extended"),
  learners = NULL,
  enable_preprocessing = TRUE,
  preprocessing_config = list(),
  task = c("auto", "regression", "binary", "multiclass"),
  nfolds = 5L,
  tuning_rule = c("one_se", "best"),
  tuning_control = NULL,
  sort_metric = "AUTO",
  include_algos = NULL,
  exclude_algos = NULL,
  use_test_as_validation = FALSE,
  init_h2o = TRUE,
  h2o_nthreads = -1L,
  h2o_max_mem_size = "2G",
  verbosity = c("quiet", "info"),
  evaluation_role = c("auto", "test", "validation", "evaluation"),
  overlap_action = c("warn", "error", "ignore"),
  validation = NULL,
  explain = TRUE,
  report = NULL,
  report_data = "summary",
  explanation_rows = 5000L
)
```

## Arguments

- data:

  Training data frame.

- target_column:

  Name of the outcome column.

- max_models:

  Maximum number of configurations in local tuning or H2O base models.
  The local budget is shared across requested learner families. `NULL`
  chooses a portfolio-aware tuning budget (15 for core, 30 for
  recommended, and 40 for extended) or 24 for H2O. Explicit values are
  honored without a hidden cap.

- max_runtime_secs:

  H2O training time budget in seconds; ignored by the guided base
  engine. Use zero to disable the wall-clock limit and let the fixed
  `max_models` budget govern the search.

- seed:

  Reproducible split and local-fitting seed. For H2O, the seed controls
  supported stochastic components but cannot guarantee an identical
  time-limited search; see the returned reproducibility provenance.

- test_data:

  Optional evaluation data. Supplied rows are labeled as a neutral
  evaluation by default; use `evaluation_role = "test"` only when their
  provenance supports an independent-test interpretation. H2O uses them
  as validation rows only when `use_test_as_validation = TRUE` and
  `nfolds = 0`.

- test_fraction:

  Fraction of `data` reserved for evaluation when `test_data` is not
  supplied. Classification splits are stratified.

- engine:

  One of `"auto"`, `"base"`, or `"h2o"`. `"auto"` currently resolves to
  the dependency-free `"base"` workflow.

- model_set:

  Guided base-engine workflow. `"tuned"` is the default. `"quick"` fits
  the pre-specified understandable model and baseline. `"comparison"`
  also fits two pre-specified trees for a descriptive Pareto view.
  `"tuned"` compares the requested behaviorally diverse learner
  portfolio using training-only resampling, retains its family winners
  for comparison, then evaluates the selected configuration once on the
  configured evaluation rows.

- portfolio:

  Local tuned-model portfolio. `"core"` is the default. `"recommended"`
  compares linear, regularized, additive (when supported), tree, forest,
  and boosting families. `"extended"` adds neural, kernel,
  nearest-neighbor, and MARS families. `"core"` retains the
  dependency-light linear/tree/neural tournament. Missing optional
  backends produce one installation command rather than silently
  changing the tournament.

- learners:

  Optional explicit learner-family vector overriding `portfolio`.
  Inspect valid names with
  [`learner_catalog()`](https://matt17br.github.io/autoXplainR/reference/learner_catalog.md).

- enable_preprocessing:

  Apply
  [`preprocess_data()`](https://matt17br.github.io/autoXplainR/reference/preprocess_data.md).

- preprocessing_config:

  Named overrides for preprocessing. Identifier removal defaults to
  `FALSE`.

- task:

  One of `"auto"`, `"regression"`, `"binary"`, or `"multiclass"`.

- nfolds:

  Number of training-only folds for local tuning or H2O
  cross-validation. Local tuning automatically reduces this when an
  outcome class contains fewer rows. For H2O, zero is accepted only with
  explicit `test_data` and `use_test_as_validation = TRUE`; this
  prevents model ranking by training error alone.

- tuning_rule:

  Local tuning selection rule. `"one_se"` chooses the first eligible
  family in the documented priority, then its smallest recorded
  flexibility proxy, among candidates whose resampled error is within
  one standard error of the best. The family priority and
  family-specific flexibility proxies are shown by
  [`learner_catalog()`](https://matt17br.github.io/autoXplainR/reference/learner_catalog.md).
  This heuristic does not establish that eligible models are equivalent
  or that every tuning dimension is ordered. `"best"` chooses the lowest
  resampled error. Ignored by other workflows.

- tuning_control:

  Optional advanced local-tuning settings returned by
  [`tuning_control()`](https://matt17br.github.io/autoXplainR/reference/tuning_control.md).
  Leave `NULL` for the beginner defaults. This argument is available
  only with `engine = "base"` and `model_set = "tuned"`.

- sort_metric:

  H2O AutoML leaderboard metric.

- include_algos, exclude_algos:

  Optional H2O algorithm filters. Supply at most one.

- use_test_as_validation:

  Whether to pass `test_data` to H2O as a validation frame when
  `nfolds = 0`. With H2O cross-validation (`nfolds >= 2`), the supplied
  frame is not passed because H2O ranks models using cross-validation
  metrics.

- init_h2o:

  Start a local H2O cluster when no connection is available.

- h2o_nthreads:

  Threads used when starting H2O.

- h2o_max_mem_size:

  Memory used when starting H2O.

- verbosity:

  One of `"quiet"` or `"info"`.

- evaluation_role:

  How to describe the evaluation rows. `"auto"` labels package-generated
  outer splits as `"test"`, supplied data as the neutral `"evaluation"`,
  and data actually used for H2O selection as `"validation"`. Use an
  explicit value to record a role established by the study design.

- overlap_action:

  What to do when supplied evaluation rows have exactly the same values
  as training rows: warn (the default), error, or ignore. Exact equality
  can indicate leakage but can also occur naturally, so this check
  cannot establish whether the samples are independent.

- validation:

  Optional
  [`validation_split()`](https://matt17br.github.io/autoXplainR/reference/validation_split.md)
  specifying whole-group or chronological evaluation. Split columns are
  excluded from model inputs.

- explain:

  Compute and retain permutation screening, an explanation audit, and up
  to eight fitted effects per audited model. Defaults to `TRUE`; use
  `FALSE` for fitting only. Screening covers all inputs for up to five
  models; the audit covers the union of their top eight inputs with 20
  permutations. These are descriptive, selected summaries.

- report:

  Optional `.html` destination, written from the retained evidence.
  Supplying a path also computes explanations when `explain = FALSE`.

- report_data:

  Data included in the HTML: `"summary"` (default) exports aggregate
  exploration, `"rows"` also exports individual observations and
  predictions, and `"none"` omits data exploration and individual
  records. Use
  [`report_data_control()`](https://matt17br.github.io/autoXplainR/reference/report_data_control.md)
  to choose columns and limit exported rows. These settings govern HTML,
  not the raw data retained in the R result.

- explanation_rows:

  Maximum evaluation rows for default permutation importance, dependence
  checks and fitted effects. Defaults to 5000; `NULL` removes this cap.
  PDP curves retain their separate 1000-row limit;
  [`explain_effect()`](https://matt17br.github.io/autoXplainR/reference/explain_effect.md)
  exposes `sample_size` for explicit curve calculations. Explanations
  record the uniform sample and its scope. Fitting, selection, model
  scores and prediction diagnostics still use their complete partitions.

## Value

An `autoxplain_result` containing fitted models, a leaderboard,
evaluation predictions, preprocessing provenance, and (by default)
`explanations`. Use `predict(result, newdata)` on raw predictor rows.
`report_file` records the HTML path when requested.

## Details

Set `engine = "h2o"` to use the optional H2O AutoML adapter. The
lower-level
[`explain_model()`](https://matt17br.github.io/autoXplainR/reference/explain_model.md)
interface accepts models fitted by any framework.

Numeric outcomes with exactly two distinct values are treated as binary
classification by default. Potentially destructive preprocessing, such
as identifier removal, is opt-in and recorded in the result.

## Examples

``` r
result <- autoxplain(mtcars, "mpg")
result
#> <AutoXplainR result>
#>   question:   predict `mpg` (regression)
#>   primary:    tuned neural network [main_model]
#>   engine:     base
#>   data:       26 training + 6 test rows
#>   design:     reproducible random holdout
#>   selection:  Selected using 5 training folds; evaluation rows did not select this model.
#>   models:     4 (selected from 15 training-resampled configurations)
#>   score:      rmse = 2.4559 on test rows
#>   baseline:   62.9% improvement in rmse
#>   caution:    Only 6 rows were available for test scoring.
#>   next:       Treat the scores as preliminary and validate on more representative rows.
#>   finding:    Near-optimal models disagree on the feature-importance ranking.
#>   inspect:    Report the supplied candidates' importance ranges; these are not bounds over a complete model class.
#>   evidence:   40 model-feature shuffle summaries; 8 fitted effects
#>   inspect:    render_model_report(result, "report.html"), evidence_summary(result)
#>   predict:    predict(result, newdata) uses the saved training recipe
#>   compare:    compare_model_behavior(result) examines the retained models
explainers <- as_explainers(result)
audit_explanations(explainers)
#> <AutoXplainR explanation evidence audit>
#>   models:             4 (3 near-optimal)
#>   max association:    0.949
#>   explanation accord: 0.647
#>   prediction accord:  0.99
#>   scope: Separate descriptive diagnostics; no overall evidence grade. Shuffle intervals omit evaluation-sampling, fitting and selection uncertainty.
#>   association: Limited pairwise screen: absolute Spearman correlation for numeric pairs, correlation ratio for mixed pairs, and Cramer's V for categorical pairs. Small values do not establish independence or exclude nonlinear or joint dependence. Categorical pairs without repeated categories are unavailable; many rare categories can inflate association.
#> 
#> Findings
#>   [warning] `cyl` exceeds the pairwise association threshold.
#>   [warning] `disp` exceeds the pairwise association threshold.
#>   [warning] `hp` exceeds the pairwise association threshold.
#>   [warning] `drat` exceeds the pairwise association threshold.
#>   [warning] `wt` exceeds the pairwise association threshold.
#>   [warning] `qsec` exceeds the pairwise association threshold.

if (FALSE) { # \dontrun{
if (identical(Sys.getenv("AUTOXPLAIN_RUN_H2O"), "true")) {
  h2o_result <- autoxplain(mtcars, "mpg", engine = "h2o", max_models = 3)
}
} # }
```
