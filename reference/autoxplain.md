# Fit and evaluate a model through a guided workflow

`autoxplain()` is the beginner-first entry point. By default it creates
a reproducible held-out split, fits an intercept-only baseline and an
understandable statistical model, evaluates both on unseen rows, and
stores everything needed for explanation and reporting. This path only
uses R and does not require Java or a cloud account.

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
  model_set = c("quick", "tuned", "comparison"),
  portfolio = c("recommended", "core", "extended"),
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
  overlap_action = c("warn", "error", "ignore")
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

  Guided base-engine workflow. `"quick"` fits the pre-specified
  understandable model and baseline. `"comparison"` also fits two
  pre-specified trees for a descriptive Pareto view. `"tuned"` compares
  the requested behaviorally diverse learner portfolio using
  training-only resampling, retains its family winners for comparison,
  then evaluates the selected configuration once on the configured
  evaluation rows.

- portfolio:

  Local tuned-model portfolio. `"recommended"` compares linear,
  regularized, additive (when supported), tree, forest, and boosting
  families. `"extended"` adds neural, kernel, nearest-neighbor, and MARS
  families. `"core"` retains the dependency-light linear/tree/neural
  tournament. Missing optional backends produce one installation command
  rather than silently changing the tournament.

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
  family in the documented reviewed priority, then its least-flexible
  configuration, among candidates whose resampled error is within one
  standard error of the best. The family priority and family-specific
  flexibility proxies are shown by
  [`learner_catalog()`](https://matt17br.github.io/autoXplainR/reference/learner_catalog.md).
  `"best"` chooses the lowest resampled error. Ignored by other
  workflows.

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

## Value

An `autoxplain_result` containing models, a data-frame leaderboard, task
metadata, preprocessing provenance, and evaluation data.

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
#> <AutoXplainR guided result>
#>   question:   predict `mpg` (regression)
#>   engine:     base
#>   data:       26 training + 6 evaluation rows
#>   models:     2 (primary + baseline)
#>   result:     primary model has rmse = 2.4413
#>   baseline:   63.1% improvement in rmse
#>   compare:    use model_set = "tuned" for automatic multi-family selection
#>   explain:    use render_model_report() or as_explainers() for fitted patterns
explainers <- as_explainers(result)
audit_explanations(explainers)
#> <AutoXplainR explanation evidence audit>
#>   grade:              C (diagnostic, not certification)
#>   models:             2 (1 near-optimal)
#>   stable claims:      0.0%
#>   max dependence:     0.949
#>   explanation accord: n/a (one model)
#>   prediction accord:  n/a (one model)
#> 
#> Findings
#>   [warning] 10 feature(s) exceed the dependence threshold.
#>   [warning] 20 model-feature claim(s) are qualified or unsupported.

if (FALSE) { # \dontrun{
h2o_result <- autoxplain(mtcars, "mpg", engine = "h2o", max_models = 3)
} # }
```
