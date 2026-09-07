#' Fit and evaluate a model through a guided workflow
#'
#' `autoxplain()` is the beginner-first entry point. By default it creates a
#' reproducible held-out split, compares linear, tree and neural model families
#' using training-only cross-validation, then evaluates the retained models and
#' an intercept-only baseline on held-out rows. It stores predictions and
#' explanations for an interactive offline report. No optional model engines,
#' Java or cloud account are needed. Use `model_set = "quick"` for a fast
#' pre-specified reference model and baseline.
#'
#' Set `engine = "h2o"` to use the optional H2O AutoML adapter. The lower-level
#' [explain_model()] interface accepts models fitted by any framework.
#'
#' Numeric outcomes with exactly two distinct values are treated as binary
#' classification by default. Potentially destructive preprocessing, such as
#' identifier removal, is opt-in and recorded in the result.
#'
#' @param data Training data frame.
#' @param target_column Name of the outcome column.
#' @param max_models Maximum number of configurations in local tuning or H2O
#'   base models. The local budget is shared across requested learner families.
#'   `NULL` chooses a portfolio-aware tuning budget (15 for core, 30 for
#'   recommended, and 40 for extended) or 24 for H2O. Explicit values are
#'   honored without a hidden cap.
#' @param max_runtime_secs H2O training time budget in seconds; ignored by the
#'   guided base engine. Use zero to disable the wall-clock limit and let the
#'   fixed `max_models` budget govern the search.
#' @param seed Reproducible split and local-fitting seed. For H2O, the seed
#'   controls supported stochastic components but cannot guarantee an identical
#'   time-limited search; see the returned reproducibility provenance.
#' @param test_data Optional evaluation data. Supplied rows are labeled as a
#'   neutral evaluation by default; use `evaluation_role = "test"` only when
#'   their provenance supports an independent-test interpretation. H2O uses
#'   them as validation rows only when `use_test_as_validation = TRUE` and
#'   `nfolds = 0`.
#' @param test_fraction Fraction of `data` reserved for evaluation when
#'   `test_data` is not supplied. Classification splits are stratified.
#' @param engine One of `"auto"`, `"base"`, or `"h2o"`. `"auto"` currently
#'   resolves to the dependency-free `"base"` workflow.
#' @param model_set Guided base-engine workflow. `"tuned"` is the default. `"quick"` fits the
#'   pre-specified understandable model and baseline. `"comparison"` also fits
#'   two pre-specified trees for a descriptive Pareto view. `"tuned"` compares
#'   the requested behaviorally diverse learner portfolio using training-only
#'   resampling, retains its family winners for comparison, then evaluates the
#'   selected configuration once on the configured evaluation rows.
#' @param portfolio Local tuned-model portfolio. `"core"` is the default. `"recommended"` compares
#'   linear, regularized, additive (when supported), tree, forest, and boosting
#'   families. `"extended"` adds neural, kernel, nearest-neighbor, and MARS
#'   families. `"core"` retains the dependency-light linear/tree/neural
#'   tournament. Missing optional backends produce one installation command
#'   rather than silently changing the tournament.
#' @param learners Optional explicit learner-family vector overriding
#'   `portfolio`. Inspect valid names with [learner_catalog()].
#' @param enable_preprocessing Apply [preprocess_data()].
#' @param preprocessing_config Named overrides for preprocessing. Identifier
#'   removal defaults to `FALSE`.
#' @param task One of `"auto"`, `"regression"`, `"binary"`, or
#'   `"multiclass"`.
#' @param nfolds Number of training-only folds for local tuning or H2O
#'   cross-validation. Local tuning automatically reduces this when an outcome
#'   class contains fewer rows. For H2O, zero is accepted only with explicit
#'   `test_data` and `use_test_as_validation = TRUE`; this prevents model ranking
#'   by training error alone.
#' @param tuning_rule Local tuning selection rule. `"one_se"` chooses the
#'   first eligible family in the documented priority, then its
#'   least-flexible configuration, among candidates whose resampled error is
#'   within one standard error of the best. The family priority and
#'   family-specific flexibility proxies
#'   are shown by [learner_catalog()]. `"best"` chooses the lowest resampled
#'   error. Ignored by other workflows.
#' @param tuning_control Optional advanced local-tuning settings returned by
#'   [tuning_control()]. Leave `NULL` for the beginner defaults. This argument
#'   is available only with `engine = "base"` and `model_set = "tuned"`.
#' @param sort_metric H2O AutoML leaderboard metric.
#' @param include_algos,exclude_algos Optional H2O algorithm filters. Supply at
#'   most one.
#' @param use_test_as_validation Whether to pass `test_data` to H2O as a
#'   validation frame when `nfolds = 0`. With H2O cross-validation (`nfolds >=
#'   2`), the supplied frame is not passed because H2O ranks models using
#'   cross-validation metrics.
#' @param init_h2o Start a local H2O cluster when no connection is available.
#' @param h2o_nthreads Threads used when starting H2O.
#' @param h2o_max_mem_size Memory used when starting H2O.
#' @param verbosity One of `"quiet"` or `"info"`.
#' @param evaluation_role How to describe the evaluation rows. `"auto"` labels
#'   package-generated outer splits as `"test"`, supplied data as the neutral
#'   `"evaluation"`, and data actually used for H2O selection as
#'   `"validation"`. Use an explicit value to record a role established by the
#'   study design.
#' @param overlap_action What to do when supplied evaluation rows have exactly
#'   the same values as training rows: warn (the default), error, or ignore.
#'   Exact equality can indicate leakage but can also occur naturally, so this
#'   check cannot establish whether the samples are independent.
#'
#' @param validation Optional [validation_split()] specifying whole-group or
#'   chronological evaluation. Split columns are excluded from model inputs.
#' @param explain Compute and retain permutation screening, an explanation audit,
#'   and up to eight fitted effects per audited model. Defaults to `TRUE`; use
#'   `FALSE` for fitting only. Screening covers all inputs for up to five models;
#'   the audit covers the union of their top eight inputs with 20 permutations.
#'   These are descriptive, selected summaries.
#' @param report Optional `.html` destination, written from the retained evidence.
#'   Supplying a path also computes explanations when `explain = FALSE`.
#'
#' @return An `autoxplain_result` containing fitted models, a leaderboard,
#'   evaluation predictions, preprocessing provenance, and (by default)
#'   `explanations`. Use `predict(result, newdata)` on raw predictor rows.
#'   `report_file` records the HTML path when requested.
#' @export
#'
#' @examples
#' result <- autoxplain(mtcars, "mpg")
#' result
#' explainers <- as_explainers(result)
#' audit_explanations(explainers)
#'
#' \dontrun{
#' if (identical(Sys.getenv("AUTOXPLAIN_RUN_H2O"), "true")) {
#'   h2o_result <- autoxplain(mtcars, "mpg", engine = "h2o", max_models = 3)
#' }
#' }
autoxplain <- function(data,
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
                       report = NULL) {
  engine <- match.arg(engine)
  resolved_engine <- if (engine == "auto") "base" else engine
  model_set <- match.arg(model_set)
  portfolio <- match.arg(portfolio)
  task <- match.arg(task)
  tuning_rule <- match.arg(tuning_rule)
  verbosity <- match.arg(verbosity)
  evaluation_role <- match.arg(evaluation_role)
  overlap_action <- match.arg(overlap_action)
  if (!is.null(tuning_control) &&
        (!identical(resolved_engine, "base") || !identical(model_set, "tuned"))) {
    stop(
      "`tuning_control` is available only with `engine = \"base\"` and ",
      "`model_set = \"tuned\"`.",
      call. = FALSE
    )
  }
  assert_flag(explain, "explain")
  assert_flag(enable_preprocessing, "enable_preprocessing")
  if (!is.null(report)) validate_html_destination(report, FALSE)
  validate_automl_inputs(data, target_column, test_data)
  assert_probability(test_fraction, "test_fraction")
  if (test_fraction <= 0 || test_fraction >= 1) {
    stop("`test_fraction` must be greater than zero and less than one.", call. = FALSE)
  }
  design <- prepare_validation_design(
    data, target_column, test_data, validation, test_fraction, seed,
    resolved_engine, model_set, task, nfolds, tuning_control
  )
  if (!is.null(design)) {
    data <- design$training
    test_data <- design$evaluation
    tuning_control <- design$tuning_control
    if (evaluation_role == "auto") evaluation_role <- "test"
  }
  if (resolved_engine == "base") {
    resolved_task <- if (task == "auto") detect_task(data[[target_column]]) else task
    resolved_learners <- NULL
    if (identical(model_set, "tuned")) {
      resolved_learners <- resolve_tuning_learners(portfolio, learners, resolved_task)
      tuning_control <- resolve_tuning_control(
        control = tuning_control,
        learners = resolved_learners,
        task = resolved_task,
        outcome = coerce_outcome_for_task(data[[target_column]], resolved_task),
        nfolds = nfolds,
        max_models = max_models,
        automatic_max_models = default_local_tuning_budget(portfolio, learners),
        test_data_supplied = !is.null(test_data)
      )
      max_models <- tuning_control$max_models
      nfolds <- if (identical(tuning_control$fold_source, "supplied_vfold")) {
        length(tuning_control$fold_labels)
      } else {
        nfolds
      }
    } else if (is.null(max_models)) {
      # Kept for a stable result/provenance shape; quick workflows do not use
      # this budget to search models.
      max_models <- 24L
    }
    result <- fit_guided_base(
      data = data,
      target_column = target_column,
      test_data = test_data,
      test_fraction = test_fraction,
      seed = seed,
      task = resolved_task,
      model_set = model_set,
      portfolio = portfolio,
      learners = resolved_learners,
      max_models = max_models,
      nfolds = nfolds,
      tuning_rule = tuning_rule,
      tuning_control = tuning_control,
      enable_preprocessing = enable_preprocessing,
      preprocessing_config = preprocessing_config,
      verbosity = verbosity,
      evaluation_role = evaluation_role,
      overlap_action = overlap_action
    )
    return(finalize_autoxplain(result, design, explain, report))
  }

  if (is.null(max_models)) max_models <- 24L
  max_models <- assert_count(max_models, "max_models")
  max_runtime_secs <- assert_count(max_runtime_secs, "max_runtime_secs", minimum = 0L)
  seed <- assert_count(seed, "seed", minimum = 0L)
  nfolds <- assert_count(nfolds, "nfolds", minimum = 0L)
  if (nfolds == 1L) stop("`nfolds` must be zero or at least two.", call. = FALSE)
  if (!is.null(include_algos) && !is.null(exclude_algos)) {
    stop("Supply only one of `include_algos` and `exclude_algos`.", call. = FALSE)
  }
  if (!is.list(preprocessing_config)) {
    stop("`preprocessing_config` must be a named list.", call. = FALSE)
  }
  if (!is.logical(use_test_as_validation) || length(use_test_as_validation) != 1L ||
        is.na(use_test_as_validation)) {
    stop("`use_test_as_validation` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(test_data) && isTRUE(use_test_as_validation) &&
        identical(evaluation_role, "test")) {
    stop(
      "`evaluation_role = \"test\"` contradicts `use_test_as_validation = TRUE`; ",
      "use `evaluation_role = \"auto\"` or `\"validation\"`.",
      call. = FALSE
    )
  }
  if (nfolds == 0L && (is.null(test_data) || !isTRUE(use_test_as_validation))) {
    stop(
      "H2O `nfolds = 0` would rank models without cross-validation. Supply explicit ",
      "`test_data` with `use_test_as_validation = TRUE`, or use at least two folds.",
      call. = FALSE
    )
  }

  resolved_task <- if (task == "auto") detect_task(data[[target_column]]) else task
  if (anyNA(data[[target_column]])) {
    stop(
      "The target contains missing values. Remove those rows or supply observed outcomes before fitting.",
      call. = FALSE
    )
  }
  validate_guided_predictors(data, target_column)
  validate_guided_target(data[[target_column]], resolved_task)
  if (!is.null(test_data)) {
    validate_guided_predictors(test_data, target_column, allow_all_missing = enable_preprocessing)
  }
  config <- utils::modifyList(
    list(
      enable_target_handling = TRUE,
      enable_character_to_factors = TRUE,
      enable_ordered_factors = FALSE,
      enable_ordinal_factors = FALSE,
      enable_id_removal = FALSE,
      missing_value_strategy = "keep",
      novel_level_strategy = "mode",
      verbose = verbosity == "info"
    ),
    preprocessing_config
  )

  preprocessing_contract <- validate_h2o_preprocessing_contract(
    enable_preprocessing, config, nfolds
  )
  require_optional("h2o", "fitting H2O AutoML models")
  prepared <- prepare_h2o_outer_split(
    data = data,
    test_data = test_data,
    target = target_column,
    task = resolved_task,
    test_fraction = test_fraction,
    seed = seed,
    enable_preprocessing = enable_preprocessing,
    preprocessing_config = config,
    overlap_action = overlap_action
  )
  train_processed <- prepared$training
  test_processed <- prepared$evaluation
  train <- train_processed$data
  evaluation <- test_processed$data
  evaluation_provenance <- resolve_h2o_evaluation_role(
    test_data_supplied = prepared$test_data_supplied,
    use_test_as_validation = use_test_as_validation,
    nfolds = nfolds,
    evaluation_role = evaluation_role
  )

  ensure_h2o_connection(init_h2o, h2o_nthreads, h2o_max_mem_size, verbosity)
  h2o_training <- h2o::as.h2o(train)
  h2o_validation <- if (evaluation_provenance$test_used_for_validation) {
    h2o::as.h2o(evaluation)
  } else {
    NULL
  }
  features <- setdiff(names(train), target_column)
  arguments <- list(
    x = features,
    y = target_column,
    training_frame = h2o_training,
    max_models = max_models,
    max_runtime_secs = max_runtime_secs,
    seed = seed,
    sort_metric = sort_metric,
    nfolds = nfolds,
    verbosity = if (verbosity == "quiet") NULL else "info"
  )
  if (!is.null(h2o_validation)) arguments$validation_frame <- h2o_validation
  if (!is.null(include_algos)) arguments$include_algos <- include_algos
  if (!is.null(exclude_algos)) arguments$exclude_algos <- exclude_algos
  arguments <- arguments[!vapply(arguments, is.null, logical(1))]

  automl <- do.call(h2o::h2o.automl, arguments)
  engine_leaderboard <- as.data.frame(automl@leaderboard)
  model_ids <- as.character(engine_leaderboard$model_id)
  h2o_models <- lapply(model_ids, h2o::h2o.getModel)
  names(h2o_models) <- model_ids
  if (!length(h2o_models)) stop("H2O AutoML returned no retrievable models.", call. = FALSE)

  primary_model_id <- model_ids[[1L]]
  baseline <- fit_h2o_baseline(train, target_column, resolved_task)
  models <- c(h2o_models, list(simple_baseline = baseline$model))
  labels <- c(
    stats::setNames(
      vapply(seq_along(h2o_models), function(index) {
        h2o_model_label(h2o_models[[index]], index)
      }, character(1)),
      model_ids
    ),
    simple_baseline = "intercept-only baseline"
  )
  roles <- stats::setNames(rep("candidate", length(models)), names(models))
  roles[[primary_model_id]] <- "primary"
  roles[["simple_baseline"]] <- "baseline"
  evaluated <- evaluate_candidates(
    models = models,
    data = evaluation,
    target = target_column,
    task = resolved_task,
    labels = labels,
    roles = roles,
    diagnostics = h2o_evaluation_diagnostics(h2o_models, baseline),
    primary_metric = h2o_outer_primary_metric(resolved_task),
    primary_model_id = primary_model_id
  )
  evaluated$summary$notes <- h2o_evaluation_notes(
    evaluated$summary,
    evaluation_provenance$evaluation_role,
    primary_model_id,
    used_for_selection = evaluation_provenance$test_used_for_validation
  )
  reproducibility <- h2o_reproducibility_provenance(
    max_runtime_secs,
    include_algos,
    exclude_algos
  )

  result <- structure(
    list(
      engine = "h2o",
      models = models,
      leaderboard = evaluated$leaderboard,
      engine_leaderboard = engine_leaderboard,
      evaluation = evaluated$summary,
      training_data = train,
      test_data = evaluation,
      evaluation_context = prepared$evaluation_context,
      evaluation_row_indices = prepared$evaluation_row_indices,
      target_column = target_column,
      features = features,
      task = resolved_task,
      automl_object = automl,
      model_characteristics = NULL,
      model_diagnostics = evaluated$model_diagnostics,
      preprocessing_metadata = list(
        enabled = enable_preprocessing,
        training_data = train_processed,
        test_data = test_processed
      ),
      provenance = list(
        created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
        seed = seed,
        max_models = max_models,
        max_runtime_secs = max_runtime_secs,
        reproducibility = reproducibility,
        preprocessing_contract = preprocessing_contract,
        nfolds = nfolds,
        sort_metric = sort_metric,
        primary_model_id = primary_model_id,
        primary_model_label = unname(labels[[primary_model_id]]),
        candidate_selection = h2o_candidate_selection_provenance(
          nfolds = nfolds,
          used_for_validation = evaluation_provenance$test_used_for_validation,
          evaluation_role = evaluation_provenance$evaluation_role
        ),
        test_used_for_validation = evaluation_provenance$test_used_for_validation,
        validation_requested_but_not_used =
          evaluation_provenance$validation_requested_but_not_used,
        evaluation_role = evaluation_provenance$evaluation_role,
        split_method = prepared$split_method,
        test_fraction_requested = prepared$test_fraction_requested,
        training_rows = nrow(train),
        evaluation_rows = nrow(evaluation)
      )
    ),
    class = "autoxplain_result"
  )
  result$model_characteristics <- tryCatch(
    extract_model_characteristics(result),
    error = function(error) structure(list(), class = "autoxplainr_model_characteristics")
  )
  finalize_autoxplain(result, design, explain, report)
}

default_local_tuning_budget <- function(portfolio, learners = NULL) {
  if (!is.null(learners)) {
    return(as.integer(5L * length(learners)))
  }
  switch(portfolio,
    core = 15L,
    recommended = 30L,
    extended = 40L,
    stop("Unknown model portfolio.", call. = FALSE)
  )
}

#' Convert an AutoML result to model-agnostic explainers
#'
#' @param x An `autoxplain_result`.
#' @param data Optional raw evaluation data. The fitted preprocessing recipe is
#'   applied automatically. Defaults to the configured `test_data` when available,
#'   otherwise training data.
#' @param models Model indices, IDs, or `NULL` for all retained models.
#'
#' @return A named list of `autoxplain_explainer` objects.
#' @export
as_explainers <- function(x, data = NULL, models = NULL) {
  if (!inherits(x, "autoxplain_result")) {
    stop("`x` must be an `autoxplain_result`.", call. = FALSE)
  }
  evaluation <- data %||% x$test_data %||% x$training_data
  if (!is.null(data)) {
    assert_data_frame(data, "data")
    if (isTRUE(x$preprocessing_metadata$enabled)) {
      recipe <- x$preprocessing_metadata$training_data$recipe
      strategy <- recipe$missing_value_strategy %||% "keep"
      evaluation <- apply_preprocessing_recipe(
        data,
        recipe,
        x$target_column,
        missing_value_strategy = strategy
      )$data
    }
    if (!x$target_column %in% names(evaluation)) {
      stop("Evaluation data must contain target column `", x$target_column, "`.",
        call. = FALSE
      )
    }
    target_levels <- if (is.factor(x$training_data[[x$target_column]])) {
      levels(x$training_data[[x$target_column]])
    } else {
      NULL
    }
    evaluation[[x$target_column]] <- coerce_outcome_for_task(
      evaluation[[x$target_column]],
      x$task,
      target_levels
    )
    if (anyNA(evaluation[[x$target_column]])) {
      stop(
        "Evaluation outcomes contain missing values or classes absent from the training data.",
        call. = FALSE
      )
    }
    if (identical(x$task, "regression") &&
          any(!is.finite(evaluation[[x$target_column]]))) {
      stop("Evaluation outcomes must be finite numeric values.", call. = FALSE)
    }
    evaluation <- validate_train_test_schema(x$training_data, evaluation, x$target_column)
  }
  assert_data_frame(evaluation, "data")
  if (!x$target_column %in% names(evaluation)) {
    stop("Evaluation data must contain target column `", x$target_column, "`.",
      call. = FALSE
    )
  }
  selected <- select_models(x$models, models)
  role <- if (!is.null(data)) "user-supplied" else x$provenance$evaluation_role
  out <- lapply(names(selected), function(id) {
    explain_model(
      selected[[id]], evaluation,
      y = x$target_column, task = x$task,
      label = id, metadata = list(
        evaluation_role = role,
        primary_metric = x$evaluation$primary_metric %||% NULL,
        source = if (identical(x$engine, "h2o")) {
          "H2O AutoML"
        } else if (inherits(x$tuning, "autoxplain_tuning")) {
          "guided local tuning workflow"
        } else {
          "guided base workflow"
        }
      )
    )
  })
  names(out) <- names(selected)
  out
}

#' @export
print.autoxplain_result <- function(x, ...) {
  view <- report_view_model(x)
  identity <- view$identity
  cat("<AutoXplainR guided result>\n")
  cat("  question:   predict `", identity$target, "` (", identity$task, ")\n", sep = "")
  cat("  primary:    ", identity$model_label, " [", identity$model_id, "]\n", sep = "")
  cat("  engine:     ", identity$engine, "\n", sep = "")
  if (!is.null(identity$positive)) {
    cat("  event:      probabilities of `", identity$positive, "`\n", sep = "")
  }
  cat("  data:       ", identity$training_rows, " training + ",
    identity$evaluation_rows, " ", identity$evaluation_role, " rows\n",
    sep = ""
  )
  cat("  design:     ", identity$split_method, "\n", sep = "")
  cat("  selection:  ", identity$selection_note, "\n", sep = "")
  cat("  models:     ", length(x$models), if (inherits(x$tuning, "autoxplain_tuning")) {
    paste0(
      " (selected from ", nrow(x$tuning$candidates),
      " training-resampled configurations)"
    )
  } else if (length(x$models) > 2L) {
    " (retained comparison set)"
  } else {
    " (primary + baseline)"
  }, "\n", sep = "")
  if (!is.null(x$evaluation$primary_metric)) {
    metric <- x$evaluation$primary_metric
    score <- x$evaluation$metrics[[identity$model_id]][[metric]]
    cat("  score:      ", metric, " = ",
      format(round(score, 4L), trim = TRUE), " on ",
      identity$evaluation_role, " rows\n",
      sep = ""
    )
    improvement <- x$evaluation$improvement_over_baseline
    if (is.finite(improvement)) {
      cat("  baseline:   ", format(round(100 * abs(improvement), 1L), trim = TRUE),
        if (improvement >= 0) "% improvement in " else "% worse in ", metric, "\n",
        sep = ""
      )
    }
  }
  notes <- x$evaluation$notes
  if (!is.null(notes) && nrow(notes)) {
    leading <- order(match(notes$severity, c("warning", "caution", "note")))[[1L]]
    cat("  caution:    ", notes$message[[leading]], "\n", sep = "")
    cat("  next:       ", notes$recommendation[[leading]], "\n", sep = "")
  } else {
    cat("  scope:      Scores describe this evaluation sample; effects describe fitted associations.\n")
  }
  if (length(view$findings)) {
    severity <- vapply(view$findings, function(finding) finding$severity, character(1))
    leading <- order(match(severity, c("critical", "warning", "caution", "note")))[[1L]]
    finding <- view$findings[[leading]]
    cat("  finding:    ", finding$title, "\n", sep = "")
    cat("  inspect:    ", finding$action, "\n", sep = "")
  }
  if (!is.null(view$audit)) {
    cat("  evidence:   ", nrow(view$audit$importance), " model-feature shuffle summaries; ",
      length(view$effects), " fitted effects\n",
      sep = ""
    )
  }
  failed <- Filter(function(diagnostic) identical(diagnostic$status, "failed"), view$diagnostics)
  if (length(failed)) {
    cat("  incomplete: ", paste(names(failed), collapse = ", "),
      "; inspect result$explanations and the report diagnostics\n",
      sep = ""
    )
  }
  if (!is.null(x$report_file)) cat("  report:     ", x$report_file, "\n", sep = "")
  cat("  inspect:    render_model_report(result, \"report.html\"), evidence_summary(result)\n")
  cat("  predict:    predict(result, newdata) uses the saved training recipe\n")
  if (length(x$models) > 2L) {
    cat("  compare:    compare_model_behavior(result) examines the retained models\n")
  }
  invisible(x)
}

#' @export
summary.autoxplain_result <- function(object, ...) {
  summary <- list(
    task = object$task,
    target = object$target_column,
    n_models = length(object$models),
    n_features = length(object$features),
    leaderboard = object$leaderboard,
    evaluation = object$evaluation,
    provenance = object$provenance
  )
  if (!is.null(object$engine_leaderboard)) {
    summary$engine_leaderboard <- object$engine_leaderboard
  }
  summary
}

validate_automl_inputs <- function(data, target, test_data) {
  assert_data_frame(data, "data")
  if (!is.character(target) || length(target) != 1L || is.na(target) || !target %in% names(data)) {
    stop("`target_column` must name one column in `data`.", call. = FALSE)
  }
  if (nrow(data) < 10L) stop("`data` must contain at least ten rows.", call. = FALSE)
  if (anyDuplicated(names(data))) stop("`data` must have unique column names.", call. = FALSE)
  if (all(is.na(data[[target]])) || length(unique(data[[target]][!is.na(data[[target]])])) < 2L) {
    stop("The target must contain at least two observed values.", call. = FALSE)
  }
  if (!is.null(test_data)) {
    assert_data_frame(test_data, "test_data")
    if (!target %in% names(test_data)) {
      stop("`test_data` is missing the target column.",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

coerce_outcome_for_task <- function(y, task, target_levels = NULL) {
  if (task == "regression") {
    if (!is.numeric(y)) stop("Regression requires a numeric target.", call. = FALSE)
    return(y)
  }
  levels <- target_levels
  if (is.null(levels)) {
    observed <- unique(as.character(y[!is.na(y)]))
    levels <- if (is.factor(y)) {
      base::levels(y)[base::levels(y) %in% observed]
    } else {
      sort(observed)
    }
  }
  factor(as.character(y), levels = levels)
}

validate_train_test_schema <- function(train, test, target) {
  missing <- setdiff(names(train), names(test))
  if (length(missing)) {
    stop("Processed test data is missing: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  for (feature in setdiff(names(train), target)) {
    if (is.factor(train[[feature]]) && is.factor(test[[feature]])) {
      unseen <- setdiff(levels(droplevels(test[[feature]])), levels(train[[feature]]))
      if (length(unseen)) {
        stop("Test feature `", feature, "` contains unseen levels: ",
          paste(unseen, collapse = ", "),
          call. = FALSE
        )
      }
      test[[feature]] <- factor(test[[feature]],
        levels = levels(train[[feature]]),
        ordered = is.ordered(train[[feature]])
      )
    }
  }
  test[names(train)]
}

ensure_h2o_connection <- function(init, nthreads, max_mem_size, verbosity) {
  connected <- tryCatch(h2o::h2o.clusterIsUp(), error = function(error) FALSE)
  if (isTRUE(connected)) {
    return(invisible(TRUE))
  }
  if (!isTRUE(init)) {
    stop("No H2O cluster is available and `init_h2o = FALSE`.", call. = FALSE)
  }
  if (verbosity == "quiet") {
    invisible(utils::capture.output(
      h2o::h2o.init(nthreads = nthreads, max_mem_size = max_mem_size)
    ))
  } else {
    h2o::h2o.init(nthreads = nthreads, max_mem_size = max_mem_size)
  }
  if (!isTRUE(tryCatch(h2o::h2o.clusterIsUp(), error = function(error) FALSE))) {
    stop("H2O did not become available after initialization.", call. = FALSE)
  }
  invisible(TRUE)
}

select_models <- function(models, selection) {
  if (is.null(selection)) {
    return(models)
  }
  if (is.numeric(selection)) {
    if (anyNA(selection) || any(!is.finite(selection)) ||
          any(selection != floor(selection))) {
      stop("Numeric `models` indices must be finite whole numbers.", call. = FALSE)
    }
    indices <- as.integer(selection)
    if (anyDuplicated(indices)) {
      stop("Numeric `models` indices must be unique.", call. = FALSE)
    }
    if (any(indices < 1L | indices > length(models))) {
      stop("Numeric `models` indices are out of range.", call. = FALSE)
    }
    return(models[indices])
  }
  if (is.character(selection)) {
    if (anyNA(selection) || any(!nzchar(selection))) {
      stop("Character `models` IDs must be non-missing and non-empty.", call. = FALSE)
    }
    if (anyDuplicated(selection)) {
      stop("Character `models` IDs must be unique.", call. = FALSE)
    }
    missing <- setdiff(selection, names(models))
    if (length(missing)) {
      stop("Unknown model IDs: ", paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
    return(models[selection])
  }
  stop("`models` must be NULL, numeric indices, or model IDs.", call. = FALSE)
}
