prepare_h2o_outer_split <- function(data,
                                    test_data,
                                    target,
                                    task,
                                    test_fraction,
                                    seed,
                                    enable_preprocessing,
                                    preprocessing_config,
                                    overlap_action = c("warn", "error", "ignore")) {
  if (!is.null(test_data)) {
    check_evaluation_row_overlap(data, test_data, overlap_action)
  }
  split <- if (is.null(test_data)) {
    make_evaluation_split(data, target, task, test_fraction, seed)
  } else {
    list(
      training = data,
      evaluation = test_data,
      method = "user-supplied evaluation data",
      moved_for_unseen_levels = 0L
    )
  }

  training <- if (enable_preprocessing) {
    do.call(
      preprocess_data,
      c(list(data = split$training, target_column = target), preprocessing_config)
    )
  } else {
    unprocessed_metadata(split$training)
  }
  evaluation <- if (enable_preprocessing) {
    apply_preprocessing_recipe(
      split$evaluation,
      training$recipe,
      target,
      missing_value_strategy = preprocessing_config$missing_value_strategy,
      novel_level_strategy = preprocessing_config$novel_level_strategy
    )
  } else {
    unprocessed_metadata(split$evaluation)
  }

  training$data[[target]] <- coerce_outcome_for_task(training$data[[target]], task)
  target_levels <- if (is.factor(training$data[[target]])) {
    levels(training$data[[target]])
  } else {
    NULL
  }
  evaluation$data[[target]] <- coerce_outcome_for_task(
    evaluation$data[[target]],
    task,
    target_levels = target_levels
  )
  if (anyNA(evaluation$data[[target]])) {
    stop(
      "Evaluation outcomes contain missing values or classes absent from the training data.",
      call. = FALSE
    )
  }
  if (identical(task, "regression") && any(!is.finite(evaluation$data[[target]]))) {
    stop("Evaluation outcomes must be finite numeric values.", call. = FALSE)
  }
  evaluation$data <- validate_train_test_schema(
    training$data,
    evaluation$data,
    target
  )
  validate_h2o_processed_split(training$data, evaluation$data, target, task)
  training$final_info <- data_info(training$data)
  evaluation$final_info <- data_info(evaluation$data)

  list(
    training = training,
    evaluation = evaluation,
    split_method = split$method,
    test_data_supplied = !is.null(test_data),
    test_fraction_requested = if (is.null(test_data)) test_fraction else NA_real_
  )
}

validate_h2o_processed_split <- function(training, evaluation, target, task) {
  predictors <- setdiff(names(training), target)
  if (!length(predictors)) {
    stop("At least one predictor must remain after preprocessing.", call. = FALSE)
  }
  invalid <- predictors[vapply(training[predictors], function(value) {
    is.numeric(value) && any(is.infinite(value), na.rm = TRUE)
  }, logical(1))]
  invalid_evaluation <- predictors[vapply(evaluation[predictors], function(value) {
    is.numeric(value) && any(is.infinite(value), na.rm = TRUE)
  }, logical(1))]
  invalid <- unique(c(invalid, invalid_evaluation))
  if (length(invalid)) {
    stop(
      "H2O predictors must not contain infinite values after preprocessing: ",
      paste(invalid, collapse = ", "), ". Replace infinities explicitly; H2O would ",
      "otherwise convert them to missing values.",
      call. = FALSE
    )
  }
  validate_guided_target(training[[target]], task)
  if (anyNA(training[[target]]) || anyNA(evaluation[[target]])) {
    stop("H2O training and evaluation outcomes must be fully observed.", call. = FALSE)
  }
  if (identical(task, "regression") &&
        (any(!is.finite(training[[target]])) || any(!is.finite(evaluation[[target]])))) {
    stop("H2O regression outcomes must be finite numeric values.", call. = FALSE)
  }
  invisible(TRUE)
}

resolve_h2o_evaluation_role <- function(test_data_supplied,
                                        use_test_as_validation,
                                        nfolds,
                                        evaluation_role = c(
                                          "auto", "test", "validation", "evaluation"
                                        )) {
  evaluation_role <- match.arg(evaluation_role)
  if (isTRUE(test_data_supplied) && isTRUE(use_test_as_validation) &&
        identical(evaluation_role, "test")) {
    stop(
      "`evaluation_role = \"test\"` contradicts `use_test_as_validation = TRUE`; ",
      "use `evaluation_role = \"auto\"` or `\"validation\"`.",
      call. = FALSE
    )
  }
  test_used_for_validation <- isTRUE(test_data_supplied) &&
    isTRUE(use_test_as_validation) && identical(as.integer(nfolds), 0L)
  if (test_used_for_validation && identical(evaluation_role, "evaluation")) {
    stop(
      "`evaluation_role = \"evaluation\"` is not valid when the same rows are ",
      "used for H2O model selection; use `\"auto\"` or `\"validation\"`.",
      call. = FALSE
    )
  }
  list(
    test_used_for_validation = test_used_for_validation,
    validation_requested_but_not_used = isTRUE(test_data_supplied) &&
      isTRUE(use_test_as_validation) && !test_used_for_validation,
    evaluation_role = if (test_used_for_validation) {
      "validation"
    } else {
      resolve_evaluation_role(test_data_supplied, evaluation_role)
    }
  )
}

h2o_candidate_selection_provenance <- function(nfolds,
                                               used_for_validation,
                                               evaluation_role) {
  if (isTRUE(used_for_validation)) {
    return(paste(
      "The primary H2O model is the first model in the engine leaderboard,",
      "ranked using the supplied validation rows because nfolds = 0.",
      "Scores on those same rows are validation evidence, not an independent",
      "test of generalization."
    ))
  }
  paste(
    "The primary H2O model is the first model in the engine leaderboard,",
    "ranked using", paste0(nfolds, "-fold"), "training-only cross-validation.",
    "The", evaluation_role, "rows were not used for engine-leaderboard ranking;",
    "their common-row scores are descriptive and did not replace the selected model."
  )
}

fit_h2o_baseline <- function(data, target, task) {
  formula <- safe_reformulate(character(), response = target)
  timed_model_fit(function() {
    if (identical(task, "regression")) {
      stats::lm(formula, data = data)
    } else if (identical(task, "binary")) {
      stats::glm(formula, data = data, family = stats::binomial())
    } else {
      nnet::multinom(formula, data = data, trace = FALSE)
    }
  })
}

h2o_model_label <- function(model, index) {
  algorithm <- tryCatch(
    as.character(model@algorithm),
    error = function(error) class(model)[[1L]]
  )
  paste0("H2O ", algorithm, " #", index)
}

h2o_evaluation_diagnostics <- function(models, baseline) {
  h2o_ids <- names(models)
  h2o_rows <- data.frame(
    model_id = h2o_ids,
    training_time_ms = vapply(models, function(model) {
      value <- tryCatch(as.numeric(model@model$run_time), error = function(error) NA_real_)
      if (length(value) == 1L) value else NA_real_
    }, numeric(1)),
    model_size_kb = vapply(models, function(model) {
      value <- tryCatch(
        as.numeric(model@model$output$model_size_in_bytes) / 1024,
        error = function(error) NA_real_
      )
      if (length(value) == 1L) value else NA_real_
    }, numeric(1)),
    complexity = NA_real_,
    fit_warning = "",
    stringsAsFactors = FALSE
  )
  rbind(
    h2o_rows,
    data.frame(
      model_id = "simple_baseline",
      training_time_ms = baseline$elapsed_ms,
      model_size_kb = as.numeric(utils::object.size(baseline$model)) / 1024,
      complexity = model_complexity(baseline$model),
      fit_warning = paste(unique(baseline$warnings), collapse = " | "),
      stringsAsFactors = FALSE
    )
  )
}

h2o_outer_primary_metric <- function(task) {
  if (identical(task, "regression")) "rmse" else "log_loss"
}

h2o_evaluation_notes <- function(evaluation,
                                 role,
                                 primary_model_id,
                                 used_for_selection = FALSE) {
  accumulator <- new.env(parent = emptyenv())
  accumulator$notes <- list()
  add <- function(severity, code, message, recommendation) {
    notes <- accumulator$notes
    notes[[length(notes) + 1L]] <- data.frame(
      severity = severity,
      code = code,
      message = message,
      recommendation = recommendation,
      stringsAsFactors = FALSE
    )
    accumulator$notes <- notes
  }
  if (evaluation$evaluated_rows < 50L) {
    add(
      "caution",
      "small_evaluation_set",
      paste0("Only ", evaluation$evaluated_rows, " rows were available for ", role, " scoring."),
      "Treat these scores as preliminary and validate on more representative rows."
    )
  }
  if (identical(role, "validation")) {
    add(
      "warning",
      "validation_not_final_test",
      if (isTRUE(used_for_selection)) {
        "H2O used the displayed validation rows to rank models."
      } else {
        "These rows are designated as validation data and are not an independent final test."
      },
      "Use a separate untouched test set before making final generalization claims."
    )
  }
  if (!identical(evaluation$winner, primary_model_id)) {
    add(
      "note",
      "outer_rank_is_descriptive",
      paste0(
        "The H2O-selected primary model was not the lowest-error model on these ", role,
        " rows; the displayed outer rank did not replace it."
      ),
      "Use the common-row scores to compare behavior descriptively, not to re-select a winner."
    )
  }
  if (identical(evaluation$beats_baseline, FALSE)) {
    add(
      "warning",
      "baseline_not_beaten",
      "The H2O-selected primary model did not improve on the intercept-only baseline.",
      "Revisit data quality and validation design before interpreting fitted patterns as useful."
    )
  }
  if (!length(accumulator$notes)) {
    return(data.frame(
      severity = character(), code = character(), message = character(),
      recommendation = character(), stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, accumulator$notes)
}

h2o_reproducibility_provenance <- function(max_runtime_secs,
                                           include_algos,
                                           exclude_algos) {
  include <- tolower(include_algos %||% character())
  exclude <- tolower(exclude_algos %||% character())
  deep_learning_possible <- if (length(include)) {
    "deeplearning" %in% include
  } else {
    !"deeplearning" %in% exclude
  }
  time_limited <- max_runtime_secs > 0L
  list(
    time_limited = time_limited,
    deep_learning_possible = deep_learning_possible,
    mode = if (!time_limited && !deep_learning_possible) {
      "more reproducible fixed-model-budget search"
    } else {
      "best-effort seeded search"
    },
    note = paste(
      "The seed controls supported stochastic components, but does not guarantee an",
      "identical AutoML search when a wall-clock limit is active or Deep Learning is eligible.",
      "For a more reproducible search, use max_runtime_secs = 0, a fixed max_models,",
      "and exclude_algos = 'DeepLearning'."
    )
  )
}
