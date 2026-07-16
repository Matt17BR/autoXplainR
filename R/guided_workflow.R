fit_guided_base <- function(data,
                            target_column,
                            test_data,
                            test_fraction,
                            seed,
                            task,
                            model_set,
                            portfolio,
                            learners,
                            max_models,
                            nfolds,
                            tuning_rule,
                            tuning_control,
                            enable_preprocessing,
                            preprocessing_config,
                            verbosity,
                            evaluation_role,
                            overlap_action) {
  seed <- assert_count(seed, "seed", minimum = 0L)
  if (!is.list(preprocessing_config) ||
        length(preprocessing_config) && is.null(names(preprocessing_config))) {
    stop("`preprocessing_config` must be a named list.", call. = FALSE)
  }
  if (anyNA(data[[target_column]])) {
    stop(
      "The target contains missing values. Remove those rows or supply observed outcomes before fitting.",
      call. = FALSE
    )
  }
  validate_guided_predictors(data, target_column)
  resolved_task <- if (task == "auto") detect_task(data[[target_column]]) else task
  validate_guided_target(data[[target_column]], resolved_task)
  resolved_evaluation_role <- resolve_evaluation_role(
    test_data_supplied = !is.null(test_data),
    evaluation_role = evaluation_role
  )

  split <- if (is.null(test_data)) {
    make_evaluation_split(data, target_column, resolved_task, test_fraction, seed)
  } else {
    check_evaluation_row_overlap(data, test_data, overlap_action)
    validate_guided_predictors(test_data, target_column)
    list(
      training = data,
      evaluation = test_data,
      method = "user-supplied evaluation data",
      moved_for_unseen_levels = 0L
    )
  }

  config <- utils::modifyList(
    list(
      enable_target_handling = TRUE,
      enable_character_to_factors = TRUE,
      enable_ordered_factors = FALSE,
      enable_ordinal_factors = FALSE,
      enable_id_removal = FALSE,
      missing_value_strategy = "impute",
      novel_level_strategy = "mode",
      verbose = verbosity == "info"
    ),
    preprocessing_config
  )
  # Keep the complete outer-training schema for resampling. Predictor removal
  # below is learned from all outer-training rows and is appropriate for the
  # final refit, but exposing it to inner resampling would leak feature-screening
  # decisions across folds. Each tuning fold therefore starts from this raw copy
  # and learns its own ID, missing-column, and constant-predictor removals.
  raw_tuning_data <- split$training
  raw_constant_features <- guided_constant_predictors(split$training, target_column)
  if (length(raw_constant_features)) {
    split$training <- split$training[
      setdiff(names(split$training), raw_constant_features)
    ]
    split$evaluation <- split$evaluation[
      setdiff(names(split$evaluation), raw_constant_features)
    ]
  }
  processed <- preprocess_guided_split(
    split$training,
    split$evaluation,
    target_column,
    resolved_task,
    enable_preprocessing,
    config
  )
  constant_result <- remove_guided_constant_predictors(
    processed,
    target_column,
    already_removed = raw_constant_features
  )
  processed <- constant_result$processed
  train <- processed$training$data
  evaluation_data <- processed$evaluation$data
  features <- setdiff(names(train), target_column)
  if (!length(features)) {
    stop("At least one predictor must remain after preprocessing.", call. = FALSE)
  }

  raw_tuning_data[[target_column]] <- coerce_outcome_for_task(
    raw_tuning_data[[target_column]],
    resolved_task
  )
  fit <- with_preserved_seed(seed, fit_base_candidates(
    data = train,
    target = target_column,
    features = features,
    task = resolved_task,
    model_set = model_set,
    tuning_data = raw_tuning_data,
    enable_preprocessing = enable_preprocessing,
    preprocessing_config = config,
    max_models = max_models,
    nfolds = nfolds,
    tuning_rule = tuning_rule,
    tuning_control = tuning_control,
    seed = seed,
    learners = learners %||% c("linear", "tree", "neural")
  ))
  evaluated <- evaluate_candidates(
    fit$models,
    evaluation_data,
    target_column,
    resolved_task,
    fit$labels,
    fit$roles,
    fit$diagnostics,
    primary_metric = if (identical(model_set, "tuned")) fit$tuning$metric else NULL
  )
  evaluated$summary$notes <- guided_evaluation_notes(
    train,
    evaluation_data,
    target_column,
    resolved_task,
    evaluated$summary,
    constant_result$removed,
    stats::setNames(fit$diagnostics$fit_warning, fit$diagnostics$model_id),
    evaluation_role = resolved_evaluation_role
  )
  if (identical(model_set, "tuned")) {
    evaluated$summary$notes <- rbind(
      evaluated$summary$notes,
      guided_tuning_refit_notes(fit$tuning)
    )
  }

  result <- structure(
    list(
      engine = "base",
      models = fit$models,
      leaderboard = evaluated$leaderboard,
      evaluation = evaluated$summary,
      training_data = train,
      test_data = evaluation_data,
      target_column = target_column,
      features = features,
      task = resolved_task,
      automl_object = NULL,
      tuning = fit$tuning,
      model_characteristics = NULL,
      model_diagnostics = evaluated$model_diagnostics,
      preprocessing_metadata = list(
        enabled = enable_preprocessing,
        training_data = processed$training,
        test_data = processed$evaluation
      ),
      provenance = list(
        created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
        seed = seed,
        engine_requested = "base",
        model_set = model_set,
        portfolio = if (identical(model_set, "tuned")) portfolio else NA_character_,
        learners = if (identical(model_set, "tuned")) fit$tuning$learners else character(),
        tuning_control = if (identical(model_set, "tuned")) fit$tuning$control else NULL,
        evaluation_role = resolved_evaluation_role,
        split_method = split$method,
        test_fraction_requested = if (is.null(test_data)) test_fraction else NA_real_,
        training_rows = nrow(train),
        evaluation_rows = nrow(evaluation_data),
        rows_moved_for_unseen_levels = 0L,
        novel_levels_mapped = sum(
          processed$evaluation$preprocessing_log$novel_level_mappings %||% integer()
        ),
        constant_features_removed = constant_result$removed,
        baseline = "intercept-only model",
        primary_model_id = evaluated$summary$primary_model_id,
        primary_model_label = unname(fit$labels[[evaluated$summary$primary_model_id]]),
        candidate_selection = if (identical(model_set, "tuned")) {
          paste0(
            paste(
              "The main model was selected by", fit$tuning$folds_used,
              "fold training-only resampling minimizing",
              paste0("`", fit$tuning$metric, "`"), "using the",
              tuning_rule_label(fit$tuning$selection_rule),
              "rule; evaluation rows did not participate in selection or fitting."
            ),
            if (isTRUE(fit$tuning$refit$fallback_used)) {
              paste0(
                " The selected configuration could not be refitted on all training rows; ",
                fit$tuning$final_configuration,
                " was used as the recorded fallback."
              )
            } else {
              ""
            }
          )
        } else {
          paste(
            "The main model was pre-specified; candidate ranks on the evaluation rows",
            "are descriptive and were not used to replace it."
          )
        }
      )
    ),
    class = "autoxplain_result"
  )
  shift <- missingness_shift(result)
  result$evaluation$diagnostics$missingness_shift <- shift
  flagged <- shift$features$feature[
    shift$features$flagged & shift$features$used_by_model
  ]
  if (length(flagged)) {
    result$evaluation$notes <- rbind(
      result$evaluation$notes,
      data.frame(
        severity = "caution",
        code = "missingness_shift",
        message = paste0(
          "Raw missing-data rates shifted by at least five percentage points for: ",
          paste(flagged, collapse = ", "), "."
        ),
        recommendation = paste(
          "Investigate collection or pipeline differences and validate performance",
          "on data with the missingness pattern expected in use."
        ),
        stringsAsFactors = FALSE
      )
    )
  }
  result$model_characteristics <- extract_model_characteristics(
    result,
    include_varimp = FALSE
  )
  result
}

resolve_evaluation_role <- function(test_data_supplied,
                                    evaluation_role = c(
                                      "auto", "test", "validation", "evaluation"
                                    )) {
  evaluation_role <- match.arg(evaluation_role)
  if (!identical(evaluation_role, "auto")) return(evaluation_role)
  if (isTRUE(test_data_supplied)) "evaluation" else "test"
}

check_evaluation_row_overlap <- function(training,
                                         evaluation,
                                         action = c("warn", "error", "ignore")) {
  action <- match.arg(action)
  if (identical(action, "ignore")) return(invisible(TRUE))
  if (is.null(evaluation)) return(invisible(TRUE))
  required <- names(training)
  if (!all(required %in% names(evaluation))) {
    # The schema validator supplies the more useful missing-column diagnosis.
    return(invisible(TRUE))
  }
  training_keys <- split_row_keys(training[required])
  evaluation_keys <- split_row_keys(evaluation[required])
  overlapping <- which(evaluation_keys %in% training_keys)
  if (length(overlapping)) {
    preview <- paste(utils::head(overlapping, 5L), collapse = ", ")
    message <- paste0(
      "Found ", length(overlapping), " row",
      if (length(overlapping) == 1L) "" else "s",
      " in `test_data` whose values exactly match a row in `data` (evaluation row",
      if (length(overlapping) == 1L) " " else "s ", preview,
      if (length(overlapping) > 5L) ", ..." else "", "). ",
      "This may indicate training/evaluation leakage, but coincident records can ",
      "occur naturally; exact matches alone do not prove that the samples are ",
      "non-independent. Check row provenance before interpreting the scores."
    )
    if (identical(action, "error")) stop(message, call. = FALSE)
    warning(message, call. = FALSE)
  }
  invisible(TRUE)
}

split_row_keys <- function(data) {
  normalized <- lapply(data, normalize_split_column)
  vapply(seq_len(nrow(data)), function(index) {
    raw <- serialize(lapply(normalized, `[[`, index), NULL, version = 2L)
    paste(format(raw), collapse = "")
  }, character(1))
}

normalize_split_column <- function(value) {
  if (is.factor(value) || is.character(value)) {
    return(structure(as.character(value), split_type = "text"))
  }
  if (is.integer(value) || is.numeric(value)) {
    return(structure(as.numeric(value), split_type = "numeric"))
  }
  if (is.logical(value)) {
    return(structure(value, split_type = "logical"))
  }
  value
}

make_evaluation_split <- function(data, target, task, fraction, seed) {
  n <- nrow(data)
  indices <- with_preserved_seed(seed, {
    if (task == "regression") {
      n_test <- max(2L, min(n - 2L, as.integer(round(n * fraction))))
      sample.int(n, n_test)
    } else {
      groups <- split(seq_len(n), as.character(data[[target]]), drop = TRUE)
      if (any(lengths(groups) < 2L)) {
        rare <- names(groups)[lengths(groups) < 2L]
        stop(
          "Every outcome class needs at least two rows for a stratified split. Too rare: ",
          paste(rare, collapse = ", "), ".",
          call. = FALSE
        )
      }
      sort(unlist(lapply(groups, function(group) {
        count <- max(1L, min(length(group) - 1L, as.integer(round(length(group) * fraction))))
        sample(group, count)
      }), use.names = FALSE))
    }
  })
  training_indices <- setdiff(seq_len(n), indices)
  list(
    training = data[training_indices, , drop = FALSE],
    evaluation = data[indices, , drop = FALSE],
    method = if (task == "regression") {
      "reproducible random holdout"
    } else {
      "reproducible stratified holdout"
    },
    moved_for_unseen_levels = 0L
  )
}

preprocess_guided_split <- function(training,
                                    evaluation,
                                    target,
                                    task,
                                    enabled,
                                    config) {
  if (enabled) {
    fitted <- do.call(
      preprocess_data,
      c(list(data = training, target_column = target), config)
    )
    applied <- apply_preprocessing_recipe(
      evaluation,
      fitted$recipe,
      target,
      missing_value_strategy = config$missing_value_strategy,
      novel_level_strategy = config$novel_level_strategy %||% "error"
    )
  } else {
    if (anyNA(training) || anyNA(evaluation)) {
      stop(
        "Missing predictor values require preprocessing. Set `enable_preprocessing = TRUE` ",
        "or handle them before fitting.",
        call. = FALSE
      )
    }
    fitted <- unprocessed_metadata(training)
    applied <- unprocessed_metadata(evaluation)
  }
  fitted$data[[target]] <- coerce_outcome_for_task(fitted$data[[target]], task)
  levels <- if (is.factor(fitted$data[[target]])) base::levels(fitted$data[[target]]) else NULL
  applied$data[[target]] <- coerce_outcome_for_task(applied$data[[target]], task, levels)
  if (anyNA(applied$data[[target]])) {
    stop(
      "Evaluation outcomes contain missing values or classes absent from the training data.",
      call. = FALSE
    )
  }
  if (task == "regression" && any(!is.finite(applied$data[[target]]))) {
    stop("Evaluation outcomes must be finite numeric values.", call. = FALSE)
  }
  applied$data <- validate_train_test_schema(fitted$data, applied$data, target)
  validate_finite_guided_data(fitted$data, target)
  validate_finite_guided_data(applied$data, target)
  list(training = fitted, evaluation = applied)
}

unprocessed_metadata <- function(data) {
  structure(
    list(
      data = data,
      preprocessing_log = list(),
      original_info = data_info(data),
      final_info = data_info(data),
      recipe = list()
    ),
    class = c("autoxplain_preprocessing", "list")
  )
}

validate_guided_predictors <- function(data, target) {
  predictors <- setdiff(names(data), target)
  if (!length(predictors)) stop("`data` must contain at least one predictor.", call. = FALSE)
  unsupported <- predictors[!vapply(data[predictors], supported_guided_predictor, logical(1))]
  if (length(unsupported)) {
    stop(
      "The guided workflow supports numeric, logical, factor, and character predictors. ",
      "Convert or remove unsupported columns: ", paste(unsupported, collapse = ", "),
      ". Use `explain_model()` with a custom model when those columns are required.",
      call. = FALSE
    )
  }
  all_missing <- predictors[vapply(data[predictors], function(x) all(is.na(x)), logical(1))]
  if (length(all_missing)) {
    stop("Predictors cannot be entirely missing: ", paste(all_missing, collapse = ", "), ".",
         call. = FALSE)
  }
  invisible(TRUE)
}

supported_guided_predictor <- function(x) {
  is.null(dim(x)) && (
    (is.numeric(x) && !is.complex(x)) || is.logical(x) || is.factor(x) || is.character(x)
  )
}

validate_guided_target <- function(y, task) {
  values <- unique(y[!is.na(y)])
  if (task == "regression") {
    if (!is.numeric(y) || is.complex(y) || any(!is.finite(y))) {
      stop("Regression requires a finite numeric target.", call. = FALSE)
    }
    return(invisible(TRUE))
  }
  if (is.numeric(y) && any(!is.finite(y))) {
    stop("Classification targets must contain finite observed values.", call. = FALSE)
  }
  expected <- if (task == "binary") 2L else 3L
  valid <- if (task == "binary") length(values) == expected else length(values) >= expected
  if (!valid) {
    stop(
      if (task == "binary") {
        "Binary classification requires exactly two observed outcome classes."
      } else {
        "Multiclass classification requires at least three observed outcome classes."
      },
      call. = FALSE
    )
  }
  invisible(TRUE)
}

guided_constant_predictors <- function(data, target) {
  predictors <- setdiff(names(data), target)
  predictors[vapply(
    data[predictors],
    function(x) length(unique(x[!is.na(x)])) < 2L,
    logical(1)
  )]
}

remove_guided_constant_predictors <- function(processed,
                                              target,
                                              already_removed = character()) {
  predictors <- setdiff(names(processed$training$data), target)
  constant <- unique(c(already_removed, guided_constant_predictors(
    processed$training$data,
    target
  )))
  if (!length(constant)) return(list(processed = processed, removed = character()))
  for (partition in c("training", "evaluation")) {
    item <- processed[[partition]]
    item$data <- item$data[setdiff(names(item$data), constant)]
    item$final_info <- data_info(item$data)
    item$preprocessing_log$constant_predictors <- list(
      action = "removed",
      columns = constant,
      reason = "fewer than two observed training values"
    )
    if (length(item$recipe)) {
      item$recipe$removed_columns <- unique(c(item$recipe$removed_columns, constant))
      item$recipe$final_columns <- setdiff(item$recipe$final_columns, constant)
      item$recipe$factor_levels[constant] <- NULL
      item$recipe$imputations[constant] <- NULL
    }
    processed[[partition]] <- item
  }
  list(processed = processed, removed = constant)
}

validate_finite_guided_data <- function(data, target) {
  numeric_predictors <- setdiff(names(data)[vapply(data, is.numeric, logical(1))], target)
  invalid <- numeric_predictors[vapply(
    data[numeric_predictors],
    function(x) any(!is.finite(x)),
    logical(1)
  )]
  if (length(invalid)) {
    stop("Numeric predictors must be finite after preprocessing: ",
         paste(invalid, collapse = ", "), ".", call. = FALSE)
  }
  invisible(TRUE)
}

fit_base_candidates <- function(data,
                                target,
                                features,
                                task,
                                model_set = "quick",
                                tuning_data = NULL,
                                enable_preprocessing = TRUE,
                                preprocessing_config = list(),
                                max_models = 10L,
                                nfolds = 5L,
                                tuning_rule = "one_se",
                                tuning_control = NULL,
                                seed = 123L,
                                learners = c("linear", "tree", "neural")) {
  primary_formula <- safe_reformulate(features, response = target)
  baseline_formula <- safe_reformulate(character(), response = target)
  if (task == "regression") {
    baseline <- timed_model_fit(function() stats::lm(baseline_formula, data = data))
    if (!identical(model_set, "tuned")) {
      primary <- timed_model_fit(function() stats::lm(primary_formula, data = data))
    }
    label <- "linear regression"
  } else if (task == "binary") {
    baseline <- timed_model_fit(function() {
      stats::glm(baseline_formula, data = data, family = stats::binomial())
    })
    if (!identical(model_set, "tuned")) {
      primary <- timed_model_fit(function() {
        stats::glm(primary_formula, data = data, family = stats::binomial())
      })
    }
    label <- "logistic regression"
  } else {
    baseline <- timed_model_fit(function() {
      nnet::multinom(baseline_formula, data = data, trace = FALSE)
    })
    if (!identical(model_set, "tuned")) {
      primary <- timed_model_fit(function() {
        nnet::multinom(primary_formula, data = data, trace = FALSE)
      })
    }
    label <- "multinomial logistic regression"
  }
  tuning <- NULL
  if (identical(model_set, "tuned")) {
    tuning <- tune_supervised_candidates(
      raw_data = tuning_data,
      target = target,
      task = task,
      enable_preprocessing = enable_preprocessing,
      preprocessing_config = preprocessing_config,
      max_models = max_models,
      nfolds = nfolds,
      selection_rule = tuning_rule,
      control = tuning_control,
      seed = seed,
      learners = learners
    )
    refitted <- refit_tuned_candidates(tuning, data, target, task)
    tuning <- refitted$tuning
    fits <- c(refitted$fits, list(simple_baseline = baseline))
    labels <- c(refitted$labels, simple_baseline = "intercept-only baseline")
    roles <- c(refitted$roles, simple_baseline = "baseline")
  } else {
    fits <- list(main_model = primary, simple_baseline = baseline)
    labels <- c(main_model = label, simple_baseline = "intercept-only baseline")
    roles <- c(main_model = "primary", simple_baseline = "baseline")
  }
  if (identical(model_set, "comparison")) {
    method <- if (task == "regression") "anova" else "class"
    small_minsplit <- max(4L, min(20L, as.integer(floor(nrow(data) * 0.10))))
    flexible_minsplit <- max(4L, min(10L, as.integer(floor(nrow(data) * 0.05))))
    fits$small_tree <- timed_model_fit(function() {
      rpart::rpart(
        primary_formula,
        data = data,
        method = method,
        control = rpart::rpart.control(
          cp = 0.01, maxdepth = 2L, minsplit = small_minsplit, xval = 0L
        )
      )
    })
    fits$flexible_tree <- timed_model_fit(function() {
      rpart::rpart(
        primary_formula,
        data = data,
        method = method,
        control = rpart::rpart.control(
          cp = 0.001, maxdepth = 5L, minsplit = flexible_minsplit, xval = 0L
        )
      )
    })
    labels <- c(
      labels,
      small_tree = "small decision tree",
      flexible_tree = "flexible decision tree"
    )
    roles <- c(roles, small_tree = "candidate", flexible_tree = "candidate")
  }
  models <- lapply(fits, `[[`, "model")
  diagnostics <- data.frame(
    model_id = names(models),
    training_time_ms = vapply(fits, `[[`, numeric(1), "elapsed_ms"),
    model_size_kb = vapply(
      models,
      function(model) as.numeric(utils::object.size(model)) / 1024,
      numeric(1)
    ),
    complexity = vapply(models, model_complexity, numeric(1)),
    fit_warning = vapply(
      fits,
      function(item) paste(unique(item$warnings), collapse = " | "),
      character(1)
    ),
    stringsAsFactors = FALSE
  )
  list(
    models = models,
    labels = labels,
    roles = roles,
    diagnostics = diagnostics,
    tuning = tuning
  )
}

refit_tuned_candidates <- function(tuning,
                                   data,
                                   target,
                                   task,
                                   fitter = fit_tuning_configuration) {
  valid <- tuning$candidates[
    tuning$candidates$status == "ok" & is.finite(tuning$candidates$cv_score),
    , drop = FALSE
  ]
  if (!nrow(valid) || sum(valid$selected) != 1L) {
    stop("Tuning must contain exactly one valid selected configuration.", call. = FALSE)
  }
  tuning$candidates$retained_model_id <- NA_character_
  tuning$candidates$refit_status <- "not_attempted"
  tuning$candidates$refit_role <- NA_character_
  tuning$candidates$refit_warning <- ""
  tuning$candidates$refit_error <- ""

  refit_state <- new.env(parent = emptyenv())
  refit_state$candidates <- tuning$candidates
  refit_state$attempts <- list()
  refit_state$attempted_ids <- character()
  record_attempt <- function(row, role, model_id, result, fit_spec) {
    id <- row$configuration_id[[1L]]
    candidates <- refit_state$candidates
    candidate_index <- match(id, candidates$configuration_id)
    candidates$refit_status[[candidate_index]] <- if (result$ok) "ok" else "failed"
    candidates$refit_role[[candidate_index]] <- role
    candidates$refit_warning[[candidate_index]] <- paste(
      unique(result$warnings), collapse = " | "
    )
    candidates$refit_error[[candidate_index]] <- result$error
    if (result$ok) {
      candidates$retained_model_id[[candidate_index]] <- model_id
    }
    refit_state$candidates <- candidates
    attempt <- data.frame(
      configuration_id = id,
      family = row$family[[1L]],
      role = role,
      model_id = model_id,
      requested_parameter_key = fit_spec$requested_parameter_key,
      effective_parameter_key = fit_spec$effective_parameter_key,
      requested_configuration_seed = fit_spec$requested_configuration_seed,
      fit_seed = fit_spec$fit_seed,
      status = if (result$ok) "ok" else "failed",
      elapsed_ms = result$elapsed_ms,
      warning = paste(unique(result$warnings), collapse = " | "),
      error = result$error,
      stringsAsFactors = FALSE
    )
    attempt$requested_parameters <- I(list(fit_spec$requested_parameters))
    attempt$effective_parameters <- I(list(fit_spec$effective_parameters))
    attempts <- refit_state$attempts
    attempts[[length(attempts) + 1L]] <- attempt
    refit_state$attempts <- attempts
    refit_state$attempted_ids <- c(refit_state$attempted_ids, id)
  }
  fit_row <- function(row, role, model_id) {
    configuration <- tuning$plan[
      tuning$plan$configuration_id == row$configuration_id[[1L]],
      , drop = FALSE
    ]
    fit_spec <- tuning_configuration_fit_spec(configuration, data, target)
    result <- safely_timed_model_fit(function() {
      fitter(configuration, data, target, task)
    })
    record_attempt(row, role, model_id, result, fit_spec)
    failure_policy <- tuning$control$failure_policy %||% "continue"
    if (!result$ok && identical(failure_policy, "stop")) {
      stop(
        "Configuration `", row$configuration_id[[1L]], "` (family `",
        row$family[[1L]], "`) failed during full-training refit under ",
        "`failure_policy = \"stop\"`: ", result$error,
        call. = FALSE
      )
    }
    result
  }

  primary_order <- order(
    !valid$selected,
    valid$cv_score,
    valid$simplicity_rank,
    valid$complexity_proxy,
    valid$configuration_id
  )
  primary_fit <- NULL
  primary_row <- NULL
  for (index in primary_order) {
    row <- valid[index, , drop = FALSE]
    role <- if (isTRUE(row$selected[[1L]])) "selected" else "fallback"
    result <- fit_row(row, role, "main_model")
    if (result$ok) {
      primary_fit <- result
      primary_row <- row
      break
    }
  }
  if (is.null(primary_fit)) {
    attempt_table <- do.call(rbind, refit_state$attempts)
    details <- unique(attempt_table$error[nzchar(attempt_table$error)])
    stop(
      "No resampling-valid configuration could be refitted on the complete training data",
      if (length(details)) paste0(": ", details[[1L]]) else ".",
      call. = FALSE
    )
  }

  fits <- list(main_model = list(
    model = primary_fit$model,
    elapsed_ms = primary_fit$elapsed_ms,
    warnings = primary_fit$warnings
  ))
  selected_id <- tuning$selected_configuration
  final_id <- primary_row$configuration_id[[1L]]
  fallback_used <- !identical(final_id, selected_id)
  labels <- c(main_model = if (fallback_used) {
    paste("refit fallback", primary_row$model[[1L]])
  } else if (primary_row$family[[1L]] == "linear") {
    paste("resampling-selected", primary_row$model[[1L]])
  } else {
    paste("tuned", primary_row$model[[1L]])
  })
  roles <- c(main_model = "primary")

  alternative_families <- setdiff(unique(valid$family), primary_row$family[[1L]])
  for (family in alternative_families) {
    family_rows <- valid[
      valid$family == family & !valid$configuration_id %in% refit_state$attempted_ids,
      , drop = FALSE
    ]
    family_rows <- family_rows[order(
      family_rows$cv_score,
      family_rows$simplicity_rank,
      family_rows$complexity_proxy,
      family_rows$configuration_id
    ), , drop = FALSE]
    if (!nrow(family_rows)) next
    model_id <- learner_definition(family)$model_id
    for (index in seq_len(nrow(family_rows))) {
      row <- family_rows[index, , drop = FALSE]
      result <- fit_row(row, "alternative", model_id)
      if (!result$ok) next
      fits[[model_id]] <- list(
        model = result$model,
        elapsed_ms = result$elapsed_ms,
        warnings = result$warnings
      )
      labels[[model_id]] <- if (family == "linear") {
        paste(row$model[[1L]], "reference")
      } else {
        paste("tuned", row$model[[1L]], "alternative")
      }
      roles[[model_id]] <- "candidate"
      break
    }
  }

  tuning$candidates <- refit_state$candidates
  attempt_table <- do.call(rbind, refit_state$attempts)
  requested_families <- tuning$learners %||% unique(tuning$candidates$family)
  resampling_failed_families <- tuning$families_resampling_failed %||% setdiff(
    requested_families,
    unique(valid$family)
  )
  retained_families <- unique(c(
    primary_row$family[[1L]],
    attempt_table$family[attempt_table$role == "alternative" & attempt_table$status == "ok"]
  ))
  refit_failed_families <- setdiff(unique(valid$family), retained_families)
  failed_families <- unique(c(resampling_failed_families, refit_failed_families))
  tuning$final_configuration <- final_id
  tuning$refit <- list(
    status = if (length(failed_families)) {
      "partial"
    } else if (fallback_used) {
      "fallback"
    } else {
      "ok"
    },
    selected_configuration = selected_id,
    final_configuration = final_id,
    fallback_used = fallback_used,
    families_requested = requested_families,
    families_retained = retained_families,
    families_resampling_failed = resampling_failed_families,
    families_refit_failed = refit_failed_families,
    families_not_retained = failed_families,
    attempts = attempt_table,
    note = paste(
      "Full-training refits were attempted independently. A failed alternative does not",
      "invalidate the primary model; if the resampling-selected fit fails, the next",
      "resampling-valid configuration is used and recorded as a fallback."
    )
  )
  list(fits = fits, labels = labels, roles = roles, tuning = tuning)
}

safely_timed_model_fit <- function(callback) {
  started <- proc.time()[["elapsed"]]
  warnings <- character()
  error <- ""
  model <- tryCatch(
    withCallingHandlers(
      callback(),
      warning = function(condition) {
        warnings <<- c(warnings, conditionMessage(condition))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(condition) {
      error <<- conditionMessage(condition)
      NULL
    }
  )
  list(
    ok = !is.null(model) && !nzchar(error),
    model = model,
    elapsed_ms = max(0, 1000 * (proc.time()[["elapsed"]] - started)),
    warnings = warnings,
    error = error
  )
}

timed_model_fit <- function(callback) {
  started <- proc.time()[["elapsed"]]
  warnings <- character()
  model <- withCallingHandlers(
    callback(),
    warning = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
  list(
    model = model,
    elapsed_ms = max(0, 1000 * (proc.time()[["elapsed"]] - started)),
    warnings = warnings
  )
}

model_complexity <- function(model) {
  if (inherits(model, "autoxplain_fitted_model")) {
    return(fitted_model_complexity(model))
  }
  if (inherits(model, "rpart")) {
    return(sum(as.character(model$frame$var) == "<leaf>"))
  }
  if (inherits(model, "autoxplain_tuned_nnet")) {
    return(length(model$model$wts))
  }
  coefficients <- tryCatch(stats::coef(model), error = function(error) numeric())
  sum(is.finite(as.numeric(coefficients)))
}

evaluate_candidates <- function(models,
                                data,
                                target,
                                task,
                                labels,
                                roles,
                                diagnostics = NULL,
                                primary_metric = NULL,
                                primary_model_id = "main_model") {
  if (!is.character(primary_model_id) || length(primary_model_id) != 1L ||
        is.na(primary_model_id) || !primary_model_id %in% names(models)) {
    stop("`primary_model_id` must identify one supplied model.", call. = FALSE)
  }
  evaluated <- lapply(names(models), function(id) {
    explainer <- explain_model(models[[id]], data, target, task = task, label = labels[[id]])
    started <- proc.time()[["elapsed"]]
    predictions <- predict(explainer, explainer$data)
    prediction_time_ms <- max(0, 1000 * (proc.time()[["elapsed"]] - started))
    calibration <- if (identical(task, "regression")) {
      NULL
    } else {
      calibration_from_explainer(explainer, predicted = predictions)
    }
    metrics <- evaluate_predictions(explainer$y, predictions, explainer)
    if (!is.null(calibration)) {
      metrics <- c(metrics, calibration_error = calibration$calibration_error)
    }
    list(
      metrics = metrics,
      predictions = predictions,
      prediction_time_ms = prediction_time_ms,
      calibration = calibration,
      explainer = explainer
    )
  })
  names(evaluated) <- names(models)
  metrics <- lapply(evaluated, `[[`, "metrics")
  primary_metric <- primary_metric %||% if (task == "regression") "rmse" else "log_loss"
  if (!is.character(primary_metric) || length(primary_metric) != 1L ||
        !all(vapply(metrics, function(x) primary_metric %in% names(x), logical(1)))) {
    stop("The requested held-out primary metric is unavailable.", call. = FALSE)
  }
  scores <- vapply(metrics, function(x) x[[primary_metric]], numeric(1))
  winner <- names(which.min(scores))[[1L]]
  baseline_score <- scores[["simple_baseline"]]
  primary_score <- scores[[primary_model_id]]
  improvement <- if (is.finite(baseline_score) && baseline_score > 0 &&
                       is.finite(primary_score)) {
    (baseline_score - primary_score) / baseline_score
  } else {
    NA_real_
  }
  metric_names <- unique(unlist(lapply(metrics, names), use.names = FALSE))
  leaderboard <- data.frame(
    rank = rank(scores, ties.method = "min"),
    model_id = names(models),
    model = unname(labels[names(models)]),
    role = unname(roles[names(models)]),
    family = vapply(names(models), function(id) {
      if (identical(roles[[id]], "baseline")) return("baseline")
      model_family_name(models[[id]])
    }, character(1)),
    backend = vapply(names(models), function(id) {
      model_backend_name(models[[id]])
    }, character(1)),
    stringsAsFactors = FALSE
  )
  for (metric in metric_names) {
    leaderboard[[metric]] <- vapply(
      metrics,
      function(x) if (metric %in% names(x)) x[[metric]] else NA_real_,
      numeric(1)
    )
  }
  if (!is.null(diagnostics)) {
    diagnostics <- diagnostics[match(leaderboard$model_id, diagnostics$model_id), , drop = FALSE]
    leaderboard$training_time_ms <- diagnostics$training_time_ms
    leaderboard$model_size_kb <- diagnostics$model_size_kb
    leaderboard$complexity <- diagnostics$complexity
    leaderboard$fit_warning <- diagnostics$fit_warning
  }
  leaderboard$prediction_time_ms <- vapply(
    leaderboard$model_id,
    function(id) evaluated[[id]]$prediction_time_ms,
    numeric(1)
  )
  leaderboard <- leaderboard[order(leaderboard$rank, leaderboard$model_id), , drop = FALSE]
  rownames(leaderboard) <- NULL
  list(
    leaderboard = leaderboard,
    model_diagnostics = leaderboard[c(
      "model_id", "training_time_ms", "prediction_time_ms", "model_size_kb", "complexity",
      "fit_warning"
    )],
    summary = list(
      primary_metric = primary_metric,
      primary_model_id = primary_model_id,
      winner = winner,
      metrics = metrics,
      improvement_over_baseline = improvement,
      beats_baseline = is.finite(improvement) && improvement > 0,
      metric_definitions = metric_definitions(task),
      evaluated_rows = nrow(data),
      predictions = guided_prediction_table(evaluated, task, primary_model_id),
      diagnostics = guided_prediction_diagnostics(evaluated, task, primary_model_id)
    )
  )
}

model_family_name <- function(model) {
  if (inherits(model, "autoxplain_fitted_model")) return(model$family)
  if (inherits(model, "H2OModel")) {
    return(tolower(tryCatch(
      as.character(model@algorithm),
      error = function(error) "h2o"
    )))
  }
  if (inherits(model, "rpart")) return("tree")
  if (inherits(model, "autoxplain_tuned_nnet")) return("neural")
  if (inherits(model, c("lm", "glm", "multinom"))) return("linear")
  class(model)[[1L]]
}

model_backend_name <- function(model) {
  if (inherits(model, "autoxplain_fitted_model")) return(model$backend)
  if (inherits(model, "H2OModel")) return("h2o")
  if (inherits(model, "rpart")) return("rpart")
  if (inherits(model, "autoxplain_tuned_nnet")) return("nnet")
  if (inherits(model, "multinom")) return("nnet")
  if (inherits(model, c("lm", "glm"))) return("stats")
  class(model)[[1L]]
}

evaluate_predictions <- function(observed, predicted, explainer) {
  if (explainer$task == "regression") {
    residual <- as.numeric(observed) - as.numeric(predicted)
    denominator <- sum((as.numeric(observed) - mean(as.numeric(observed)))^2)
    return(c(
      rmse = sqrt(mean(residual^2)),
      mae = mean(abs(residual)),
      r_squared = if (denominator > 0) 1 - sum(residual^2) / denominator else NA_real_
    ))
  }
  if (explainer$task == "binary") {
    truth <- as.character(observed) == explainer$positive
    probability <- pmin(pmax(as.numeric(predicted), 1e-15), 1 - 1e-15)
    hard <- probability >= 0.5
    sensitivity <- if (any(truth)) mean(hard[truth]) else NA_real_
    specificity <- if (any(!truth)) mean(!hard[!truth]) else NA_real_
    return(c(
      log_loss = -mean(ifelse(truth, log(probability), log1p(-probability))),
      brier_score = mean((probability - as.numeric(truth))^2),
      accuracy = mean(hard == truth),
      balanced_accuracy = if (all(is.finite(c(sensitivity, specificity)))) {
        mean(c(sensitivity, specificity))
      } else {
        NA_real_
      },
      roc_auc = guided_binary_auc(truth, probability)
    ))
  }
  probability <- as.matrix(predicted)[, explainer$class_levels, drop = FALSE]
  truth <- as.character(observed)
  selected <- probability[cbind(seq_along(truth), match(truth, explainer$class_levels))]
  predicted_class <- explainer$class_levels[max.col(probability, ties.method = "first")]
  one_hot <- matrix(0, nrow(probability), ncol(probability))
  one_hot[cbind(seq_along(truth), match(truth, explainer$class_levels))] <- 1
  recalls <- vapply(explainer$class_levels, function(level) {
    rows <- truth == level
    if (any(rows)) mean(predicted_class[rows] == level) else NA_real_
  }, numeric(1))
  c(
    log_loss = -mean(log(pmax(selected, 1e-15))),
    brier_score = mean(rowSums((probability - one_hot)^2)),
    accuracy = mean(predicted_class == truth),
    macro_recall = if (all(is.finite(recalls))) mean(recalls) else NA_real_
  )
}

guided_prediction_table <- function(evaluated, task, primary_model_id = "main_model") {
  reference_id <- if (primary_model_id %in% names(evaluated)) {
    primary_model_id
  } else {
    names(evaluated)[[1L]]
  }
  reference <- evaluated[[reference_id]]
  baseline <- evaluated[["simple_baseline"]]
  observed <- reference$explainer$y
  output <- data.frame(
    evaluation_row = seq_along(observed),
    observed = if (task == "regression") as.numeric(observed) else as.character(observed),
    stringsAsFactors = FALSE
  )
  if (task == "regression") {
    output$primary_prediction <- as.numeric(reference$predictions)
    output$baseline_prediction <- if (is.null(baseline)) {
      NA_real_
    } else {
      as.numeric(baseline$predictions)
    }
    output$error <- output$observed - output$primary_prediction
    output$absolute_error <- abs(output$error)
    return(output)
  }
  levels <- reference$explainer$class_levels
  if (task == "binary") {
    positive <- reference$explainer$positive
    negative <- setdiff(levels, positive)[[1L]]
    probability <- as.numeric(reference$predictions)
    output$primary_probability <- probability
    output$primary_prediction <- ifelse(probability >= 0.5, positive, negative)
    output$baseline_probability <- if (is.null(baseline)) {
      NA_real_
    } else {
      as.numeric(baseline$predictions)
    }
  } else {
    probability <- as.matrix(reference$predictions)[, levels, drop = FALSE]
    output$primary_prediction <- levels[max.col(probability, ties.method = "first")]
    output$primary_confidence <- apply(probability, 1L, max)
    if (!is.null(baseline)) {
      baseline_probability <- as.matrix(baseline$predictions)[, levels, drop = FALSE]
      output$baseline_prediction <- levels[max.col(baseline_probability, ties.method = "first")]
    }
  }
  output$correct <- output$observed == output$primary_prediction
  output
}

guided_prediction_diagnostics <- function(evaluated,
                                          task,
                                          primary_model_id = "main_model") {
  predictions <- guided_prediction_table(evaluated, task, primary_model_id)
  if (task == "regression") {
    return(list(
      mean_error = mean(predictions$error),
      median_absolute_error = stats::median(predictions$absolute_error),
      p90_absolute_error = as.numeric(stats::quantile(
        predictions$absolute_error,
        0.9,
        names = FALSE
      ))
    ))
  }
  confusion <- as.data.frame(
    table(observed = predictions$observed, predicted = predictions$primary_prediction),
    stringsAsFactors = FALSE
  )
  names(confusion)[[3L]] <- "rows"
  reference_id <- if (primary_model_id %in% names(evaluated)) {
    primary_model_id
  } else {
    names(evaluated)[[1L]]
  }
  list(
    confusion_matrix = confusion,
    calibration = evaluated[[reference_id]]$calibration
  )
}

guided_tuning_refit_notes <- function(tuning) {
  notes <- list()
  if (isTRUE(tuning$refit$fallback_used)) {
    selected_attempt <- tuning$refit$attempts[
      tuning$refit$attempts$configuration_id == tuning$selected_configuration,
      , drop = FALSE
    ]
    failure <- unique(selected_attempt$error[nzchar(selected_attempt$error)])
    notes[[length(notes) + 1L]] <- data.frame(
      severity = "warning",
      code = "tuning_refit_fallback",
      message = paste0(
        "The resampling-selected configuration `", tuning$selected_configuration,
        "` failed when refitted on all training rows",
        if (length(failure)) paste0(": ", failure[[1L]]) else "",
        ". The primary model therefore uses `", tuning$final_configuration, "`."
      ),
      recommendation = paste(
        "Inspect `tuning_results(result)$refit$attempts`; treat the fallback as an",
        "operational recovery and investigate why the selected refit failed."
      ),
      stringsAsFactors = FALSE
    )
  }
  if (length(tuning$refit$families_resampling_failed)) {
    notes[[length(notes) + 1L]] <- data.frame(
      severity = "note",
      code = "tuning_family_resampling_failed",
      message = paste0(
        "Every attempted configuration failed in at least one training-only fold for: ",
        paste(tuning$refit$families_resampling_failed, collapse = ", "), "."
      ),
      recommendation = paste(
        "Inspect `tuning_results(result)$fold_scores$error`; these families were excluded",
        "from selection and cannot be compared using complete out-of-fold evidence."
      ),
      stringsAsFactors = FALSE
    )
  }
  if (length(tuning$refit$families_refit_failed)) {
    notes[[length(notes) + 1L]] <- data.frame(
      severity = "note",
      code = "tuning_alternative_refit_failed",
      message = paste0(
        "No full-training alternative was retained for: ",
        paste(tuning$refit$families_refit_failed, collapse = ", "), "."
      ),
      recommendation = paste(
        "The primary model remains usable. Inspect the refit attempts before comparing",
        "model families, because the final leaderboard omits these alternatives."
      ),
      stringsAsFactors = FALSE
    )
  }
  if (!length(notes)) {
    return(data.frame(
      severity = character(), code = character(), message = character(),
      recommendation = character(), stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, notes)
}

guided_evaluation_notes <- function(training,
                                    evaluation,
                                    target,
                                    task,
                                    summary,
                                    constant_features = character(),
                                    fit_warnings = character(),
                                    evaluation_role = "evaluation") {
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
  if (nrow(evaluation) < 50L) {
    add(
      "caution",
      "small_evaluation_set",
      paste0(
        "Only ", nrow(evaluation), " rows were available for ",
        evaluation_role, " scoring."
      ),
      "Treat the scores as preliminary and validate on more representative rows."
    )
  }
  if (length(constant_features)) {
    add(
      "note",
      "constant_inputs_removed",
      paste0("Constant training inputs were removed: ", paste(constant_features, collapse = ", "), "."),
      "Check whether constant inputs indicate a data-export or sampling problem."
    )
  }
  warned_models <- names(fit_warnings)[nzchar(fit_warnings)]
  if (length(warned_models)) {
    add(
      "warning",
      "model_fit_warning",
      paste0("Numerical fitting warnings occurred for: ",
             paste(warned_models, collapse = ", "), "."),
      paste(
        "Inspect the recorded model diagnostics for separation, convergence, or",
        "instability before trusting probabilities or coefficients."
      )
    )
  }
  n_features <- ncol(training) - 1L
  if (nrow(training) / max(1L, n_features) < 10) {
    add(
      "caution",
      "few_rows_per_feature",
      paste0(nrow(training), " training rows were used with ", n_features, " input features."),
      "Use fewer justified features or more training data, and expect unstable coefficients."
    )
  }
  if (task %in% c("binary", "multiclass")) {
    counts <- table(evaluation[[target]])
    if (any(counts < 5L)) {
      scarce <- paste(names(counts)[counts < 5L], collapse = ", ")
      add(
        "caution",
        "few_rows_in_class",
        paste0("Fewer than five evaluation rows were observed for: ", scarce, "."),
        "Collect more evaluation examples for each class before trusting class-specific metrics."
      )
    }
  }
  if (identical(summary$beats_baseline, FALSE)) {
    add(
      "warning",
      "baseline_not_beaten",
      "The primary model did not improve on the intercept-only baseline.",
      "Revisit data quality and model assumptions before interpreting fitted patterns as useful."
    )
  }
  if (task == "regression" && is.finite(summary$metrics$main_model[["r_squared"]]) &&
        summary$metrics$main_model[["r_squared"]] < 0) {
    add(
      "warning",
      "negative_heldout_r_squared",
      "Evaluation R-squared is negative, so squared error exceeded an evaluation-mean reference.",
      "Do not rely on this model for prediction without substantially better validation performance."
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

guided_binary_auc <- function(truth, score) {
  positives <- sum(truth)
  negatives <- sum(!truth)
  if (!positives || !negatives) return(NA_real_)
  ranks <- rank(score, ties.method = "average")
  (sum(ranks[truth]) - positives * (positives + 1) / 2) / (positives * negatives)
}

metric_definitions <- function(task) {
  if (task == "regression") {
    return(c(
      rmse = "Typical prediction error, with larger mistakes weighted more heavily; lower is better.",
      mae = "Average absolute prediction error in the target's units; lower is better.",
      r_squared = paste(
        "Share of evaluation-set variation explained relative to predicting the",
        "evaluation-set mean; higher is better."
      )
    ))
  }
  definitions <- c(
    log_loss = "Probability error that penalizes confident wrong answers; lower is better.",
    brier_score = if (task == "binary") {
      paste(
        "Mean squared error of the positive-class probability, from 0 (best)",
        "to 1 (worst); lower is better."
      )
    } else {
      paste(
        "Unscaled mean sum of squared errors across all class probabilities,",
        "from 0 (best) to 2 (worst); lower is better. Its scale depends on the",
        "class set, so do not compare it directly with binary Brier scores or",
        "scores for a differently defined outcome."
      )
    },
    calibration_error = paste(
      "Average absolute gap between grouped probabilities and observed frequencies;",
      "lower is better, but the value depends on the evaluation sample and grouping."
    ),
    accuracy = "Share of evaluation rows assigned to the correct class; higher is better."
  )
  if (task == "binary") {
    return(c(
      definitions,
      balanced_accuracy = paste(
        "Average recall across the positive and negative classes; higher is better.",
        "Unavailable when either class is absent from the evaluation rows."
      ),
      roc_auc = "Chance that a random positive receives a higher score than a random negative; higher is better."
    ))
  }
  c(
    definitions,
    macro_recall = paste(
      "Recall calculated for each class and then averaged equally; higher is better.",
      "Unavailable when any trained class is absent from the evaluation rows."
    )
  )
}
