fit_guided_base <- function(data,
                            target_column,
                            test_data,
                            test_fraction,
                            seed,
                            task,
                            model_set,
                            max_models,
                            nfolds,
                            tuning_rule,
                            enable_preprocessing,
                            preprocessing_config,
                            verbosity) {
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

  split <- if (is.null(test_data)) {
    make_evaluation_split(data, target_column, resolved_task, test_fraction, seed)
  } else {
    validate_guided_predictors(test_data, target_column)
    list(
      training = data,
      evaluation = test_data,
      method = "user-supplied test data",
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
      verbose = verbosity == "info"
    ),
    preprocessing_config
  )
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

  raw_tuning_data <- split$training[c(features, target_column)]
  raw_tuning_data[[target_column]] <- coerce_outcome_for_task(
    raw_tuning_data[[target_column]],
    resolved_task,
    target_levels = if (is.factor(train[[target_column]])) levels(train[[target_column]]) else NULL
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
    tuning_rule = tuning_rule
  ))
  evaluated <- evaluate_candidates(
    fit$models,
    evaluation_data,
    target_column,
    resolved_task,
    fit$labels,
    fit$roles,
    fit$diagnostics
  )
  evaluated$summary$notes <- guided_evaluation_notes(
    train,
    evaluation_data,
    target_column,
    resolved_task,
    evaluated$summary,
    constant_result$removed,
    stats::setNames(fit$diagnostics$fit_warning, fit$diagnostics$model_id)
  )

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
        evaluation_role = "held-out test",
        split_method = split$method,
        test_fraction_requested = if (is.null(test_data)) test_fraction else NA_real_,
        training_rows = nrow(train),
        evaluation_rows = nrow(evaluation_data),
        rows_moved_for_unseen_levels = split$moved_for_unseen_levels,
        constant_features_removed = constant_result$removed,
        baseline = "intercept-only model",
        primary_model = fit$labels[["main_model"]],
        candidate_selection = if (identical(model_set, "tuned")) {
          paste(
            "The main model was selected by", fit$tuning$folds_used,
            "fold training-only resampling using the",
            tuning_rule_label(fit$tuning$selection_rule),
            "rule; held-out evaluation rows were untouched until final scoring."
          )
        } else {
          paste(
            "The main model was pre-specified; held-out candidate ranks are descriptive",
            "and were not used to replace it."
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
  repaired <- keep_known_predictor_levels(
    data[training_indices, , drop = FALSE],
    data[indices, , drop = FALSE],
    target
  )
  if (nrow(repaired$evaluation) < 2L) {
    stop(
      "The automatic split left fewer than two evaluable rows after preserving categorical levels. ",
      "Supply `test_data` or use more observations.",
      call. = FALSE
    )
  }
  list(
    training = repaired$training,
    evaluation = repaired$evaluation,
    method = if (task == "regression") {
      "reproducible random holdout"
    } else {
      "reproducible stratified holdout"
    },
    moved_for_unseen_levels = repaired$moved
  )
}

keep_known_predictor_levels <- function(training, evaluation, target) {
  categorical <- setdiff(names(training), target)
  categorical <- categorical[vapply(
    training[categorical],
    function(x) is.factor(x) || is.character(x),
    logical(1)
  )]
  move <- rep(FALSE, nrow(evaluation))
  for (feature in categorical) {
    known <- unique(as.character(training[[feature]][!is.na(training[[feature]])]))
    value <- as.character(evaluation[[feature]])
    move <- move | (!is.na(value) & !value %in% known)
  }
  moved <- sum(move)
  if (moved) {
    training <- rbind(training, evaluation[move, , drop = FALSE])
    evaluation <- evaluation[!move, , drop = FALSE]
  }
  list(training = training, evaluation = evaluation, moved = moved)
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
      missing_value_strategy = config$missing_value_strategy
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
                                tuning_rule = "one_se") {
  primary_formula <- stats::reformulate(features, response = target)
  baseline_formula <- stats::reformulate(character(), response = target)
  if (task == "regression") {
    baseline <- timed_model_fit(function() stats::lm(baseline_formula, data = data))
    primary <- timed_model_fit(function() stats::lm(primary_formula, data = data))
    label <- "linear regression"
  } else if (task == "binary") {
    baseline <- timed_model_fit(function() {
      stats::glm(baseline_formula, data = data, family = stats::binomial())
    })
    primary <- timed_model_fit(function() {
      stats::glm(primary_formula, data = data, family = stats::binomial())
    })
    label <- "logistic regression"
  } else {
    baseline <- timed_model_fit(function() {
      nnet::multinom(baseline_formula, data = data, trace = FALSE)
    })
    primary <- timed_model_fit(function() {
      nnet::multinom(primary_formula, data = data, trace = FALSE)
    })
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
      selection_rule = tuning_rule
    )
    valid <- tuning$candidates[tuning$candidates$status == "ok", , drop = FALSE]
    selected <- valid[valid$selected, , drop = FALSE]
    retained <- selected
    for (family in setdiff(unique(valid$family), selected$family[[1L]])) {
      family_rows <- valid[valid$family == family, , drop = FALSE]
      retained <- rbind(retained, family_rows[which.min(family_rows$cv_score), , drop = FALSE])
    }
    identifiers <- c(
      statistical_reference = "reference_model",
      decision_tree = "tuned_tree",
      neural_network = "tuned_neural_network"
    )
    final_fits <- list()
    final_labels <- character()
    final_roles <- character()
    tuning$candidates$retained_model_id <- NA_character_
    for (index in seq_len(nrow(retained))) {
      row <- retained[index, , drop = FALSE]
      model_id <- if (isTRUE(row$selected[[1L]])) {
        "main_model"
      } else {
        unname(identifiers[[row$family[[1L]]]])
      }
      configuration <- tuning$plan[
        tuning$plan$configuration_id == row$configuration_id[[1L]],
        , drop = FALSE
      ]
      final_fits[[model_id]] <- timed_model_fit(function() {
        fit_tuning_configuration(configuration, data, target, task)
      })
      final_labels[[model_id]] <- if (model_id == "main_model") {
        if (row$family[[1L]] == "statistical_reference") {
          paste("resampling-selected", row$model[[1L]])
        } else {
          paste("tuned", row$model[[1L]])
        }
      } else if (row$family[[1L]] == "statistical_reference") {
        paste(row$model[[1L]], "reference")
      } else {
        paste("tuned", row$model[[1L]], "alternative")
      }
      final_roles[[model_id]] <- if (model_id == "main_model") "primary" else "candidate"
      tuning$candidates$retained_model_id[
        tuning$candidates$configuration_id == row$configuration_id[[1L]]
      ] <- model_id
    }
    fits <- c(final_fits, list(simple_baseline = baseline))
    labels <- c(final_labels, simple_baseline = "intercept-only baseline")
    roles <- c(final_roles, simple_baseline = "baseline")
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
                                diagnostics = NULL) {
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
  primary_metric <- if (task == "regression") "rmse" else "log_loss"
  scores <- vapply(metrics, function(x) x[[primary_metric]], numeric(1))
  winner <- names(which.min(scores))[[1L]]
  baseline_score <- scores[["simple_baseline"]]
  main_score <- scores[["main_model"]]
  improvement <- if (is.finite(baseline_score) && baseline_score > 0) {
    (baseline_score - main_score) / baseline_score
  } else {
    NA_real_
  }
  metric_names <- unique(unlist(lapply(metrics, names), use.names = FALSE))
  leaderboard <- data.frame(
    rank = rank(scores, ties.method = "min"),
    model_id = names(models),
    model = unname(labels[names(models)]),
    role = unname(roles[names(models)]),
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
      winner = winner,
      metrics = metrics,
      improvement_over_baseline = improvement,
      beats_baseline = is.finite(improvement) && improvement > 0,
      metric_definitions = metric_definitions(task),
      evaluated_rows = nrow(data),
      predictions = guided_prediction_table(evaluated, task),
      diagnostics = guided_prediction_diagnostics(evaluated, task)
    )
  )
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
      balanced_accuracy = mean(c(sensitivity, specificity), na.rm = TRUE),
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
    macro_recall = mean(recalls, na.rm = TRUE)
  )
}

guided_prediction_table <- function(evaluated, task) {
  reference_id <- if ("main_model" %in% names(evaluated)) "main_model" else names(evaluated)[[1L]]
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

guided_prediction_diagnostics <- function(evaluated, task) {
  predictions <- guided_prediction_table(evaluated, task)
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
  reference_id <- if ("main_model" %in% names(evaluated)) {
    "main_model"
  } else {
    names(evaluated)[[1L]]
  }
  list(
    confusion_matrix = confusion,
    calibration = evaluated[[reference_id]]$calibration
  )
}

guided_evaluation_notes <- function(training,
                                    evaluation,
                                    target,
                                    task,
                                    summary,
                                    constant_features = character(),
                                    fit_warnings = character()) {
  notes <- list()
  add <- function(severity, code, message, recommendation) {
    notes[[length(notes) + 1L]] <<- data.frame(
      severity = severity,
      code = code,
      message = message,
      recommendation = recommendation,
      stringsAsFactors = FALSE
    )
  }
  if (nrow(evaluation) < 50L) {
    add(
      "caution",
      "small_evaluation_set",
      paste0("Only ", nrow(evaluation), " rows were available for held-out evaluation."),
      "Treat the scores as preliminary and validate on more representative unseen rows."
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
        "Collect more held-out examples for each class before trusting class-specific metrics."
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
      "Held-out R-squared is negative, so squared error exceeded a held-out-mean reference.",
      "Do not rely on this model for prediction without substantially better validation performance."
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
      r_squared = "Share of held-out variation explained relative to predicting the held-out mean; higher is better."
    ))
  }
  definitions <- c(
    log_loss = "Probability error that penalizes confident wrong answers; lower is better.",
    brier_score = "Average squared probability error; lower is better.",
    calibration_error = paste(
      "Average absolute gap between grouped probabilities and observed frequencies;",
      "lower is better, but the value depends on the evaluation sample and grouping."
    ),
    accuracy = "Share of held-out rows assigned to the correct class; higher is better."
  )
  if (task == "binary") {
    return(c(
      definitions,
      balanced_accuracy = "Average recall across the positive and negative classes; higher is better.",
      roc_auc = "Chance that a random positive receives a higher score than a random negative; higher is better."
    ))
  }
  c(
    definitions,
    macro_recall = "Recall calculated for each class and then averaged equally; higher is better."
  )
}
