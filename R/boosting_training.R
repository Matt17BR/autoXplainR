# A calibration split is made before its recipe is fitted. Preparing the whole
# outer fold first would let these validation rows influence imputation and
# category handling, even though they never enter the boosting fit itself.
prepare_boosting_calibration <- function(raw_training,
                                         target,
                                         task,
                                         enable_preprocessing,
                                         preprocessing_config,
                                         seed,
                                         groups = NULL,
                                         validation_fraction = 0.2) {
  seed <- assert_count(seed, "seed", minimum = 0L)
  assert_probability(validation_fraction, "validation_fraction", open = TRUE)
  rows <- nrow(raw_training)
  outcome <- raw_training[[target]]
  skipped <- function(reason) {
    list(status = "skipped", reason = reason, seed = seed, rows_requested = rows)
  }
  if (rows < 20L) return(skipped("Fewer than 20 training rows are available for an inner split."))
  if (task != "regression" && any(table(outcome) < 2L)) {
    return(skipped("Every outcome class needs at least two rows for an inner split."))
  }
  if (!is.null(groups)) {
    if (length(groups) != rows || anyNA(groups)) {
      stop("Inner calibration groups must identify every raw training row without missing values.",
        call. = FALSE
      )
    }
    groups <- as.character(groups)
    if (length(unique(groups)) < 2L) {
      return(skipped("Fewer than two independent training groups remain for an inner split."))
    }
    folds <- min(length(unique(groups)), max(2L, as.integer(round(1 / validation_fraction))))
    if (task != "regression") {
      coverage <- table(groups, outcome) > 0L
      folds <- min(folds, min(colSums(coverage)))
      if (folds < 2L) {
        return(skipped("An outcome class occurs in fewer than two independent training groups."))
      }
    }
    assignment <- grouped_fold_ids(
      groups, requested = folds, seed = seed,
      outcome = if (task == "regression") NULL else outcome
    )
    validation_rows <- which(assignment == 1L)
  } else {
    validation_rows <- with_preserved_seed(seed, {
      if (task == "regression") {
        sort(sample.int(rows, max(2L, min(rows - 2L, as.integer(round(rows * validation_fraction))))))
      } else {
        by_class <- split(seq_len(rows), outcome, drop = TRUE)
        sort(unlist(lapply(by_class, function(indices) {
          size <- max(1L, min(length(indices) - 1L, as.integer(round(length(indices) * validation_fraction))))
          indices[sample.int(length(indices), size)]
        }), use.names = FALSE))
      }
    })
  }
  training_rows <- setdiff(seq_len(rows), validation_rows)
  if (min(length(training_rows), length(validation_rows)) < 2L) {
    return(skipped("The group-preserving inner split has fewer than two rows in a partition."))
  }
  training <- raw_training[training_rows, , drop = FALSE]
  validation <- raw_training[validation_rows, , drop = FALSE]
  assert_drop_rows_class_coverage(
    training, validation, target, task, "boosting calibration",
    enable_preprocessing, preprocessing_config
  )
  processed <- preprocess_guided_split(
    training, validation, target, task, enable_preprocessing,
    utils::modifyList(preprocessing_config, list(verbose = FALSE))
  )
  processed <- remove_guided_constant_predictors(processed, target)$processed
  if (ncol(processed$training$data) < 2L) {
    return(skipped("No predictor varies inside the inner-training partition."))
  }
  if (min(nrow(processed$training$data), nrow(processed$evaluation$data)) < 2L) {
    return(skipped("Inner preprocessing retained fewer than two rows in a partition."))
  }
  kept_training <- training_rows[processed$training$row_indices]
  kept_validation <- validation_rows[processed$evaluation$row_indices]
  list(
    status = "ready", reason = "", seed = seed,
    training = processed$training$data,
    validation = processed$evaluation$data,
    training_row = kept_training,
    validation_row = kept_validation,
    rows_requested = rows,
    training_rows_omitted = length(training_rows) - length(kept_training),
    validation_rows_omitted = length(validation_rows) - length(kept_validation),
    requested_validation_fraction = validation_fraction,
    actual_validation_fraction = length(validation_rows) / rows,
    grouped = !is.null(groups),
    training_groups = if (is.null(groups)) NULL else length(unique(groups[kept_training])),
    validation_groups = if (is.null(groups)) NULL else length(unique(groups[kept_validation])),
    recipe = processed$training$recipe,
    novel_levels_mapped = sum(processed$evaluation$preprocessing_log$novel_level_mappings %||% integer())
  )
}

boosting_stopping_callback <- function(validation, observed, task, metric, class_levels,
                                       patience, maximum_rounds) {
  state <- new.env(parent = emptyenv())
  state$validation <- validation
  state$observed <- observed
  state$contract <- list(
    task = task, class_levels = class_levels,
    positive = if (task == "binary") class_levels[[2L]] else NULL
  )
  state$metric <- metric
  state$curve <- numeric(maximum_rounds)
  state$stopping <- xgboost::xgb.cb.early.stop(
    patience, maximize = selection_metric_direction(metric) == "maximize",
    metric_name = "inner_selection", verbose = FALSE
  )
  xgboost::xgb.Callback(
    cb_name = "autoxplain_round_selection", env = state,
    f_before_training = function(env, model, data, evals, begin_iteration, end_iteration) {
      env$stopping$f_before_training(
        env$stopping$env, model, data, list(inner = env$validation), begin_iteration, end_iteration
      )
    },
    f_after_iter = function(env, model, data, evals, iteration, iter_feval) {
      # R's custom_metric receives raw margins. Scoring native predictions here
      # also preserves float probability ties, which can matter for AUC.
      predicted <- stats::predict(model, env$validation, iterationrange = "all")
      if (env$contract$task == "multiclass") {
        if (!is.matrix(predicted) || nrow(predicted) != length(env$observed) ||
              ncol(predicted) != length(env$contract$class_levels)) {
          stop("XGBoost returned an unexpected multiclass calibration shape.", call. = FALSE)
        }
        colnames(predicted) <- env$contract$class_levels
      }
      score <- tuning_prediction_loss(env$observed, predicted, env$metric, env$contract)
      if (length(score) != 1L || !is.finite(score)) {
        stop("The inner boosting calibration metric is not finite.", call. = FALSE)
      }
      env$curve[[iteration]] <- score
      env$stopping$f_after_iter(
        env$stopping$env, model, data, list(inner = env$validation), iteration,
        stats::setNames(score, "inner_selection")
      )
    },
    f_after_training = function(env, model, data, evals, iteration, final_feval, prev_cb_res) {
      record <- env$stopping$f_after_training(
        env$stopping$env, model, data, list(inner = env$validation), iteration,
        final_feval, prev_cb_res
      )
      record$curve <- data.frame(round = seq_len(iteration), score = env$curve[seq_len(iteration)])
      record
    }
  )
}

# With no validation input this is the fixed-round native fit. Calibration uses
# a separate fit and returns a round count; callers refit on all their training
# rows instead of retaining the smaller calibration model.
fit_boosting_core <- function(data,
                              target,
                              task,
                              parameters,
                              seed,
                              threads = 1L,
                              validation = NULL,
                              early_stopping_rounds = NULL,
                              metric = NULL) {
  require_optional("xgboost", "fitting gradient-boosted trees")
  threads <- assert_count(threads, "threads", minimum = 1L)
  features <- setdiff(names(data), target)
  computation <- resolve_boosting_encoding(parameters, data, target)
  native <- computation$encoding == "native"
  blueprint <- if (native) {
    fit_boosting_native_blueprint(data, features)
  } else {
    fit_matrix_blueprint(data, predictors = features)
  }
  bake <- if (native) bake_boosting_native_blueprint else bake_matrix_blueprint
  x <- bake(blueprint, data)
  class_levels <- if (task == "regression") NULL else levels(data[[target]])
  label <- function(values) {
    if (task == "regression") values else match(as.character(values), class_levels) - 1L
  }
  xgb_parameters <- list(
    objective = switch(task,
      regression = "reg:squarederror", binary = "binary:logistic", multiclass = "multi:softprob"
    ),
    eval_metric = switch(task, regression = "rmse", binary = "logloss", multiclass = "mlogloss"),
    eta = parameters$eta,
    max_depth = parameters$max_depth,
    min_child_weight = parameters$min_child_weight,
    subsample = parameters$subsample,
    colsample_bytree = parameters$colsample_bytree,
    alpha = parameters$reg_alpha,
    lambda = parameters$reg_lambda,
    nthread = threads,
    seed = seed,
    verbosity = 0L
  )
  if (task == "multiclass") xgb_parameters$num_class <- length(class_levels)
  matrix <- if (native) {
    xgb_parameters$tree_method <- "hist"
    xgb_parameters$max_bin <- computation$max_bin
    xgboost::xgb.QuantileDMatrix(
      data = x, label = label(data[[target]]), nthread = threads, max_bin = computation$max_bin
    )
  } else {
    xgboost::xgb.DMatrix(data = x, label = label(data[[target]]), nthread = threads)
  }
  arguments <- list(params = xgb_parameters, data = matrix, nrounds = parameters$nrounds, verbose = 0L)
  if (!is.null(validation)) {
    early_stopping_rounds <- assert_count(early_stopping_rounds, "early_stopping_rounds", minimum = 1L)
    metric <- metric %||% if (task == "regression") "rmse" else "log_loss"
    validation_x <- bake(blueprint, validation)
    validation_matrix <- if (native) {
      xgboost::xgb.QuantileDMatrix(
        data = validation_x, label = label(validation[[target]]), nthread = threads,
        ref = matrix, max_bin = computation$max_bin
      )
    } else {
      xgboost::xgb.DMatrix(data = validation_x, label = label(validation[[target]]), nthread = threads)
    }
    arguments$params$eval_metric <- NULL
    arguments$params$disable_default_eval_metric <- TRUE
    arguments$callbacks <- list(boosting_stopping_callback(
      validation_matrix, validation[[target]], task, metric, class_levels,
      early_stopping_rounds, parameters$nrounds
    ))
  } else if (!is.null(early_stopping_rounds)) {
    stop("Early stopping requires an explicit inner-validation partition.", call. = FALSE)
  }
  fit <- do.call(xgboost::xgb.train, arguments)
  # do.call would otherwise retain native matrices and callback closures in the
  # fitted call. The actual inputs and round-selection record live in the wrapper.
  attr(fit, "call") <- quote(xgboost::xgb.train(params = params, data = training_matrix, nrounds = nrounds))
  calibration <- if (!is.null(validation)) {
    log <- attr(fit, "autoxplain_round_selection")$curve
    rounds <- as.integer(xgboost::xgb.attr(fit, "best_iteration")) + 1L
    attempted <- nrow(log)
    best_score <- as.numeric(xgboost::xgb.attr(fit, "best_score"))
    if (length(rounds) != 1L || !is.finite(rounds) || rounds < 1L || rounds > attempted) {
      stop("XGBoost did not return a valid selected boosting round.", call. = FALSE)
    }
    list(
      selected_rounds = rounds,
      attempted_rounds = attempted,
      maximum_rounds = as.integer(parameters$nrounds),
      patience = early_stopping_rounds,
      stop_reason = if (attempted - rounds >= early_stopping_rounds) "patience_reached" else "round_limit",
      metric = metric,
      direction = selection_metric_direction(metric),
      best_score = best_score,
      curve = log
    )
  } else {
    NULL
  }
  list(
    fit = fit, blueprint = blueprint, computation = computation,
    class_levels = class_levels, calibration = calibration, threads = threads
  )
}

calibrate_boosting_rounds <- function(prepared,
                                      target,
                                      task,
                                      parameters,
                                      seed,
                                      threads = 1L,
                                      patience = 30L,
                                      metric = if (task == "regression") "rmse" else "log_loss") {
  if (identical(prepared$status, "skipped")) {
    return(list(
      rounds = as.integer(parameters$nrounds),
      evidence = list(
        status = "skipped", stop_reason = prepared$reason,
        selected_rounds = as.integer(parameters$nrounds), maximum_rounds = as.integer(parameters$nrounds),
        split_seed = prepared$seed, rows_requested = prepared$rows_requested
      )
    ))
  }
  if (!identical(prepared$status, "ready")) {
    stop("Boosting calibration requires a prepared inner split.", call. = FALSE)
  }
  trial <- fit_boosting_core(
    prepared$training, target, task, parameters, seed,
    threads = threads, validation = prepared$validation,
    early_stopping_rounds = patience, metric = metric
  )
  evidence <- c(list(
    status = "calibrated", split_seed = prepared$seed, fit_seed = as.integer(seed), threads = threads,
    training_rows = nrow(prepared$training), validation_rows = nrow(prepared$validation),
    training_rows_omitted = prepared$training_rows_omitted,
    validation_rows_omitted = prepared$validation_rows_omitted,
    grouped = prepared$grouped, training_groups = prepared$training_groups,
    validation_groups = prepared$validation_groups,
    requested_validation_fraction = prepared$requested_validation_fraction,
    actual_validation_fraction = prepared$actual_validation_fraction,
    novel_levels_mapped = prepared$novel_levels_mapped,
    predictors = setdiff(names(prepared$training), target),
    scope = paste(
      "Rounds were chosen on an inner split of this fit's training data.",
      "Preprocessing and input encoding were learned from inner-training rows only.",
      "The scored model is refitted on all fold-training rows using those rounds."
    )
  ), trial$calibration)
  list(rounds = trial$calibration$selected_rounds, evidence = evidence)
}
