fit_guided_base <- function(data,
                            target_column,
                            test_data,
                            test_fraction,
                            seed,
                            task,
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
  processed <- preprocess_guided_split(
    split$training,
    split$evaluation,
    target_column,
    resolved_task,
    enable_preprocessing,
    config
  )
  train <- processed$training$data
  evaluation_data <- processed$evaluation$data
  features <- setdiff(names(train), target_column)
  if (!length(features)) {
    stop("At least one predictor must remain after preprocessing.", call. = FALSE)
  }

  fit <- with_preserved_seed(
    seed,
    fit_base_candidates(train, target_column, features, resolved_task)
  )
  evaluated <- evaluate_candidates(
    fit$models,
    evaluation_data,
    target_column,
    resolved_task,
    fit$labels,
    fit$roles
  )

  structure(
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
      model_characteristics = NULL,
      preprocessing_metadata = list(
        enabled = enable_preprocessing,
        training_data = processed$training,
        test_data = processed$evaluation
      ),
      provenance = list(
        created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
        seed = seed,
        engine_requested = "base",
        evaluation_role = "held-out test",
        split_method = split$method,
        test_fraction_requested = if (is.null(test_data)) test_fraction else NA_real_,
        training_rows = nrow(train),
        evaluation_rows = nrow(evaluation_data),
        rows_moved_for_unseen_levels = split$moved_for_unseen_levels,
        baseline = "intercept-only model",
        primary_model = fit$labels[["main_model"]]
      )
    ),
    class = "autoxplain_result"
  )
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
      preprocess_for_h2o,
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
  unsupported <- predictors[!vapply(data[predictors], is.atomic, logical(1))]
  if (length(unsupported)) {
    stop(
      "The guided workflow does not support list-like predictors: ",
      paste(unsupported, collapse = ", "), ". Use `explain_model()` with a custom model instead.",
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

fit_base_candidates <- function(data, target, features, task) {
  primary_formula <- stats::reformulate(features, response = target)
  baseline_formula <- stats::reformulate(character(), response = target)
  started <- proc.time()[["elapsed"]]
  if (task == "regression") {
    baseline <- stats::lm(baseline_formula, data = data)
    primary <- stats::lm(primary_formula, data = data)
    label <- "linear regression"
  } else if (task == "binary") {
    baseline <- stats::glm(baseline_formula, data = data, family = stats::binomial())
    primary <- stats::glm(primary_formula, data = data, family = stats::binomial())
    label <- "logistic regression"
  } else {
    baseline <- nnet::multinom(baseline_formula, data = data, trace = FALSE)
    primary <- nnet::multinom(primary_formula, data = data, trace = FALSE)
    label <- "multinomial logistic regression"
  }
  elapsed <- proc.time()[["elapsed"]] - started
  models <- list(main_model = primary, simple_baseline = baseline)
  list(
    models = models,
    labels = c(main_model = label, simple_baseline = "intercept-only baseline"),
    roles = c(main_model = "primary", simple_baseline = "baseline"),
    elapsed = elapsed
  )
}

evaluate_candidates <- function(models, data, target, task, labels, roles) {
  metrics <- lapply(names(models), function(id) {
    explainer <- explain_model(models[[id]], data, target, task = task, label = labels[[id]])
    predictions <- predict(explainer, explainer$data)
    evaluate_predictions(explainer$y, predictions, explainer)
  })
  names(metrics) <- names(models)
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
  leaderboard <- leaderboard[order(leaderboard$rank, leaderboard$model_id), , drop = FALSE]
  rownames(leaderboard) <- NULL
  list(
    leaderboard = leaderboard,
    summary = list(
      primary_metric = primary_metric,
      winner = winner,
      metrics = metrics,
      improvement_over_baseline = improvement,
      beats_baseline = is.finite(improvement) && improvement > 0,
      metric_definitions = metric_definitions(task),
      evaluated_rows = nrow(data)
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
      accuracy = mean(hard == truth),
      balanced_accuracy = mean(c(sensitivity, specificity), na.rm = TRUE),
      roc_auc = guided_binary_auc(truth, probability)
    ))
  }
  probability <- as.matrix(predicted)[, explainer$class_levels, drop = FALSE]
  truth <- as.character(observed)
  selected <- probability[cbind(seq_along(truth), match(truth, explainer$class_levels))]
  predicted_class <- explainer$class_levels[max.col(probability, ties.method = "first")]
  recalls <- vapply(explainer$class_levels, function(level) {
    rows <- truth == level
    if (any(rows)) mean(predicted_class[rows] == level) else NA_real_
  }, numeric(1))
  c(
    log_loss = -mean(log(pmax(selected, 1e-15))),
    accuracy = mean(predicted_class == truth),
    macro_recall = mean(recalls, na.rm = TRUE)
  )
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
