#' Inspect automatic tuning evidence
#'
#' Returns the complete training-only resampling record created by
#' `autoxplain(..., model_set = "tuned")`. The outer evaluation rows are never
#' used to rank configurations.
#'
#' @param result An `autoxplain_result` fitted with `model_set = "tuned"`.
#'
#' @return An object of class `autoxplain_tuning` containing the candidate table,
#'   fold-level scores, selected configuration, resampling boundary, and search
#'   settings.
#' @export
#'
#' @examples
#' tuned <- autoxplain(iris, "Sepal.Length", model_set = "tuned",
#'                     max_models = 4, nfolds = 3, seed = 2026)
#' tuning_results(tuned)
tuning_results <- function(result) {
  if (!inherits(result, "autoxplain_result")) {
    stop("`result` must be returned by `autoxplain()`.", call. = FALSE)
  }
  if (!inherits(result$tuning, "autoxplain_tuning")) {
    stop(
      "No local tuning record is available. Use `model_set = \"tuned\"`.",
      call. = FALSE
    )
  }
  result$tuning
}

#' @export
print.autoxplain_tuning <- function(x, ...) {
  selected <- x$candidates[x$candidates$selected, , drop = FALSE]
  cat("<AutoXplainR training-only tuning>\n")
  cat("  search:     ", nrow(x$candidates), " configurations across ",
      length(unique(x$candidates$family)), " model families\n", sep = "")
  cat("  resampling: ", x$folds_used, " folds; ", x$metric,
      " minimized\n", sep = "")
  cat("  rule:       ", tuning_rule_label(x$selection_rule), "\n", sep = "")
  cat("  selected:   ", selected$model[[1L]], " (",
      selected$configuration_id[[1L]], ")\n", sep = "")
  cat("  score:      ", format(round(selected$cv_score[[1L]], 5L), trim = TRUE),
      " +/- ", format(round(selected$cv_se[[1L]], 5L), trim = TRUE), " SE\n", sep = "")
  cat("  boundary:   ", x$scope_note, "\n", sep = "")
  display <- x$candidates[c(
    "configuration_id", "model", "hyperparameters", "cv_score", "cv_se",
    "complexity_proxy", "selected", "status"
  )]
  print.data.frame(display, row.names = FALSE, ...)
  invisible(x)
}

tune_supervised_candidates <- function(raw_data,
                                       target,
                                       task,
                                       enable_preprocessing,
                                       preprocessing_config,
                                       max_models,
                                       nfolds,
                                       selection_rule) {
  plan <- local_tuning_plan(
    max_models = max_models,
    n = nrow(raw_data),
    p = ncol(raw_data) - 1L,
    task = task,
    n_classes = if (task == "regression") 1L else length(levels(raw_data[[target]]))
  )
  fold_assignment <- tuning_fold_assignment(
    raw_data[[target]], task = task, requested = nfolds
  )
  folds <- lapply(seq_len(fold_assignment$folds), function(fold) {
    prepare_tuning_fold(
      raw_data = raw_data,
      target = target,
      task = task,
      fold_assignment = fold_assignment$id,
      fold = fold,
      enable_preprocessing = enable_preprocessing,
      preprocessing_config = preprocessing_config
    )
  })

  fold_rows <- list()
  row_index <- 0L
  for (configuration in seq_len(nrow(plan))) {
    for (fold in seq_along(folds)) {
      row_index <- row_index + 1L
      fold_rows[[row_index]] <- score_tuning_configuration(
        plan[configuration, , drop = FALSE], folds[[fold]], target, task, fold
      )
    }
  }
  fold_scores <- do.call(rbind, fold_rows)
  candidates <- summarize_tuning_candidates(plan, fold_scores, fold_assignment$folds, task)
  valid <- candidates$status == "ok" & is.finite(candidates$cv_score)
  if (!any(valid)) {
    detail <- unique(fold_scores$error[nzchar(fold_scores$error)])
    stop(
      "Automatic tuning could not fit a complete configuration across every fold",
      if (length(detail)) paste0(": ", detail[[1L]]) else ".",
      call. = FALSE
    )
  }
  best_index <- which(valid)[which.min(candidates$cv_score[valid])][[1L]]
  eligible <- if (selection_rule == "one_se") {
    valid & candidates$cv_score <= candidates$cv_score[[best_index]] +
      candidates$cv_se[[best_index]]
  } else {
    seq_len(nrow(candidates)) == best_index
  }
  selected_index <- which(eligible)[order(
    candidates$complexity_proxy[eligible],
    candidates$cv_score[eligible],
    candidates$configuration_id[eligible]
  )][[1L]]
  candidates$selected <- seq_len(nrow(candidates)) == selected_index
  selected_id <- candidates$configuration_id[[selected_index]]
  plan$selected <- plan$configuration_id == selected_id
  candidates <- candidates[order(
    !candidates$selected,
    candidates$status != "ok",
    candidates$cv_score,
    candidates$complexity_proxy,
    candidates$configuration_id
  ), , drop = FALSE]
  rownames(candidates) <- NULL

  structure(
    list(
      method = "K-fold resampling with fold-specific preprocessing",
      metric = if (task == "regression") "rmse" else "log_loss",
      direction = "minimize",
      selection_rule = selection_rule,
      folds_requested = nfolds,
      folds_used = fold_assignment$folds,
      configurations_requested = max_models,
      candidates = candidates,
      fold_scores = fold_scores,
      plan = plan,
      selected_configuration = selected_id,
      scope_note = paste(
        "Configuration ranking used only resamples of the outer training rows;",
        "the held-out evaluation rows were untouched until final scoring."
      )
    ),
    class = "autoxplain_tuning"
  )
}

local_tuning_plan <- function(max_models, n, p, task, n_classes) {
  max_models <- assert_count(max_models, "max_models")
  if (max_models < 3L) {
    stop("`max_models` must be at least 3 for local tuning.", call. = FALSE)
  }
  max_models <- min(max_models, 25L)
  tree <- data.frame(
    maxdepth = c(2L, 4L, 6L, 10L, 3L, 5L, 8L, 10L, 2L, 4L, 6L, 10L),
    cp = c(0.03, 0.01, 0.003, 0.0005, 0.01, 0.003, 0.001, 0, 0.01, 0.003, 0.001, 0),
    minsplit_fraction = c(0.20, 0.12, 0.08, 0.05, 0.20, 0.08, 0.05, 0.03,
                          0.08, 0.05, 0.03, 0.02),
    stringsAsFactors = FALSE
  )
  tree$minsplit <- pmax(4L, as.integer(round(n * tree$minsplit_fraction)))
  tree$minsplit_fraction <- NULL
  tree$family <- "decision_tree"
  tree$size <- NA_integer_
  tree$decay <- NA_real_
  tree$complexity_proxy <- pmax(
    2,
    pmin(2^tree$maxdepth, ceiling(n / tree$minsplit))
  )

  neural <- data.frame(
    size = c(1L, 2L, 4L, 6L, 8L, 2L, 4L, 6L, 8L, 12L, 12L, 16L),
    decay = c(0.1, 0.03, 0.01, 0.01, 0.001, 0.001, 0.001, 0, 0, 0.001, 0, 0.0001),
    stringsAsFactors = FALSE
  )
  neural$family <- "neural_network"
  neural$maxdepth <- NA_integer_
  neural$cp <- NA_real_
  neural$minsplit <- NA_integer_
  outputs <- if (task == "multiclass") n_classes else 1L
  neural$complexity_proxy <- (p + 1L) * neural$size +
    (neural$size + 1L) * outputs

  tunable_count <- max_models - 1L
  tree_count <- ceiling(tunable_count / 2)
  neural_count <- floor(tunable_count / 2)
  interleaved <- list()
  for (index in seq_len(max(tree_count, neural_count))) {
    if (index <= tree_count) {
      interleaved[[length(interleaved) + 1L]] <- tree[index, , drop = FALSE]
    }
    if (index <= neural_count) {
      interleaved[[length(interleaved) + 1L]] <- neural[index, , drop = FALSE]
    }
  }
  tunable <- do.call(rbind, interleaved)
  reference_complexity <- if (task == "multiclass") {
    (p + 1L) * (n_classes - 1L)
  } else {
    p + 1L
  }
  reference <- data.frame(
    maxdepth = NA_integer_, cp = NA_real_, minsplit = NA_integer_,
    family = "statistical_reference", size = NA_integer_, decay = NA_real_,
    complexity_proxy = max(1, reference_complexity),
    stringsAsFactors = FALSE
  )
  plan <- rbind(reference, tunable)
  counts <- stats::ave(seq_len(nrow(plan)), plan$family, FUN = seq_along)
  prefixes <- c(
    statistical_reference = "reference",
    decision_tree = "tree",
    neural_network = "neural"
  )
  plan$configuration_id <- paste0(prefixes[plan$family], "_", sprintf("%02d", counts))
  plan$model <- unname(c(
    statistical_reference = switch(
      task,
      regression = "linear regression",
      binary = "logistic regression",
      multiclass = "multinomial logistic regression"
    ),
    decision_tree = "decision tree",
    neural_network = "neural network"
  )[plan$family])
  plan$hyperparameters <- vapply(seq_len(nrow(plan)), function(index) {
    tuning_hyperparameters(plan[index, , drop = FALSE])
  }, character(1))
  plan$selected <- FALSE
  plan[c(
    "configuration_id", "family", "model", "hyperparameters", "maxdepth", "cp",
    "minsplit", "size", "decay", "complexity_proxy", "selected"
  )]
}

tuning_hyperparameters <- function(configuration) {
  family <- configuration$family[[1L]]
  if (family == "statistical_reference") return("default statistical fit")
  if (family == "decision_tree") {
    return(paste0(
      "max depth = ", configuration$maxdepth[[1L]],
      ", pruning cp = ", format(configuration$cp[[1L]], trim = TRUE),
      ", minimum split = ", configuration$minsplit[[1L]]
    ))
  }
  paste0(
    "hidden units = ", configuration$size[[1L]],
    ", weight decay = ", format(configuration$decay[[1L]], trim = TRUE)
  )
}

tuning_fold_assignment <- function(y, task, requested) {
  requested <- assert_count(requested, "nfolds")
  if (requested < 2L) stop("`nfolds` must be at least 2 for tuning.", call. = FALSE)
  n <- length(y)
  maximum <- if (task == "regression") {
    floor(n / 2L)
  } else {
    min(table(y, useNA = "no"))
  }
  folds <- min(requested, maximum)
  if (folds < 2L) {
    stop("At least two resampling folds require two usable rows per outcome class.",
         call. = FALSE)
  }
  id <- integer(n)
  if (task == "regression") {
    order <- sample.int(n)
    id[order] <- rep(seq_len(folds), length.out = n)
  } else {
    groups <- split(seq_len(n), as.character(y), drop = TRUE)
    for (group in groups) {
      order <- sample(group)
      id[order] <- rep(seq_len(folds), length.out = length(group))
    }
  }
  list(id = id, folds = folds)
}

prepare_tuning_fold <- function(raw_data,
                                target,
                                task,
                                fold_assignment,
                                fold,
                                enable_preprocessing,
                                preprocessing_config) {
  training <- raw_data[fold_assignment != fold, , drop = FALSE]
  validation <- raw_data[fold_assignment == fold, , drop = FALSE]
  repaired <- keep_known_predictor_levels(training, validation, target)
  if (nrow(repaired$evaluation) < 2L) {
    stop(
      "A tuning fold retained fewer than two validation rows after categorical-level checks.",
      call. = FALSE
    )
  }
  processed <- preprocess_guided_split(
    repaired$training,
    repaired$evaluation,
    target,
    task,
    enable_preprocessing,
    utils::modifyList(preprocessing_config, list(verbose = FALSE))
  )
  constant <- remove_guided_constant_predictors(processed, target)
  processed <- constant$processed
  if (nrow(processed$evaluation$data) < 2L) {
    stop("A tuning fold retained fewer than two rows after preprocessing.", call. = FALSE)
  }
  list(
    training = processed$training$data,
    validation = processed$evaluation$data,
    rows_moved_for_unseen_levels = repaired$moved,
    removed_features = constant$removed
  )
}

score_tuning_configuration <- function(configuration, fold, target, task, fold_id) {
  started <- proc.time()[["elapsed"]]
  error <- ""
  fit_warning <- character()
  score <- NA_real_
  result <- tryCatch(
    withCallingHandlers(
      {
        model <- fit_tuning_configuration(configuration, fold$training, target, task)
        explainer <- explain_model(
          model,
          fold$validation,
          target,
          task = task,
          label = configuration$model[[1L]]
        )
        predictions <- predict(explainer, explainer$data)
        evaluate_predictions(explainer$y, predictions, explainer)[[
          if (task == "regression") "rmse" else "log_loss"
        ]]
      },
      warning = function(condition) {
        fit_warning <<- c(fit_warning, conditionMessage(condition))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(condition) {
      error <<- conditionMessage(condition)
      NA_real_
    }
  )
  score <- result
  data.frame(
    configuration_id = configuration$configuration_id[[1L]],
    fold = fold_id,
    score = score,
    validation_rows = nrow(fold$validation),
    rows_moved_for_unseen_levels = fold$rows_moved_for_unseen_levels,
    elapsed_ms = max(0, 1000 * (proc.time()[["elapsed"]] - started)),
    warning = paste(unique(fit_warning), collapse = " | "),
    error = error,
    stringsAsFactors = FALSE
  )
}

summarize_tuning_candidates <- function(plan, fold_scores, expected_folds, task) {
  rows <- lapply(seq_len(nrow(plan)), function(index) {
    id <- plan$configuration_id[[index]]
    scores <- fold_scores[fold_scores$configuration_id == id, , drop = FALSE]
    complete <- sum(is.finite(scores$score))
    cv_score <- if (complete == expected_folds) {
      if (task == "regression") {
        sqrt(stats::weighted.mean(scores$score^2, scores$validation_rows))
      } else {
        stats::weighted.mean(scores$score, scores$validation_rows)
      }
    } else {
      NA_real_
    }
    data.frame(
      configuration_id = id,
      family = plan$family[[index]],
      model = plan$model[[index]],
      hyperparameters = plan$hyperparameters[[index]],
      cv_score = cv_score,
      cv_sd = if (complete > 1L) stats::sd(scores$score[is.finite(scores$score)]) else NA_real_,
      cv_se = if (complete > 1L) {
        stats::sd(scores$score[is.finite(scores$score)]) / sqrt(complete)
      } else {
        NA_real_
      },
      folds_completed = complete,
      evaluated_rows = sum(scores$validation_rows[is.finite(scores$score)]),
      complexity_proxy = plan$complexity_proxy[[index]],
      selected = FALSE,
      status = if (complete == expected_folds) "ok" else "failed",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

fit_tuning_configuration <- function(configuration, data, target, task) {
  family <- configuration$family[[1L]]
  features <- setdiff(names(data), target)
  formula <- stats::reformulate(features, response = target)
  if (family == "statistical_reference") {
    if (task == "regression") return(stats::lm(formula, data = data))
    if (task == "binary") {
      return(stats::glm(formula, data = data, family = stats::binomial()))
    }
    return(nnet::multinom(formula, data = data, trace = FALSE))
  }
  if (family == "decision_tree") {
    return(rpart::rpart(
      formula,
      data = data,
      method = if (task == "regression") "anova" else "class",
      control = rpart::rpart.control(
        cp = configuration$cp[[1L]],
        maxdepth = configuration$maxdepth[[1L]],
        minsplit = configuration$minsplit[[1L]],
        xval = 0L
      )
    ))
  }
  fit_tuned_neural_network(
    data = data,
    target = target,
    task = task,
    size = configuration$size[[1L]],
    decay = configuration$decay[[1L]]
  )
}

fit_tuned_neural_network <- function(data, target, task, size, decay) {
  features <- setdiff(names(data), target)
  feature_formula <- stats::reformulate(features)
  frame <- stats::model.frame(feature_formula, data = data, na.action = stats::na.fail)
  terms <- stats::terms(frame)
  matrix <- stats::model.matrix(terms, frame)
  if ("(Intercept)" %in% colnames(matrix)) {
    matrix <- matrix[, colnames(matrix) != "(Intercept)", drop = FALSE]
  }
  center <- colMeans(matrix)
  scale <- apply(matrix, 2L, stats::sd)
  scale[!is.finite(scale) | scale == 0] <- 1
  x <- sweep(sweep(matrix, 2L, center, "-"), 2L, scale, "/")
  y <- data[[target]]
  levels <- if (task == "regression") NULL else base::levels(y)
  y_center <- 0
  y_scale <- 1
  arguments <- list(
    x = x,
    size = size,
    decay = decay,
    maxit = 500L,
    trace = FALSE,
    rang = 0.1
  )
  if (task == "regression") {
    y_center <- mean(y)
    y_scale <- stats::sd(y)
    if (!is.finite(y_scale) || y_scale == 0) {
      stop("A regression tuning fold has no outcome variation.", call. = FALSE)
    }
    arguments$y <- (y - y_center) / y_scale
    arguments$linout <- TRUE
  } else if (task == "binary") {
    arguments$y <- as.numeric(y == levels[[2L]])
    arguments$entropy <- TRUE
  } else {
    arguments$y <- nnet::class.ind(y)
    arguments$softmax <- TRUE
  }
  outputs <- if (task == "multiclass") length(levels) else 1L
  arguments$MaxNWts <- max(
    1000L,
    as.integer((ncol(x) + 1L) * size + (size + 1L) * outputs + 100L)
  )
  fitted <- do.call(nnet::nnet, arguments)
  structure(
    list(
      model = fitted,
      terms = terms,
      columns = colnames(matrix),
      xlevels = lapply(data[features][vapply(data[features], is.factor, logical(1))], levels),
      center = center,
      scale = scale,
      y_center = y_center,
      y_scale = y_scale,
      task = task,
      class_levels = levels,
      size = size,
      decay = decay
    ),
    class = "autoxplain_tuned_nnet"
  )
}

#' @export
predict.autoxplain_tuned_nnet <- function(object, newdata, ...) {
  assert_data_frame(newdata, "newdata")
  for (feature in intersect(names(object$xlevels), names(newdata))) {
    newdata[[feature]] <- factor(
      as.character(newdata[[feature]]),
      levels = object$xlevels[[feature]]
    )
  }
  frame <- stats::model.frame(
    object$terms,
    data = newdata,
    xlev = object$xlevels,
    na.action = stats::na.fail
  )
  matrix <- stats::model.matrix(object$terms, frame)
  if ("(Intercept)" %in% colnames(matrix)) {
    matrix <- matrix[, colnames(matrix) != "(Intercept)", drop = FALSE]
  }
  if (!identical(colnames(matrix), object$columns)) {
    stop("Neural-network prediction columns do not match the fitted schema.", call. = FALSE)
  }
  x <- sweep(sweep(matrix, 2L, object$center, "-"), 2L, object$scale, "/")
  raw <- stats::predict(object$model, x, type = "raw")
  if (object$task == "regression") {
    return(as.numeric(raw) * object$y_scale + object$y_center)
  }
  if (object$task == "binary") return(as.numeric(raw))
  output <- as.matrix(raw)
  colnames(output) <- object$class_levels
  output
}

tuning_rule_label <- function(rule) {
  if (identical(rule, "one_se")) {
    "one-standard-error (prefer the simpler near-best configuration)"
  } else {
    "lowest resampled error"
  }
}
