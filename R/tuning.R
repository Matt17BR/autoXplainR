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
                                       selection_rule,
                                       seed = 123L,
                                       learners = c("linear", "tree", "neural")) {
  plan <- local_tuning_plan(
    max_models = max_models,
    n = nrow(raw_data),
    p = ncol(raw_data) - 1L,
    task = task,
    n_classes = if (task == "regression") 1L else length(levels(raw_data[[target]])),
    learners = learners,
    seed = seed
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
    candidates$simplicity_rank[eligible],
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
      learners = unique(plan$family),
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

local_tuning_plan <- function(max_models,
                              n,
                              p,
                              task,
                              n_classes,
                              learners = c("linear", "tree", "neural"),
                              seed = 123L) {
  max_models <- assert_count(max_models, "max_models")
  seed <- assert_count(seed, "seed", minimum = 0L)
  if (!is.character(learners) || !length(learners) || anyNA(learners) ||
        any(!nzchar(learners)) || anyDuplicated(learners)) {
    stop("`learners` must be a unique, non-empty character vector.", call. = FALSE)
  }
  registry <- autoxplain_learner_registry()
  unknown <- setdiff(learners, names(registry))
  if (length(unknown)) {
    stop(
      "Unknown learner families: ", paste(unknown, collapse = ", "),
      ". Use `learner_catalog()` to inspect supported families.",
      call. = FALSE
    )
  }
  unsupported <- learners[!vapply(learners, function(family) {
    task %in% registry[[family]]$tasks
  }, logical(1))]
  if (length(unsupported)) {
    stop(
      "Learner families do not support the `", task, "` task: ",
      paste(unsupported, collapse = ", "), ".",
      call. = FALSE
    )
  }
  unavailable <- learners[!vapply(learners, function(family) {
    learner_is_available(registry[[family]])
  }, logical(1))]
  if (length(unavailable)) {
    packages <- unique(vapply(unavailable, function(family) {
      registry[[family]]$package %||% family
    }, character(1)))
    stop(
      "Learner dependencies are not installed: ", paste(packages, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (max_models < length(learners)) {
    stop(
      "`max_models` must be at least the number of learner families (",
      length(learners), ").",
      call. = FALSE
    )
  }
  max_models <- min(max_models, 50L)
  grids <- lapply(learners, function(family) {
    registry[[family]]$grid(n = n, p = p, task = task, n_classes = n_classes)
  })
  names(grids) <- learners
  selected <- list()
  depth <- 1L
  while (length(selected) < max_models) {
    added <- FALSE
    for (family in learners) {
      if (length(selected) >= max_models) break
      if (length(grids[[family]]) >= depth) {
        selected[[length(selected) + 1L]] <- list(
          family = family,
          parameters = grids[[family]][[depth]]
        )
        added <- TRUE
      }
    }
    if (!added) break
    depth <- depth + 1L
  }
  family_counts <- setNames(integer(length(learners)), learners)
  rows <- lapply(seq_along(selected), function(index) {
    family <- selected[[index]]$family
    parameters <- selected[[index]]$parameters
    definition <- registry[[family]]
    family_counts[[family]] <<- family_counts[[family]] + 1L
    data.frame(
      configuration_id = paste0(family, "_", sprintf("%02d", family_counts[[family]])),
      family = family,
      backend = definition$backend,
      model = learner_model_label(definition, task),
      hyperparameters = definition$describe(parameters),
      parameters = I(list(parameters)),
      simplicity_rank = definition$simplicity_rank,
      complexity_proxy = max(1, definition$complexity(
        parameters, n = n, p = p, task = task, n_classes = n_classes
      )),
      seed = as.integer((as.double(seed) + index - 1) %% .Machine$integer.max),
      selected = FALSE,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
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
  processed <- preprocess_guided_split(
    training,
    validation,
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
    novel_levels_mapped = sum(
      processed$evaluation$preprocessing_log$novel_level_mappings %||% integer()
    ),
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
    novel_levels_mapped = fold$novel_levels_mapped,
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
      backend = plan$backend[[index]],
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
      simplicity_rank = plan$simplicity_rank[[index]],
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
  definition <- learner_definition(family)
  parameters <- configuration$parameters[[1L]]
  fit_seed <- configuration$seed[[1L]]
  with_preserved_seed(
    fit_seed,
    definition$fit(
      data = data,
      target = target,
      task = task,
      parameters = parameters,
      seed = fit_seed
    )
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
