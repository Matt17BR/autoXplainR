#' Find held-out rows where supplied models disagree
#'
#' `prediction_ambiguity()` compares predictions from at least two retained
#' models on the same evaluation rows. Regression output reports the range of
#' predicted values. Classification output reports hard-class disagreement and
#' the largest pairwise probability distance for each row.
#'
#' By default the simple baseline is excluded and every other supplied model is
#' compared. These models need not form a statistically defined Rashomon set.
#' Their performance table is retained beside the ambiguity results so that
#' disagreement from a weak candidate is not mistaken for near-optimal model
#' uncertainty. Set `performance_tolerance` to keep only models within a
#' relative performance gap from the best selected model.
#'
#' @param result An `autoxplain_result` containing at least two comparable
#'   retained models.
#' @param models Model IDs, indices, or `NULL`. `NULL` selects every retained
#'   model not labeled as a baseline.
#' @param performance_tolerance Optional non-negative relative gap from the best
#'   selected evaluation score. `NULL` keeps all selected models.
#'
#' @return An `autoxplain_prediction_ambiguity` object with model performance,
#'   case-level `rows`, and aggregate summaries.
#' @export
#'
#' @examples
#' fit <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 2026)
#' prediction_ambiguity(fit)
prediction_ambiguity <- function(result,
                                 models = NULL,
                                 performance_tolerance = NULL) {
  if (!inherits(result, "autoxplain_result")) {
    stop("`result` must be returned by `autoxplain()`.", call. = FALSE)
  }
  performance_tolerance <- validate_ambiguity_tolerance(performance_tolerance)
  leaderboard <- as.data.frame(result$leaderboard)
  if (!"model_id" %in% names(leaderboard)) {
    stop("The result leaderboard does not contain model IDs.", call. = FALSE)
  }
  if (is.null(models)) {
    if ("role" %in% names(leaderboard)) {
      models <- leaderboard$model_id[leaderboard$role != "baseline"]
    } else {
      models <- setdiff(names(result$models), "simple_baseline")
    }
  }
  selected <- select_models(result$models, models)
  if (length(selected) < 2L) {
    stop(
      "Prediction ambiguity requires at least two selected non-baseline models. ",
      "Use `model_set = \"tuned\"` or `model_set = \"comparison\"`, or supply multiple fitted models.",
      call. = FALSE
    )
  }
  model_performance <- ambiguity_model_performance(
    result, names(selected), performance_tolerance
  )
  selected <- selected[model_performance$model_id[model_performance$included]]
  if (length(selected) < 2L) {
    stop(
      "`performance_tolerance` left fewer than two comparable models. ",
      "Use a wider tolerance or inspect the performance table first.",
      call. = FALSE
    )
  }
  explainers <- as_explainers(result, models = names(selected))
  predictions <- lapply(explainers, function(explainer) {
    predict(explainer, explainer$data)
  })
  reference <- explainers[[1L]]
  row_ids <- rownames(reference$data)
  if (is.null(row_ids) || any(!nzchar(row_ids))) {
    row_ids <- as.character(seq_len(nrow(reference$data)))
  }
  computed <- if (identical(result$task, "regression")) {
    regression_prediction_ambiguity(
      predictions, reference$y, row_ids
    )
  } else {
    classification_prediction_ambiguity(
      predictions,
      reference$y,
      reference$class_levels,
      reference$positive,
      row_ids
    )
  }
  role <- result$provenance$evaluation_role %||% "unspecified evaluation"
  structure(
    c(
      list(
        task = result$task,
        model_ids = names(selected),
        n_models = length(selected),
        evaluation_role = role,
        performance_metric = attr(model_performance, "performance_metric"),
        performance_tolerance = performance_tolerance,
        model_performance = model_performance,
        scope_note = paste(
          "Descriptive disagreement among the included supplied models on", role,
          "rows. It is not a confidence interval or proof that one model is correct."
        )
      ),
      computed
    ),
    class = c("autoxplain_prediction_ambiguity", "list")
  )
}

#' @export
print.autoxplain_prediction_ambiguity <- function(x, ...) {
  cat("<AutoXplainR prediction ambiguity>\n")
  cat("  task:       ", x$task, "\n", sep = "")
  cat("  models:     ", x$n_models, " (", paste(x$model_ids, collapse = ", "), ")\n",
      sep = "")
  cat("  rows:       ", nrow(x$rows), " (", x$evaluation_role, ")\n", sep = "")
  if (identical(x$task, "regression")) {
    cat("  median gap: ", format(round(x$median_prediction_range, 4L), trim = TRUE),
        " target units\n", sep = "")
  } else {
    cat("  class split:", format(round(100 * x$class_disagreement_rate, 1L), trim = TRUE),
        "% of rows\n", sep = "")
  }
  cat("  caution:    supplied-model disagreement is descriptive, not uncertainty coverage\n")
  invisible(x)
}

validate_ambiguity_tolerance <- function(value) {
  if (is.null(value)) return(NULL)
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        !is.finite(value) || value < 0) {
    stop("`performance_tolerance` must be NULL or one non-negative finite number.",
         call. = FALSE)
  }
  as.numeric(value)
}

ambiguity_model_performance <- function(result, model_ids, tolerance) {
  leaderboard <- enrich_tradeoff_leaderboard(result)
  primary <- result$evaluation$primary_metric %||% NULL
  if (!is.character(primary) || length(primary) != 1L || is.na(primary) ||
        !primary %in% names(leaderboard)) {
    primary <- NULL
  }
  performance_metric <- resolve_tradeoff_metric(
    leaderboard, primary, result$task, kind = "performance"
  )
  index <- match(model_ids, leaderboard$model_id)
  if (anyNA(index)) {
    stop("Every selected model needs an evaluation-leaderboard row.", call. = FALSE)
  }
  output <- leaderboard[index, intersect(
    c("model_id", "model", "role", performance_metric),
    names(leaderboard)
  ), drop = FALSE]
  if (any(!is.finite(output[[performance_metric]]))) {
    stop("Every selected model needs a finite evaluation score.", call. = FALSE)
  }
  higher_is_better <- performance_metric %in% higher_is_better_metrics()
  best <- if (higher_is_better) {
    max(output[[performance_metric]])
  } else {
    min(output[[performance_metric]])
  }
  denominator <- max(abs(best), sqrt(.Machine$double.eps))
  output$relative_performance_gap <- if (higher_is_better) {
    (best - output[[performance_metric]]) / denominator
  } else {
    (output[[performance_metric]] - best) / denominator
  }
  output$included <- if (is.null(tolerance)) {
    rep(TRUE, nrow(output))
  } else {
    output$relative_performance_gap <= tolerance
  }
  attr(output, "performance_metric") <- performance_metric
  output
}

regression_prediction_ambiguity <- function(predictions, observed, row_ids) {
  matrix <- do.call(cbind, lapply(predictions, as.numeric))
  colnames(matrix) <- names(predictions)
  prediction_min <- apply(matrix, 1L, min)
  prediction_max <- apply(matrix, 1L, max)
  prediction_range <- prediction_max - prediction_min
  rows <- data.frame(
    evaluation_row = seq_along(observed),
    row_id = row_ids,
    observed = as.numeric(observed),
    prediction_min = prediction_min,
    prediction_max = prediction_max,
    prediction_range = prediction_range,
    prediction_sd = apply(matrix, 1L, stats::sd),
    stringsAsFactors = FALSE
  )
  list(
    rows = rows,
    median_prediction_range = stats::median(prediction_range),
    p90_prediction_range = as.numeric(stats::quantile(
      prediction_range, 0.9, names = FALSE
    )),
    max_prediction_range = max(prediction_range),
    probability_distance = NULL
  )
}

classification_prediction_ambiguity <- function(predictions,
                                                observed,
                                                class_levels,
                                                positive,
                                                row_ids) {
  probability_arrays <- lapply(predictions, function(prediction) {
    if (length(class_levels) == 2L) {
      probability <- as.numeric(prediction)
      cbind(
        stats::setNames(1 - probability, class_levels[[1L]]),
        stats::setNames(probability, positive)
      )
    } else {
      as.matrix(prediction)[, class_levels, drop = FALSE]
    }
  })
  probability_arrays <- lapply(probability_arrays, function(probability) {
    colnames(probability) <- class_levels
    probability
  })
  predicted_classes <- vapply(probability_arrays, function(probability) {
    class_levels[max.col(probability, ties.method = "first")]
  }, character(nrow(probability_arrays[[1L]])))
  if (is.null(dim(predicted_classes))) {
    predicted_classes <- matrix(
      predicted_classes, ncol = length(probability_arrays),
      dimnames = list(NULL, names(probability_arrays))
    )
  }
  probability_distance <- vapply(seq_len(nrow(predicted_classes)), function(index) {
    case_probability_distance(lapply(probability_arrays, function(x) x[index, ]))
  }, numeric(1))
  n_classes <- apply(predicted_classes, 1L, function(x) length(unique(x)))
  class_labels <- apply(predicted_classes, 1L, function(x) {
    paste(sort(unique(x)), collapse = " | ")
  })
  rows <- data.frame(
    evaluation_row = seq_along(observed),
    row_id = row_ids,
    observed = as.character(observed),
    predicted_classes = class_labels,
    n_predicted_classes = n_classes,
    class_disagreement = n_classes > 1L,
    probability_distance = probability_distance,
    stringsAsFactors = FALSE
  )
  list(
    rows = rows,
    class_disagreement_rate = mean(rows$class_disagreement),
    median_probability_distance = stats::median(probability_distance),
    p90_probability_distance = as.numeric(stats::quantile(
      probability_distance, 0.9, names = FALSE
    )),
    max_probability_distance = max(probability_distance),
    probability_distance = if (length(class_levels) == 2L) {
      "positive-class probability range"
    } else {
      "maximum pairwise total-variation distance"
    }
  )
}

case_probability_distance <- function(probabilities) {
  if (length(probabilities) < 2L) return(0)
  pairs <- utils::combn(seq_along(probabilities), 2L)
  max(apply(pairs, 2L, function(pair) {
    0.5 * sum(abs(probabilities[[pair[[1L]]]] - probabilities[[pair[[2L]]]]))
  }))
}
