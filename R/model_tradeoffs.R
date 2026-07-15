#' Compare supplied models without hiding trade-offs
#'
#' Builds a two-objective comparison from an [autoxplain()] result. Predictive
#' performance is taken from the evaluation leaderboard and complexity defaults
#' to serialized model size. A model is Pareto-efficient when no other supplied
#' model is at least as good on both dimensions and strictly better on one.
#'
#' Pareto status is descriptive and candidate-set-relative. It is not a tuning
#' rule, evidence that every useful model was considered, or permission to use
#' the held-out evaluation data repeatedly for model selection.
#'
#' @param result An `autoxplain_result`.
#' @param performance_metric Numeric leaderboard metric. `NULL` selects the
#'   task-appropriate primary metric.
#' @param complexity_metric Numeric leaderboard or model-metadata column.
#'   `NULL` prefers model size, then training or prediction time.
#' @param include_baseline Include models labeled with the baseline role.
#'
#' @return A data frame of class `autoxplain_model_tradeoffs` with Pareto status
#'   and metric-direction metadata.
#' @export
model_tradeoffs <- function(result,
                            performance_metric = NULL,
                            complexity_metric = NULL,
                            include_baseline = TRUE) {
  if (!inherits(result, "autoxplain_result")) {
    stop("`result` must be returned by `autoxplain()`.", call. = FALSE)
  }
  if (!is.logical(include_baseline) || length(include_baseline) != 1L ||
        is.na(include_baseline)) {
    stop("`include_baseline` must be TRUE or FALSE.", call. = FALSE)
  }
  leaderboard <- enrich_tradeoff_leaderboard(result)
  if (!"model_id" %in% names(leaderboard)) {
    stop("The result leaderboard does not contain model IDs.", call. = FALSE)
  }
  if (!"model" %in% names(leaderboard)) leaderboard$model <- leaderboard$model_id
  if (!"role" %in% names(leaderboard)) leaderboard$role <- "candidate"
  if (!include_baseline) {
    leaderboard <- leaderboard[leaderboard$role != "baseline", , drop = FALSE]
  }
  performance_metric <- resolve_tradeoff_metric(
    leaderboard,
    performance_metric,
    result$task,
    kind = "performance"
  )
  complexity_metric <- resolve_tradeoff_metric(
    leaderboard,
    complexity_metric,
    result$task,
    kind = "complexity"
  )
  usable <- is.finite(leaderboard[[performance_metric]]) &
    is.finite(leaderboard[[complexity_metric]])
  leaderboard <- leaderboard[usable, , drop = FALSE]
  if (nrow(leaderboard) < 2L) {
    stop("At least two models need finite performance and complexity values.", call. = FALSE)
  }
  higher_is_better <- performance_metric %in% higher_is_better_metrics()
  loss <- if (higher_is_better) {
    -leaderboard[[performance_metric]]
  } else {
    leaderboard[[performance_metric]]
  }
  output <- leaderboard[c("model_id", "model", "role", performance_metric, complexity_metric)]
  output$pareto_optimal <- pareto_nondominated(loss, leaderboard[[complexity_metric]])
  output <- output[order(
    !output$pareto_optimal,
    loss,
    output[[complexity_metric]],
    output$model_id
  ), , drop = FALSE]
  rownames(output) <- NULL
  attr(output, "performance_metric") <- performance_metric
  attr(output, "complexity_metric") <- complexity_metric
  attr(output, "higher_is_better") <- higher_is_better
  attr(output, "scope_note") <- paste(
    "Pareto status compares only the supplied models on the supplied evaluation data;",
    "it is not a final model-selection rule."
  )
  class(output) <- c("autoxplain_model_tradeoffs", "data.frame")
  output
}

#' @export
print.autoxplain_model_tradeoffs <- function(x, ...) {
  cat("<AutoXplainR model trade-offs>\n")
  cat("  performance: ", attr(x, "performance_metric"), " (",
      if (isTRUE(attr(x, "higher_is_better"))) "higher" else "lower",
      " is better)\n", sep = "")
  cat("  complexity:  ", attr(x, "complexity_metric"), " (lower is better)\n", sep = "")
  cat("  Pareto set:  ", sum(x$pareto_optimal), " / ", nrow(x), " supplied models\n", sep = "")
  print.data.frame(x, row.names = FALSE, ...)
  cat("  note: ", attr(x, "scope_note"), "\n", sep = "")
  invisible(x)
}

enrich_tradeoff_leaderboard <- function(result) {
  leaderboard <- as.data.frame(result$leaderboard)
  characteristics <- result$model_characteristics
  if (!inherits(characteristics, "autoxplainr_model_characteristics") ||
        !length(characteristics)) {
    return(leaderboard)
  }
  ids <- vapply(characteristics, `[[`, character(1), "model_id")
  index <- match(leaderboard$model_id, ids)
  additions <- list(
    model_size_kb = vapply(characteristics, function(x) x$size_bytes / 1024, numeric(1)),
    training_time_ms = 1000 * vapply(
      characteristics,
      `[[`,
      numeric(1),
      "training_time_s"
    )
  )
  for (column in names(additions)) {
    if (!column %in% names(leaderboard)) leaderboard[[column]] <- additions[[column]][index]
  }
  leaderboard
}

resolve_tradeoff_metric <- function(leaderboard, metric, task, kind) {
  numeric_columns <- names(leaderboard)[vapply(leaderboard, is.numeric, logical(1))]
  if (!is.null(metric)) {
    if (!is.character(metric) || length(metric) != 1L || is.na(metric) ||
          !metric %in% numeric_columns) {
      stop("`", kind, "_metric` must name a finite numeric leaderboard column.",
           call. = FALSE)
    }
    return(metric)
  }
  preferred <- if (kind == "performance") {
    if (task == "regression") {
      c("rmse", "mae", "mse", "r_squared")
    } else {
      c("log_loss", "logloss", "mean_per_class_error", "brier_score", "accuracy", "auc")
    }
  } else {
    c(
      "model_size_kb", "size_mb", "model_size", "training_time_ms",
      "training_time_s", "prediction_time_ms", "complexity"
    )
  }
  available <- intersect(preferred, numeric_columns)
  if (!length(available)) {
    stop("No suitable numeric ", kind, " metric is available.", call. = FALSE)
  }
  available[[1L]]
}

higher_is_better_metrics <- function() {
  c("accuracy", "auc", "roc_auc", "balanced_accuracy", "r_squared", "macro_recall")
}

pareto_nondominated <- function(performance_loss, complexity) {
  vapply(seq_along(performance_loss), function(index) {
    no_worse <- performance_loss <= performance_loss[[index]] &
      complexity <= complexity[[index]]
    strictly_better <- performance_loss < performance_loss[[index]] |
      complexity < complexity[[index]]
    !any(no_worse & strictly_better)
  }, logical(1))
}
