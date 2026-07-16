#' Compare held-out performance across an explicitly chosen group
#'
#' `subgroup_performance()` calculates the same prediction metrics separately
#' for each value of one evaluation-data column. It is intended to reveal where
#' aggregate performance may be hiding weak spots and to prompt better data
#' collection or validation.
#'
#' The function never guesses which columns are sensitive and never labels the
#' result a fairness assessment. Observed differences can reflect small sample
#' sizes, case mix, measurement quality, or genuine model behavior. They are
#' descriptive context from the supplied evaluation rows.
#'
#' @param result An `autoxplain_result`.
#' @param by Name of one categorical or low-cardinality evaluation column.
#' @param model One model ID or index. `NULL` uses `main_model` when available,
#'   otherwise the first retained model.
#' @param min_rows Minimum rows used to label a group as large enough for a
#'   preliminary comparison. Smaller groups remain visible and are flagged.
#'
#' @return An `autoxplain_subgroups` object containing overall metrics and a
#'   group-level `performance` data frame. Its primary metric matches the
#'   result's evaluation metric (including MAE or Brier score), and
#'   `secondary_metric` names a different supporting metric.
#' @export
#'
#' @examples
#' cars <- transform(mtcars, transmission = factor(am, labels = c("auto", "manual")))
#' fit <- autoxplain(cars, "mpg", seed = 2026)
#' subgroup_performance(fit, by = "transmission", min_rows = 3)
subgroup_performance <- function(result, by, model = NULL, min_rows = 10L) {
  if (!inherits(result, "autoxplain_result")) {
    stop("`result` must be returned by `autoxplain()`.", call. = FALSE)
  }
  if (!is.character(by) || length(by) != 1L || is.na(by) || !nzchar(by)) {
    stop("`by` must be one non-empty evaluation-column name.", call. = FALSE)
  }
  evaluation <- result$test_data %||% result$training_data
  if (!by %in% names(evaluation)) {
    stop("`by` is not available in the evaluation data: ", by, ".", call. = FALSE)
  }
  if (identical(by, result$target_column)) {
    stop("`by` must name an input or context column, not the outcome.", call. = FALSE)
  }
  min_rows <- assert_count(min_rows, "min_rows")
  groups <- as.character(evaluation[[by]])
  groups[is.na(groups)] <- "(missing)"
  group_levels <- sort(unique(groups))
  if (length(group_levels) < 2L) {
    stop("`by` must contain at least two observed groups in the evaluation data.", call. = FALSE)
  }
  if (length(group_levels) > 20L) {
    stop(
      "`by` has more than 20 evaluation groups. Combine values into meaningful groups first.",
      call. = FALSE
    )
  }
  available <- names(result$models)
  model <- model %||% if ("main_model" %in% available) "main_model" else available[[1L]]
  selected <- select_models(result$models, model)
  if (length(selected) != 1L) {
    stop("`model` must select exactly one retained model.", call. = FALSE)
  }
  model_id <- names(selected)[[1L]]
  explainer <- as_explainers(result, models = model_id)[[1L]]
  predicted <- predict(explainer, explainer$data)
  overall <- evaluate_predictions(explainer$y, predicted, explainer)
  primary_metric <- subgroup_primary_metric(result, names(overall))
  secondary_metric <- subgroup_secondary_metric(result$task, primary_metric, names(overall))
  rows <- lapply(group_levels, function(group) {
    index <- which(groups == group)
    group_prediction <- subset_task_predictions(predicted, index, result$task)
    metrics <- evaluate_predictions(explainer$y[index], group_prediction, explainer)
    data.frame(
      group = group,
      rows = length(index),
      share = length(index) / length(groups),
      enough_rows = length(index) >= min_rows,
      as.list(metrics),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  performance <- do.call(rbind, rows)
  rownames(performance) <- NULL
  performance$gap_from_overall <- performance[[primary_metric]] - overall[[primary_metric]]
  performance <- performance[c(
    "group", "rows", "share", primary_metric, secondary_metric,
    "gap_from_overall", "enough_rows",
    setdiff(names(performance), c(
      "group", "rows", "share", primary_metric, secondary_metric,
      "gap_from_overall", "enough_rows"
    ))
  )]
  role <- result$provenance$evaluation_role %||% "unspecified evaluation"
  structure(
    list(
      model_id = model_id,
      by = by,
      task = result$task,
      rows = length(groups),
      n_groups = length(group_levels),
      min_rows = min_rows,
      primary_metric = primary_metric,
      secondary_metric = secondary_metric,
      overall_metrics = overall,
      largest_observed_gap = diff(range(performance[[primary_metric]], na.rm = TRUE)),
      performance = performance,
      evaluation_role = role,
      scope_note = paste0(
        "Descriptive performance by `", by, "` on ", role,
        " rows. This does not certify fairness or explain why groups differ."
      )
    ),
    class = c("autoxplain_subgroups", "list")
  )
}

subgroup_primary_metric <- function(result, available) {
  fallback <- if (identical(result$task, "regression")) "rmse" else "log_loss"
  primary <- result$evaluation$primary_metric %||% fallback
  if (!is.character(primary) || length(primary) != 1L || is.na(primary) ||
        !primary %in% available) {
    stop(
      "The result's primary evaluation metric is unavailable for subgroup scoring.",
      call. = FALSE
    )
  }
  primary
}

subgroup_secondary_metric <- function(task, primary, available) {
  preferred <- if (identical(task, "regression")) {
    c("mae", "rmse", "r_squared")
  } else {
    c("accuracy", "log_loss", "brier_score")
  }
  candidates <- setdiff(intersect(preferred, available), primary)
  if (length(candidates)) candidates[[1L]] else NULL
}

#' @export
print.autoxplain_subgroups <- function(x, ...) {
  cat("<AutoXplainR subgroup performance>\n")
  cat("  model:       ", x$model_id, "\n", sep = "")
  cat("  compared by: ", x$by, " (", x$n_groups, " groups)\n", sep = "")
  cat("  metric:      ", x$primary_metric, " (lower is better)\n", sep = "")
  cat(
    "  largest gap: ", format(round(x$largest_observed_gap, 4L), trim = TRUE),
    "\n", sep = ""
  )
  cat("  caution:     descriptive holdout check; not fairness certification\n")
  invisible(x)
}

subset_task_predictions <- function(predicted, index, task) {
  if (identical(task, "multiclass")) {
    return(as.matrix(predicted)[index, , drop = FALSE])
  }
  predicted[index]
}
