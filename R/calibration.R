#' Check whether held-out class probabilities behave literally
#'
#' `calibration_diagnostics()` groups classification probabilities on the
#' evaluation rows and compares each group's average probability with what
#' actually happened. For binary classification the event is the positive
#' class. For multiclass classification it checks whether the confidence of
#' the predicted class matches its observed accuracy.
#'
#' This is a descriptive, binned diagnostic. Its value depends on the
#' evaluation sample and grouping, so it is context for probability quality,
#' not a population guarantee or a replacement for log loss and Brier score.
#'
#' @param result An `autoxplain_result`.
#' @param model One model ID or index. `NULL` uses `main_model` when available,
#'   otherwise the first retained model.
#' @param bins Maximum number of approximately equal-sized probability groups.
#'   Small evaluation sets automatically use fewer groups so that each group
#'   is not presented with false precision.
#'
#' @return An `autoxplain_calibration` object containing the overall binned
#'   calibration error and a `groups` data frame.
#' @export
#'
#' @examples
#' flowers <- autoxplain(iris, "Species", seed = 2026)
#' calibration_diagnostics(flowers)
calibration_diagnostics <- function(result, model = NULL, bins = 5L) {
  if (!inherits(result, "autoxplain_result")) {
    stop("`result` must be returned by `autoxplain()`.", call. = FALSE)
  }
  if (identical(result$task, "regression")) {
    stop(
      "Calibration diagnostics require classification probabilities, not regression predictions.",
      call. = FALSE
    )
  }
  bins <- assert_count(bins, "bins")
  available <- names(result$models)
  model <- model %||% if ("main_model" %in% available) "main_model" else available[[1L]]
  selected <- select_models(result$models, model)
  if (length(selected) != 1L) {
    stop("`model` must select exactly one retained model.", call. = FALSE)
  }
  model_id <- names(selected)[[1L]]
  explainer <- as_explainers(result, models = model_id)[[1L]]
  diagnostic <- calibration_from_explainer(explainer, bins = bins)
  diagnostic$model_id <- model_id
  diagnostic$model_label <- explainer$label
  role <- result$provenance$evaluation_role %||% "unspecified evaluation"
  diagnostic$evaluation_role <- role
  diagnostic$scope_note <- paste0(
    "Descriptive binned calibration on ", role,
    " rows; results depend on this sample and the probability grouping."
  )
  structure(diagnostic, class = c("autoxplain_calibration", "list"))
}

#' @export
print.autoxplain_calibration <- function(x, ...) {
  cat("<AutoXplainR probability calibration>\n")
  cat("  model:      ", x$model_id, "\n", sep = "")
  cat("  check:      ", x$event_label, "\n", sep = "")
  cat("  rows:       ", x$rows, " in ", x$n_groups, " probability groups\n", sep = "")
  cat(
    "  average:    ", format(round(x$mean_probability, 3L), trim = TRUE),
    " predicted vs ", format(round(x$observed_rate, 3L), trim = TRUE),
    " observed\n", sep = ""
  )
  cat(
    "  binned gap: ", format(round(x$calibration_error, 3L), trim = TRUE),
    " (lower is better)\n", sep = ""
  )
  cat("  caution:    sample- and grouping-dependent; not a population guarantee\n")
  invisible(x)
}

calibration_from_explainer <- function(explainer, bins = 5L, predicted = NULL) {
  if (!inherits(explainer, "autoxplain_explainer")) {
    stop("`explainer` must be an AutoXplainR explainer.", call. = FALSE)
  }
  if (identical(explainer$task, "regression")) {
    stop("Calibration requires a classification explainer.", call. = FALSE)
  }
  bins <- assert_count(bins, "bins")
  predicted <- predicted %||% predict(explainer, explainer$data)
  truth <- as.character(explainer$y)
  if (identical(explainer$task, "binary")) {
    probability <- as.numeric(predicted)
    event <- truth == explainer$positive
    event_label <- paste0("probability of the positive class `", explainer$positive, "`")
  } else {
    probability_matrix <- as.matrix(predicted)[, explainer$class_levels, drop = FALSE]
    predicted_class <- explainer$class_levels[
      max.col(probability_matrix, ties.method = "first")
    ]
    probability <- apply(probability_matrix, 1L, max)
    event <- truth == predicted_class
    event_label <- "confidence in the predicted class"
  }
  grouped <- calibration_groups(probability, event, bins)
  list(
    task = explainer$task,
    positive_class = explainer$positive,
    event_label = event_label,
    rows = length(probability),
    n_groups = nrow(grouped),
    mean_probability = mean(probability),
    observed_rate = mean(event),
    calibration_error = sum(grouped$rows * grouped$calibration_gap) / length(probability),
    groups = grouped
  )
}

calibration_groups <- function(probability, event, bins) {
  if (!is.numeric(probability) || !length(probability) ||
        length(probability) != length(event) || any(!is.finite(probability)) ||
        any(probability < 0 | probability > 1) || anyNA(event)) {
    stop("Calibration inputs must be finite probabilities and observed events.", call. = FALSE)
  }
  bins <- assert_count(bins, "bins")
  effective_bins <- min(bins, max(1L, floor(length(probability) / 10L)))
  ranks <- rank(probability, ties.method = "average")
  group <- pmin(effective_bins, ceiling(ranks / length(probability) * effective_bins))
  group <- match(group, sort(unique(group)))
  indices <- split(seq_along(probability), group)
  output <- do.call(rbind, lapply(seq_along(indices), function(index) {
    rows <- indices[[index]]
    average <- mean(probability[rows])
    observed <- mean(as.logical(event[rows]))
    data.frame(
      probability_group = index,
      rows = length(rows),
      probability_min = min(probability[rows]),
      probability_max = max(probability[rows]),
      mean_probability = average,
      observed_rate = observed,
      calibration_gap = abs(average - observed),
      stringsAsFactors = FALSE
    )
  }))
  rownames(output) <- NULL
  output
}
