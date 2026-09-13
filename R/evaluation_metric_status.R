# An undefined primary score is distinct from poor performance. Preserve the
# model and all valid diagnostics, with a reason alongside its missing score.
evaluation_metric_status <- function(evaluated, metric) {
  rows <- lapply(names(evaluated), function(id) {
    value <- evaluated[[id]]
    label <- paste0(value$explainer$label %||% id, " (", id, ")")
    available <- is.finite(value$metrics[[metric]])
    invalid_predictions <- 0L
    reason <- ""
    if (!available && identical(metric, "rmsle")) {
      invalid_predictions <- sum(!is.finite(value$predictions) | value$predictions < 0)
      reason <- paste0(
        "RMSLE is unavailable for ", label, ": ", invalid_predictions, " of ",
        length(value$predictions), " evaluation predictions are negative or non-finite."
      )
    } else if (!available && identical(metric, "roc_auc")) {
      reason <- paste0("ROC AUC is unavailable for ", label, ": evaluation requires both outcome classes.")
    } else if (!available) {
      reason <- paste0("The primary score ", metric, " is unavailable for ", label, " on these evaluation rows.")
    }
    data.frame(
      model_id = id, metric = metric, status = if (available) "computed" else "unavailable",
      invalid_predictions = invalid_predictions, reason = reason, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
