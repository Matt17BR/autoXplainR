#' Explore binary decision-threshold trade-offs
#'
#' A binary model returns a probability; a decision threshold turns that
#' probability into a class label. `threshold_diagnostics()` shows how observed
#' sensitivity, specificity, precision, accuracy, false positives, and false
#' negatives change across candidate thresholds on the evaluation rows.
#'
#' The default 0.5 threshold is a convention, not a universal optimum. The
#' output is descriptive and must not be used to choose a threshold and then
#' report performance on the same rows as if it were independent validation.
#' Threshold choice requires domain consequences and fresh validation data.
#'
#' @param result An `autoxplain_result` for binary classification.
#' @param thresholds Numeric candidate thresholds between 0 and 1.
#' @param model One model ID or index. `NULL` uses the recorded primary model,
#'   otherwise the first retained model.
#' @param false_positive_cost Non-negative relative cost assigned to one false
#'   positive.
#' @param false_negative_cost Non-negative relative cost assigned to one false
#'   negative.
#'
#' @return An `autoxplain_thresholds` object containing a `performance` data
#'   frame. Expected cost is the average user-supplied relative cost per
#'   evaluation row, not a monetary estimate.
#' @export
#'
#' @examples
#' cars <- transform(mtcars, am = factor(am, labels = c("automatic", "manual")))
#' fit <- autoxplain(cars, "am", seed = 2026)
#' threshold_diagnostics(fit, thresholds = c(0.3, 0.5, 0.7))
threshold_diagnostics <- function(result,
                                  thresholds = (2:18) / 20,
                                  model = NULL,
                                  false_positive_cost = 1,
                                  false_negative_cost = 1) {
  if (!inherits(result, "autoxplain_result")) {
    stop("`result` must be returned by `autoxplain()`.", call. = FALSE)
  }
  if (!identical(result$task, "binary")) {
    stop("Threshold diagnostics require a binary-classification result.", call. = FALSE)
  }
  thresholds <- validate_thresholds(thresholds)
  false_positive_cost <- validate_threshold_cost(
    false_positive_cost, "false_positive_cost"
  )
  false_negative_cost <- validate_threshold_cost(
    false_negative_cost, "false_negative_cost"
  )
  if (false_positive_cost == 0 && false_negative_cost == 0) {
    stop("At least one threshold error cost must be greater than zero.", call. = FALSE)
  }
  available <- names(result$models)
  model <- model %||% result$provenance$primary_model_id %||% available[[1L]]
  selected <- select_models(result$models, model)
  if (length(selected) != 1L) {
    stop("`model` must select exactly one retained model.", call. = FALSE)
  }
  model_id <- names(selected)[[1L]]
  explainers <- report_explainers(result, models = model_id)
  explainer <- explainers[[1L]]
  probability <- as.numeric(report_predictions(result, models = model_id, explainers = explainers)[[1L]])
  truth <- as.character(explainer$y) == explainer$positive
  performance <- threshold_performance(
    truth,
    probability,
    thresholds,
    false_positive_cost,
    false_negative_cost
  )
  negative <- setdiff(explainer$class_levels, explainer$positive)[[1L]]
  role <- result$provenance$evaluation_role %||% "unspecified evaluation"
  minimum_cost <- min(performance$expected_cost)
  minimum_rows <- performance$expected_cost == minimum_cost
  structure(
    list(
      model_id = model_id,
      positive_class = explainer$positive,
      negative_class = negative,
      rows = length(truth),
      evaluation_role = role,
      false_positive_cost = false_positive_cost,
      false_negative_cost = false_negative_cost,
      lowest_observed_cost = minimum_cost,
      lowest_observed_cost_thresholds = performance$threshold[minimum_rows],
      performance = performance,
      scope_note = paste(
        "Descriptive threshold trade-offs on", role,
        "rows. Choosing a cutoff here requires validation on different data."
      )
    ),
    class = c("autoxplain_thresholds", "list")
  )
}

#' @export
print.autoxplain_thresholds <- function(x, ...) {
  cat("<AutoXplainR decision-threshold check>\n")
  cat("  model:       ", x$model_id, "\n", sep = "")
  cat("  positive:    ", x$positive_class, "\n", sep = "")
  cat("  rows:        ", x$rows, " (", x$evaluation_role, ")\n", sep = "")
  cat("  thresholds:  ", nrow(x$performance), " checked\n", sep = "")
  cat(
    "  lowest cost: ", format(round(x$lowest_observed_cost, 4L), trim = TRUE),
    " at ", paste(format(x$lowest_observed_cost_thresholds, trim = TRUE), collapse = ", "),
    " (descriptive only)\n", sep = ""
  )
  cat("  caution:     validate a chosen threshold on different data\n")
  invisible(x)
}

validate_thresholds <- function(thresholds) {
  if (!is.numeric(thresholds) || !length(thresholds) || anyNA(thresholds) ||
        any(!is.finite(thresholds)) || any(thresholds < 0 | thresholds > 1)) {
    stop("`thresholds` must be finite numeric values between 0 and 1.", call. = FALSE)
  }
  sort(unique(as.numeric(thresholds)))
}

validate_threshold_cost <- function(value, name) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        !is.finite(value) || value < 0) {
    stop("`", name, "` must be one non-negative finite number.", call. = FALSE)
  }
  as.numeric(value)
}

threshold_performance <- function(truth,
                                  probability,
                                  thresholds,
                                  false_positive_cost,
                                  false_negative_cost) {
  if (!is.logical(truth) || anyNA(truth) || !is.numeric(probability) ||
        length(truth) != length(probability) || !length(truth) ||
        any(!is.finite(probability)) || any(probability < 0 | probability > 1)) {
    stop("Threshold inputs must be observed events and finite probabilities.", call. = FALSE)
  }
  rows <- lapply(thresholds, function(threshold) {
    predicted <- probability >= threshold
    metrics <- binary_confusion_metrics(truth, predicted)
    data.frame(
      threshold = threshold,
      as.list(metrics),
      expected_cost = (
        false_positive_cost * metrics[["false_positives"]] +
          false_negative_cost * metrics[["false_negatives"]]
      ) / length(truth),
      stringsAsFactors = FALSE
    )
  })
  output <- do.call(rbind, rows)
  rownames(output) <- NULL
  output
}

safe_threshold_rate <- function(numerator, denominator) {
  if (denominator == 0) NA_real_ else numerator / denominator
}

# Shared with ordinary evaluation so absence of a class has one metric policy.
binary_confusion_metrics <- function(truth, predicted) {
  if (!is.logical(truth) || !is.logical(predicted) || !length(truth) ||
        length(truth) != length(predicted) || anyNA(truth) || anyNA(predicted)) {
    stop("Confusion metrics require matching non-missing logical events and decisions.",
         call. = FALSE)
  }
  tp <- sum(predicted & truth)
  fp <- sum(predicted & !truth)
  tn <- sum(!predicted & !truth)
  fn <- sum(!predicted & truth)
  sensitivity <- safe_threshold_rate(tp, tp + fn)
  specificity <- safe_threshold_rate(tn, tn + fp)
  c(
    predicted_positive_rate = mean(predicted),
    sensitivity = sensitivity,
    specificity = specificity,
    precision = safe_threshold_rate(tp, tp + fp),
    negative_predictive_value = safe_threshold_rate(tn, tn + fn),
    accuracy = mean(predicted == truth),
    balanced_accuracy = if (all(is.finite(c(sensitivity, specificity)))) {
      mean(c(sensitivity, specificity))
    } else {
      NA_real_
    },
    f1 = safe_threshold_rate(2 * tp, 2 * tp + fp + fn),
    false_positives = fp,
    false_negatives = fn
  )
}
