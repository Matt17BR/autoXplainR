# Scores stay on their usual scale in retained evidence. Only ordering and
# eligibility convert a maximized score into a loss.
selection_metric_direction <- function(metric) {
  if (metric %in% c("auc", "roc_auc")) "maximize" else "minimize"
}

selection_metric_loss <- function(score, metric) {
  if (selection_metric_direction(metric) == "maximize") -score else score
}

selection_metric_threshold <- function(best_score, best_se, metric) {
  if (selection_metric_direction(metric) == "maximize") best_score - best_se else best_score + best_se
}

selection_metric_eligible <- function(score, threshold, metric) {
  is.finite(score) & selection_metric_loss(score, metric) <= selection_metric_loss(threshold, metric)
}

selection_root_mean_metric <- function(metric) {
  metric %in% c("rmse", "rmsle")
}

# Validation-row weighting is an explicit fold-aggregation convention. AUC
# compares positive/negative pairs within each fold; scores from different
# fitted models are never ranked together. Its fold scatter is a selection
# heuristic, not a standard error for the AUC statistic or a confidence interval.
selection_fold_summary <- function(scores, validation_rows, metric) {
  if (!length(scores) && !length(validation_rows)) {
    return(list(score = NA_real_, sd = NA_real_, se = NA_real_))
  }
  if (!is.numeric(scores) || !is.numeric(validation_rows) ||
        !is.null(dim(scores)) || !is.null(dim(validation_rows)) ||
        length(scores) != length(validation_rows) ||
        any(!is.finite(scores)) || any(!is.finite(validation_rows)) ||
        any(validation_rows <= 0) || any(validation_rows != floor(validation_rows))) {
    stop("Fold summaries require finite scores and matching positive integer validation-row counts.",
      call. = FALSE
    )
  }
  weights <- validation_rows / sum(validation_rows)
  root_mean <- selection_root_mean_metric(metric)
  components <- if (root_mean) scores^2 else scores
  component_mean <- sum(weights * components)
  score <- if (root_mean) sqrt(component_mean) else component_mean
  if (length(scores) < 2L) {
    return(list(score = score, sd = NA_real_, se = NA_real_))
  }
  correction <- 1 - sum(weights^2)
  if (!is.finite(correction) || correction <= 0) {
    return(list(score = score, sd = NA_real_, se = NA_real_))
  }
  component_variance <- sum(weights * (components - component_mean)^2) / correction
  component_sd <- sqrt(max(0, component_variance))
  effective_folds <- 1 / sum(weights^2)
  component_se <- component_sd / sqrt(effective_folds)
  if (root_mean) {
    if (score == 0) {
      metric_sd <- metric_se <- 0
    } else {
      metric_sd <- component_sd / (2 * score)
      metric_se <- component_se / (2 * score)
    }
  } else {
    metric_sd <- component_sd
    metric_se <- component_se
  }
  list(score = score, sd = metric_sd, se = metric_se)
}

selection_validate_rmsle_target <- function(observed) {
  selection_assert_numeric_vector(observed, "RMSLE outcomes")
  if (any(observed < 0)) {
    stop("RMSLE requires nonnegative outcomes; negative outcomes are not clipped.", call. = FALSE)
  }
  invisible(TRUE)
}

selection_rmsle_case_loss <- function(observed, predicted) {
  selection_validate_rmsle_target(observed)
  selection_assert_numeric_vector(predicted, "RMSLE predictions")
  if (length(observed) != length(predicted)) {
    stop("RMSLE outcomes and predictions must have the same length.", call. = FALSE)
  }
  if (any(predicted < 0)) {
    stop(structure(list(
      message = "RMSLE requires nonnegative predictions; negative predictions are not clipped.",
      call = NULL
    ), class = c("autoxplain_rmsle_prediction_domain", "error", "condition")))
  }
  (log1p(observed) - log1p(predicted))^2
}

selection_rmsle <- function(observed, predicted) {
  sqrt(mean(selection_rmsle_case_loss(observed, predicted)))
}

selection_binary_auc <- function(truth, probability) {
  if (!is.logical(truth) || !is.null(dim(truth)) || !length(truth) || anyNA(truth)) {
    stop("AUC truth must identify the positive class with nonmissing logical values.", call. = FALSE)
  }
  selection_assert_numeric_vector(probability, "AUC probabilities")
  if (length(truth) != length(probability)) {
    stop("AUC truth and probabilities must have the same length.", call. = FALSE)
  }
  if (any(probability < 0 | probability > 1)) {
    stop("AUC probabilities must be between zero and one; probabilities are not clipped.", call. = FALSE)
  }
  positives <- sum(truth)
  negatives <- length(truth) - positives
  if (!positives || !negatives) {
    stop(structure(list(
      message = "AUC requires both outcome classes in every scored fold or evaluation set.",
      call = NULL
    ), class = c("autoxplain_auc_outcome_domain", "error", "condition")))
  }
  ranks <- rank(probability, ties.method = "average")
  (sum(ranks[truth]) - positives * (positives + 1) / 2) / (as.double(positives) * negatives)
}

selection_assert_numeric_vector <- function(value, label) {
  if (!is.numeric(value) || is.complex(value) || !is.null(dim(value)) ||
        !length(value) || any(!is.finite(value))) {
    stop(label, " must be a nonempty, finite numeric vector.", call. = FALSE)
  }
  invisible(TRUE)
}
