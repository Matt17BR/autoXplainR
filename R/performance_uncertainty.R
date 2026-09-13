#' Estimate evaluation-sample uncertainty with a paired bootstrap
#'
#' Resamples evaluation observations, using the same sampled rows for the primary
#' model and designated reference model (the intercept-only baseline in guided
#' workflows). The models stay fixed. The difference is primary score minus
#' reference score. Positive differences favor the primary for AUC;
#' negative differences favor the primary for losses.
#' This estimates evaluation-sample variability conditional on the fitted models;
#' it does not include fitting, tuning, or feature-selection uncertainty.
#'
#' @param result An [autoxplain()] or [evaluate_models()] result.
#' @param n_boot Number of bootstrap draws (at least 20). Use at least 1000 for
#'   analysis; smaller values are useful for examples and software tests.
#' @param confidence Percentile interval level, strictly between zero and one.
#' @param seed Reproducible resampling seed; the calling session's RNG is preserved.
#'
#' @details For ordinary data this is an IID row bootstrap. With
#'   `validation_split(group = ...)`, whole evaluation groups are sampled with
#'   replacement and all their rows retained; losses remain observation-weighted.
#'   This assumes independent groups and enough representative groups. It does
#'   not estimate equally weighted group performance. Temporal designs are
#'   rejected because an IID bootstrap would ignore serial dependence.
#'
#'   Intervals are approximate, can be unreliable in small or degenerate samples,
#'   and are not simultaneous across metrics or model comparisons. The configured
#'   primary metric is used; no model is selected using these intervals. A
#'   validation-set interval does not make that set an independent test set.
#'
#'   Binary AUC is computed from positive-negative pairs within each resampled
#'   evaluation set, with half credit for tied probabilities. Draws containing
#'   only one class are discarded and their IDs are retained in `bootstrap`.
#'   At least 20 usable draws and 80% of the requested draws are required.
#'   This availability rule does not guarantee interval accuracy. When any
#'   draws are removed, the interval is conditional on sampling both classes.
#'   RMSLE requires nonnegative outcomes and predictions; negatives are not clipped.
#'
#' @return An `autoxplain_uncertainty` list with `estimates` (primary, baseline,
#'   and paired difference), all `draws`, resampling `unit`, and interpretation
#'   `notes`. Metrics are RMSE, RMSLE or MAE for regression; log loss, Brier,
#'   or binary ROC AUC for classification, following the fitted result's primary
#'   metric. For AUC, `bootstrap` records retained and discarded draw counts and
#'   identities; `n_boot` remains the number requested.
#' @references Davison, A. C. and Hinkley, D. V. (1997). Bootstrap Methods and
#'   Their Application. Cambridge University Press. <doi:10.1017/CBO9780511802843>.
#' @export
#' @examples
#' result <- autoxplain(mtcars, "mpg", explain = FALSE)
#' performance_uncertainty(result, n_boot = 50)
performance_uncertainty <- function(result, n_boot = 1000L, confidence = 0.95, seed = 123L) {
  if (!inherits(result, "autoxplain_result")) {
    stop("`result` must be returned by `autoxplain()`.", call. = FALSE)
  }
  n_boot <- assert_count(n_boot, "n_boot", minimum = 20L)
  seed <- assert_count(seed, "seed", minimum = 0L)
  assert_probability(confidence, "confidence", open = TRUE)
  if (identical(result$validation$method, "temporal")) {
    stop("Temporal evaluation requires a dependence-aware uncertainty method, not an IID bootstrap.", call. = FALSE)
  }
  reference_id <- result_reference_id(result)
  ids <- c(result$provenance$primary_model_id, reference_id)
  if (length(ids) != 2L || anyNA(ids) || anyDuplicated(ids) || !all(ids %in% names(result$models))) {
    stop("Paired uncertainty requires distinct retained primary and reference models.", call. = FALSE)
  }
  explainers <- report_explainers(result, models = ids)
  reference <- explainers[[1L]]
  metric <- resolve_metric(result$evaluation$primary_metric, result$task)
  if (!metric %in% c("rmse", "rmsle", "mae", "logloss", "brier", "auc")) {
    stop("Bootstrap uncertainty supports RMSE, RMSLE, MAE, log loss, Brier, or binary ROC AUC.", call. = FALSE)
  }
  predictions <- report_predictions(result, models = ids, explainers = explainers)
  n <- length(reference$y)
  unit <- "observation"
  members <- NULL
  n_units <- n
  if (identical(result$validation$method, "group")) {
    units <- result$validation$evaluation_groups[
      match(rownames(result$test_data), result$validation$evaluation_row_names)
    ]
    if (anyNA(units)) stop("Evaluation group alignment is unavailable.", call. = FALSE)
    unit <- "group"
    members <- split(seq_len(n), match(units, unique(units)))
    n_units <- length(members)
  }
  if (n_units < 2L) stop("At least two evaluation sampling units are required.", call. = FALSE)
  if (metric == "auc") {
    truth <- as.character(reference$y) == reference$positive
    plans <- lapply(predictions, function(prediction) auc_bootstrap_plan(truth, prediction))
    score <- function(rows) {
      counts <- tabulate(rows, nbins = n)
      values <- vapply(plans, auc_bootstrap_score, numeric(1), row_counts = counts)
      stats::setNames(c(values, values[[1L]] - values[[2L]]), c("primary", "baseline", "difference"))
    }
  } else {
    contributions <- lapply(predictions, function(prediction) {
      uncertainty_case_values(reference$y, prediction, metric, reference)
    })
    score <- function(rows) {
      values <- vapply(contributions, function(contribution) {
        value <- mean(contribution[rows])
        if (metric %in% c("rmse", "rmsle")) sqrt(value) else if (metric == "logloss") -value else value
      }, numeric(1))
      stats::setNames(c(values, values[[1L]] - values[[2L]]), c("primary", "baseline", "difference"))
    }
  }
  draws <- with_preserved_seed(seed, {
    t(replicate(n_boot, {
      sampled <- sample.int(n_units, n_units, replace = TRUE)
      rows <- if (is.null(members)) sampled else unlist(members[sampled], use.names = FALSE)
      score(rows)
    }))
  })
  bootstrap <- NULL
  if (metric == "auc") {
    completed <- auc_bootstrap_complete_draws(draws, n_boot)
    draws <- completed$draws
    bootstrap <- completed$record
  }
  probabilities <- c((1 - confidence) / 2, (1 + confidence) / 2)
  intervals <- apply(draws, 2L, stats::quantile, probs = probabilities, names = FALSE, type = 7)
  estimates <- data.frame(
    quantity = c("primary", "baseline", "difference"),
    estimate = unname(score(seq_len(n))),
    lower = intervals[1L, ], upper = intervals[2L, ], row.names = NULL
  )
  output <- structure(list(
    estimates = estimates, draws = as.data.frame(draws), metric = result$evaluation$primary_metric,
    confidence = confidence, n_boot = n_boot, seed = seed, unit = unit, units = n_units,
    primary_model_id = ids[[1L]], reference_model_id = ids[[2L]],
    evaluation_role = result$provenance$evaluation_role,
    notes = c(
      paste0("Paired percentile intervals conditional on the fitted models; ",
        if (metric == "auc") "positive" else "negative", " differences favor the primary model."
      ),
      "These intervals omit fitting and selection uncertainty and assume independent sampling units.",
      if (n_units < 20L) "Fewer than 20 sampling units: interval endpoints may be very unstable.",
      if (any(vapply(as.data.frame(draws), function(x) length(unique(x)) == 1L, logical(1)))) {
        "At least one bootstrap distribution is degenerate; a zero-width interval is not proof of certainty."
      }
    )
  ), class = "autoxplain_uncertainty")
  if (!is.null(bootstrap)) {
    output$bootstrap <- bootstrap
    if (bootstrap$discarded > 0L) {
      output$notes <- c(output$notes, paste(
        "Discarded", bootstrap$discarded, "of", n_boot,
        "bootstrap draws containing only one class. These intervals are conditional on sampling both classes."
      ))
    }
  }
  output
}

# The supported losses have fixed row contributions. Keep metric_score's exact
# arithmetic here, including unclipped binary Brier probabilities and the sign
# outside mean() for log loss. Resampling still indexes rows in their drawn order;
# aggregating group totals or bootstrap frequencies would change summation order.
uncertainty_case_values <- function(y, prediction, metric, explainer) {
  if (is.factor(prediction)) {
    stop("This metric requires probabilities; the prediction adapter supplies class labels. ",
      "Use `metric = \"accuracy\"`.", call. = FALSE
    )
  }
  if (metric == "rmse") return((as.numeric(y) - prediction)^2)
  if (metric == "rmsle") return(selection_rmsle_case_loss(y, prediction))
  if (metric == "mae") return(abs(as.numeric(y) - prediction))
  if (metric == "brier") {
    if (explainer$task == "binary") {
      observed <- as.numeric(as.character(y) == explainer$positive)
      return((as.numeric(prediction) - observed)^2)
    }
    if (!is.matrix(prediction)) {
      stop("Multiclass Brier score requires class-probability predictions.", call. = FALSE)
    }
    classes <- explainer$class_levels
    indices <- match(as.character(y), classes)
    if (anyNA(indices) || !all(classes %in% colnames(prediction))) {
      stop("Multiclass prediction columns must be named with every outcome class.", call. = FALSE)
    }
    probability <- prediction[, classes, drop = FALSE]
    one_hot <- matrix(0, nrow(probability), ncol(probability))
    one_hot[cbind(seq_along(indices), indices)] <- 1
    return(rowSums((probability - one_hot)^2))
  }
  epsilon <- 1e-15
  if (explainer$task == "binary") {
    observed <- as.numeric(as.character(y) == explainer$positive)
    probability <- pmin(pmax(prediction, epsilon), 1 - epsilon)
    return(observed * log(probability) + (1 - observed) * log(1 - probability))
  }
  if (!is.matrix(prediction)) {
    stop("Multiclass log loss requires class-probability predictions.", call. = FALSE)
  }
  indices <- match(as.character(y), colnames(prediction))
  if (anyNA(indices)) {
    stop("Multiclass prediction columns must be named with every outcome class.", call. = FALSE)
  }
  probability <- prediction[cbind(seq_along(indices), indices)]
  log(pmax(probability, epsilon))
}

#' @export
print.autoxplain_uncertainty <- function(x, ...) {
  cat("<AutoXplainR paired evaluation bootstrap>\n")
  cat("  metric: ", x$metric, " | units: ", x$units, " ", x$unit, "s\n", sep = "")
  print(x$estimates, row.names = FALSE)
  cat(paste(x$notes, collapse = "\n"), "\n")
  invisible(x)
}
