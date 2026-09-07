#' Estimate evaluation-sample uncertainty with a paired bootstrap
#'
#' Resamples evaluation observations, using the same sampled rows for the primary
#' model and intercept-only baseline. The models stay fixed. The difference is
#' primary loss minus baseline loss, so negative values favor the primary model.
#' This estimates evaluation-sample variability conditional on the fitted models;
#' it does not include fitting, tuning, or feature-selection uncertainty.
#'
#' @param result An [autoxplain()] result.
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
#'   primary loss is used; no model is selected using these intervals. A
#'   validation-set interval does not make that set an independent test set.
#'
#' @return An `autoxplain_uncertainty` list with `estimates` (primary, baseline,
#'   and paired difference), all `draws`, resampling `unit`, and interpretation
#'   `notes`. Losses are RMSE or MAE for regression, log loss or Brier for
#'   classification, following the fitted result's primary metric.
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
  ids <- c(result$provenance$primary_model_id, "simple_baseline")
  explainers <- as_explainers(result, models = ids)
  reference <- explainers[[1L]]
  metric <- resolve_metric(result$evaluation$primary_metric, result$task)
  if (!metric %in% c("rmse", "mae", "logloss", "brier")) {
    stop("Bootstrap uncertainty supports RMSE, MAE, log loss, or Brier loss.", call. = FALSE)
  }
  predictions <- lapply(explainers, function(x) predict(x, x$data))
  n <- length(reference$y)
  units <- seq_len(n)
  unit <- "observation"
  if (identical(result$validation$method, "group")) {
    units <- result$validation$evaluation_groups[
      match(rownames(result$test_data), result$validation$evaluation_row_names)
    ]
    if (anyNA(units)) stop("Evaluation group alignment is unavailable.", call. = FALSE)
    unit <- "group"
  }
  members <- split(seq_len(n), match(units, unique(units)))
  if (length(members) < 2L) stop("At least two evaluation sampling units are required.", call. = FALSE)
  score <- function(rows) {
    values <- vapply(predictions, function(prediction) {
      selected <- if (is.matrix(prediction)) prediction[rows, , drop = FALSE] else prediction[rows]
      metric_score(reference$y[rows], selected, metric, reference)
    }, numeric(1))
    stats::setNames(c(values, values[[1L]] - values[[2L]]), c("primary", "baseline", "difference"))
  }
  draws <- with_preserved_seed(seed, {
    t(replicate(n_boot, {
      rows <- unlist(members[sample.int(length(members), length(members), replace = TRUE)], use.names = FALSE)
      score(rows)
    }))
  })
  probabilities <- c((1 - confidence) / 2, (1 + confidence) / 2)
  intervals <- apply(draws, 2L, stats::quantile, probs = probabilities, names = FALSE, type = 7)
  estimates <- data.frame(
    quantity = c("primary", "baseline", "difference"),
    estimate = unname(score(seq_len(n))),
    lower = intervals[1L, ], upper = intervals[2L, ], row.names = NULL
  )
  structure(list(
    estimates = estimates, draws = as.data.frame(draws), metric = result$evaluation$primary_metric,
    confidence = confidence, n_boot = n_boot, seed = seed, unit = unit, units = length(members),
    evaluation_role = result$provenance$evaluation_role,
    notes = c(
      "Paired percentile intervals conditional on the fitted models; negative differences favor the primary model.",
      "These intervals omit fitting and selection uncertainty and assume independent sampling units.",
      if (length(members) < 20L) "Fewer than 20 sampling units: interval endpoints may be very unstable.",
      if (any(vapply(as.data.frame(draws), function(x) length(unique(x)) == 1L, logical(1)))) {
        "At least one bootstrap distribution is degenerate; a zero-width interval is not proof of certainty."
      }
    )
  ), class = "autoxplain_uncertainty")
}

#' @export
print.autoxplain_uncertainty <- function(x, ...) {
  cat("<AutoXplainR paired evaluation bootstrap>\n")
  cat("  metric: ", x$metric, " | units: ", x$units, " ", x$unit, "s\n", sep = "")
  print(x$estimates, row.names = FALSE)
  cat(paste(x$notes, collapse = "\n"), "\n")
  invisible(x)
}
