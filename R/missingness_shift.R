#' Compare raw missing-value rates between training and evaluation data
#'
#' `missingness_shift()` compares each predictor's missing-value rate before
#' imputation or other missing-value handling. A changed missingness pattern can
#' be a useful clue that data collection, population, or pipeline behavior
#' differs between model fitting and evaluation.
#'
#' The threshold is a practical flag, not a hypothesis test. This diagnostic
#' only concerns missing values; it does not establish that the full predictor
#' distribution is stable or shifted.
#'
#' @param result An `autoxplain_result` with retained preprocessing metadata for
#'   both training and evaluation data.
#' @param threshold Absolute missing-rate difference used to flag a predictor.
#'   The default is 0.05, or five percentage points.
#'
#' @return An `autoxplain_missingness_shift` object containing a predictor-level
#'   `features` data frame.
#' @export
#'
#' @examples
#' training <- data.frame(x = c(NA, 2:40), y = 1:40)
#' evaluation <- data.frame(x = c(rep(NA, 5), 46:60), y = 41:60)
#' fit <- autoxplain(training, "y", test_data = evaluation)
#' missingness_shift(fit)
missingness_shift <- function(result, threshold = 0.05) {
  if (!inherits(result, "autoxplain_result")) {
    stop("`result` must be returned by `autoxplain()`.", call. = FALSE)
  }
  assert_probability(threshold, "threshold")
  metadata <- result$preprocessing_metadata
  training_info <- metadata$training_data$original_info
  evaluation_info <- metadata$test_data$original_info
  if (is.null(training_info) || is.null(evaluation_info)) {
    stop(
      "Missingness shift requires retained training and evaluation preprocessing metadata.",
      call. = FALSE
    )
  }
  training_rate <- training_info$missing_fraction_by_column
  evaluation_rate <- evaluation_info$missing_fraction_by_column
  if (is.null(training_rate) || is.null(evaluation_rate)) {
    stop(
      "This result predates per-column missingness metadata; refit it to run the diagnostic.",
      call. = FALSE
    )
  }
  predictors <- setdiff(
    intersect(names(training_rate), names(evaluation_rate)),
    result$target_column
  )
  if (!length(predictors)) {
    stop("No shared predictors are available for a missingness comparison.", call. = FALSE)
  }
  training <- unname(training_rate[predictors])
  evaluation <- unname(evaluation_rate[predictors])
  shift <- evaluation - training
  features <- data.frame(
    feature = predictors,
    used_by_model = predictors %in% result$features,
    training_missing_rate = training,
    evaluation_missing_rate = evaluation,
    rate_change = shift,
    absolute_shift = abs(shift),
    direction = ifelse(shift > 0, "more missing in evaluation", ifelse(
      shift < 0, "less missing in evaluation", "no observed change"
    )),
    flagged = abs(shift) >= threshold,
    stringsAsFactors = FALSE
  )
  features <- features[order(
    !features$flagged,
    !features$used_by_model,
    -features$absolute_shift,
    features$feature
  ), , drop = FALSE]
  rownames(features) <- NULL
  strategy <- metadata$training_data$recipe$missing_value_strategy %||% "not applied"
  structure(
    list(
      threshold = threshold,
      training_rows = training_info$nrows,
      evaluation_rows = evaluation_info$nrows,
      preprocessing_strategy = strategy,
      n_features = nrow(features),
      n_with_missing = sum(training > 0 | evaluation > 0),
      n_flagged = sum(features$flagged),
      n_flagged_model_features = sum(features$flagged & features$used_by_model),
      largest_shift = max(features$absolute_shift),
      features = features,
      scope_note = paste(
        "Raw pre-processing missing-value rates only; the flag threshold is a",
        "practical prompt, not a statistical test or a general distribution-shift check."
      )
    ),
    class = c("autoxplain_missingness_shift", "list")
  )
}

#' @export
print.autoxplain_missingness_shift <- function(x, ...) {
  cat("<AutoXplainR missingness shift>\n")
  cat(
    "  data:        ", x$training_rows, " training + ", x$evaluation_rows,
    " evaluation rows\n", sep = ""
  )
  cat("  predictors:  ", x$n_with_missing, " with any missing values\n", sep = "")
  cat(
    "  flagged:     ", x$n_flagged_model_features, " model inputs at ",
    format(round(100 * x$threshold, 1L), trim = TRUE), " percentage points\n", sep = ""
  )
  cat("  caution:     practical flag; not a statistical test or general drift check\n")
  invisible(x)
}
