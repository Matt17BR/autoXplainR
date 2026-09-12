assert_flag <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop("`", name, "` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(value)
}

finalize_autoxplain <- function(result, design, explain, report, report_data = "summary",
                                explanation_rows = 5000L) {
  result$schema_version <- "2.0"
  result$provenance$package_version <- package_version_or_development()
  result$provenance$r_version <- paste(R.version$major, R.version$minor, sep = ".")
  if (!is.null(design)) {
    result$data_context <- capture_data_context(
      design$raw_training, design$raw_evaluation, result$target_column, result$features,
      result$preprocessing_metadata$training_data, result$preprocessing_metadata$test_data,
      training_source_rows = design$provenance$training_rows,
      evaluation_source_rows = design$provenance$evaluation_rows,
      evaluation_source = "data", split_method = design$provenance$method,
      split_column = design$provenance$column,
      excluded = design$raw_excluded, excluded_source_rows = design$provenance$excluded_rows
    )
    indices <- result$evaluation_row_indices %||% seq_len(nrow(result$test_data))
    result$evaluation_context <- design$evaluation_context[indices, , drop = FALSE]
    result$validation <- design$provenance
    result$validation$evaluation_rows <- result$validation$evaluation_rows[indices]
    result$validation$evaluation_row_names <- result$validation$evaluation_row_names[indices]
    if (!is.null(result$validation$evaluation_groups)) {
      result$validation$evaluation_groups <- result$validation$evaluation_groups[indices]
    }
    result$provenance$split_method <- design$provenance$method
    result$provenance$test_fraction_requested <- design$provenance$fraction
    if (!is.null(result$tuning)) {
      result$tuning$control$fold_source <- "group_vfold"
      result$tuning$fold_source <- "group_vfold"
      result$tuning$method <- "whole-group training-only cross-validation"
    }
  }
  result <- seal_evaluation_result(result)
  if (isTRUE(explain) || !is.null(report)) {
    result$explanations <- prepare_model_report_data(result, explanation_rows = explanation_rows)
  }
  if (!is.null(report)) {
    result$report_file <- render_model_report(result, report, report_data = report_data)
  }
  result
}

#' Predict from an AutoXplainR result
#'
#' Applies the stored training recipe to raw predictor rows, then predicts with
#' the pre-specified or training-selected primary model. Existing models brought
#' through [evaluate_models()] use their recorded prediction contracts, including
#' any preprocessing inside the supplied predictor. No model is refitted.
#' The target column is optional and ignored. Row order and row count are kept;
#' recipes that drop incomplete rows return `NA` at those positions.
#'
#' @param object An [autoxplain()] or [evaluate_models()] result.
#' @param newdata Data frame of raw predictor rows.
#' @param model One model ID or index. `NULL` uses the primary model.
#' @param type `"response"` returns numeric predictions for regression, positive-
#'   class probabilities for binary tasks, and a named probability matrix for
#'   multiclass tasks. `"class"` returns a factor for classification.
#' @param ... Reserved for future use; additional arguments are rejected.
#'
#' @return A vector or matrix with one prediction per input row. Classification
#'   levels follow the recorded class contract. Binary probabilities refer to
#'   the recorded positive class: the second outcome level for [autoxplain()],
#'   or the explicitly chosen event for [evaluate_models()]. Novel categorical
#'   levels follow the stored recipe or supplied prediction contract.
#' @export
#' @examples
#' result <- autoxplain(mtcars, "mpg", explain = FALSE)
#' predict(result, mtcars[1:3, c("wt", "hp", "cyl", "disp", "drat",
#'                              "qsec", "vs", "am", "gear", "carb")])
predict.autoxplain_result <- function(object, newdata, model = NULL,
                                      type = c("response", "class"), ...) {
  if (length(list(...))) stop("Unused prediction arguments in `...`.", call. = FALSE)
  type <- match.arg(type)
  if (identical(object$provenance$workflow, "supplied-model evaluation")) {
    return(predict_supplied_result(object, newdata, model, type))
  }
  assert_data_frame(newdata, "newdata")
  if (anyDuplicated(names(newdata))) stop("`newdata` must have unique column names.", call. = FALSE)
  if (type == "class" && object$task == "regression") {
    stop("`type = \"class\"` requires a classification model.", call. = FALSE)
  }
  selected <- select_models(object$models, model %||% object$provenance$primary_model_id)
  if (length(selected) != 1L) stop("Select exactly one model for prediction.", call. = FALSE)
  missing <- setdiff(object$features, names(newdata))
  if (length(missing)) {
    stop("`newdata` is missing model features: ", paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  levels <- if (object$task == "regression") NULL else levels(object$training_data[[object$target_column]])
  n <- nrow(newdata)
  data <- newdata[object$features]
  keep <- rep(TRUE, n)
  if (isTRUE(object$preprocessing_metadata$enabled)) {
    recipe <- object$preprocessing_metadata$training_data$recipe
    recipe$final_columns <- setdiff(recipe$final_columns, object$target_column)
    recipe$factor_levels[object$target_column] <- NULL
    if (identical(recipe$missing_value_strategy, "drop_rows")) keep <- stats::complete.cases(data)
    if (any(keep)) data <- apply_preprocessing_recipe(data, recipe, object$target_column)$data
  }
  output <- if (object$task == "multiclass") {
    matrix(NA_real_, n, length(levels), dimnames = list(NULL, levels))
  } else {
    rep(NA_real_, n)
  }
  if (any(keep)) {
    validate_finite_guided_data(data, object$target_column)
    adapter <- make_prediction_adapter(
      selected[[1L]], object$task,
      positive = if (object$task == "binary") levels[[2L]] else NULL,
      class_levels = levels, predict_function = NULL
    )
    predictions <- adapter(data)
    validate_predictions(predictions, sum(keep), object$task, levels)
    if (is.matrix(output)) output[keep, ] <- predictions else output[keep] <- as.numeric(predictions)
  }
  if (type == "class") {
    labels <- rep(NA_character_, n)
    if (any(keep)) {
      labels[keep] <- if (object$task == "binary") {
        levels[1L + as.integer(output[keep] >= 0.5)]
      } else {
        levels[max.col(output[keep, , drop = FALSE], ties.method = "first")]
      }
    }
    return(factor(labels, levels = levels))
  }
  output
}
