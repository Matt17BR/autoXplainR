#' Prepare modeling data with an auditable recipe
#'
#' Performs conservative type normalization and missing-value handling. The
#' fitted recipe is returned and can be applied to evaluation data with the same
#' factor levels and imputation values. Identifier removal and ordinal coercion
#' are opt-in because guessing either can silently change the scientific
#' question.
#'
#' @param data Data frame to prepare.
#' @param target_column Outcome column name.
#' @param enable_target_handling Convert a character outcome to a factor.
#' @param enable_character_to_factors Convert character predictors to factors.
#' @param enable_ordered_factors Convert explicitly ordered predictors to their
#'   integer scores. Defaults to `FALSE` because spacing may not be meaningful.
#' @param enable_ordinal_factors Convert factors whose levels are integer-like
#'   strings to numeric. Defaults to `FALSE`.
#' @param enable_id_removal Remove columns with identifier-like names. Defaults
#'   to `FALSE`; unique values alone never trigger removal.
#' @param missing_value_strategy One of `"keep"`, `"drop_rows"`,
#'   `"drop_columns"`, or `"impute"`. Legacy names `"remove_rows"` and
#'   `"remove_columns"` are accepted.
#' @param missing_column_threshold Fraction missing above which `drop_columns`
#'   removes a predictor.
#' @param novel_level_strategy How evaluation-time predictor categories absent
#'   from training are handled. `"error"` stops; `"mode"` maps them to the
#'   most frequent observed training category and records the mapping count.
#' @param verbose Emit concise progress messages.
#' @param handle_missing,convert_characters,remove_id_columns Deprecated logical
#'   aliases retained for compatibility.
#'
#' @return A list containing `data`, a structured `preprocessing_log`, data
#'   summaries, and a reusable `recipe`.
#' @export
preprocess_data <- function(data,
                            target_column,
                            enable_target_handling = TRUE,
                            enable_character_to_factors = TRUE,
                            enable_ordered_factors = FALSE,
                            enable_ordinal_factors = FALSE,
                            enable_id_removal = FALSE,
                            missing_value_strategy = "keep",
                            missing_column_threshold = 0.5,
                            novel_level_strategy = c("error", "mode"),
                            verbose = FALSE,
                            handle_missing = NULL,
                            convert_characters = NULL,
                            remove_id_columns = NULL) {
  assert_data_frame(data, "data")
  if (!is.character(target_column) || length(target_column) != 1L ||
        !target_column %in% names(data)) {
    stop("`target_column` must name one column in `data`.", call. = FALSE)
  }
  if (!is.null(convert_characters)) enable_character_to_factors <- isTRUE(convert_characters)
  if (!is.null(remove_id_columns)) enable_id_removal <- isTRUE(remove_id_columns)
  if (!is.null(handle_missing) && !isTRUE(handle_missing)) missing_value_strategy <- "keep"
  missing_value_strategy <- normalize_missing_strategy(missing_value_strategy)
  novel_level_strategy <- match.arg(novel_level_strategy)
  assert_probability(missing_column_threshold, "missing_column_threshold")
  logical_arguments <- list(
    enable_target_handling = enable_target_handling,
    enable_character_to_factors = enable_character_to_factors,
    enable_ordered_factors = enable_ordered_factors,
    enable_ordinal_factors = enable_ordinal_factors,
    enable_id_removal = enable_id_removal,
    verbose = verbose
  )
  invalid <- !vapply(logical_arguments, function(x) is.logical(x) && length(x) == 1L && !is.na(x), logical(1))
  if (any(invalid)) stop("Preprocessing switches must be TRUE or FALSE.", call. = FALSE)

  original <- data_info(data)
  log <- list()
  recipe <- list(
    target_column = target_column,
    removed_columns = character(),
    character_levels = list(),
    ordered_columns = list(),
    numeric_factor_columns = character(),
    imputations = list(),
    missing_value_strategy = missing_value_strategy,
    novel_level_strategy = novel_level_strategy,
    factor_fallbacks = list()
  )

  if (enable_id_removal) {
    id_result <- remove_id_columns(data, target_column, verbose)
    data <- id_result$data
    recipe$removed_columns <- id_result$columns
    log$id_removal <- id_result$log
  }

  missing_result <- handle_missing_values(
    data, target_column, missing_value_strategy, missing_column_threshold, verbose
  )
  data <- missing_result$data
  recipe$removed_columns <- unique(c(recipe$removed_columns, missing_result$removed_columns))
  recipe$imputations <- missing_result$imputations
  log$missing_values <- missing_result$log

  if (enable_target_handling && is.character(data[[target_column]])) {
    levels <- sort(unique(data[[target_column]][!is.na(data[[target_column]])]))
    data[[target_column]] <- factor(data[[target_column]], levels = levels)
    log$target_handling <- list(action = "character_to_factor", levels = levels)
  }

  predictors <- setdiff(names(data), target_column)
  if (enable_character_to_factors) {
    character_columns <- predictors[vapply(data[predictors], is.character, logical(1))]
    for (column in character_columns) {
      levels <- sort(unique(data[[column]][!is.na(data[[column]])]))
      data[[column]] <- factor(data[[column]], levels = levels)
      recipe$character_levels[[column]] <- levels
    }
    log$character_to_factors <- list(columns = character_columns)
    preprocessing_message(verbose, "Converted ", length(character_columns), " character predictor(s) to factors.")
  }

  if (enable_ordered_factors) {
    ordered_columns <- predictors[vapply(data[predictors], is.ordered, logical(1))]
    for (column in ordered_columns) {
      recipe$ordered_columns[[column]] <- base::levels(data[[column]])
      data[[column]] <- as.integer(data[[column]])
    }
    log$ordered_factors <- list(columns = ordered_columns)
  }

  if (enable_ordinal_factors) {
    candidates <- predictors[vapply(data[predictors], is.factor, logical(1))]
    numeric_factors <- candidates[vapply(data[candidates], factor_is_numeric, logical(1))]
    for (column in numeric_factors) data[[column]] <- as.numeric(as.character(data[[column]]))
    recipe$numeric_factor_columns <- numeric_factors
    log$numeric_factors <- list(columns = numeric_factors)
  }

  factor_predictors <- setdiff(names(data)[vapply(data, is.factor, logical(1))], target_column)
  for (column in factor_predictors) {
    data[[column]] <- droplevels(data[[column]])
  }
  log$unused_factor_levels <- list(
    action = "dropped from the training recipe",
    columns = factor_predictors
  )

  recipe$final_columns <- names(data)
  recipe$factor_levels <- lapply(data[vapply(data, is.factor, logical(1))], base::levels)
  factor_predictors <- setdiff(names(recipe$factor_levels), target_column)
  recipe$factor_fallbacks <- lapply(factor_predictors, function(column) {
    values <- data[[column]][!is.na(data[[column]])]
    if (!length(values)) return(NULL)
    as.character(statistical_mode(values))
  })
  names(recipe$factor_fallbacks) <- factor_predictors
  result <- list(
    data = data,
    preprocessing_log = log,
    original_info = original,
    final_info = data_info(data),
    recipe = recipe
  )
  class(result) <- c("autoxplain_preprocessing", "list")
  preprocessing_message(verbose, "Preprocessing complete: ", nrow(data), " rows x ", ncol(data), " columns.")
  result
}

#' Prepare a data frame for the H2O adapter
#'
#' Compatibility alias for [preprocess_data()]. New code should use the
#' engine-neutral name.
#'
#' @inheritParams preprocess_data
#' @param ... Additional preprocessing controls passed to [preprocess_data()].
#'
#' @return The same auditable preprocessing result as [preprocess_data()].
#' @export
preprocess_for_h2o <- function(data, target_column, ...) {
  preprocess_data(data, target_column, ...)
}

apply_preprocessing_recipe <- function(data,
                                       recipe,
                                       target_column,
                                       missing_value_strategy = recipe$missing_value_strategy,
                                       novel_level_strategy = recipe$novel_level_strategy %||% "error") {
  assert_data_frame(data, "data")
  novel_level_strategy <- match.arg(novel_level_strategy, c("error", "mode"))
  original <- data_info(data)
  novel_level_mappings <- integer()
  if (length(recipe$removed_columns)) {
    data <- data[setdiff(names(data), recipe$removed_columns)]
  }
  if (identical(missing_value_strategy, "drop_rows")) {
    data <- data[stats::complete.cases(data), , drop = FALSE]
  }
  if (identical(missing_value_strategy, "impute")) {
    for (column in intersect(names(recipe$imputations), names(data))) {
      data[[column]][is.na(data[[column]])] <- recipe$imputations[[column]]
    }
  }
  for (column in intersect(names(recipe$ordered_columns), names(data))) {
    data[[column]] <- as.integer(factor(
      data[[column]], levels = recipe$ordered_columns[[column]], ordered = TRUE
    ))
  }
  for (column in intersect(recipe$numeric_factor_columns, names(data))) {
    data[[column]] <- as.numeric(as.character(data[[column]]))
  }
  for (column in intersect(names(recipe$factor_levels), names(data))) {
    raw <- as.character(data[[column]])
    unseen <- setdiff(unique(raw[!is.na(raw)]), recipe$factor_levels[[column]])
    if (length(unseen)) {
      if (identical(column, target_column) || identical(novel_level_strategy, "error")) {
        stop("Column `", column, "` has unseen levels: ", paste(unseen, collapse = ", "),
             call. = FALSE)
      }
      fallback <- recipe$factor_fallbacks[[column]] %||% NULL
      if (is.null(fallback) || !fallback %in% recipe$factor_levels[[column]]) {
        stop("Column `", column, "` has no observed training level for fallback mapping.",
             call. = FALSE)
      }
      replace <- !is.na(raw) & raw %in% unseen
      novel_level_mappings[[column]] <- sum(replace)
      raw[replace] <- fallback
    }
    data[[column]] <- factor(raw, levels = recipe$factor_levels[[column]])
  }
  missing <- setdiff(recipe$final_columns, names(data))
  if (length(missing)) stop("Evaluation data is missing processed columns: ",
                            paste(missing, collapse = ", "), call. = FALSE)
  data <- data[recipe$final_columns]
  structure(
    list(
      data = data,
      preprocessing_log = list(
        action = "applied_training_recipe",
        novel_level_strategy = novel_level_strategy,
        novel_level_mappings = novel_level_mappings
      ),
      original_info = original,
      final_info = data_info(data),
      recipe = recipe
    ),
    class = c("autoxplain_preprocessing", "list")
  )
}

#' @export
print.autoxplain_preprocessing <- function(x, ...) {
  cat("<AutoXplainR preprocessing>\n")
  cat("  before: ", x$original_info$nrows, " rows x ", x$original_info$ncols, " columns\n", sep = "")
  cat("  after:  ", x$final_info$nrows, " rows x ", x$final_info$ncols, " columns\n", sep = "")
  cat("  missing strategy: ", x$recipe$missing_value_strategy, "\n", sep = "")
  cat("  novel levels:     ", x$recipe$novel_level_strategy %||% "error", "\n", sep = "")
  if (length(x$recipe$removed_columns)) {
    cat("  removed: ", paste(x$recipe$removed_columns, collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}

remove_id_columns <- function(data, target_column, verbose = FALSE) {
  predictors <- setdiff(names(data), target_column)
  pattern <- "(^id$|^id_|_id$|^identifier$|^row_?id$|^index$)"
  columns <- predictors[grepl(pattern, tolower(predictors), perl = TRUE)]
  if (length(columns)) data <- data[setdiff(names(data), columns)]
  preprocessing_message(verbose, "Removed identifier-named columns: ",
                        if (length(columns)) paste(columns, collapse = ", ") else "none", ".")
  list(
    data = data,
    columns = columns,
    log = lapply(columns, function(column) list(column = column, reason = "identifier-like name"))
  )
}

handle_missing_values <- function(data,
                                  target_column,
                                  strategy = "keep",
                                  threshold = 0.5,
                                  verbose = FALSE) {
  strategy <- normalize_missing_strategy(strategy)
  missing <- vapply(data, function(x) sum(is.na(x)), integer(1))
  original_rows <- nrow(data)
  removed_columns <- character()
  imputations <- list()
  if (strategy == "drop_rows") {
    data <- data[stats::complete.cases(data), , drop = FALSE]
  } else if (strategy == "drop_columns") {
    fraction <- missing / max(1L, nrow(data))
    removed_columns <- setdiff(names(fraction)[fraction > threshold], target_column)
    if (length(removed_columns)) data <- data[setdiff(names(data), removed_columns)]
  } else if (strategy == "impute") {
    if (anyNA(data[[target_column]])) {
      stop("Target values cannot be imputed; remove or supply them explicitly.", call. = FALSE)
    }
    for (column in setdiff(names(data), target_column)) {
      if (!anyNA(data[[column]])) next
      value <- if (is.numeric(data[[column]])) {
        stats::median(data[[column]], na.rm = TRUE)
      } else {
        statistical_mode(data[[column]])
      }
      data[[column]][is.na(data[[column]])] <- value
      imputations[[column]] <- value
    }
  }
  preprocessing_message(verbose, "Missing-value strategy `", strategy, "` applied.")
  list(
    data = data,
    removed_columns = removed_columns,
    imputations = imputations,
    log = list(
      strategy = strategy,
      original_missing = missing[missing > 0L],
      rows_removed = original_rows - nrow(data),
      columns_removed = removed_columns,
      imputed_columns = names(imputations)
    )
  )
}

handle_target_variable <- function(data, target_column) {
  assert_data_frame(data, "data")
  if (is.character(data[[target_column]])) data[[target_column]] <- factor(data[[target_column]])
  list(data = data, conversions = list(target = class1(data[[target_column]])))
}

convert_character_to_factors <- function(data, target_column) {
  columns <- setdiff(names(data), target_column)
  columns <- columns[vapply(data[columns], is.character, logical(1))]
  for (column in columns) data[[column]] <- factor(data[[column]])
  list(data = data, conversions = as.list(setNames(rep("factor", length(columns)), columns)))
}

convert_ordered_factors <- function(data, target_column) {
  columns <- setdiff(names(data), target_column)
  columns <- columns[vapply(data[columns], is.ordered, logical(1))]
  for (column in columns) data[[column]] <- as.integer(data[[column]])
  list(data = data, conversions = as.list(setNames(rep("integer score", length(columns)), columns)))
}

convert_ordinal_factors <- function(data, target_column) {
  columns <- setdiff(names(data), target_column)
  columns <- columns[vapply(data[columns], is.factor, logical(1))]
  columns <- columns[vapply(data[columns], factor_is_numeric, logical(1))]
  for (column in columns) data[[column]] <- as.numeric(as.character(data[[column]]))
  list(data = data, conversions = as.list(setNames(rep("numeric", length(columns)), columns)))
}

normalize_missing_strategy <- function(strategy) {
  aliases <- c(remove_rows = "drop_rows", remove_columns = "drop_columns")
  if (length(strategy) == 1L && strategy %in% names(aliases)) strategy <- aliases[[strategy]]
  match.arg(strategy, c("keep", "drop_rows", "drop_columns", "impute"))
}

factor_is_numeric <- function(x) {
  is.factor(x) && all(grepl("^[+-]?[0-9]+(?:[.][0-9]+)?$", base::levels(x)))
}

statistical_mode <- function(x) {
  counts <- sort(table(x, useNA = "no"), decreasing = TRUE)
  if (!length(counts)) stop("Cannot impute a column containing only missing values.", call. = FALSE)
  value <- names(counts)[[1L]]
  if (is.factor(x)) factor(value, levels = base::levels(x)) else value
}

data_info <- function(data) {
  missing_by_column <- vapply(data, function(column) sum(is.na(column)), integer(1))
  list(
    nrows = nrow(data),
    ncols = ncol(data),
    column_names = names(data),
    column_types = vapply(data, class1, character(1)),
    missing_values = sum(missing_by_column),
    missing_by_column = missing_by_column,
    missing_fraction_by_column = missing_by_column / max(1L, nrow(data))
  )
}

preprocessing_message <- function(verbose, ...) {
  if (isTRUE(verbose)) message(...)
  invisible(NULL)
}
