#' Keep groups or future observations out of training
#'
#' Defines a study boundary for [autoxplain()]. Group splitting holds out whole
#' groups, targeting prediction for previously unseen groups. Temporal splitting
#' holds out the latest distinct times, targeting prediction after the training
#' period. Neither design guarantees transport to a different population.
#'
#' @param group Name of a group column (for example patient or site ID).
#' @param time Name of a numeric, `Date`, or `POSIXct` time column. Supply exactly
#'   one of `group` and `time`. Tied times always stay together.
#' @param gap Number of distinct time values to exclude immediately before the
#'   test period. Only available with `time`; the units are time values, not rows
#'   or elapsed days. This does not prevent leakage from incorrectly built lagged
#'   features or delayed labels.
#'
#' @details The split column is removed from predictors. `test_fraction` in
#'   [autoxplain()] refers to groups or distinct time values; row fractions can
#'   differ. `test_data` cannot be combined with this design.
#'
#'   Grouped tuning allocates entire training groups to inner validation folds,
#'   balancing row counts. Each classification fold must contain all classes;
#'   infeasible designs fail with an explanation. Temporal designs currently
#'   support `model_set = "quick"` or `"comparison"`; random-fold tuning and H2O
#'   are rejected. Use an external rolling-origin workflow for temporal tuning.
#'
#' @return A validated `autoxplain_validation` specification. The fitted result's
#'   `validation` component records original row indices for each partition,
#'   excluded gap rows, and group fold membership where applicable.
#' @export
#' @examples
#' data <- transform(mtcars, vehicle_group = rep(1:8, each = 4))
#' result <- autoxplain(data, "mpg", validation = validation_split(group = "vehicle_group"),
#'                      explain = FALSE)
validation_split <- function(group = NULL, time = NULL, gap = 0L) {
  if (is.null(group) == is.null(time)) {
    stop("Supply exactly one of `group` and `time`.", call. = FALSE)
  }
  column <- group %||% time
  if (!is.character(column) || length(column) != 1L || is.na(column) || !nzchar(column)) {
    stop("The split column must be one non-empty column name.", call. = FALSE)
  }
  gap <- assert_count(gap, "gap", minimum = 0L)
  if (!is.null(group) && gap > 0L) stop("`gap` is available only with `time`.", call. = FALSE)
  structure(list(method = if (is.null(group)) "temporal" else "group",
                 column = column, gap = gap), class = "autoxplain_validation")
}

prepare_validation_design <- function(data, target, test_data, validation, fraction, seed,
                                      engine, model_set, task, nfolds, control) {
  if (is.null(validation)) return(NULL)
  if (!inherits(validation, "autoxplain_validation")) {
    stop("`validation` must be created by `validation_split()`.", call. = FALSE)
  }
  if (!is.null(test_data)) stop("Supply `validation` or `test_data`, not both.", call. = FALSE)
  if (engine != "base") stop("Structured validation currently requires the base engine.", call. = FALSE)
  if (validation$method == "temporal" && model_set == "tuned") {
    stop("Temporal tuning requires rolling-origin resampling; use `quick` or `comparison`.", call. = FALSE)
  }
  column <- validation$column
  if (!column %in% names(data) || column == target) {
    stop("The split column must exist and must differ from the target.", call. = FALSE)
  }
  value <- data[[column]]
  if (anyNA(value) || !is.null(dim(value)) || is.list(value)) {
    stop("The split column must be an atomic vector without missing values.", call. = FALSE)
  }
  if (is.numeric(value) && any(!is.finite(value))) {
    stop("Numeric split values must be finite.", call. = FALSE)
  }
  seed <- assert_count(seed, "seed", minimum = 0L)
  gap_rows <- integer()
  fold_ids <- NULL
  if (validation$method == "group") {
    values <- unique(as.character(value))
    if (any(!nzchar(values)) || length(values) < 3L) {
      stop("Grouped validation requires at least three non-empty groups.", call. = FALSE)
    }
    count <- max(1L, min(length(values) - 2L, as.integer(round(length(values) * fraction))))
    selected <- with_preserved_seed(seed, values[sample.int(length(values), count)])
    evaluation <- which(as.character(value) %in% selected)
    training <- setdiff(seq_len(nrow(data)), evaluation)
    if (model_set == "tuned") {
      if (!is.null(control) && !inherits(control, "autoxplain_tuning_control")) {
        stop("`tuning_control` must be created by `tuning_control()`.", call. = FALSE)
      }
      if (!is.null(control$fold_ids)) {
        stop("Grouped validation creates its own fold IDs; remove supplied `fold_ids`.", call. = FALSE)
      }
      fold_ids <- grouped_fold_ids(as.character(value[training]), nfolds, seed)
      fold_ids <- match(fold_ids, unique(fold_ids))
      control <- control %||% tuning_control()
      control$fold_ids <- fold_ids
    }
  } else {
    if (!(is.numeric(value) || inherits(value, c("Date", "POSIXct")))) {
      stop("Temporal validation requires a numeric, Date, or POSIXct column.", call. = FALSE)
    }
    values <- sort(unique(value))
    count <- max(1L, as.integer(round(length(values) * fraction)))
    last_training <- length(values) - count - validation$gap
    if (last_training < 1L) stop("Too few distinct times for this test fraction and gap.", call. = FALSE)
    training <- which(value <= values[[last_training]])
    evaluation <- which(value >= values[[length(values) - count + 1L]])
    gap_rows <- setdiff(seq_len(nrow(data)), c(training, evaluation))
  }
  if (length(training) < 10L || length(evaluation) < 2L) {
    stop("The design must leave at least ten training rows and two evaluation rows.", call. = FALSE)
  }
  task <- if (task == "auto") detect_task(data[[target]]) else task
  validate_guided_target(data[[target]][training], task)
  if (task != "regression" &&
        length(setdiff(as.character(data[[target]][evaluation]), as.character(data[[target]][training])))) {
    stop("Evaluation classes must all occur in the training partition.", call. = FALSE)
  }
  predictors <- setdiff(names(data), column)
  list(
    training = data[training, predictors, drop = FALSE],
    evaluation = data[evaluation, predictors, drop = FALSE],
    tuning_control = control,
    provenance = list(
      method = validation$method, column = column, fraction = fraction, gap = validation$gap,
      training_rows = training, evaluation_rows = evaluation, excluded_rows = gap_rows,
      evaluation_row_names = rownames(data)[evaluation],
      evaluation_groups = if (validation$method == "group") as.character(value[evaluation]) else NULL,
      training_groups = if (validation$method == "group") as.character(value[training]) else NULL,
      fold_ids = fold_ids,
      estimand = if (validation$method == "group") "new groups" else "later observations"
    )
  )
}

grouped_fold_ids <- function(groups, requested, seed) {
  requested <- assert_count(requested, "nfolds", minimum = 2L)
  labels <- unique(groups)
  folds <- min(requested, length(labels))
  sizes <- tabulate(match(groups, labels), nbins = length(labels))
  order <- with_preserved_seed(seed, sample.int(length(labels)))
  order <- order[order(sizes[order], decreasing = TRUE)]
  totals <- integer(folds)
  assignment <- integer(length(labels))
  for (index in order) {
    fold <- which.min(totals)
    assignment[[index]] <- fold
    totals[[fold]] <- totals[[fold]] + sizes[[index]]
  }
  assignment[match(groups, labels)]
}
