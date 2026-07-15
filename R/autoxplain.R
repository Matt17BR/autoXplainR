#' Fit a reproducible H2O AutoML candidate set
#'
#' `autoxplain()` is the optional model-fitting convenience layer. The core
#' explanation and audit API does not require H2O; users can call
#' [explain_model()] for any fitted model.
#'
#' Numeric outcomes with exactly two distinct values are treated as binary
#' classification by default. Potentially destructive preprocessing, such as
#' identifier removal, is opt-in and recorded in the result.
#'
#' @param data Training data frame.
#' @param target_column Name of the outcome column.
#' @param max_models Maximum number of base models.
#' @param max_runtime_secs Overall training time budget in seconds.
#' @param seed Reproducible H2O seed.
#' @param test_data Optional held-out evaluation data. It is not used as an H2O
#'   validation frame unless `use_test_as_validation = TRUE`.
#' @param enable_preprocessing Apply [preprocess_for_h2o()].
#' @param preprocessing_config Named overrides for preprocessing. Identifier
#'   removal defaults to `FALSE`.
#' @param task One of `"auto"`, `"regression"`, `"binary"`, or
#'   `"multiclass"`.
#' @param nfolds Number of H2O cross-validation folds. At least two folds are
#'   required when stacked ensembles are desired.
#' @param sort_metric H2O AutoML leaderboard metric.
#' @param include_algos,exclude_algos Optional H2O algorithm filters. Supply at
#'   most one.
#' @param use_test_as_validation Whether to pass `test_data` to H2O as a
#'   validation frame. The default keeps held-out explanation data independent.
#' @param init_h2o Start a local H2O cluster when no connection is available.
#' @param h2o_nthreads Threads used when starting H2O.
#' @param h2o_max_mem_size Memory used when starting H2O.
#' @param verbosity One of `"quiet"` or `"info"`.
#'
#' @return An `autoxplain_result` containing models, a data-frame leaderboard,
#'   task metadata, preprocessing provenance, and evaluation data.
#' @export
#'
#' @examples
#' \dontrun{
#' result <- autoxplain(mtcars, "mpg", max_models = 3)
#' explainers <- as_explainers(result)
#' audit_explanations(explainers)
#' }
autoxplain <- function(data,
                       target_column,
                       max_models = 10L,
                       max_runtime_secs = 300L,
                       seed = 123L,
                       test_data = NULL,
                       enable_preprocessing = TRUE,
                       preprocessing_config = list(),
                       task = c("auto", "regression", "binary", "multiclass"),
                       nfolds = 5L,
                       sort_metric = "AUTO",
                       include_algos = NULL,
                       exclude_algos = NULL,
                       use_test_as_validation = FALSE,
                       init_h2o = TRUE,
                       h2o_nthreads = -1L,
                       h2o_max_mem_size = "2G",
                       verbosity = c("quiet", "info")) {
  require_optional("h2o", "fitting H2O AutoML models")
  task <- match.arg(task)
  verbosity <- match.arg(verbosity)
  validate_automl_inputs(data, target_column, test_data)
  max_models <- assert_count(max_models, "max_models")
  max_runtime_secs <- assert_count(max_runtime_secs, "max_runtime_secs")
  seed <- assert_count(seed, "seed", minimum = 0L)
  nfolds <- assert_count(nfolds, "nfolds", minimum = 0L)
  if (nfolds == 1L) stop("`nfolds` must be zero or at least two.", call. = FALSE)
  if (!is.null(include_algos) && !is.null(exclude_algos)) {
    stop("Supply only one of `include_algos` and `exclude_algos`.", call. = FALSE)
  }
  if (!is.list(preprocessing_config)) {
    stop("`preprocessing_config` must be a named list.", call. = FALSE)
  }
  if (!is.logical(use_test_as_validation) || length(use_test_as_validation) != 1L ||
        is.na(use_test_as_validation)) {
    stop("`use_test_as_validation` must be TRUE or FALSE.", call. = FALSE)
  }

  resolved_task <- if (task == "auto") detect_task(data[[target_column]]) else task
  config <- utils::modifyList(
    list(
      enable_target_handling = TRUE,
      enable_character_to_factors = TRUE,
      enable_ordered_factors = FALSE,
      enable_ordinal_factors = FALSE,
      enable_id_removal = FALSE,
      missing_value_strategy = "keep",
      verbose = verbosity == "info"
    ),
    preprocessing_config
  )

  train_processed <- if (enable_preprocessing) {
    do.call(preprocess_for_h2o, c(list(data = data, target_column = target_column), config))
  } else {
    list(data = data, preprocessing_log = list(), original_info = data_info(data),
         final_info = data_info(data), recipe = list())
  }
  train <- train_processed$data
  train[[target_column]] <- coerce_outcome_for_task(train[[target_column]], resolved_task)

  test_processed <- NULL
  evaluation <- test_data
  if (!is.null(test_data)) {
    test_processed <- if (enable_preprocessing) {
      apply_preprocessing_recipe(test_data, train_processed$recipe, target_column,
                                 missing_value_strategy = config$missing_value_strategy)
    } else {
      list(data = test_data, preprocessing_log = list(), original_info = data_info(test_data),
           final_info = data_info(test_data), recipe = list())
    }
    evaluation <- test_processed$data
    evaluation[[target_column]] <- coerce_outcome_for_task(
      evaluation[[target_column]], resolved_task,
      target_levels = if (is.factor(train[[target_column]])) levels(train[[target_column]]) else NULL
    )
    evaluation <- validate_train_test_schema(train, evaluation, target_column)
  }

  ensure_h2o_connection(init_h2o, h2o_nthreads, h2o_max_mem_size, verbosity)
  h2o_training <- h2o::as.h2o(train)
  h2o_validation <- if (use_test_as_validation && !is.null(evaluation)) {
    h2o::as.h2o(evaluation)
  } else {
    NULL
  }
  features <- setdiff(names(train), target_column)
  arguments <- list(
    x = features,
    y = target_column,
    training_frame = h2o_training,
    max_models = max_models,
    max_runtime_secs = max_runtime_secs,
    seed = seed,
    sort_metric = sort_metric,
    nfolds = nfolds,
    verbosity = if (verbosity == "quiet") NULL else "info"
  )
  if (!is.null(h2o_validation)) arguments$validation_frame <- h2o_validation
  if (!is.null(include_algos)) arguments$include_algos <- include_algos
  if (!is.null(exclude_algos)) arguments$exclude_algos <- exclude_algos
  arguments <- arguments[!vapply(arguments, is.null, logical(1))]

  automl <- do.call(h2o::h2o.automl, arguments)
  leaderboard <- as.data.frame(automl@leaderboard)
  model_ids <- head(as.character(leaderboard$model_id), max_models)
  models <- lapply(model_ids, h2o::h2o.getModel)
  names(models) <- model_ids
  if (!length(models)) stop("H2O AutoML returned no retrievable models.", call. = FALSE)

  result <- structure(
    list(
      models = models,
      leaderboard = leaderboard,
      training_data = train,
      test_data = evaluation,
      target_column = target_column,
      features = features,
      task = resolved_task,
      automl_object = automl,
      model_characteristics = NULL,
      preprocessing_metadata = list(
        enabled = enable_preprocessing,
        training_data = train_processed,
        test_data = test_processed
      ),
      provenance = list(
        created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
        seed = seed,
        max_models = max_models,
        max_runtime_secs = max_runtime_secs,
        nfolds = nfolds,
        sort_metric = sort_metric,
        test_used_for_validation = use_test_as_validation,
        evaluation_role = if (is.null(test_data)) "training" else if (use_test_as_validation) {
          "validation"
        } else {
          "test"
        }
      )
    ),
    class = "autoxplain_result"
  )
  result$model_characteristics <- tryCatch(
    extract_model_characteristics(result),
    error = function(error) structure(list(), class = "autoxplainr_model_characteristics")
  )
  result
}

#' Convert an AutoML result to model-agnostic explainers
#'
#' @param x An `autoxplain_result`.
#' @param data Evaluation data. Defaults to held-out `test_data` when available,
#'   otherwise training data.
#' @param models Model indices, IDs, or `NULL` for all retained models.
#'
#' @return A named list of `autoxplain_explainer` objects.
#' @export
as_explainers <- function(x, data = NULL, models = NULL) {
  if (!inherits(x, "autoxplain_result")) {
    stop("`x` must be an `autoxplain_result`.", call. = FALSE)
  }
  evaluation <- data %||% x$test_data %||% x$training_data
  assert_data_frame(evaluation, "data")
  if (!x$target_column %in% names(evaluation)) {
    stop("Evaluation data must contain target column `", x$target_column, "`.",
         call. = FALSE)
  }
  selected <- select_models(x$models, models)
  role <- if (!is.null(data)) "user-supplied" else x$provenance$evaluation_role
  out <- lapply(names(selected), function(id) {
    explain_model(
      selected[[id]], evaluation, y = x$target_column, task = x$task,
      label = id, metadata = list(evaluation_role = role, source = "H2O AutoML")
    )
  })
  names(out) <- names(selected)
  out
}

#' @export
print.autoxplain_result <- function(x, ...) {
  cat("<AutoXplainR H2O AutoML result>\n")
  cat("  task:       ", x$task, "\n", sep = "")
  cat("  target:     ", x$target_column, "\n", sep = "")
  cat("  models:     ", length(x$models), "\n", sep = "")
  cat("  training:   ", nrow(x$training_data), " rows x ", length(x$features),
      " features\n", sep = "")
  cat("  evaluation: ", x$provenance$evaluation_role, "\n", sep = "")
  invisible(x)
}

#' @export
summary.autoxplain_result <- function(object, ...) {
  list(
    task = object$task,
    target = object$target_column,
    n_models = length(object$models),
    n_features = length(object$features),
    leaderboard = object$leaderboard,
    provenance = object$provenance
  )
}

validate_automl_inputs <- function(data, target, test_data) {
  assert_data_frame(data, "data")
  if (!is.character(target) || length(target) != 1L || is.na(target) || !target %in% names(data)) {
    stop("`target_column` must name one column in `data`.", call. = FALSE)
  }
  if (nrow(data) < 10L) stop("`data` must contain at least ten rows.", call. = FALSE)
  if (anyDuplicated(names(data))) stop("`data` must have unique column names.", call. = FALSE)
  if (all(is.na(data[[target]])) || length(unique(data[[target]][!is.na(data[[target]])])) < 2L) {
    stop("The target must contain at least two observed values.", call. = FALSE)
  }
  if (!is.null(test_data)) {
    assert_data_frame(test_data, "test_data")
    if (!target %in% names(test_data)) stop("`test_data` is missing the target column.",
                                            call. = FALSE)
  }
  invisible(TRUE)
}

coerce_outcome_for_task <- function(y, task, target_levels = NULL) {
  if (task == "regression") {
    if (!is.numeric(y)) stop("Regression requires a numeric target.", call. = FALSE)
    return(y)
  }
  factor(
    y,
    levels = target_levels %||% if (is.factor(y)) base::levels(y) else sort(unique(y[!is.na(y)]))
  )
}

validate_train_test_schema <- function(train, test, target) {
  missing <- setdiff(names(train), names(test))
  if (length(missing)) stop("Processed test data is missing: ", paste(missing, collapse = ", "),
                            call. = FALSE)
  for (feature in setdiff(names(train), target)) {
    if (is.factor(train[[feature]]) && is.factor(test[[feature]])) {
      unseen <- setdiff(levels(droplevels(test[[feature]])), levels(train[[feature]]))
      if (length(unseen)) {
        stop("Test feature `", feature, "` contains unseen levels: ",
             paste(unseen, collapse = ", "), call. = FALSE)
      }
      test[[feature]] <- factor(test[[feature]], levels = levels(train[[feature]]))
    }
  }
  test[names(train)]
}

ensure_h2o_connection <- function(init, nthreads, max_mem_size, verbosity) {
  connected <- tryCatch(h2o::h2o.clusterIsUp(), error = function(error) FALSE)
  if (isTRUE(connected)) return(invisible(TRUE))
  if (!isTRUE(init)) {
    stop("No H2O cluster is available and `init_h2o = FALSE`.", call. = FALSE)
  }
  if (verbosity == "quiet") {
    invisible(utils::capture.output(
      h2o::h2o.init(nthreads = nthreads, max_mem_size = max_mem_size)
    ))
  } else {
    h2o::h2o.init(nthreads = nthreads, max_mem_size = max_mem_size)
  }
  if (!isTRUE(tryCatch(h2o::h2o.clusterIsUp(), error = function(error) FALSE))) {
    stop("H2O did not become available after initialization.", call. = FALSE)
  }
  invisible(TRUE)
}

select_models <- function(models, selection) {
  if (is.null(selection)) return(models)
  if (is.numeric(selection)) {
    indices <- as.integer(selection)
    if (any(indices < 1L | indices > length(models))) {
      stop("Numeric `models` indices are out of range.", call. = FALSE)
    }
    return(models[indices])
  }
  if (is.character(selection)) {
    missing <- setdiff(selection, names(models))
    if (length(missing)) stop("Unknown model IDs: ", paste(missing, collapse = ", "),
                              call. = FALSE)
    return(models[selection])
  }
  stop("`models` must be NULL, numeric indices, or model IDs.", call. = FALSE)
}
