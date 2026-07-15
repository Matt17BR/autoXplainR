#' Create a model-agnostic explainer
#'
#' `explain_model()` defines the prediction contract used by AutoXplainR.  It
#' deliberately separates model fitting from explanation, so base R models,
#' H2O models, and models from other frameworks can be audited in the same way.
#'
#' @param model A fitted model object.
#' @param data A data frame. It may contain the outcome when `y` is the name of
#'   a column; the outcome is removed from the feature data stored in the
#'   explainer.
#' @param y Outcome values, or a single column name in `data`.
#' @param predict_function Optional prediction function. It may have signature
#'   `function(model, newdata)` or `function(newdata)`. Regression functions
#'   should return a numeric vector. Classification functions should return
#'   probabilities (a vector for binary outcomes or a matrix/data frame with
#'   one column per class).
#' @param task One of `"auto"`, `"regression"`, `"binary"`, or
#'   `"multiclass"`.
#' @param label Human-readable model label.
#' @param positive Positive outcome level for binary classification. By
#'   default the second outcome level is used.
#' @param metadata Optional named list recorded in the explainer provenance.
#'
#' @return An object of class `autoxplain_explainer`.
#' @export
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' explainer <- explain_model(fit, mtcars, y = "mpg", label = "linear model")
#' explainer
explain_model <- function(model,
                          data,
                          y,
                          predict_function = NULL,
                          task = c("auto", "regression", "binary", "multiclass"),
                          label = NULL,
                          positive = NULL,
                          metadata = list()) {
  task <- match.arg(task)
  assert_data_frame(data, "data")
  if (nrow(data) < 2L) {
    stop("`data` must contain at least two rows.", call. = FALSE)
  }
  if (anyDuplicated(names(data))) {
    stop("`data` must have unique column names.", call. = FALSE)
  }

  target <- NULL
  if (is.character(y) && length(y) == 1L && y %in% names(data)) {
    target <- y
    outcome <- data[[y]]
    feature_data <- data[setdiff(names(data), y)]
  } else {
    outcome <- y
    feature_data <- data
  }

  if (length(outcome) != nrow(feature_data)) {
    stop("`y` must have one value for every row in `data`.", call. = FALSE)
  }
  if (anyNA(outcome)) {
    stop("`y` contains missing values; use a complete evaluation set.", call. = FALSE)
  }
  if (ncol(feature_data) < 1L) {
    stop("At least one predictor column is required.", call. = FALSE)
  }
  if (!is.null(predict_function) && !is.function(predict_function)) {
    stop("`predict_function` must be a function or NULL.", call. = FALSE)
  }
  if (!is.null(label) && (!is.character(label) || length(label) != 1L || is.na(label))) {
    stop("`label` must be a single non-missing string.", call. = FALSE)
  }
  if (!is.list(metadata) || is.null(names(metadata)) && length(metadata)) {
    stop("`metadata` must be a named list.", call. = FALSE)
  }

  resolved_task <- if (identical(task, "auto")) detect_task(outcome) else task
  outcome_levels <- if (resolved_task %in% c("binary", "multiclass")) {
    if (is.factor(outcome)) levels(outcome) else sort(unique(as.character(outcome)))
  } else {
    NULL
  }
  if (identical(resolved_task, "binary")) {
    if (length(outcome_levels) != 2L) {
      stop("Binary classification requires exactly two outcome levels.", call. = FALSE)
    }
    positive <- positive %||% outcome_levels[[2L]]
    if (!as.character(positive) %in% outcome_levels) {
      stop("`positive` must be one of the observed outcome levels.", call. = FALSE)
    }
    positive <- as.character(positive)
  }

  prediction_adapter <- make_prediction_adapter(
    model = model,
    task = resolved_task,
    positive = positive,
    class_levels = outcome_levels,
    predict_function = predict_function
  )

  # Fail early with a small prediction, before an expensive audit starts.
  probe_n <- min(3L, nrow(feature_data))
  probe <- prediction_adapter(feature_data[seq_len(probe_n), , drop = FALSE])
  validate_predictions(probe, probe_n, resolved_task, outcome_levels)

  label <- label %||% paste(class(model), collapse = "/")
  created_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  provenance <- list(
    created_at = created_at,
    package_version = package_version_or_development(),
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    model_class = class(model),
    data_rows = nrow(feature_data),
    data_columns = ncol(feature_data),
    feature_names = names(feature_data),
    fingerprint = lightweight_fingerprint(
      list(label, class(model), names(feature_data),
           vapply(feature_data, class1, character(1)), nrow(feature_data),
           resolved_task, outcome_levels)
    )
  )

  structure(
    list(
      model = model,
      data = feature_data,
      y = outcome,
      target = target,
      task = resolved_task,
      positive = positive,
      class_levels = outcome_levels,
      label = label,
      predict_function = prediction_adapter,
      metadata = metadata,
      provenance = provenance
    ),
    class = "autoxplain_explainer"
  )
}

#' @export
print.autoxplain_explainer <- function(x, ...) {
  cat("<AutoXplainR explainer>\n", sep = "")
  cat("  model:    ", x$label, "\n", sep = "")
  cat("  task:     ", x$task, "\n", sep = "")
  cat("  data:     ", nrow(x$data), " rows x ", ncol(x$data), " features\n", sep = "")
  if (!is.null(x$positive)) cat("  positive: ", x$positive, "\n", sep = "")
  cat("  id:       ", x$provenance$fingerprint, "\n", sep = "")
  invisible(x)
}

#' Predict with an AutoXplainR explainer
#'
#' @param object An `autoxplain_explainer`.
#' @param newdata Data frame with the explainer's feature schema.
#' @param ... Unused.
#'
#' @return A numeric vector for regression or binary classification, or a
#'   probability matrix for multiclass classification.
#' @export
predict.autoxplain_explainer <- function(object, newdata, ...) {
  assert_data_frame(newdata, "newdata")
  missing_features <- setdiff(names(object$data), names(newdata))
  if (length(missing_features)) {
    stop(
      "`newdata` is missing required features: ",
      paste(missing_features, collapse = ", "),
      call. = FALSE
    )
  }
  newdata <- newdata[names(object$data)]
  out <- object$predict_function(newdata)
  validate_predictions(out, nrow(newdata), object$task, object$class_levels)
  out
}

detect_task <- function(y) {
  values <- unique(y[!is.na(y)])
  if (is.logical(y) || is.factor(y) || is.character(y)) {
    return(if (length(values) == 2L) "binary" else "multiclass")
  }
  if (is.numeric(y) && length(values) == 2L) return("binary")
  "regression"
}

make_prediction_adapter <- function(model,
                                    task,
                                    positive = NULL,
                                    class_levels = NULL,
                                    predict_function = NULL) {
  function(newdata) {
    raw <- if (!is.null(predict_function)) {
      invoke_user_predict(predict_function, model, newdata)
    } else if (inherits(model, "H2OModel")) {
      require_optional("h2o", "explaining H2O models")
      as.data.frame(h2o::h2o.predict(model, h2o::as.h2o(newdata)))
    } else if (inherits(model, "glm") && identical(model$family$family, "binomial")) {
      stats::predict(model, newdata = newdata, type = "response")
    } else {
      default_predict(model, newdata, task)
    }
    normalize_predictions(raw, task, positive, class_levels)
  }
}

invoke_user_predict <- function(fun, model, newdata) {
  n_args <- length(formals(fun))
  if (n_args <= 1L) fun(newdata) else fun(model, newdata)
}

default_predict <- function(model, newdata, task) {
  if (task %in% c("binary", "multiclass")) {
    probability <- tryCatch(
      stats::predict(model, newdata = newdata, type = "prob"),
      error = function(e) NULL
    )
    if (!is.null(probability)) return(probability)
  }
  tryCatch(
    stats::predict(model, newdata = newdata, type = "response"),
    error = function(e) stats::predict(model, newdata = newdata)
  )
}

normalize_predictions <- function(x, task, positive = NULL, class_levels = NULL) {
  if (task == "regression") {
    if (is.data.frame(x) || is.matrix(x)) {
      if ("predict" %in% colnames(x)) x <- x[, "predict"] else x <- x[, 1L]
    }
    return(as.numeric(x))
  }

  if (task == "binary") {
    if (is.factor(x) || is.character(x) || is.logical(x)) {
      return(as.numeric(as.character(x) == positive))
    }
    if (is.data.frame(x) || is.matrix(x)) {
      x <- as.data.frame(x, check.names = FALSE)
      probability_names <- setdiff(names(x), "predict")
      candidates <- unique(c(
        positive,
        make.names(positive),
        paste0("p", positive),
        paste0("prob_", positive)
      ))
      selected <- intersect(candidates, probability_names)
      if (length(selected)) return(as.numeric(x[[selected[[1L]]]]))
      if (length(probability_names) >= 2L) {
        return(as.numeric(x[[probability_names[[2L]]]]))
      }
      if (length(probability_names) == 1L) {
        return(as.numeric(x[[probability_names[[1L]]]]))
      }
    }
    return(as.numeric(x))
  }

  if (is.vector(x) && !is.list(x)) {
    # Hard multiclass predictions remain usable for accuracy, but probability
    # metrics will reject them with a targeted message.
    return(factor(x, levels = class_levels))
  }
  out <- as.matrix(x)
  if ("predict" %in% colnames(out)) out <- out[, colnames(out) != "predict", drop = FALSE]
  out
}

validate_predictions <- function(x, n, task, class_levels = NULL) {
  actual_n <- if (is.matrix(x) || is.data.frame(x)) nrow(x) else length(x)
  if (actual_n != n) {
    stop("The prediction function returned ", actual_n, " predictions for ", n,
         " rows.", call. = FALSE)
  }
  if (task %in% c("regression", "binary") && (!is.numeric(x) || any(!is.finite(x)))) {
    stop("The prediction function must return finite numeric predictions.", call. = FALSE)
  }
  if (task == "binary" && any(x < 0 | x > 1)) {
    stop("Binary predictions must be probabilities between 0 and 1.", call. = FALSE)
  }
  if (task == "multiclass" && is.matrix(x)) {
    if (ncol(x) < 2L || any(!is.finite(x)) || any(x < 0 | x > 1)) {
      stop("Multiclass predictions must contain finite probabilities for each class.",
           call. = FALSE)
    }
    if (is.null(colnames(x)) || !all(class_levels %in% colnames(x))) {
      stop("Multiclass probability columns must be named with every outcome class.",
           call. = FALSE)
    }
    if (any(abs(rowSums(x[, class_levels, drop = FALSE]) - 1) > 1e-6)) {
      stop("Multiclass probabilities must sum to one for each row.", call. = FALSE)
    }
  }
  if (task == "multiclass" && !is.matrix(x) && anyNA(x)) {
    stop("Hard multiclass predictions must use observed outcome classes.", call. = FALSE)
  }
  invisible(TRUE)
}

assert_data_frame <- function(x, name) {
  if (!is.data.frame(x)) stop("`", name, "` must be a data frame.", call. = FALSE)
  invisible(TRUE)
}

assert_count <- function(x, name, minimum = 1L) {
  if (length(x) != 1L || is.na(x) || !is.numeric(x) || x < minimum || x != as.integer(x)) {
    stop("`", name, "` must be a whole number >= ", minimum, ".", call. = FALSE)
  }
  as.integer(x)
}

assert_probability <- function(x, name, open = FALSE) {
  valid <- length(x) == 1L && is.numeric(x) && is.finite(x)
  valid <- valid && if (open) x > 0 && x < 1 else x >= 0 && x <= 1
  if (!valid) stop("`", name, "` must be ", if (open) "between" else "from",
                   " 0 and 1", if (open) " (exclusive)" else " (inclusive)", ".",
                   call. = FALSE)
  invisible(TRUE)
}

require_optional <- function(package, reason) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Package `", package, "` is required for ", reason,
         ". Install it with install.packages(\"", package, "\").", call. = FALSE)
  }
  invisible(TRUE)
}

with_preserved_seed <- function(seed, code) {
  assert_count(seed, "seed", minimum = 0L)
  withr::with_seed(as.integer(seed), code)
}

class1 <- function(x) class(x)[[1L]]

package_version_or_development <- function() {
  tryCatch(as.character(utils::packageVersion("AutoXplainR")), error = function(e) "development")
}

lightweight_fingerprint <- function(x) {
  bytes <- as.integer(serialize(x, NULL, version = 2L))
  hash <- 2166136261
  for (byte in bytes) hash <- (hash * 16777619 + byte) %% 4294967291
  sprintf("axr-%08x", as.integer(hash %% .Machine$integer.max))
}
