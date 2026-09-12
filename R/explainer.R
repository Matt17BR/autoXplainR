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
#'   Hard classification labels are retained as factors for accuracy only;
#'   they cannot supply probability losses or probability effects. An ellipsis
#'   after the supported arguments is allowed.
#' @param task One of `"auto"`, `"regression"`, `"binary"`, or
#'   `"multiclass"`.
#' @param label Human-readable model label.
#' @param positive Positive outcome level for binary classification. By
#'   default the second outcome level is used.
#' @param metadata Optional named list recorded in the explainer provenance.
#' @param probability_class Event represented by a binary probability vector.
#'   For custom functions it defaults to `positive`. For native adapters it
#'   overrides the inferred event; supply it when fitted response levels are
#'   unavailable, such as a factor GLM fitted with `model = FALSE`. Numeric
#'   binomial GLMs model event `"1"`, logical GLMs model `"TRUE"`, and factor
#'   GLMs model their second fitted level. Named probability matrices identify
#'   their events directly.
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
                          metadata = list(),
                          probability_class = NULL) {
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

  resolved_task <- if (identical(task, "auto")) detect_task(outcome, declared_levels = TRUE) else task
  if (resolved_task == "regression") {
    validate_guided_target(outcome, resolved_task)
  } else if (!is.factor(outcome)) {
    validate_guided_target(outcome, resolved_task)
  }
  outcome_levels <- if (resolved_task %in% c("binary", "multiclass")) {
    if (is.factor(outcome)) levels(outcome) else sort(unique(as.character(outcome)))
  } else {
    NULL
  }
  if (identical(resolved_task, "multiclass") && length(outcome_levels) < 3L) {
    stop("Multiclass evaluation requires at least three declared outcome levels.", call. = FALSE)
  }
  if (identical(resolved_task, "binary")) {
    if (length(outcome_levels) != 2L) {
      stop("Binary classification requires exactly two outcome levels.", call. = FALSE)
    }
    positive <- positive %||% outcome_levels[[2L]]
    if (length(positive) != 1L || is.na(positive) || !as.character(positive) %in% outcome_levels) {
      stop("`positive` must be one of the observed outcome levels.", call. = FALSE)
    }
    positive <- as.character(positive)
  }

  prediction_adapter <- make_prediction_adapter(
    model = model,
    task = resolved_task,
    positive = positive,
    class_levels = outcome_levels,
    predict_function = predict_function,
    probability_class = probability_class
  )

  # Fail early with a small prediction, before an expensive audit starts.
  probe_n <- min(3L, nrow(feature_data))
  probe <- prediction_adapter(feature_data[seq_len(probe_n), , drop = FALSE])
  validate_predictions(probe, probe_n, resolved_task, outcome_levels)

  reference_predictions <- prediction_adapter(feature_data)
  validate_predictions(reference_predictions, nrow(feature_data), resolved_task, outcome_levels)
  custom_instance <- if (!is.null(predict_function)) new_explainer_instance() else NULL

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
    identity_version = "3",
    custom_instance = custom_instance,
    fingerprint = explainer_content_fingerprint(
      model, feature_data, outcome, resolved_task, positive, outcome_levels,
      reference_predictions, custom_instance, prediction_adapter
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
      reference_predictions = reference_predictions,
      prediction_type = if (is.factor(probe)) {
        "class"
      } else if (resolved_task == "regression") {
        "numeric"
      } else {
        "probability"
      },
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
#'   probability matrix for multiclass classification. Adapters that supply only
#'   class labels return a factor; probability-based diagnostics reject those labels.
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

detect_task <- function(y, declared_levels = FALSE) {
  # Evaluation data may omit a class that the fitted model still predicts.
  # Training callers keep inferring from observed classes, so unused factor
  # levels do not introduce classes that the model cannot learn.
  if (isTRUE(declared_levels) && is.factor(y)) {
    return(if (nlevels(y) == 2L) "binary" else "multiclass")
  }
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
                                    predict_function = NULL,
                                    probability_class = NULL) {
  native_event <- if (task == "binary") {
    probability_class %||% if (!is.null(predict_function)) {
      positive
    } else {
      binary_model_event(model, class_levels)
    }
  } else {
    NULL
  }
  if (!is.null(native_event) && (length(native_event) != 1L || is.na(native_event) ||
                                   !native_event %in% class_levels)) {
    stop("`probability_class` must name one binary outcome level.", call. = FALSE)
  }
  function(newdata) {
    raw <- if (!is.null(predict_function)) {
      invoke_user_predict(predict_function, model, newdata)
    } else if (inherits(model, "H2OModel")) {
      require_optional("h2o", "explaining H2O models")
      as.data.frame(h2o::h2o.predict(model, h2o::as.h2o(newdata)))
    } else if (inherits(model, "glm") && model$family$family %in% c("binomial", "quasibinomial")) {
      stats::predict(model, newdata = newdata, type = "response")
    } else {
      default_predict(model, newdata, task)
    }
    normalize_predictions(
      raw,
      task,
      positive,
      class_levels,
      n = nrow(newdata),
      probability_class = native_event
    )
  }
}

invoke_user_predict <- function(fun, model, newdata) {
  n_args <- length(setdiff(names(formals(fun)), "..."))
  if (!n_args %in% 1:2) {
    stop("`predict_function` must take newdata, or model and newdata, with optional `...`.",
         call. = FALSE)
  }
  if (n_args <= 1L) fun(newdata) else fun(model, newdata)
}

binary_model_event <- function(model, class_levels) {
  if (inherits(model, "autoxplain_fitted_model")) return(model$class_levels[[2L]])
  if (inherits(model, "autoxplain_tuned_nnet")) return(model$class_levels[[2L]])
  if (inherits(model, "glm") && model$family$family %in% c("binomial", "quasibinomial")) {
    response <- if (!is.null(model$model)) stats::model.response(model$model) else NULL
    if (is.factor(response) && nlevels(response) == 2L) return(levels(response)[[2L]])
    if (is.logical(response)) return("TRUE")
    if (is.numeric(response) && is.null(dim(response))) return("1")
    response_type <- attr(model$terms, "dataClasses")[[1L]] %||% "unknown"
    if (identical(response_type, "numeric")) return("1")
    if (identical(response_type, "logical")) return("TRUE")
    stop("The native binomial GLM probability event is unavailable. ",
         "Supply `probability_class` with the event represented by its response probability, ",
         "or retain the fitted model frame with `model = TRUE`.", call. = FALSE)
  }
  class_levels[[2L]]
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

normalize_predictions <- function(x,
                                  task,
                                  positive = NULL,
                                  class_levels = NULL,
                                  n = NULL,
                                  probability_class = NULL) {
  # mgcv can return one prediction per row as a one-dimensional array.
  # Flatten only that unambiguous shape; matrices still need named columns.
  if (task %in% c("regression", "binary") && is.numeric(x) && length(dim(x)) == 1L) {
    x <- as.numeric(x)
  }
  if (task == "regression") {
    if (is.data.frame(x) || is.matrix(x)) {
      if ("predict" %in% colnames(x)) {
        x <- x[, "predict"]
      } else if (ncol(x) == 1L) {
        x <- x[, 1L]
      } else {
        stop(
          "Regression prediction output with multiple columns must include a ",
          "column named `predict`.",
          call. = FALSE
        )
      }
    }
    if (!is.numeric(x) || !is.null(dim(x))) {
      stop("Regression prediction output must be numeric, not factors or labels.", call. = FALSE)
    }
    return(as.numeric(x))
  }

  if (task == "binary") {
    if (is.factor(x) || is.character(x) || is.logical(x)) {
      labels <- as.character(x)
      if (anyNA(labels) || is.null(class_levels) || any(!labels %in% class_levels)) {
        stop("Hard binary predictions must use the declared outcome classes.", call. = FALSE)
      }
      return(factor(labels, levels = class_levels))
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
      if (length(selected) == 1L) {
        probability <- x[[selected]]
        if (!is.numeric(probability)) stop("Probability columns must be numeric.", call. = FALSE)
        return(as.numeric(probability))
      }
      if (length(probability_names) >= 2L) {
        stop(
          "Binary probability output with multiple columns must identify the ",
          "configured positive class `", positive, "` by one column name.",
          call. = FALSE
        )
      }
      if (length(probability_names) == 1L) {
        # A named column for the other event is unambiguous; otherwise the
        # supplied adapter's vector-event contract applies.
        name <- probability_names[[1L]]
        if (!is.null(class_levels) && name %in% class_levels) probability_class <- name
        x <- x[[name]]
      }
    }
    if (!is.numeric(x) || !is.null(dim(x))) {
      stop("Binary probabilities must be a numeric vector or named numeric columns.", call. = FALSE)
    }
    if (!is.null(probability_class) && probability_class != positive) x <- 1 - x
    return(as.numeric(x))
  }

  if (is.numeric(x) && is.null(dim(x)) && !is.null(n) && n == 1L &&
        length(x) == length(class_levels)) {
    columns <- names(x)
    if (is.null(columns) || anyNA(columns) || anyDuplicated(columns)) {
      stop(
        "One-row multiclass probability vectors must name every outcome class.",
        call. = FALSE
      )
    }
    probability <- matrix(
      as.numeric(x),
      nrow = 1L,
      dimnames = list(NULL, columns)
    )
    probability <- align_multiclass_probability_columns(probability, class_levels)
    if (!identical(colnames(probability), class_levels)) {
      stop(
        "One-row multiclass probability vectors must name every outcome class.",
        call. = FALSE
      )
    }
    return(probability)
  }
  if ((is.vector(x) && !is.list(x)) || is.factor(x)) {
    # Hard multiclass predictions remain usable for accuracy, but probability
    # metrics will reject them with a targeted message.
    if (anyNA(x) || any(!as.character(x) %in% class_levels)) {
      stop("Hard multiclass predictions must use observed outcome classes.", call. = FALSE)
    }
    return(factor(x, levels = class_levels))
  }
  if (is.data.frame(x)) {
    # H2O classification output includes a factor hard-label column named
    # `predict`. Remove it before matrix conversion so it cannot coerce valid
    # numeric probability columns to character.
    probability_names <- setdiff(names(x), "predict")
    out <- as.matrix(x[probability_names])
  } else {
    out <- as.matrix(x)
    if ("predict" %in% colnames(out)) {
      out <- out[, colnames(out) != "predict", drop = FALSE]
    }
  }
  if (!is.numeric(out)) stop("Probability columns must be numeric.", call. = FALSE)
  align_multiclass_probability_columns(out, class_levels)
}

align_multiclass_probability_columns <- function(probability, class_levels) {
  columns <- colnames(probability)
  if (is.null(columns) || is.null(class_levels)) return(probability)
  matched <- vapply(class_levels, function(class_level) {
    candidates <- unique(c(
      class_level,
      make.names(class_level),
      paste0("p", class_level),
      make.names(paste0("p", class_level)),
      paste0("prob_", class_level),
      make.names(paste0("prob_", class_level))
    ))
    hits <- intersect(candidates, columns)
    if (length(hits) == 1L) hits else NA_character_
  }, character(1))
  if (anyNA(matched) || anyDuplicated(matched)) return(probability)
  probability <- probability[, matched, drop = FALSE]
  colnames(probability) <- class_levels
  probability
}

validate_predictions <- function(x, n, task, class_levels = NULL) {
  actual_n <- if (is.matrix(x) || is.data.frame(x)) nrow(x) else length(x)
  if (actual_n != n) {
    stop("The prediction function returned ", actual_n, " predictions for ", n,
         " rows.", call. = FALSE)
  }
  if (task %in% c("binary", "multiclass") && is.factor(x)) {
    if (anyNA(x) || any(!as.character(x) %in% class_levels)) {
      stop("Hard classification predictions must use the declared classes.", call. = FALSE)
    }
    return(invisible(TRUE))
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
  valid <- length(x) == 1L && is.numeric(x) && !is.na(x) && is.finite(x)
  valid <- valid && x >= minimum && x <= .Machine$integer.max && x == floor(x)
  if (!valid) {
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
