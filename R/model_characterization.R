#' Extract comparable model metadata
#'
#' Extracts stable, compact metadata from guided base models or retained H2O
#' models and joins it to the leaderboard. Native H2O variable importance is
#' labeled as such; it is not substituted for model-agnostic permutation
#' importance.
#'
#' @param autoxplain_result An `autoxplain_result`.
#' @param include_hyperparams Include a compact set of configured parameters.
#' @param include_performance Include numeric leaderboard metrics.
#' @param include_varimp Include native H2O importance when available.
#'
#' @return A list of class `autoxplainr_model_characteristics`.
#' @export
extract_model_characteristics <- function(autoxplain_result,
                                          include_hyperparams = TRUE,
                                          include_performance = TRUE,
                                          include_varimp = TRUE) {
  if (!inherits(autoxplain_result, "autoxplain_result")) {
    stop("`autoxplain_result` must be returned by `autoxplain()`.", call. = FALSE)
  }
  switches <- c(include_hyperparams, include_performance, include_varimp)
  if (anyNA(switches) || !all(vapply(as.list(switches), is.logical, logical(1)))) {
    stop("Inclusion switches must be TRUE or FALSE.", call. = FALSE)
  }
  leaderboard <- as.data.frame(autoxplain_result$leaderboard)
  models <- autoxplain_result$models
  output <- lapply(seq_along(models), function(index) {
    model <- models[[index]]
    id <- names(models)[[index]]
    h2o_model <- inherits(model, "H2OModel")
    algorithm <- if (h2o_model) {
      tryCatch(as.character(model@algorithm), error = function(error) extract_model_type(id))
    } else {
      friendly_model_type(model)
    }
    row <- leaderboard[match(id, leaderboard$model_id), , drop = FALSE]
    numeric_metrics <- if (nrow(row)) {
      row[vapply(row, is.numeric, logical(1))]
    } else {
      data.frame()
    }
    info <- list(
      rank = index,
      model_id = id,
      algorithm = algorithm,
      training_time_s = if (h2o_model) {
        tryCatch(as.numeric(model@model$run_time) / 1000, error = function(error) NA_real_)
      } else {
        NA_real_
      },
      size_bytes = if (h2o_model) {
        tryCatch(as.numeric(model@model$output$model_size_in_bytes),
                 error = function(error) NA_real_)
      } else {
        as.numeric(utils::object.size(model))
      }
    )
    info$size_mb <- info$size_bytes / 1024^2
    if (include_performance) {
      info$performance_metrics <- if (ncol(numeric_metrics)) {
        as.list(numeric_metrics[1L, , drop = TRUE])
      } else {
        list()
      }
    }
    if (include_hyperparams) {
      info$hyperparameters <- if (h2o_model) {
        parameters <- tryCatch(model@allparameters, error = function(error) list())
        compact_hyperparameters(parameters, algorithm)
      } else {
        base_model_hyperparameters(model)
      }
    }
    if (include_varimp) {
      info$native_variable_importance <- if (h2o_model) {
        tryCatch({
          require_optional("h2o", "extracting native H2O variable importance")
          value <- h2o::h2o.varimp(model, use_pandas = FALSE)
          if (is.null(value)) NULL else head(as.data.frame(value), 10L)
        }, error = function(error) NULL)
      } else {
        NULL
      }
    }
    info
  })
  names(output) <- names(models)
  total_time <- sum(vapply(output, function(x) x$training_time_s, numeric(1)), na.rm = TRUE)
  attr(output, "summary") <- list(
    total_models = length(output),
    algorithms_used = unique(vapply(output, `[[`, character(1), "algorithm")),
    total_training_time_s = total_time,
    dataset_info = list(
      target_column = autoxplain_result$target_column,
      n_rows = nrow(autoxplain_result$training_data),
      n_features = length(autoxplain_result$features),
      evaluation_role = autoxplain_result$provenance$evaluation_role %||% "unspecified"
    )
  )
  class(output) <- c("autoxplainr_model_characteristics", "list")
  output
}

friendly_model_type <- function(model) {
  if (inherits(model, "glm") && !is.null(model$family) && model$family$family == "binomial") {
    return("logistic regression")
  }
  if (inherits(model, "lm")) return("linear regression")
  if (inherits(model, "multinom")) return("multinomial logistic regression")
  paste(class(model), collapse = "/")
}

base_model_hyperparameters <- function(model) {
  formula <- tryCatch(paste(deparse(stats::formula(model)), collapse = " "),
                      error = function(error) NA_character_)
  output <- list(formula = formula)
  if (inherits(model, "glm")) output$family <- model$family$family
  if (inherits(model, "multinom")) output$decay <- model$decay %||% 0
  output
}

#' @export
print.autoxplainr_model_characteristics <- function(x, ...) {
  summary <- attr(x, "summary")
  cat("<AutoXplainR model characteristics>\n")
  cat("  models:   ", summary$total_models, "\n", sep = "")
  cat("  families: ", paste(summary$algorithms_used, collapse = ", "), "\n", sep = "")
  cat("  target:   ", summary$dataset_info$target_column, "\n", sep = "")
  table <- model_characteristics_table(x)
  print.data.frame(table, row.names = FALSE, ...)
  invisible(x)
}

#' @export
summary.autoxplainr_model_characteristics <- function(object, ...) {
  list(summary = attr(object, "summary"), models = model_characteristics_table(object))
}

#' Calculate a relative weighted model score
#'
#' Min-max combines predictive performance and training time. Because results
#' depend on the candidate set and the subjective weight, use this only as a
#' sensitivity analysis; AutoXplainR reports the underlying dimensions
#' separately in its primary workflow.
#'
#' @param performance_scores Numeric model scores.
#' @param training_times Numeric training times in seconds.
#' @param performance_weight Weight on predictive performance.
#' @param higher_is_better Whether larger performance is better.
#'
#' @return Numeric relative scores from zero to one.
#' @export
calculate_weighted_efficiency <- function(performance_scores,
                                          training_times,
                                          performance_weight = 0.7,
                                          higher_is_better = TRUE) {
  if (!is.numeric(performance_scores) || !is.numeric(training_times) ||
        length(performance_scores) != length(training_times) || length(performance_scores) < 2L) {
    stop("Scores and times must be numeric vectors of equal length with at least two models.",
         call. = FALSE)
  }
  if (any(!is.finite(performance_scores)) || any(!is.finite(training_times)) ||
        any(training_times < 0)) {
    stop("Scores and times must be finite, and times cannot be negative.", call. = FALSE)
  }
  assert_probability(performance_weight, "performance_weight")
  if (!is.logical(higher_is_better) || length(higher_is_better) != 1L || is.na(higher_is_better)) {
    stop("`higher_is_better` must be TRUE or FALSE.", call. = FALSE)
  }
  warning(
    "Weighted efficiency is candidate-set-relative and weight-dependent; report both raw dimensions.",
    call. = FALSE
  )
  performance <- minmax(performance_scores)
  if (!higher_is_better) performance <- 1 - performance
  speed <- 1 - minmax(training_times)
  performance_weight * performance + (1 - performance_weight) * speed
}

#' Create a compact model metadata report
#'
#' @param model_characteristics Output from [extract_model_characteristics()].
#' @param output_file Destination HTML file.
#' @param include_plots Retained for compatibility and ignored.
#'
#' @return The normalized output path, invisibly.
#' @export
create_model_comparison_report <- function(model_characteristics,
                                           output_file = "model-comparison.html",
                                           include_plots = FALSE) {
  if (!inherits(model_characteristics, "autoxplainr_model_characteristics")) {
    stop("`model_characteristics` must be returned by `extract_model_characteristics()`.",
         call. = FALSE)
  }
  if (!is.character(output_file) || length(output_file) != 1L ||
        tolower(tools::file_ext(output_file)) != "html") {
    stop("`output_file` must be a single .html path.", call. = FALSE)
  }
  summary <- attr(model_characteristics, "summary")
  table <- model_characteristics_table(model_characteristics)
  html <- paste0(
    "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">",
    "<title>AutoXplainR model metadata</title><style>", report_css(), "</style></head><body>",
    "<header><div class=\"shell\"><p class=\"eyebrow\">AutoXplainR</p>",
    "<h1>Model metadata</h1><p class=\"lede\">Training characteristics and leaderboard metrics. ",
    "Model selection should use held-out performance and the separate explanation evidence audit.</p>",
    "</div></header><main class=\"shell\"><section><h2>Run summary</h2>",
    definition_list(c(
      Target = summary$dataset_info$target_column,
      Models = as.character(summary$total_models),
      Features = as.character(summary$dataset_info$n_features),
      `Evaluation role` = summary$dataset_info$evaluation_role,
      `Total model runtime (s)` = report_number(summary$total_training_time_s, 2L)
    )), "</section><section><h2>Retained models</h2>", html_table(table, digits = 4L),
    "</section></main></body></html>"
  )
  directory <- dirname(output_file)
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  writeLines(html, output_file, useBytes = TRUE)
  invisible(normalizePath(output_file, mustWork = TRUE))
}

compact_hyperparameters <- function(parameters, algorithm) {
  preferred <- switch(
    tolower(algorithm),
    glm = c("family", "alpha", "lambda", "solver", "standardize"),
    gbm = c("ntrees", "max_depth", "learn_rate", "sample_rate", "min_rows"),
    drf = c("ntrees", "max_depth", "sample_rate", "mtries", "min_rows"),
    xgboost = c("ntrees", "max_depth", "learn_rate", "sample_rate", "reg_alpha", "reg_lambda"),
    deeplearning = c("hidden", "epochs", "activation", "input_dropout_ratio", "l1", "l2"),
    stackedensemble = c("metalearner_algorithm"),
    names(parameters)
  )
  selected <- intersect(preferred, names(parameters))
  values <- lapply(parameters[selected], function(value) {
    if (is.atomic(value) && length(value) <= 8L) value else paste0("<", length(value), " values>")
  })
  values
}

model_characteristics_table <- function(x) {
  data.frame(
    rank = vapply(x, `[[`, integer(1), "rank"),
    model_id = vapply(x, `[[`, character(1), "model_id"),
    algorithm = vapply(x, `[[`, character(1), "algorithm"),
    training_time_s = vapply(x, `[[`, numeric(1), "training_time_s"),
    size_mb = vapply(x, `[[`, numeric(1), "size_mb"),
    stringsAsFactors = FALSE
  )
}

minmax <- function(x) {
  range <- range(x)
  if (range[[1L]] == range[[2L]]) return(rep(0.5, length(x)))
  (x - range[[1L]]) / diff(range)
}
