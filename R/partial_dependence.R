#' Estimate a model feature effect
#'
#' Computes either accumulated local effects (ALE) or partial dependence (PDP).
#' ALE is the default because it avoids the most direct extrapolation problem of
#' marginal PDPs when predictors are correlated. PDP output includes local
#' support and dependence diagnostics so unsupported curves are not presented as
#' unqualified facts.
#'
#' @param model An `autoxplain_explainer` or fitted model.
#' @param data Reference data for a fitted model. For an explainer, defaults to
#'   its stored feature data.
#' @param feature A single feature name.
#' @param method `"ale"` or `"pdp"`.
#' @param n_points Number of quantile bins for ALE or grid points for PDP.
#' @param quantile_range Numeric length-two range used for a numeric PDP grid.
#' @param sample_size Maximum number of reference rows used by PDP. `NULL` uses
#'   all rows. ALE always uses all rows that fall inside its bins.
#' @param seed Sampling seed; the caller's random-number state is restored.
#' @param predict_function Optional prediction function for a fitted model.
#' @param task Prediction task for a fitted model.
#' @param positive Positive class for binary classification.
#' @param class For multiclass predictions, the class whose probability is
#'   explained. By default the first prediction column is used.
#' @param grid_size Deprecated alias for `n_points`.
#' @param return_all_classes Retained for compatibility. Multiclass callers
#'   should make separate class-specific explainers; `TRUE` is not supported by
#'   ALE.
#'
#' @return A data frame of class `autoxplain_effect`. Both methods return the
#'   effect estimate, relative empirical support, a descriptive standard error,
#'   and normal-approximation limits. PDP limits summarize across-row variation
#'   in fixed-model predictions at each grid value. ALE limits propagate
#'   within-bin variation in fixed-model local prediction differences and are
#'   unavailable when any bin has fewer than two rows. Neither is a
#'   model-fitting or population confidence interval.
#' @export
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp + disp, data = mtcars)
#' x <- explain_model(fit, mtcars, y = "mpg")
#' explain_effect(x, feature = "wt")
explain_effect <- function(model,
                           data = NULL,
                           feature = NULL,
                           method = c("ale", "pdp"),
                           n_points = 20L,
                           quantile_range = c(0.05, 0.95),
                           sample_size = 1000L,
                           seed = 123L,
                           predict_function = NULL,
                           task = "auto",
                           positive = NULL,
                           class = NULL,
                           grid_size = NULL,
                           return_all_classes = FALSE) {
  method <- match.arg(method)
  if (!is.null(grid_size)) {
    warning("`grid_size` is deprecated; use `n_points`.", call. = FALSE)
    n_points <- grid_size
  }
  n_points <- assert_count(n_points, "n_points", minimum = 2L)
  validate_quantile_range(quantile_range)
  if (!is.null(sample_size)) sample_size <- assert_count(sample_size, "sample_size", 2L)

  input <- effect_input(
    model, data, feature, predict_function, task, positive, class
  )
  if (isTRUE(return_all_classes) && method == "ale") {
    stop("ALE supports one prediction target at a time; set `class` explicitly.",
         call. = FALSE)
  }
  effect <- if (method == "ale") {
    calculate_ale_impl(input$predict, input$data, input$feature, n_points)
  } else {
    calculate_pdp_impl(
      input$predict, input$data, input$feature, n_points, quantile_range,
      sample_size, seed
    )
  }
  attr(effect, "task") <- input$task
  attr(effect, "prediction_target") <- input$prediction_target
  attr(effect, "prediction_class") <- input$prediction_class
  effect
}

#' Calculate partial dependence data
#'
#' Backward-compatible PDP entry point. New code should generally call
#' [explain_effect()] and keep its default ALE method.
#'
#' @inheritParams explain_effect
#' @return A data frame of class `autoxplain_effect`.
#' @export
calculate_partial_dependence <- function(model,
                                         data = NULL,
                                         feature = NULL,
                                         n_points = 50L,
                                         quantile_range = c(0.05, 0.95),
                                         return_all_classes = FALSE,
                                         grid_size = NULL,
                                         sample_size = 1000L,
                                         seed = 123L,
                                         predict_function = NULL,
                                         task = "auto",
                                         positive = NULL,
                                         class = NULL) {
  explain_effect(
    model = model,
    data = data,
    feature = feature,
    method = "pdp",
    n_points = n_points,
    quantile_range = quantile_range,
    return_all_classes = return_all_classes,
    grid_size = grid_size,
    sample_size = sample_size,
    seed = seed,
    predict_function = predict_function,
    task = task,
    positive = positive,
    class = class
  )
}

#' Calculate accumulated local effects data
#'
#' @inheritParams explain_effect
#' @return A data frame of class `autoxplain_effect`.
#' @export
calculate_accumulated_local_effects <- function(model,
                                                data = NULL,
                                                feature = NULL,
                                                n_points = 20L,
                                                predict_function = NULL,
                                                task = "auto",
                                                positive = NULL,
                                                class = NULL) {
  explain_effect(
    model = model,
    data = data,
    feature = feature,
    method = "ale",
    n_points = n_points,
    predict_function = predict_function,
    task = task,
    positive = positive,
    class = class
  )
}

#' Calculate PDPs for multiple features
#'
#' @param features Non-empty character vector of feature names.
#' @inheritParams calculate_partial_dependence
#' @return A named list of effect data frames.
#' @export
calculate_partial_dependence_multi <- function(model,
                                               data = NULL,
                                               features,
                                               n_points = 50L,
                                               quantile_range = c(0.05, 0.95),
                                               return_all_classes = FALSE,
                                               grid_size = NULL,
                                               sample_size = 1000L,
                                               seed = 123L,
                                               predict_function = NULL,
                                               task = "auto",
                                               positive = NULL,
                                               class = NULL) {
  if (!is.character(features) || !length(features) || anyNA(features)) {
    stop("`features` must be a non-empty character vector.", call. = FALSE)
  }
  reference <- if (inherits(model, "autoxplain_explainer") && is.null(data)) model$data else data
  assert_data_frame(reference, "data")
  missing <- setdiff(features, names(reference))
  if (length(missing)) stop("Unknown features: ", paste(missing, collapse = ", "),
                            call. = FALSE)
  results <- lapply(seq_along(features), function(index) {
    calculate_partial_dependence(
      model = model,
      data = data,
      feature = features[[index]],
      n_points = n_points,
      quantile_range = quantile_range,
      return_all_classes = return_all_classes,
      grid_size = grid_size,
      sample_size = sample_size,
      seed = seed + index - 1L,
      predict_function = predict_function,
      task = task,
      positive = positive,
      class = class
    )
  })
  names(results) <- features
  results
}

#' @export
print.autoxplain_effect <- function(x, ...) {
  cat("<AutoXplainR ", toupper(attr(x, "method")), " effect>\n", sep = "")
  cat("  feature: ", attr(x, "feature"), " | rows: ", attr(x, "n_reference"),
      " | max association: ", format(attr(x, "max_association"), digits = 3), "\n",
      sep = "")
  if (!is.null(attr(x, "prediction_target"))) {
    cat("  target:  ", attr(x, "prediction_target"), "\n", sep = "")
  }
  if (isTRUE(attr(x, "dependence_warning"))) {
    cat("  caution: strong feature dependence can make a marginal PDP misleading\n")
  }
  if (!is.null(attr(x, "interval_note"))) {
    cat("  bands:   ", attr(x, "interval_note"), "\n", sep = "")
  }
  print.data.frame(x, row.names = FALSE, ...)
  invisible(x)
}

effect_input <- function(model,
                         data,
                         feature,
                         predict_function,
                         task,
                         positive,
                         class) {
  if (inherits(model, "autoxplain_explainer")) {
    if (is.character(data) && length(data) == 1L && is.null(feature)) {
      feature <- data
      data <- NULL
    }
    reference <- data %||% model$data
    assert_data_frame(reference, "data")
    missing <- setdiff(names(model$data), names(reference))
    if (length(missing)) stop("`data` is missing: ", paste(missing, collapse = ", "),
                              call. = FALSE)
    prediction_function <- function(newdata) predict(model, newdata)
    resolved_task <- model$task
    prediction_class <- if (identical(resolved_task, "multiclass")) {
      class %||% model$class_levels[[1L]]
    } else if (identical(resolved_task, "binary")) {
      model$positive
    } else {
      NA_character_
    }
  } else {
    assert_data_frame(data, "data")
    reference <- data
    resolved_task <- if (task == "auto") infer_task_from_model(model) else task
    class_levels <- infer_model_class_levels(model, resolved_task)
    if (identical(resolved_task, "binary")) {
      positive <- resolve_effect_class(
        positive,
        class_levels,
        argument = "positive",
        task = resolved_task,
        default_index = 2L
      )
    }
    selected_class <- if (identical(resolved_task, "multiclass")) {
      resolve_effect_class(
        class,
        class_levels,
        argument = "class",
        task = resolved_task,
        default_index = 1L
      )
    } else {
      NULL
    }
    adapter <- make_prediction_adapter(
      model,
      resolved_task,
      positive,
      class_levels,
      predict_function
    )
    prediction_function <- adapter
    prediction_class <- if (identical(resolved_task, "multiclass")) {
      selected_class %||% "first probability column (class name unavailable)"
    } else if (identical(resolved_task, "binary")) {
      positive %||% "positive class (name unavailable)"
    } else {
      NA_character_
    }
  }
  if (!is.character(feature) || length(feature) != 1L || is.na(feature) ||
        !feature %in% names(reference)) {
    stop("`feature` must name one column in the reference data.", call. = FALSE)
  }
  selected_target <- if (identical(resolved_task, "multiclass")) {
    prediction_class
  } else {
    NULL
  }
  prediction_function <- select_prediction_target(prediction_function, selected_target)
  prediction_target <- switch(
    resolved_task,
    regression = "predicted value",
    binary = paste0("probability for positive class `", prediction_class, "`"),
    multiclass = paste0("probability for class `", prediction_class, "`")
  )
  list(
    data = reference,
    feature = feature,
    predict = prediction_function,
    task = resolved_task,
    prediction_target = prediction_target,
    prediction_class = prediction_class
  )
}

infer_model_class_levels <- function(model, task = infer_task_from_model(model)) {
  if (!task %in% c("binary", "multiclass")) return(NULL)
  levels <- if (inherits(model, "autoxplain_fitted_model") ||
                  inherits(model, "autoxplain_tuned_nnet")) {
    model$class_levels
  } else if (inherits(model, "multinom")) {
    model$lev
  } else if (inherits(model, "rpart")) {
    attr(model, "ylevels")
  } else if (inherits(model, "glm") && !is.null(model$family) &&
               identical(model$family$family, "binomial")) {
    response <- tryCatch(
      stats::model.response(stats::model.frame(model)),
      error = function(error) NULL
    )
    if (is.factor(response)) {
      levels(response)
    } else if (is.atomic(response) && is.null(dim(response)) &&
                 length(unique(response)) == 2L) {
      sort(unique(as.character(response)))
    } else {
      NULL
    }
  } else {
    NULL
  }
  levels <- as.character(levels %||% character())
  levels <- levels[!is.na(levels) & nzchar(levels)]
  valid_length <- if (identical(task, "binary")) {
    length(levels) == 2L
  } else {
    length(levels) >= 2L
  }
  if (!valid_length || anyDuplicated(levels)) NULL else levels
}

resolve_effect_class <- function(value,
                                 class_levels,
                                 argument,
                                 task,
                                 default_index) {
  if (!is.null(value) && (!is.character(value) || length(value) != 1L ||
                            is.na(value) || !nzchar(value))) {
    stop("`", argument, "` must be one non-empty class name.", call. = FALSE)
  }
  if (!length(class_levels)) return(value)
  value <- value %||% class_levels[[default_index]]
  if (!value %in% class_levels) {
    stop(
      "`", argument, "` must name one ", task, " outcome level: ",
      paste(class_levels, collapse = ", "), ".",
      call. = FALSE
    )
  }
  value
}

infer_task_from_model <- function(model) {
  if (inherits(model, "autoxplain_fitted_model") ||
        inherits(model, "autoxplain_tuned_nnet")) {
    return(model$task)
  }
  if (inherits(model, "glm") && !is.null(model$family) && model$family$family == "binomial") {
    return("binary")
  }
  if (inherits(model, "multinom")) {
    return(if (length(model$lev) == 2L) "binary" else "multiclass")
  }
  if (inherits(model, "rpart") && identical(model$method, "class")) {
    levels <- attr(model, "ylevels")
    return(if (length(levels) == 2L) "binary" else "multiclass")
  }
  if (inherits(model, "H2OBinomialModel")) return("binary")
  if (inherits(model, "H2OMultinomialModel")) return("multiclass")
  "regression"
}

select_prediction_target <- function(fun, class) {
  # Force promises before the caller rebinds its local prediction function;
  # otherwise R's lazy evaluation can make `fun` resolve to this wrapper.
  force(fun)
  force(class)
  function(newdata) {
    prediction <- fun(newdata)
    if (is.matrix(prediction) || is.data.frame(prediction)) {
      prediction <- as.matrix(prediction)
      selected <- class %||% colnames(prediction)[[1L]]
      if (is.null(selected) || !selected %in% colnames(prediction)) {
        stop("`class` must name a column in multiclass probability predictions.",
             call. = FALSE)
      }
      prediction <- prediction[, selected]
    }
    if (!is.numeric(prediction)) {
      stop("Feature effects require numeric predictions or class probabilities.",
           call. = FALSE)
    }
    as.numeric(prediction)
  }
}

calculate_pdp_impl <- function(predict_function,
                               data,
                               feature,
                               n_points,
                               quantile_range,
                               sample_size,
                               seed) {
  grid <- get_feature_grid(data[[feature]], n_points, quantile_range)
  reference <- data
  reference_rows <- seq_len(nrow(data))
  if (!is.null(sample_size) && nrow(reference) > sample_size) {
    indices <- with_preserved_seed(seed, sample.int(nrow(reference), sample_size))
    reference <- reference[indices, , drop = FALSE]
    reference_rows <- reference_rows[indices]
  }

  values <- numeric(length(grid))
  std_error <- numeric(length(grid))
  for (index in seq_along(grid)) {
    modified <- reference
    modified[[feature]] <- coerce_grid_value(grid[[index]], data[[feature]])
    prediction <- predict_function(modified)
    values[[index]] <- mean(prediction)
    std_error[[index]] <- stats::sd(prediction) / sqrt(length(prediction))
  }
  support <- grid_support(data[[feature]], grid)
  result <- data.frame(
    feature_value = grid,
    partial_dependence = values,
    std_error = std_error,
    conf_low = values - stats::qnorm(0.975) * std_error,
    conf_high = values + stats::qnorm(0.975) * std_error,
    support = support,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(result)[[1L]] <- feature
  out <- finalize_effect(result, data, feature, "pdp", nrow(reference))
  attr(out, "interval_note") <- paste(
    "Descriptive fixed-model bands from across-row prediction variation at each",
    "grid value; not model-fitting uncertainty, population confidence, or causal intervals."
  )
  attr(out, "reference_rows") <- reference_rows
  out
}

calculate_ale_impl <- function(predict_function, data, feature, n_points) {
  values <- data[[feature]]
  reference_rows <- seq_len(nrow(data))
  if (!is.numeric(values)) {
    stop("ALE currently supports numeric features. Use `method = \"pdp\"` for categorical features.",
         call. = FALSE)
  }
  if (anyNA(values)) {
    keep <- !is.na(values)
    data <- data[keep, , drop = FALSE]
    values <- values[keep]
    reference_rows <- reference_rows[keep]
  }
  boundaries <- unique(as.numeric(stats::quantile(
    values, probs = seq(0, 1, length.out = n_points + 1L), na.rm = TRUE,
    names = FALSE, type = 7
  )))
  if (length(boundaries) < 2L) {
    stop("ALE requires at least two distinct feature values.", call. = FALSE)
  }
  bins <- findInterval(values, boundaries, all.inside = TRUE)
  bins[bins == length(boundaries)] <- length(boundaries) - 1L
  n_bins <- length(boundaries) - 1L

  lower_data <- data
  upper_data <- data
  lower_data[[feature]] <- boundaries[bins]
  upper_data[[feature]] <- boundaries[bins + 1L]
  local_effect <- predict_function(upper_data) - predict_function(lower_data)

  bin_effect <- vapply(seq_len(n_bins), function(index) {
    selected <- bins == index
    if (any(selected)) mean(local_effect[selected]) else 0
  }, numeric(1))
  bin_n <- tabulate(bins, nbins = n_bins)
  bin_se <- vapply(seq_len(n_bins), function(index) {
    selected <- bins == index
    if (sum(selected) > 1L) stats::sd(local_effect[selected]) / sqrt(sum(selected)) else NA_real_
  }, numeric(1))
  accumulated <- cumsum(bin_effect)
  weights <- bin_n / sum(bin_n)
  centered <- accumulated - sum(accumulated * weights)
  cumulative_se <- ale_centered_standard_error(bin_se, weights)

  result <- data.frame(
    feature_value = (boundaries[-1L] + boundaries[-length(boundaries)]) / 2,
    accumulated_effect = centered,
    std_error = cumulative_se,
    conf_low = centered - stats::qnorm(0.975) * cumulative_se,
    conf_high = centered + stats::qnorm(0.975) * cumulative_se,
    n = bin_n,
    support = bin_n / max(bin_n),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(result)[[1L]] <- feature
  out <- finalize_effect(result, data, feature, "ale", nrow(data))
  attr(out, "interval_note") <- paste(
    "Descriptive fixed-model bands propagated from within-bin variation in local",
    "prediction differences under an independent-bin approximation; unavailable if",
    "a bin has fewer than two rows and not model-fitting uncertainty, population",
    "confidence, or causal intervals."
  )
  attr(out, "reference_rows") <- reference_rows
  out
}

ale_centered_standard_error <- function(bin_se, weights) {
  n_bins <- length(bin_se)
  if (!n_bins || length(weights) != n_bins || any(!is.finite(bin_se))) {
    return(rep(NA_real_, n_bins))
  }
  accumulation <- lower.tri(matrix(0, n_bins, n_bins), diag = TRUE) * 1
  centering <- diag(n_bins) - outer(rep(1, n_bins), weights)
  coefficients <- centering %*% accumulation
  sqrt(rowSums(sweep(coefficients, 2L, bin_se, "*")^2))
}

finalize_effect <- function(result, data, feature, method, n_reference) {
  associations <- feature_associations(data, feature)
  max_association <- if (length(associations)) max(associations, na.rm = TRUE) else 0
  if (!is.finite(max_association)) max_association <- 0
  class(result) <- c("autoxplain_effect", "data.frame")
  attr(result, "method") <- method
  attr(result, "feature") <- feature
  attr(result, "n_reference") <- n_reference
  attr(result, "max_association") <- max_association
  attr(result, "associated_feature") <- if (length(associations)) {
    names(associations)[which.max(associations)]
  } else {
    NA_character_
  }
  attr(result, "dependence_warning") <- method == "pdp" && max_association >= 0.7
  attr(result, "support_note") <- "Support is relative local empirical density on a 0-1 scale."
  result
}

validate_quantile_range <- function(x) {
  if (!is.numeric(x) || length(x) != 2L || anyNA(x) || x[[1L]] < 0 ||
        x[[2L]] > 1 || x[[1L]] >= x[[2L]]) {
    stop("`quantile_range` must contain two increasing probabilities from 0 to 1.",
         call. = FALSE)
  }
  invisible(TRUE)
}

get_feature_grid <- function(feature_data, n_points, quantile_range = c(0.05, 0.95)) {
  validate_quantile_range(quantile_range)
  n_points <- assert_count(n_points, "n_points", 2L)
  if (is.numeric(feature_data)) {
    limits <- stats::quantile(feature_data, quantile_range, na.rm = TRUE, names = FALSE)
    if (!all(is.finite(limits))) stop("The feature has no finite values.", call. = FALSE)
    if (limits[[1L]] == limits[[2L]]) return(unique(feature_data[!is.na(feature_data)]))
    return(seq(limits[[1L]], limits[[2L]], length.out = n_points))
  }
  if (is.factor(feature_data) || is.character(feature_data) || is.logical(feature_data)) {
    counts <- sort(table(feature_data, useNA = "no"), decreasing = TRUE)
    return(names(counts)[seq_len(min(length(counts), n_points))])
  }
  stop("Unsupported feature type: ", paste(class(feature_data), collapse = "/"),
       call. = FALSE)
}

coerce_grid_value <- function(value, prototype) {
  if (is.factor(prototype)) return(factor(value, levels = levels(prototype), ordered = is.ordered(prototype)))
  if (is.logical(prototype)) return(as.logical(value))
  value
}

grid_support <- function(observed, grid) {
  observed <- observed[!is.na(observed)]
  if (is.numeric(observed)) {
    if (length(grid) == 1L) return(1)
    width <- stats::median(diff(sort(unique(grid))))
    if (!is.finite(width) || width <= 0) width <- stats::sd(observed) / 5
    counts <- vapply(grid, function(value) mean(abs(observed - value) <= width / 2), numeric(1))
  } else {
    frequency <- table(as.character(observed)) / length(observed)
    counts <- as.numeric(frequency[as.character(grid)])
    counts[is.na(counts)] <- 0
  }
  maximum <- max(counts)
  if (!is.finite(maximum) || maximum == 0) rep(0, length(grid)) else counts / maximum
}
