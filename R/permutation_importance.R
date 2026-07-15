#' Repeated permutation feature importance with uncertainty diagnostics
#'
#' Measures the change in out-of-sample performance after jointly permuting one
#' feature (or a user-defined feature group). Unlike a single permutation bar
#' chart, the result retains every repeat, Monte Carlo uncertainty, sign
#' stability, and the evaluation contract required for an explanation audit.
#'
#' The confidence limits quantify Monte Carlo variation across permutations.
#' They are not confidence intervals for population-level variable importance.
#' Use a genuinely held-out evaluation set and a formal inference method when
#' population claims are required.
#'
#' @param model An `autoxplain_explainer` or a fitted model.
#' @param data Evaluation data. Required for a fitted model; ignored when
#'   `model` is an explainer unless supplied to replace its evaluation data.
#' @param target_column Outcome column name when `model` is a fitted model.
#' @param metric One of `"auto"`, `"rmse"`, `"mae"`, `"logloss"`,
#'   `"accuracy"`, or `"auc"`.
#' @param n_repeats Number of independent permutations.
#' @param seed Random seed. The caller's random-number state is restored.
#' @param features Character vector of features to evaluate. Defaults to all.
#' @param feature_groups Optional named list of character vectors. Features in
#'   each element are permuted together, which is useful for one-hot encodings
#'   and semantically coupled predictors.
#' @param within Optional character vector naming columns that define strata,
#'   or a vector/factor with one stratum per row. Permutations occur within
#'   strata. This is a blocked sensitivity analysis, not a general conditional
#'   permutation importance estimator.
#' @param confidence Confidence level for Monte Carlo intervals.
#' @param predict_function Optional prediction function for a fitted model.
#' @param task Task type passed to [explain_model()].
#' @param positive Positive class passed to [explain_model()].
#' @param n_permutations Deprecated alias for `n_repeats`.
#'
#' @return A data frame of class `autoxplain_importance` with repeat-level
#'   values stored in the `repeat_scores` attribute.
#' @export
#'
#' @examples
#' train <- mtcars[1:24, ]
#' test <- mtcars[25:32, ]
#' fit <- lm(mpg ~ wt + hp + disp, data = train)
#' x <- explain_model(fit, test, y = "mpg")
#' calculate_permutation_importance(x, n_repeats = 10)
calculate_permutation_importance <- function(model,
                                             data = NULL,
                                             target_column = NULL,
                                             metric = "auto",
                                             n_repeats = 20L,
                                             seed = 123L,
                                             features = NULL,
                                             feature_groups = NULL,
                                             within = NULL,
                                             confidence = 0.95,
                                             predict_function = NULL,
                                             task = "auto",
                                             positive = NULL,
                                             n_permutations = NULL) {
  if (!is.null(n_permutations)) {
    warning("`n_permutations` is deprecated; use `n_repeats`.", call. = FALSE)
    n_repeats <- n_permutations
  }
  n_repeats <- assert_count(n_repeats, "n_repeats")
  assert_probability(confidence, "confidence", open = TRUE)

  explainer <- coerce_to_explainer(
    model, data, target_column, predict_function, task, positive
  )
  x <- if (is.null(data) || !inherits(model, "autoxplain_explainer")) {
    explainer$data
  } else {
    replacement_explainer_data(explainer, data, target_column)$data
  }
  y <- if (is.null(data) || !inherits(model, "autoxplain_explainer")) {
    explainer$y
  } else {
    replacement_explainer_data(explainer, data, target_column)$y
  }

  groups <- normalize_feature_groups(features, feature_groups, names(x))
  strata <- normalize_strata(within, x)
  metric <- resolve_metric(metric, explainer$task)
  baseline_prediction <- predict(explainer, x)
  baseline <- metric_score(y, baseline_prediction, metric, explainer)

  repeat_scores <- matrix(
    NA_real_, nrow = length(groups), ncol = n_repeats,
    dimnames = list(names(groups), paste0("repeat_", seq_len(n_repeats)))
  )
  permuted_metric <- repeat_scores

  with_preserved_seed(seed, {
    for (group_index in seq_along(groups)) {
      group <- groups[[group_index]]
      for (repeat_index in seq_len(n_repeats)) {
        permutation <- stratified_permutation(nrow(x), strata)
        permuted <- x
        permuted[group] <- x[permutation, group, drop = FALSE]
        score <- metric_score(y, predict(explainer, permuted), metric, explainer)
        permuted_metric[group_index, repeat_index] <- score
        repeat_scores[group_index, repeat_index] <- importance_delta(
          baseline, score, metric
        )
      }
    }
  })

  alpha <- 1 - confidence
  critical <- if (n_repeats > 1L) {
    stats::qt(1 - alpha / 2, df = n_repeats - 1L)
  } else {
    NA_real_
  }
  importance <- rowMeans(repeat_scores)
  standard_deviation <- apply(repeat_scores, 1L, stats::sd)
  standard_error <- standard_deviation / sqrt(n_repeats)
  lower <- importance - critical * standard_error
  upper <- importance + critical * standard_error
  if (n_repeats == 1L) lower[] <- upper[] <- NA_real_

  out <- data.frame(
    feature = names(groups),
    importance = as.numeric(importance),
    std_error = as.numeric(standard_error),
    conf_low = as.numeric(lower),
    conf_high = as.numeric(upper),
    sign_stability = apply(repeat_scores, 1L, sign_stability),
    baseline = rep(baseline, length(groups)),
    permuted = rowMeans(permuted_metric),
    metric = rep(metric, length(groups)),
    n_repeats = rep(n_repeats, length(groups)),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$importance, decreasing = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  repeat_scores <- repeat_scores[out$feature, , drop = FALSE]
  permuted_metric <- permuted_metric[out$feature, , drop = FALSE]

  class(out) <- c("autoxplain_importance", "data.frame")
  attr(out, "repeat_scores") <- repeat_scores
  attr(out, "permuted_metric") <- permuted_metric
  attr(out, "metric") <- metric
  attr(out, "baseline_score") <- baseline
  attr(out, "n_repeats") <- n_repeats
  attr(out, "confidence") <- confidence
  attr(out, "interval_type") <- "Monte Carlo t interval across permutations"
  attr(out, "feature_groups") <- groups
  attr(out, "blocked_within") <- if (is.null(within)) NULL else within
  attr(out, "explainer_fingerprint") <- explainer$provenance$fingerprint
  out
}

#' @export
print.autoxplain_importance <- function(x, ...) {
  cat("<AutoXplainR permutation importance>\n")
  cat("  metric: ", attr(x, "metric"), " | repeats: ", attr(x, "n_repeats"),
      " | baseline: ", format(attr(x, "baseline_score"), digits = 5), "\n", sep = "")
  print.data.frame(x[c("feature", "importance", "std_error", "conf_low", "conf_high",
                       "sign_stability")], row.names = FALSE, ...)
  cat("  intervals describe permutation Monte Carlo variation, not population inference\n")
  invisible(x)
}

coerce_to_explainer <- function(model,
                                data,
                                target_column,
                                predict_function,
                                task,
                                positive) {
  if (inherits(model, "autoxplain_explainer")) return(model)
  assert_data_frame(data, "data")
  if (is.null(target_column) || !is.character(target_column) || length(target_column) != 1L) {
    stop("`target_column` is required when `model` is not an AutoXplainR explainer.",
         call. = FALSE)
  }
  explain_model(
    model = model,
    data = data,
    y = target_column,
    predict_function = predict_function,
    task = task,
    positive = positive
  )
}

replacement_explainer_data <- function(explainer, data, target_column = NULL) {
  assert_data_frame(data, "data")
  target <- target_column %||% explainer$target
  if (!is.null(target) && target %in% names(data)) {
    y <- data[[target]]
    x <- data[setdiff(names(data), target)]
  } else {
    if (nrow(data) != length(explainer$y)) {
      stop("Replacement `data` without an outcome must have the same number of rows as the explainer.",
           call. = FALSE)
    }
    y <- explainer$y
    x <- data
  }
  missing_features <- setdiff(names(explainer$data), names(x))
  if (length(missing_features)) {
    stop("Replacement `data` is missing: ", paste(missing_features, collapse = ", "),
         call. = FALSE)
  }
  list(data = x[names(explainer$data)], y = y)
}

normalize_feature_groups <- function(features, feature_groups, available) {
  if (!is.null(features)) {
    if (!is.character(features) || !length(features) || anyNA(features)) {
      stop("`features` must be a non-empty character vector.", call. = FALSE)
    }
    missing <- setdiff(features, available)
    if (length(missing)) stop("Unknown features: ", paste(missing, collapse = ", "),
                              call. = FALSE)
  } else {
    features <- available
  }
  if (is.null(feature_groups)) {
    groups <- as.list(features)
    names(groups) <- features
    return(groups)
  }
  if (!is.list(feature_groups) || is.null(names(feature_groups)) ||
        any(names(feature_groups) == "") || anyDuplicated(names(feature_groups))) {
    stop("`feature_groups` must be a uniquely named list.", call. = FALSE)
  }
  valid_group <- vapply(feature_groups, function(z) {
    is.character(z) && length(z) > 0L && !anyNA(z) && !length(setdiff(z, available))
  }, logical(1))
  if (!all(valid_group)) {
    stop("Every feature group must contain one or more known feature names.", call. = FALSE)
  }
  feature_groups
}

normalize_strata <- function(within, data) {
  if (is.null(within)) return(NULL)
  if (is.character(within) && length(within) <= ncol(data) && all(within %in% names(data))) {
    values <- lapply(data[within], function(x) {
      if (is.numeric(x) && length(unique(x)) > 10L) {
        boundaries <- unique(stats::quantile(x, seq(0, 1, length.out = 6L), na.rm = TRUE))
        if (length(boundaries) > 1L) cut(x, boundaries, include.lowest = TRUE) else factor("all")
      } else {
        factor(x, exclude = NULL)
      }
    })
    return(interaction(values, drop = TRUE, lex.order = TRUE))
  }
  if (length(within) != nrow(data)) {
    stop("`within` must name data columns or have one value per row.", call. = FALSE)
  }
  factor(within, exclude = NULL)
}

stratified_permutation <- function(n, strata = NULL) {
  if (is.null(strata)) return(sample.int(n))
  permutation <- seq_len(n)
  split_indices <- split(seq_len(n), strata, drop = TRUE)
  for (indices in split_indices) permutation[indices] <- sample(indices)
  permutation
}

resolve_metric <- function(metric, task) {
  choices <- c("auto", "rmse", "mae", "logloss", "accuracy", "auc")
  if (!is.character(metric) || length(metric) != 1L || !metric %in% choices) {
    stop("`metric` must be one of: ", paste(choices, collapse = ", "), ".",
         call. = FALSE)
  }
  if (metric == "auto") {
    return(switch(task, regression = "rmse", binary = "logloss", multiclass = "logloss"))
  }
  if (task == "regression" && !metric %in% c("rmse", "mae")) {
    stop("Regression supports `rmse` and `mae`.", call. = FALSE)
  }
  if (task == "multiclass" && metric == "auc") {
    stop("`auc` is only available for binary classification.", call. = FALSE)
  }
  metric
}

metric_score <- function(y, prediction, metric, explainer) {
  if (metric == "rmse") return(sqrt(mean((as.numeric(y) - prediction)^2)))
  if (metric == "mae") return(mean(abs(as.numeric(y) - prediction)))
  if (metric == "accuracy") {
    if (explainer$task == "binary") {
      observed <- as.character(y) == explainer$positive
      return(mean(observed == (prediction >= 0.5)))
    }
    if (is.matrix(prediction)) {
      predicted <- colnames(prediction)[max.col(prediction, ties.method = "first")]
    } else {
      predicted <- as.character(prediction)
    }
    return(mean(as.character(y) == predicted))
  }
  if (metric == "auc") {
    observed <- as.character(y) == explainer$positive
    return(binary_auc(observed, prediction))
  }
  epsilon <- 1e-15
  if (explainer$task == "binary") {
    observed <- as.numeric(as.character(y) == explainer$positive)
    probability <- pmin(pmax(prediction, epsilon), 1 - epsilon)
    return(-mean(observed * log(probability) + (1 - observed) * log(1 - probability)))
  }
  if (!is.matrix(prediction)) {
    stop("Multiclass log loss requires class-probability predictions.", call. = FALSE)
  }
  indices <- match(as.character(y), colnames(prediction))
  if (anyNA(indices)) {
    stop("Multiclass prediction columns must be named with every outcome class.", call. = FALSE)
  }
  probability <- prediction[cbind(seq_along(indices), indices)]
  -mean(log(pmax(probability, epsilon)))
}

binary_auc <- function(observed, probability) {
  positives <- sum(observed)
  negatives <- length(observed) - positives
  if (!positives || !negatives) {
    stop("AUC requires both outcome classes in the evaluation data.", call. = FALSE)
  }
  ranks <- rank(probability, ties.method = "average")
  (sum(ranks[observed]) - positives * (positives + 1) / 2) / (positives * negatives)
}

importance_delta <- function(baseline, permuted, metric) {
  if (metric %in% c("accuracy", "auc")) baseline - permuted else permuted - baseline
}

sign_stability <- function(x) {
  if (!length(x)) return(NA_real_)
  max(mean(x >= 0), mean(x <= 0))
}

# Retained for backward compatibility with code using this internal helper.
calculate_metric_score <- function(model, h2o_data, target_column, metric) {
  if (!inherits(model, "H2OModel")) {
    stop("`calculate_metric_score()` is an internal H2O compatibility helper.", call. = FALSE)
  }
  require_optional("h2o", "scoring an H2O model")
  frame <- as.data.frame(h2o_data)
  explainer <- explain_model(model, frame, y = target_column)
  metric_score(explainer$y, predict(explainer, explainer$data),
               resolve_metric(metric, explainer$task), explainer)
}
