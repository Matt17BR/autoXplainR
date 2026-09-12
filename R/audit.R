#' Stress-test the evidence behind model explanations
#'
#' `audit_explanations()` is AutoXplainR's advanced reliability layer. It evaluates
#' repeated permutation importance, feature dependence, Monte Carlo stability,
#' prediction disagreement, and explanation disagreement among near-equivalent
#' models. The output keeps shuffle variation, a limited pairwise association
#' screen, evaluation scope, and candidate disagreement separate. It does not
#' combine these questions into an evidence grade.
#'
#' This is a diagnostic protocol, not a formal certification or a substitute
#' for domain review, causal identification, or external validation.
#'
#' @param explainers An `autoxplain_explainer` or a list of explainers.
#' @param features Features shared by every explainer. Defaults to their
#'   intersection.
#' @param metric Performance metric passed to
#'   [calculate_permutation_importance()]. With `"auto"`, explainers carrying
#'   an `autoxplain_result` primary metric use that same metric for both
#'   performance screening and permutation importance.
#' @param n_repeats Number of permutations per model and feature.
#' @param seed Reproducible seed.
#' @param confidence Monte Carlo interval level.
#' @param performance_tolerance Relative tolerance defining the empirical set
#'   of near-optimal supplied models. For example, `0.05` retains models whose
#'   evaluation score is within five percent of the best supplied score.
#' @param dependence_threshold Pairwise association above which marginal
#'   importance receives a warning. Numeric pairs use absolute Spearman
#'   correlation, mixed pairs use a correlation ratio, and categorical pairs
#'   use Cramer's V. Small values do not establish independence or rule out
#'   nonlinear or joint dependence.
#' @param max_rows Maximum evaluation rows for permutation importance and
#'   feature-dependence checks. `NULL` uses all rows. Model performance and
#'   prediction comparisons still use the complete evaluation set. Sampling
#'   is uniform without replacement and shared across models; its uncertainty
#'   is not included in shuffle intervals.
#'
#' @return An object of class `autoxplain_audit`.
#' @export
#'
#' @examples
#' train <- mtcars[1:24, ]
#' test <- mtcars[25:32, ]
#' lm1 <- lm(mpg ~ wt + hp + disp, train)
#' lm2 <- lm(mpg ~ wt + hp + qsec, train)
#' e1 <- explain_model(lm1, test, "mpg", label = "model A")
#' e2 <- explain_model(lm2, test, "mpg", label = "model B")
#' audit <- audit_explanations(list(e1, e2), n_repeats = 5)
#' audit
audit_explanations <- function(explainers,
                               features = NULL,
                               metric = "auto",
                               n_repeats = 20L,
                               seed = 123L,
                               confidence = 0.95,
                               performance_tolerance = 0.05,
                               dependence_threshold = 0.7,
                               max_rows = NULL) {
  explainers <- normalize_explainers(explainers)
  n_repeats <- assert_count(n_repeats, "n_repeats")
  assert_probability(confidence, "confidence", open = TRUE)
  assert_probability(performance_tolerance, "performance_tolerance")
  assert_probability(dependence_threshold, "dependence_threshold", open = TRUE)

  assert_common_evaluation(explainers)
  available <- Reduce(intersect, lapply(explainers, function(x) names(x$data)))
  features <- features %||% available
  if (!is.character(features) || !length(features) || anyNA(features)) {
    stop("`features` must be a non-empty character vector.", call. = FALSE)
  }
  missing <- setdiff(features, available)
  if (length(missing)) {
    stop("Features are not available in every explainer: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  resolved_metrics <- vapply(explainers, function(explainer) {
    resolve_metric(
      metric,
      explainer$task,
      primary_metric = explainer$metadata$primary_metric %||% NULL
    )
  }, character(1))
  if (length(unique(resolved_metrics)) != 1L) {
    stop(
      "All explainers in an audit must resolve to the same performance metric. ",
      "Supply `metric` explicitly when their primary metrics differ.",
      call. = FALSE
    )
  }
  resolved_metric <- resolved_metrics[[1L]]

  importance_objects <- vector("list", length(explainers))
  for (index in seq_along(explainers)) {
    importance_objects[[index]] <- calculate_permutation_importance(
      explainers[[index]],
      metric = resolved_metric,
      n_repeats = n_repeats,
      seed = seed + index - 1L,
      features = features,
      confidence = confidence,
      max_rows = max_rows, sample_seed = seed
    )
  }
  names(importance_objects) <- names(explainers)

  performance <- data.frame(
    model = names(explainers),
    score = vapply(importance_objects, function(x) {
      attr(x, "full_baseline_score") %||% attr(x, "baseline_score")
    }, numeric(1)),
    metric = resolved_metric,
    stringsAsFactors = FALSE
  )
  performance$near_optimal <- near_optimal_models(
    performance$score, resolved_metric, performance_tolerance
  )
  performance$relative_gap <- relative_performance_gap(performance$score, resolved_metric)

  sampling <- attr(importance_objects[[1L]], "sampling")
  dependence_data <- explainers[[1L]]$data[sampling$row_indices, , drop = FALSE]
  dependence <- dependence_table(dependence_data, dependence_threshold,
    features = features
  )
  importance <- combine_importance(importance_objects, dependence)
  importance$shuffle_status <- shuffle_diagnostic_status(importance, importance_objects)
  importance$dependence_status <- dependence$screen_status[
    match(importance$feature, dependence$feature)
  ]
  importance$claim <- shuffle_diagnostic_claim(importance$shuffle_status)

  agreement <- explanation_agreement(importance_objects, performance$near_optimal, features)
  prediction <- prediction_agreement(explainers, performance$near_optimal)
  findings <- audit_findings(
    importance, dependence, performance, agreement, prediction,
    n_repeats, dependence_threshold, explainers
  )
  summary <- audit_summary(
    importance, dependence, performance, agreement, prediction, findings
  )
  if (sampling$sampled) {
    scope <- explanation_sampling_note(sampling)
    summary$scope_note <- paste(summary$scope_note, scope)
    summary$association_scope <- paste(summary$association_scope, scope)
    findings$scope <- sub("full evaluation predictor context", "sampled evaluation predictor context",
      findings$scope,
      fixed = TRUE
    )
  }

  structure(
    list(
      summary = summary,
      findings = findings,
      performance = performance,
      importance = importance,
      dependence = dependence,
      explanation_agreement = agreement,
      prediction_agreement = prediction,
      importance_objects = importance_objects,
      model_diagnostics = audit_model_diagnostics(importance),
      diagnostic_status = audit_diagnostic_status(
        explainers, features, importance, dependence, performance,
        agreement, prediction, n_repeats, sampling
      ),
      config = list(
        features = features,
        metric = resolved_metric,
        n_repeats = n_repeats,
        seed = seed,
        confidence = confidence,
        performance_tolerance = performance_tolerance,
        dependence_threshold = dependence_threshold,
        max_rows = max_rows, sampling = sampling
      ),
      provenance = list(
        created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
        package_version = package_version_or_development(),
        explainer_fingerprints = vapply(
          explainers, current_explainer_fingerprint, character(1)
        ),
        model_labels = names(explainers),
        diagnostic_scope = paste(
          "Descriptive explanation reliability audit; not causal inference,",
          "population inference, or certification."
        )
      )
    ),
    class = "autoxplain_audit"
  )
}

#' @export
print.autoxplain_audit <- function(x, ...) {
  cat("<AutoXplainR explanation evidence audit>\n")
  cat("  models:             ", x$summary$n_models, " (", x$summary$n_near_optimal,
    " near-optimal)\n",
    sep = ""
  )
  cat("  max association:    ", format(x$summary$max_association, digits = 3), "\n", sep = "")
  cat("  explanation accord: ", format_optional(x$summary$mean_rank_agreement), "\n", sep = "")
  cat("  prediction accord:  ", format_optional(x$summary$prediction_agreement), "\n", sep = "")
  cat("  scope: ", x$summary$scope_note, "\n", sep = "")
  cat("  association: ", x$summary$association_scope, "\n", sep = "")
  if (x$diagnostic_status$comparison$status != "computed") {
    cat("  comparison: ", x$diagnostic_status$comparison$reason, "\n", sep = "")
  }
  if (nrow(x$findings)) {
    cat("\nFindings\n")
    for (index in seq_len(min(6L, nrow(x$findings)))) {
      cat("  [", x$findings$severity[[index]], "] ", x$findings$message[[index]], "\n", sep = "")
    }
  }
  invisible(x)
}

#' Summarize an explanation audit
#'
#' @param object An `autoxplain_audit`.
#' @param ... Unused.
#' @return The audit summary list.
#' @export
summary.autoxplain_audit <- function(object, ...) object$summary

normalize_explainers <- function(explainers) {
  if (inherits(explainers, "autoxplain_explainer")) explainers <- list(explainers)
  if (!is.list(explainers) || !length(explainers) ||
        !all(vapply(explainers, inherits, logical(1), "autoxplain_explainer"))) {
    stop("`explainers` must be an AutoXplainR explainer or a non-empty list of them.",
      call. = FALSE
    )
  }
  labels <- vapply(explainers, `[[`, character(1), "label")
  if (anyDuplicated(labels)) labels <- make.unique(labels)
  names(explainers) <- labels
  explainers
}

near_optimal_models <- function(scores, metric, tolerance) {
  if (metric %in% c("accuracy", "auc")) {
    best <- max(scores)
    gap <- best - scores
  } else {
    best <- min(scores)
    gap <- scores - best
  }
  gap <= tolerance * max(abs(best), sqrt(.Machine$double.eps)) + sqrt(.Machine$double.eps)
}

relative_performance_gap <- function(scores, metric) {
  best <- if (metric %in% c("accuracy", "auc")) max(scores) else min(scores)
  gap <- if (metric %in% c("accuracy", "auc")) best - scores else scores - best
  gap / max(abs(best), sqrt(.Machine$double.eps))
}

combine_importance <- function(objects, dependence) {
  rows <- lapply(seq_along(objects), function(index) {
    item <- as.data.frame(objects[[index]])
    item$model <- names(objects)[[index]]
    item$max_association <- dependence$max_association[
      match(item$feature, dependence$feature)
    ]
    item$associated_feature <- dependence$associated_feature[
      match(item$feature, dependence$feature)
    ]
    item
  })
  out <- do.call(rbind, rows)
  out$unavailable_reason <- vapply(seq_len(nrow(out)), function(i) {
    attr(objects[[out$model[[i]]]], "unavailable_reason") %||% ""
  }, character(1))
  rownames(out) <- NULL
  out[c("model", setdiff(names(out), "model"))]
}

dependence_table <- function(data, threshold, features = names(data)) {
  rows <- lapply(features, function(feature) {
    association <- feature_associations(data, feature)
    usable <- association[is.finite(association)]
    if (length(usable)) {
      maximum <- max(usable)
      paired <- names(usable)[which.max(usable)]
    } else {
      maximum <- if (length(association)) NA_real_ else 0
      paired <- NA_character_
    }
    flagged <- is.finite(maximum) && maximum >= threshold
    data.frame(
      feature = feature,
      max_association = maximum,
      associated_feature = paired,
      high_dependence = flagged,
      screen_status = if (!length(association)) {
        "no_other_predictors"
      } else if (!length(usable)) {
        "association_unavailable"
      } else if (flagged) {
        "association_flagged"
      } else {
        "limited_screen"
      },
      predictors_checked = length(association),
      predictors_unavailable = sum(!is.finite(association)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

feature_associations <- function(data, feature) {
  others <- setdiff(names(data), feature)
  if (!length(others)) {
    return(setNames(numeric(), character()))
  }
  values <- vapply(others, function(other) {
    feature_association(data[[feature]], data[[other]])
  }, numeric(1))
  values
}

feature_association <- function(x, y) {
  complete <- stats::complete.cases(x, y)
  x <- x[complete]
  y <- y[complete]
  if (length(x) < 3L || length(unique(x)) < 2L || length(unique(y)) < 2L) {
    return(0)
  }
  x_numeric <- is.numeric(x)
  y_numeric <- is.numeric(y)
  if ((!x_numeric && length(unique(x)) == length(x)) ||
        (!y_numeric && length(unique(y)) == length(y))) {
    return(NA_real_)
  }
  if (x_numeric && y_numeric) {
    return(abs(stats::cor(x, y, method = "spearman")))
  }
  if (x_numeric != y_numeric) {
    numeric_value <- if (x_numeric) x else y
    category <- factor(if (x_numeric) y else x)
    overall <- mean(numeric_value)
    group_n <- table(category)
    group_mean <- tapply(numeric_value, category, mean)
    between <- sum(group_n * (group_mean - overall)^2)
    total <- sum((numeric_value - overall)^2)
    return(if (total > 0) sqrt(between / total) else 0)
  }
  x_levels <- unique(x)
  y_levels <- unique(y)
  if (as.double(length(x_levels)) * length(y_levels) > 1e6) {
    # Pearson's statistic can be computed from observed cells alone:
    # sum(O^2/E) - n. Empty cross-product cells need no allocation.
    x_id <- match(x, x_levels)
    y_id <- match(y, y_levels)
    n <- length(x)
    keys <- (as.double(x_id) - 1) * length(y_levels) + y_id
    first <- !duplicated(keys)
    counts <- tabulate(match(keys, keys[first]))
    expected <- as.double(tabulate(x_id)[x_id[first]]) * tabulate(y_id)[y_id[first]] / n
    chi_squared <- max(0, sum(counts^2 / expected) - n)
    denominator <- n * (min(length(x_levels), length(y_levels)) - 1)
    return(sqrt(chi_squared / denominator))
  }
  table_xy <- table(factor(x), factor(y))
  n <- sum(table_xy)
  expected <- outer(rowSums(table_xy), colSums(table_xy)) / n
  valid <- expected > 0
  chi_squared <- sum((table_xy[valid] - expected[valid])^2 / expected[valid])
  denominator <- n * min(nrow(table_xy) - 1L, ncol(table_xy) - 1L)
  if (denominator > 0) sqrt(chi_squared / denominator) else 0
}

association_screen_scope <- function() {
  paste(
    "Limited pairwise screen: absolute Spearman correlation for numeric pairs,",
    "correlation ratio for mixed pairs, and Cramer's V for categorical pairs.",
    "Small values do not establish independence or exclude nonlinear or joint dependence.",
    "Categorical pairs without repeated categories are unavailable; many rare categories can inflate association."
  )
}

shuffle_diagnostic_status <- function(importance, objects) {
  status <- rep("interval_includes_zero", nrow(importance))
  unavailable <- !is.finite(importance$conf_low) | !is.finite(importance$conf_high)
  status[unavailable] <- "interval_unavailable"
  status[!unavailable & importance$conf_low > 0] <- "positive_loss_change"
  status[!unavailable & importance$conf_high < 0] <- "negative_loss_change"
  unchanged <- vapply(seq_len(nrow(importance)), function(index) {
    repeats <- attr(objects[[importance$model[[index]]]], "repeat_scores")
    values <- repeats[importance$feature[[index]], ]
    all(is.finite(values)) && all(values == 0)
  }, logical(1))
  status[unchanged] <- "no_observed_change"
  status[!is.finite(importance$importance)] <- "importance_unavailable"
  status
}

shuffle_diagnostic_claim <- function(status) {
  unname(c(
    no_observed_change = "No loss change in these shuffles; this is not proof of no population importance.",
    positive_loss_change = "Shuffling increased loss; the fixed-sample Monte Carlo interval excludes zero.",
    negative_loss_change = "Shuffling reduced loss; the fixed-sample Monte Carlo interval excludes zero.",
    interval_includes_zero = "The fixed-sample Monte Carlo interval includes zero.",
    interval_unavailable = "The permutation budget does not provide a Monte Carlo interval.",
    importance_unavailable = "Permutation importance was unavailable for the sampled rows."
  )[status])
}

explanation_agreement <- function(objects, near_optimal, features) {
  selected <- objects[near_optimal]
  matrix_values <- vapply(selected, function(object) {
    values <- object$importance[match(features, object$feature)]
    setNames(values, features)
  }, numeric(length(features)))
  matrix_values <- matrix(
    matrix_values,
    nrow = length(features),
    ncol = length(selected),
    dimnames = list(features, names(selected))
  )
  correlation <- if (ncol(matrix_values) > 1L) {
    suppressWarnings(stats::cor(matrix_values, method = "spearman", use = "pairwise.complete.obs"))
  } else {
    matrix(1, 1L, 1L, dimnames = list(colnames(matrix_values), colnames(matrix_values)))
  }
  mean_agreement <- if (ncol(correlation) > 1L) {
    mean(correlation[lower.tri(correlation)], na.rm = TRUE)
  } else {
    NA_real_
  }
  ranges <- data.frame(
    feature = features,
    min_importance = apply(matrix_values, 1L, function(x) if (any(is.finite(x))) min(x, na.rm = TRUE) else NA_real_),
    max_importance = apply(matrix_values, 1L, function(x) if (any(is.finite(x))) max(x, na.rm = TRUE) else NA_real_),
    mean_importance = rowMeans(matrix_values, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  list(
    rank_correlation = correlation,
    mean_rank_correlation = mean_agreement,
    importance_ranges = ranges
  )
}

prediction_agreement <- function(explainers, near_optimal) {
  selected <- explainers[near_optimal]
  if (length(selected) < 2L) {
    return(list(score = NA_real_, pairwise = NULL, ambiguity = NA_real_))
  }
  task <- selected[[1L]]$task
  predictions <- lapply(selected, function(x) predict(x, x$data))
  if (task == "binary" && any(vapply(predictions, is.factor, logical(1)))) {
    hard <- vapply(seq_along(predictions), function(index) {
      value <- predictions[[index]]
      if (is.factor(value)) {
        return(as.character(value))
      }
      positive <- selected[[index]]$positive
      negative <- setdiff(selected[[index]]$class_levels, positive)[[1L]]
      ifelse(value >= 0.5, positive, negative)
    }, character(nrow(selected[[1L]]$data)))
    pairwise <- outer(seq_along(selected), seq_along(selected), Vectorize(function(i, j) {
      mean(hard[, i] == hard[, j])
    }))
    dimnames(pairwise) <- list(names(selected), names(selected))
    ambiguity <- mean(apply(hard, 1L, function(x) length(unique(x)) > 1L))
    return(list(
      score = mean(pairwise[lower.tri(pairwise)]), pairwise = pairwise,
      ambiguity = ambiguity
    ))
  }
  if (task == "multiclass") {
    hard <- vapply(predictions, function(x) {
      if (is.matrix(x)) colnames(x)[max.col(x, ties.method = "first")] else as.character(x)
    }, character(nrow(selected[[1L]]$data)))
    pairwise <- outer(seq_along(selected), seq_along(selected), Vectorize(function(i, j) {
      mean(hard[, i] == hard[, j])
    }))
    dimnames(pairwise) <- list(names(selected), names(selected))
    ambiguity <- mean(apply(hard, 1L, function(x) length(unique(x)) > 1L))
    return(list(score = 1 - ambiguity, pairwise = pairwise, ambiguity = ambiguity))
  }
  prediction_matrix <- do.call(cbind, predictions)
  colnames(prediction_matrix) <- names(selected)
  pairwise <- suppressWarnings(stats::cor(prediction_matrix, method = "spearman"))
  rank_score <- mean(pairwise[lower.tri(pairwise)], na.rm = TRUE)
  if (task == "binary") {
    hard <- prediction_matrix >= 0.5
    ambiguity <- mean(apply(hard, 1L, function(x) length(unique(x)) > 1L))
  } else {
    scale <- stats::sd(as.vector(prediction_matrix))
    row_spread <- apply(prediction_matrix, 1L, function(x) max(x) - min(x))
    ambiguity <- if (is.finite(scale) && scale > 0) mean(row_spread) / scale else 0
  }
  list(score = rank_score, pairwise = pairwise, ambiguity = ambiguity)
}

audit_findings <- function(importance,
                           dependence,
                           performance,
                           agreement,
                           prediction,
                           n_repeats,
                           threshold,
                           explainers) {
  accumulator <- new.env(parent = emptyenv())
  accumulator$findings <- list()
  add <- function(severity, code, message, evidence, recommendation,
                  model = NA_character_, feature = NA_character_,
                  scope = "Supplied fitted models and evaluation rows") {
    findings <- accumulator$findings
    findings[[length(findings) + 1L]] <- data.frame(
      severity = severity, code = code, message = message, evidence = evidence,
      recommendation = recommendation, model = model, feature = feature,
      scope = scope, stringsAsFactors = FALSE
    )
    findings[[length(findings)]]$entities <- I(list(list(
      models = if (is.na(model)) names(explainers) else model,
      features = if (is.na(feature)) character() else feature
    )))
    accumulator$findings <- findings
  }
  high <- dependence[dependence$high_dependence, , drop = FALSE]
  for (index in seq_len(nrow(high))) {
    item <- high[index, , drop = FALSE]
    add(
      "warning", "feature_dependence",
      paste0("`", item$feature, "` exceeds the pairwise association threshold."),
      paste0(
        "Association with `", item$associated_feature, "`: ",
        format(item$max_association, digits = 3), "."
      ),
      "Inspect joint support; interpret marginal shuffling as fitted reliance and consider ALE for effects.",
      feature = item$feature,
      scope = "Selected feature checked against the full evaluation predictor context"
    )
  }
  limited <- importance[
    importance$shuffle_status %in% c("interval_includes_zero", "interval_unavailable"), , drop = FALSE
  ]
  for (index in seq_len(nrow(limited))) {
    item <- limited[index, , drop = FALSE]
    add(
      "note", "shuffle_interval_unresolved",
      paste0(
        "The shuffle interval for `", item$model, "` / `", item$feature,
        "` does not resolve the sign of the mean loss change."
      ),
      item$claim,
      "Inspect the repeat distribution; more shuffles address Monte Carlo error only.",
      model = item$model, feature = item$feature,
      scope = "Permutation randomness conditional on this fitted model and evaluation sample"
    )
  }
  reasons <- unique(importance$unavailable_reason[nzchar(importance$unavailable_reason)])
  if (length(reasons)) {
    add("note", "importance_unavailable", "Some permutation importance was unavailable.",
      paste(reasons, collapse = " "), "Increase the explanation row limit or use every evaluation row.",
      scope = "Sampled explanation rows"
    )
  }
  add(
    "note", "association_screen_scope",
    "The pairwise association screen does not assess every form of dependence.",
    association_screen_scope(),
    "Review nonlinear relationships and joint support before interpreting shuffled inputs or marginal effects.",
    scope = "Limits of the association screen"
  )
  unavailable <- dependence$feature[dependence$predictors_unavailable > 0L]
  if (length(unavailable)) {
    add(
      "note", "association_unavailable",
      "Some feature associations could not be assessed.",
      paste("Affected features:", paste(unavailable, collapse = ", ")),
      "Inspect category replication and finite paired values; an unavailable association does not mean independence.",
      scope = "Unavailable pairwise associations"
    )
  }
  if (sum(performance$near_optimal) > 1L &&
        is.finite(agreement$mean_rank_correlation) && agreement$mean_rank_correlation < 0.7) {
    add(
      "critical", "rashomon_disagreement",
      "Near-optimal models disagree on the feature-importance ranking.",
      paste("Mean Spearman agreement:", format(agreement$mean_rank_correlation, digits = 3)),
      "Report the supplied candidates' importance ranges; these are not bounds over a complete model class."
    )
  }
  if (sum(performance$near_optimal) > 1L && is.finite(prediction$ambiguity) &&
        prediction$ambiguity > 0.1) {
    add(
      "warning", "predictive_multiplicity",
      "Near-optimal models have material prediction disagreement.",
      paste("Disagreement/ambiguity diagnostic:", format(prediction$ambiguity, digits = 3)),
      "Inspect case-level disagreement before deployment and document the model-selection rule."
    )
  }
  if (n_repeats < 20L) {
    add(
      "note", "low_monte_carlo_budget",
      "The permutation budget is small; its interval endpoints may depend on the shuffles drawn.",
      paste(n_repeats, "repeats per model-feature pair."),
      paste(
        "Inspect Monte Carlo error and increase repeats when it is material;",
        "more repeats do not add evaluation observations."
      )
    )
  }
  evaluation_roles <- vapply(
    explainers, function(x) x$metadata$evaluation_role %||% "unspecified",
    character(1)
  )
  if (any(evaluation_roles == "unspecified")) {
    add(
      "note", "evaluation_role_unspecified",
      "The audit cannot verify that explanation data are independent of model fitting.",
      "At least one explainer has no `metadata$evaluation_role`.",
      "Use held-out data and record metadata = list(evaluation_role = 'test')."
    )
  }
  do.call(rbind, accumulator$findings)
}

audit_summary <- function(importance,
                          dependence,
                          performance,
                          agreement,
                          prediction,
                          findings) {
  list(
    scope_note = paste(
      "Separate descriptive diagnostics; no overall evidence grade.",
      "Shuffle intervals omit evaluation-sampling, fitting and selection uncertainty."
    ),
    association_scope = association_screen_scope(),
    n_models = nrow(performance),
    n_near_optimal = sum(performance$near_optimal),
    max_association = if (any(is.finite(dependence$max_association))) {
      max(dependence$max_association, na.rm = TRUE)
    } else {
      NA_real_
    },
    mean_rank_agreement = agreement$mean_rank_correlation,
    prediction_agreement = prediction$score,
    critical_findings = sum(findings$severity == "critical"),
    warning_findings = sum(findings$severity == "warning")
  )
}

audit_model_diagnostics <- function(importance) {
  statuses <- c(
    "no_observed_change", "positive_loss_change", "negative_loss_change",
    "interval_includes_zero", "interval_unavailable", "importance_unavailable"
  )
  rows <- lapply(unique(importance$model), function(model) {
    item <- importance[importance$model == model, , drop = FALSE]
    counts <- table(factor(item$shuffle_status, levels = statuses))
    data.frame(
      model = model, features = nrow(item),
      stats::setNames(as.list(as.integer(counts)), statuses),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  output <- do.call(rbind, rows)
  names(output) <- c("model", "features", statuses)
  rownames(output) <- NULL
  output
}

audit_diagnostic_status <- function(explainers, features, importance, dependence,
                                    performance, agreement, prediction, n_repeats, sampling = NULL) {
  models <- names(explainers)
  selected <- performance$model[performance$near_optimal]
  comparison_available <- length(selected) >= 2L
  entry <- function(id, status, scope, entities, evidence, interpretation, reason = NULL) {
    list(
      id = id, status = status, scope = scope, entities = entities,
      evidence = evidence, interpretation = interpretation, reason = reason
    )
  }
  list(
    evaluation = entry(
      "evaluation", "computed", "Evaluation contract and reported sample size",
      list(models = models),
      list(
        rows = nrow(explainers[[1L]]$data),
        roles = vapply(
          explainers, function(x) x$metadata$evaluation_role %||% "unspecified",
          character(1)
        )
      ),
      paste(
        "Ordered observations, outcomes and event semantics agree;",
        "independence and representativeness are not established by these checks."
      )
    ),
    association = entry(
      "association", if (all(dependence$predictors_checked == 0L)) {
        "inapplicable"
      } else if (all(!is.finite(dependence$max_association))) {
        "unavailable"
      } else {
        "computed"
      },
      if (isTRUE(sampling$sampled)) {
        "Selected features against all predictors on sampled evaluation rows"
      } else {
        "Selected features against all evaluation predictors"
      },
      list(models = models, features = features), dependence, association_screen_scope(),
      if (all(dependence$predictors_checked == 0L)) "No other predictors are available for pairwise association."
    ),
    permutation = entry(
      "permutation", if (all(!is.finite(importance$importance))) "unavailable" else "computed",
      if (isTRUE(sampling$sampled)) {
        "Fixed models and sampled evaluation observations"
      } else {
        "Fixed models and fixed evaluation observations"
      },
      list(models = models, features = features),
      list(
        repeats = n_repeats, rows = sampling$rows_used %||% nrow(explainers[[1L]]$data),
        rows_available = nrow(explainers[[1L]]$data), feature_model_pairs = nrow(importance),
        interval = "Monte Carlo t interval", sampling = sampling
      ),
      paste(
        "Loss changes describe these shuffles. The intervals quantify shuffle randomness,",
        "not sampling, fitting or selection uncertainty."
      )
    ),
    comparison = entry(
      "comparison", if (comparison_available) "computed" else "insufficient_evidence",
      "Supplied candidates within the configured empirical performance tolerance",
      list(models = selected, features = features),
      list(
        mean_rank_agreement = agreement$mean_rank_correlation,
        prediction_agreement = prediction$score, ambiguity = prediction$ambiguity
      ),
      "Observed candidate disagreement does not bound all models or establish a unique explanation.",
      if (!comparison_available) "Fewer than two supplied models meet the performance tolerance."
    )
  )
}

format_percent <- function(x) {
  if (!is.finite(x)) {
    return("n/a")
  }
  paste0(format(round(100 * x, 1), nsmall = 1), "%")
}

format_optional <- function(x) {
  if (length(x) != 1L || !is.finite(x)) "unavailable" else format(x, digits = 3)
}
