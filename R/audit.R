#' Stress-test the evidence behind model explanations
#'
#' `audit_explanations()` is AutoXplainR's advanced reliability layer. It evaluates
#' repeated permutation importance, feature dependence, Monte Carlo stability,
#' prediction disagreement, and explanation disagreement among near-equivalent
#' models. The output distinguishes robust descriptive evidence from claims
#' that need more data or a conditional method.
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
#' @param dependence_threshold Association above which marginal importance and
#'   PDP claims receive a dependence warning.
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
                               dependence_threshold = 0.7) {
  explainers <- normalize_explainers(explainers)
  n_repeats <- assert_count(n_repeats, "n_repeats")
  assert_probability(confidence, "confidence", open = TRUE)
  assert_probability(performance_tolerance, "performance_tolerance")
  assert_probability(dependence_threshold, "dependence_threshold", open = TRUE)

  tasks <- unique(vapply(explainers, `[[`, character(1), "task"))
  if (length(tasks) != 1L) {
    stop("All explainers in an audit must have the same task.", call. = FALSE)
  }
  row_counts <- unique(vapply(explainers, function(x) nrow(x$data), integer(1)))
  if (length(row_counts) != 1L) {
    stop("All explainers must use evaluation data with the same number of rows.",
         call. = FALSE)
  }
  available <- Reduce(intersect, lapply(explainers, function(x) names(x$data)))
  features <- features %||% available
  if (!is.character(features) || !length(features) || anyNA(features)) {
    stop("`features` must be a non-empty character vector.", call. = FALSE)
  }
  missing <- setdiff(features, available)
  if (length(missing)) {
    stop("Features are not available in every explainer: ", paste(missing, collapse = ", "),
         call. = FALSE)
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
      confidence = confidence
    )
  }
  names(importance_objects) <- names(explainers)

  performance <- data.frame(
    model = names(explainers),
    score = vapply(importance_objects, function(x) attr(x, "baseline_score"), numeric(1)),
    metric = resolved_metric,
    stringsAsFactors = FALSE
  )
  performance$near_optimal <- near_optimal_models(
    performance$score, resolved_metric, performance_tolerance
  )
  performance$relative_gap <- relative_performance_gap(performance$score, resolved_metric)

  dependence <- dependence_table(explainers[[1L]]$data[features], dependence_threshold)
  importance <- combine_importance(importance_objects, dependence)
  importance$evidence_grade <- evidence_grade(
    importance$sign_stability,
    importance$conf_low,
    importance$conf_high,
    importance$max_association,
    dependence_threshold
  )
  importance$claim <- evidence_claim(importance$evidence_grade)

  agreement <- explanation_agreement(importance_objects, performance$near_optimal, features)
  prediction <- prediction_agreement(explainers, performance$near_optimal)
  findings <- audit_findings(
    importance, dependence, performance, agreement, prediction,
    n_repeats, dependence_threshold, explainers
  )
  summary <- audit_summary(
    importance, dependence, performance, agreement, prediction, findings
  )

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
      config = list(
        features = features,
        metric = resolved_metric,
        n_repeats = n_repeats,
        seed = seed,
        confidence = confidence,
        performance_tolerance = performance_tolerance,
        dependence_threshold = dependence_threshold
      ),
      provenance = list(
        created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
        package_version = package_version_or_development(),
        explainer_fingerprints = vapply(
          explainers, function(x) x$provenance$fingerprint, character(1)
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
  cat("  grade:              ", x$summary$grade, " (diagnostic, not certification)\n", sep = "")
  cat("  models:             ", x$summary$n_models, " (", x$summary$n_near_optimal,
      " near-optimal)\n", sep = "")
  cat("  stable claims:      ", format_percent(x$summary$stable_claim_rate), "\n", sep = "")
  cat("  max dependence:     ", format(x$summary$max_association, digits = 3), "\n", sep = "")
  cat("  explanation accord: ", format_optional(x$summary$mean_rank_agreement), "\n", sep = "")
  cat("  prediction accord:  ", format_optional(x$summary$prediction_agreement), "\n", sep = "")
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
         call. = FALSE)
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
  rownames(out) <- NULL
  out[c("model", setdiff(names(out), "model"))]
}

dependence_table <- function(data, threshold) {
  features <- names(data)
  rows <- lapply(features, function(feature) {
    association <- feature_associations(data, feature)
    if (length(association)) {
      maximum <- max(association, na.rm = TRUE)
      if (!is.finite(maximum)) maximum <- 0
      paired <- names(association)[which.max(association)]
    } else {
      maximum <- 0
      paired <- NA_character_
    }
    data.frame(
      feature = feature,
      max_association = maximum,
      associated_feature = paired,
      high_dependence = maximum >= threshold,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

feature_associations <- function(data, feature) {
  others <- setdiff(names(data), feature)
  if (!length(others)) return(setNames(numeric(), character()))
  values <- vapply(others, function(other) {
    feature_association(data[[feature]], data[[other]])
  }, numeric(1))
  values[is.na(values)] <- 0
  values
}

feature_association <- function(x, y) {
  complete <- stats::complete.cases(x, y)
  x <- x[complete]
  y <- y[complete]
  if (length(x) < 3L || length(unique(x)) < 2L || length(unique(y)) < 2L) return(0)
  x_numeric <- is.numeric(x)
  y_numeric <- is.numeric(y)
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
  table_xy <- table(factor(x), factor(y))
  n <- sum(table_xy)
  expected <- outer(rowSums(table_xy), colSums(table_xy)) / n
  valid <- expected > 0
  chi_squared <- sum((table_xy[valid] - expected[valid])^2 / expected[valid])
  denominator <- n * min(nrow(table_xy) - 1L, ncol(table_xy) - 1L)
  if (denominator > 0) sqrt(chi_squared / denominator) else 0
}

evidence_grade <- function(stability, lower, upper, association, threshold) {
  excludes_zero <- !is.na(lower) & !is.na(upper) & (lower > 0 | upper < 0)
  grade <- rep("D", length(stability))
  grade[stability >= 0.6] <- "C"
  grade[stability >= 0.8 & association < threshold & excludes_zero] <- "B"
  grade[stability >= 0.9 & association < min(0.5, threshold) & excludes_zero] <- "A"
  factor(grade, levels = c("A", "B", "C", "D"), ordered = TRUE)
}

evidence_claim <- function(grade) {
  unname(c(
    A = "stable marginal evidence",
    B = "usable with stated uncertainty",
    C = "sensitivity finding only",
    D = "do not make a feature claim"
  )[as.character(grade)])
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
    min_importance = apply(matrix_values, 1L, min, na.rm = TRUE),
    max_importance = apply(matrix_values, 1L, max, na.rm = TRUE),
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
  if (task == "multiclass") {
    hard <- vapply(predictions, function(x) {
      if (is.matrix(x)) colnames(x)[max.col(x, ties.method = "first")] else as.character(x)
    }, character(nrow(selected[[1L]]$data)))
    class_levels <- sort(unique(as.vector(hard)))
    coded <- apply(hard, 2L, function(x) as.integer(factor(x, levels = class_levels)))
    pairwise <- suppressWarnings(stats::cor(coded, method = "spearman"))
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
  add <- function(severity, code, message, evidence, recommendation) {
    findings <- accumulator$findings
    findings[[length(findings) + 1L]] <- data.frame(
      severity = severity, code = code, message = message, evidence = evidence,
      recommendation = recommendation, stringsAsFactors = FALSE
    )
    accumulator$findings <- findings
  }
  high <- dependence[dependence$high_dependence, , drop = FALSE]
  if (nrow(high)) {
    add(
      "warning", "feature_dependence",
      paste(nrow(high), "feature(s) exceed the dependence threshold."),
      paste0("Maximum observed association is ", format(max(high$max_association), digits = 3), "."),
      paste(
        "Prefer ALE for effects; treat marginal PFI as model reliance, and use a conditional",
        "method for conditional claims."
      )
    )
  }
  limited <- importance[as.character(importance$evidence_grade) %in% c("C", "D"), , drop = FALSE]
  if (nrow(limited)) {
    crosses_zero <- with(limited, is.na(conf_low) | is.na(conf_high) |
                           !(conf_low > 0 | conf_high < 0))
    unstable <- limited$sign_stability < 0.8
    dependent <- limited$max_association >= threshold
    add(
      "warning", "limited_importance_evidence",
      paste(nrow(limited), "model-feature claim(s) are qualified or unsupported."),
      paste0(
        sum(crosses_zero), " interval(s) include zero; ", sum(unstable),
        " have sign stability below 0.8; ", sum(dependent),
        " exceed the dependence threshold."
      ),
      paste(
        "Inspect repeat distributions and dependence, increase evaluation data when uncertainty",
        "is material, and avoid ranking unsupported features."
      )
    )
  }
  if (sum(performance$near_optimal) > 1L &&
        is.finite(agreement$mean_rank_correlation) && agreement$mean_rank_correlation < 0.7) {
    add(
      "critical", "rashomon_disagreement",
      "Near-optimal models disagree on the feature-importance ranking.",
      paste("Mean Spearman agreement:", format(agreement$mean_rank_correlation, digits = 3)),
      "Report model-class importance ranges instead of presenting one best model's ranking as unique."
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
      "The permutation budget is suitable for smoke testing, not a final report.",
      paste(n_repeats, "repeats per model-feature pair."),
      "Use at least 20 repeats for routine work and more when feature ranks are close."
    )
  }
  evaluation_roles <- vapply(explainers, function(x) x$metadata$evaluation_role %||% "unspecified",
                             character(1))
  if (any(evaluation_roles == "unspecified")) {
    add(
      "note", "evaluation_role_unspecified",
      "The audit cannot verify that explanation data are independent of model fitting.",
      "At least one explainer has no `metadata$evaluation_role`.",
      "Use held-out data and record metadata = list(evaluation_role = 'test')."
    )
  }
  if (!length(accumulator$findings)) {
    add(
      "note", "no_automatic_red_flags",
      "No automatic reliability threshold was breached.",
      "Diagnostics passed the configured heuristic thresholds.",
      "Continue with domain review, external validation, and explicit limitations."
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
  grades <- as.character(importance$evidence_grade)
  stable <- grades %in% c("A", "B")
  numeric_grade <- c(A = 4, B = 3, C = 2, D = 1)[grades]
  overall <- names(which.min(abs(c(A = 4, B = 3, C = 2, D = 1) - stats::median(numeric_grade))))
  if (any(findings$severity == "critical")) overall <- downgrade_grade(overall, 2L)
  list(
    grade = overall,
    grade_note = "Heuristic evidence grade; not a certification or formal inferential guarantee.",
    n_models = nrow(performance),
    n_near_optimal = sum(performance$near_optimal),
    stable_claim_rate = mean(stable),
    max_association = max(dependence$max_association),
    mean_rank_agreement = agreement$mean_rank_correlation,
    prediction_agreement = prediction$score,
    critical_findings = sum(findings$severity == "critical"),
    warning_findings = sum(findings$severity == "warning")
  )
}

downgrade_grade <- function(grade, steps = 1L) {
  levels <- c("A", "B", "C", "D")
  levels[min(match(grade, levels) + steps, length(levels))]
}

format_percent <- function(x) {
  if (!is.finite(x)) return("n/a")
  paste0(format(round(100 * x, 1), nsmall = 1), "%")
}

format_optional <- function(x) {
  if (length(x) != 1L || !is.finite(x)) "n/a (one model)" else format(x, digits = 3)
}
