#' Compare what retained models do differently
#'
#' `compare_model_behavior()` turns a multi-model [autoxplain()] result into a
#' common, model-agnostic comparison. It keeps two kinds of evidence separate:
#'
#' * learner behavior cards describe what each model family can represent;
#'   they are reviewed prior knowledge, not findings from the supplied data;
#' * performance and paired prediction differences are computed from the
#'   result's common evaluation rows; serialized size or runtime, when shown,
#'   is an operational resource proxy rather than structural model complexity.
#'
#' Optionally, pass an audit created with
#' `audit_explanations(as_explainers(result, models = ...))`. The comparison
#' then adds compact repeated-permutation-importance ranks and pairwise rank
#' agreement. Permutation importance measures model reliance on the supplied
#' evaluation data; it is not a causal effect or population-level inference.
#'
#' Effect-shape comparisons are deliberately not generated automatically.
#' Marginal curves need feature-specific grids and dependence checks, so a
#' generic shape claim would be easy to overstate. Use [explain_effect()] for a
#' named feature after reviewing the audit's dependence diagnostics.
#'
#' @param result An `autoxplain_result` containing at least two comparable
#'   retained models.
#' @param models Model IDs, indices, or `NULL`. `NULL` compares every retained
#'   model not labeled as a baseline.
#' @param performance_tolerance Optional non-negative relative gap from the
#'   best selected evaluation score. `NULL` keeps all selected models.
#' @param explanation_audit Optional `autoxplain_audit` made from the same
#'   result, selected model IDs, and evaluation rows.
#'
#' @return An `autoxplain_model_behavior` object. Its `models` table combines
#'   performance, a clearly labeled trade-off proxy, and behavior cards;
#'   `prediction_pairs` contains paired disagreement; `ambiguity` retains
#'   case-level disagreement;
#'   `feature_evidence` contains optional top-feature ranks and pairwise
#'   explanation-rank agreement; and `findings` contains plain-language
#'   comparison statements.
#' @export
#'
#' @examples
#' fit <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 2026)
#' compare_model_behavior(fit)
compare_model_behavior <- function(result,
                                   models = NULL,
                                   performance_tolerance = NULL,
                                   explanation_audit = NULL) {
  if (!inherits(result, "autoxplain_result")) {
    stop("`result` must be returned by `autoxplain()`.", call. = FALSE)
  }
  ambiguity <- prediction_ambiguity(
    result,
    models = models,
    performance_tolerance = performance_tolerance
  )
  model_ids <- ambiguity$model_ids
  explainers <- as_explainers(result, models = model_ids)
  predictions <- lapply(explainers, function(explainer) {
    predict(explainer, explainer$data)
  })
  model_table <- behavior_model_table(result, model_ids, ambiguity)
  prediction_pairs <- behavior_prediction_pairs(
    predictions,
    explainers[[1L]],
    model_table
  )
  feature_evidence <- behavior_feature_evidence(
    explanation_audit,
    model_ids,
    explainers
  )
  findings <- behavior_findings(
    model_table,
    prediction_pairs,
    feature_evidence
  )
  evidence_sources <- data.frame(
    source = c(
      "learner behavior cards",
      "evaluation performance and resource trade-off",
      "paired prediction disagreement",
      "repeated permutation importance"
    ),
    evidence_kind = c("prior knowledge", rep("computed", 3L)),
    available = c(TRUE, TRUE, TRUE, !is.null(feature_evidence)),
    meaning = c(
      "Patterns the model family can represent, not patterns proven to be fitted.",
      paste(
        "Observed performance on common evaluation rows plus an operational",
        "resource proxy when model size or runtime is available."
      ),
      "Observed differences between model predictions on the same rows.",
      "Model reliance with Monte Carlo diagnostics; not a causal effect."
    ),
    stringsAsFactors = FALSE
  )
  scope <- list(
    evaluation_role = ambiguity$evaluation_role,
    prediction = ambiguity$scope_note,
    behavior_cards = paste(
      "Behavior cards are reviewed descriptions of model-family capacity;",
      "they do not prove that a fitted model used every available pattern."
    ),
    tradeoff = paste(
      "The secondary Pareto axis is labeled in `tradeoff_kind`. Serialized",
      "model size and runtime are resource proxies, not structural complexity",
      "or evidence that a model used its available capacity."
    ),
    feature_evidence = if (is.null(feature_evidence)) {
      paste(
        "No feature-ranking comparison was computed. Pass an explanation audit",
        "made from the same selected models and evaluation rows to add one."
      )
    } else {
      feature_evidence$scope_note
    },
    shape_evidence = paste(
      "No automatic effect-shape claims are made. Use explain_effect() for a",
      "named feature after checking dependence and evaluation scope."
    )
  )
  summary <- behavior_summary(
    model_table,
    prediction_pairs,
    ambiguity,
    feature_evidence
  )
  structure(
    list(
      task = result$task,
      model_ids = model_ids,
      n_models = length(model_ids),
      n_evaluation_rows = nrow(explainers[[1L]]$data),
      performance_metric = ambiguity$performance_metric,
      tradeoff_metric = attr(model_table, "tradeoff_metric"),
      tradeoff_kind = attr(model_table, "tradeoff_kind"),
      distance_definition = unique(prediction_pairs$distance_definition),
      models = model_table,
      prediction_pairs = prediction_pairs,
      ambiguity = ambiguity,
      feature_evidence = feature_evidence,
      shape_evidence = NULL,
      findings = findings,
      evidence_sources = evidence_sources,
      summary = summary,
      scope = scope
    ),
    class = c("autoxplain_model_behavior", "list")
  )
}

#' @export
print.autoxplain_model_behavior <- function(x, ...) {
  cat("<AutoXplainR model behavior comparison>\n")
  cat("  models:      ", x$n_models, " (", paste(x$model_ids, collapse = ", "), ")\n",
      sep = "")
  cat("  evidence:    ", x$n_evaluation_rows, " ", x$scope$evaluation_role,
      " rows\n", sep = "")
  cat("  performance: ", x$performance_metric, " (",
      if (isTRUE(attr(x$models, "higher_is_better"))) "higher" else "lower",
      " is better)\n", sep = "")
  cat("  trade-off:   ", x$tradeoff_metric, " (", x$tradeoff_kind, ")\n", sep = "")
  cat("  distance:    ", x$distance_definition, "\n", sep = "")
  cat("  feature check: ", if (is.null(x$feature_evidence)) {
    "not computed"
  } else {
    paste0(
      "repeated permutation importance with ",
      x$feature_evidence$n_repeats,
      " repeats"
    )
  }, "\n", sep = "")
  cat("\nModels at a glance\n")
  print.data.frame(behavior_print_table(x), row.names = FALSE)
  cat("  * best supplied evaluation score; rankings remain descriptive\n")
  cat("\nWhat differs\n")
  for (message in utils::head(x$findings$message, 4L)) {
    cat("  - ", message, "\n", sep = "")
  }
  cat("\nEvidence key\n")
  cat("  behavior cards = prior knowledge about model capacity\n")
  cat(paste(
    "  metrics, prediction gaps, and optional permutation importance",
    "= computed evidence\n"
  ))
  cat("  caution: descriptive comparison, not causal or uncertainty coverage\n")
  invisible(x)
}

behavior_print_table <- function(x) {
  output <- data.frame(
    model = ifelse(
      x$models$best_performance,
      paste0(x$models$model_id, " *"),
      x$models$model_id
    ),
    family = x$models$family,
    backend = x$models$backend,
    score = signif(x$models$performance_score, 4L),
    relative_gap = paste0(
      format(round(100 * x$models$relative_performance_gap, 1L), trim = TRUE),
      "%"
    ),
    stringsAsFactors = FALSE
  )
  names(output)[names(output) == "score"] <- x$performance_metric
  tradeoff_metric <- x$tradeoff_metric
  if (length(tradeoff_metric) == 1L && !is.na(tradeoff_metric) &&
        any(is.finite(x$models$tradeoff_proxy))) {
    output[[tradeoff_metric]] <- signif(x$models$tradeoff_proxy, 4L)
  }
  output
}

#' @export
summary.autoxplain_model_behavior <- function(object, ...) object$summary

behavior_model_table <- function(result, model_ids, ambiguity) {
  leaderboard <- enrich_tradeoff_leaderboard(result)
  leaderboard_index <- match(model_ids, leaderboard$model_id)
  performance <- ambiguity$model_performance[
    match(model_ids, ambiguity$model_performance$model_id),
    ,
    drop = FALSE
  ]
  metric <- ambiguity$performance_metric
  score <- as.numeric(performance[[metric]])
  higher_is_better <- metric %in% higher_is_better_metrics()
  performance_loss <- if (higher_is_better) -score else score
  tradeoff <- behavior_tradeoff_proxy(model_ids, leaderboard)
  output <- data.frame(
    model_id = model_ids,
    model = behavior_leaderboard_value(
      leaderboard,
      leaderboard_index,
      "model",
      model_ids
    ),
    role = behavior_leaderboard_value(
      leaderboard,
      leaderboard_index,
      "role",
      rep("candidate", length(model_ids))
    ),
    family = vapply(seq_along(model_ids), function(index) {
      behavior_model_family(
        result$models[[model_ids[[index]]]],
        leaderboard,
        leaderboard_index[[index]]
      )
    }, character(1)),
    backend = vapply(seq_along(model_ids), function(index) {
      behavior_model_backend(
        result$models[[model_ids[[index]]]],
        leaderboard,
        leaderboard_index[[index]]
      )
    }, character(1)),
    performance_rank = rank(performance_loss, ties.method = "min"),
    performance_score = score,
    relative_performance_gap = performance$relative_performance_gap,
    tradeoff_proxy = tradeoff$values,
    tradeoff_kind = rep(tradeoff$kind, length(model_ids)),
    stringsAsFactors = FALSE
  )
  best <- min(performance_loss)
  output$best_performance <- performance_loss <=
    best + sqrt(.Machine$double.eps)
  if (all(is.finite(output$tradeoff_proxy))) {
    output$pareto_optimal <- pareto_nondominated(
      performance_loss,
      output$tradeoff_proxy
    )
  } else {
    output$pareto_optimal <- rep(NA, nrow(output))
  }
  cards <- lapply(output$family, behavior_card)
  for (field in c(
    "behavior_source", "nonlinearity", "interactions", "scaling",
    "strengths", "cautions"
  )) {
    output[[field]] <- vapply(cards, `[[`, character(1), field)
  }
  attr(output, "performance_metric") <- metric
  attr(output, "higher_is_better") <- higher_is_better
  attr(output, "tradeoff_metric") <- tradeoff$metric
  attr(output, "tradeoff_kind") <- tradeoff$kind
  class(output) <- c("autoxplain_behavior_models", "data.frame")
  output
}

behavior_leaderboard_value <- function(leaderboard, index, column, fallback) {
  if (!column %in% names(leaderboard)) return(fallback)
  value <- as.character(leaderboard[[column]][index])
  missing <- is.na(value) | !nzchar(value)
  value[missing] <- fallback[missing]
  value
}

behavior_model_family <- function(model, leaderboard, index) {
  if ("family" %in% names(leaderboard)) {
    family <- as.character(leaderboard$family[[index]])
    if (!is.na(family) && nzchar(family)) return(family)
  }
  algorithm <- behavior_h2o_algorithm(model)
  if (!is.na(algorithm)) {
    mapping <- c(
      glm = "linear",
      gbm = "boosting",
      xgboost = "boosting",
      drf = "forest",
      deeplearning = "neural",
      stackedensemble = "ensemble"
    )
    mapped <- unname(mapping[algorithm])
    return(if (is.na(mapped)) algorithm else mapped)
  }
  model_family_name(model)
}

behavior_model_backend <- function(model, leaderboard, index) {
  if ("backend" %in% names(leaderboard)) {
    backend <- as.character(leaderboard$backend[[index]])
    if (!is.na(backend) && nzchar(backend)) return(backend)
  }
  algorithm <- behavior_h2o_algorithm(model)
  if (!is.na(algorithm)) return(paste0("h2o/", algorithm))
  model_backend_name(model)
}

behavior_h2o_algorithm <- function(model) {
  if (!inherits(model, "H2OModel")) return(NA_character_)
  tolower(tryCatch(
    as.character(model@algorithm),
    error = function(error) NA_character_
  ))
}

behavior_card <- function(family) {
  registry <- autoxplain_learner_registry()
  if (family %in% names(registry)) {
    definition <- registry[[family]]
    return(list(
      behavior_source = "AutoXplainR learner registry",
      nonlinearity = definition$nonlinearity,
      interactions = definition$interactions,
      scaling = definition$scaling,
      strengths = definition$strengths,
      cautions = definition$cautions
    ))
  }
  if (identical(family, "baseline")) {
    return(list(
      behavior_source = "AutoXplainR baseline contract",
      nonlinearity = "none",
      interactions = "none",
      scaling = "not applicable",
      strengths = "Shows the performance available without predictor information.",
      cautions = "It is a reference point, not a competitive predictive model."
    ))
  }
  list(
    behavior_source = "unregistered family",
    nonlinearity = "not described by the learner registry",
    interactions = "not described by the learner registry",
    scaling = "inspect the backend documentation",
    strengths = "No reviewed AutoXplainR behavior card is available.",
    cautions = "Do not infer model behavior from its name alone."
  )
}

behavior_tradeoff_proxy <- function(model_ids, leaderboard) {
  index <- match(model_ids, leaderboard$model_id)
  preferred <- c(
    "model_size_kb", "size_mb", "model_size", "training_time_ms",
    "training_time_s", "prediction_time_ms", "complexity"
  )
  candidates <- intersect(preferred, names(leaderboard))
  if (!length(candidates)) {
    return(list(
      metric = NA_character_,
      values = rep(NA_real_, length(model_ids)),
      kind = "unavailable"
    ))
  }
  finite_counts <- vapply(candidates, function(column) {
    sum(is.finite(as.numeric(leaderboard[[column]][index])))
  }, integer(1))
  if (max(finite_counts) == 0L) {
    return(list(
      metric = NA_character_,
      values = rep(NA_real_, length(model_ids)),
      kind = "unavailable"
    ))
  }
  metric <- candidates[[which.max(finite_counts)]]
  values <- as.numeric(leaderboard[[metric]][index])
  list(
    metric = metric,
    values = values,
    kind = behavior_tradeoff_kind(metric)
  )
}

behavior_tradeoff_kind <- function(metric) {
  resource_metrics <- c(
    "model_size_kb", "size_mb", "model_size", "training_time_ms",
    "training_time_s", "prediction_time_ms"
  )
  if (metric %in% resource_metrics) return("resource proxy")
  if (identical(metric, "complexity")) return("structural complexity proxy")
  "secondary trade-off proxy"
}

behavior_prediction_pairs <- function(predictions, reference, model_table) {
  pair_indices <- utils::combn(seq_along(predictions), 2L)
  rows <- lapply(seq_len(ncol(pair_indices)), function(column) {
    pair <- pair_indices[, column]
    first <- predictions[[pair[[1L]]]]
    second <- predictions[[pair[[2L]]]]
    distance <- behavior_prediction_distance(
      first,
      second,
      reference$task,
      reference$class_levels
    )
    hard_disagreement <- behavior_hard_disagreement(
      first,
      second,
      reference$task,
      reference$class_levels,
      reference$positive
    )
    correlations <- behavior_prediction_correlations(
      first,
      second,
      reference$task,
      reference$class_levels
    )
    row <- data.frame(
      model_a = names(predictions)[[pair[[1L]]]],
      family_a = model_table$family[[pair[[1L]]]],
      model_b = names(predictions)[[pair[[2L]]]],
      family_b = model_table$family[[pair[[2L]]]],
      class_disagreement_rate = hard_disagreement,
      mean_prediction_distance = mean(distance),
      median_prediction_distance = stats::median(distance),
      p90_prediction_distance = as.numeric(stats::quantile(
        distance,
        0.9,
        names = FALSE
      )),
      max_prediction_distance = max(distance),
      prediction_rank_correlation = correlations$overall,
      mean_class_rank_correlation = correlations$mean_class,
      distance_definition = behavior_distance_definition(reference$task),
      stringsAsFactors = FALSE
    )
    row$per_class_rank_correlations <- I(list(correlations$per_class))
    row
  })
  output <- do.call(rbind, rows)
  output <- output[order(
    -output$mean_prediction_distance,
    output$model_a,
    output$model_b
  ), , drop = FALSE]
  rownames(output) <- NULL
  output
}

behavior_prediction_distance <- function(first, second, task, class_levels) {
  if (task %in% c("regression", "binary")) {
    return(abs(as.numeric(first) - as.numeric(second)))
  }
  first <- as.matrix(first)[, class_levels, drop = FALSE]
  second <- as.matrix(second)[, class_levels, drop = FALSE]
  0.5 * rowSums(abs(first - second))
}

behavior_hard_disagreement <- function(first,
                                       second,
                                       task,
                                       class_levels,
                                       positive) {
  if (identical(task, "regression")) return(NA_real_)
  if (identical(task, "binary")) {
    negative <- setdiff(class_levels, positive)[[1L]]
    first_class <- ifelse(as.numeric(first) >= 0.5, positive, negative)
    second_class <- ifelse(as.numeric(second) >= 0.5, positive, negative)
  } else {
    first <- as.matrix(first)[, class_levels, drop = FALSE]
    second <- as.matrix(second)[, class_levels, drop = FALSE]
    first_class <- class_levels[max.col(first, ties.method = "first")]
    second_class <- class_levels[max.col(second, ties.method = "first")]
  }
  mean(first_class != second_class)
}

behavior_distance_definition <- function(task) {
  if (identical(task, "regression")) {
    return("absolute difference in predicted target units")
  }
  if (identical(task, "binary")) {
    return("absolute difference in positive-class probability")
  }
  "total-variation distance between class-probability vectors"
}

behavior_safe_spearman <- function(first, second) {
  if (length(first) < 2L || length(unique(first)) < 2L ||
        length(unique(second)) < 2L) {
    return(NA_real_)
  }
  suppressWarnings(stats::cor(first, second, method = "spearman"))
}

behavior_prediction_correlations <- function(first, second, task, class_levels) {
  if (!identical(task, "multiclass")) {
    return(list(
      overall = behavior_safe_spearman(as.numeric(first), as.numeric(second)),
      mean_class = NA_real_,
      per_class = numeric()
    ))
  }
  first <- as.matrix(first)[, class_levels, drop = FALSE]
  second <- as.matrix(second)[, class_levels, drop = FALSE]
  per_class <- vapply(class_levels, function(class) {
    behavior_safe_spearman(first[, class], second[, class])
  }, numeric(1))
  finite <- is.finite(per_class)
  list(
    overall = NA_real_,
    mean_class = if (any(finite)) mean(per_class[finite]) else NA_real_,
    per_class = per_class
  )
}

behavior_feature_evidence <- function(audit, model_ids, explainers) {
  if (is.null(audit)) return(NULL)
  if (!inherits(audit, "autoxplain_audit")) {
    stop("`explanation_audit` must be returned by `audit_explanations()`.",
         call. = FALSE)
  }
  objects <- audit$importance_objects
  missing <- setdiff(model_ids, names(objects))
  if (length(missing)) {
    stop(
      "`explanation_audit` does not cover selected model IDs: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  expected_fingerprints <- vapply(
    explainers,
    function(explainer) explainer$provenance$fingerprint,
    character(1)
  )
  observed_fingerprints <- audit$provenance$explainer_fingerprints[model_ids]
  if (length(observed_fingerprints) != length(model_ids) ||
        anyNA(observed_fingerprints) ||
        !identical(unname(observed_fingerprints), unname(expected_fingerprints))) {
    stop(
      "`explanation_audit` was not made from the same selected model explainers.",
      call. = FALSE
    )
  }
  objects <- objects[model_ids]
  features <- Reduce(intersect, lapply(objects, function(object) object$feature))
  if (!length(features)) {
    stop("`explanation_audit` has no feature shared by every selected model.",
         call. = FALSE)
  }
  agreement <- explanation_agreement(
    objects,
    rep(TRUE, length(objects)),
    features
  )
  importance <- audit$importance[
    audit$importance$model %in% model_ids &
      audit$importance$feature %in% features,
    ,
    drop = FALSE
  ]
  importance$model <- factor(importance$model, levels = model_ids)
  top_rows <- lapply(model_ids, function(model_id) {
    item <- importance[importance$model == model_id, , drop = FALSE]
    item <- item[order(-item$importance, item$feature), , drop = FALSE]
    item$rank_within_model <- rank(-item$importance, ties.method = "min")
    item <- item[item$rank_within_model <= 5L, , drop = FALSE]
    item
  })
  top_features <- do.call(rbind, top_rows)
  top_features$model <- as.character(top_features$model)
  keep <- intersect(
    c(
      "model", "rank_within_model", "feature", "importance", "std_error",
      "conf_low", "conf_high", "sign_stability", "evidence_grade", "claim",
      "max_association", "associated_feature"
    ),
    names(top_features)
  )
  top_features <- top_features[keep]
  rownames(top_features) <- NULL
  rank_agreement <- behavior_rank_agreement_table(
    agreement$rank_correlation,
    model_ids
  )
  ranges <- agreement$importance_ranges
  ranges$importance_range <- ranges$max_importance - ranges$min_importance
  ranges <- ranges[order(-ranges$importance_range, ranges$feature), , drop = FALSE]
  rownames(ranges) <- NULL
  list(
    metric = audit$config$metric,
    n_repeats = audit$config$n_repeats,
    confidence = audit$config$confidence,
    top_features = top_features,
    rank_agreement = rank_agreement,
    feature_ranges = ranges,
    mean_rank_agreement = agreement$mean_rank_correlation,
    scope_note = paste(
      "Repeated permutation importance describes model reliance on the supplied",
      "evaluation rows. Its intervals quantify permutation Monte Carlo variation,",
      "not population uncertainty; correlated features can share reliance."
    )
  )
}

behavior_rank_agreement_table <- function(correlation, model_ids) {
  pairs <- utils::combn(model_ids, 2L)
  rows <- lapply(seq_len(ncol(pairs)), function(index) {
    model_a <- pairs[[1L, index]]
    model_b <- pairs[[2L, index]]
    data.frame(
      model_a = model_a,
      model_b = model_b,
      spearman_rank_agreement = correlation[model_a, model_b],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

behavior_findings <- function(models, prediction_pairs, feature_evidence) {
  best <- models[models$best_performance, , drop = FALSE]
  best_message <- if (nrow(best) == 1L) {
    paste0(
      best$model_id[[1L]], " has the best supplied ",
      attr(models, "performance_metric"), " score (",
      format(best$performance_score[[1L]], digits = 4L), ")."
    )
  } else {
    paste0(
      "The best supplied ", attr(models, "performance_metric"),
      " score is tied by ", paste(best$model_id, collapse = ", "), "."
    )
  }
  widest <- prediction_pairs[1L, , drop = FALSE]
  gap_message <- paste0(
    widest$model_a, " and ", widest$model_b,
    " differ most on average (",
    format(widest$mean_prediction_distance, digits = 3L), " using ",
    widest$distance_definition, ")."
  )
  structural <- behavior_structural_contrast(models)
  messages <- c(best_message, gap_message, structural)
  topics <- c("performance", "prediction disagreement", "model capacity")
  evidence_kind <- c("computed", "computed", "prior knowledge")
  if (!is.null(feature_evidence)) {
    weakest <- feature_evidence$rank_agreement[
      which.min(feature_evidence$rank_agreement$spearman_rank_agreement),
      ,
      drop = FALSE
    ]
    feature_message <- if (
      nrow(weakest) &&
        is.finite(weakest$spearman_rank_agreement[[1L]])
    ) {
      paste0(
        weakest$model_a, " and ", weakest$model_b,
        " have the least similar permutation-importance ranks (Spearman ",
        format(weakest$spearman_rank_agreement[[1L]], digits = 3L), ")."
      )
    } else {
      paste(
        "The supplied audit could not estimate a finite cross-model",
        "feature-rank agreement."
      )
    }
    messages <- c(messages, feature_message)
    topics <- c(topics, "feature reliance")
    evidence_kind <- c(evidence_kind, "computed")
  }
  data.frame(
    topic = topics,
    evidence_kind = evidence_kind,
    message = messages,
    stringsAsFactors = FALSE
  )
}

behavior_structural_contrast <- function(models) {
  pair_indices <- utils::combn(seq_len(nrow(models)), 2L)
  difference_count <- apply(pair_indices, 2L, function(pair) {
    sum(c(
      models$nonlinearity[[pair[[1L]]]] != models$nonlinearity[[pair[[2L]]]],
      models$interactions[[pair[[1L]]]] != models$interactions[[pair[[2L]]]],
      models$scaling[[pair[[1L]]]] != models$scaling[[pair[[2L]]]]
    ))
  })
  pair <- pair_indices[, which.max(difference_count)]
  first <- models[pair[[1L]], , drop = FALSE]
  second <- models[pair[[2L]], , drop = FALSE]
  paste0(
    "Before considering this dataset, ", first$model_id, " allows ",
    first$nonlinearity, " with ", first$interactions, "; ",
    second$model_id, " allows ", second$nonlinearity, " with ",
    second$interactions, "."
  )
}

behavior_summary <- function(models,
                             prediction_pairs,
                             ambiguity,
                             feature_evidence) {
  best <- models$model_id[models$best_performance]
  widest <- prediction_pairs[1L, , drop = FALSE]
  list(
    task = ambiguity$task,
    n_models = nrow(models),
    n_evaluation_rows = nrow(ambiguity$rows),
    evaluation_role = ambiguity$evaluation_role,
    performance_metric = ambiguity$performance_metric,
    best_performance_models = best,
    tradeoff_metric = attr(models, "tradeoff_metric"),
    tradeoff_kind = attr(models, "tradeoff_kind"),
    pareto_models = models$model_id[models$pareto_optimal %in% TRUE],
    largest_disagreement_pair = c(widest$model_a, widest$model_b),
    largest_mean_prediction_distance = widest$mean_prediction_distance,
    distance_definition = widest$distance_definition,
    feature_rank_agreement = if (is.null(feature_evidence)) {
      NA_real_
    } else {
      feature_evidence$mean_rank_agreement
    },
    scope_note = paste(
      "Descriptive comparison of supplied fitted models on common evaluation",
      "rows; not causal inference, uncertainty coverage, or a deployment rule."
    )
  )
}
