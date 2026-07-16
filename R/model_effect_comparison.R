#' Compare one fitted feature effect across retained models
#'
#' `compare_model_effects()` evaluates the same feature, prediction target,
#' evaluation rows, and effect grid for every selected model. It is the
#' feature-level companion to [compare_model_behavior()]: use it when the
#' question is not only whether models predict differently, but how their
#' fitted response to a named input differs.
#'
#' ALE remains the default for numeric features. Categorical features require
#' `method = "pdp"`. Comparisons are restricted to grid points meeting
#' `min_support` for both models. PDP dependence diagnostics remain attached;
#' curves are descriptive properties of fitted prediction functions, not
#' intervention effects or population confidence bands.
#'
#' Pairwise columns have deliberately narrow meanings:
#'
#' * `overlap` is the supported numeric grid span or supported categorical
#'   levels shared by the pair; `n_overlap` is its number of grid points.
#' * `mean_absolute_level_gap` is the mean absolute prediction-scale gap and
#'   therefore includes vertical offsets between curves.
#' * `centered_shape_rmse` is the root mean squared gap after centering each
#'   curve on its own supported mean. It describes shape disagreement and is
#'   undefined with fewer than two shared points.
#' * `max_absolute_gap` is the largest prediction-scale gap.
#' * `effect_rank_correlation` is Spearman correlation between paired effect
#'   values. `direction_agreement` is the fraction of adjacent numeric grid
#'   intervals in which the two curves change in the same direction.
#'
#' Effect bands are descriptive, conditional on each already-fitted model.
#' They summarize across-row variation for PDP or propagated within-bin local
#' difference variation for ALE; they do not include model-fitting uncertainty.
#'
#' @param result An `autoxplain_result` containing at least two retained,
#'   non-baseline models.
#' @param feature One predictor name shared by the retained explainers.
#' @param models Model IDs, indices, or `NULL`. `NULL` selects retained
#'   non-baseline models.
#' @param method `"ale"` or `"pdp"`.
#' @param class For multiclass results, the probability class to compare.
#'   Defaults to the first outcome level.
#' @param n_points Number of ALE bins or PDP grid points.
#' @param quantile_range Numeric length-two range for a numeric PDP grid.
#' @param sample_size Maximum common evaluation rows used for PDP. `NULL` uses
#'   all rows.
#' @param seed Reproducible PDP sampling seed.
#' @param min_support Minimum relative empirical support, from zero to one, for
#'   a grid point to enter pairwise summaries.
#' @param performance_tolerance Optional non-negative relative performance gap
#'   from the best selected model.
#'
#' @return An `autoxplain_model_effects` object containing aligned `curves`,
#'   pairwise `comparisons`, model performance context, failures, findings, and
#'   interpretation scope. `n_evaluation_rows` counts the common evaluation
#'   set; `n_effect_rows` counts rows that actually entered each effect estimate
#'   after reproducible PDP sampling or ALE missing-value exclusion. `models`
#'   retains every requested model with `effect_status` and `effect_error`.
#' @export
#'
#' @examples
#' fit <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 2026)
#' compare_model_effects(fit, "wt")
compare_model_effects <- function(result,
                                  feature,
                                  models = NULL,
                                  method = c("ale", "pdp"),
                                  class = NULL,
                                  n_points = 20L,
                                  quantile_range = c(0.05, 0.95),
                                  sample_size = 1000L,
                                  seed = 123L,
                                  min_support = 0.1,
                                  performance_tolerance = NULL) {
  if (!inherits(result, "autoxplain_result")) {
    stop("`result` must be returned by `autoxplain()`.", call. = FALSE)
  }
  if (!is.character(feature) || length(feature) != 1L || is.na(feature) ||
        !nzchar(feature)) {
    stop("`feature` must be one non-empty predictor name.", call. = FALSE)
  }
  method <- match.arg(method)
  n_points <- assert_count(n_points, "n_points", minimum = 2L)
  validate_quantile_range(quantile_range)
  if (!is.null(sample_size)) {
    sample_size <- assert_count(sample_size, "sample_size", minimum = 2L)
  }
  seed <- assert_count(seed, "seed", minimum = 0L)
  assert_probability(min_support, "min_support")

  ambiguity <- prediction_ambiguity(
    result,
    models = models,
    performance_tolerance = performance_tolerance
  )
  explainers <- as_explainers(result, models = ambiguity$model_ids)
  missing <- names(explainers)[!vapply(explainers, function(explainer) {
    feature %in% names(explainer$data)
  }, logical(1))]
  if (length(missing)) {
    stop(
      "`feature` is unavailable for selected models: ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
  comparison_assert_common_effect_rows(explainers, feature)
  if (identical(method, "ale") && !is.numeric(explainers[[1L]]$data[[feature]])) {
    stop(
      "ALE supports numeric features. Use `method = \"pdp\"` to compare a ",
      "categorical or logical feature.",
      call. = FALSE
    )
  }
  if (!identical(result$task, "multiclass") && !is.null(class)) {
    stop("`class` is only used for multiclass probability effects.", call. = FALSE)
  }
  if (identical(result$task, "multiclass")) {
    class <- class %||% explainers[[1L]]$class_levels[[1L]]
    if (!is.character(class) || length(class) != 1L || is.na(class) ||
          !class %in% explainers[[1L]]$class_levels) {
      stop(
        "`class` must name one multiclass outcome level: ",
        paste(explainers[[1L]]$class_levels, collapse = ", "), ".",
        call. = FALSE
      )
    }
  }

  attempted <- lapply(names(explainers), function(model_id) {
    tryCatch(
      list(
        effect = explain_effect(
          explainers[[model_id]],
          feature = feature,
          method = method,
          n_points = n_points,
          quantile_range = quantile_range,
          sample_size = sample_size,
          seed = seed,
          class = class
        ),
        error = ""
      ),
      error = function(condition) list(effect = NULL, error = conditionMessage(condition))
    )
  })
  names(attempted) <- names(explainers)
  successful <- names(attempted)[vapply(attempted, function(item) {
    inherits(item$effect, "autoxplain_effect")
  }, logical(1))]
  failed <- setdiff(names(attempted), successful)
  failures <- data.frame(
    model_id = failed,
    error = vapply(attempted[failed], `[[`, character(1), "error"),
    stringsAsFactors = FALSE
  )
  if (length(successful) < 2L) {
    details <- if (nrow(failures)) {
      paste0(" Failures: ", paste0(failures$model_id, ": ", failures$error,
                                   collapse = " | "))
    } else {
      ""
    }
    stop(
      "At least two selected models must produce a comparable feature effect.",
      details,
      call. = FALSE
    )
  }
  effects <- lapply(attempted[successful], `[[`, "effect")
  comparison_assert_aligned_effects(effects, feature)

  model_table <- behavior_model_table(
    result,
    ambiguity$model_ids,
    ambiguity
  )
  model_table <- comparison_effect_model_status(
    model_table,
    successful,
    failures,
    ambiguity$performance_metric
  )
  comparison_models <- model_table[
    match(successful, model_table$model_id),
    ,
    drop = FALSE
  ]
  curves <- comparison_effect_curves(effects, comparison_models, feature, min_support)
  comparisons <- comparison_effect_pairs(curves, feature, method, min_support)
  prediction_target <- unique(vapply(effects, function(effect) {
    attr(effect, "prediction_target")
  }, character(1)))
  if (length(prediction_target) != 1L) {
    stop("Selected effects do not share one prediction target.", call. = FALSE)
  }
  dependence_warning <- any(vapply(effects, function(effect) {
    isTRUE(attr(effect, "dependence_warning"))
  }, logical(1)))
  findings <- comparison_effect_findings(
    comparisons,
    failures,
    dependence_warning,
    method,
    min_support
  )
  role <- result$provenance$evaluation_role %||% "unspecified evaluation"
  structure(
    list(
      feature = feature,
      method = method,
      task = result$task,
      prediction_target = prediction_target,
      prediction_class = if (identical(result$task, "multiclass")) class else NULL,
      requested_model_ids = ambiguity$model_ids,
      model_ids = successful,
      n_models = length(successful),
      n_evaluation_rows = nrow(explainers[[1L]]$data),
      n_effect_rows = as.integer(attr(effects[[1L]], "n_reference")),
      evaluation_role = role,
      performance_metric = ambiguity$performance_metric,
      min_support = min_support,
      models = model_table,
      curves = curves,
      comparisons = comparisons,
      effects = effects,
      failures = failures,
      findings = findings,
      dependence_warning = dependence_warning,
      scope = list(
        alignment = comparison_effect_alignment_scope(
          method,
          role,
          nrow(explainers[[1L]]$data),
          as.integer(attr(effects[[1L]], "n_reference"))
        ),
        support = paste0(
          "Relative support uses the common ", nrow(explainers[[1L]]$data),
          "-row evaluation feature distribution. Pairwise summaries use only grid ",
          "points where both curves have relative ",
          "support >= ", format(min_support, trim = TRUE), "."
        ),
        dependence = if (dependence_warning) {
          paste(
            "At least one marginal PDP has a strong predictor-association warning;",
            "prefer ALE where supported and do not interpret the PDP causally."
          )
        } else {
          "No strong predictor-association warning was triggered for these curves."
        },
        intervals = attr(effects[[1L]], "interval_note"),
        inference = paste(
          "Differences describe supplied fitted models on supplied evaluation rows.",
          "They are not causal effects, confidence intervals, or population inference."
        )
      )
    ),
    class = c("autoxplain_model_effects", "list")
  )
}

comparison_effect_model_status <- function(models,
                                           successful,
                                           failures,
                                           performance_metric) {
  models$effect_status <- ifelse(
    models$model_id %in% successful,
    "included",
    "failed"
  )
  models$effect_error <- ""
  if (nrow(failures)) {
    index <- match(failures$model_id, models$model_id)
    models$effect_error[index] <- failures$error
  }
  models$best_performance_before_effects <- models$best_performance
  models$best_performance <- FALSE
  models$effect_performance_rank <- NA_integer_
  included <- which(models$effect_status == "included")
  score <- models$performance_score[included]
  loss <- if (performance_metric %in% higher_is_better_metrics()) -score else score
  models$effect_performance_rank[included] <- rank(loss, ties.method = "min")
  best <- min(loss)
  models$best_performance[included] <- loss <= best + sqrt(.Machine$double.eps)
  models
}

comparison_effect_alignment_scope <- function(method,
                                              role,
                                              n_evaluation_rows,
                                              n_effect_rows) {
  if (identical(method, "pdp") && n_effect_rows < n_evaluation_rows) {
    return(paste(
      "Every curve uses the same reproducible sample of", n_effect_rows,
      "rows drawn from the same", n_evaluation_rows, role,
      "rows, plus the same prediction target, grid, and sampling seed."
    ))
  }
  if (n_effect_rows < n_evaluation_rows) {
    return(paste(
      "Every curve uses the same", n_effect_rows, "rows remaining after method-specific",
      "missing-value exclusion from the same", n_evaluation_rows, role,
      "rows, plus the same prediction target and grid."
    ))
  }
  paste(
    "Every curve uses the same", n_effect_rows, role,
    "rows, prediction target, and grid."
  )
}

comparison_assert_common_effect_rows <- function(explainers, feature) {
  reference <- explainers[[1L]]$data
  valid <- vapply(explainers[-1L], function(explainer) {
    identical(explainer$data, reference) &&
      identical(explainer$y, explainers[[1L]]$y)
  }, logical(1))
  if (length(valid) && !all(valid)) {
    stop(
      "Selected explainers must share identical evaluation rows, including `",
      feature,
      "` values.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

comparison_assert_aligned_effects <- function(effects, feature) {
  valid_reference_rows <- vapply(effects, function(effect) {
    rows <- attr(effect, "reference_rows")
    count <- attr(effect, "n_reference")
    length(count) == 1L && is.finite(count) && count >= 0 &&
      length(rows) == count && !anyDuplicated(rows)
  }, logical(1))
  if (!all(valid_reference_rows)) {
    stop(
      "Each effect must record distinct reference-row positions matching its row count.",
      call. = FALSE
    )
  }
  reference <- effects[[1L]][[feature]]
  aligned_grid <- vapply(effects[-1L], function(effect) {
    identical(effect[[feature]], reference)
  }, logical(1))
  if (length(aligned_grid) && !all(aligned_grid)) {
    stop(
      "Effect grids did not align despite using common evaluation rows.",
      call. = FALSE
    )
  }
  attributes <- c(
    "method", "feature", "task", "prediction_target", "prediction_class",
    "n_reference", "reference_rows", "interval_note", "support_note"
  )
  aligned_attributes <- vapply(effects[-1L], function(effect) {
    all(vapply(attributes, function(attribute) {
      identical(attr(effect, attribute), attr(effects[[1L]], attribute))
    }, logical(1)))
  }, logical(1))
  if (length(aligned_attributes) && !all(aligned_attributes)) {
    stop(
      "Effects did not use the same method, target, or reference rows.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

comparison_effect_curves <- function(effects, models, feature, min_support) {
  rows <- lapply(seq_along(effects), function(index) {
    effect <- effects[[index]]
    method <- attr(effect, "method")
    value_column <- if (identical(method, "ale")) {
      "accumulated_effect"
    } else {
      "partial_dependence"
    }
    output <- data.frame(
      model_id = rep(models$model_id[[index]], nrow(effect)),
      family = rep(models$family[[index]], nrow(effect)),
      backend = rep(models$backend[[index]], nrow(effect)),
      grid_index = seq_len(nrow(effect)),
      effect_value = as.numeric(effect[[value_column]]),
      std_error = as.numeric(effect$std_error),
      conf_low = as.numeric(effect$conf_low),
      conf_high = as.numeric(effect$conf_high),
      support = as.numeric(effect$support),
      supported = as.numeric(effect$support) >= min_support,
      dependence_warning = rep(isTRUE(attr(effect, "dependence_warning")), nrow(effect)),
      stringsAsFactors = FALSE
    )
    output$feature_value <- effect[[feature]]
    output
  })
  output <- do.call(rbind, rows)
  rownames(output) <- NULL
  output[c(
    "model_id", "family", "backend", "grid_index", "feature_value",
    "effect_value", "std_error", "conf_low", "conf_high", "support",
    "supported", "dependence_warning"
  )]
}

comparison_effect_pairs <- function(curves, feature, method, min_support) {
  model_ids <- unique(curves$model_id)
  pairs <- utils::combn(model_ids, 2L)
  rows <- lapply(seq_len(ncol(pairs)), function(index) {
    model_a <- pairs[[1L, index]]
    model_b <- pairs[[2L, index]]
    first <- curves[curves$model_id == model_a, , drop = FALSE]
    second <- curves[curves$model_id == model_b, , drop = FALSE]
    first <- first[order(first$grid_index), , drop = FALSE]
    second <- second[order(second$grid_index), , drop = FALSE]
    supported <- first$supported & second$supported &
      is.finite(first$effect_value) & is.finite(second$effect_value)
    first_value <- first$effect_value[supported]
    second_value <- second$effect_value[supported]
    feature_value <- first$feature_value[supported]
    differences <- first_value - second_value
    numeric_feature <- is.numeric(feature_value)
    data.frame(
      model_a = model_a,
      family_a = first$family[[1L]],
      model_b = model_b,
      family_b = second$family[[1L]],
      n_overlap = sum(supported),
      overlap = comparison_effect_overlap(feature_value),
      mean_absolute_level_gap = comparison_safe_mean(abs(differences)),
      centered_shape_rmse = comparison_centered_rmse(first_value, second_value),
      max_absolute_gap = comparison_safe_max(abs(differences)),
      effect_rank_correlation = behavior_safe_spearman(first_value, second_value),
      direction_agreement = if (numeric_feature) {
        comparison_direction_agreement(feature_value, first_value, second_value)
      } else {
        NA_real_
      },
      method = method,
      support_threshold = min_support,
      dependence_warning = any(first$dependence_warning | second$dependence_warning),
      stringsAsFactors = FALSE
    )
  })
  output <- do.call(rbind, rows)
  output <- output[order(
    -output$centered_shape_rmse,
    output$model_a,
    output$model_b,
    na.last = TRUE
  ), , drop = FALSE]
  rownames(output) <- NULL
  attr(output, "feature") <- feature
  output
}

comparison_effect_overlap <- function(values) {
  if (!length(values)) return("none")
  if (is.numeric(values)) {
    limits <- range(values, na.rm = TRUE)
    return(paste0(
      "[", format(limits[[1L]], digits = 5L, trim = TRUE), ", ",
      format(limits[[2L]], digits = 5L, trim = TRUE), "]"
    ))
  }
  paste(as.character(values), collapse = ", ")
}

comparison_safe_mean <- function(values) {
  if (!length(values)) NA_real_ else mean(values)
}

comparison_safe_max <- function(values) {
  if (!length(values)) NA_real_ else max(values)
}

comparison_centered_rmse <- function(first, second) {
  if (length(first) < 2L || length(second) < 2L) return(NA_real_)
  first <- first - mean(first)
  second <- second - mean(second)
  sqrt(mean((first - second)^2))
}

comparison_direction_agreement <- function(x, first, second) {
  if (length(x) < 2L) return(NA_real_)
  ordering <- order(x)
  first_change <- diff(first[ordering])
  second_change <- diff(second[ordering])
  tolerance <- sqrt(.Machine$double.eps)
  informative <- abs(first_change) > tolerance | abs(second_change) > tolerance
  if (!any(informative)) return(NA_real_)
  mean(sign(first_change[informative]) == sign(second_change[informative]))
}

comparison_effect_findings <- function(comparisons,
                                       failures,
                                       dependence_warning,
                                       method,
                                       min_support) {
  messages <- list()
  insufficient <- which(comparisons$n_overlap < 2L)
  if (length(insufficient)) {
    labels <- paste0(
      "`", comparisons$model_a[insufficient], "` vs `",
      comparisons$model_b[insufficient], "` (n = ",
      comparisons$n_overlap[insufficient], ")"
    )
    messages[[length(messages) + 1L]] <- data.frame(
      severity = "warning",
      code = "insufficient_overlap",
      message = paste0(
        "Shape disagreement is unavailable with fewer than two supported grid ",
        "points: ", paste(labels, collapse = "; "), "."
      ),
      stringsAsFactors = FALSE
    )
  }
  finite <- which(is.finite(comparisons$centered_shape_rmse))
  if (length(finite)) {
    index <- finite[[which.max(comparisons$centered_shape_rmse[finite])]]
    row <- comparisons[index, , drop = FALSE]
    messages[[length(messages) + 1L]] <- data.frame(
      severity = "note",
      code = "largest_shape_gap",
      message = paste0(
        "The largest centered curve difference was between `", row$model_a,
        "` and `", row$model_b, "` (RMSE = ",
        format(signif(row$centered_shape_rmse, 4L), trim = TRUE),
        ") across ", row$n_overlap, " supported grid points."
      ),
      stringsAsFactors = FALSE
    )
  }
  if (dependence_warning && identical(method, "pdp")) {
    messages[[length(messages) + 1L]] <- data.frame(
      severity = "warning",
      code = "pdp_dependence",
      message = paste(
        "Strong predictor association can make the marginal PDP compare",
        "unrealistic combinations; inspect ALE where numeric and supported."
      ),
      stringsAsFactors = FALSE
    )
  }
  if (nrow(failures)) {
    messages[[length(messages) + 1L]] <- data.frame(
      severity = "warning",
      code = "effect_failures",
      message = paste0(
        "No comparable curve was produced for: ",
        paste(failures$model_id, collapse = ", "), "."
      ),
      stringsAsFactors = FALSE
    )
  }
  messages[[length(messages) + 1L]] <- data.frame(
    severity = "boundary",
    code = "descriptive_scope",
    message = paste0(
      "Pairwise summaries exclude grid points below relative support ",
      format(min_support, trim = TRUE),
      " and describe fitted predictions, not causal effects."
    ),
    stringsAsFactors = FALSE
  )
  do.call(rbind, messages)
}

#' @export
print.autoxplain_model_effects <- function(x, ...) {
  cat("<AutoXplainR cross-model effect comparison>\n")
  cat("  feature:   ", x$feature, " (", toupper(x$method), ")\n", sep = "")
  cat("  target:    ", x$prediction_target, "\n", sep = "")
  cat(
    "  models:    ", x$n_models, " of ", length(x$requested_model_ids),
    " requested (", paste(x$model_ids, collapse = ", "), ")\n",
    sep = ""
  )
  cat(
    "  evidence:  ", x$n_effect_rows, " effect rows from ",
    x$n_evaluation_rows, " ", x$evaluation_role, " rows\n",
    sep = ""
  )
  cat("  support:   relative support >= ", format(x$min_support, trim = TRUE), "\n",
      sep = "")
  if (nrow(x$findings)) {
    cat("\nFindings\n")
    for (index in seq_len(nrow(x$findings))) {
      cat(
        "  [", x$findings$severity[[index]], "] ",
        x$findings$message[[index]], "\n",
        sep = ""
      )
    }
  }
  cat("\nPairwise summary\n")
  print.data.frame(comparison_effect_print_table(x$comparisons), row.names = FALSE, ...)
  cat("  bands:     ", x$scope$intervals, "\n", sep = "")
  cat("  caution:   fitted-curve disagreement is descriptive, not causal inference\n")
  invisible(x)
}

comparison_effect_print_table <- function(comparisons) {
  output <- comparisons[c(
    "model_a", "model_b", "n_overlap", "mean_absolute_level_gap",
    "centered_shape_rmse", "max_absolute_gap", "direction_agreement"
  )]
  numeric <- vapply(output, is.numeric, logical(1))
  output[numeric] <- lapply(output[numeric], function(column) signif(column, 4L))
  names(output) <- c(
    "model A", "model B", "points", "mean level gap", "shape RMSE",
    "max gap", "direction agreement"
  )
  output
}

#' Plot aligned cross-model effects
#'
#' @param x An `autoxplain_model_effects` object.
#' @param ... Additional arguments passed to [graphics::matplot()].
#'
#' @details Grid points below `x$min_support` are omitted rather than joined by
#'   a visually authoritative line. Plot styles supplied through `...` replace
#'   defaults and are reused in the legend.
#'
#' @return `x`, invisibly.
#' @export
plot.autoxplain_model_effects <- function(x, ...) {
  grid <- x$curves[x$curves$model_id == x$model_ids[[1L]], "feature_value"]
  supported <- vapply(x$model_ids, function(model_id) {
    rows <- x$curves[x$curves$model_id == model_id, , drop = FALSE]
    rows$supported[order(rows$grid_index)]
  }, logical(length(grid)))
  values <- vapply(x$model_ids, function(model_id) {
    rows <- x$curves[x$curves$model_id == model_id, , drop = FALSE]
    rows <- rows[order(rows$grid_index), , drop = FALSE]
    ifelse(rows$supported, rows$effect_value, NA_real_)
  }, numeric(length(grid)))
  if (!any(is.finite(values))) {
    stop(
      "No effect points meet `min_support`; lower the threshold before plotting.",
      call. = FALSE
    )
  }
  numeric_grid <- is.numeric(grid)
  x_values <- if (numeric_grid) grid else seq_along(grid)
  model_count <- length(x$model_ids)
  defaults <- list(
    x = x_values,
    y = values,
    type = "b",
    col = grDevices::hcl.colors(model_count, "Dark 3"),
    lty = seq_len(model_count),
    pch = seq_len(model_count),
    xlab = x$feature,
    ylab = x$prediction_target,
    xaxt = if (numeric_grid) "s" else "n"
  )
  arguments <- utils::modifyList(defaults, list(...), keep.null = TRUE)
  do.call(graphics::matplot, arguments)
  if (!numeric_grid && identical(arguments$xaxt %||% "s", "n")) {
    graphics::axis(1L, at = x_values, labels = as.character(grid))
  }
  if (any(!supported)) {
    graphics::mtext(
      "Points below the support threshold are omitted",
      side = 3L,
      line = 0.1,
      adj = 0,
      cex = 0.7
    )
  }
  colors <- rep_len(arguments$col %||% seq_len(model_count), model_count)
  line_types <- rep_len(arguments$lty %||% seq_len(model_count), model_count)
  point_types <- rep_len(arguments$pch %||% seq_len(model_count), model_count)
  graphics::legend(
    "topright",
    legend = x$model_ids,
    col = colors,
    lty = line_types,
    pch = point_types,
    bty = "n"
  )
  invisible(x)
}
