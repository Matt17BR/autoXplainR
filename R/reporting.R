#' Render a beginner-first model report
#'
#' Creates a standalone HTML report from an [autoxplain()] result. The report
#' leads with the prediction question, evaluation role, simple-baseline
#' comparison, and plain metric definitions. Feature reliance, fitted effects,
#' and an explanation evidence audit follow with progressively more detail.
#'
#' @param result An `autoxplain_result`.
#' @param output_file Destination `.html` path.
#' @param title Optional report title. `NULL` uses the target name.
#' @param target_units Optional unit label for a numeric target. Used in the
#'   analysis brief and effect captions; no units are inferred.
#' @param audit Optional precomputed `autoxplain_audit`.
#' @param effects Optional named list of feature-effect objects.
#' @param narrative Optional narrative returned by
#'   [generate_natural_language_report()].
#' @param subgroup Optional name of one categorical or low-cardinality column.
#'   When supplied, the report includes an explicit evaluation-set subgroup
#'   performance check. See [subgroup_performance()].
#' @param uncertainty Include [performance_uncertainty()] using its default paired
#'   bootstrap. Off by default; temporal evaluation is not supported.
#' @param open Open the report in a browser after writing it.
#' @param top_features Maximum number of features audited when `audit` is not
#'   supplied.
#' @param n_repeats Permutation repeats when `audit` is not supplied.
#' @param max_models Maximum models audited when `audit` is not supplied.
#'
#' @return The normalized output path, invisibly. Its `diagnostic_status`
#'   attribute records optional checks performed for this report. The input
#'   result is not changed by rendering.
#' @export
#'
#' @examples
#' result <- autoxplain(mtcars, "mpg", seed = 2026)
#' path <- tempfile(fileext = ".html")
#' render_model_report(result, path, n_repeats = 3)
#' unlink(path)
render_model_report <- function(result,
                                output_file = "autoxplain-report.html",
                                title = NULL,
                                audit = NULL,
                                effects = NULL,
                                narrative = NULL,
                                subgroup = NULL,
                                open = FALSE,
                                top_features = 8L,
                                n_repeats = 20L,
                                max_models = 5L,
                                uncertainty = FALSE,
                                target_units = NULL) {
  if (!inherits(result, "autoxplain_result")) {
    stop("`result` must be returned by `autoxplain()`.", call. = FALSE)
  }
  explicit_effects <- !is.null(effects)
  if (!is.null(target_units)) {
    if (!is.character(target_units) || length(target_units) != 1L || is.na(target_units) || !nzchar(target_units)) {
      stop("`target_units` must be one non-empty unit label or NULL.", call. = FALSE)
    }
    result$provenance$target_units <- target_units
  }
  use_retained <- missing(top_features) && missing(n_repeats) && missing(max_models)
  assert_flag(uncertainty, "uncertainty")
  if (uncertainty) result$performance_uncertainty <- performance_uncertainty(result)
  top_features <- assert_count(top_features, "top_features")
  n_repeats <- assert_count(n_repeats, "n_repeats")
  max_models <- assert_count(max_models, "max_models")
  validate_html_destination(output_file, open)
  title <- title %||% result$provenance$analysis_label %||% paste("Predict", result$target_column)
  if (!is.character(title) || length(title) != 1L || is.na(title) || !nzchar(title)) {
    stop("`title` must be a single non-empty string or NULL.", call. = FALSE)
  }
  if (is.null(audit)) {
    prepared <- if (!is.null(result$explanations) && use_retained) {
      result$explanations
    } else {
      prepare_model_report_data(result, top_features, n_repeats, max_models)
    }
    result$explanations <- prepared
    audit <- prepared$audit
    effects <- effects %||% prepared$effects
  }
  if (!inherits(audit, "autoxplain_audit")) {
    stop("`audit` must be returned by `audit_explanations()`.", call. = FALSE)
  }
  ids <- names(audit$importance_objects)
  if (!length(ids) || any(!ids %in% names(result$models))) {
    stop("The audit references model IDs absent from this result.", call. = FALSE)
  }
  validate_attached_audit(audit, as_explainers(result, models = ids))
  effects <- effects %||% list()
  if (!is.list(effects)) stop("`effects` must be a list or NULL.", call. = FALSE)
  if (length(effects) && (is.null(names(effects)) || anyNA(names(effects)) || any(!nzchar(names(effects))) || anyDuplicated(names(effects)))) {
    stop("`effects` must have unique non-empty feature names.", call. = FALSE)
  }
  if (length(effects)) {
    primary_id <- result$evaluation$primary_model_id %||% result$provenance$primary_model_id
    primary_explainer <- as_explainers(result, models = primary_id)[[1L]]
    expected <- current_explainer_fingerprint(primary_explainer)
    for (feature in names(effects)) {
      effect <- effects[[feature]]
      if (!identical(attr(effect, "feature") %||% names(effect)[[1L]], feature))
        stop("Attached effect names must match their recorded feature.", call. = FALSE)
      observed <- attr(effect, "explainer_fingerprint")
      if (is.null(observed) || !identical(observed, expected)) {
        stop("An attached effect was not made from the same primary model and evaluation evidence. Recompute it from as_explainers(result).", call. = FALSE)
      }
    }
  }
  if (explicit_effects) result$explanations$failures <- NULL
  if (!is.null(narrative) &&
        (!is.character(narrative) || length(narrative) != 1L || is.na(narrative))) {
    stop("`narrative` must be a single string or NULL.", call. = FALSE)
  }
  subgroup_check <- if (is.null(subgroup)) {
    NULL
  } else {
    subgroup_performance(result, by = subgroup)
  }
  directory <- dirname(output_file)
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(directory)) stop("Could not create output directory: ", directory, call. = FALSE)
  result <- prepare_report_diagnostics(result)
  writeLines(
    model_report_html(
      result, audit, effects, narrative, subgroup_check, title
    ), output_file,
    useBytes = TRUE
  )
  output_path <- normalizePath(output_file, mustWork = TRUE)
  attr(output_path, "diagnostic_status") <- lapply(result$explanations$report_diagnostics, function(record) {
    record[c("id", "status", "scope", "entities", "reason")]
  })
  if (open && interactive()) utils::browseURL(output_path)
  invisible(output_path)
}

validate_html_destination <- function(output_file, open) {
  if (!is.character(output_file) || length(output_file) != 1L || is.na(output_file) ||
        !nzchar(output_file) || tolower(tools::file_ext(output_file)) != "html") {
    stop("`output_file` must be a single non-empty .html path.", call. = FALSE)
  }
  if (!is.logical(open) || length(open) != 1L || is.na(open)) {
    stop("`open` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(TRUE)
}

model_report_html <- function(result, audit, effects, narrative, subgroup_check, title) {
  if (is.null(result$explanations$report_diagnostics)) result <- prepare_report_diagnostics(result)
  model_explorer_html(result, audit, effects, narrative, subgroup_check, title)
}

render_model_tuning <- function(result) {
  tuning <- result$tuning
  if (!inherits(tuning, "autoxplain_tuning")) {
    return("")
  }
  evaluation_role <- normalize_report_evaluation_role(
    result$provenance$evaluation_role %||% "evaluation"
  )
  evaluation_name <- switch(evaluation_role,
    test = "outer test set",
    validation = "validation set",
    "evaluation set"
  )
  score_interpretation <- if (identical(evaluation_role, "test")) {
    paste(
      "The test result above estimates performance on rows kept outside model",
      "fitting and selection."
    )
  } else if (identical(evaluation_role, "validation")) {
    paste(
      "The validation result above is not a final test estimate; evaluate the",
      "refitted model on a separate independent test set before generalization claims."
    )
  } else {
    paste(
      "The result above describes the supplied evaluation rows. Their independence",
      "has not been asserted, so it is not by itself a generalization estimate."
    )
  }
  candidates <- tuning$candidates
  selected <- candidates[candidates$selected, , drop = FALSE]
  final_id <- tuning$final_configuration %||% selected$configuration_id[[1L]]
  if (length(final_id) != 1L || is.na(final_id) || !nzchar(final_id)) {
    final_id <- selected$configuration_id[[1L]]
  }
  final <- candidates[candidates$configuration_id == final_id, , drop = FALSE]
  if (!nrow(final)) final <- selected
  fallback_used <- isTRUE(tuning$refit$fallback_used)
  display_columns <- intersect(c(
    "configuration_id", "model", "hyperparameters", "cv_score", "cv_se",
    "complexity_proxy", "selected", "status", "refit_status", "retained_model_id"
  ), names(candidates))
  display <- candidates[display_columns]
  display_labels <- c(
    configuration_id = "Configuration", model = "Model family", Settings = "Settings",
    hyperparameters = "Settings", cv_score = pretty_metric(tuning$metric),
    cv_se = "Score SE", complexity_proxy = "Within-family flexibility proxy",
    selected = "Resampling choice", status = "Resampling status",
    refit_status = "Full-training refit", retained_model_id = "Retained model ID"
  )
  names(display) <- unname(display_labels[names(display)])
  failed <- sum(candidates$status != "ok")
  paste0(
    "<section id=\"tuning\" class=\"tuning-section\" aria-labelledby=\"tuning-title\">",
    "<p class=\"eyebrow\">Automatic tuning</p>",
    "<h2 id=\"tuning-title\">How was the primary model selected?</h2>",
    "<p>AutoXplainR divided only the training portion into ", tuning$folds_used,
    " folds. Each configuration repeatedly fitted on some training folds and scored on the ",
    "remaining fold. Preprocessing was learned again inside every fold. The ",
    html_escape(evaluation_name), " ",
    "shown in the evaluation section did not participate in this choice.</p>",
    render_tuning_refit_status(tuning, selected$configuration_id[[1L]], final_id),
    "<details class=\"advanced\"><summary>Selection rule, settings and resampling scores</summary><div class=\"cards comparison-cards\">",
    metric_card(
      "Configurations", as.character(nrow(candidates)),
      paste(length(unique(candidates$family)), "model families")
    ),
    metric_card(
      "Training folds", as.character(tuning$folds_used),
      "Fold-specific preprocessing"
    ),
    metric_card(
      "Resampling choice", selected$model[[1L]],
      selected$configuration_id[[1L]]
    ),
    metric_card(
      "Final fitted configuration", final$model[[1L]],
      paste0(final_id, if (fallback_used) "; recorded fallback" else "; refit succeeded")
    ),
    metric_card(
      "Resampled score", report_number(selected$cv_score[[1L]], 4L),
      paste0(pretty_metric(tuning$metric), "; lower is better")
    ),
    "</div><div class=\"columns\"><div><h3>Selection rule</h3><p>",
    html_escape(tuning_rule_label(tuning$selection_rule)),
    ". The default one-standard-error rule first uses the configured family priority, then ",
    "prefers the least-flexible eligible setting inside that family. Use the best-score ",
    "rule when predictive score alone should decide.</p></div>",
    "<div><h3>Selected settings</h3><p><strong>",
    html_escape(selected$model[[1L]]), ":</strong> ",
    html_escape(selected$hyperparameters[[1L]]), ".</p>",
    if (fallback_used) {
      paste0(
        "<p><strong>Final fallback settings (", html_escape(final_id), "):</strong> ",
        html_escape(final$hyperparameters[[1L]]), ".</p>"
      )
    } else {
      ""
    },
    "</div></div>",
    html_table(display, digits = 5L),
    "<p class=\"microcopy\"><strong>Within-family flexibility proxy:</strong> each family ",
    "uses the definition recorded in <code>tuning_results(result)$learner_manifest</code>. ",
    "Those values order settings only inside the same family; their units are not comparable ",
    "across linear, additive, tree, ensemble, kernel, neighbor, or neural models.</p>",
    if (failed) {
      paste0(
        "<p class=\"callout\"><strong>", failed,
        " configuration(s) failed.</strong> They were not eligible for selection; inspect ",
        "<code>tuning_results(result)$fold_scores</code> for the recorded errors.</p>"
      )
    } else {
      ""
    },
    "<p class=\"callout\"><strong>Do not quote the resampled tuning score as final performance.</strong> ",
    "It guided model selection. ", html_escape(score_interpretation), "</p>",
    "<p class=\"microcopy\">", html_escape(tuning$scope_note), "</p></details></section>"
  )
}

render_tuning_refit_status <- function(tuning, selected_id, final_id) {
  refit <- tuning$refit %||% list()
  resampling_failed <- unique(
    tuning$families_resampling_failed %||% refit$families_resampling_failed %||% character()
  )
  refit_failed <- unique(refit$families_refit_failed %||% character())
  messages <- c(paste0(
    "<p class=\"microcopy\"><strong>Actual final configuration:</strong> <code>",
    html_escape(final_id), "</code>. This is the configuration behind the primary fitted model.</p>"
  ))
  if (isTRUE(refit$fallback_used)) {
    messages <- c(messages, paste0(
      "<p class=\"callout\"><strong>The resampling choice could not be refitted.</strong> ",
      "AutoXplainR recorded <code>", html_escape(selected_id), "</code> as the training-only ",
      "choice and used <code>", html_escape(final_id), "</code> as the first successful ",
      "resampling-valid fallback.</p>"
    ))
  }
  if (length(resampling_failed)) {
    messages <- c(messages, paste0(
      "<p class=\"callout\"><strong>No complete resampling result:</strong> ",
      html_escape(paste(resampling_failed, collapse = ", ")),
      ". These families were not eligible for selection.</p>"
    ))
  }
  if (length(refit_failed)) {
    messages <- c(messages, paste0(
      "<p class=\"callout\"><strong>Full-training refit failed:</strong> ",
      html_escape(paste(refit_failed, collapse = ", ")),
      ". They remain in the audit trail but have no retained fitted model.</p>"
    ))
  }
  paste(messages, collapse = "")
}

render_subgroup_performance <- function(subgroups) {
  if (is.null(subgroups)) {
    return("")
  }
  table <- subgroups$performance
  secondary <- subgroups$secondary_metric %||%
    if (identical(subgroups$task, "regression")) "mae" else "accuracy"
  keep <- intersect(c(
    "group", "rows", "share", subgroups$primary_metric, secondary,
    "gap_from_overall", "enough_rows"
  ), names(table))
  display <- table[keep]
  display$share <- vapply(display$share, format_percent, character(1))
  names(display) <- vapply(names(display), function(name) {
    labels <- c(
      group = "Group", rows = "Rows", share = "Share",
      gap_from_overall = "Gap from overall", enough_rows = "Enough rows?"
    )
    if (name %in% names(labels)) {
      return(unname(labels[[name]]))
    }
    pretty_metric(name)
  }, character(1))
  small_groups <- sum(!subgroups$performance$enough_rows)
  definition <- metric_definitions(subgroups$task)[[subgroups$primary_metric]]
  paste0(
    "<section id=\"subgroups\" aria-labelledby=\"subgroups-title\">",
    "<p class=\"eyebrow\">Performance context</p>",
    "<h2 id=\"subgroups-title\">Did performance vary across groups?</h2>",
    "<p>These rows compare the primary model across values of <strong>",
    html_escape(subgroups$by), "</strong>. Positive gaps mean worse ",
    html_escape(pretty_metric(subgroups$primary_metric)), " than the overall evaluation result.</p>",
    "<div class=\"cards\">",
    metric_card("Compared by", subgroups$by, "Chosen explicitly for this report"),
    metric_card(
      "Observed groups", as.character(subgroups$n_groups),
      "Values present in the evaluation rows"
    ),
    metric_card(
      "Largest score gap", report_number(subgroups$largest_observed_gap, 4L),
      pretty_metric(subgroups$primary_metric)
    ),
    metric_card(
      "Small groups", as.character(small_groups),
      paste0("Fewer than ", subgroups$min_rows, " evaluation rows")
    ),
    "</div><p class=\"callout\"><strong>This is not fairness certification.</strong> ",
    "Observed gaps can reflect small samples, different case mix, data quality, or model behavior. ",
    "They do not establish a cause or prove equal treatment.</p>",
    html_table(display, digits = 4L),
    "<p class=\"microcopy\"><strong>",
    html_escape(pretty_metric(subgroups$primary_metric)), ":</strong> ",
    html_escape(definition), " ", html_escape(subgroups$scope_note), "</p></section>"
  )
}

render_model_comparison <- function(result) {
  if (length(result$models) <= 2L) {
    return("")
  }
  record <- report_get_diagnostic(result, "resources", function() model_tradeoffs(result))
  attempt <- list(value = record$evidence, reason = record$reason)
  if (!is.null(attempt$reason)) {
    return(paste0(
      "<section id=\"models\"><h2>Candidate comparison</h2>",
      html_table(guided_leaderboard_table(result), 3L, caption = "Available evaluation scores"),
      render_diagnostic_state("Resource comparison", "failed", attempt$reason),
      render_prediction_ambiguity(result), "</section>"
    ))
  }
  tradeoffs <- attempt$value
  metric <- attr(tradeoffs, "performance_metric")
  resource <- attr(tradeoffs, "complexity_metric")
  display <- data.frame(Model = tradeoffs$model, Role = tradeoffs$role, check.names = FALSE)
  display[[pretty_metric(metric)]] <- tradeoffs[[metric]]
  resource_table <- display
  resource_table[[pretty_complexity(resource)]] <- tradeoffs[[resource]]
  resource_table[["Pareto-efficient"]] <- tradeoffs$pareto_optimal
  selection <- report_view_model(result)$identity$selection_note
  paste0(
    "<section id=\"models\" aria-labelledby=\"models-title\"><p class=\"eyebrow\">Sensitivity to model choice</p>",
    "<h2 id=\"models-title\">Candidate scores and prediction differences</h2>",
    "<p>", html_escape(selection), " Candidate scores use the same evaluation rows.</p>",
    html_table(display, digits = 3L, caption = "Scores on common evaluation rows"),
    render_prediction_ambiguity(result),
    "<details class=\"advanced\"><summary>Resource measurements and Pareto comparison</summary>",
    "<p>Approximate R object size is an operational measurement; it does not measure how complex a learned relationship is. ",
    "Use this comparison only when the displayed measurement matters to your application. Lower resource use is to the left; better prediction is higher.</p>",
    tradeoff_svg(tradeoffs), html_table(resource_table, digits = 3L, caption = "Resource comparison values"),
    "<p class=\"microcopy\">Outlined points are Pareto-efficient among these supplied models on these two axes. ",
    html_escape(attr(tradeoffs, "scope_note")), "</p></details>",
    "<details class=\"advanced\"><summary>What each model family can represent</summary>",
    render_behavior_comparison(result), "</details></section>"
  )
}

render_behavior_comparison <- function(result) {
  record <- report_get_diagnostic(result, "model_behavior", function() compare_model_behavior(result))
  if (record$status != "computed") {
    return(render_diagnostic_state("Model-family comparison", record$status, record$reason))
  }
  behavior <- record$evidence
  models <- behavior$models
  display <- data.frame(
    Model = models$model,
    Family = models$family,
    Backend = models$backend,
    `Capacity: nonlinearity` = models$nonlinearity,
    `Capacity: interactions` = models$interactions,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  display[[paste0("Computed ", pretty_metric(behavior$performance_metric))]] <-
    models$performance_score
  computed <- behavior$findings$message[
    behavior$findings$evidence_kind == "computed"
  ]
  if (length(computed)) for (id in names(result$models)) computed <- gsub(id, report_model_label(result, id), computed, fixed = TRUE)
  computed_html <- if (length(computed)) {
    paste0(
      "<ul>",
      paste0("<li>", vapply(computed, html_escape, character(1L)), "</li>", collapse = ""),
      "</ul>"
    )
  } else {
    ""
  }
  paste0(
    "<div class=\"behavior-comparison\"><h3>How are these model families different?</h3>",
    "<p><strong>Prior/model-capacity knowledge:</strong> the nonlinearity and interaction ",
    "columns describe each model family. They say what each family can represent; they do ",
    "not show that this fitted model actually used those patterns.</p>",
    html_table(display, digits = 4L),
    "<p><strong>Computed evidence from this analysis:</strong> evaluation performance and ",
    "paired prediction disagreement are calculated on common evaluation rows. Repeated ",
    "permutation feature importance in the Patterns section is also computed evidence of ",
    "model reliance, not a property guaranteed by the family card.</p>",
    computed_html,
    "</div>"
  )
}

render_prediction_ambiguity <- function(result) {
  record <- report_get_diagnostic(result, "prediction_disagreement", function() prediction_ambiguity(result))
  if (record$status != "computed") {
    return(render_diagnostic_state("Prediction disagreement", record$status, record$reason))
  }
  ambiguity <- record$evidence
  if (identical(ambiguity$task, "regression")) {
    top <- ambiguity$rows[order(
      ambiguity$rows$prediction_range,
      decreasing = TRUE
    ), c(
      "evaluation_row", "observed", "prediction_min", "prediction_max",
      "prediction_range"
    ), drop = FALSE]
    top <- head(top, 5L)
    names(top) <- c("Row", "Observed", "Lowest prediction", "Highest prediction", "Range")
    cards <- paste0(
      "<div class=\"cards diagnostic-cards\">",
      metric_card(
        "Compared candidates", as.character(ambiguity$n_models),
        "Simple baseline excluded"
      ),
      metric_card(
        "Median prediction range",
        report_number(ambiguity$median_prediction_range, 4L),
        "In outcome units"
      ),
      metric_card(
        "90th-percentile range",
        report_number(ambiguity$p90_prediction_range, 4L),
        "Nine in ten rows were below this"
      ),
      metric_card(
        "Largest prediction range",
        report_number(ambiguity$max_prediction_range, 4L),
        "Most specification-sensitive row"
      ),
      "</div>"
    )
  } else {
    top <- ambiguity$rows[order(
      ambiguity$rows$probability_distance,
      decreasing = TRUE
    ), c(
      "evaluation_row", "observed", "predicted_classes", "class_disagreement",
      "probability_distance"
    ), drop = FALSE]
    top <- head(top, 5L)
    names(top) <- c(
      "Row", "Observed", "Candidate predictions", "Different classes?",
      "Probability distance"
    )
    cards <- paste0(
      "<div class=\"cards diagnostic-cards\">",
      metric_card(
        "Compared candidates", as.character(ambiguity$n_models),
        "Simple baseline excluded"
      ),
      metric_card(
        "Rows with class disagreement",
        format_percent(ambiguity$class_disagreement_rate),
        "Candidates chose different labels"
      ),
      metric_card(
        "Median probability distance",
        format_percent(ambiguity$median_probability_distance),
        ambiguity$probability_distance
      ),
      metric_card(
        "Largest probability distance",
        format_percent(ambiguity$max_probability_distance),
        "Most specification-sensitive row"
      ),
      "</div>"
    )
  }
  paste0(
    "<div class=\"ambiguity\"><h3>Where did supplied model choices disagree?</h3>",
    "<p>The same evaluation rows were scored by every supplied non-baseline candidate. ",
    "A large gap means the answer depends on model specification, even when the data row ",
    "is unchanged.</p>", cards, html_table(top, digits = 4L),
    "<p class=\"callout\"><strong>Disagreement is a review signal, not an error bar.</strong> ",
    "The compared candidates can have very different evaluation performance; read this beside ",
    "the score table above. It does not identify the correct prediction or provide uncertainty ",
    "coverage.</p></div>"
  )
}

tradeoff_svg <- function(tradeoffs) {
  metric <- attr(tradeoffs, "performance_metric")
  resource <- attr(tradeoffs, "complexity_metric")
  higher <- isTRUE(attr(tradeoffs, "higher_is_better"))
  width <- 680
  height <- 340
  left <- 76
  right <- 26
  top <- 32
  bottom <- 70
  xv <- tradeoffs[[resource]]
  yv <- tradeoffs[[metric]]
  xr <- plot_limits(xv)
  yr <- plot_limits(yv)
  px <- function(v) left + (v - xr[1]) / diff(xr) * (width - left - right)
  py <- function(v) {
    if (higher) {
      height - bottom - (v - yr[1]) / diff(yr) * (height - top - bottom)
    } else {
      top + (v - yr[1]) / diff(yr) * (height - top - bottom)
    }
  }
  ticks <- paste0(
    vapply(pretty(xr, 4), function(v) {
      if (v < xr[1] || v > xr[2]) {
        ""
      } else {
        paste0(
          '<text x="', px(v), '" y="', height - bottom + 23, '" class="tick" text-anchor="middle">', report_number(v, 2), "</text>"
        )
      }
    }, character(1)),
    collapse = ""
  )
  ticks <- paste0(ticks, paste(vapply(pretty(yr, 4), function(v) {
    if (v < yr[1] || v > yr[2]) {
      ""
    } else {
      paste0(
        '<line x1="', left, '" x2="', width - right, '" y1="', py(v), '" y2="', py(v), '" class="grid-line"/>',
        '<text x="', left - 9, '" y="', py(v) + 5, '" class="tick" text-anchor="end">', report_number(v, 2), "</text>"
      )
    }
  }, character(1)), collapse = ""))
  frontier <- which(tradeoffs$pareto_optimal)
  frontier <- frontier[order(xv[frontier])]
  line <- if (length(frontier) > 1) {
    paste0(
      '<polyline class="pareto-line" points="',
      paste(px(xv[frontier]), py(yv[frontier]), sep = ",", collapse = " "), '"/>'
    )
  } else {
    ""
  }
  points <- paste(vapply(seq_len(nrow(tradeoffs)), function(i) {
    paste0(
      '<circle cx="', px(xv[i]), '" cy="', py(yv[i]), '" r="7" class="tradeoff-point tradeoff-',
      html_escape(tradeoffs$role[i]), if (tradeoffs$pareto_optimal[i]) " tradeoff-pareto" else "",
      '"/><text x="', px(xv[i]) + if (px(xv[i]) > width / 2) -12 else 12, '" y="',
      py(yv[i]) + if (py(yv[i]) < top + 22) 22 else -10, '" text-anchor="',
      if (px(xv[i]) > width / 2) "end" else "start", '" class="point-label">', i, "</text>"
    )
  }, character(1)), collapse = "")
  paste0(
    '<div class="chart-scroll" role="region" aria-label="Resource comparison chart" tabindex="0">',
    '<svg class="tradeoff-plot" viewBox="0 0 ', width, " ", height, '" role="img" aria-label="',
    html_escape(paste("Candidate", pretty_metric(metric), "versus", pretty_complexity(resource), ". Numbered points match the key below.")), '">',
    ticks, '<line x1="', left, '" x2="', width - right, '" y1="', height - bottom, '" y2="', height - bottom, '" class="axis"/>',
    '<text x="', left, '" y="18" class="axis-label">', html_escape(pretty_metric(metric)),
    if (higher) " (higher is better)" else " (lower is better)", "</text>",
    '<text x="', width / 2, '" y="', height - 15, '" text-anchor="middle" class="axis-label">',
    html_escape(pretty_complexity(resource)), " (lower is left)</text>", line, points, "</svg></div>",
    '<ol class="chart-key">', paste0("<li>", html_escape(tradeoffs$model), "</li>", collapse = ""), "</ol>"
  )
}

plot_limits <- function(values, include_zero = FALSE) {
  limits <- range(c(values[is.finite(values)], if (include_zero) 0), na.rm = TRUE)
  if (any(!is.finite(limits))) {
    return(c(0, 1))
  }
  if (diff(limits) == 0) limits <- limits + c(-1, 1) * max(abs(limits[1]) * 0.1, 0.1)
  limits
}

scale_plot_values <- function(values, lower, upper) {
  limits <- range(values, na.rm = TRUE)
  if (!is.finite(diff(limits)) || diff(limits) == 0) {
    return(rep((lower + upper) / 2, length(values)))
  }
  lower + (values - limits[[1L]]) / diff(limits) * (upper - lower)
}

pretty_complexity <- function(metric) {
  labels <- c(
    model_size_kb = "approximate model-object size (KB)",
    size_mb = "approximate model-object size (MB)",
    model_size = "approximate model-object size",
    training_time_ms = "training time (ms)",
    training_time_s = "training time (s)",
    prediction_time_ms = "prediction time (ms)",
    complexity = "model complexity"
  )
  if (metric %in% names(labels)) unname(labels[[metric]]) else gsub("_", " ", metric)
}

model_report_evaluation <- function(result, audit) {
  evaluation <- result$evaluation
  primary_id <- evaluation$primary_model_id %||%
    if ("main_model" %in% names(evaluation$metrics)) "main_model" else NULL
  if (!is.null(evaluation$primary_metric) && !is.null(primary_id) &&
        primary_id %in% names(evaluation$metrics)) {
    metric <- evaluation$primary_metric
    primary <- evaluation$metrics[[primary_id]][[metric]]
    baseline <- if ("simple_baseline" %in% names(evaluation$metrics)) {
      evaluation$metrics$simple_baseline[[metric]] %||% NA_real_
    } else {
      NA_real_
    }
    definition <- evaluation$metric_definitions[[metric]] %||% "See the metric documentation."
    return(list(
      metric = metric,
      primary = primary,
      baseline = baseline,
      improvement = evaluation$improvement_over_baseline,
      beats_baseline = evaluation$beats_baseline,
      definition = definition,
      rows = evaluation$evaluated_rows,
      table = guided_leaderboard_table(result),
      notes = evaluation$notes,
      diagnostics = evaluation$diagnostics,
      role = normalize_report_evaluation_role(
        result$provenance$evaluation_role %||% "evaluation"
      )
    ))
  }
  primary_id <- primary_id %||% result$provenance$primary_model_id
  best <- match(primary_id, audit$performance$model)
  if (length(best) != 1L || is.na(best)) stop("Primary-model evaluation evidence is unavailable.", call. = FALSE)
  metric <- audit$config$metric
  definition_key <- switch(metric,
    logloss = "log_loss",
    brier = "brier_score",
    metric
  )
  definitions <- metric_definitions(result$task)
  list(
    metric = metric,
    primary = audit$performance$score[[best]],
    baseline = NA_real_,
    improvement = NA_real_,
    beats_baseline = NA,
    definition = definitions[[definition_key]] %||% "See the model-engine documentation.",
    rows = nrow(result$test_data %||% result$training_data),
    table = guided_leaderboard_table(result),
    notes = NULL,
    diagnostics = NULL,
    role = normalize_report_evaluation_role(
      result$provenance$evaluation_role %||% "evaluation"
    )
  )
}

normalize_report_evaluation_role <- function(role) {
  if (role %in% c("test", "held-out test")) {
    return("test")
  }
  if (identical(role, "validation")) {
    return("validation")
  }
  "evaluation"
}

guided_leaderboard_table <- function(result) {
  table <- as.data.frame(result$leaderboard)
  definitions <- names(result$evaluation$metric_definitions %||% character())
  preferred <- unique(c(
    result$evaluation$primary_metric %||% character(),
    definitions,
    "rmse", "mae", "r_squared", "log_loss", "brier_score", "accuracy",
    "calibration_error", "balanced_accuracy", "roc_auc", "macro_recall", "auc",
    "logloss"
  ))
  identity <- if ("model" %in% names(table)) "model" else "model_id"
  keep <- intersect(c("rank", identity, "role", preferred), names(table))
  table <- table[keep]
  names(table) <- vapply(names(table), function(name) {
    if (name == "rank") {
      return("Rank")
    }
    if (name %in% c("model", "model_id")) {
      return("Model")
    }
    if (name == "role") {
      return("Role")
    }
    pretty_metric(name)
  }, character(1))
  table
}

render_model_overview <- function(result, evaluation) {
  identity <- report_view_model(result)$identity
  improvement <- if (is.finite(evaluation$improvement %||% NA_real_)) format_percent(evaluation$improvement) else "not available"
  conclusion <- if (isTRUE(evaluation$beats_baseline)) {
    paste0(
      pretty_metric(evaluation$metric), " was ", improvement,
      if (evaluation$metric %in% c("accuracy", "auc", "roc_auc")) " higher" else " lower",
      " than the intercept-only baseline on these ", identity$evaluation_role, " rows."
    )
  } else if (identical(evaluation$beats_baseline, FALSE)) {
    "The primary model did not improve on the intercept-only baseline on these evaluation rows."
  } else {
    "A comparable baseline score is unavailable."
  }
  scores <- data.frame(
    Model = c(identity$model_label, "Intercept-only baseline"),
    Estimate = c(evaluation$primary, evaluation$baseline), check.names = FALSE
  )
  metric_label <- paste0(
    pretty_metric(evaluation$metric),
    if (!is.null(identity$target_units) && evaluation$metric %in% c("rmse", "mae")) paste0(" (", identity$target_units, ")") else ""
  )
  names(scores)[2] <- metric_label
  notes <- evaluation$notes
  if (!is.null(notes) && nrow(notes)) {
    priority <- match(notes$severity, c("critical", "error", "warning", "caution", "note"))
    notes <- notes[order(priority, na.last = TRUE), , drop = FALSE]
  }
  caveat <- if (!is.null(notes) && nrow(notes)) {
    paste0(
      '<p class="leading-caveat"><strong>', html_escape(notes$message[1]), "</strong> ",
      html_escape(notes$recommendation[1]), "</p>"
    )
  } else {
    paste0('<p class="microcopy">', if (identity$evaluation_role == "test") {
      "Test independence is an assertion of this analysis. The split does not rule out upstream leakage."
    } else {
      "These scores are descriptive; model or workflow choices may have used these rows."
    }, "</p>")
  }
  paste0(
    '<section id="overview" class="brief" aria-labelledby="overview-title">',
    '<h2 id="overview-title">Prediction and evidence</h2><p class="verdict">', html_escape(conclusion), "</p>",
    caveat, html_table(scores, 3L, caption = paste(metric_label, "on", evaluation$rows, identity$evaluation_role, "rows")),
    '<div class="identity"><p><strong>Target:</strong> ', html_escape(identity$target),
    if (!is.null(identity$target_units)) paste0(" (", html_escape(identity$target_units), ")") else "",
    if (!is.null(identity$positive)) paste0(" \u00b7 <strong>Probability event:</strong> ", html_escape(identity$positive)) else "",
    " \u00b7 <strong>Training rows:</strong> ", identity$training_rows, " \u00b7 <strong>Evaluation rows:</strong> ", identity$evaluation_rows,
    "</p><p><strong>Design:</strong> ", html_escape(identity$split_method), ". ", html_escape(identity$selection_note),
    ' <a href="#provenance">Run details and R commands</a></p></div></section>'
  )
}

render_model_evaluation <- function(result, evaluation) {
  validation_role <- identical(evaluation$role, "validation")
  test_role <- identical(evaluation$role, "test")
  eyebrow <- if (validation_role) {
    "Model-selection validation"
  } else if (test_role) {
    "Independent-test check"
  } else {
    "Configured evaluation"
  }
  heading <- if (validation_role) {
    "How did the model score on validation rows?"
  } else if (test_role) {
    "Prediction performance on test rows"
  } else {
    "How did the model score on the evaluation rows?"
  }
  caution <- if (validation_role) {
    paste0(
      "<p class=\"callout\"><strong>This is not a final test estimate.</strong> ",
      "These rows may have influenced model or workflow decisions. Use a separate ",
      "untouched test set for final generalization claims.</p>"
    )
  } else if (!test_role) {
    paste0(
      "<p class=\"callout\"><strong>This is a descriptive evaluation.</strong> ",
      "The supplied rows have not been asserted as an independent test set. These ",
      "scores describe these records and should not be presented as evidence of ",
      "generalization.</p>"
    )
  } else {
    ""
  }
  metric_context <- if (validation_role) {
    "validation"
  } else if (test_role) {
    "test-set"
  } else {
    "evaluation-set"
  }
  paste0(
    "<section id=\"evaluation\" aria-labelledby=\"evaluation-title\"><p class=\"eyebrow\">",
    eyebrow, "</p><h2 id=\"evaluation-title\">", heading, "</h2>", caution,
    "<p class=\"callout\"><strong>", html_escape(pretty_metric(evaluation$metric)),
    ":</strong> ", html_escape(evaluation$definition), "</p>",
    "<details class=\"advanced\"><summary>All evaluation metrics and definitions</summary><p>The table reports every computed ", metric_context,
    " metric. Compare models using the metric definitions, ",
    "not the rank column alone.</p>", html_table(evaluation$table, digits = 4L),
    render_metric_definitions(result), render_guided_notes(evaluation$notes), "</details>", render_prediction_diagnostics(result, evaluation$diagnostics),
    "</section>"
  )
}

render_guided_notes <- function(notes) {
  if (is.null(notes) || !nrow(notes)) {
    return("")
  }
  items <- vapply(seq_len(nrow(notes)), function(index) {
    paste0(
      "<article class=\"guided-note guided-note-", html_escape(notes$severity[[index]]), "\">",
      "<h3>", html_escape(notes$message[[index]]), "</h3><p><strong>Next step:</strong> ",
      html_escape(notes$recommendation[[index]]), "</p></article>"
    )
  }, character(1))
  paste0(
    "<div class=\"guided-notes\"><h3>Important context for these scores</h3>",
    paste(items, collapse = ""), "</div>"
  )
}

render_prediction_diagnostics <- function(result, diagnostics) {
  if (is.null(diagnostics)) {
    return(render_diagnostic_state("Prediction errors", "not_run", "No prediction diagnostics were retained."))
  }
  missingness_html <- render_missingness_shift(diagnostics$missingness_shift)
  if (result$task == "regression") {
    return(paste0(
      "<h3>How large were individual errors?</h3><div class=\"cards diagnostic-cards\">",
      metric_card(
        "Mean error", report_number(diagnostics$mean_error, 4L),
        "Observed minus predicted; near zero means little average bias"
      ),
      metric_card(
        "Median absolute error", report_number(diagnostics$median_absolute_error, 4L),
        "Half of absolute errors were below this value"
      ),
      metric_card(
        "90th-percentile error", report_number(diagnostics$p90_absolute_error, 4L),
        "Nine in ten absolute errors were below this value"
      ),
      "</div>", missingness_html
    ))
  }
  paste0(
    "<h3>Which classes were confused?</h3><p>Rows on the diagonal are correct predictions. ",
    "Off-diagonal rows show the specific mistakes.</p>",
    html_table(diagnostics$confusion_matrix, digits = 0L),
    render_calibration_diagnostic(result, diagnostics$calibration),
    render_threshold_diagnostic(result),
    missingness_html
  )
}

render_threshold_diagnostic <- function(result) {
  if (!identical(result$task, "binary")) {
    return("")
  }
  record <- report_get_diagnostic(result, "decision_cutoffs", function() threshold_diagnostics(result, thresholds = c(0.3, 0.5, 0.7)))
  if (record$status != "computed") {
    return(render_diagnostic_state("Decision cutoffs", record$status, record$reason))
  }
  diagnostic <- record$evidence
  display <- diagnostic$performance[c(
    "threshold", "predicted_positive_rate", "sensitivity", "specificity",
    "precision", "accuracy", "false_positives", "false_negatives"
  )]
  percentage_columns <- c(
    "predicted_positive_rate", "sensitivity", "specificity", "precision", "accuracy"
  )
  for (column in percentage_columns) {
    display[[column]] <- vapply(display[[column]], format_percent, character(1))
  }
  names(display) <- c(
    "Cutoff", "Predicted positive", "Sensitivity", "Specificity", "Precision",
    "Accuracy", "False positives", "False negatives"
  )
  paste0(
    "<h3>What changes when the decision cutoff moves?</h3>",
    "<p>The model reports the probability of <strong>",
    html_escape(diagnostic$positive_class),
    "</strong>. Calling that class at 0.5 is a convention. A lower cutoff usually catches ",
    "more positives and creates more false positives; a higher cutoff usually does the reverse.</p>",
    html_table(display, digits = 2L),
    "<p class=\"callout\"><strong>No cutoff is recommended here.</strong> The relative ",
    "consequences of false positives and false negatives belong to the application. If a cutoff ",
    "is chosen using these evaluation rows, evaluate it again on different representative data.</p>"
  )
}

render_missingness_shift <- function(shift) {
  if (is.null(shift)) {
    return(render_diagnostic_state("Missingness comparison", "not_run", "No missingness comparison was retained."))
  }
  if (shift$n_with_missing == 0L) {
    return("<p class=\"microcopy\">No missing input values were observed in training or evaluation.</p>")
  }
  display <- shift$features[
    shift$features$training_missing_rate > 0 |
      shift$features$evaluation_missing_rate > 0,
    c(
      "feature", "used_by_model", "training_missing_rate", "evaluation_missing_rate",
      "rate_change", "flagged"
    ),
    drop = FALSE
  ]
  display$training_missing_rate <- vapply(
    display$training_missing_rate, format_percent, character(1)
  )
  display$evaluation_missing_rate <- vapply(
    display$evaluation_missing_rate, format_percent, character(1)
  )
  display$rate_change <- vapply(display$rate_change, function(value) {
    paste0(if (value > 0) "+" else "", format_percent(value))
  }, character(1))
  names(display) <- c(
    "Input", "Used by model?", "Training missing", "Evaluation missing",
    "Change", "Flagged?"
  )
  verdict <- if (shift$n_flagged_model_features > 0L) {
    paste0(
      "<p class=\"callout\"><strong>Some model inputs crossed the practical flag.</strong> ",
      "Check whether collection or pipeline behavior changed before treating the evaluation ",
      "result as representative.</p>"
    )
  } else {
    paste0(
      "<p class=\"callout\"><strong>No model input crossed the practical flag.</strong> ",
      "Missingness can still matter even when rate differences are smaller.</p>"
    )
  }
  paste0(
    "<h3>Did missing data change between fitting and evaluation?</h3>",
    "<p>This compares raw missing-value rates before the configured <strong>",
    html_escape(shift$preprocessing_strategy), "</strong> handling was applied.</p>",
    "<div class=\"cards diagnostic-cards\">",
    metric_card(
      "Inputs with missing values", as.character(shift$n_with_missing),
      "Observed before preprocessing"
    ),
    metric_card(
      "Flagged model inputs", as.character(shift$n_flagged_model_features),
      paste0("At least ", format_percent(shift$threshold), " absolute change")
    ),
    metric_card(
      "Largest observed shift", format_percent(shift$largest_shift),
      "Absolute training-versus-evaluation difference"
    ),
    "</div>", verdict, html_table(display, digits = 3L),
    "<p class=\"microcopy\">", html_escape(shift$scope_note), "</p>"
  )
}

render_calibration_diagnostic <- function(result, calibration) {
  if (is.null(calibration)) {
    return(render_diagnostic_state("Probability calibration", "not_run", "No calibration diagnostic was retained."))
  }
  binary <- identical(result$task, "binary")
  observed_label <- if (binary) "Observed positive rate" else "Observed accuracy"
  probability_label <- if (binary) "Average predicted probability" else "Average confidence"
  display <- calibration$groups[c(
    "probability_group", "rows", "mean_probability", "observed_rate", "calibration_gap"
  )]
  names(display) <- c(
    "Probability group", "Rows", probability_label, observed_label, "Absolute gap"
  )
  event <- if (binary) {
    paste0(
      "the model's probability for the positive class <strong>",
      html_escape(calibration$positive_class), "</strong>"
    )
  } else {
    "the confidence attached to the model's predicted class"
  }
  paste0(
    "<h3>Can the reported probabilities be taken literally?</h3>",
    "<p>Calibration compares ", event, " with how often that event occurred on evaluation rows. ",
    "For example, predictions near 70% are well calibrated when the event happens about 70% ",
    "of the time in comparable evaluation groups.</p>",
    "<div class=\"cards diagnostic-cards\">",
    metric_card(
      if (binary) "Average probability" else "Average confidence",
      format_percent(calibration$mean_probability),
      if (binary) "Mean positive-class probability" else "Mean predicted-class confidence"
    ),
    metric_card(
      if (binary) "Observed positive rate" else "Observed accuracy",
      format_percent(calibration$observed_rate),
      "What actually happened on evaluation rows"
    ),
    metric_card(
      "Binned calibration gap", format_percent(calibration$calibration_error),
      "Average absolute discrepancy; lower is better"
    ),
    "</div>", html_table(display, digits = 3L),
    "<p class=\"microcopy\">This binned gap is descriptive and changes with the evaluation ",
    "sample and grouping. It is not an uncertainty interval or a guarantee for future data. ",
    "Use log loss and Brier score alongside it.</p>"
  )
}

render_metric_definitions <- function(result) {
  definitions <- result$evaluation$metric_definitions
  if (is.null(definitions)) {
    return("")
  }
  names(definitions) <- vapply(names(definitions), pretty_metric, character(1))
  paste0(
    "<details class=\"learn-more\"><summary>Definitions for every metric</summary>",
    definition_list(definitions), "</details>"
  )
}

pretty_metric <- function(metric) {
  labels <- c(
    rmse = "RMSE", mae = "MAE", r_squared = "R-squared",
    log_loss = "log loss", logloss = "log loss", accuracy = "accuracy",
    brier = "Brier score", brier_score = "Brier score",
    calibration_error = "binned calibration gap",
    balanced_accuracy = "balanced accuracy", roc_auc = "ROC AUC",
    macro_recall = "macro recall", auc = "ROC AUC"
  )
  if (metric %in% names(labels)) {
    unname(labels[[metric]])
  } else {
    gsub("_", " ", metric, fixed = TRUE)
  }
}

render_guided_narrative <- function(narrative) {
  if (is.null(narrative) || !nzchar(narrative)) {
    return("")
  }
  provenance <- attr(narrative, "narrative_provenance")
  disclosure <- if (!is.null(provenance)) {
    paste0(
      "Provider used: ", provenance$provider_used, if (!is.null(provenance$model)) {
        paste0(" / ", provenance$model)
      } else {
        ""
      }, ". Verify this prose against the computed sections."
    )
  } else {
    "Verify this prose against the computed sections."
  }
  paste0(
    "<section id=\"narrative\" aria-labelledby=\"narrative-title\"><p class=\"eyebrow\">Optional communication aid</p>",
    "<h2 id=\"narrative-title\">Plain-language memo</h2><p>", html_escape(disclosure),
    "</p><div class=\"narrative\">", simple_markdown_html(narrative), "</div></section>"
  )
}

simple_markdown_html <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  output <- character()
  in_list <- FALSE
  close_list <- function() {
    if (in_list) {
      output <<- c(output, "</ul>")
      in_list <<- FALSE
    }
  }
  for (line in lines) {
    trimmed <- trimws(line)
    if (!nzchar(trimmed)) {
      close_list()
    } else if (grepl("^#{1,4} ", trimmed)) {
      close_list()
      level <- min(4L, nchar(sub("^(#+).*", "\\1", trimmed)) + 2L)
      content <- sub("^#{1,4} ", "", trimmed)
      output <- c(output, paste0("<h", level, ">", inline_markdown(content), "</h", level, ">"))
    } else if (grepl("^- ", trimmed)) {
      if (!in_list) {
        output <- c(output, "<ul>")
        in_list <- TRUE
      }
      output <- c(output, paste0("<li>", inline_markdown(sub("^- ", "", trimmed)), "</li>"))
    } else {
      close_list()
      output <- c(output, paste0("<p>", inline_markdown(trimmed), "</p>"))
    }
  }
  close_list()
  paste(output, collapse = "")
}

inline_markdown <- function(text) {
  escaped <- html_escape(text)
  escaped <- gsub("[*][*]([^*]+)[*][*]", "<strong>\\1</strong>", escaped, perl = TRUE)
  gsub("`([^`]+)`", "<code>\\1</code>", escaped, perl = TRUE)
}

render_guided_importance <- function(importance, model = NULL, metric = "loss") {
  if (is.null(importance) || !nrow(importance)) {
    return(render_diagnostic_state("Feature shuffling", "not_run", "No feature-shuffling results were retained."))
  }
  model <- model %||% if ("main_model" %in% importance$model) "main_model" else importance$model[[1L]]
  item <- importance[importance$model == model, , drop = FALSE]
  if (!nrow(item)) return(render_diagnostic_state("Primary-model feature shuffling", "not_run", "The supplied audit does not include the primary model."))
  item <- item[order(item$importance, decreasing = TRUE), , drop = FALSE]
  rows <- paste(vapply(seq_len(nrow(item)), function(i) {
    paste0(
      '<tr><th scope="row">', html_escape(item$feature[i]), '</th><td class="number">', report_number(item$importance[i], 3L),
      '</td><td class="number">[', report_number(item$conf_low[i], 3L), ", ", report_number(item$conf_high[i], 3L),
      "]</td><td>", html_escape(item$claim[i]), "</td></tr>"
    )
  }, character(1)), collapse = "")
  paste0(
    '<div class="table-wrap" role="region" aria-label="Primary-model feature evidence" tabindex="0"><table>',
    '<caption>Primary-model loss changes after shuffling</caption><thead><tr><th scope="col">Input</th><th scope="col">Change in ',
    html_escape(pretty_metric(metric)), '</th><th scope="col">Shuffle MC interval</th><th scope="col">Interpretation</th></tr></thead><tbody>',
    rows, '</tbody></table></div><p class="microcopy">Positive loss changes mean prediction worsened after shuffling. ',
    "Intervals describe shuffle Monte Carlo variation, not population confidence. Feature association can make shuffled combinations unrealistic.</p>"
  )
}

render_effects <- function(effects, result) {
  if (!length(effects)) {
    return(render_diagnostic_state(
      "Fitted effects",
      if (!is.null(result$explanations$failures) && nrow(result$explanations$failures)) "failed" else "not_run",
      "No fitted effect curves are available in this report. Recorded failures, if any, follow below."
    ))
  }
  plots <- vapply(names(effects), function(feature) {
    effect <- effects[[feature]]
    method <- attr(effect, "method") %||% "pdp"
    target <- attr(effect, "prediction_target") %||% paste("predicted", result$target_column)
    units <- result$provenance$target_units %||% if (result$task == "regression") "target units (unit label not supplied)" else "probability units"
    table <- as.data.frame(effect)
    names(table)[1] <- feature
    paste0(
      '<article class="effect-card" id="effect-', report_anchor(feature), '"><h3>', html_escape(feature),
      '</h3><p class="effect-context">', html_escape(toupper(method)), ": ",
      if (method == "ale") "centered change in " else "average ", html_escape(target), " \u00b7 ", html_escape(units),
      "</p><p>", html_escape(effect_plain_summary(effect, result)), "</p>", effect_svg(effect, feature, result),
      '<p class="microcopy">Support shows relative observed counts (0\u20131). ',
      html_escape(attr(effect, "interval_note") %||% "Bands, when available, describe fixed-model variation."),
      '</p><details class="advanced"><summary>Values, support and descriptive intervals for ', html_escape(feature), "</summary>",
      html_table(table, 3L, caption = paste(toupper(method), "values for", feature, ";", target, ";", units)),
      "</details></article>"
    )
  }, character(1))
  paste0(
    "<h3>How predictions vary across each input</h3><p>Each effect belongs to this fitted model. ",
    'The plot does not predict the consequences of intervening on the input.</p><div class="effect-grid">', paste(plots, collapse = ""), "</div>"
  )
}

effect_plain_summary <- function(effect, result) {
  method <- attr(effect, "method") %||% "pdp"
  y <- effect[[if (method == "ale") "accumulated_effect" else "partial_dependence"]]
  if (!length(y) || !any(is.finite(y))) {
    return("No finite effect estimate is available.")
  }
  target <- attr(effect, "prediction_target") %||% paste("predicted", result$target_column)
  paste0(
    if (method == "ale") "Centered fitted effects for " else "Average fitted predictions for ", target, " range from ",
    report_number(min(y, na.rm = TRUE), 3L), " to ", report_number(max(y, na.rm = TRUE), 3L),
    ". Read changes against the input values and support below."
  )
}

effect_svg <- function(effect, feature, result = NULL) {
  method <- attr(effect, "method") %||% "pdp"
  values <- effect[[if (method == "ale") "accumulated_effect" else "partial_dependence"]]
  if (!length(values) || any(!is.finite(values))) {
    return(render_diagnostic_state("Effect plot", "failed", "The effect contains missing or non-finite estimates; inspect the retained values."))
  }
  xval <- effect[[1L]]
  numeric_x <- is.numeric(xval)
  width <- if (numeric_x) 600 else max(600, length(values) * max(105, max(nchar(as.character(xval))) * 8) + 100)
  height <- 390
  left <- 70
  right <- 28
  top <- 30
  baseline <- 235
  xv <- if (numeric_x) xval else seq_along(xval)
  xr <- if (numeric_x) plot_limits(xv) else c(.5, length(xv) + .5)
  yr <- plot_limits(c(values, effect$conf_low, effect$conf_high), include_zero = method == "ale")
  px <- function(v) left + (v - xr[1]) / diff(xr) * (width - left - right)
  py <- function(v) baseline - (v - yr[1]) / diff(yr) * (baseline - top)
  ticks_x <- if (numeric_x) pretty(xr, 4) else seq_along(xval)
  ticks_x <- ticks_x[ticks_x >= xr[1] & ticks_x <= xr[2]]
  ticks <- paste(vapply(ticks_x, function(v) {
    paste0(
      '<text x="', px(v), '" y="', baseline + 22,
      '" class="tick" text-anchor="middle">', html_escape(if (numeric_x) report_number(v, 2L) else as.character(xval[v])),
      "</text>"
    )
  }, character(1)), collapse = "")
  ticks <- paste0(ticks, paste(vapply(pretty(yr, 4), function(v) {
    if (v < yr[1] || v > yr[2]) {
      ""
    } else {
      paste0(
        '<line class="grid-line" x1="', left, '" x2="', width - right, '" y1="', py(v), '" y2="', py(v), '"/>',
        '<text x="', left - 10, '" y="', py(v) + 5, '" text-anchor="end" class="tick">', report_number(v, 2L), "</text>"
      )
    }
  }, character(1)), collapse = ""))
  zero <- if (method == "ale") {
    paste0(
      '<line class="zero-line" x1="', left, '" x2="', width - right, '" y1="', py(0), '" y2="', py(0), '"/>',
      '<text x="', width - right, '" y="', py(0) - 6, '" class="tick" text-anchor="end">zero</text>'
    )
  } else {
    ""
  }
  bands <- if (numeric_x && all(c("conf_low", "conf_high") %in% names(effect)) &&
                 all(is.finite(c(effect$conf_low, effect$conf_high)))) {
    paste0(
      '<polygon class="effect-band" points="',
      paste(c(px(xv), rev(px(xv))), c(py(effect$conf_low), rev(py(effect$conf_high))), sep = ",", collapse = " "), '"/>'
    )
  } else {
    ""
  }
  line <- if (numeric_x) paste0('<polyline class="effect-line" points="', paste(px(xv), py(values), sep = ",", collapse = " "), '"/>') else ""
  points <- paste0('<circle class="effect-point" cx="', px(xv), '" cy="', py(values), '" r="3.5"/>', collapse = "")
  support <- effect$support %||% rep(NA_real_, length(values))
  bars <- paste(vapply(seq_along(support), function(i) {
    if (!is.finite(support[i])) {
      ""
    } else {
      paste0(
        '<line class="support-bar" x1="', px(xv[i]), '" x2="', px(xv[i]), '" y1="342" y2="', 342 - 20 * support[i], '"/>'
      )
    }
  }, character(1)), collapse = "")
  paste0(
    '<div class="chart-scroll" role="region" tabindex="0" aria-label="Effect chart for ', html_escape(feature), '">',
    '<svg class="effect-plot" data-axis-type="', if (numeric_x) "numeric" else "categorical",
    '" style="min-width:', width, 'px" viewBox="0 0 ', width, " ", height,
    '" role="img" aria-label="', html_escape(paste(toupper(method), "for", feature, ": input values, fitted effect, zero reference for ALE, and relative support. Full values in the following table.")), '">',
    '<text x="', left, '" y="18" class="axis-label">', if (method == "ale") "Centered fitted effect" else "Average prediction", "</text>",
    bands, ticks, zero, line, points, '<text x="', width / 2, '" y="298" class="axis-label" text-anchor="middle">', html_escape(feature), "</text>",
    bars, '<text x="', left, '" y="378" class="tick">Relative support 0\u20131</text></svg></div>'
  )
}

render_reliability_section <- function(audit, result = NULL) {
  attention <- audit$findings
  if (!is.null(attention)) attention <- attention[attention$code != "association_screen_scope", , drop = FALSE]
  statuses <- c(audit$diagnostic_status, result$explanations$report_diagnostics %||% list())
  status_html <- if (is.list(statuses) && length(statuses)) {
    paste(vapply(statuses, function(x) {
      if (!is.list(x)) {
        return("")
      }
      render_diagnostic_state(x$id %||% "Diagnostic", x$status %||% "not_run", x$reason %||% x$interpretation %||% "")
    }, character(1)), collapse = "")
  } else {
    ""
  }
  model_summary <- if (!is.null(audit$model_diagnostics)) html_table(audit$model_diagnostics, 3L, caption = "Checks by supplied model") else ""
  paste0(
    '<section id="reliability" aria-labelledby="reliability-title"><h2 id="reliability-title">Checks requiring attention</h2>',
    render_findings(attention, audit, result),
    '<details class="advanced"><summary>Diagnostic coverage and association evidence</summary>', status_html, model_summary,
    "<p>", html_escape(audit$summary$scope_note %||% "Each check has its own scope. No overall model-quality grade is assigned."), "</p>",
    "<h3>Feature association diagnostics</h3>", html_table(audit$dependence, 3L, caption = "Feature association screen"),
    '<p class="microcopy">', html_escape(audit$summary$association_scope %||% "A limited association screen does not establish independence."),
    "</p></details></section>"
  )
}

render_model_provenance <- function(result, audit) {
  paste0(
    "<section id=\"provenance\" aria-labelledby=\"provenance-title\"><p class=\"eyebrow\">Reproducibility</p>",
    "<h2 id=\"provenance-title\">How this result was produced</h2>",
    "<p>Keep the fitted result with its data version and analysis code.</p><pre><code>saveRDS(result, &quot;analysis.rds&quot;)\nresult$evaluation\nresult$explanations$audit</code></pre>",
    "<details class=\"advanced\"><summary>Complete run metadata</summary>",
    definition_list(c(
      "Generated" = audit$provenance$created_at,
      "Package version" = audit$provenance$package_version,
      "Engine" = result$engine %||% "h2o",
      "Target" = result$target_column,
      "Task" = result$task,
      "Training rows" = as.character(nrow(result$training_data)),
      "Evaluation rows" = as.character(nrow(result$test_data %||% result$training_data)),
      "Evaluation role" = result$provenance$evaluation_role %||% "unspecified",
      "Split method" = result$provenance$split_method %||% "user configured",
      "Primary model ID" = result$provenance$primary_model_id %||%
        result$evaluation$primary_model_id %||% "unspecified",
      "Primary model label" = result$provenance$primary_model_label %||% "unspecified",
      "Permutation repeats" = as.character(audit$config$n_repeats),
      "Seed" = as.character(audit$config$seed),
      "Explainer IDs" = paste(audit$provenance$explainer_fingerprints, collapse = ", ")
    )), "</details></section>"
  )
}

#' Render a standalone explanation evidence report
#'
#' Creates a standalone HTML report from an explanation audit.
#' The report presents separate diagnostic scopes and findings rather than presenting
#' every numerical explanation as equally trustworthy.
#'
#' @param audit An `autoxplain_audit`, an `autoxplain_explainer`, or a list of
#'   explainers. Non-audit inputs are passed to [audit_explanations()].
#' @param output_file Destination `.html` path.
#' @param title Report title.
#' @param open Open the report in a browser after writing it. Defaults to
#'   `FALSE`, which is safe for non-interactive and CRAN environments.
#' @param ... Additional arguments passed to [audit_explanations()] when
#'   `audit` is not already an audit.
#'
#' @return The normalized output path, invisibly.
#' @export
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' x <- explain_model(fit, mtcars, "mpg")
#' audit <- audit_explanations(x, n_repeats = 3)
#' path <- tempfile(fileext = ".html")
#' render_explanation_report(audit, path)
#' unlink(path)
render_explanation_report <- function(audit,
                                      output_file = "autoxplain-report.html",
                                      title = "Explanation Evidence Report",
                                      open = FALSE,
                                      ...) {
  if (!inherits(audit, "autoxplain_audit")) audit <- audit_explanations(audit, ...)
  if (!is.character(output_file) || length(output_file) != 1L || is.na(output_file) ||
        !nzchar(output_file)) {
    stop("`output_file` must be a single non-empty path.", call. = FALSE)
  }
  if (!is.character(title) || length(title) != 1L || is.na(title)) {
    stop("`title` must be a single non-missing string.", call. = FALSE)
  }
  if (!is.logical(open) || length(open) != 1L || is.na(open)) {
    stop("`open` must be TRUE or FALSE.", call. = FALSE)
  }
  extension <- tolower(tools::file_ext(output_file))
  if (extension != "html") stop("`output_file` must use the .html extension.", call. = FALSE)
  directory <- dirname(output_file)
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(directory)) stop("Could not create output directory: ", directory, call. = FALSE)

  html <- explanation_report_html(audit, title)
  writeLines(html, con = output_file, useBytes = TRUE)
  output_path <- normalizePath(output_file, mustWork = TRUE)
  if (open && interactive()) utils::browseURL(output_path)
  invisible(output_path)
}

explanation_report_html <- function(audit, title) {
  summary <- audit$summary
  findings <- audit$findings
  generated <- audit$provenance$created_at
  narrative <- if (!is.null(audit$optional_narrative) &&
                     length(audit$optional_narrative) == 1L &&
                     !is.na(audit$optional_narrative) &&
                     nzchar(audit$optional_narrative)) {
    paste0(
      "<section id=\"narrative\" aria-labelledby=\"narrative-title\">",
      "<p class=\"eyebrow\">Optional secondary summary</p>",
      "<h2 id=\"narrative-title\">Narrative</h2>",
      "<p>This text is subordinate to the computed evidence above. Verify it against the tables.</p>",
      "<pre class=\"narrative\">", html_escape(audit$optional_narrative), "</pre></section>"
    )
  } else {
    ""
  }
  paste0(
    "<!doctype html>\n<html lang=\"en\"><head><meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">",
    "<title>", html_escape(title), "</title><style>", report_css(), "</style></head>",
    "<body><a class=\"skip\" href=\"#main\">Skip to report</a>",
    "<header><div class=\"shell\"><p class=\"eyebrow\">AutoXplainR / Evidence audit</p>",
    "<h1>", html_escape(title), "</h1>",
    "<p class=\"lede\">A stress test of explanation stability, feature dependence, ",
    "and near-optimal model disagreement. Generated ", html_escape(generated), ".</p>",
    "<nav aria-label=\"Report sections\"><a href=\"#findings\">Findings</a>",
    "<a href=\"#performance\">Models</a><a href=\"#importance\">Importance</a>",
    "<a href=\"#dependence\">Dependence</a><a href=\"#provenance\">Provenance</a></nav>",
    "</div></header><main id=\"main\" class=\"shell\">",
    "<section aria-labelledby=\"summary-title\"><h2 id=\"summary-title\">Diagnostic scope</h2>",
    "<p>", html_escape(summary$scope_note %||% "Each diagnostic concerns supplied fitted models and evaluation rows."), "</p>",
    if (!is.null(audit$model_diagnostics)) html_table(audit$model_diagnostics, caption = "Checks by supplied model") else "",
    "</section>",
    "<section id=\"findings\" aria-labelledby=\"findings-title\"><p class=\"eyebrow\">Triage</p>",
    "<h2 id=\"findings-title\">Findings and actions</h2>", render_findings(findings, audit), "</section>",
    narrative,
    "<section id=\"performance\" aria-labelledby=\"performance-title\"><p class=\"eyebrow\">Rashomon check</p>",
    "<h2 id=\"performance-title\">Supplied model set</h2>",
    "<p>Near-optimal status is relative to the supplied models and evaluation data; it is not a claim ",
    "that the full Rashomon set has been enumerated.</p>",
    html_table(audit$performance, digits = 5L), "</section>",
    "<section id=\"importance\" aria-labelledby=\"importance-title\"><p class=\"eyebrow\">Evidence, not rank alone</p>",
    "<h2 id=\"importance-title\">Permutation importance claims</h2>",
    "<p>Intervals below capture variation across random permutations. They do not provide population-level ",
    "inference. Negative importance is retained because it can reveal noise, sampling error, or model pathologies.</p>",
    render_importance(audit$importance), "</section>",
    "<section id=\"dependence\" aria-labelledby=\"dependence-title\"><p class=\"eyebrow\">Assumption pressure</p>",
    "<h2 id=\"dependence-title\">Feature dependence</h2>",
    "<p>High association warns that marginal permutations and PDPs can create unrealistic feature combinations. ",
    "Use ALE for effects and a suitable conditional importance estimator for conditional claims.</p>",
    html_table(audit$dependence, digits = 3L), "</section>",
    "<section aria-labelledby=\"method-title\"><p class=\"eyebrow\">Interpretation contract</p>",
    "<h2 id=\"method-title\">What this report can and cannot say</h2>",
    "<div class=\"columns\"><div><h3>Supported</h3><ul>",
    "<li>Descriptive model reliance on the evaluation data</li>",
    "<li>Monte Carlo stability of the implemented permutation procedure</li>",
    "<li>Observed dependence and supplied-model disagreement diagnostics</li>",
    "</ul></div><div><h3>Not established</h3><ul>",
    "<li>Causal effects or actionable interventions</li>",
    "<li>Population inference from permutation repeats</li>",
    "<li>Fairness, safety, or regulatory compliance</li>",
    "</ul></div></div></section>",
    "<section id=\"provenance\" aria-labelledby=\"provenance-title\"><p class=\"eyebrow\">Reproducibility</p>",
    "<h2 id=\"provenance-title\">Audit provenance</h2>",
    definition_list(c(
      "Generated" = audit$provenance$created_at,
      "Package version" = audit$provenance$package_version,
      "Metric" = audit$config$metric,
      "Permutation repeats" = as.character(audit$config$n_repeats),
      "Seed" = as.character(audit$config$seed),
      "Explainer IDs" = paste(audit$provenance$explainer_fingerprints, collapse = ", "),
      "Scope" = audit$provenance$diagnostic_scope
    )), "</section></main>",
    "<footer><div class=\"shell\">Generated by AutoXplainR. Preserve this report with the model, ",
    "evaluation-data version, and analysis code.</div></footer>", report_interaction_script(), "</body></html>"
  )
}

render_diagnostic_state <- function(label, status, reason = "") {
  paste0(
    '<p class="diagnostic-state"><strong>', html_escape(label), ": ", html_escape(gsub("_", " ", status)),
    ".</strong> ", html_escape(reason %||% ""), "</p>"
  )
}

report_anchor <- function(x) paste(as.character(charToRaw(enc2utf8(as.character(x)))), collapse = "")
report_evidence_id <- function(model, feature) paste0("evidence-", report_anchor(model), "-", report_anchor(feature))

render_findings <- function(findings, audit = NULL, result = NULL) {
  if (is.null(findings) || !nrow(findings)) {
    return("<p>No diagnostic findings were recorded. Review the coverage below; an absent finding is not a guarantee.</p>")
  }
  cards <- vapply(seq_len(nrow(findings)), function(i) {
    links <- ""
    model <- if ("model" %in% names(findings)) findings$model[[i]] else NA_character_
    feature <- if ("feature" %in% names(findings)) findings$feature[[i]] else NA_character_
    if (!is.na(model) && !is.na(feature) && nzchar(model) && nzchar(feature)) {
      label <- if (is.null(result)) model else report_model_label(result, model)
      links <- paste0('<a href="#', report_evidence_id(model, feature), '">', html_escape(label), " / ", html_escape(feature), "</a>")
    } else if (!is.na(feature) && nzchar(feature) && !is.null(audit$importance)) {
      items <- audit$importance[audit$importance$feature == feature, , drop = FALSE]
      if (nrow(items)) {
        links <- paste(vapply(seq_len(nrow(items)), function(j) {
          paste0(
            '<a href="#',
            report_evidence_id(items$model[j], items$feature[j]), '">', html_escape(if (is.null(result)) items$model[j] else report_model_label(result, items$model[j])),
            " / ", html_escape(items$feature[j]), "</a>"
          )
        }, character(1)), collapse = ", ")
      }
    } else if (!is.null(audit$importance) && grepl("importance|shuffle|permutation", findings$code[[i]])) {
      items <- audit$importance
      if ("shuffle_status" %in% names(items)) items <- items[items$shuffle_status %in% c("negative_loss_change", "interval_includes_zero", "interval_unavailable"), , drop = FALSE]
      if (nrow(items)) {
        links <- paste(vapply(seq_len(nrow(items)), function(j) {
          paste0(
            '<a href="#',
            report_evidence_id(items$model[j], items$feature[j]), '">', html_escape(if (is.null(result)) items$model[j] else report_model_label(result, items$model[j])),
            " / ", html_escape(items$feature[j]), "</a>"
          )
        }, character(1)), collapse = ", ")
      }
    }
    paste0(
      '<article class="finding finding-', html_escape(findings$severity[[i]]), '">',
      "<h3>", html_escape(findings$message[[i]]), "</h3>", if (nzchar(links)) paste0('<p class="affected">Affected evidence: ', links, "</p>") else "",
      "<p>", html_escape(findings$evidence[[i]]), "</p><p><strong>Next:</strong> ", html_escape(findings$recommendation[[i]]),
      "</p></article>"
    )
  }, character(1))
  paste0('<div class="findings">', paste(cards, collapse = ""), "</div>")
}

render_importance <- function(importance) {
  if (is.null(importance) || !nrow(importance)) {
    return(render_diagnostic_state("Feature evidence", "not_run", "No feature evidence was retained."))
  }
  panels <- vapply(unique(importance$model), function(model) {
    item <- importance[importance$model == model, , drop = FALSE]
    rows <- paste(vapply(seq_len(nrow(item)), function(i) {
      paste0(
        '<tr id="', report_evidence_id(model, item$feature[i]),
        '"><th scope="row">', html_escape(item$feature[i]), '</th><td class="number">', report_number(item$importance[i], 4L),
        '</td><td class="number">[', report_number(item$conf_low[i], 4L), ", ", report_number(item$conf_high[i], 4L),
        "]</td><td>", html_escape(if ("shuffle_status" %in% names(item)) gsub("_", " ", item$shuffle_status[i]) else "See interval"),
        "</td><td>", html_escape(item$claim[i]), "</td></tr>"
      )
    }, character(1)), collapse = "")
    paste0(
      '<article class="model-panel"><h3>', html_escape(model), '</h3><div class="table-wrap" role="region" tabindex="0" aria-label="Feature evidence for ',
      html_escape(model), '"><table><caption>Shuffle evidence for ', html_escape(model), "</caption><thead><tr>",
      '<th scope="col">Feature</th><th scope="col">Loss change</th><th scope="col">MC interval</th>',
      '<th scope="col">Shuffle result</th><th scope="col">Interpretation</th></tr></thead><tbody>', rows, "</tbody></table></div></article>"
    )
  }, character(1))
  paste(panels, collapse = "")
}

html_table <- function(data, digits = 3L, caption = NULL) {
  if (is.null(data)) {
    return(render_diagnostic_state("Table", "not_run", "No values were retained."))
  }
  if (is.null(caption)) caption <- paste("Values:", paste(head(names(data), 3L), collapse = ", "))
  headers <- paste0("<th scope=\"col\">", html_escape(names(data)), "</th>", collapse = "")
  rows <- vapply(seq_len(nrow(data)), function(row) {
    cells <- vapply(data, function(column) {
      value <- column[[row]]
      if (is.integer(value)) {
        value <- format(value, trim = TRUE)
      } else if (is.numeric(value)) {
        value <- report_number(value, digits)
      }
      if (is.logical(value)) value <- if (isTRUE(value)) "yes" else "no"
      paste0("<td", if (is.numeric(column)) " class=\"number\"" else "", ">", html_escape(as.character(value)), "</td>")
    }, character(1))
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  }, character(1))
  paste0(
    "<div class=\"table-wrap\" role=\"region\" tabindex=\"0\" aria-label=\"", html_escape(caption), "\"><table><caption>", html_escape(caption), "</caption><thead><tr>", headers,
    "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div>"
  )
}

metric_card <- function(label, value, detail) {
  paste0(
    "<article class=\"metric\"><p>", html_escape(label), "</p><strong>",
    html_escape(value), "</strong><small>", html_escape(detail), "</small></article>"
  )
}

definition_list <- function(values) {
  entries <- paste0("<dt>", html_escape(names(values)), "</dt><dd>",
    html_escape(unname(values)), "</dd>",
    collapse = ""
  )
  paste0("<dl>", entries, "</dl>")
}

html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}

report_number <- function(x, digits = 3L) {
  if (length(x) != 1L || !is.finite(x)) {
    return("n/a")
  }
  format(round(x, digits), nsmall = min(2L, digits), trim = TRUE, scientific = FALSE)
}

report_css <- function() {
  paste0(
    ":root{--ink:#202c32;--muted:#536269;--paper:#fbfcfa;--line:#d5dddb;--green:#17654e;--amber:#87520c;--blue:#245590}*{box-sizing:border-box}html{scroll-behavior:smooth;scroll-padding-top:76px}body{margin:0;background:var(--paper);color:var(--ink);font:16px/1.5 ui-sans-serif,system-ui,-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif}.shell{width:min(1060px,calc(100% - 48px));margin:auto}.skip{position:absolute;left:-9999px}.skip:focus{left:12px;top:12px;background:white;padding:10px;z-index:20}header{padding:28px 0 20px;border-top:5px solid var(--green);background:#fff}h1{font-family:Georgia,'Times New Roman',serif;font-weight:500;font-size:clamp(1.8rem,3vw,2.6rem);line-height:1.12;letter-spacing:-.025em;margin:9px 0}.eyebrow{text-transform:uppercase;font-size:.7rem;letter-spacing:.12em;font-weight:750;color:var(--green);margin:0 0 7px}.lede{margin:0;color:var(--muted);font-size:.95rem}.report-nav{position:sticky;top:0;z-index:10;background:#fff;border-block:1px solid var(--line)}.report-nav .shell{display:flex;gap:4px;overflow-x:auto;padding-block:7px}.report-nav a{flex:none;padding:6px 9px;font-size:.84rem;text-decoration:none;color:var(--ink);border-bottom:2px solid transparent}.report-nav a:hover{border-bottom-color:var(--green)}a{color:#165a84;text-underline-offset:3px}a:focus-visible,summary:focus-visible,[tabindex]:focus-visible{outline:3px solid #245590;outline-offset:3px}main{padding-bottom:32px}section{padding:28px 0;border-bottom:1px solid var(--line);scroll-margin-top:70px}h2{font-size:1.45rem;letter-spacing:-.018em;line-height:1.2;margin:0 0 14px}h3{font-size:1.04rem;line-height:1.3;margin:22px 0 10px}p{margin:10px 0}ul,ol{padding-left:1.4em}li{margin:5px 0}.brief{padding-top:25px}.verdict{font-size:1.08rem;font-weight:650;margin:0 0 12px;max-width:850px}.leading-caveat{border-left:3px solid #ad7219;padding:8px 13px;margin:12px 0;background:#fff7e8;font-size:.94rem}.identity{font-size:.83rem;color:var(--muted);margin-top:13px}.identity p{margin:6px 0}.brief table{max-width:100%}.brief .number{font-size:1.12rem;font-weight:700}.table-wrap{overflow:auto;max-width:100%;margin:12px 0;border-block:1px solid var(--line)}table{border-collapse:collapse;width:100%;font-size:.87rem;background:#fff}caption{text-align:left;font-size:.77rem;color:var(--muted);padding:8px 10px;border-bottom:1px solid var(--line)}th,td{padding:10px 12px;text-align:left;vertical-align:top;border-bottom:1px solid #e3e8e6}thead th{font-size:.79rem;font-weight:700;background:#f2f5f3}tbody th{font-weight:650}.number{font-variant-numeric:tabular-nums;text-align:right;white-space:nowrap}tbody tr:last-child td,tbody tr:last-child th{border-bottom:0}.microcopy{font-size:.83rem;color:var(--muted)}.cards{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:18px;margin:16px 0}.metric{border-left:2px solid var(--line);padding:2px 0 2px 14px;min-width:0}.metric p{font-size:.8rem;font-weight:600;color:var(--muted);margin:0}.metric strong{display:block;font-size:1.35rem;font-variant-numeric:tabular-nums;font-weight:650;margin:3px 0}.metric small{font-size:.77rem;color:var(--muted)}.findings{display:grid;gap:10px;min-width:0}.finding{min-width:0;overflow-wrap:anywhere;border-left:3px solid #ad7219;padding:4px 14px;background:#fffaf0}.finding h3{margin:4px 0;font-size:.96rem}.finding p{font-size:.88rem;margin:5px 0}.finding-note{border-left-color:#81938b;background:#f3f6f4}.finding-critical,.finding-warning{border-left-color:#a94833}.affected{font-size:.84rem}.diagnostic-state{font-size:.87rem;padding:7px 0;border-bottom:1px solid var(--line)}.callout{border-left:3px solid #8b9c95;background:#f3f6f4;padding:9px 13px;font-size:.9rem}.guided-note{border-left:3px solid #ad7219;padding:8px 13px;background:#fff7e8}.guided-note h3{margin:0;font-size:.93rem}.guided-note p{font-size:.87rem;margin:5px 0}.guided-notes{margin-top:16px}.advanced,.learn-more{margin:15px 0;border-block:1px solid var(--line);padding:0 0 2px}.advanced>summary,.learn-more>summary{padding:12px 2px;font-size:.9rem;font-weight:650;color:var(--green);cursor:pointer}.advanced[open],.learn-more[open]{padding-bottom:14px}.advanced>*:not(summary),.learn-more>*:not(summary){margin-inline:3px}.effect-grid{display:grid;gap:24px}.effect-card{min-width:0;border-top:1px solid var(--line);padding-top:12px}.effect-card h3{font-size:1.15rem;margin:0}.effect-context{font-size:.86rem;font-weight:600;color:var(--muted)}.chart-scroll{overflow:auto;max-width:100%;margin:14px 0;background:#fff}.effect-plot{display:block;width:100%;max-width:850px;height:auto}.tradeoff-plot{display:block;width:100%;min-width:680px;max-width:900px;height:auto}.axis{stroke:#74877d;stroke-width:1.2}.grid-line{stroke:#e1e7e3;stroke-width:1}.zero-line{stroke:#52685f;stroke-width:1.3;stroke-dasharray:5 4}.effect-line{fill:none;stroke:#17654e;stroke-width:2.5}.effect-point{fill:#17654e}.effect-band{fill:#bedacc;opacity:.6}.support-bar{stroke:#7d9486;stroke-width:7}.tick,.axis-label,.point-label{fill:#33443b;font:14px ui-sans-serif,system-ui,sans-serif}.axis-label{font-weight:600}.pareto-line{fill:none;stroke:#527d6a;stroke-width:2;stroke-dasharray:5 4}.tradeoff-point{fill:#496686;stroke:white;stroke-width:2}.tradeoff-primary{fill:#17654e}.tradeoff-baseline{fill:#9a641c}.tradeoff-pareto{stroke:#203b31;stroke-width:3}.chart-key{font-size:.87rem;columns:2}.columns{display:grid;grid-template-columns:1fr 1fr;gap:25px}dl{display:grid;grid-template-columns:minmax(140px,220px) 1fr;gap:7px 18px;font-size:.9rem}dt{font-weight:650}dd{margin:0;color:var(--muted);overflow-wrap:anywhere}code{font-size:.84em;background:#edf2ee;padding:1px 4px;border-radius:3px}pre{overflow:auto;max-width:100%;padding:12px;background:#edf2ee;font-size:.87rem}pre code{padding:0}.narrative{overflow-wrap:anywhere}.model-panel{margin:20px 0}.model-panel h3{overflow-wrap:anywhere}footer{font-size:.8rem;color:var(--muted);padding:20px 0;background:#edf2ef}tr[id]{scroll-margin-top:80px}tr:target{background:#fff0c9}nav:not(.report-nav){display:flex;gap:12px;flex-wrap:wrap;font-size:.85rem}",
    "@media(max-width:700px){.shell{width:calc(100% - 30px)}header{padding-top:20px}.report-nav .shell{width:100%;padding-inline:9px}.cards{grid-template-columns:1fr 1fr;gap:14px}.columns{grid-template-columns:1fr}section{padding-block:22px}.chart-key{columns:1}h2{font-size:1.3rem}.brief .number{font-size:1rem}th,td{padding:9px 10px}.effect-plot{max-width:none}dl{grid-template-columns:1fr;gap:2px}dd{margin-bottom:8px}}",
    "@media(max-width:420px){body{font-size:15px}.cards{grid-template-columns:1fr 1fr}.metric strong{font-size:1.2rem}.identity{font-size:.8rem}.table-wrap{position:relative}}",
    "@page{margin:12mm}@media print{html{scroll-padding:0}body{background:#fff;font-size:10pt;color:#111}.shell{width:100%}header{padding:8px 0;border-top:2px solid #333}h1{font-size:23pt}h2{font-size:17pt;break-after:avoid}h3,summary{break-after:avoid}.report-nav,nav,.skip,footer{display:none}section{padding:14px 0;break-inside:auto}table{font-size:9pt}th,td{padding:6px;overflow-wrap:anywhere}thead{display:table-header-group}tr,.metric{break-inside:avoid}.table-wrap,.chart-scroll,pre{overflow:visible}svg.effect-plot,svg.tradeoff-plot{min-width:0!important;max-width:100%;width:100%;break-inside:avoid}details::details-content{display:block!important;content-visibility:visible!important}details>summary{list-style:none;font-size:11pt}.cards{grid-template-columns:repeat(3,1fr)}a{color:inherit}.finding{break-inside:avoid}.advanced,.learn-more{border-top:1px solid #999;padding:0 0 14px!important}.effect-card{break-inside:auto}}",
    "@media(prefers-reduced-motion:reduce){html{scroll-behavior:auto}}"
  )
}

report_interaction_script <- function() {
  paste0(
    "<script>(function(){",
    "function reveal(){if (!location.hash)return;var e=document.getElementById(decodeURIComponent(location.hash.slice(1)));",
    'if (!e)return;for(var p=e.parentElement;p;p=p.parentElement)if (p.tagName==="DETAILS")p.open=true;',
    'e.scrollIntoView({block:"start"});}',
    'addEventListener("hashchange",reveal);reveal();var closed=[];',
    'addEventListener("beforeprint",function(){closed=Array.from(document.querySelectorAll("details:not([open])"));closed.forEach(function(e){e.open=true;});});',
    'addEventListener("afterprint",function(){closed.forEach(function(e){e.open=false;});closed=[];});',
    "})();</script>"
  )
}

render_validation_design <- function(result) {
  design <- result$validation
  if (is.null(design)) {
    return("")
  }
  paste0(
    "<section aria-labelledby=\"design-title\"><h2 id=\"design-title\">Validation design</h2><p>",
    if (design$method == "group") {
      "Whole groups were kept out of training. The evaluation describes prediction for new groups."
    } else {
      "The latest times were kept out of training. The evaluation describes prediction later in time."
    },
    "</p><p>Split column: <code>", html_escape(design$column),
    "</code> (excluded from predictors). Gap rows excluded: ", length(design$excluded_rows),
    ".</p><p class=\"microcopy\">The design preserves this boundary; it does not establish ",
    "that the data represent every future population or prevent leakage in upstream feature construction.</p></section>"
  )
}

render_effect_failures <- function(result) {
  failures <- result$explanations$failures
  if (is.null(failures) || !nrow(failures)) {
    return("")
  }
  paste0(
    "<div class=\"callout\"><h3>Effects that could not be estimated</h3><ul>",
    paste0("<li><code>", html_escape(failures$feature), "</code>: ",
      html_escape(failures$reason), "</li>",
      collapse = ""
    ),
    "</ul><p>Inspect support and the fitted model before interpreting these inputs.</p></div>"
  )
}

render_performance_uncertainty <- function(uncertainty) {
  if (is.null(uncertainty)) {
    return(paste0(
      '<section id="uncertainty"><h2>Evaluation-sample uncertainty</h2>',
      render_diagnostic_state("Paired bootstrap", "not_run", "No evaluation-sample interval was requested."),
      '<details class="advanced"><summary>Compute intervals in R</summary><pre><code>render_model_report(result, "report.html", uncertainty = TRUE)</code></pre>',
      "<p>The bootstrap requires supported independent sampling units; time-based evaluation needs a different design.</p></details></section>"
    ))
  }
  paste0(
    "<section id=\"uncertainty\" aria-labelledby=\"uncertainty-title\"><h2 id=\"uncertainty-title\">How variable is this score?</h2>",
    "<p>Paired ", format_percent(uncertainty$confidence), " percentile intervals from ",
    uncertainty$n_boot, " bootstrap draws over ", uncertainty$units, " ", uncertainty$unit,
    "s. Negative differences favor the primary model.</p>",
    html_table(uncertainty$estimates),
    "<p class=\"microcopy\">", html_escape(paste(uncertainty$notes, collapse = " ")), "</p></section>"
  )
}
