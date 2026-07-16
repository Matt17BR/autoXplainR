#' Render a beginner-first model report
#'
#' Creates a standalone HTML report from an [autoxplain()] result. The report
#' leads with the prediction question, held-out evaluation, simple-baseline
#' comparison, and plain metric definitions. Feature reliance, fitted effects,
#' and an explanation evidence audit follow with progressively more detail.
#'
#' @param result An `autoxplain_result`.
#' @param output_file Destination `.html` path.
#' @param title Optional report title. `NULL` uses the target name.
#' @param audit Optional precomputed `autoxplain_audit`.
#' @param effects Optional named list of feature-effect objects.
#' @param narrative Optional narrative returned by
#'   [generate_natural_language_report()].
#' @param subgroup Optional name of one categorical or low-cardinality column.
#'   When supplied, the report includes an explicit held-out subgroup
#'   performance check. See [subgroup_performance()].
#' @param open Open the report in a browser after writing it.
#' @param top_features Maximum number of features audited when `audit` is not
#'   supplied.
#' @param n_repeats Permutation repeats when `audit` is not supplied.
#' @param max_models Maximum models audited when `audit` is not supplied.
#'
#' @return The normalized output path, invisibly.
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
                                max_models = 5L) {
  if (!inherits(result, "autoxplain_result")) {
    stop("`result` must be returned by `autoxplain()`.", call. = FALSE)
  }
  top_features <- assert_count(top_features, "top_features")
  n_repeats <- assert_count(n_repeats, "n_repeats")
  max_models <- assert_count(max_models, "max_models")
  validate_html_destination(output_file, open)
  title <- title %||% paste("Understanding", result$target_column)
  if (!is.character(title) || length(title) != 1L || is.na(title) || !nzchar(title)) {
    stop("`title` must be a single non-empty string or NULL.", call. = FALSE)
  }
  if (is.null(audit)) {
    prepared <- prepare_model_report_data(result, top_features, n_repeats, max_models)
    audit <- prepared$audit
    effects <- effects %||% prepared$effects
  }
  if (!inherits(audit, "autoxplain_audit")) {
    stop("`audit` must be returned by `audit_explanations()`.", call. = FALSE)
  }
  effects <- effects %||% list()
  if (!is.list(effects)) stop("`effects` must be a list or NULL.", call. = FALSE)
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
  writeLines(model_report_html(
    result, audit, effects, narrative, subgroup_check, title
  ), output_file,
  useBytes = TRUE)
  output_path <- normalizePath(output_file, mustWork = TRUE)
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
  evaluation <- model_report_evaluation(result, audit)
  narrative_html <- render_guided_narrative(narrative)
  tuning_html <- render_model_tuning(result)
  comparison_html <- render_model_comparison(result)
  comparison_nav <- if (nzchar(comparison_html)) "<a href=\"#models\">Compare</a>" else ""
  tuning_nav <- if (nzchar(tuning_html)) "<a href=\"#tuning\">Tune</a>" else ""
  subgroup_nav <- if (!is.null(subgroup_check)) "<a href=\"#subgroups\">Groups</a>" else ""
  paste0(
    "<!doctype html>\n<html lang=\"en\"><head><meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">",
    "<title>", html_escape(title), "</title><style>", report_css(), "</style></head>",
    "<body><a class=\"skip\" href=\"#main\">Skip to report</a>",
    "<header><div class=\"shell\"><p class=\"eyebrow\">AutoXplainR / Guided model report</p>",
    "<h1>", html_escape(title), "</h1>",
    "<p class=\"lede\">What was fitted, how it performed on data kept out of fitting, ",
    "what trade-offs appeared across candidates, which patterns it used, and how cautiously ",
    "those patterns should be communicated.</p>",
    "<nav aria-label=\"Report sections\"><a href=\"#overview\">Overview</a>",
    "<a href=\"#evaluation\">Evaluation</a>", tuning_nav, comparison_nav, subgroup_nav,
    "<a href=\"#patterns\">Patterns</a>",
    "<a href=\"#reliability\">Reliability</a><a href=\"#limits\">Limits</a></nav>",
    "</div></header><main id=\"main\" class=\"shell\">",
    render_model_overview(result, evaluation),
    render_model_evaluation(result, evaluation),
    tuning_html,
    comparison_html,
    render_subgroup_performance(subgroup_check),
    narrative_html,
    "<section id=\"patterns\" aria-labelledby=\"patterns-title\"><p class=\"eyebrow\">How the model works</p>",
    "<h2 id=\"patterns-title\">Patterns used for prediction</h2>",
    "<p>Permutation importance asks how much held-out performance worsens when one input is shuffled. ",
    "It describes model reliance, not cause and effect.</p>",
    render_guided_importance(audit$importance),
    render_effects(effects, result), "</section>",
    render_reliability_section(audit),
    "<section id=\"limits\" aria-labelledby=\"limits-title\"><p class=\"eyebrow\">Interpretation boundaries</p>",
    "<h2 id=\"limits-title\">What this analysis does not establish</h2>",
    "<div class=\"columns\"><div><h3>Reasonable statements</h3><ul>",
    "<li>How the fitted model performed on this held-out set</li>",
    "<li>Which inputs the model relied on under the configured shuffling test</li>",
    "<li>Which fitted prediction patterns appeared in the evaluation data</li>",
    "</ul></div><div><h3>Statements requiring other evidence</h3><ul>",
    "<li>That changing an input will cause the predicted outcome to change</li>",
    "<li>That performance will transfer to another population or future period</li>",
    "<li>That the model is fair, safe, or suitable for deployment</li>",
    "</ul></div></div></section>",
    render_model_provenance(result, audit),
    "</main><footer><div class=\"shell\">Generated by AutoXplainR. Preserve this report with the ",
    "model, evaluation-data version, and analysis code.</div></footer></body></html>"
  )
}

render_model_tuning <- function(result) {
  tuning <- result$tuning
  if (!inherits(tuning, "autoxplain_tuning")) return("")
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
    "remaining fold. Preprocessing was learned again inside every fold. The outer holdout ",
    "shown in the evaluation section did not participate in this choice.</p>",
    "<div class=\"cards comparison-cards\">",
    metric_card("Configurations", as.character(nrow(candidates)),
                paste(length(unique(candidates$family)), "model families")),
    metric_card("Training folds", as.character(tuning$folds_used),
                "Fold-specific preprocessing"),
    metric_card("Resampling choice", selected$model[[1L]],
                selected$configuration_id[[1L]]),
    metric_card("Final fitted configuration", final$model[[1L]],
                paste0(final_id, if (fallback_used) "; recorded fallback" else "; refit succeeded")),
    metric_card("Resampled score", report_number(selected$cv_score[[1L]], 4L),
                paste0(pretty_metric(tuning$metric), "; lower is better")),
    "</div><div class=\"columns\"><div><h3>Selection rule</h3><p>",
    html_escape(tuning_rule_label(tuning$selection_rule)),
    ". The default one-standard-error rule first uses the reviewed family priority, then ",
    "prefers the least-flexible eligible setting inside that family. Use the best-score ",
    "rule when predictive score alone should decide.</p></div>",
    "<div><h3>Selected settings</h3><p><strong>",
    html_escape(selected$model[[1L]]), ":</strong> ",
    html_escape(selected$hyperparameters[[1L]]), ".</p>",
    if (fallback_used) paste0(
      "<p><strong>Final fallback settings (", html_escape(final_id), "):</strong> ",
      html_escape(final$hyperparameters[[1L]]), ".</p>"
    ) else "",
    "</div></div>",
    html_table(display, digits = 5L),
    "<p class=\"microcopy\"><strong>Within-family flexibility proxy:</strong> each family ",
    "uses the definition recorded in <code>tuning_results(result)$learner_manifest</code>. ",
    "Those values order settings only inside the same family; their units are not comparable ",
    "across linear, additive, tree, ensemble, kernel, neighbor, or neural models.</p>",
    if (failed) paste0(
      "<p class=\"callout\"><strong>", failed,
      " configuration(s) failed.</strong> They were not eligible for selection; inspect ",
      "<code>tuning_results(result)$fold_scores</code> for the recorded errors.</p>"
    ) else "",
    render_tuning_refit_status(tuning, selected$configuration_id[[1L]], final_id),
    "<p class=\"callout\"><strong>Do not quote the resampled tuning score as final performance.</strong> ",
    "It guided model selection. The held-out result above is the separate estimate of how the ",
    "selected, refitted model performed on unseen rows.</p>",
    "<p class=\"microcopy\">", html_escape(tuning$scope_note), "</p></section>"
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
  if (is.null(subgroups)) return("")
  table <- subgroups$performance
  secondary <- if (identical(subgroups$task, "regression")) "mae" else "accuracy"
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
    if (name %in% names(labels)) return(unname(labels[[name]]))
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
    html_escape(pretty_metric(subgroups$primary_metric)), " than the overall held-out result.</p>",
    "<div class=\"cards\">",
    metric_card("Compared by", subgroups$by, "Chosen explicitly for this report"),
    metric_card("Observed groups", as.character(subgroups$n_groups),
                "Values present in the evaluation rows"),
    metric_card("Largest score gap", report_number(subgroups$largest_observed_gap, 4L),
                pretty_metric(subgroups$primary_metric)),
    metric_card("Small groups", as.character(small_groups),
                paste0("Fewer than ", subgroups$min_rows, " evaluation rows")),
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
  if (length(result$models) <= 2L) return("")
  tradeoffs <- tryCatch(
    model_tradeoffs(result),
    error = function(error) NULL
  )
  if (is.null(tradeoffs)) return("")
  performance_metric <- attr(tradeoffs, "performance_metric")
  complexity_metric <- attr(tradeoffs, "complexity_metric")
  tradeoff_kind <- behavior_tradeoff_kind(complexity_metric)
  higher_is_better <- isTRUE(attr(tradeoffs, "higher_is_better"))
  best_index <- if (higher_is_better) {
    which.max(tradeoffs[[performance_metric]])[[1L]]
  } else {
    which.min(tradeoffs[[performance_metric]])[[1L]]
  }
  smallest_index <- which.min(tradeoffs[[complexity_metric]])[[1L]]
  display <- data.frame(
    Model = tradeoffs$model,
    Role = tradeoffs$role,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  display[[pretty_metric(performance_metric)]] <- tradeoffs[[performance_metric]]
  display[[pretty_complexity(complexity_metric)]] <- tradeoffs[[complexity_metric]]
  display[["Pareto-efficient"]] <- tradeoffs$pareto_optimal
  selection_explanation <- if (inherits(result$tuning, "autoxplain_tuning")) {
    paste0(
      "<p><strong>The primary model was chosen before this holdout was opened.</strong> ",
      "Training-only resampling selected and refitted it; the candidate ranks below describe ",
      "their later performance on the same outer evaluation rows.</p>"
    )
  } else {
    paste0(
      "<p><strong>The primary model remains pre-specified.</strong> Candidate ranks are ",
      "descriptive; selecting a winner on these same held-out rows and quoting its score as ",
      "final performance would be optimistic.</p>"
    )
  }
  paste0(
    "<section id=\"models\" class=\"comparison-section\" aria-labelledby=\"models-title\">",
    "<p class=\"eyebrow\">Model comparison</p>",
    "<h2 id=\"models-title\">What trade-offs did the candidates make?</h2>",
    "<p>AutoXplainR compares the supplied candidates on two visible dimensions rather than ",
    "hiding judgment inside one weighted score. A Pareto-efficient model is not beaten by ",
    "another supplied model on both held-out performance and the displayed ",
    html_escape(tradeoff_kind), ".</p>",
    "<div class=\"cards comparison-cards\">",
    metric_card("Compared models", as.character(nrow(tradeoffs)),
                "Primary, candidates, and simple baseline"),
    metric_card("Pareto-efficient", as.character(sum(tradeoffs$pareto_optimal)),
                "Not dominated on both displayed dimensions"),
    metric_card("Best observed score", tradeoffs$model[[best_index]],
                paste0(pretty_metric(performance_metric), " on this holdout")),
    metric_card("Lowest trade-off proxy", tradeoffs$model[[smallest_index]],
                paste0(pretty_complexity(complexity_metric), "; ", tradeoff_kind)),
    "</div><div class=\"tradeoff-layout\"><div>", tradeoff_svg(tradeoffs),
    "</div><div class=\"tradeoff-explainer\"><h3>How to read this</h3><ol>",
    "<li>Up means better held-out performance.</li>",
    "<li>Left means lower ", html_escape(pretty_complexity(complexity_metric)),
    ", used here as a ", html_escape(tradeoff_kind), ".</li>",
    "<li>Outlined points form the supplied Pareto frontier.</li>",
    "</ol>", selection_explanation, "</div></div>",
    html_table(display, digits = 4L),
    render_behavior_comparison(result),
    render_prediction_ambiguity(result),
    "<p class=\"microcopy\"><strong>Trade-off boundary:</strong> ",
    if (identical(tradeoff_kind, "resource proxy")) {
      "Serialized model size or runtime measures operational resource use, not structural complexity. "
    } else {
      "The displayed proxy does not prove which available model capacity was used. "
    },
    html_escape(attr(tradeoffs, "scope_note")), "</p></section>"
  )
}

render_behavior_comparison <- function(result) {
  behavior <- tryCatch(
    compare_model_behavior(result),
    error = function(error) NULL
  )
  if (is.null(behavior)) return("")
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
    "columns are reviewed behavior cards. They say what each family can represent; they do ",
    "not show that this fitted model actually used those patterns.</p>",
    html_table(display, digits = 4L),
    "<p><strong>Computed evidence from this analysis:</strong> held-out performance and ",
    "paired prediction disagreement are calculated on common evaluation rows. Repeated ",
    "permutation feature importance in the Patterns section is also computed evidence of ",
    "model reliance, not a property guaranteed by the family card.</p>",
    computed_html,
    "</div>"
  )
}

render_prediction_ambiguity <- function(result) {
  ambiguity <- tryCatch(
    prediction_ambiguity(result),
    error = function(error) NULL
  )
  if (is.null(ambiguity)) return("")
  if (identical(ambiguity$task, "regression")) {
    top <- ambiguity$rows[order(
      ambiguity$rows$prediction_range, decreasing = TRUE
    ), c(
      "evaluation_row", "observed", "prediction_min", "prediction_max",
      "prediction_range"
    ), drop = FALSE]
    top <- head(top, 5L)
    names(top) <- c("Row", "Observed", "Lowest prediction", "Highest prediction", "Range")
    cards <- paste0(
      "<div class=\"cards diagnostic-cards\">",
      metric_card("Compared candidates", as.character(ambiguity$n_models),
                  "Simple baseline excluded"),
      metric_card("Median prediction range",
                  report_number(ambiguity$median_prediction_range, 4L),
                  "In outcome units"),
      metric_card("90th-percentile range",
                  report_number(ambiguity$p90_prediction_range, 4L),
                  "Nine in ten rows were below this"),
      metric_card("Largest prediction range",
                  report_number(ambiguity$max_prediction_range, 4L),
                  "Most specification-sensitive row"),
      "</div>"
    )
  } else {
    top <- ambiguity$rows[order(
      ambiguity$rows$probability_distance, decreasing = TRUE
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
      metric_card("Compared candidates", as.character(ambiguity$n_models),
                  "Simple baseline excluded"),
      metric_card("Rows with class disagreement",
                  format_percent(ambiguity$class_disagreement_rate),
                  "Candidates chose different labels"),
      metric_card("Median probability distance",
                  format_percent(ambiguity$median_probability_distance),
                  ambiguity$probability_distance),
      metric_card("Largest probability distance",
                  format_percent(ambiguity$max_probability_distance),
                  "Most specification-sensitive row"),
      "</div>"
    )
  }
  paste0(
    "<div class=\"ambiguity\"><h3>Where did supplied model choices disagree?</h3>",
    "<p>The same held-out rows were scored by every supplied non-baseline candidate. ",
    "A large gap means the answer depends on model specification, even when the data row ",
    "is unchanged.</p>", cards, html_table(top, digits = 4L),
    "<p class=\"callout\"><strong>Disagreement is a review signal, not an error bar.</strong> ",
    "The compared candidates can have very different held-out performance; read this beside ",
    "the score table above. It does not identify the correct prediction or provide uncertainty ",
    "coverage.</p></div>"
  )
}

tradeoff_svg <- function(tradeoffs) {
  performance_metric <- attr(tradeoffs, "performance_metric")
  complexity_metric <- attr(tradeoffs, "complexity_metric")
  higher_is_better <- isTRUE(attr(tradeoffs, "higher_is_better"))
  width <- 720
  height <- 360
  left <- 76
  right <- 36
  top <- 34
  bottom <- 68
  x <- scale_plot_values(tradeoffs[[complexity_metric]], left, width - right)
  performance <- if (higher_is_better) {
    tradeoffs[[performance_metric]]
  } else {
    -tradeoffs[[performance_metric]]
  }
  y <- scale_plot_values(performance, height - bottom, top)
  frontier <- which(tradeoffs$pareto_optimal)
  frontier <- frontier[order(x[frontier])]
  line <- if (length(frontier) > 1L) {
    points <- paste0(round(x[frontier], 2L), ",", round(y[frontier], 2L), collapse = " ")
    paste0("<polyline points=\"", points, "\" class=\"pareto-line\"/>")
  } else {
    ""
  }
  points <- vapply(seq_len(nrow(tradeoffs)), function(index) {
    role <- if (tradeoffs$role[[index]] %in% c("primary", "baseline", "candidate")) {
      tradeoffs$role[[index]]
    } else {
      "candidate"
    }
    label <- substr(tradeoffs$model[[index]], 1L, 28L)
    label_on_left <- x[[index]] > width - 190
    label_x <- x[[index]] + if (label_on_left) -13 else 13
    label_y <- y[[index]] + if (y[[index]] < top + 24) 24 else -11
    paste0(
      "<g><circle cx=\"", round(x[[index]], 2L), "\" cy=\"", round(y[[index]], 2L),
      "\" r=\"", if (tradeoffs$pareto_optimal[[index]]) "10" else "8",
      "\" class=\"tradeoff-point tradeoff-", role,
      if (tradeoffs$pareto_optimal[[index]]) " tradeoff-pareto" else "", "\"/>",
      "<text x=\"", round(label_x, 2L), "\" y=\"", round(label_y, 2L),
      "\" text-anchor=\"", if (label_on_left) "end" else "start",
      "\" class=\"point-label\">", html_escape(label), "</text></g>"
    )
  }, character(1))
  aria <- paste(
    "Model trade-off chart comparing", pretty_metric(performance_metric), "and",
    pretty_complexity(complexity_metric), "for", nrow(tradeoffs), "models."
  )
  paste0(
    "<svg class=\"tradeoff-plot\" viewBox=\"0 0 ", width, " ", height,
    "\" role=\"img\" aria-label=\"", html_escape(aria), "\">",
    "<line x1=\"", left, "\" y1=\"", height - bottom, "\" x2=\"", width - right,
    "\" y2=\"", height - bottom, "\" class=\"chart-axis\"/>",
    "<line x1=\"", left, "\" y1=\"", top, "\" x2=\"", left,
    "\" y2=\"", height - bottom, "\" class=\"chart-axis\"/>",
    "<text x=\"", (left + width - right) / 2, "\" y=\"", height - 18,
    "\" class=\"axis-label\">lower ", html_escape(pretty_complexity(complexity_metric)),
    " &rarr;</text><text x=\"18\" y=\"", (top + height - bottom) / 2,
    "\" transform=\"rotate(-90 18 ", (top + height - bottom) / 2,
    ")\" class=\"axis-label\">better ",
    html_escape(pretty_metric(performance_metric)), " &rarr;</text>",
    line, paste(points, collapse = ""), "</svg>"
  )
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
    model_size_kb = "model size (KB)",
    size_mb = "model size (MB)",
    model_size = "model size",
    training_time_ms = "training time (ms)",
    training_time_s = "training time (s)",
    prediction_time_ms = "prediction time (ms)",
    complexity = "model complexity"
  )
  if (metric %in% names(labels)) unname(labels[[metric]]) else gsub("_", " ", metric)
}

model_report_evaluation <- function(result, audit) {
  evaluation <- result$evaluation
  if (!is.null(evaluation$primary_metric) && "main_model" %in% names(evaluation$metrics)) {
    metric <- evaluation$primary_metric
    primary <- evaluation$metrics$main_model[[metric]]
    baseline <- evaluation$metrics$simple_baseline[[metric]] %||% NA_real_
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
      diagnostics = evaluation$diagnostics
    ))
  }
  best <- which.min(audit$performance$relative_gap)[[1L]]
  metric <- audit$config$metric
  definition_key <- if (metric == "logloss") "log_loss" else metric
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
    diagnostics = NULL
  )
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
    if (name == "rank") return("Rank")
    if (name %in% c("model", "model_id")) return("Model")
    if (name == "role") return("Role")
    pretty_metric(name)
  }, character(1))
  table
}

render_model_overview <- function(result, evaluation) {
  task_label <- switch(
    result$task,
    regression = "predict a numeric value",
    binary = "choose between two classes",
    multiclass = "choose among several classes"
  )
  improvement <- if (is.finite(evaluation$improvement %||% NA_real_)) {
    format_percent(evaluation$improvement)
  } else {
    "not available"
  }
  conclusion <- if (isTRUE(evaluation$beats_baseline)) {
    paste0(
      "The primary model improved ", html_escape(pretty_metric(evaluation$metric)), " by ",
      html_escape(improvement), " relative to the simple baseline on held-out rows."
    )
  } else if (identical(evaluation$beats_baseline, FALSE)) {
    paste0(
      "The primary model did not beat the simple baseline on ",
      html_escape(pretty_metric(evaluation$metric)),
      ". Treat its fitted patterns as exploratory rather than useful predictive evidence."
    )
  } else {
    "A simple-baseline comparison was not available for this fitted-engine result."
  }
  paste0(
    "<section id=\"overview\" aria-labelledby=\"overview-title\"><p class=\"eyebrow\">Start here</p>",
    "<h2 id=\"overview-title\">The modeling question</h2>",
    "<p>This is a <strong>", html_escape(result$task), "</strong> task: ",
    html_escape(task_label), " for <strong>", html_escape(result$target_column), "</strong>.</p>",
    "<p class=\"verdict\">", conclusion, "</p><div class=\"cards\">",
    metric_card("Primary model", report_number(evaluation$primary, 4L),
                pretty_metric(evaluation$metric)),
    metric_card("Simple baseline", report_number(evaluation$baseline, 4L),
                "Predicts without using input features"),
    metric_card("Relative improvement", improvement,
                "Positive means the primary error was lower"),
    metric_card("Held-out rows", as.character(evaluation$rows),
                "Not used to fit the primary model"),
    "</div>", render_guided_notes(evaluation$notes), "</section>"
  )
}

render_model_evaluation <- function(result, evaluation) {
  paste0(
    "<section id=\"evaluation\" aria-labelledby=\"evaluation-title\"><p class=\"eyebrow\">Unseen-data check</p>",
    "<h2 id=\"evaluation-title\">Did the model generalize?</h2>",
    "<p class=\"callout\"><strong>", html_escape(pretty_metric(evaluation$metric)),
    ":</strong> ", html_escape(evaluation$definition), "</p>",
    "<p>The table reports every computed held-out metric. Compare models using the metric definitions, ",
    "not the rank column alone.</p>", html_table(evaluation$table, digits = 4L),
    render_metric_definitions(result), render_prediction_diagnostics(result, evaluation$diagnostics),
    "</section>"
  )
}

render_guided_notes <- function(notes) {
  if (is.null(notes) || !nrow(notes)) return("")
  items <- vapply(seq_len(nrow(notes)), function(index) {
    paste0(
      "<article class=\"guided-note guided-note-", html_escape(notes$severity[[index]]), "\">",
      "<h3>", html_escape(notes$message[[index]]), "</h3><p><strong>Next step:</strong> ",
      html_escape(notes$recommendation[[index]]), "</p></article>"
    )
  }, character(1))
  paste0("<div class=\"guided-notes\"><h3>Important context for these scores</h3>",
         paste(items, collapse = ""), "</div>")
}

render_prediction_diagnostics <- function(result, diagnostics) {
  if (is.null(diagnostics)) return("")
  missingness_html <- render_missingness_shift(diagnostics$missingness_shift)
  if (result$task == "regression") {
    return(paste0(
      "<h3>How large were individual errors?</h3><div class=\"cards diagnostic-cards\">",
      metric_card("Mean error", report_number(diagnostics$mean_error, 4L),
                  "Observed minus predicted; near zero means little average bias"),
      metric_card("Median absolute error", report_number(diagnostics$median_absolute_error, 4L),
                  "Half of absolute errors were below this value"),
      metric_card("90th-percentile error", report_number(diagnostics$p90_absolute_error, 4L),
                  "Nine in ten absolute errors were below this value"),
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
  if (!identical(result$task, "binary")) return("")
  diagnostic <- tryCatch(
    threshold_diagnostics(result, thresholds = c(0.3, 0.5, 0.7)),
    error = function(error) NULL
  )
  if (is.null(diagnostic)) return("")
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
    "is chosen using these held-out rows, evaluate it again on different representative data.</p>"
  )
}

render_missingness_shift <- function(shift) {
  if (is.null(shift) || shift$n_with_missing == 0L) return("")
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
      "Check whether collection or pipeline behavior changed before treating the held-out ",
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
    metric_card("Inputs with missing values", as.character(shift$n_with_missing),
                "Observed before preprocessing"),
    metric_card("Flagged model inputs", as.character(shift$n_flagged_model_features),
                paste0("At least ", format_percent(shift$threshold), " absolute change")),
    metric_card("Largest observed shift", format_percent(shift$largest_shift),
                "Absolute training-versus-evaluation difference"),
    "</div>", verdict, html_table(display, digits = 3L),
    "<p class=\"microcopy\">", html_escape(shift$scope_note), "</p>"
  )
}

render_calibration_diagnostic <- function(result, calibration) {
  if (is.null(calibration)) return("")
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
    "<p>Calibration compares ", event, " with how often that event occurred on held-out rows. ",
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
      "What actually happened on held-out rows"
    ),
    metric_card(
      "Binned calibration gap", format_percent(calibration$calibration_error),
      "Average absolute discrepancy; lower is better"
    ),
    "</div>", html_table(display, digits = 3L),
    "<p class=\"microcopy\">This binned gap is descriptive and changes with the held-out ",
    "sample and grouping. It is not an uncertainty interval or a guarantee for future data. ",
    "Use log loss and Brier score alongside it.</p>"
  )
}

render_metric_definitions <- function(result) {
  definitions <- result$evaluation$metric_definitions
  if (is.null(definitions)) return("")
  names(definitions) <- vapply(names(definitions), pretty_metric, character(1))
  paste0("<details class=\"learn-more\"><summary>Definitions for every metric</summary>",
         definition_list(definitions), "</details>")
}

pretty_metric <- function(metric) {
  labels <- c(
    rmse = "RMSE", mae = "MAE", r_squared = "R-squared",
    log_loss = "log loss", logloss = "log loss", accuracy = "accuracy",
    brier_score = "Brier score",
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
  if (is.null(narrative) || !nzchar(narrative)) return("")
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

render_guided_importance <- function(importance) {
  model <- if ("main_model" %in% importance$model) "main_model" else importance$model[[1L]]
  item <- importance[importance$model == model, , drop = FALSE]
  item <- item[order(item$importance, decreasing = TRUE), , drop = FALSE]
  rows <- vapply(seq_len(nrow(item)), function(index) {
    paste0(
      "<tr><th scope=\"row\">", html_escape(item$feature[[index]]), "</th>",
      "<td>", report_number(item$importance[[index]], 4L), "</td>",
      "<td>", html_escape(format_percent(item$sign_stability[[index]])), "</td>",
      "<td>", html_escape(item$claim[[index]]), "</td></tr>"
    )
  }, character(1))
  paste0(
    "<div class=\"table-wrap\"><table><thead><tr><th>Input</th><th>Reliance score</th>",
    "<th>Repeat consistency</th><th>How to communicate it</th></tr></thead><tbody>",
    paste(rows, collapse = ""), "</tbody></table></div>",
    "<p class=\"microcopy\">Reliance scores are changes in the selected performance metric after shuffling. ",
    "Repeat consistency is computational stability, not population confidence.</p>"
  )
}

render_effects <- function(effects, result) {
  if (!length(effects)) return("")
  cards <- vapply(names(effects), function(feature) {
    effect <- effects[[feature]]
    paste0(
      "<article class=\"effect-card\"><div><p class=\"eyebrow\">",
      html_escape(toupper(attr(effect, "method"))), " fitted effect</p><h3>",
      html_escape(feature), "</h3><p>", html_escape(effect_plain_summary(effect, result)),
      "</p></div>", effect_svg(effect, feature), "</article>"
    )
  }, character(1))
  paste0(
    "<h3>What direction did the fitted pattern take?</h3>",
    "<p>These curves change one input within observed support and summarize the model's prediction. ",
    "They are descriptions of the fitted model, not intervention effects.</p>",
    "<div class=\"effect-grid\">", paste(cards, collapse = ""), "</div>"
  )
}

effect_plain_summary <- function(effect, result) {
  method <- attr(effect, "method")
  y_name <- if (method == "ale") "accumulated_effect" else "partial_dependence"
  y <- effect[[y_name]]
  x <- effect[[1L]]
  multiclass_label <- if (result$task == "multiclass") {
    levels(result$training_data[[result$target_column]])[[1L]]
  } else {
    NULL
  }
  target <- switch(
    result$task,
    regression = "predicted value",
    binary = "positive-class probability",
    multiclass = paste0("probability for class ", multiclass_label)
  )
  association <- if (is.numeric(x) && length(unique(x)) > 1L) {
    suppressWarnings(stats::cor(x, y, method = "spearman"))
  } else {
    NA_real_
  }
  direction <- if (is.finite(association) && association >= 0.6) {
    "generally increased"
  } else if (is.finite(association) && association <= -0.6) {
    "generally decreased"
  } else {
    "varied without one clear direction"
  }
  paste0(
    "Within the evaluated range, the model's ", target, " ", direction,
    ". The displayed effect spans ", report_number(diff(range(y, na.rm = TRUE)), 4L),
    " prediction units."
  )
}

effect_svg <- function(effect, feature) {
  method <- attr(effect, "method")
  values <- effect[[if (method == "ale") "accumulated_effect" else "partial_dependence"]]
  if (!length(values) || any(!is.finite(values))) return("")
  width <- 520
  height <- 170
  pad <- 22
  x <- seq(pad, width - pad, length.out = length(values))
  limits <- range(values)
  y <- if (diff(limits) == 0) {
    rep(height / 2, length(values))
  } else {
    height - pad - (values - limits[[1L]]) / diff(limits) * (height - 2 * pad)
  }
  points <- paste0(
    format(round(x, 2L), trim = TRUE), ",", format(round(y, 2L), trim = TRUE),
    collapse = " "
  )
  aria <- paste(
    toupper(method), "curve for", feature, "ranging from",
    report_number(limits[[1L]], 3L), "to", report_number(limits[[2L]], 3L)
  )
  paste0(
    "<svg class=\"effect-plot\" viewBox=\"0 0 ", width, " ", height,
    "\" role=\"img\" aria-label=\"", html_escape(aria), "\">",
    "<line x1=\"", pad, "\" y1=\"", height - pad, "\" x2=\"", width - pad,
    "\" y2=\"", height - pad, "\" class=\"axis\"/>",
    "<polyline points=\"", points, "\" class=\"effect-line\"/></svg>"
  )
}

render_reliability_section <- function(audit) {
  summary <- audit$summary
  paste0(
    "<section id=\"reliability\" aria-labelledby=\"reliability-title\"><div class=\"section-head\"><div>",
    "<p class=\"eyebrow\">How cautious should I be?</p><h2 id=\"reliability-title\">Explanation reliability</h2>",
    "</div><span class=\"grade grade-", tolower(summary$grade), "\" aria-label=\"Diagnostic grade ",
    html_escape(summary$grade), "\">", html_escape(summary$grade), "</span></div>",
    "<p class=\"callout\"><strong>This grade is a triage aid, not a certification.</strong> ",
    html_escape(summary$grade_note), "</p>",
    "<div class=\"cards\">",
    metric_card("Repeat-stable claims", format_percent(summary$stable_claim_rate),
                "Feature statements graded A or B"),
    metric_card("Competitive models", paste0(summary$n_near_optimal, " / ", summary$n_models),
                "Within the configured performance tolerance"),
    metric_card("Rank agreement", report_number(summary$mean_rank_agreement),
                "Agreement among competitive supplied models"),
    metric_card("Largest association", report_number(summary$max_association),
                "High values pressure marginal explanations"),
    "</div><h3>Warnings and next actions</h3>", render_findings(audit$findings),
    "<details class=\"advanced\"><summary>Open the technical evidence audit</summary>",
    "<h3>Supplied models</h3>", html_table(audit$performance, digits = 5L),
    "<h3>Repeat-level importance evidence</h3>", render_importance(audit$importance),
    "<h3>Feature association diagnostics</h3>", html_table(audit$dependence, digits = 3L),
    "<p>Permutation intervals describe random shuffling variation, not uncertainty about a population. ",
    "Near-optimal status is relative only to models supplied here.</p></details></section>"
  )
}

render_model_provenance <- function(result, audit) {
  paste0(
    "<section id=\"provenance\" aria-labelledby=\"provenance-title\"><p class=\"eyebrow\">Reproducibility</p>",
    "<h2 id=\"provenance-title\">How this result was produced</h2>",
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
      "Permutation repeats" = as.character(audit$config$n_repeats),
      "Seed" = as.character(audit$config$seed),
      "Explainer IDs" = paste(audit$provenance$explainer_fingerprints, collapse = ", ")
    )), "</section>"
  )
}

#' Render a standalone explanation evidence report
#'
#' Creates a dependency-free, accessible HTML report from an explanation audit.
#' The report leads with limitations and evidence grades rather than presenting
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
    "<section aria-labelledby=\"summary-title\"><div class=\"section-head\"><div>",
    "<p class=\"eyebrow\">Decision summary</p><h2 id=\"summary-title\">Evidence at a glance</h2>",
    "</div><span class=\"grade grade-", tolower(summary$grade), "\" aria-label=\"Evidence grade ",
    html_escape(summary$grade), "\">", html_escape(summary$grade), "</span></div>",
    "<p class=\"callout\"><strong>Scope:</strong> ", html_escape(summary$grade_note), "</p>",
    "<div class=\"cards\">",
    metric_card("Stable claims", format_percent(summary$stable_claim_rate),
                "Model-feature claims graded A or B"),
    metric_card("Near-optimal models", paste0(summary$n_near_optimal, " / ", summary$n_models),
                "Within the configured performance tolerance"),
    metric_card("Rank agreement", report_number(summary$mean_rank_agreement),
                "Mean Spearman correlation across near-optimal models"),
    metric_card("Max dependence", report_number(summary$max_association),
                "Largest pairwise feature association"),
    "</div></section>",
    "<section id=\"findings\" aria-labelledby=\"findings-title\"><p class=\"eyebrow\">Triage</p>",
    "<h2 id=\"findings-title\">Findings and actions</h2>", render_findings(findings), "</section>",
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
    "evaluation-data version, and analysis code.</div></footer></body></html>"
  )
}

render_findings <- function(findings) {
  cards <- vapply(seq_len(nrow(findings)), function(index) {
    severity <- findings$severity[[index]]
    paste0(
      "<article class=\"finding finding-", html_escape(severity), "\">",
      "<div><span class=\"severity\">", html_escape(severity), "</span>",
      "<code>", html_escape(findings$code[[index]]), "</code></div>",
      "<h3>", html_escape(findings$message[[index]]), "</h3>",
      "<p><strong>Evidence:</strong> ", html_escape(findings$evidence[[index]]), "</p>",
      "<p><strong>Next action:</strong> ", html_escape(findings$recommendation[[index]]), "</p></article>"
    )
  }, character(1))
  paste0("<div class=\"findings\">", paste(cards, collapse = ""), "</div>")
}

render_importance <- function(importance) {
  models <- unique(importance$model)
  panels <- vapply(models, function(model) {
    item <- importance[importance$model == model, , drop = FALSE]
    item <- item[order(item$importance, decreasing = TRUE), , drop = FALSE]
    maximum <- max(abs(item$importance), na.rm = TRUE)
    if (!is.finite(maximum) || maximum == 0) maximum <- 1
    rows <- vapply(seq_len(nrow(item)), function(index) {
      width <- min(100, 100 * abs(item$importance[[index]]) / maximum)
      direction <- if (item$importance[[index]] < 0) "negative" else "positive"
      paste0(
        "<tr><th scope=\"row\">", html_escape(item$feature[[index]]), "</th>",
        "<td class=\"bar-cell\"><span class=\"bar bar-", direction,
        "\" style=\"width:", format(width, trim = TRUE, scientific = FALSE), "%\"></span></td>",
        "<td>", report_number(item$importance[[index]], 4L), "</td>",
        "<td>[", report_number(item$conf_low[[index]], 4L), ", ",
        report_number(item$conf_high[[index]], 4L), "]</td>",
        "<td><span class=\"mini-grade grade-", tolower(as.character(item$evidence_grade[[index]])),
        "\">", html_escape(as.character(item$evidence_grade[[index]])), "</span></td>",
        "<td>", html_escape(item$claim[[index]]), "</td></tr>"
      )
    }, character(1))
    paste0(
      "<article class=\"model-panel\"><h3>", html_escape(model), "</h3>",
      "<div class=\"table-wrap\"><table><thead><tr><th>Feature</th><th>Relative magnitude</th>",
      "<th>Importance</th><th>MC interval</th><th>Grade</th><th>Permitted claim</th>",
      "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div></article>"
    )
  }, character(1))
  paste(panels, collapse = "")
}

html_table <- function(data, digits = 3L) {
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
      paste0("<td>", html_escape(as.character(value)), "</td>")
    }, character(1))
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  }, character(1))
  paste0("<div class=\"table-wrap\"><table><thead><tr>", headers,
         "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div>")
}

metric_card <- function(label, value, detail) {
  paste0("<article class=\"metric\"><p>", html_escape(label), "</p><strong>",
         html_escape(value), "</strong><small>", html_escape(detail), "</small></article>")
}

definition_list <- function(values) {
  entries <- paste0("<dt>", html_escape(names(values)), "</dt><dd>",
                    html_escape(unname(values)), "</dd>", collapse = "")
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
  if (length(x) != 1L || !is.finite(x)) return("n/a")
  format(round(x, digits), nsmall = min(2L, digits), trim = TRUE, scientific = FALSE)
}

report_css <- function() {
  paste0(
    ":root{--ink:#15231f;--muted:#5d6b66;--paper:#f6f7f2;--card:#fff;--line:#d9dfd8;",
    "--green:#176b4d;--mint:#dff3e7;--amber:#9a5b00;--amber-bg:#fff2d4;--red:#a93131;",
    "--red-bg:#fde5e2;--blue:#244d8f}*{box-sizing:border-box}html{scroll-behavior:smooth}",
    "body{margin:0;background:var(--paper);color:var(--ink);font-family:Inter,ui-sans-serif,system-ui,-apple-system,",
    "BlinkMacSystemFont,'Segoe UI',sans-serif;line-height:1.55}.shell{width:min(1120px,calc(100% - 40px));margin:auto}",
    ".skip{position:absolute;left:-9999px}.skip:focus{left:12px;top:12px;background:#fff;padding:10px;z-index:5}",
    "header{background:#102a23;color:#f8fff9;padding:64px 0 34px;border-bottom:5px solid #69c194}h1{font-size:clamp(2.2rem,5vw,4.4rem);",
    "line-height:1.02;max-width:850px;margin:8px 0 18px;letter-spacing:-.04em}.lede{max-width:760px;color:#cbe0d7;font-size:1.1rem}",
    ".eyebrow{text-transform:uppercase;letter-spacing:.14em;font-weight:800;font-size:.76rem;color:#55a97f;margin:0}",
    "nav{display:flex;flex-wrap:wrap;gap:9px;margin-top:28px}nav a{color:#effff5;text-decoration:none;border:1px solid #45675c;border-radius:999px;padding:7px 13px}",
    "main{padding:28px 0 70px}section{background:var(--card);border:1px solid var(--line);border-radius:18px;padding:clamp(22px,4vw,42px);margin:20px 0;box-shadow:0 8px 24px #173b2b0b}",
    "h2{font-size:clamp(1.55rem,3vw,2.25rem);line-height:1.15;margin:7px 0 18px;letter-spacing:-.025em}h3{line-height:1.25}.section-head{display:flex;align-items:center;justify-content:space-between;gap:20px}",
    ".grade{display:grid;place-items:center;width:82px;height:82px;border-radius:22px;font-weight:900;font-size:2.2rem}.grade-a,.mini-grade.grade-a{background:var(--mint);color:var(--green)}",
    ".grade-b,.mini-grade.grade-b{background:#e7f0fc;color:var(--blue)}.grade-c,.mini-grade.grade-c{background:var(--amber-bg);color:var(--amber)}.grade-d,.mini-grade.grade-d{background:var(--red-bg);color:var(--red)}",
    ".callout{background:#eef3ee;border-left:4px solid #628071;padding:14px 16px;border-radius:7px}.verdict{font-size:1.18rem;font-weight:750;background:#e8f5ed;border-left:5px solid var(--green);padding:18px 20px;border-radius:9px}.microcopy{color:var(--muted);font-size:.9rem}.cards{display:grid;grid-template-columns:repeat(4,1fr);gap:12px;margin-top:22px}.diagnostic-cards{grid-template-columns:repeat(3,1fr)}.comparison-cards .metric strong{font-size:1.2rem;line-height:1.2;min-height:2.9em}.guided-notes{margin-top:24px}.guided-note{border:1px solid #e3c783;border-left:5px solid #c77a0a;background:#fffaf0;border-radius:11px;padding:14px 18px;margin:10px 0}.guided-note h3{font-size:1rem;margin:0 0 6px}.guided-note p{margin:0;color:var(--muted)}.guided-note-warning{border-color:#e3a2a0;border-left-color:var(--red);background:#fff8f7}",
    ".metric{background:#f5f7f3;border:1px solid var(--line);border-radius:13px;padding:17px}.metric p{margin:0;color:var(--muted);font-weight:700;font-size:.83rem}.metric strong{display:block;font-size:1.8rem;margin:5px 0}.metric small{color:var(--muted)}",
    ".findings{display:grid;gap:12px}.finding{border:1px solid var(--line);border-left-width:5px;border-radius:12px;padding:18px}.finding h3{margin:9px 0}.finding p{margin:7px 0}.finding-critical{border-left-color:var(--red);background:#fff9f8}.finding-warning{border-left-color:#d18718;background:#fffdf6}.finding-note{border-left-color:#5283b7;background:#f9fcff}",
    ".severity{text-transform:uppercase;font-size:.72rem;font-weight:900;letter-spacing:.08em;margin-right:9px}code{background:#edf1ec;border-radius:5px;padding:2px 6px;color:#415149}.table-wrap{overflow-x:auto;border:1px solid var(--line);border-radius:11px}table{border-collapse:collapse;width:100%;font-size:.9rem;background:#fff}th,td{text-align:left;padding:11px 12px;border-bottom:1px solid #e7ebe6;vertical-align:middle}thead th{background:#edf2ed;font-size:.78rem;text-transform:uppercase;letter-spacing:.04em}tbody tr:last-child td,tbody tr:last-child th{border-bottom:0}",
    ".model-panel{margin:28px 0}.bar-cell{min-width:150px}.bar{display:block;height:9px;border-radius:999px;min-width:2px}.bar-positive{background:#2f8c65}.bar-negative{background:#c6534f}.mini-grade{display:inline-grid;place-items:center;width:30px;height:30px;border-radius:8px;font-weight:900}",
    ".columns{display:grid;grid-template-columns:1fr 1fr;gap:18px}.columns>div{background:#f5f7f3;padding:18px 22px;border-radius:12px}.columns h3{margin-top:0}.narrative{white-space:pre-wrap;overflow-wrap:anywhere;background:#f5f7f3;border:1px solid var(--line);border-radius:12px;padding:18px;font:inherit}.learn-more,.advanced{margin-top:24px;border:1px solid var(--line);border-radius:12px;padding:14px 18px;background:#fbfcfa}.learn-more summary,.advanced summary{cursor:pointer;font-weight:850;color:var(--green);padding:5px}.advanced>h3:first-of-type{margin-top:24px}.tradeoff-layout{display:grid;grid-template-columns:minmax(0,1.7fr) minmax(240px,.8fr);gap:20px;align-items:center;margin:24px 0}.tradeoff-plot{width:100%;height:auto;background:linear-gradient(145deg,#f7faf7,#eef5f0);border:1px solid var(--line);border-radius:14px}.tradeoff-explainer{background:#f5f7f3;border-radius:14px;padding:18px 22px}.tradeoff-explainer h3{margin-top:0}.tradeoff-explainer ol{padding-left:1.25rem}.chart-axis{stroke:#86968f;stroke-width:1.2}.pareto-line{fill:none;stroke:#176b4d;stroke-width:3;stroke-dasharray:7 6}.tradeoff-point{stroke:#fff;stroke-width:2;fill:#6d7c76}.tradeoff-primary{fill:#176b4d}.tradeoff-candidate{fill:#315c9b}.tradeoff-baseline{fill:#b46a22}.tradeoff-pareto{stroke:#102a23;stroke-width:4}.point-label{font-size:12px;font-weight:750;fill:#25352f}.axis-label{font-size:12px;font-weight:800;fill:#53635d;text-anchor:middle}.effect-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:14px}.effect-card{border:1px solid var(--line);border-radius:14px;padding:18px;background:#fbfcfa}.effect-card h3{font-size:1.35rem;margin:5px 0}.effect-plot{width:100%;height:auto;background:#f2f6f2;border-radius:9px}.axis{stroke:#9aa9a2;stroke-width:1}.effect-line{fill:none;stroke:#176b4d;stroke-width:5;stroke-linecap:round;stroke-linejoin:round}dl{display:grid;grid-template-columns:minmax(150px,220px) 1fr;gap:8px 18px}dt{font-weight:800}dd{margin:0;color:var(--muted);overflow-wrap:anywhere}",
    "footer{background:#102a23;color:#cbe0d7;padding:28px 0}@media(max-width:820px){.cards{grid-template-columns:1fr 1fr}.columns,.effect-grid,.tradeoff-layout{grid-template-columns:1fr}}@media(max-width:520px){.shell{width:min(100% - 24px,1120px)}header{padding-top:42px}.cards{grid-template-columns:1fr}.section-head{align-items:flex-start}.grade{width:62px;height:62px;font-size:1.7rem}section{border-radius:12px}.point-label{font-size:10px}dl{grid-template-columns:1fr;gap:3px}dd{margin-bottom:10px}}",
    "@media(prefers-reduced-motion:reduce){html{scroll-behavior:auto}}"
  )
}
