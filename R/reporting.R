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
  directory <- dirname(output_file)
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(directory)) stop("Could not create output directory: ", directory, call. = FALSE)
  writeLines(model_report_html(result, audit, effects, narrative, title), output_file,
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

model_report_html <- function(result, audit, effects, narrative, title) {
  evaluation <- model_report_evaluation(result, audit)
  narrative_html <- render_guided_narrative(narrative)
  paste0(
    "<!doctype html>\n<html lang=\"en\"><head><meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">",
    "<title>", html_escape(title), "</title><style>", report_css(), "</style></head>",
    "<body><a class=\"skip\" href=\"#main\">Skip to report</a>",
    "<header><div class=\"shell\"><p class=\"eyebrow\">AutoXplainR / Guided model report</p>",
    "<h1>", html_escape(title), "</h1>",
    "<p class=\"lede\">What was fitted, how it performed on data kept out of fitting, ",
    "which patterns it used, and how cautiously those patterns should be communicated.</p>",
    "<nav aria-label=\"Report sections\"><a href=\"#overview\">Overview</a>",
    "<a href=\"#evaluation\">Evaluation</a><a href=\"#patterns\">Patterns</a>",
    "<a href=\"#reliability\">Reliability</a><a href=\"#limits\">Limits</a></nav>",
    "</div></header><main id=\"main\" class=\"shell\">",
    render_model_overview(result, evaluation),
    render_model_evaluation(result, evaluation),
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
      table = result$leaderboard,
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
    table = result$leaderboard,
    notes = NULL,
    diagnostics = NULL
  )
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
  if (result$task == "regression") {
    return(paste0(
      "<h3>How large were individual errors?</h3><div class=\"cards diagnostic-cards\">",
      metric_card("Mean error", report_number(diagnostics$mean_error, 4L),
                  "Observed minus predicted; near zero means little average bias"),
      metric_card("Median absolute error", report_number(diagnostics$median_absolute_error, 4L),
                  "Half of absolute errors were below this value"),
      metric_card("90th-percentile error", report_number(diagnostics$p90_absolute_error, 4L),
                  "Nine in ten absolute errors were below this value"),
      "</div>"
    ))
  }
  paste0(
    "<h3>Which classes were confused?</h3><p>Rows on the diagonal are correct predictions. ",
    "Off-diagonal rows show the specific mistakes.</p>",
    html_table(diagnostics$confusion_matrix, digits = 0L)
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
      if (is.numeric(value)) value <- report_number(value, digits)
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
    ".callout{background:#eef3ee;border-left:4px solid #628071;padding:14px 16px;border-radius:7px}.verdict{font-size:1.18rem;font-weight:750;background:#e8f5ed;border-left:5px solid var(--green);padding:18px 20px;border-radius:9px}.microcopy{color:var(--muted);font-size:.9rem}.cards{display:grid;grid-template-columns:repeat(4,1fr);gap:12px;margin-top:22px}.diagnostic-cards{grid-template-columns:repeat(3,1fr)}.guided-notes{margin-top:24px}.guided-note{border:1px solid #e3c783;border-left:5px solid #c77a0a;background:#fffaf0;border-radius:11px;padding:14px 18px;margin:10px 0}.guided-note h3{font-size:1rem;margin:0 0 6px}.guided-note p{margin:0;color:var(--muted)}.guided-note-warning{border-color:#e3a2a0;border-left-color:var(--red);background:#fff8f7}",
    ".metric{background:#f5f7f3;border:1px solid var(--line);border-radius:13px;padding:17px}.metric p{margin:0;color:var(--muted);font-weight:700;font-size:.83rem}.metric strong{display:block;font-size:1.8rem;margin:5px 0}.metric small{color:var(--muted)}",
    ".findings{display:grid;gap:12px}.finding{border:1px solid var(--line);border-left-width:5px;border-radius:12px;padding:18px}.finding h3{margin:9px 0}.finding p{margin:7px 0}.finding-critical{border-left-color:var(--red);background:#fff9f8}.finding-warning{border-left-color:#d18718;background:#fffdf6}.finding-note{border-left-color:#5283b7;background:#f9fcff}",
    ".severity{text-transform:uppercase;font-size:.72rem;font-weight:900;letter-spacing:.08em;margin-right:9px}code{background:#edf1ec;border-radius:5px;padding:2px 6px;color:#415149}.table-wrap{overflow-x:auto;border:1px solid var(--line);border-radius:11px}table{border-collapse:collapse;width:100%;font-size:.9rem;background:#fff}th,td{text-align:left;padding:11px 12px;border-bottom:1px solid #e7ebe6;vertical-align:middle}thead th{background:#edf2ed;font-size:.78rem;text-transform:uppercase;letter-spacing:.04em}tbody tr:last-child td,tbody tr:last-child th{border-bottom:0}",
    ".model-panel{margin:28px 0}.bar-cell{min-width:150px}.bar{display:block;height:9px;border-radius:999px;min-width:2px}.bar-positive{background:#2f8c65}.bar-negative{background:#c6534f}.mini-grade{display:inline-grid;place-items:center;width:30px;height:30px;border-radius:8px;font-weight:900}",
    ".columns{display:grid;grid-template-columns:1fr 1fr;gap:18px}.columns>div{background:#f5f7f3;padding:18px 22px;border-radius:12px}.columns h3{margin-top:0}.narrative{white-space:pre-wrap;overflow-wrap:anywhere;background:#f5f7f3;border:1px solid var(--line);border-radius:12px;padding:18px;font:inherit}.learn-more,.advanced{margin-top:24px;border:1px solid var(--line);border-radius:12px;padding:14px 18px;background:#fbfcfa}.learn-more summary,.advanced summary{cursor:pointer;font-weight:850;color:var(--green);padding:5px}.advanced>h3:first-of-type{margin-top:24px}.effect-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:14px}.effect-card{border:1px solid var(--line);border-radius:14px;padding:18px;background:#fbfcfa}.effect-card h3{font-size:1.35rem;margin:5px 0}.effect-plot{width:100%;height:auto;background:#f2f6f2;border-radius:9px}.axis{stroke:#9aa9a2;stroke-width:1}.effect-line{fill:none;stroke:#176b4d;stroke-width:5;stroke-linecap:round;stroke-linejoin:round}dl{display:grid;grid-template-columns:minmax(150px,220px) 1fr;gap:8px 18px}dt{font-weight:800}dd{margin:0;color:var(--muted);overflow-wrap:anywhere}",
    "footer{background:#102a23;color:#cbe0d7;padding:28px 0}@media(max-width:820px){.cards{grid-template-columns:1fr 1fr}.columns,.effect-grid{grid-template-columns:1fr}}@media(max-width:520px){.shell{width:min(100% - 24px,1120px)}header{padding-top:42px}.cards{grid-template-columns:1fr}.section-head{align-items:flex-start}.grade{width:62px;height:62px;font-size:1.7rem}section{border-radius:12px}dl{grid-template-columns:1fr;gap:3px}dd{margin-bottom:10px}}",
    "@media(prefers-reduced-motion:reduce){html{scroll-behavior:auto}}"
  )
}
