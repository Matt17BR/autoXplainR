#' Render an interactive model comparison
#'
#' Creates a standalone HTML report from an [autoxplain()] or [evaluate_models()]
#' result. It opens with model scores, effective settings and available measured
#' costs. Focused tabs show recorded model selection, supplied data, feature
#' importance, class-specific fitted effects, prediction errors and checks.
#' Model details expose the retained fit and recorded preprocessing; supporting
#' explanations use optional help and expandable details.
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
#' @param uncertainty `"auto"` (default) includes the primary-versus-reference
#'   paired bootstrap from [performance_uncertainty()] when supported and records
#'   why it is unavailable otherwise. `TRUE` requires it; `FALSE` omits it.
#'   Intervals condition on the fitted models. Temporal evaluation is unsupported.
#'   Guided workflows use the intercept-only baseline as their reference;
#'   supplied-model results need an explicit reference.
#' @param open Open the report in a browser after writing it.
#' @param top_features Maximum displayed features per model when `audit` is not
#'   supplied. The audit uses their union; multiclass curves cover each outcome class.
#' @param n_repeats Permutation repeats when `audit` is not supplied.
#' @param max_models Maximum models audited when `audit` is not supplied.
#' @param report_data Data included in HTML: `"summary"` for aggregate views,
#'   `"rows"` to include individual observations and predictions, or `"none"`
#'   to omit data exploration and case-level displays. [report_data_control()]
#'   selects exported columns and the maximum number of rows. Hidden rows and
#'   panels remain accessible to anyone receiving the file.
#' @param benchmark Optional result of [benchmark_predictions()] made from the
#'   same unchanged models and evaluation data. Adds repeated prediction costs
#'   and their measurement protocol; rendering does not run a benchmark.
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
                                uncertainty = "auto",
                                target_units = NULL,
                                report_data = "summary",
                                benchmark = NULL) {
  if (!inherits(result, "autoxplain_result")) {
    stop("`result` must be returned by `autoxplain()`.", call. = FALSE)
  }
  explicit_effects <- !is.null(effects)
  report_data <- normalize_report_data_control(report_data)
  if (!is.null(target_units)) {
    if (!is.character(target_units) || length(target_units) != 1L || is.na(target_units) || !nzchar(target_units)) {
      stop("`target_units` must be one non-empty unit label or NULL.", call. = FALSE)
    }
    result$provenance$target_units <- target_units
  }
  use_retained <- missing(top_features) && missing(n_repeats) && missing(max_models)
  top_features <- assert_count(top_features, "top_features")
  n_repeats <- assert_count(n_repeats, "n_repeats")
  max_models <- assert_count(max_models, "max_models")
  validate_html_destination(output_file, open)
  title <- title %||% result$provenance$analysis_label %||% paste("Predict", result$target_column)
  if (!is.character(title) || length(title) != 1L || is.na(title) || !nzchar(title)) {
    stop("`title` must be a single non-empty string or NULL.", call. = FALSE)
  }
  result$.report_context <- prepare_report_context(result)
  result <- prepare_report_benchmark(result, benchmark)
  result <- prepare_report_uncertainty(result, uncertainty)
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
  validate_attached_audit(audit, report_explainers(result, models = ids),
    expected_fingerprints = report_fingerprints(result, models = ids)
  )
  effects <- effects %||% list()
  if (!is.list(effects)) stop("`effects` must be a list or NULL.", call. = FALSE)
  if (length(effects) && (is.null(names(effects)) || anyNA(names(effects)) || any(!nzchar(names(effects))) || anyDuplicated(names(effects)))) {
    stop("`effects` must have unique non-empty feature names.", call. = FALSE)
  }
  if (length(effects)) {
    primary_id <- result$evaluation$primary_model_id %||% result$provenance$primary_model_id
    expected <- report_fingerprints(result, models = primary_id)[[1L]]
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
  result <- prepare_report_effects(result, audit, effects, explicit_effects)
  result$.report_export <- prepare_data_explorer(result, report_data)
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
  html <- model_report_html(result, audit, effects, narrative, subgroup_check, title)
  validate_evaluation_snapshot(result)
  writeLines(html, output_file, useBytes = TRUE)
  output_path <- normalizePath(output_file, mustWork = TRUE)
  attr(output_path, "diagnostic_status") <- lapply(report_view_model(result)$diagnostics, function(record) {
    record[c("id", "status", "scope", "entities", "reason")]
  })
  attr(output_path, "data_manifest") <- result$.report_export$manifest
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
  if (is.null(result$.report_context)) result$.report_context <- prepare_report_context(result)
  if (is.null(result$.report_export)) result$.report_export <- prepare_data_explorer(result, "summary")
  if (is.null(result$explanations$report_diagnostics)) result <- prepare_report_diagnostics(result)
  model_explorer_html(result, audit, effects, narrative, subgroup_check, title)
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

pretty_complexity <- function(metric) {
  labels <- c(
    model_size_kb = "approximate model-object size (KiB)",
    size_mb = "approximate model-object size (MB)",
    model_size = "approximate model-object size",
    training_time_ms = "training time (ms)",
    training_time_s = "training time (s)",
    prediction_time_ms = "prediction time (ms)",
    repeated_prediction_ms_per_row = "Repeated prediction (ms / row)",
    complexity = "model complexity"
  )
  if (metric %in% names(labels)) unname(labels[[metric]]) else gsub("_", " ", metric)
}

render_guided_notes <- function(notes, result = NULL) {
  if (is.null(notes) || !nrow(notes)) {
    return("")
  }
  selection <- if (!is.null(result) && any(notes$code == "tuning_family_resampling_failed")) {
    tuning_evidence(result)
  } else {
    NULL
  }
  items <- vapply(seq_len(nrow(notes)), function(index) {
    message <- html_escape(notes$message[[index]])
    recommendation <- html_escape(notes$recommendation[[index]])
    command <- ""
    if (identical(notes$code[[index]], "tuning_family_resampling_failed") &&
          identical(selection$status, "computed") && length(selection$family_failures$resampling)) {
      families <- selection$family_failures$resampling
      candidates <- selection$candidates
      failed <- candidates[candidates$family %in% families & candidates$status != "ok", , drop = FALSE]
      families <- intersect(families, failed$family)
      if (length(families)) {
        labels <- vapply(families, selection_family_label, character(1), task = result$task)
        message <- paste0(
          "Every configuration failed in at least one training-only fold for: ",
          html_escape(paste(labels, collapse = ", ")), "."
        )
        links <- vapply(seq_along(families), function(i) {
          rows <- failed[failed$family == families[[i]], , drop = FALSE]
          paste0(
            '<a href="#selection-detail-', report_anchor(rows$configuration_id[[1L]]),
            '" data-navigate>Inspect ', html_escape(labels[[i]]), " fold failures</a>",
            if (nrow(rows) > 1L) paste0(" (first of ", nrow(rows), " configurations)")
          )
        }, character(1))
        recommendation <- paste0(
          paste(links, collapse = "; "),
          ". These families were excluded from selection because their cross-validation evidence is incomplete."
        )
        command <- paste0(
          '<details class="guided-note-command"><summary>Inspect in R</summary>',
          "<pre><code>tuning_results(result)$fold_scores$error</code></pre></details>"
        )
      }
    }
    paste0(
      "<article class=\"guided-note guided-note-", html_escape(notes$severity[[index]]), "\">",
      "<h3>", message, "</h3><p><strong>Next step:</strong> ",
      recommendation, "</p>", command, "</article>"
    )
  }, character(1))
  paste0(
    "<div class=\"guided-notes\"><h3>Important context for these scores</h3>",
    paste(items, collapse = ""), "</div>"
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
    paste0(if (value > 0) "+" else "", sub("%", "", format_percent(value), fixed = TRUE))
  }, character(1))
  names(display) <- c(
    "Input", "Used by model?", "Training missing", "Evaluation missing",
    "Change (pp)", "Flagged?"
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
      paste0("At least ", sub("%", "", format_percent(shift$threshold), fixed = TRUE), " percentage points")
    ),
    metric_card(
      "Largest observed shift", sub("%", " pp", format_percent(shift$largest_shift), fixed = TRUE),
      "Absolute difference in percentage points"
    ),
    "</div>", verdict, html_table(
      display, digits = 3L,
      caption = "Raw missing-value rates before preprocessing; change is in percentage points (pp)"
    ),
    "<p class=\"microcopy\">", html_escape(shift$scope_note), "</p>"
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
    render_candidate_importance_ranges(audit, result),
    "<h3>Feature association diagnostics</h3>", html_table(audit$dependence, 3L, caption = "Feature association screen"),
    '<p class="microcopy">', html_escape(audit$summary$association_scope %||% "A limited association screen does not establish independence."),
    "</p></details></section>"
  )
}

render_candidate_importance_ranges <- function(audit, result = NULL) {
  ranges <- audit$explanation_agreement$importance_ranges
  if (is.null(ranges) || !nrow(ranges)) return("")
  candidates <- audit$performance$model[audit$performance$near_optimal %in% TRUE]
  if (length(candidates) < 2L) return("")
  labels <- if (is.null(result)) {
    candidates
  } else {
    vapply(candidates, function(id) explorer_label(result, id), character(1))
  }
  table <- ranges[c("feature", "min_importance", "max_importance", "mean_importance")]
  names(table) <- c("Input", "Lowest", "Highest", "Mean")
  metric_id <- audit$config$metric %||% result$evaluation$primary_metric %||% "loss"
  metric <- pretty_metric(metric_id)
  direction <- if (metric_id %in% c("accuracy", "auc")) "Decrease in" else "Increase in"
  paste0(
    '<div id="candidate-importance-ranges"><h3>How feature importance varies across candidates</h3>',
    "<p>", html_escape(paste(labels, collapse = "; ")), ".</p>",
    html_table(table, digits = 4L, caption = paste(direction, metric, "when each input is shuffled")),
    '<p class="microcopy">Ranges cover the supplied near-optimal candidates. They are not confidence intervals or bounds over all possible models.</p></div>'
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

render_shuffle_findings <- function(findings, result = NULL) {
  rows <- vapply(seq_len(nrow(findings)), function(i) {
    model <- findings$model[[i]]
    feature <- findings$feature[[i]]
    label <- if (is.null(result)) model else report_model_label(result, model)
    paste0(
      "<tr><td>", html_escape(label), "</td><td><a href=\"#", report_evidence_id(model, feature),
      '" data-evidence-model="', html_escape(model), '" data-evidence-feature="', html_escape(feature),
      '">', html_escape(feature), "</a></td><td>", html_escape(findings$evidence[[i]]), "</td></tr>"
    )
  }, character(1))
  paste0(
    '<article class="finding finding-note finding-group"><h3>Unresolved shuffle intervals</h3>',
    "<p>For these ", nrow(findings), " model and feature pairs, the interval does not resolve the sign of the mean loss change.</p>",
    '<div class="table-wrap" tabindex="0" role="region" aria-label="Unresolved shuffle intervals">',
    "<table><caption>Permutation uncertainty conditional on the fitted models and evaluation rows</caption>",
    '<thead><tr><th scope="col">Model</th><th scope="col">Feature</th><th scope="col">Evidence</th></tr></thead>',
    "<tbody>", paste(rows, collapse = ""), "</tbody></table></div><p><strong>Next:</strong> ",
    html_escape(paste(unique(findings$recommendation), collapse = " ")), "</p></article>"
  )
}

render_findings <- function(findings, audit = NULL, result = NULL) {
  if (is.null(findings) || !nrow(findings)) {
    return("<p>No diagnostic findings were recorded. Review the coverage below; an absent finding is not a guarantee.</p>")
  }
  grouped <- if (all(c("model", "feature") %in% names(findings))) {
    which(findings$code == "shuffle_interval_unresolved" &
            !is.na(findings$model) & !is.na(findings$feature))
  } else {
    integer()
  }
  has_candidate_ranges <- !is.null(result) && nzchar(render_candidate_importance_ranges(audit, result))
  cards <- vapply(seq_len(nrow(findings)), function(i) {
    if (length(grouped) > 1L && i %in% grouped) {
      return(if (i == grouped[1L]) render_shuffle_findings(findings[grouped, , drop = FALSE], result) else "")
    }
    links <- ""
    action <- html_escape(findings$recommendation[[i]])
    if (identical(findings$code[[i]], "rashomon_disagreement") && has_candidate_ranges) {
      action <- '<a href="#candidate-importance-ranges" data-navigate>Compare feature-importance ranges across the supplied candidates.</a>'
    }
    model <- if ("model" %in% names(findings)) findings$model[[i]] else NA_character_
    feature <- if ("feature" %in% names(findings)) findings$feature[[i]] else NA_character_
    if (!is.na(model) && !is.na(feature) && nzchar(model) && nzchar(feature)) {
      label <- if (is.null(result)) model else report_model_label(result, model)
      links <- paste0('<a href="#', report_evidence_id(model, feature), '" data-evidence-model="', html_escape(model), '" data-evidence-feature="', html_escape(feature), '">', html_escape(label), " / ", html_escape(feature), "</a>")
    } else if (!is.na(feature) && nzchar(feature) && !is.null(audit$importance)) {
      items <- audit$importance[audit$importance$feature == feature, , drop = FALSE]
      if (nrow(items)) {
        links <- paste(vapply(seq_len(nrow(items)), function(j) {
          paste0(
            '<a href="#',
            report_evidence_id(items$model[j], items$feature[j]), '" data-evidence-model="', html_escape(items$model[j]), '" data-evidence-feature="', html_escape(items$feature[j]), '">', html_escape(if (is.null(result)) items$model[j] else report_model_label(result, items$model[j])),
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
            report_evidence_id(items$model[j], items$feature[j]), '" data-evidence-model="', html_escape(items$model[j]), '" data-evidence-feature="', html_escape(items$feature[j]), '">', html_escape(if (is.null(result)) items$model[j] else report_model_label(result, items$model[j])),
            " / ", html_escape(items$feature[j]), "</a>"
          )
        }, character(1)), collapse = ", ")
      }
    }
    paste0(
      '<article class="finding finding-', html_escape(findings$severity[[i]]), '">',
      "<h3>", html_escape(findings$message[[i]]), "</h3>", if (nzchar(links)) paste0('<p class="affected">Affected evidence: ', links, "</p>") else "",
      "<p>", html_escape(findings$evidence[[i]]), "</p><p><strong>Next:</strong> ", action,
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

report_axis_number <- function(x) {
  if (!is.finite(x)) return("n/a")
  scientific <- x != 0 && (abs(x) < .001 || abs(x) >= 1e6)
  format(signif(x, 4L), trim = TRUE, scientific = scientific)
}

report_number <- function(x, digits = 3L) {
  if (length(x) != 1L || !is.finite(x)) {
    return("n/a")
  }
  if (x != 0 && round(x, digits) == 0) {
    return(format(signif(x, max(1L, digits)), trim = TRUE, scientific = TRUE))
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

render_performance_uncertainty <- function(uncertainty, record = NULL, result = NULL) {
  if (is.null(uncertainty)) {
    return(paste0(
      '<section id="uncertainty"><h2>Evaluation-sample uncertainty</h2>',
      render_diagnostic_state(
        "Paired bootstrap", record$status %||% "not_run",
        record$reason %||% "No evaluation-sample interval was requested."
      ),
      if (is.null(record) || identical(record$status, "not_run")) {
        '<details class="advanced"><summary>Compute intervals in R</summary><pre><code>performance_uncertainty(result)</code></pre><p>The bootstrap requires supported independent sampling units.</p></details>'
      }, "</section>"
    ))
  }
  metric <- uncertainty$metric %||% result$evaluation$primary_metric %||% "score"
  units <- if (metric %in% c("rmse", "mae")) result$provenance$target_units else NULL
  metric_label <- paste0(pretty_metric(metric), if (length(units) && nzchar(units)) paste0(" (", units, ")"))
  primary <- explorer_label(result, uncertainty$primary_model_id %||% "primary")
  reference <- explorer_label(result, uncertainty$reference_model_id %||% "reference")
  estimates <- uncertainty$estimates
  labels <- c(primary = paste0(primary, " (primary)"), baseline = paste0(reference, " (reference)"),
              difference = "Primary minus reference")
  table <- data.frame(
    comparison = unname(labels[estimates$quantity]), estimate = estimates$estimate,
    interval = vapply(seq_len(nrow(estimates)), function(i) {
      paste(report_number(estimates$lower[[i]]), "to", report_number(estimates$upper[[i]]))
    }, character(1)), check.names = FALSE
  )
  names(table) <- c("Model or comparison", "Estimate", paste(format_percent(uncertainty$confidence), "interval"))
  notes <- uncertainty$notes
  important <- grepl("Fewer than|degenerate|reused", notes)
  assumptions <- notes[!important & !startsWith(notes, "Paired percentile intervals conditional on the fitted models;")]
  paste0(
    "<section id=\"uncertainty\" aria-labelledby=\"uncertainty-title\"><h2 id=\"uncertainty-title\">How variable is this score?</h2>",
    html_table(table, caption = paste0(
      metric_label, " \u00b7 ", uncertainty$units, " evaluation ",
      uncertainty$unit, if (uncertainty$units != 1L) "s"
    )),
    '<p class="microcopy">Negative differences favor ', html_escape(primary), ".</p>",
    if (any(important)) paste0('<p class="baseline-caution">', html_escape(paste(notes[important], collapse = " ")), "</p>"),
    '<details class="uncertainty-method"><summary>Interval method and assumptions</summary>',
    "<p>Paired percentile intervals conditional on the fitted models, from ", uncertainty$n_boot,
    " bootstrap draws. ", html_escape(paste(assumptions, collapse = " ")), "</p></details></section>"
  )
}
