# The report is an offline model explorer. Calculations stay in R; browser
# controls select already-computed evidence and never fit or rank a new model.
report_asset <- function(name) {
  path <- system.file("report", name, package = "AutoXplainR")
  if (!nzchar(path)) stop("Missing installed report asset: ", name, call. = FALSE)
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

explorer_label <- function(result, id) {
  label <- report_model_label(result, id)
  label <- sub("^(resampling-selected |tuned )", "", label)
  label <- sub(" (reference|alternative)$", "", label)
  paste0(toupper(substr(label, 1, 1)), substring(label, 2))
}

explorer_help <- function(label, text, id = paste0("help-", report_anchor(label))) {
  id <- html_escape(id)
  paste0(
    '<span class="help"><button type="button" class="help-button" aria-expanded="false" aria-controls="',
    id, '" aria-describedby="', id, '"><span aria-hidden="true">?</span><span class="sr-only">',
    html_escape(label), '</span></button><span role="tooltip" class="help-tip" id="', id, '">',
    html_escape(text), "</span></span>"
  )
}

explorer_options <- function(values, labels = values, selected = values[1]) {
  paste0('<option value="', html_escape(values), '"',
    ifelse(values == selected, " selected", ""), ">", html_escape(labels), "</option>",
    collapse = ""
  )
}

explorer_models <- function(result) {
  board <- enrich_tradeoff_leaderboard(result)
  board$model <- vapply(board$model_id, function(id) explorer_label(result, id), character(1))
  metrics <- intersect(names(result$evaluation$metric_definitions), names(board))
  if (!length(metrics)) metrics <- intersect(c("rmse", "mae", "logloss", "auc"), names(board))
  resources <- intersect(
    c("repeated_prediction_ms_per_row", "model_size_kb", "training_time_ms", "prediction_time_ms"), names(board)
  )
  for (column in c(metrics, resources)) {
    if (!is.numeric(board[[column]])) stop("Report measurements must be numeric.", call. = FALSE)
  }
  resources <- resources[vapply(resources, function(column) any(is.finite(board[[column]])), logical(1))]
  # Start with a cost that can actually compare the displayed models. A partial
  # benchmark remains inspectable without making an empty chart the default.
  comparable <- vapply(resources, function(column) sum(is.finite(board[[column]])) >= min(2L, nrow(board)), logical(1))
  resources <- resources[order(!comparable)]
  list(table = board, metrics = metrics, resources = resources)
}

explorer_measurement <- function(x, resource = FALSE) {
  if (!is.finite(x)) {
    return("Unavailable")
  }
  if (resource && x == 0) {
    return("~0")
  }
  format(signif(x, 4), trim = TRUE, scientific = FALSE)
}

explorer_model_table <- function(result, models) {
  board <- models$table
  primary <- result$provenance$primary_model_id
  selected_label <- if (inherits(result$tuning, "autoxplain_tuning")) {
    "CV choice"
  } else if (identical(result$engine, "h2o")) {
    "H2O choice"
  } else {
    "Default prediction"
  }
  resources <- c(
    training_time_ms = "Fit (ms)", prediction_time_ms = "Predict batch (ms)", model_size_kb = "Size (KiB)",
    repeated_prediction_ms_per_row = "Repeated predict (ms / row)"
  )
  headers <- paste0('<th scope="col" data-score-column="', models$metrics, '">',
    '<button type="button" data-sort="', models$metrics, '">',
    html_escape(vapply(models$metrics, pretty_metric, character(1))), ' <span aria-hidden="true">\u2195</span>',
    "</button></th>",
    collapse = ""
  )
  headers <- paste0(headers, paste0('<th scope="col"><button type="button" data-sort="',
    models$resources, '">', resources[models$resources], ' <span aria-hidden="true">\u2195</span></button></th>',
    collapse = ""
  ))
  rows <- vapply(seq_len(nrow(board)), function(i) {
    values <- paste0(" data-value-", c(models$metrics, models$resources), '="',
      vapply(
        board[i, c(models$metrics, models$resources), drop = FALSE],
        function(x) if (is.finite(x)) sprintf("%.17g", x) else "", character(1)
      ), '"',
      collapse = ""
    )
    scores <- paste0('<td class="number" data-score-column="', models$metrics, '">',
      vapply(board[i, models$metrics, drop = FALSE], explorer_measurement, character(1)), "</td>",
      collapse = ""
    )
    costs <- paste0('<td class="number">',
      vapply(
        models$resources, function(key) explorer_measurement(board[[key]][i], key != "model_size_kb"),
        character(1)
      ), "</td>",
      collapse = ""
    )
    role <- if (board$model_id[i] == primary) selected_label else if (board$role[i] == "baseline") "Baseline" else ""
    spec <- model_specification(result, board$model_id[i])
    paste0(
      '<tr data-model-row="', html_escape(board$model_id[i]), '"', values, ">",
      '<th scope="row"><a class="model-link" data-pick-model="', html_escape(board$model_id[i]),
      '" href="#patterns"><span class="model-dot" style="background:',
      report_model_color(board$model_id[i], result), '"></span>',
      html_escape(board$model[i]), "</a>", if (nzchar(role)) paste0('<small class="role">', role, "</small>"),
      '<span class="model-settings">', html_escape(spec$summary), "</span>", explorer_spec_link(spec),
      "</th>", scores, costs, "</tr>"
    )
  }, character(1))
  paste0(
    '<div class="table-wrap model-table-wrap" tabindex="0" role="region" aria-label="Model comparison">',
    '<table class="model-table"><caption>Scores and measured costs for every retained model</caption>',
    '<thead><tr><th scope="col">Model \u00b7 click to explore</th>', headers, "</tr></thead><tbody>",
    paste(rows, collapse = ""), "</tbody></table></div>"
  )
}

explorer_tradeoffs <- function(result, models) {
  if (nrow(models$table) < 2L || !length(models$resources)) return("")
  resources <- c(
    training_time_ms = "Training time (ms)", prediction_time_ms = "Prediction time (ms)",
    model_size_kb = "Model size (KiB)", repeated_prediction_ms_per_row = "Repeated prediction (ms / row)"
  )
  plots <- lapply(models$metrics, function(metric) {
    paste(vapply(models$resources, function(resource) {
      trade <- tryCatch(model_tradeoffs(result, metric, resource), error = function(e) e)
      content <- if (inherits(trade, "error")) {
        render_diagnostic_state("Comparison", "unavailable", conditionMessage(trade))
      } else {
        trade$model <- vapply(trade$model_id, function(id) explorer_label(result, id), character(1))
        tradeoff_chart(trade, result)
      }
      paste0('<div data-cost-plot="', metric, '" data-resource="', resource, '">', content, "</div>")
    }, character(1)), collapse = "")
  })
  paste0(
    '<div class="comparison-chart"><div class="section-heading"><h3>Performance vs cost ',
    explorer_help("Reading the cost chart", paste(
      "Left means lower measured cost. For a loss such as RMSE or log loss, lower is better;",
      "for accuracy or R-squared, higher is better. Outlined models are not beaten on both axes.",
      "Single-fit millisecond timings are descriptive readings, not reliable speed rankings.",
      "R object size includes retained diagnostics, may count shared data repeatedly",
      "and excludes native engine allocations.",
      "It is not a saved-file or deployment-memory comparison."
    )), "</h3>",
    '<label class="control">Compare cost <select id="resource-select">',
    explorer_options(models$resources, resources[models$resources]), "</select></label>",
    '<div class="cost-scale-control"><label class="control">Cost scale ',
    '<select id="cost-scale-select" disabled aria-describedby="cost-scale-note">',
    '<option value="linear">Linear</option><option value="log">Log</option></select></label>',
    explorer_help("Choosing a cost scale", paste(
      "Linear spacing compares absolute cost differences. Log spacing compares ratios:",
      "1 to 10 uses the same space as 10 to 100. It can separate inexpensive models when one model",
      "is much larger or slower. All plotted costs must be positive; scores and the Pareto frontier do not change."
    )), "</div>",
    '<div class="cost-scale-control"><label class="control">Score scale ',
    '<select id="score-scale-select" disabled aria-describedby="score-scale-note">',
    '<option value="linear">Linear</option><option value="log">Log</option></select></label>',
    explorer_help("Choosing a score scale", paste(
      "Log spacing compares ratios between positive losses. It can separate useful models when",
      "one alternative has a much larger error. All models and their original scores remain visible.",
      "Zero losses and scores such as accuracy or R-squared use a linear axis."
    )), "</div></div>",
    '<p id="score-scale-note" class="microcopy" role="status" hidden></p>',
    '<p id="cost-scale-note" class="microcopy" role="status">',
    "Linear cost axis. Scale switching requires JavaScript.</p>",
    paste(plots, collapse = ""),
    "<details><summary>How costs were measured</summary><p>Times are measured on this machine. ",
    "Fit time covers the retained fit, not the entire cross-validation search; prediction time covers this ",
    "evaluation batch. A displayed ~0 means no elapsed time was recorded; timer resolution varies ",
    "by platform. It does not mean the operation takes no time. ",
    if (identical(result$engine, "h2o")) {
      "H2O model size is reported by the engine when available; the native baseline uses R object size. "
    } else {
      "Size estimates the retained R model object. "
    },
    "R object size includes retained diagnostics, may count shared data repeatedly ",
    "and excludes native engine allocations. ",
    "It is not a saved-file or deployment-memory comparison, or a count of learned rules. ",
    "These measurements are not hardware-independent benchmarks.</p></details></div>"
  )
}

explorer_overview <- function(result, audit, models) {
  view <- report_view_model(result, audit)
  board <- models$table
  metric <- result$evaluation$primary_metric %||% models$metrics[1]
  higher <- metric %in% higher_is_better_metrics()
  primary <- match(view$identity$model_id, board$model_id)
  role_note <- switch(view$identity$evaluation_role,
    validation = "Validation scores: these rows may have influenced model selection.",
    evaluation = "Supplied evaluation rows: independence from model selection is not asserted.",
    ""
  )
  choice <- if (inherits(result$tuning, "autoxplain_tuning")) {
    paste0(
      board$model[primary], " was selected using ", result$tuning$folds_used,
      "-fold cross-validation on the training rows. ", nrow(result$tuning$candidates),
      " settings were tried; one fitted representative per successful family is kept here."
    )
  } else if (identical(result$engine, "h2o")) {
    result$provenance$candidate_selection %||% view$identity$selection_note
  } else if (identical(result$provenance$workflow, "supplied-model evaluation")) {
    paste0(
      board$model[primary], " is the user-chosen primary model. ",
      "AutoXplainR did not fit or select these models; their training and selection history is not inferred."
    )
  } else {
    paste0(
      board$model[primary], " is the pre-specified default for predict(). ",
      "Use the model links to inspect alternatives."
    )
  }
  paste0(
    '<section id="overview" class="workspace-page" data-page="models" aria-labelledby="overview-title">',
    '<div id="models"><div class="section-heading"><div><p class="section-number">Models</p>',
    '<h2 id="overview-title">', if (nrow(board) > 1L) "Compare the models" else "Evaluate the model",
    '</h2></div><label class="control">Score ',
    '<select id="metric-select">', explorer_options(
      models$metrics,
      vapply(models$metrics, pretty_metric, character(1)), metric
    ), "</select></label></div>",
    '<p class="task-intro" id="score-summary" data-rows="', view$identity$evaluation_rows,
    '" data-role="', html_escape(view$identity$evaluation_role), '">', html_escape(paste0(
      report_count(view$identity$evaluation_rows), " ", view$identity$evaluation_role, " rows \u00b7 ",
      pretty_metric(metric), ": ", if (higher) "higher" else "lower", " is better. ",
      "Table order is descriptive; the primary model is unchanged."
    )), "</p>",
    if (nzchar(role_note)) paste0('<p class="evaluation-note">', role_note, "</p>"),
    '<div class="comparison-context"><span>', html_escape(if (inherits(result$tuning, "autoxplain_tuning")) {
      paste0("CV choice: ", board$model[primary])
    } else {
      paste0("Default: ", board$model[primary])
    }), "</span>",
    explorer_help("How the default model was chosen", choice),
    '<a href="#selection" data-navigate>',
    if (inherits(result$tuning, "autoxplain_tuning") || identical(result$engine, "h2o")) {
      "Inspect the search"
    } else {
      "Model provenance"
    }, "</a>",
    "<span>Score definitions</span>", explorer_help("Score definitions", paste(
      paste(vapply(models$metrics, pretty_metric, character(1)),
        unname(result$evaluation$metric_definitions[models$metrics]),
        sep = ": "
      ),
      collapse = " "
    )), "</div>",
    explorer_baseline_comparison(result, models),
    '<div class="compare-layout', if (nrow(board) == 1L || !length(models$resources)) " single-model" else "",
    '"><div>', explorer_model_table(result, models), "</div>",
    explorer_tradeoffs(result, models), "</div>",
    render_report_benchmark(result),
    "</div></section>"
  )
}

explorer_importance <- function(rows, metric, model_id) {
  if (is.null(rows) || !nrow(rows)) {
    return(render_diagnostic_state("Feature importance", "not_run", "This model was outside the explanation budget."))
  }
  if (!any(is.finite(rows$importance))) {
    reason <- unique(rows$unavailable_reason[nzchar(rows$unavailable_reason)])
    return(render_diagnostic_state(
      "Feature importance", "unavailable",
      if (length(reason)) paste(reason, collapse = " ") else "No finite permutation importance was available."
    ))
  }
  rows <- rows[order(-rows$importance), , drop = FALSE]
  limits <- unlist(rows[intersect(c("importance", "conf_low", "conf_high"), names(rows))], use.names = FALSE)
  limits <- limits[is.finite(limits)]
  lower <- min(0, limits)
  upper <- max(0, limits)
  scale <- upper - lower
  if (!is.finite(scale) || scale == 0) scale <- 1
  zero <- -lower / scale * 95
  bars <- vapply(seq_len(nrow(rows)), function(i) {
    value <- rows$importance[i]
    pct <- abs(value) / scale * 95
    interval <- if (all(c("conf_low", "conf_high") %in% names(rows))) {
      c(rows$conf_low[i], rows$conf_high[i])
    } else {
      c(NA_real_, NA_real_)
    }
    has_interval <- all(is.finite(interval))
    detail <- paste0(
      rows$feature[i], ": ", explorer_measurement(value),
      if (has_interval) paste0(
        "; shuffle interval ", explorer_measurement(interval[1]), " to ", explorer_measurement(interval[2]),
        ". This is shuffle variation, not population uncertainty."
      )
    )
    paste0(
      '<button class="importance-row" type="button" data-pick-feature="', html_escape(rows$feature[i]),
      '" data-for-model="', html_escape(model_id), '" title="', html_escape(detail),
      '"><span class="feature-name">', html_escape(rows$feature[i]),
      '</span><span class="importance-track"><span class="importance-zero" style="left:', zero, '%"></span>',
      '<span class="importance-bar ', if (value < 0) "negative" else "positive", '" style="left:',
      if (value < 0) zero - pct else zero, "%;width:", pct, '%"></span>',
      if (has_interval) paste0(
        '<span class="importance-interval" style="left:',
        (interval[1] - lower) / scale * 95, "%;width:", diff(interval) / scale * 95, '%"></span>'
      ), "</span>",
      "<strong>", if (value > 0) "+" else "", explorer_measurement(value), "</strong></button>"
    )
  }, character(1))
  paste0(
    '<div class="importance-bars" role="group" aria-label="Feature importance for ', html_escape(model_id), '">',
    '<div class="importance-key">Change in ', html_escape(pretty_metric(metric)),
    " when shuffled ",
    explorer_help(
      paste("Reading feature importance for", model_id),
      paste(
        "Positive bars mean predictions worsened when this input was shuffled. Larger bars show greater",
        "fitted reliance. Whiskers show shuffle variation, not population uncertainty.",
        "Click a feature for its curve. Correlated inputs can share importance."
      )
    ),
    "<span>\u2190 improves \u00b7 worsens \u2192</span></div>", paste(bars, collapse = ""), "</div>",
    "<details><summary>Shuffle values and intervals</summary>",
    html_table(rows[c("feature", "importance", "conf_low", "conf_high")], 3,
      caption = "Repeated-shuffle loss changes"
    ),
    "<p>Intervals measure random-shuffle variation on these rows. Correlated inputs can share importance; ",
    "inspect joint data distributions before treating an input as uniquely useful.</p></details>"
  )
}

explorer_model_effects <- function(result, audit, effects, class = NULL) {
  ids <- names(audit$importance_objects)
  primary <- result$provenance$primary_model_id
  multiclass <- identical(result$task, "multiclass")
  first_class <- if (multiclass) result_class_levels(result)[1] else NULL
  class <- class %||% first_class
  saved <- if (identical(class, first_class)) {
    result$explanations$effects_by_model %||% list()
  } else {
    result$explanations$effects_by_class[[class]] %||% list()
  }
  if (!multiclass || !length(effects)) {
    if (identical(class, first_class)) saved[[primary]] <- effects
  } else {
    overrides <- Filter(function(effect) identical(attr(effect, "prediction_class"), class), effects)
    if (length(overrides)) saved[[primary]] <- overrides
  }
  explainers <- report_explainers(result, models = ids)
  fingerprints <- report_fingerprints(result, models = ids, explainers = explainers)
  output <- lapply(ids, function(id) {
    if (!is.null(saved[[id]])) {
      expected <- fingerprints[[id]]
      for (effect in saved[[id]]) {
        if (inherits(effect, "effect_failure")) next
        if (!identical(attr(effect, "explainer_fingerprint"), expected)) {
          stop("A model-specific effect is stale; recompute explanations before reporting.", call. = FALSE)
        }
        if (multiclass && !identical(attr(effect, "prediction_class"), class)) {
          stop("A retained effect has the wrong prediction class; recompute explanations.", call. = FALSE)
        }
      }
      return(saved[[id]])
    }
    rows <- audit$importance[audit$importance$model == id, , drop = FALSE]
    features <- head(rows$feature[order(-rows$importance)], result$explanations$config$top_features %||% 8L)
    values <- lapply(features, function(feature) {
      tryCatch(
        explain_effect(explainers[[id]], feature,
          method = if (is.numeric(explainers[[id]]$data[[feature]])) "ale" else "pdp",
          n_points = 16L, seed = result$provenance$seed,
          max_rows = result$explanations$config$explanation_rows,
          class = class
        ),
        error = function(e) structure(conditionMessage(e), class = "effect_failure")
      )
    })
    stats::setNames(values, features)
  })
  stats::setNames(output, ids)
}

explorer_class_effects <- function(result, audit, effects) {
  if (!identical(result$task, "multiclass")) {
    return(NULL)
  }
  classes <- result_class_levels(result)
  stats::setNames(lapply(classes, function(class) {
    explorer_model_effects(result, audit, effects, class = class)
  }), classes)
}

explorer_features <- function(result, audit, effects, models) {
  all_effects <- explorer_model_effects(result, audit, effects)
  class_effects <- explorer_class_effects(result, audit, effects)
  classes <- names(class_effects)
  panels <- vapply(models$table$model_id, function(id) {
    rows <- audit$importance[audit$importance$model == id, , drop = FALSE]
    rows <- head(rows[order(-rows$importance), , drop = FALSE], result$explanations$config$top_features %||% 8L)
    values <- all_effects[[id]] %||% list()
    features <- if (nrow(rows)) rows$feature[order(-rows$importance)] else character()
    if (!length(features)) {
      return(paste0(
        '<div class="model-panel" data-model-panel="', html_escape(id), '">',
        '<h3 class="static-model-heading">', html_escape(explorer_label(result, id)), "</h3>",
        explorer_model_identity(result, id),
        '<p class="empty-state">Feature explanations were not computed for this model. ',
        "Rebuild the report with a larger model budget to include it.</p><pre><code>",
        "render_model_report(result, max_models = length(result$models))", "</code></pre></div>"
      ))
    }
    curves <- vapply(features, function(feature) {
      content_for_class <- function(class = NULL) {
        effect <- if (is.null(class)) values[[feature]] else class_effects[[class]][[id]][[feature]]
        content <- if (is.null(effect) || inherits(effect, "effect_failure")) {
          paste0(
            "<h4>", html_escape(feature), "</h4>",
            '<p class="empty-state">', if (inherits(effect, "effect_failure")) {
              html_escape(as.character(effect))
            } else {
              "No curve is available for this input. See the recorded explanation or compute it explicitly."
            },
            "</p><pre><code>", html_escape(paste0(
              "explain_effect(as_explainers(result, models = ", deparse(id),
              ")[[1]], ", deparse(feature), if (!is.null(class)) paste0(", class = ", deparse(class)), ")"
            )), "</code></pre>"
          )
        } else {
          effect_chart(effect, feature, result, model_id = id, comparison = {
            pool <- if (is.null(class)) all_effects else class_effects[[class]]
            comparison <- lapply(pool[setdiff(names(pool), id)], `[[`, feature)
            comparison[!vapply(comparison, function(x) is.null(x) || inherits(x, "effect_failure"), logical(1))]
          })
        }
        if (is.null(class)) {
          content
        } else {
          paste0(
            '<div data-class-panel="', html_escape(class), '">', content, "</div>"
          )
        }
      }
      content <- if (length(classes)) {
        paste(vapply(classes, content_for_class, character(1)), collapse = "")
      } else {
        content_for_class()
      }
      paste0(
        '<article class="effect-card" data-feature-panel="', html_escape(feature), '">',
        content, "</article>"
      )
    }, character(1))
    paste0(
      '<div class="model-panel" data-model-panel="', html_escape(id), '">',
      '<h3 class="static-model-heading">', html_escape(explorer_label(result, id)), "</h3>",
      explorer_model_identity(result, id),
      '<div class="feature-layout">',
      "<div>", explorer_importance(rows, audit$config$metric, id), '</div><div class="effect-workspace">',
      '<label class="control">Fitted pattern <select class="feature-select">', explorer_options(features),
      "</select></label>", paste(curves, collapse = ""), "</div></div></div>"
    )
  }, character(1))
  paste0(
    '<section id="patterns" class="workspace-page" data-page="patterns" aria-labelledby="patterns-title">',
    '<p class="section-number">Features</p><h2 id="patterns-title">Feature importance &amp; effects',
    explorer_help(
      "Exploring fitted effects",
      paste(
        "ALE follows local changes in predictions and centers the effect at zero.",
        "Negative effects mean below this reference, not negative probabilities.",
        "PDP averages predictions with an input set to each displayed value.",
        "A flat average curve can hide interactions, even for an important feature.",
        "These describe fitted associations, not the consequences of an intervention."
      )
    ),
    "</h2>",
    if (isTRUE(audit$config$sampling$sampled)) paste0(
      '<p class="data-chart-note">', html_escape(explanation_sampling_note(audit$config$sampling)), "</p>"
    ),
    "<div class=\"feature-controls\">",
    explorer_model_control(models, result$provenance$primary_model_id, "feature-model-select"),
    '<label class="control">Compare with <select id="comparison-model-select">',
    explorer_options(c("", models$table$model_id), c("No comparison", models$table$model), ""),
    "</select></label>",
    if (length(classes)) {
      paste0(
        '<label class="control">Curve for class <select id="effect-class-select">',
        explorer_options(classes), "</select></label>",
        explorer_help("Class-specific curves", paste(
          "Importance summarizes prediction loss across all classes.",
          "The fitted curve describes the probability of the selected class."
        ))
      )
    }, "</div>",
    paste(panels, collapse = ""), "</section>"
  )
}

explorer_model_control <- function(models, selected, id) {
  paste0(
    '<label class="control model-control">Inspect model <select id="', id, '" class="model-select">',
    explorer_options(models$table$model_id, models$table$model, selected), "</select></label>"
  )
}

explorer_relationship_data <- function(result, audit, max_features = 12L) {
  features <- head(unique(c(audit$config$features, result$features)), max_features)
  data <- result$training_data[features]
  pairs <- expand.grid(a = features, b = features, stringsAsFactors = FALSE)
  pairs$value <- NA_real_
  pairs$n <- integer(nrow(pairs))
  pairs$method <- character(nrow(pairs))
  for (i in seq_len(nrow(pairs))) {
    x <- data[[pairs$a[i]]]
    y <- data[[pairs$b[i]]]
    ok <- stats::complete.cases(x, y)
    pairs$n[i] <- sum(ok)
    numeric <- is.numeric(x) && is.numeric(y)
    pairs$method[i] <- if (numeric) {
      "Spearman correlation"
    } else if (is.numeric(x) != is.numeric(y)) {
      "Correlation ratio (unsigned)"
    } else {
      "Cramer's V (unsigned)"
    }
    if (sum(ok) >= 3 && length(unique(x[ok])) > 1 && length(unique(y[ok])) > 1) {
      pairs$value[i] <- if (numeric) {
        stats::cor(x[ok], y[ok], method = "spearman")
      } else {
        feature_association(x, y)
      }
    }
  }
  list(features = features, pairs = pairs, rows = nrow(data), total = length(result$features))
}

explorer_checks <- function(result, audit) {
  concise <- audit
  repeated <- concise$findings$code %in% c("association_screen_scope", "feature_dependence")
  concise$findings <- concise$findings[!repeated, , drop = FALSE]
  paste0(
    '<section id="checks" class="workspace-page" data-page="checks" aria-labelledby="checks-title">',
    '<p class="section-number">Checks</p><h2 id="checks-title">What needs a closer look?</h2>',
    '<p class="check-links">',
    if (!identical(result$.report_export$mode, "none")) '<a href="#data" data-navigate>Explore data</a> \u00b7 ',
    '<a href="#evaluation" data-navigate>Inspect prediction errors</a></p>',
    render_guided_notes(result$evaluation$notes, result), render_reliability_section(concise, result),
    render_effect_failures(result),
    render_performance_uncertainty(result$performance_uncertainty, result$.report_uncertainty, result),
    render_missingness_shift(result$evaluation$diagnostics$missingness_shift),
    "</section>"
  )
}

model_explorer_html <- function(result, audit, effects, narrative, subgroup_check, title) {
  models <- explorer_models(result)
  identity <- report_view_model(result, audit)$identity
  nav <- c(
    overview = "Compare models", selection = "Model selection", data = "Explore data",
    patterns = "Feature effects", evaluation = "Predictions", checks = "Checks",
    provenance = "Methods & export"
  )
  if (identical(result$.report_export$mode, "none")) nav <- nav[names(nav) != "data"]
  subgroup_html <- ""
  if (!is.null(subgroup_check)) {
    nav <- append(nav, c(subgroups = "Group performance"), after = 5L)
    subgroup_html <- sub('<section id="subgroups"',
      '<section class="workspace-page" data-page="subgroups" id="subgroups"',
      render_subgroup_performance(subgroup_check),
      fixed = TRUE
    )
  }
  pages <- paste0(
    explorer_overview(result, audit, models), render_model_selection(result),
    explorer_data(result, result$.report_export), explorer_features(result, audit, effects, models),
    explorer_predictions(result, models), explorer_checks(result, audit),
    subgroup_html,
    '<section id="provenance" class="workspace-page" data-page="provenance" aria-labelledby="provenance-title">',
    '<p class="section-number">Methods & export</p><h2 id="provenance-title">Reproduce and share this analysis</h2>',
    render_report_data_manifest(result$.report_export),
    '<pre><code>saveRDS(result, "analysis.rds")\nevidence_summary(result)\n',
    "result$leaderboard\nresult$explanations$audit</code></pre>",
    "<h3>Retained model specifications</h3>",
    paste(vapply(names(result$models), function(id) explorer_model_spec_details(result, id), character(1)),
      collapse = ""
    ),
    "<details><summary>Run settings and validation design</summary>",
    definition_list(c(
      Target = identity$target, Task = identity$task, Engine = identity$engine,
      `Training rows` = if (is.na(identity$training_rows)) "Not supplied" else identity$training_rows,
      `Evaluation rows` = identity$evaluation_rows,
      `Evaluation role` = identity$evaluation_role, `Split` = identity$split_method,
      `Selection` = identity$selection_note, Seed = result$provenance$seed,
      `Package version` = result$provenance$package_version
    )), render_validation_design(result), "</details>",
    "<details><summary>Metric definitions</summary>", render_metric_definitions(result), "</details>",
    "<details><summary>All retained feature evidence</summary>", render_importance(audit$importance), "</details>",
    '<div id="limits"><h3>Reading this analysis</h3><p>Scores describe the recorded evaluation split. ',
    "Feature effects describe fitted associations. Use independent evaluation data for a final assessment ",
    "after changing models or predictors. Neither this report nor a passing diagnostic ",
    "establishes causal effects.</p></div>",
    render_guided_narrative(narrative), "</section>"
  )
  paste0(
    '<!doctype html><html lang="en"><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width,initial-scale=1"><title>', html_escape(title), "</title>",
    "<style>", report_css(), "\n", report_asset("explorer.css"), "\n",
    report_asset("selection.css"), "\n", report_asset("data-explorer.css"), "\n",
    report_asset("predictions.css"), '</style></head><body class="explorer">',
    '<a class="skip" href="#main">Skip to report</a><aside class="sidebar"><a class="wordmark" href="#overview">',
    '<svg viewBox="0 0 28 28" aria-hidden="true"><path d="M3 23V5M3 23h22M7 18l5-9 6 5 6-10"/></svg>',
    'AutoXplainR</a><p class="sidebar-caption">Model comparison</p>',
    '<nav class="explorer-nav" aria-label="Report sections">',
    paste0('<a href="#', names(nav), '" data-page-link="', names(nav), '"><span>',
      sprintf("%02d", seq_along(nav)), "</span>", html_escape(nav), "</a>",
      collapse = ""
    ), "</nav>",
    '<div class="sidebar-foot">Computed in R.<br>Explore offline.</div></aside>',
    '<div class="report-body"><header class="workspace-header"><div><p class="eyebrow">',
    html_escape(identity$task), " \u00b7 ", html_escape(identity$evaluation_role),
    if (!identical(identity$evaluation_role, "evaluation")) " evaluation" else "", "</p>",
    "<h1>", html_escape(title), '</h1><p class="run-context">', report_count(nrow(models$table)),
    if (nrow(models$table) == 1L) " retained model \u00b7 " else " retained models \u00b7 ",
    if (length(identity$training_rows) && !is.na(identity$training_rows)) {
      paste0(report_count(identity$training_rows), " training rows \u00b7 ")
    } else {
      "Training rows not supplied \u00b7 "
    },
    report_count(identity$evaluation_rows), " evaluation rows",
    if (!is.null(identity$target_units)) paste0(" \u00b7 ", html_escape(identity$target_units)) else "",
    if (!is.null(identity$positive)) paste0(" \u00b7 Probability event: ", html_escape(identity$positive)) else "",
    '</p></div><button type="button" id="print-report" class="quiet-button" title="Print this view">',
    '<svg class="print-icon" viewBox="0 0 24 24" aria-hidden="true">',
    '<path d="M6 8V3h12v5M6 17H3V9h18v8h-3M6 14h12v7H6zM17 11h1"/></svg>',
    '<span class="print-label">Print this view</span></button></header>',
    '<main id="main">', pages, "</main><footer>AutoXplainR ", html_escape(result$provenance$package_version),
    " \u00b7 ", html_escape(identity$split_method), "</footer></div>",
    "<script>", report_asset("explorer.js"), "</script><script>", report_asset("charts.js"),
    "</script><script>", report_asset("selection.js"), "</script><script>",
    report_asset("fflate-0.8.3.js"), "</script><script>", report_asset("payload.js"), "</script><script>",
    report_asset("data-explorer.js"), "</script><script>", report_asset("predictions.js"),
    "</script></body></html>"
  )
}
