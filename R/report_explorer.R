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

explorer_help <- function(label, text) {
  id <- paste0("help-", report_anchor(label))
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
  resources <- intersect(c("training_time_ms", "prediction_time_ms", "model_size_kb"), names(board))
  for (column in c(metrics, resources)) {
    if (!is.numeric(board[[column]])) stop("Report measurements must be numeric.", call. = FALSE)
  }
  list(table = board, metrics = metrics, resources = resources)
}

explorer_measurement <- function(x, resource = FALSE) {
  if (!is.finite(x)) {
    return("Unavailable")
  }
  if (resource && x == 0) {
    return("<1")
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
  resources <- c(training_time_ms = "Fit (ms)", prediction_time_ms = "Predict (ms)", model_size_kb = "Size (KiB)")
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
    paste0(
      '<tr data-model-row="', html_escape(board$model_id[i]), '"', values, ">",
      '<th scope="row"><a class="model-link" data-pick-model="', html_escape(board$model_id[i]),
      '" href="#patterns"><span class="model-dot model-color-', board$role[i], '"></span>',
      html_escape(board$model[i]), "</a>", if (nzchar(role)) paste0('<small class="role">', role, "</small>"),
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
  resources <- c(
    training_time_ms = "Training time (ms)", prediction_time_ms = "Prediction time (ms)",
    model_size_kb = "Model size (KiB)"
  )
  plots <- lapply(models$metrics, function(metric) {
    paste(vapply(models$resources, function(resource) {
      trade <- tryCatch(model_tradeoffs(result, metric, resource), error = function(e) e)
      content <- if (inherits(trade, "error")) {
        render_diagnostic_state("Comparison", "unavailable", conditionMessage(trade))
      } else {
        trade$model <- vapply(trade$model_id, function(id) explorer_label(result, id), character(1))
        tradeoff_svg(trade)
      }
      paste0('<div data-cost-plot="', metric, '" data-resource="', resource, '">', content, "</div>")
    }, character(1)), collapse = "")
  })
  paste0(
    '<div class="comparison-chart"><div class="section-heading"><h3>Performance vs cost ',
    explorer_help("Reading the cost chart", paste(
      "The upper left combines better prediction with lower cost. Outlined models are not beaten",
      "on both displayed measures. Compare only the measured models and axes."
    )), "</h3>",
    '<label class="control">Compare cost <select id="resource-select">',
    explorer_options(models$resources, resources[models$resources]), "</select></label></div>",
    paste(plots, collapse = ""),
    "<details><summary>How costs were measured</summary><p>Times are measured on this machine. ",
    "Fit time covers the retained fit, not the entire cross-validation search; prediction time covers this ",
    "evaluation batch. A displayed &lt;1 ms means below the recorded millisecond resolution. ",
    if (identical(result$engine, "h2o")) {
      "H2O model size is reported by the engine when available; the native baseline uses R object size. "
    } else {
      "Size is an approximate R object measurement, not a count of learned rules. "
    },
    "These measurements are not hardware-independent benchmarks.</p></details></div>"
  )
}

explorer_overview <- function(result, audit, models) {
  view <- report_view_model(result, audit)
  board <- models$table
  metric <- result$evaluation$primary_metric %||% models$metrics[1]
  higher <- metric %in% higher_is_better_metrics()
  score <- board[[metric]]
  best <- order(if (higher) -score else score, na.last = TRUE)[1]
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
  } else {
    paste0(
      board$model[primary], " is the pre-specified default for predict(). ",
      "Use the model links to inspect alternatives."
    )
  }
  paste0(
    '<section id="overview" class="workspace-page" data-page="models" aria-labelledby="overview-title">',
    '<div id="models"><div class="section-heading"><div><p class="section-number">01 / Models</p>',
    '<h2 id="overview-title">Compare the models</h2></div><label class="control">Score ',
    '<select id="metric-select">', explorer_options(
      models$metrics,
      vapply(models$metrics, pretty_metric, character(1)), metric
    ), "</select></label></div>",
    '<p class="task-intro" id="score-summary" data-rows="', view$identity$evaluation_rows,
    '" data-role="', html_escape(view$identity$evaluation_role), '">', html_escape(paste0(
      board$model[best], " has the ",
      if (higher) "highest " else "lowest ", pretty_metric(metric), " on these ",
      view$identity$evaluation_rows, " ", view$identity$evaluation_role, " rows: ",
      explorer_measurement(score[best]), "."
    )), "</p>",
    if (nzchar(role_note)) paste0('<p class="evaluation-note">', role_note, "</p>"),
    '<div class="comparison-context"><span>', html_escape(if (inherits(result$tuning, "autoxplain_tuning")) {
      paste0("CV choice: ", board$model[primary])
    } else {
      paste0("Default: ", board$model[primary])
    }), "</span>",
    explorer_help("How the default model was chosen", choice),
    "<span>Score definitions</span>", explorer_help("Score definitions", paste(
      paste(vapply(models$metrics, pretty_metric, character(1)),
        unname(result$evaluation$metric_definitions[models$metrics]),
        sep = ": "
      ),
      collapse = " "
    )), "</div>",
    '<div class="compare-layout"><div>', explorer_model_table(result, models), "</div>",
    explorer_tradeoffs(result, models), "</div>",
    '<details class="selection-detail"><summary>Training selection, settings and failures</summary>',
    render_model_tuning(result), render_behavior_comparison(result), "</details></div></section>"
  )
}

explorer_importance <- function(rows, metric, model_id) {
  if (is.null(rows) || !nrow(rows)) {
    return(render_diagnostic_state("Feature importance", "not_run", "This model was outside the explanation budget."))
  }
  rows <- rows[order(-rows$importance), , drop = FALSE]
  lower <- min(0, rows$importance, na.rm = TRUE)
  upper <- max(0, rows$importance, na.rm = TRUE)
  scale <- upper - lower
  if (!is.finite(scale) || scale == 0) scale <- 1
  zero <- -lower / scale * 95
  bars <- vapply(seq_len(nrow(rows)), function(i) {
    value <- rows$importance[i]
    pct <- abs(value) / scale * 95
    paste0(
      '<button class="importance-row" type="button" data-pick-feature="', html_escape(rows$feature[i]),
      '" data-for-model="', html_escape(model_id), '"><span class="feature-name">', html_escape(rows$feature[i]),
      '</span><span class="importance-track"><span class="importance-zero" style="left:', zero, '%"></span>',
      '<span class="importance-bar ', if (value < 0) "negative" else "positive", '" style="left:',
      if (value < 0) zero - pct else zero, "%;width:", pct, '%"></span></span>',
      "<strong>", if (value > 0) "+" else "", explorer_measurement(value), "</strong></button>"
    )
  }, character(1))
  paste0(
    '<div class="importance-bars" aria-label="Feature importance for ', html_escape(model_id), '">',
    '<div class="importance-key">Change in ', html_escape(pretty_metric(metric)),
    " when shuffled ",
    explorer_help(
      paste("Reading feature importance for", model_id),
      paste(
        "Positive bars mean predictions worsened when this input was shuffled. Larger bars show greater",
        "fitted reliance. Click a feature for its curve. Correlated inputs can share importance."
      )
    ),
    "<span>\u2190 improves \u00b7 worsens \u2192</span></div>", paste(bars, collapse = ""), "</div>",
    "<details><summary>Shuffle values and intervals</summary>",
    html_table(rows[c("feature", "importance", "conf_low", "conf_high")], 3,
      caption = "Repeated-shuffle loss changes"
    ),
    "<p>Intervals measure random-shuffle variation on these rows. Correlated inputs can share importance; ",
    "see Relationships before treating an input as uniquely useful.</p></details>"
  )
}

explorer_model_effects <- function(result, audit, effects) {
  ids <- names(audit$importance_objects)
  primary <- result$provenance$primary_model_id
  saved <- result$explanations$effects_by_model %||% list()
  saved[[primary]] <- effects
  explainers <- as_explainers(result, models = ids)
  output <- lapply(ids, function(id) {
    if (!is.null(saved[[id]])) {
      expected <- current_explainer_fingerprint(explainers[[id]])
      for (effect in saved[[id]]) {
        if (inherits(effect, "effect_failure")) next
        if (!identical(attr(effect, "explainer_fingerprint"), expected)) {
          stop("A model-specific effect is stale; recompute explanations before reporting.", call. = FALSE)
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
          class = if (result$task == "multiclass") explainers[[id]]$class_levels[1] else NULL
        ),
        error = function(e) structure(conditionMessage(e), class = "effect_failure")
      )
    })
    stats::setNames(values, features)
  })
  stats::setNames(output, ids)
}

explorer_effect_summary <- function(effect, feature, result) {
  y <- effect[[if (identical(attr(effect, "method"), "ale")) "accumulated_effect" else "partial_dependence"]]
  x <- effect[[1]]
  if (!length(y) || any(!is.finite(y))) {
    return("No finite fitted curve is available.")
  }
  unit <- result$provenance$target_units %||% if (result$task == "regression") "target units" else "probability units"
  change <- utils::tail(y, 1) - y[1]
  paste0(
    "From ", feature, " = ", format(x[1], trim = TRUE), " to ", format(utils::tail(x, 1), trim = TRUE),
    ", this model's ", if (identical(attr(effect, "method"), "ale")) "centered effect" else "average prediction",
    if (change >= 0) " rises by " else " falls by ", explorer_measurement(abs(change)), " ", unit, "."
  )
}

explorer_features <- function(result, audit, effects, models) {
  all_effects <- explorer_model_effects(result, audit, effects)
  panels <- vapply(models$table$model_id, function(id) {
    rows <- audit$importance[audit$importance$model == id, , drop = FALSE]
    rows <- head(rows[order(-rows$importance), , drop = FALSE], result$explanations$config$top_features %||% 8L)
    values <- all_effects[[id]] %||% list()
    features <- if (nrow(rows)) rows$feature[order(-rows$importance)] else character()
    curves <- vapply(features, function(feature) {
      effect <- values[[feature]]
      content <- if (is.null(effect) || inherits(effect, "effect_failure")) {
        paste0(
          '<p class="empty-state">', if (inherits(effect, "effect_failure")) {
            html_escape(as.character(effect))
          } else {
            "No curve is available for this input. See the recorded explanation or compute it explicitly."
          },
          "</p><pre><code>", html_escape(paste0(
            'explain_effect(as_explainers(result, models = "',
            id, '")[[1]], "', feature, '")'
          )), "</code></pre>"
        )
      } else {
        paste0(
          '<p class="effect-description">', html_escape(explorer_effect_summary(effect, feature, result)), "</p>",
          '<p class="effect-context">', html_escape(paste(toupper(attr(effect, "method")),
            attr(effect, "prediction_target") %||% result$target_column,
            sep = " \u00b7 "
          )), "</p>",
          effect_svg(effect, feature, result), "<details><summary>Curve values, support and method</summary>",
          html_table(as.data.frame(effect), 3,
            caption = paste("Fitted", feature, "effect for", explorer_label(result, id))
          ),
          "<p>", html_escape(attr(effect, "interval_note") %||% ""), "</p></details>"
        )
      }
      paste0(
        '<article class="effect-card" data-feature-panel="', html_escape(feature), '">',
        "<h4>", html_escape(feature), "</h4>", content, "</article>"
      )
    }, character(1))
    paste0(
      '<div class="model-panel" data-model-panel="', html_escape(id), '">',
      '<h3 class="static-model-heading">', html_escape(explorer_label(result, id)), "</h3>",
      '<div class="feature-layout">',
      "<div>", explorer_importance(rows, audit$config$metric, id), '</div><div class="effect-workspace">',
      '<label class="control">Fitted pattern <select class="feature-select">', explorer_options(features),
      "</select></label>", paste(curves, collapse = ""), "</div></div></div>"
    )
  }, character(1))
  paste0(
    '<section id="patterns" class="workspace-page" data-page="patterns" aria-labelledby="patterns-title">',
    '<p class="section-number">02 / Features</p><h2 id="patterns-title">Feature importance &amp; effects',
    explorer_help(
      "Exploring fitted effects",
      paste(
        "Switch models to compare fitted reliance. Click an input to select its curve. ALE shows centered",
        "local effects; PDP shows average predictions. These are fitted associations,",
        "not predictions of an intervention."
      )
    ),
    "</h2>", explorer_model_control(models, result$provenance$primary_model_id, "feature-model-select"),
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

explorer_relationships <- function(result, audit) {
  relationships <- explorer_relationship_data(result, audit)
  features <- relationships$features
  pairs <- relationships$pairs
  rows <- vapply(features, function(a) {
    cells <- vapply(features, function(b) {
      pair <- pairs[pairs$a == a & pairs$b == b, ]
      value <- pair$value
      strength <- if (is.finite(value)) abs(value) else 0
      color <- if (!is.finite(value)) {
        "#eeeeec"
      } else if (value < 0) {
        grDevices::rgb(1 - .15 * strength, 1 - .62 * strength, 1 - .67 * strength)
      } else {
        grDevices::rgb(1 - .83 * strength, 1 - .55 * strength, 1 - .42 * strength)
      }
      rgb <- as.numeric(grDevices::col2rgb(color)) / 255
      linear <- ifelse(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
      luminance <- sum(linear * c(0.2126, 0.7152, 0.0722))
      text <- if (luminance < 0.179) "#ffffff" else "#000000"
      detail <- paste0(
        a, " and ", b, ": ", pair$method, " = ", explorer_measurement(value),
        " from ", pair$n, " complete training rows."
      )
      paste0(
        '<td><button type="button" class="association-cell" style="background:', color,
        ";color:", text, '" data-pair-detail="', html_escape(detail), '" aria-label="', html_escape(detail),
        '">', if (is.finite(value)) format(round(value, 2), nsmall = 2) else "n/a", "</button></td>"
      )
    }, character(1))
    paste0('<tr><th scope="row">', html_escape(a), "</th>", paste(cells, collapse = ""), "</tr>")
  }, character(1))
  paste0(
    '<section id="relationships" class="workspace-page" data-page="relationships" ',
    'aria-labelledby="relationships-title"><p class="section-number">03 / Relationships</p>',
    '<h2 id="relationships-title">Which inputs move together?',
    explorer_help(
      "Reading relationships",
      paste(
        "Numeric pairs use signed Spearman correlation. Pairs involving categories use unsigned association:",
        "correlation ratio for mixed types and Cramer's V for categories.",
        "Click a cell for its method and sample count.",
        "A value near zero does not rule out nonlinear or joint relationships."
      )
    ),
    '</h2><p class="matrix-legend"><span>\u22121 \u00b7 numeric, opposite direction</span>',
    "<span>0 \u00b7 weak measured association</span><span>1 \u00b7 strong association</span></p>",
    '<div class="table-wrap matrix-wrap" tabindex="0" role="region" aria-label="Input relationships">',
    '<table class="relationship-matrix"><caption>', length(features), " of ", relationships$total,
    " model inputs \u00b7 ", relationships$rows, " training rows after preprocessing</caption><thead><tr><th></th>",
    paste0('<th scope="col">', html_escape(features), "</th>", collapse = ""), "</tr></thead><tbody>",
    paste(rows, collapse = ""), "</tbody></table></div>",
    '<p id="pair-detail" class="pair-detail" role="status">Select a pair above.</p>',
    "<details><summary>All displayed pairs and sample counts</summary>",
    html_table(pairs[pairs$a != pairs$b, ], 3, caption = "Measured pairwise associations"), "</details></section>"
  )
}

explorer_predictions <- function(result, models) {
  explainers <- as_explainers(result, models = models$table$model_id)
  predictions <- lapply(explainers, function(x) predict(x, x$data))
  observed <- explainers[[1]]$y
  panels <- vapply(names(explainers), function(id) {
    prediction <- predictions[[id]]
    if (result$task == "regression") {
      errors <- observed - prediction
      display <- data.frame(
        Row = seq_along(observed), Observed = observed,
        Predicted = prediction, Error = errors
      )
      display <- head(display[order(-abs(errors)), ], 10)
      chart <- explorer_prediction_svg(observed, prediction, result, id)
      summary <- paste0(
        "Average absolute error: ", explorer_measurement(mean(abs(errors))), ". ",
        "Largest absolute error: ", explorer_measurement(max(abs(errors))), "."
      )
    } else {
      labels <- explainers[[id]]$class_levels
      predicted <- if (is.matrix(prediction)) {
        labels[max.col(prediction, ties.method = "first")]
      } else {
        ifelse(prediction >= .5, labels[2], labels[1])
      }
      confusion <- table(Observed = factor(observed, levels = labels), Predicted = factor(predicted, levels = labels))
      display <- data.frame(Observed = rownames(confusion), as.data.frame.matrix(confusion), check.names = FALSE)
      chart <- html_table(display, 0, caption = "Observed classes (rows) and predicted classes (columns)")
      summary <- paste0(
        sum(predicted != observed), " of ", length(observed), " rows were classified incorrectly.",
        if (result$task == "binary") paste0(" Probability \u22650.5 predicts ", labels[2], ".") else ""
      )
      display <- data.frame(Row = seq_along(observed), Observed = as.character(observed), Predicted = predicted)
      display <- head(display[predicted != observed, , drop = FALSE], 10)
    }
    paste0(
      '<div class="model-panel" data-model-panel="', html_escape(id), '"><h3 class="model-heading">',
      html_escape(explorer_label(result, id)), '</h3><p class="task-intro">', html_escape(summary), "</p>", chart,
      "<details><summary>", if (result$task == "regression") "Largest observed errors" else "Example mistakes",
      "</summary>", html_table(display, 3, caption = "Evaluation-row positions, not training rows"), "</details>",
      if (result$task != "regression") explorer_calibration(explainers[[id]], prediction),
      '<h3>Use this model in R</h3><pre tabindex="0" aria-label="Prediction command"><code>',
      html_escape(paste0('predict(result, new_data, model = "', id, '")')),
      "</code></pre></div>"
    )
  }, character(1))
  labels <- lapply(names(explainers), function(id) {
    p <- predictions[[id]]
    if (result$task == "regression") {
      return(p)
    }
    if (is.matrix(p)) {
      colnames(p)[max.col(p, ties.method = "first")]
    } else {
      ifelse(p >= .5, explainers[[id]]$class_levels[2], explainers[[id]]$class_levels[1])
    }
  })
  agreement <- outer(seq_along(labels), seq_along(labels), Vectorize(function(a, b) {
    if (result$task == "regression") {
      if (stats::sd(labels[[a]]) == 0 || stats::sd(labels[[b]]) == 0) {
        return(NA_real_)
      }
      stats::cor(labels[[a]], labels[[b]], method = "spearman")
    } else {
      mean(labels[[a]] == labels[[b]])
    }
  }))
  colnames(agreement) <- models$table$model
  agreement <- data.frame(Model = models$table$model, agreement, check.names = FALSE)
  paste0(
    '<section id="evaluation" class="workspace-page" data-page="evaluation" ',
    'aria-labelledby="evaluation-title"><p class="section-number">04 / Predictions</p>',
    '<h2 id="evaluation-title">Where do predictions go wrong?</h2>',
    explorer_model_control(models, result$provenance$primary_model_id, "prediction-model-select"),
    paste(panels, collapse = ""), "<details><summary>How similarly do the models predict?</summary>",
    html_table(agreement, 3, caption = if (result$task == "regression") {
      "Signed Spearman correlation between predictions; constant predictions are unavailable"
    } else {
      "Fraction of evaluation rows assigned the same class (0\u20131)"
    }),
    "<p>Agreement compares these fitted models on the same rows. Similar predictions can still be wrong.</p>",
    "</details><details><summary>Where do model predictions differ most?</summary>",
    render_prediction_ambiguity(result), "</details></section>"
  )
}

explorer_prediction_svg <- function(observed, predicted, result, model) {
  selected <- unique(round(seq(1, length(observed), length.out = min(400, length(observed)))))
  limits <- plot_limits(c(observed, predicted))
  px <- function(x) 70 + (x - limits[1]) / diff(limits) * 470
  py <- function(x) 360 - (x - limits[1]) / diff(limits) * 320
  ticks <- paste(vapply(pretty(limits, 5), function(x) {
    if (x < limits[1] || x > limits[2]) {
      return("")
    }
    paste0(
      '<line class="grid-line" x1="70" x2="540" y1="', py(x), '" y2="', py(x), '"/>',
      '<text class="tick" x="58" y="', py(x) + 5, '" text-anchor="end">', explorer_measurement(x), "</text>",
      '<text class="tick" x="', px(x), '" y="384" text-anchor="middle">', explorer_measurement(x), "</text>"
    )
  }, character(1)), collapse = "")
  points <- paste0('<circle class="prediction-point" cx="', px(observed[selected]), '" cy="',
    py(predicted[selected]), '" r="4"><title>Row ', selected, ": observed ",
    html_escape(format(observed[selected], digits = 4)), ", predicted ",
    html_escape(format(predicted[selected], digits = 4)), "</title></circle>",
    collapse = ""
  )
  paste0(
    '<div class="chart-scroll" tabindex="0" role="region" aria-label="Observed versus predicted">',
    '<svg class="prediction-plot" viewBox="0 0 600 430" role="img" aria-label="Observed versus predicted for ',
    html_escape(model), '">', ticks,
    '<line class="zero-line" x1="70" x2="540" y1="360" y2="40"/>', points,
    '<text class="axis-label" x="70" y="22">Predicted ', html_escape(result$target_column), "</text>",
    '<text class="axis-label" x="305" y="415" text-anchor="middle">Observed ',
    html_escape(result$target_column), "</text></svg></div>",
    '<p class="chart-help">Points on the diagonal are exact predictions. Above it: overprediction. ',
    "Below it: underprediction. Showing ", length(selected), " of ", length(observed), " evaluation rows.</p>"
  )
}

explorer_checks <- function(result, audit) {
  concise <- audit
  repeated <- concise$findings$code %in% c("association_screen_scope", "feature_dependence")
  concise$findings <- concise$findings[!repeated, , drop = FALSE]
  paste0(
    '<section id="checks" class="workspace-page" data-page="checks" aria-labelledby="checks-title">',
    '<p class="section-number">05 / Checks</p><h2 id="checks-title">What needs a closer look?</h2>',
    "<p>Input relationships are shown in the Relationships view. Here are the other checks and ",
    "recorded calculation failures.</p>",
    render_guided_notes(result$evaluation$notes), render_reliability_section(concise, result),
    render_effect_failures(result), render_performance_uncertainty(result$performance_uncertainty),
    render_threshold_diagnostic(result), render_missingness_shift(result$evaluation$diagnostics$missingness_shift),
    "</section>"
  )
}

model_explorer_html <- function(result, audit, effects, narrative, subgroup_check, title) {
  models <- explorer_models(result)
  identity <- report_view_model(result, audit)$identity
  nav <- c(
    overview = "Compare models", patterns = "Explore features", relationships = "Input relationships",
    evaluation = "Inspect predictions", checks = "Checks & uncertainty", provenance = "Methods & export"
  )
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
    explorer_overview(result, audit, models), explorer_features(result, audit, effects, models),
    explorer_relationships(result, audit), explorer_predictions(result, models), explorer_checks(result, audit),
    subgroup_html,
    '<section id="provenance" class="workspace-page" data-page="provenance" aria-labelledby="provenance-title">',
    '<p class="section-number">06 / Methods & export</p><h2 id="provenance-title">Keep the analysis usable</h2>',
    '<pre><code>saveRDS(result, "analysis.rds")\nevidence_summary(result)\n',
    "result$leaderboard\nresult$explanations$audit</code></pre>",
    "<details><summary>Run settings and validation design</summary>",
    definition_list(c(
      Target = identity$target, Task = identity$task, Engine = identity$engine,
      `Training rows` = identity$training_rows, `Evaluation rows` = identity$evaluation_rows,
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
    "<style>", report_css(), "\n", report_asset("explorer.css"), '</style></head><body class="explorer">',
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
    html_escape(identity$task), " \u00b7 ", html_escape(identity$evaluation_role), " evaluation</p>",
    "<h1>", html_escape(title), '</h1><p class="run-context">', nrow(models$table), " retained models \u00b7 ",
    identity$training_rows, " training rows \u00b7 ", identity$evaluation_rows, " evaluation rows",
    if (!is.null(identity$target_units)) paste0(" \u00b7 ", html_escape(identity$target_units)) else "",
    if (!is.null(identity$positive)) paste0(" \u00b7 Probability event: ", html_escape(identity$positive)) else "",
    '</p></div><button type="button" id="print-report" class="quiet-button">Print this view</button></header>',
    '<main id="main">', pages, "</main><footer>AutoXplainR ", html_escape(result$provenance$package_version),
    " \u00b7 ", html_escape(identity$split_method), "</footer></div>",
    "<script>", report_asset("explorer.js"), "</script></body></html>"
  )
}

explorer_calibration <- function(explainer, prediction) {
  calibration <- calibration_from_explainer(explainer, predicted = prediction)
  paste0(
    '<details class="calibration"><summary>Probability calibration</summary>',
    "<p>Binned calibration gap: ", explorer_measurement(calibration$calibration_error), "</p>",
    html_table(calibration$groups, 3, caption = "Average predicted probability and observed frequency"),
    "<p>Descriptive binned comparison for this model and these evaluation rows; ",
    "results depend on the grouping and sample size.</p></details>"
  )
}
