# This section only renders retained training evidence. Browser controls filter
# displayed candidates; they never fit, score or alter the official selection.
render_model_selection <- function(result) {
  evidence <- tuning_evidence(result)
  opening <- paste0(
    '<section id="selection" class="workspace-page selection-section" ',
    'data-page="selection" aria-labelledby="selection-title"><h2 id="selection-title">Why these settings?</h2>'
  )
  if (!identical(evidence$status, "computed")) {
    return(selection_model_provenance(result, opening, evidence$reason))
  }
  decision <- evidence$selection
  candidates <- evidence$candidates
  picked <- candidates[match(decision$selected_configuration, candidates$configuration_id), , drop = FALSE]
  final <- candidates[match(evidence$final_configuration, candidates$configuration_id), , drop = FALSE]
  best <- candidates[match(decision$best_configuration, candidates$configuration_id), , drop = FALSE]
  families <- unique(candidates$family)
  labels <- stats::setNames(vapply(families, selection_family_label, character(1), task = evidence$task), families)
  options <- paste0('<option value="', html_escape(families), '"',
    ifelse(families == final$family, " selected", ""), ">", html_escape(labels), "</option>",
    collapse = ""
  )
  card <- function(label, row, note = "") {
    metric_card(
      label,
      paste(report_number(row$cv_score, 4), pretty_metric(evidence$metric)),
      paste(
        selection_family_label(row$family, evidence$task), "\u00b7",
        selection_short_parameters(row, evidence), note
      )
    )
  }
  agreed <- identical(best$configuration_id, picked$configuration_id) &&
    identical(picked$configuration_id, final$configuration_id)
  cards <- if (agreed) {
    settings <- paste(
      selection_family_label(final$family, evidence$task), "\u00b7",
      selection_short_parameters(final, evidence)
    )
    paste0(
      '<div class="selection-agreed"><strong>',
      html_escape(paste(report_number(final$cv_score, 4), pretty_metric(evidence$metric))), "</strong><span>",
      html_escape(settings), "</span>",
      "<small>Lowest CV loss, policy choice and final fit are the same configuration.</small></div>"
    )
  } else {
    paste0(
      '<div class="cards selection-decision">', card("Lowest CV loss", best),
      card("Policy choice", picked), card(
        "Final primary fit", final,
        if (identical(final$configuration_id, picked$configuration_id)) "(same configuration)" else "(refit fallback)"
      ),
      "</div>"
    )
  }
  threshold <- if (identical(decision$rule, "one_se")) {
    paste0(
      "Eligible loss \u2264 ", report_number(decision$best_score, 6), " + ",
      report_number(decision$best_se, 6), " = <strong>", report_number(decision$threshold, 6), "</strong>. ",
      "Among eligible candidates: first the preferred family, then its smallest recorded capacity proxy, ",
      "then score and configuration ID to break a tie."
    )
  } else {
    "Lowest pooled CV loss wins. Tied minima use the first scheduled configuration."
  }
  reason <- if (identical(decision$rule, "one_se")) {
    paste0(
      selection_family_label(picked$family, evidence$task),
      " was the first preferred family within the allowed loss of ",
      report_number(decision$threshold, 4), ". ",
      if (sum(candidates$family == picked$family) == 1L) {
        "Only one configuration was scheduled for this family."
      } else {
        paste(
          "Its chosen setting had the smallest recorded capacity proxy among eligible settings in that family;",
          "score breaks a proxy tie."
        )
      }
    )
  } else {
    "The policy chose the lowest pooled training CV loss."
  }
  if (!identical(picked$configuration_id, final$configuration_id)) {
    reason <- paste(reason, "The original choice failed refitting; a recorded CV-ordered fallback became the primary.")
  }
  priority <- decision$family_priority
  priority$family <- vapply(priority$family, selection_family_label, character(1), task = evidence$task)
  names(priority) <- c("Family", "Preference order", "Within-family capacity proxy")
  family_views <- vapply(families, function(family) {
    rows <- candidates[candidates$family == family, , drop = FALSE]
    paste0(
      '<div class="selection-family" data-selection-family="', html_escape(family), '">',
      selection_family_rationale(family, evidence), selection_candidate_plot(rows, evidence),
      '<details class="selection-precision"><summary>Exact settings and CV scores \u00b7 ',
      nrow(rows), if (nrow(rows) == 1L) " configuration" else " configurations", "</summary>",
      selection_candidate_table(rows, evidence), "</details></div>"
    )
  }, character(1))
  details <- vapply(seq_len(nrow(candidates)), function(index) {
    selection_candidate_details(candidates[index, , drop = FALSE], evidence)
  }, character(1))
  paste0(
    opening,
    '<p class="selection-context">Training-only cross-validation; lower loss is better. ',
    "These CV scores are not final performance estimates.</p>", cards,
    '<p class="selection-reason">', html_escape(reason), "</p>",
    selection_failure_summary(evidence), selection_family_summary(evidence),
    '<div class="selection-filter" hidden><label for="selection-family-filter">Inspect settings for</label> ',
    '<select id="selection-family-filter">', options, '<option value="all">All families</option></select>',
    ' <span id="selection-visible-count" role="status" aria-live="polite"></span></div>',
    "<noscript><style>@media(max-width:650px){.selection-chart-wrap,.selection-chart-help{display:none}}</style>",
    "<p>All candidate and fold numbers are available under Exact settings and CV scores ",
    "and in the expandable fold details below.</p></noscript>",
    paste(family_views, collapse = ""),
    '<div class="selection-fold-details">', paste(details, collapse = ""), "</div>",
    '<details class="advanced"><summary>Exact selection arithmetic and preference order</summary>',
    '<p class="selection-threshold">', threshold, "</p>", html_table(priority),
    "<p>", html_escape(decision$priority_scope), "</p><p>", html_escape(decision$score_method), " ",
    html_escape(decision$variability_scope), "</p>",
    "<p>Preprocessing was learned again inside every fold. The evaluation partition was not used ",
    "to choose hyperparameters. Full fold assignments and any retained out-of-fold predictions ",
    "remain in <code>tuning_results(result)</code>.</p></details>",
    selection_input_policy(evidence), selection_search_details(evidence), selection_refit_details(evidence),
    '<details class="advanced"><summary>Copy and change this search grid in R</summary>',
    "<p>This executable control reproduces the scheduled tuples and budgets. Edit a tuple to test a ",
    "specific hypothesis. Refit using your original data, preprocessing, validation design and seed. ",
    "This control alone does not reproduce the entire analysis. ",
    "Avoid repeatedly tuning against the evaluation partition.</p>",
    "<pre><code>", html_escape(selection_grid_code(result)), "</code></pre>",
    "<p>Use <code>tuning_evidence(result)</code> for the aggregate record.</p></details>",
    selection_source_details(evidence),
    report_json_script(
      list(schema_version = 1L, candidates = candidates[c("configuration_id", "family")]),
      "selection-evidence"
    ), "</section>"
  )
}

selection_input_policy <- function(evidence) {
  policies <- evidence$input_policy
  if (!length(policies)) {
    return("")
  }
  entries <- vapply(names(policies), function(family) {
    policy <- policies[[family]]
    value <- policy$encoding %||% policy$solver
    dimensions <- if (identical(family, "boosting")) {
      data.frame(
        `Training rows` = policy$rows, `Input columns` = policy$input_columns,
        `Estimated contrast columns` = policy$matrix_columns_estimate,
        `Estimated matrix cells` = policy$matrix_cells_estimate, check.names = FALSE
      )
    } else if (length(policy$configurations)) {
      do.call(rbind, lapply(names(policy$configurations), function(id) {
        decision <- policy$configurations[[id]]
        data.frame(
          Configuration = id, Solver = decision$solver, `Training rows` = decision$fitting_rows,
          `Estimated coefficients` = decision$estimated_coefficients,
          `Work index` = decision$work_index, check.names = FALSE
        )
      }))
    } else {
      NULL
    }
    paste0(
      "<p><strong>", html_escape(selection_family_label(family, evidence$task)), ": ",
      html_escape(value), "</strong>. ", html_escape(policy$reason), "</p>",
      if (!is.null(dimensions)) html_table(dimensions, digits = 0), "<p>",
      html_escape(policy$scope), "</p>"
    )
  }, character(1))
  paste0(
    '<details class="advanced"><summary>Computation choices for this search</summary>',
    paste(entries, collapse = ""), "</details>"
  )
}

selection_model_provenance <- function(result, opening, unavailable_reason = NULL) {
  opening <- sub("Why these settings?", "Model provenance", opening, fixed = TRUE)
  workflow <- result$provenance$workflow
  supplied <- identical(workflow, "supplied-model evaluation")
  primary <- result$provenance$primary_model_id
  description <- if (supplied) {
    paste(
      "These models were fitted before this evaluation. The caller chose the primary model.",
      "AutoXplainR did not observe their fitting or an internal hyperparameter search."
    )
  } else if (identical(result$engine, "h2o")) {
    paste(
      "H2O AutoML created the retained models using its engine search.",
      "The recorded engine selection and leaderboard are shown below; no local preset grid is inferred."
    )
  } else if (isTRUE(result$provenance$model_set %in% c("quick", "comparison"))) {
    paste(
      "This run used a predefined model set, without a hyperparameter search.",
      "The primary linear or logistic model and intercept-only baseline were specified before evaluation.",
      if (identical(result$provenance$model_set, "comparison")) {
        "Two trees with preset depth and pruning controls were added for comparison."
      } else {
        ""
      }
    )
  } else if (inherits(result$tuning, "autoxplain_tuning") && !is.null(unavailable_reason)) {
    unavailable_reason
  } else {
    "No local training-only tuning record is retained; the available selection statement is shown below."
  }
  training <- result_training_rows(result)
  training_context <- if (is.na(training)) {
    "Training data were not provided."
  } else if (supplied) {
    paste(training, "rows supplied as training context; their use in the original fit is not verified.")
  } else {
    paste(training, "processed training rows retained from this run.")
  }
  ids <- names(result$models)
  links <- paste0('<li><a href="#spec-', vapply(ids, report_anchor, character(1)), '">',
    html_escape(vapply(ids, function(id) explorer_label(result, id), character(1))), "</a>",
    ifelse(ids == primary, " (primary)", ""), "</li>",
    collapse = ""
  )
  paste0(
    opening, "<p>", html_escape(description), "</p>",
    definition_list(c(
      `Primary model` = explorer_label(result, primary),
      `Recorded selection` = result$provenance$candidate_selection %||% "Not recorded",
      `Training context` = training_context
    )), "<h3>Inspect the retained models</h3><ul>", links, "</ul>",
    if (identical(result$engine, "h2o")) {
      paste0(
        "<p>Requested leaderboard metric: <code>", html_escape(result$provenance$sort_metric %||% "Not recorded"),
        "</code>. Engine leaderboard columns and values are retained as returned by H2O.</p>",
        html_table(result$engine_leaderboard, caption = "Recorded H2O engine leaderboard")
      )
    },
    if (inherits(result$tuning, "autoxplain_tuning")) {
      candidates <- result$tuning$candidates
      columns <- intersect(
        c("configuration_id", "family", "hyperparameters", "cv_score", "status", "retained_model_id"),
        names(candidates)
      )
      html_table(candidates[columns],
        caption = "Retained candidate results; exact original selection decision unavailable"
      )
    }, "</section>"
  )
}

selection_family_label <- function(family, task = NULL) {
  labels <- c(
    linear = switch(task %||% "regression",
      binary = "Logistic regression",
      multiclass = "Multinomial logistic",
      "Linear regression"
    ), regularized = "Regularized linear", additive = "Additive model",
    tree = "Decision tree", forest = "Random forest", boosting = "Boosted trees", neural = "Neural network",
    kernel = "Radial support vector", neighbors = "Nearest neighbors", mars = "Adaptive splines"
  )
  unname(labels[[family]])
}

selection_short_parameters <- function(candidate, evidence) {
  index <- match(candidate$configuration_id, evidence$folds$configuration_id)
  p <- evidence$folds$requested_parameters[[index]]
  text <- switch(candidate$family,
    linear = "No tuned controls",
    neural = paste(p$size, if (p$size == 1L) "unit; decay" else "units; decay", p$decay),
    tree = paste0("depth ", p$maxdepth, "; cp ", p$cp, "; split ", p$minsplit),
    regularized = paste0("alpha ", p$alpha, "; path ", p$path_fraction),
    additive = paste0("k ", p$k, "; gamma ", p$gamma, "; select ", p$select),
    neighbors = paste0("k ", p$k, "; ", p$kernel),
    kernel = paste0("cost ", p$cost, "; gamma \u00d7 ", p$gamma_multiplier),
    forest = paste0("mtry ", p$mtry, "; node ", p$min.node.size, "; ", p$num.trees, " trees"),
    boosting = paste0(p$nrounds, " rounds; eta ", p$eta, "; depth ", p$max_depth),
    mars = paste0("degree ", p$degree, "; terms ", p$nprune),
    candidate$hyperparameters
  )
  as.character(text)
}

selection_family_summary <- function(evidence) {
  table <- evidence$families
  rows <- vapply(seq_len(nrow(table)), function(index) {
    family <- table$family[[index]]
    candidates <- evidence$candidates[evidence$candidates$family == family, , drop = FALSE]
    best <- candidates$cv_score[match(table$best_cv[[index]], candidates$configuration_id)]
    paste0(
      '<tr><th scope="row"><button type="button" class="selection-family-pick" ',
      'data-selection-pick-family="', html_escape(family), '">',
      html_escape(selection_family_label(family, evidence$task)),
      '</button></th><td class="number">', report_number(best, 4), '</td><td class="number">',
      sum(candidates$within_threshold), " / ", table$valid[[index]], '</td><td class="number">',
      table$failed[[index]], "</td></tr>"
    )
  }, character(1))
  paste0(
    '<div class="table-wrap selection-family-overview"><table><caption>Training CV across families</caption>',
    '<thead><tr><th scope="col">Family</th><th scope="col">Lowest CV loss</th>',
    '<th scope="col">Within limit / successful ',
    explorer_help("Which settings could become the primary model?", paste(
      "The first count meets the primary-selection loss limit; the second completed cross-validation successfully.",
      "A successful setting outside that limit can still be retained as a family alternative."
    )), '</th><th scope="col">Failed</th></tr></thead><tbody>',
    paste(rows, collapse = ""), "</tbody></table></div>"
  )
}

selection_family_rationale <- function(family, evidence) {
  space <- evidence$search_space
  if (identical(space$status, "not_recorded")) {
    return(paste0("<p>", html_escape(space$limitation), "</p>"))
  }
  entry <- space$families[space$families$family == family, , drop = FALSE]
  candidates <- evidence$candidates[evidence$candidates$family == family, , drop = FALSE]
  first_folds <- match(candidates$configuration_id, evidence$folds$configuration_id)
  tuples <- evidence$folds$requested_parameters[first_folds]
  parameters <- if (length(tuples)) names(tuples[[1L]]) else character()
  tested <- paste(vapply(parameters, function(parameter) {
    values <- unique(vapply(tuples, function(tuple) model_spec_value(tuple[[parameter]]), character(1)))
    paste0(parameter, ": ", paste(values, collapse = ", "))
  }, character(1)), collapse = " \u00b7 ")
  bounds <- evidence$boundaries
  bounds <- if (nrow(bounds)) bounds[bounds$family == family, ] else bounds
  edge <- if (nrow(bounds)) bounds[bounds$position %in% c("lower edge", "upper edge"), ] else bounds
  boundary <- if (nrow(edge)) {
    paste0(
      "Family CV winner is at a tested edge: ",
      paste(paste0(edge$parameter, " = ", edge$selected_value), collapse = "; "),
      ". Other controls may vary too; extending one range is a new experiment, not an expected improvement."
    )
  } else if (!any(candidates$status == "ok")) {
    "No candidate completed every fold, so this family has no CV winner."
  } else if (!nrow(bounds)) {
    "There were no numeric tuning controls to assess at a boundary."
  } else if (all(bounds$position == "fixed")) {
    "All numeric controls were fixed; this run did not test their sensitivity."
  } else {
    "The family CV winner is interior on every varied numeric control in the scheduled range."
  }
  if (any(candidates$status != "ok")) {
    boundary <- paste(
      boundary,
      "Scheduled ranges include failed attempts; only", sum(candidates$status == "ok"),
      "of", nrow(candidates), "configurations completed every fold."
    )
  }
  paste0(
    '<div class="selection-rationale"><h3>', html_escape(selection_family_label(family, evidence$task)),
    ": search rationale</h3>",
    "<p>", html_escape(entry$rationale), " ", entry$scheduled, " of ", entry$available,
    " available tuples scheduled; ", entry$untested, " untested.</p>",
    if (entry$origin == "package_presets" && length(parameters)) {
      '<p class="microcopy">Preset values are package choices, not data-optimized ranges.</p>'
    } else {
      ""
    },
    if (nzchar(tested)) paste0('<p class="selection-tested"><strong>Tested:</strong> ', html_escape(tested), "</p>"),
    '<p class="selection-boundary">', html_escape(boundary), "</p></div>"
  )
}

selection_candidate_table <- function(candidates, evidence) {
  rows <- vapply(seq_len(nrow(candidates)), function(index) {
    row <- candidates[index, , drop = FALSE]
    target <- paste0("selection-detail-", report_anchor(row$configuration_id))
    family_best <- evidence$families$best_cv[match(row$family, evidence$families$family)]
    role <- c(
      if (isTRUE(row$selected)) "policy choice", if (isTRUE(row$final_fit)) "primary",
      if (isTRUE(row$lowest_cv)) "lowest CV" else if (identical(row$configuration_id, family_best)) "family CV best",
      if (!isTRUE(row$final_fit) && !is.na(row$retained_model_id %||% NA_character_)) "retained alternative"
    )
    paste0(
      '<tr><td><a href="#', target, '" data-selection-inspect="', target, '">',
      html_escape(if (row$family == "linear") "No tuned controls" else row$hyperparameters),
      "</a><small>", html_escape(row$configuration_id),
      if (length(role)) paste0(" \u00b7 ", html_escape(paste(role, collapse = ", "))), "</small></td>",
      '<td class="number">', report_number(row$cv_score, 4), "</td><td>",
      if (row$status != "ok") "failed" else if (isTRUE(row$within_threshold)) "eligible" else "outside threshold",
      "</td></tr>"
    )
  }, character(1))
  paste0(
    '<div class="table-wrap selection-candidate-table"><table>',
    "<caption>Choose settings to inspect fold evidence</caption>",
    '<thead><tr><th scope="col">Actual requested settings</th><th scope="col">CV loss</th>',
    '<th scope="col">Selection status</th></tr></thead><tbody>', paste(rows, collapse = ""), "</tbody></table></div>"
  )
}

selection_candidate_plot <- function(candidates, evidence) {
  folds <- evidence$folds[evidence$folds$configuration_id %in% candidates$configuration_id, , drop = FALSE]
  values <- c(candidates$cv_score, folds$score, evidence$selection$threshold)
  values <- values[is.finite(values)]
  if (!length(values)) {
    return("<p>No complete candidate score was available.</p>")
  }
  limits <- range(values)
  padding <- max(diff(limits) * .08, abs(mean(limits)) * .02, .001)
  limits <- c(max(0, limits[1L] - padding), limits[2L] + padding)
  left <- 285
  right <- 710
  x <- function(value) left + (value - limits[1L]) / diff(limits) * (right - left)
  height <- 72 + 38 * nrow(candidates)
  ticks <- pretty(limits, n = 4)
  ticks <- ticks[ticks >= limits[1L] & ticks <= limits[2L]]
  grid <- paste(vapply(ticks, function(tick) {
    paste0(
      '<line class="grid-line selection-grid" data-value="', tick, '" x1="', x(tick), '" x2="', x(tick),
      '" y1="16" y2="', height - 43, '"/>',
      '<text class="tick selection-axis-tick" data-value="', tick, '" x="', x(tick), '" y="', height - 22,
      '" text-anchor="middle">', html_escape(report_axis_number(tick)), "</text>"
    )
  }, character(1)), collapse = "")
  rows <- vapply(seq_len(nrow(candidates)), function(index) {
    candidate <- candidates[index, , drop = FALSE]
    y <- 31 + (index - 1L) * 38
    id <- candidate$configuration_id
    fold_scores <- folds$score[folds$configuration_id == id]
    fold_scores <- fold_scores[is.finite(fold_scores)]
    points <- if (length(fold_scores)) {
      paste0('<circle class="selection-fold-point" data-value="', fold_scores, '" cx="', x(fold_scores), '" cy="', y,
        '" r="3"><title>Fold loss: ', html_escape(format(fold_scores, digits = 8)), "</title></circle>",
        collapse = ""
      )
    } else {
      ""
    }
    labels <- c(
      if (isTRUE(candidate$lowest_cv)) "lowest CV",
      if (isTRUE(candidate$selected)) "policy", if (isTRUE(candidate$final_fit)) "final"
    )
    status <- if (candidate$status != "ok") {
      "failed"
    } else {
      paste(c(
        if (isTRUE(candidate$final_fit)) {
          "primary"
        } else if (isTRUE(candidate$selected)) {
          "policy choice"
        } else if (isTRUE(candidate$lowest_cv)) {
          "lowest CV"
        } else if (!is.na(candidate$retained_model_id %||% NA_character_)) {
          "retained"
        },
        if (isTRUE(candidate$within_threshold)) "eligible" else "outside limit"
      ), collapse = " \u00b7 ")
    }
    target <- paste0("selection-detail-", report_anchor(id))
    description <- paste(
      candidate$hyperparameters, "; pooled CV loss", report_number(candidate$cv_score, 6),
      ";", paste(labels, collapse = ", "), status, "; open fold details"
    )
    score <- if (is.finite(candidate$cv_score)) {
      paste0(
        '<circle class="selection-score-point', if (isTRUE(candidate$selected)) " selection-picked",
        '" data-value="', candidate$cv_score, '" cx="', x(candidate$cv_score), '" cy="', y, '" r="6"><title>',
        html_escape(paste(id, "pooled loss", format(candidate$cv_score, digits = 8))), "</title></circle>"
      )
    } else {
      ""
    }
    paste0(
      '<a class="selection-plot-link" href="#', target, '" data-selection-inspect="', target,
      '" aria-label="', html_escape(description), '">',
      '<g class="selection-plot-row" data-row="', index - 1L,
      '"><rect class="selection-row-hit" x="0" y="', y - 17, '" width="920" height="36"/>',
      '<text class="tick selection-row-label" x="8" y="', y + 4,
      '"><title>', html_escape(candidate$hyperparameters), "</title>",
      "<tspan>", html_escape(selection_short_parameters(candidate, evidence)), "</tspan></text>", points, score,
      '<text class="selection-role" x="725" y="', y - 3, '">',
      '<tspan class="selection-score-label">', report_number(candidate$cv_score, 4), "</tspan>",
      '<tspan class="selection-status-label" x="725" dy="15">', html_escape(status),
      "</tspan></text></g></a>"
    )
  }, character(1))
  threshold <- x(evidence$selection$threshold)
  paste0(
    '<p class="selection-chart-help"><span>',
    '<span aria-hidden="true" class="selection-key-pooled">\u25cf</span> pooled CV</span>',
    '<span><span aria-hidden="true" class="selection-key-fold">\u25cb</span> fold</span>',
    '<span><span aria-hidden="true" class="selection-key-limit">\u2506</span> loss limit</span>',
    explorer_help("Reading the candidate comparison", paste(
      "Filled dots show pooled CV loss; open dots show each fold. The dashed line marks the allowed loss.",
      "Fold scores show variability, not confidence intervals. Each family has its own numeric scale.",
      "Choose a row to inspect its fold scores, requested and effective settings, seeds and optimizer status."
    ), id = paste0("selection-chart-help-", report_anchor(candidates$family[[1L]]))), "</p>",
    '<div class="selection-chart-wrap"><svg class="selection-plot" data-min="', limits[[1L]],
    '" data-max="', limits[[2L]], '" data-rows="', nrow(candidates), '" viewBox="0 0 920 ', height,
    '" role="group" aria-label="', html_escape(paste(
      candidates$family[[1L]],
      "candidate and fold", evidence$metric, "scores; numeric values are in candidate details below."
    )), '">', grid,
    '<line class="selection-cutoff" data-value="', evidence$selection$threshold,
    '" x1="', threshold, '" x2="', threshold, '" y1="12" y2="', height - 43, '"/>',
    paste(rows, collapse = ""), '<text class="axis-label selection-axis-label" x="495" y="', height - 3,
    '" text-anchor="middle">', html_escape(pretty_metric(evidence$metric)), " (lower is better)</text></svg></div>"
  )
}

selection_candidate_details <- function(candidate, evidence) {
  id <- candidate$configuration_id
  folds <- evidence$folds[evidence$folds$configuration_id == id, , drop = FALSE]
  columns <- intersect(c(
    "fold", "score", "training_rows", "validation_rows", "validation_rows_omitted",
    "fit_seed", "optimization_status", "warning", "error"
  ), names(folds))
  table <- folds[columns]
  for (column in intersect(c("warning", "error"), names(table))) {
    if (all(is.na(table[[column]]) | !nzchar(trimws(table[[column]])))) table[[column]] <- NULL
  }
  no_omissions <- "validation_rows_omitted" %in% names(table) && nrow(table) > 0L &&
    all(!is.na(table$validation_rows_omitted) & table$validation_rows_omitted == 0L)
  if (no_omissions) table$validation_rows_omitted <- NULL
  if ("optimization_status" %in% names(table)) {
    table$optimization_status <- selection_status_label(table$optimization_status)
  }
  labels <- c(
    fold = "Fold", score = "CV loss", training_rows = "Training rows", validation_rows = "Validation rows",
    validation_rows_omitted = "Rows omitted", fit_seed = "Seed", optimization_status = "Optimizer",
    warning = "Warnings", error = "Error"
  )
  names(table) <- unname(labels[names(table)])
  issues <- unique(c(folds$error, folds$warning))
  issues <- issues[!is.na(issues) & nzchar(issues)]
  detail <- vapply(seq_len(nrow(folds)), function(index) {
    learned <- folds$learned[[index]] %||% NULL
    # Reconstruction instructions belong in model details, not every CV fold.
    if (is.list(learned)) learned$call_reconstruction <- NULL
    paste0(
      "<li><strong>Fold ", html_escape(folds$fold[[index]]), "</strong>: ",
      "requested: ", html_escape(model_spec_value(folds$requested_parameters[[index]])),
      "; effective: ", html_escape(model_spec_value(folds$effective_parameters[[index]])),
      "; learned: ", html_escape(model_spec_value(learned)), "</li>"
    )
  }, character(1))
  paste0(
    '<details id="selection-detail-', report_anchor(id),
    '" class="advanced selection-candidate" data-selection-family="', html_escape(candidate$family),
    '"><summary>', html_escape(id), " \u00b7 ", html_escape(selection_status_label(candidate$status)), " \u00b7 CV ",
    report_number(candidate$cv_score, 6), " \u00b7 fold SE ", report_number(candidate$cv_se, 6),
    if (isTRUE(candidate$within_threshold)) " \u00b7 eligible" else "", "</summary>",
    if (length(issues)) paste0('<p class="callout">', html_escape(paste(issues, collapse = " ")), "</p>"),
    if (no_omissions) "<p>No validation rows were omitted in these folds.</p>",
    html_table(table, digits = 6L, caption = paste(id, "fold scores, sizes, seeds and engine status")),
    '<ul class="selection-settings">', paste(detail, collapse = ""), "</ul></details>"
  )
}

selection_status_label <- function(status) {
  status <- ifelse(status == "ok", "completed", gsub("_", " ", status, fixed = TRUE))
  ifelse(is.na(status), NA_character_, paste0(toupper(substr(status, 1L, 1L)), substring(status, 2L)))
}

selection_search_details <- function(evidence) {
  space <- evidence$search_space
  if (identical(space$status, "not_recorded")) {
    return(paste0('<p class="callout">', html_escape(space$limitation), "</p>"))
  }
  family_table <- space$families
  names(family_table) <- c(
    "Family", "Grid source", "Available tuples", "Scheduled tuples",
    "Untested tuples", "Rationale"
  )
  parameters <- lapply(names(space$grids), function(family) {
    grid <- space$grids[[family]]
    names <- names(grid[[1L]])
    if (!length(names)) {
      return(NULL)
    }
    do.call(rbind, lapply(names, function(parameter) {
      values <- unique(vapply(grid, function(tuple) model_spec_value(tuple[[parameter]]), character(1)))
      data.frame(
        Family = family, Parameter = parameter, `Available preset values` = paste(values, collapse = ", "),
        Meaning = tuning_parameter_meaning(family, parameter), check.names = FALSE
      )
    }))
  })
  parameters <- parameters[!vapply(parameters, is.null, logical(1))]
  boundaries <- evidence$boundaries
  boundary_table <- if (nrow(boundaries)) {
    html_table(boundaries[setdiff(names(boundaries), "interpretation")],
      caption = "Position of each family CV winner within scheduled numeric settings"
    )
  } else {
    ""
  }
  paste0(
    '<details class="advanced"><summary>Why this grid? Coverage, parameter meaning and boundary winners</summary>',
    "<p>", html_escape(space$allocation), " Outer training rows: ", space$outer_training_rows,
    "; raw inputs: ", space$raw_predictors, ".</p>", '<p class="callout">', html_escape(space$limitation), "</p>",
    html_table(family_table, caption = "Recorded search budget and rationale"),
    if (length(parameters)) {
      html_table(do.call(rbind, parameters), caption = "Available values and parameter meanings")
    } else {
      ""
    },
    boundary_table, '<p class="microcopy">A fixed setting was not varied. An edge winner can motivate a ',
    "new training-only comparison; it does not show that extending one control would help, because ",
    "several controls may differ between the tested tuples.</p></details>"
  )
}

selection_failure_summary <- function(evidence) {
  failures <- evidence$family_failures
  labels <- function(values) {
    paste(vapply(values, function(family) {
      paste0(selection_family_label(family, evidence$task), " (", family, ")")
    }, character(1)), collapse = ", ")
  }
  messages <- c(
    if (length(failures$resampling)) paste0("No complete resampling result: ", labels(failures$resampling), "."),
    if (length(failures$refit)) paste0("Full-training refit failed: ", labels(failures$refit), ".")
  )
  if (!length(messages)) {
    return("")
  }
  paste0(
    '<p class="callout selection-failures">', html_escape(paste(messages, collapse = " ")),
    " These families have no retained fit from the corresponding stage.</p>"
  )
}

selection_refit_details <- function(evidence) {
  refit <- evidence$refit
  attempts <- refit$attempts
  if (!is.data.frame(attempts) || !nrow(attempts)) {
    return("")
  }
  columns <- names(attempts)[!vapply(attempts, is.list, logical(1))]
  table <- attempts[columns]
  for (column in intersect(c("warning", "warnings", "error"), names(table))) {
    if (all(is.na(table[[column]]) | !nzchar(trimws(table[[column]])))) table[[column]] <- NULL
  }
  for (column in intersect(c("status", "optimization_status"), names(table))) {
    table[[column]] <- selection_status_label(table[[column]])
  }
  names(table) <- selection_status_label(names(table))
  paste0(
    '<details class="advanced"><summary>Full-training refit attempts and optimizer status</summary>',
    html_table(table, caption = "Refit attempts; evaluation scores do not choose a fallback"), "</details>"
  )
}

selection_grid_code <- function(result) {
  tuning <- result$tuning
  families <- unique(tuning$plan$family)
  grids <- stats::setNames(lapply(families, function(family) {
    unclass(tuning$plan$parameters[tuning$plan$family == family])
  }), families)
  budgets <- stats::setNames(vapply(grids, length, integer(1)), families)
  r_value <- function(value) paste(trimws(utils::capture.output(dput(value)), which = "right"), collapse = "\n")
  metric <- if (identical(tuning$metric, "brier_score")) "brier" else tuning$metric
  paste0(
    "control <- tuning_control(\n  grids = ", r_value(grids),
    ",\n  family_budgets = ", r_value(budgets), ",\n  metric = ", r_value(metric),
    ",\n  optimization_policy = ", r_value(tuning$control$optimization_policy %||% "exclude"),
    ",\n  family_priority = ", r_value(tuning[["selection", exact = TRUE]]$family_priority$family %||% NULL), "\n)\n",
    "# In your original autoxplain(...) call, use:\n",
    "#   learners = ", r_value(families), ", max_models = NULL, tuning_control = control\n",
    "#   tuning_rule = ", r_value(tuning$selection_rule), ", seed = ", result$provenance$seed, "\n",
    "# Keep the original preprocessing and validation design (including supplied fold IDs)."
  )
}

selection_source_details <- function(evidence) {
  rows <- evidence$sources
  links <- paste0('<li><a href="', html_escape(rows$url), '">', html_escape(rows$topic), "</a></li>", collapse = "")
  paste0(
    '<details class="advanced"><summary>Method references and limits</summary>',
    "<p>These sources explain selection bias, fold variability and backend controls. ",
    "They do not validate this package\u2019s exact preset numbers or family preference order.</p><ul>", links,
    "</ul></details>"
  )
}
