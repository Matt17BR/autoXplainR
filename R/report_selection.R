# This section only renders retained training evidence. Browser controls filter
# displayed candidates; they never fit, score or alter the official selection.
render_model_selection <- function(result) {
  # SVG coordinates and numeric data attributes require decimal points.
  withr::local_options(OutDec = ".")
  evidence <- tuning_evidence(result)
  opening <- paste0(
    '<section id="selection" class="workspace-page selection-section" ',
    'data-page="selection" aria-labelledby="selection-title"><h2 id="selection-title">Why these settings?</h2>'
  )
  if (!identical(evidence$status, "computed")) {
    return(selection_model_provenance(result, opening, evidence$reason))
  }
  decision <- evidence$selection
  higher <- selection_metric_direction(evidence$metric %||% "rmse") == "maximize"
  best_label <- if (higher) "Highest CV score" else "Lowest CV loss"
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
  complete_count <- sum(candidates$status == "ok")
  cards <- if (agreed) {
    settings <- paste(
      selection_family_label(final$family, evidence$task), "\u00b7",
      selection_short_parameters(final, evidence)
    )
    paste0(
      '<div class="selection-agreed"><strong>',
      html_escape(paste(report_number(final$cv_score, 4), pretty_metric(evidence$metric))), "</strong><span>",
      html_escape(settings), "</span>",
      if (complete_count == 1L) {
        if (!is.null(evidence$screening)) {
          paste0(
            "<small>Only one setting reached full cross-validation; its score did not establish superiority ",
            "over the screened-out settings.</small>"
          )
        } else {
          "<small>Only one setting received cross-validation; no alternative settings were compared.</small>"
        }
      } else {
        paste0("<small>Selected from ", complete_count, " settings that completed cross-validation.</small>")
      }, "</div>"
    )
  } else {
    paste0(
      '<div class="cards selection-decision">', card(best_label, best),
      card("Policy choice", picked), card(
        "Final primary fit", final,
        if (identical(final$configuration_id, picked$configuration_id)) "(same configuration)" else "(refit fallback)"
      ),
      "</div>"
    )
  }
  threshold <- if (identical(decision$rule, "one_se")) {
    paste0(
      if (higher) "Eligible score \u2265 " else "Eligible loss \u2264 ",
      report_number(decision$best_score, 6), if (higher) " - " else " + ",
      report_number(decision$best_se, 6), " = <strong>", report_number(decision$threshold, 6), "</strong>. ",
      "Among eligible candidates: first the preferred family, then its smallest recorded capacity proxy, ",
      "then score and configuration ID to break a tie."
    )
  } else {
    if (higher) {
      "Highest mean fold score wins. Tied maxima use the first scheduled configuration."
    } else {
      "Lowest pooled CV loss wins. Tied minima use the first scheduled configuration."
    }
  }
  reason <- if (agreed) {
    ""
  } else if (identical(decision$rule, "one_se")) {
    "The one-SE rule used the configured family preference to choose among settings close to the best CV score."
  } else {
    if (higher) {
      "The policy chose the highest mean training-fold score."
    } else {
      "The policy chose the lowest pooled training CV loss."
    }
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
      "<h3>", html_escape(selection_family_label(family, evidence$task)), "</h3>",
      selection_screening_table(rows, evidence),
      if (!is.null(evidence$screening)) "<h4>Cross-validation</h4>",
      selection_candidate_plot(rows, evidence), selection_family_rationale(family, evidence),
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
    '<p class="selection-context">Training-only cross-validation; ',
    if (higher) "higher scores are better. " else "lower loss is better. ",
    "These CV scores are not final performance estimates.</p>", cards,
    selection_forest_validation_note(evidence$input_policy$forest),
    if (nzchar(reason)) paste0('<p class="selection-reason">', html_escape(reason), "</p>"),
    selection_search_progress(evidence), selection_failure_summary(evidence), selection_family_summary(evidence),
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
    selection_screening_details(evidence), selection_input_policy(evidence),
    selection_search_details(evidence), selection_refit_details(evidence),
    '<details class="advanced"><summary>Repeat or change this search in R</summary>',
    if (!is.null(evidence$screening)) {
      "<p>This control repeats the screening policy using the original search budget and seed. "
    } else {
      "<p>This control repeats the scheduled tuples and budgets. "
    },
    "Use the same package and engine versions, data, preprocessing and validation design. ",
    "Time-limited searches can stop at different points on different machines. ",
    "Avoid repeatedly tuning against the evaluation partition.</p>",
    if (identical(result$tuning$fold_source, "supplied_vfold") && is.null(result$validation)) {
      paste0(
        "<p>This run used supplied folds. Bind the original fitted object as <code>original_result</code> ",
        "before running the code. Its raw-training fold assignments are read from that object; ",
        "they are not included in this HTML.</p>"
      )
    },
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
    if (identical(family, "forest")) return(selection_forest_policy(policy))
    if (identical(family, "threads")) return(selection_thread_policy(policy))
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

selection_thread_policy <- function(policy) {
  automatic <- identical(policy$requested, "auto")
  table <- data.frame(
    Choice = c("Requested CPU use", "Threads per forest or boosting fit"),
    Setting = c(if (automatic) "Automatic" else "Explicit thread count", as.character(policy$effective)),
    stringsAsFactors = FALSE
  )
  if (length(policy$available_cores) && is.finite(policy$available_cores)) {
    table <- rbind(table, data.frame(
      Choice = "Available CPU allocation", Setting = as.character(policy$available_cores)
    ))
  }
  paste0(
    "<h4>Native CPU use", explorer_help("How was the thread count chosen?", paste(
      "Automatic adaptive forest and boosting searches use up to four available cores when",
      "training rows multiplied by predictors reaches one million. Smaller and exact searches use one.",
      "CPU discovery accounts for process and job limits. An explicit thread count overrides the automatic choice.",
      "Folds and settings run sequentially. More threads need not make every model faster.",
      "Replay keeps the recorded count on another machine."
    ), id = "selection-thread-policy-help"), "</h4>",
    html_table(table, caption = "CPU allocation used by native forest and boosting fits")
  )
}

selection_forest_policy <- function(policy) {
  count <- function(value) format(value, trim = TRUE, scientific = FALSE, big.mark = ",")
  screening <- policy$screening_num_trees
  tree_budgets <- paste0(
    if (length(screening) && is.finite(screening)) paste0(screening, " screening; ") else "",
    policy$validation_num_trees %||% policy$num.trees, " per CV fit; ",
    policy$final_num_trees %||% policy$num.trees, " final"
  )
  table <- data.frame(
    Choice = c(
      "Training table", "Predictors tried per split", "Smallest split-node size",
      "Node-size growth", "Trees at each stage"
    ),
    Setting = c(
      paste(count(policy$rows), "rows;", count(policy$predictors), "predictors"),
      paste("Up to", policy$mtry_upper, "of", policy$predictors),
      as.character(policy$node_lower),
      if (isTRUE(policy$large_work)) {
        paste0("sqrt(max(1, rows / 50,000)) = ", report_number(policy$row_scale, 3))
      } else {
        "Unscaled below the size threshold"
      },
      tree_budgets
    ), stringsAsFactors = FALSE
  )
  paste0(
    "<h4>Forest search limits", explorer_help("Why these forest limits?", paste(
      "When rows multiplied by predictors reaches one million, automatic search bounds predictors tried per split",
      "by max(round(sqrt(p)), ceiling(p / 3)), where p counts predictors, and scales node sizes with training rows.",
      "Larger nodes reduce work but can miss fine structure. These limits apply to automatic settings;",
      "explicit grids can request more predictors or smaller nodes.",
      "Screening uses fewer trees and adjusts node-size growth to its smaller sample.",
      "Every CV fit uses all rows in its training partition. On large adaptive searches it uses fewer trees",
      "than the final model, as shown in the table. This approximates the final forest and can change predictions.",
      policy$validation_budget_reason %||% "",
      "Use grid search to validate each requested forest at its full tree count.",
      "The node control sets the minimum size needed to attempt a split, not a guaranteed leaf size."
    ), id = "selection-forest-policy-help"), "</h4>",
    html_table(table, caption = "Automatic forest settings planned from the outer-training table")
  )
}

selection_forest_validation_note <- function(policy) {
  if (!isTRUE(policy$validation_budget_active)) return("")
  paste0(
    '<p class="microcopy">Forest CV scores use ', policy$validation_num_trees,
    " trees per fit; final forests use ", policy$final_num_trees,
    ". All fold-training rows are used.</p>"
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
  if (identical(candidate$family, "linear")) return("No tuned controls")
  p <- selection_candidate_parameters(candidate, evidence)
  if (!length(p)) return(as.character(candidate$hyperparameters))
  text <- switch(candidate$family,
    linear = "No tuned controls",
    neural = paste(p$size, if (p$size == 1L) "unit; decay" else "units; decay", p$decay),
    tree = paste0("depth ", p$maxdepth, "; cp ", p$cp, "; split ", p$minsplit),
    regularized = paste0("alpha ", p$alpha, "; path ", p$path_fraction),
    additive = paste0("k ", p$k, "; gamma ", p$gamma, "; select ", p$select),
    neighbors = paste0("k ", p$k, "; ", p$kernel),
    kernel = paste0("cost ", p$cost, "; gamma \u00d7 ", p$gamma_multiplier),
    forest = paste0("mtry ", p$mtry, "; node ", p$min.node.size, "; ", p$num.trees, " trees"),
    boosting = paste0(
      selection_round_label(candidate, evidence, p$nrounds), "; eta ", p$eta, "; depth ", p$max_depth
    ),
    mars = paste0("degree ", p$degree, "; terms ", p$nprune),
    candidate$hyperparameters
  )
  as.character(text)
}

selection_candidate_parameters <- function(candidate, evidence) {
  if (is.list(candidate$parameters) && length(candidate$parameters) &&
        is.list(candidate$parameters[[1L]])) return(candidate$parameters[[1L]])
  index <- match(candidate$configuration_id, evidence$folds$configuration_id)
  if (!is.na(index)) return(evidence$folds$requested_parameters[[index]])
  scores <- evidence$screening$scores
  index <- match(candidate$configuration_id, scores$configuration_id)
  if (!is.na(index)) return(scores$full_parameters[[index]] %||% scores$requested_parameters[[index]])
  NULL
}

selection_round_label <- function(candidate, evidence, maximum) {
  attempts <- evidence$refit$attempts
  successful <- which(attempts$configuration_id == candidate$configuration_id & attempts$status == "ok")
  if (length(successful)) {
    effective <- attempts$effective_parameters[[successful[[1L]]]]$nrounds
    if (length(effective) && is.finite(effective) && effective != maximum) {
      return(paste0(effective, " rounds (cap ", maximum, ")"))
    }
  }
  folds <- evidence$folds
  records <- folds$effective_parameters[folds$configuration_id == candidate$configuration_id]
  rounds <- unlist(lapply(records, function(parameters) parameters$nrounds), use.names = FALSE)
  rounds <- rounds[is.finite(rounds)]
  if (length(rounds) && any(rounds != maximum)) {
    limits <- range(rounds)
    value <- if (diff(limits)) paste(limits, collapse = " to ") else as.character(limits[[1L]])
    return(paste0("CV ", value, " rounds (cap ", maximum, ")"))
  }
  paste0(maximum, if (!is.null(evidence$screening)) " round cap" else " rounds")
}

selection_candidate_status <- function(candidate) {
  if (candidate$status != "ok") return(selection_status_label(candidate$status))
  if (isTRUE(candidate$within_threshold)) "eligible" else "outside threshold"
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
      sum(candidates$status %in% c("failed", "screening_failed")),
      if (!is.null(evidence$screening)) paste0(
        '</td><td class="number">',
        sum(candidates$status %in% c("screened_out", "not_screened_time_limit", "not_validated_time_limit"))
      ),
      "</td></tr>"
    )
  }, character(1))
  paste0(
    '<div class="table-wrap selection-family-overview',
    if (!is.null(evidence$screening)) " selection-family-screened",
    '" tabindex="0" role="region" aria-label="Training CV across families">',
    "<table><caption>Training CV across families</caption>",
    '<thead><tr><th scope="col">Family</th><th scope="col">',
    if (selection_metric_direction(evidence$metric %||% "rmse") == "maximize") "Highest CV score" else "Lowest CV loss",
    "</th>",
    '<th scope="col">Within limit / successful ',
    explorer_help("Which settings could become the primary model?", paste(
      "The first count meets the primary-selection loss limit; the second completed cross-validation successfully.",
      "A successful setting outside that limit can still be retained as a family alternative."
    )), '</th><th scope="col">Failed</th>',
    if (!is.null(evidence$screening)) '<th scope="col">Not advanced</th>', "</tr></thead><tbody>",
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
  tuples <- lapply(seq_len(nrow(candidates)), function(index) {
    selection_candidate_parameters(candidates[index, , drop = FALSE], evidence)
  })
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
      if (is.null(evidence$screening)) {
        "Scheduled ranges include failed attempts; only"
      } else {
        "Scheduled ranges include settings that were not fully validated;"
      }, sum(candidates$status == "ok"),
      "of", nrow(candidates), "configurations completed every fold."
    )
  }
  paste0(
    '<details class="selection-rationale"><summary>Why these settings?</summary>',
    "<p>", html_escape(entry$rationale), " ", entry$scheduled, " of ", entry$available,
    " available tuples scheduled; ", entry$untested, " untested.</p>",
    if (entry$origin == "package_presets" && length(parameters)) {
      '<p class="microcopy">Preset values are package choices, not data-optimized ranges.</p>'
    } else {
      ""
    },
    if (nzchar(tested)) paste0('<p class="selection-tested"><strong>Tested:</strong> ', html_escape(tested), "</p>"),
    '<p class="selection-boundary">', html_escape(boundary), "</p></details>"
  )
}

selection_candidate_table <- function(candidates, evidence) {
  rows <- vapply(seq_len(nrow(candidates)), function(index) {
    row <- candidates[index, , drop = FALSE]
    target <- paste0("selection-detail-", report_anchor(row$configuration_id))
    family_best <- evidence$families$best_cv[match(row$family, evidence$families$family)]
    role <- c(
      if (isTRUE(row$selected)) "policy choice", if (isTRUE(row$final_fit)) "primary",
      if (isTRUE(row$lowest_cv)) {
        if (selection_metric_direction(evidence$metric %||% "rmse") == "maximize") "highest CV" else "lowest CV"
      } else {
        if (identical(row$configuration_id, family_best)) "family CV best"
      },
      if (!isTRUE(row$final_fit) && !is.na(row$retained_model_id %||% NA_character_)) "retained alternative"
    )
    paste0(
      '<tr><td><a href="#', target, '" data-selection-inspect="', target, '">',
      html_escape(selection_requested_tuple(row, evidence)),
      "</a><small>", html_escape(row$configuration_id),
      if (length(role)) paste0(" \u00b7 ", html_escape(paste(role, collapse = ", "))), "</small></td>",
      '<td class="number">', report_number(row$cv_score, 4), "</td><td>",
      html_escape(selection_candidate_status(row)),
      "</td></tr>"
    )
  }, character(1))
  paste0(
    '<div class="table-wrap selection-candidate-table"><table>',
    "<caption>Choose settings to inspect fold evidence</caption>",
    '<thead><tr><th scope="col">Actual requested settings</th><th scope="col">CV ',
    if (selection_metric_direction(evidence$metric %||% "rmse") == "maximize") "score" else "loss", "</th>",
    '<th scope="col">Selection status</th></tr></thead><tbody>', paste(rows, collapse = ""), "</tbody></table></div>"
  )
}

selection_candidate_plot <- function(candidates, evidence) {
  candidates <- candidates[candidates$configuration_id %in% evidence$folds$configuration_id, , drop = FALSE]
  if (!nrow(candidates)) {
    return("<p>No setting from this family reached cross-validation. Its search settings remain available below.</p>")
  }
  folds <- evidence$folds[evidence$folds$configuration_id %in% candidates$configuration_id, , drop = FALSE]
  higher <- selection_metric_direction(evidence$metric %||% "rmse") == "maximize"
  aggregate <- if (higher) "mean fold score" else "pooled CV loss"
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
        '" r="3"><title>Fold ', if (higher) "score" else "loss", ": ",
        html_escape(format(fold_scores, digits = 8)), "</title></circle>",
        collapse = ""
      )
    } else {
      ""
    }
    labels <- c(
      if (isTRUE(candidate$lowest_cv)) if (higher) "highest CV" else "lowest CV",
      if (isTRUE(candidate$selected)) "policy", if (isTRUE(candidate$final_fit)) "final"
    )
    status <- if (candidate$status != "ok") {
      selection_status_label(candidate$status)
    } else {
      paste(c(
        if (isTRUE(candidate$final_fit)) {
          "primary"
        } else if (isTRUE(candidate$selected)) {
          "policy choice"
        } else if (isTRUE(candidate$lowest_cv)) {
          if (higher) "highest CV" else "lowest CV"
        } else if (!is.na(candidate$retained_model_id %||% NA_character_)) {
          "retained"
        },
        if (isTRUE(candidate$within_threshold)) "eligible" else "outside limit"
      ), collapse = " \u00b7 ")
    }
    target <- paste0("selection-detail-", report_anchor(id))
    description <- paste(
      candidate$hyperparameters, ";", aggregate, report_number(candidate$cv_score, 6),
      ";", paste(labels, collapse = ", "), status, "; open fold details"
    )
    score <- if (is.finite(candidate$cv_score)) {
      paste0(
        '<circle class="selection-score-point', if (isTRUE(candidate$selected)) " selection-picked",
        '" data-value="', candidate$cv_score, '" cx="', x(candidate$cv_score), '" cy="', y, '" r="6"><title>',
        html_escape(paste(id, aggregate, format(candidate$cv_score, digits = 8))), "</title></circle>"
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
    '<span aria-hidden="true" class="selection-key-pooled">\u25cf</span> ',
    if (higher) "mean fold score" else "pooled CV", "</span>",
    '<span><span aria-hidden="true" class="selection-key-fold">\u25cb</span> fold</span>',
    '<span><span aria-hidden="true" class="selection-key-limit">\u2506</span> ',
    if (higher) "score" else "loss", " limit</span>",
    explorer_help("Reading the candidate comparison", paste(
      if (higher) {
        "Filled dots show mean fold scores; open dots show each fold. The dashed line marks the allowed score."
      } else {
        "Filled dots show pooled CV loss; open dots show each fold. The dashed line marks the allowed loss."
      },
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
    '" text-anchor="middle">', html_escape(pretty_metric(evidence$metric)),
    if (higher) " (higher is better)" else " (lower is better)", "</text></svg></div>"
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
  round_records <- lapply(folds$learned, function(learned) learned$round_selection %||% NULL)
  has_rounds <- length(round_records) && any(vapply(round_records, is.list, logical(1)))
  forest <- identical(candidate$family, "forest") && nrow(folds) > 0L
  if (forest) {
    table[c("fit_seed", "optimization_status")] <- NULL
    table$trees <- vapply(folds$effective_parameters, function(parameters) {
      as.integer(parameters$num.trees %||% NA_integer_)
    }, integer(1))
  }
  if (has_rounds) {
    table[c("fit_seed", "optimization_status")] <- NULL
    table$chosen_rounds <- vapply(folds$effective_parameters, function(parameters) {
      as.integer(parameters$nrounds %||% NA_integer_)
    }, integer(1))
    table$tried_rounds <- vapply(round_records, function(record) {
      as.integer(record$attempted_rounds %||% NA_integer_)
    }, integer(1))
    table$inner_rows <- vapply(round_records, function(record) {
      if (is.null(record$training_rows)) return("Not available")
      paste(record$training_rows, record$validation_rows, sep = " / ")
    }, character(1))
    table$stopping <- vapply(round_records, selection_stopping_reason, character(1))
  }
  # Put the fitted tree budget beside the score, so it remains visible before
  # scrolling a wide fold record on a narrow screen.
  leading <- intersect(c("fold", "score", "trees", "chosen_rounds", "tried_rounds"), names(table))
  table <- table[c(leading, setdiff(names(table), leading))]
  labels <- c(
    fold = "Fold",
    score = if (selection_metric_direction(evidence$metric %||% "rmse") == "maximize") "CV score" else "CV loss",
    training_rows = "Training rows", validation_rows = "Validation rows",
    validation_rows_omitted = "Rows omitted", fit_seed = "Seed", optimization_status = "Optimizer",
    warning = "Warnings", error = "Error", chosen_rounds = "Chosen rounds", tried_rounds = "Tried rounds",
    inner_rows = "Inner train / monitor", stopping = "Stopped because", trees = "Trees"
  )
  names(table) <- unname(labels[names(table)])
  issues <- unique(c(folds$error, folds$warning))
  issues <- issues[!is.na(issues) & nzchar(issues)]
  paste0(
    '<details id="selection-detail-', report_anchor(id),
    '" class="advanced selection-candidate" data-selection-family="', html_escape(candidate$family),
    '"><summary>', html_escape(id), " \u00b7 ", html_escape(selection_status_label(candidate$status)),
    if (nrow(folds)) paste0(
      " \u00b7 CV ", report_number(candidate$cv_score, 6), " \u00b7 fold SE ", report_number(candidate$cv_se, 6)
    ),
    if (isTRUE(candidate$within_threshold)) " \u00b7 eligible" else "", "</summary>",
    '<p class="selection-settings"><strong>Search settings:</strong> ',
    html_escape(selection_requested_tuple(candidate, evidence)), "</p>",
    selection_round_choice_note(candidate, evidence),
    if (length(issues)) paste0('<p class="callout">', html_escape(paste(issues, collapse = " ")), "</p>"),
    if (no_omissions) "<p>No validation rows were omitted in these folds.</p>",
    if (nrow(folds)) {
      html_table(table, digits = 6L, caption = paste(
        id, if (has_rounds) "fold scores and stopping choices" else if (forest) {
          "fold scores, rows and tree counts"
        } else {
          "fold scores, sizes, seeds and engine status"
        }
      ))
    } else {
      "<p>This configuration did not receive complete cross-validation and could not become the primary model.</p>"
    },
    if (has_rounds) selection_stopping_chart(
      stats::setNames(round_records, paste("Fold", folds$fold)), evidence$metric,
      paste0("selection-rounds-", report_anchor(id))
    ),
    selection_fold_technical_details(folds), selection_candidate_screening(candidate, evidence), "</details>"
  )
}

selection_status_label <- function(status) {
  status <- ifelse(status == "ok", "completed", gsub("_", " ", status, fixed = TRUE))
  ifelse(is.na(status), NA_character_, paste0(toupper(substr(status, 1L, 1L)), substring(status, 2L)))
}

selection_search_progress <- function(evidence) {
  screening <- evidence$screening
  if (is.null(screening)) return("")
  promoted <- sum(screening$promotion$promoted)
  complete <- sum(evidence$candidates$status == "ok")
  paste0(
    '<p class="selection-context"><strong>', nrow(screening$scores), " screened</strong> \u00b7 ",
    promoted, " advanced \u00b7 ", complete, " completed cross-validation",
    explorer_help("How were settings screened?", paste(
      "All settings use the same smaller training and assessment sample, within the outer training data.",
      "The best successful settings within each family advance to complete cross-validation.",
      "Screening scores and full CV scores use different amounts of data and must not be compared directly."
    ), id = "selection-screening-help"), "</p>"
  )
}

selection_candidate_screening <- function(candidate, evidence) {
  scores <- evidence$screening$scores
  if (is.null(scores)) return("")
  index <- match(candidate$configuration_id, scores$configuration_id)
  if (is.na(index)) return("<p>No screening fit was started for this configuration.</p>")
  row <- scores[index, , drop = FALSE]
  promotion <- evidence$screening$promotion
  reason <- promotion$reason[match(candidate$configuration_id, promotion$configuration_id)]
  row_names <- c(score = "Screening score", training_rows = "Training rows", validation_rows = "Assessment rows")
  table <- row[intersect(names(row_names), names(row))]
  names(table) <- unname(row_names[names(table)])
  learned <- row$learned[[1L]] %||% list()
  issue <- unique(c(row$error, row$warning))
  issue <- issue[!is.na(issue) & nzchar(issue)]
  paste0(
    html_table(table, digits = 6L, caption = paste(candidate$configuration_id, "screening result")),
    if (length(reason) && !is.na(reason)) paste0("<p>", html_escape(reason), "</p>"),
    if (length(issue)) paste0('<p class="callout">', html_escape(paste(issue, collapse = " ")), "</p>"),
    '<details class="advanced"><summary>Screening settings and stopping trace</summary>',
    selection_stopping_chart(
      list(Screening = learned$round_selection), evidence$metric,
      paste0("selection-screen-", report_anchor(candidate$configuration_id))
    ),
    html_table(selection_technical_rows(list(
      requested = row$requested_parameters[[1L]], effective = row$effective_parameters[[1L]],
      seed = row$fit_seed[[1L]], learned = learned
    )), caption = "Screening technical record"), "</details>"
  )
}

selection_screening_details <- function(evidence) {
  screening <- evidence$screening
  resources <- evidence$resources
  if (is.null(screening) && is.null(resources)) return("")
  score_table <- if (!is.null(screening)) {
    scores <- screening$scores
    ids <- match(scores$configuration_id, evidence$candidates$configuration_id)
    candidates <- evidence$candidates[ids, , drop = FALSE]
    data.frame(
      Configuration = scores$configuration_id,
      Family = vapply(candidates$family, selection_family_label, character(1), task = evidence$task),
      `Screening score` = scores$score,
      Outcome = vapply(seq_len(nrow(candidates)), function(index) {
        row <- candidates[index, , drop = FALSE]
        if (row$status == "ok") "Completed CV" else selection_status_label(row$status)
      }, character(1)),
      check.names = FALSE
    )
  } else {
    NULL
  }
  resource_table <- if (length(resources)) {
    values <- resources[vapply(resources, function(value) is.atomic(value) && length(value) == 1L, logical(1))]
    values$limitation <- NULL
    labels <- c(
      threads = "Threads per native fit", search_time_limit = "Search scheduling limit (s)",
      search_elapsed_seconds = "Search elapsed (s)", scheduling_limit_reached = "Scheduling limit reached",
      screening_fits = "Screening evaluations", validation_fits = "CV evaluations",
      calibration_fit_attempts = "Stopping pilot attempts", model_fit_attempts = "Screening/CV model fit attempts",
      refit_attempts = "Final refit attempts", baseline_fit_attempts = "Baseline fit attempts",
      total_backend_fit_attempts = "Total backend fit attempts"
    )
    names <- ifelse(names(values) %in% names(labels), labels[names(values)], selection_status_label(names(values)))
    data.frame(
      Measure = names,
      Value = vapply(values, model_spec_value, character(1)), check.names = FALSE
    )
  } else {
    NULL
  }
  paste0(
    '<details class="advanced"><summary>Screening results and compute budget</summary>',
    if (!is.null(screening)) paste0(
      "<p>", screening$partition$training_rows, " training rows and ",
      screening$partition$validation_rows, " assessment rows in the common screen. ",
      "Trees and boosting rounds are reduced during screening; requested final settings are kept separately.</p>",
      html_table(score_table, digits = 6L, caption = "Screening scores; use candidate details for exact fit settings"),
      "<p>", html_escape(screening$fit_scope), "</p>",
      if (evidence$task != "regression") paste0(
        "<p>Sampling preserves classes. ",
        if (identical(evidence$metric, "roc_auc")) {
          "AUC compares positive against negative observations within the sampled assessment set. "
        } else {
          "Losses are weighted to match the assessment pool's class frequencies. "
        },
        "The smaller training sample can have a different class mixture.</p>"
      )
    ),
    if (!is.null(resource_table)) html_table(resource_table, caption = "Recorded search work"),
    if (length(resources$limitation)) paste0("<p>", html_escape(resources$limitation), "</p>"),
    "</details>"
  )
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
  adaptive <- identical(tuning$control$search, "adaptive") || !is.null(tuning$screening)
  settings <- if (adaptive) {
    list(
      search = "adaptive", screening_rows = tuning$control$screening_rows,
      finalists_per_family = tuning$control$finalists_per_family
    )
  } else {
    grids <- stats::setNames(lapply(families, function(family) {
      unclass(tuning$plan$parameters[tuning$plan$family == family])
    }), families)
    list(search = "grid", grids = grids, family_budgets = stats::setNames(vapply(grids, length, integer(1)), families))
  }
  r_value <- function(value) paste(trimws(utils::capture.output(dput(value)), which = "right"), collapse = "\n")
  inputs <- selection_replay_inputs(result, r_value)
  metric <- switch(tuning$metric, brier_score = "brier", roc_auc = "auc", tuning$metric)
  settings <- c(settings, list(
    metric = metric,
    threads = tuning$control[["threads", exact = TRUE]] %||% 1L,
    early_stopping = tuning$control$early_stopping %||% FALSE,
    patience = tuning$control$patience %||% 30L,
    time_limit = tuning$control$time_limit,
    retain_oof = tuning$control$retain_oof %||% TRUE,
    failure_policy = tuning$control$failure_policy %||% "continue",
    optimization_policy = tuning$control$optimization_policy %||% "exclude",
    family_priority = tuning[["selection", exact = TRUE]]$family_priority$family %||% NULL
  ))
  if (!is.null(inputs$fold_ids)) settings$fold_ids <- inputs$fold_ids
  arguments <- paste(vapply(names(settings), function(name) {
    paste0("  ", name, " = ", r_value(settings[[name]]))
  }, character(1)), collapse = ",\n")
  paste0(
    inputs$code,
    "control <- tuning_control(\n", arguments, "\n)\n",
    "# In your original autoxplain(...) call, use:\n",
    "#   learners = ", r_value(families), ", max_models = ",
    if (adaptive) r_value(tuning$configurations_requested) else "NULL", ", tuning_control = control\n",
    "#   tuning_rule = ", r_value(tuning$selection_rule), ", seed = ", result$provenance$seed, "\n",
    "#   nfolds = ", r_value(tuning$folds_requested), "\n",
    inputs$call_notes,
    if (adaptive) "# Adaptive settings depend on package/engine versions and the original data.\n",
    "# Use the original raw input data and preprocessing, not result$training_data."
  )
}

selection_replay_inputs <- function(result, r_value) {
  design <- result$validation
  if (identical(design$method, "group")) {
    return(list(
      code = paste0("replay_validation <- validation_split(group = ", r_value(design$column), ")\n"),
      fold_ids = NULL,
      call_notes = paste0(
        "#   validation = replay_validation, test_fraction = ", r_value(design$fraction), "\n",
        "# Keep the group column in the full original input data; omit test_data.\n"
      )
    ))
  }
  if (!identical(result$tuning$fold_source, "supplied_vfold")) {
    return(list(code = "", fold_ids = NULL, call_notes = "# Keep the original test_data or holdout split.\n"))
  }
  rows <- nrow(result$tuning$fold_assignment)
  list(
    code = paste0(
      "# Bind the original fitted object first, for example: original_result <- result\n",
      "# The report deliberately omits row-level fold assignments.\n",
      "if (!exists(\"original_result\") || !inherits(original_result, \"autoxplain_result\") ||\n",
      "    !identical(original_result$tuning$fold_source, \"supplied_vfold\")) {\n",
      "  stop(\"This search used supplied folds. Set original_result to the original fitted object.\")\n",
      "}\n",
      "original_fold_assignment <- original_result$tuning$fold_assignment\n",
      "if (!is.data.frame(original_fold_assignment) ||\n",
      "    !all(c(\"training_row\", \"fold_label\") %in% names(original_fold_assignment)) ||\n",
      "    nrow(original_fold_assignment) != ", r_value(rows), " ||\n",
      "    anyNA(original_fold_assignment$fold_label) ||\n",
      "    anyDuplicated(original_fold_assignment$training_row) ||\n",
      "    !setequal(original_fold_assignment$training_row, seq_len(", r_value(rows), "))) {\n",
      "  stop(\"The original raw-training fold assignment is missing or incompatible with this report.\")\n",
      "}\n",
      "replay_fold_ids <- original_fold_assignment$fold_label[order(original_fold_assignment$training_row)]\n"
    ),
    fold_ids = quote(replay_fold_ids),
    call_notes = "# Keep the same raw training rows in the same order and the same explicit test_data.\n"
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
