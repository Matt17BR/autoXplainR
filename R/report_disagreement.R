# Aggregate comparisons are safe in every export mode. Source records are only
# linked when their processed positions are in the explicitly exported sample.
report_disagreement_view <- function(result) {
  ids <- setdiff(names(result$models), result_reference_id(result))
  if (length(ids) < 2L) {
    return(NULL)
  }
  record <- report_get_diagnostic(result, "model_behavior", function() compare_model_behavior(result))
  if (record$status != "computed") {
    return(list(status = record$status, reason = record$reason))
  }
  behavior <- record$evidence
  pairs <- behavior$prediction_pairs
  pairs$model_a_label <- vapply(pairs$model_a, function(id) explorer_label(result, id), character(1))
  pairs$model_b_label <- vapply(pairs$model_b, function(id) explorer_label(result, id), character(1))
  performance <- behavior$models[, c("model_id", "performance_score"), drop = FALSE]
  performance$model <- vapply(performance$model_id, function(id) explorer_label(result, id), character(1))
  cases <- NULL
  if (identical(result$.report_export$mode, "rows")) {
    exported <- Filter(function(row) {
      identical(row$partition, "evaluation") && isTRUE(row$retained) &&
        length(row$processed_position) == 1L && !is.na(row$processed_position)
    }, result$.report_export$rows %||% list())
    cases <- lapply(exported, function(row) {
      index <- match(row$processed_position, behavior$ambiguity$rows$evaluation_row)
      if (is.na(index)) stop("Disagreement records do not match exported evaluation positions.", call. = FALSE)
      evidence <- behavior$ambiguity$rows[index, , drop = FALSE]
      list(
        row_key = row$row_key, source = row$source, source_row = row$source_row,
        gap = if (result$task == "regression") evidence$prediction_range else evidence$probability_distance
      )
    })
    if (length(cases)) cases <- cases[order(-vapply(cases, `[[`, numeric(1), "gap"))]
  }
  list(
    status = "computed", task = result$task, n = behavior$n_evaluation_rows,
    metric = behavior$performance_metric, performance = performance,
    pairs = pairs, cases = cases, distance = behavior$distance_definition
  )
}

explorer_disagreement <- function(result) {
  view <- report_disagreement_view(result)
  if (is.null(view)) {
    return("")
  }
  if (view$status != "computed") {
    return(render_diagnostic_state("Model disagreement", view$status, view$reason))
  }
  pairs <- view$pairs
  display <- data.frame(
    `First model` = pairs$model_a_label, `Second model` = pairs$model_b_label,
    `Mean absolute distance` = pairs$mean_prediction_distance,
    `90th percentile distance` = pairs$p90_prediction_distance, check.names = FALSE
  )
  if (view$task != "regression") {
    display[["Different predicted classes"]] <- sprintf("%.1f%%", pairs$class_disagreement_rate * 100)
  }
  performance <- view$performance[, c("model", "performance_score")]
  names(performance) <- c("Included model", pretty_metric(view$metric))
  cases <- head(view$cases, 5L)
  paste0(
    '<details class="prediction-disagreement"><summary>Do the models disagree?</summary>',
    "<p>", view$n, " evaluation rows \u00b7 ", nrow(view$performance), " models",
    if (!is.null(result_reference_id(result))) " \u00b7 declared reference excluded", ". ",
    explorer_help("Interpreting model disagreement", paste(
      "These are descriptive differences among the included models, not confidence intervals.",
      "A weak model can disagree with a strong one; compare their scores before treating a gap as useful evidence.",
      if (view$task == "binary") "Class decisions here use the recorded event class and a fixed 0.50 cutoff."
    )), "</p>",
    html_table(display, digits = 4L, caption = paste("Pairwise", paste(view$distance, collapse = "; "))),
    html_table(performance, digits = 4L, caption = "Evaluation scores of the models being compared"),
    if (length(cases)) {
      paste0(
        '<p>Largest gaps among exported evaluation records:</p><ul class="disagreement-records">',
        paste(vapply(cases, function(row) {
          paste0(
            '<li><a href="#data" data-navigate data-select-row="', html_escape(row$row_key), '">',
            html_escape(paste(row$source, row$source_row)), "</a> \u00b7 ", report_axis_number(row$gap),
            if (view$task == "regression") " target units" else " probability distance", "</li>"
          )
        }, character(1)), collapse = ""), "</ul>"
      )
    },
    '<details><summary>Inspect or restrict this comparison in R</summary><pre tabindex="0"><code>',
    "prediction_ambiguity(result)\ncompare_model_behavior(result)\n",
    "# Keep models within a chosen relative evaluation-score gap:\n",
    "prediction_ambiguity(result, performance_tolerance = 0.1)",
    "</code></pre><p>The tolerance filters this descriptive comparison; it does not select a new primary model.</p>",
    "</details></details>"
  )
}
