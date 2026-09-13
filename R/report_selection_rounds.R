# Summarize and plot retained inner-monitoring traces for the selection inspector.
selection_stopping_reason <- function(record) {
  if (is.null(record)) return("No round calibration recorded")
  if (identical(record$stop_reason, "patience_reached")) {
    return(paste("No improvement for", record$patience, "rounds"))
  }
  if (identical(record$stop_reason, "round_limit")) return("Reached the round cap")
  if (identical(record$status, "skipped")) return(paste("Calibration skipped:", record$stop_reason))
  record$stop_reason %||% "Not recorded"
}

selection_round_choice_note <- function(candidate, evidence) {
  if (candidate$family != "boosting") return("")
  attempts <- evidence$refit$attempts
  index <- which(attempts$configuration_id == candidate$configuration_id & attempts$status == "ok")
  if (!length(index)) return("")
  record <- attempts$learned[[index[[1L]]]]$round_selection
  if (!identical(record$status, "fold_aggregate")) return("")
  paste0(
    "<p><strong>Full-training fit: ", record$selected_rounds, " rounds.</strong> The rounded-up median of ",
    length(record$fold_rounds), " fold choices (", paste(record$fold_rounds, collapse = ", "),
    "). ",
    if (record$calibrated_folds == length(record$fold_rounds)) {
      "Each fold chose its rounds on a separate inner monitoring split."
    } else {
      paste(record$calibrated_folds, "folds used inner monitoring; the others kept the round cap.")
    }, "</p>"
  )
}

selection_stopping_chart <- function(records, metric, id) {
  withr::local_options(OutDec = ".")
  valid <- vapply(records, function(record) {
    curve <- record$curve
    is.data.frame(curve) && all(c("round", "score") %in% names(curve)) && nrow(curve) > 0L &&
      all(is.finite(curve$round)) && all(is.finite(curve$score))
  }, logical(1))
  records <- records[valid]
  if (!length(records)) return("")
  rounds <- unlist(lapply(records, function(record) record$curve$round), use.names = FALSE)
  scores <- unlist(lapply(records, function(record) record$curve$score), use.names = FALSE)
  x_limits <- c(1, max(2, rounds))
  y_limits <- range(scores)
  pad <- max(diff(y_limits) * .08, .001)
  y_limits <- y_limits + c(-pad, pad)
  x <- function(value) 65 + (value - x_limits[[1L]]) / diff(x_limits) * 630
  y <- function(value) 165 - (value - y_limits[[1L]]) / diff(y_limits) * 145
  y_ticks <- pretty(y_limits, 4L)
  y_ticks <- y_ticks[y_ticks >= y_limits[[1L]] & y_ticks <= y_limits[[2L]]]
  x_ticks <- pretty(x_limits, 5L)
  x_ticks <- x_ticks[x_ticks >= x_limits[[1L]] & x_ticks <= x_limits[[2L]] & x_ticks == floor(x_ticks)]
  grid <- paste(vapply(y_ticks, function(value) {
    paste0(
      '<line x1="65" x2="695" y1="', y(value), '" y2="', y(value),
      '" class="grid-line selection-round-grid" data-value="', value, '"/>',
      '<text x="55" y="', y(value) + 4,
      '" class="selection-round-y-tick" data-value="', value,
      '" text-anchor="end" font-size="12" fill="#31544d">',
      html_escape(report_axis_number(value)), "</text>"
    )
  }, character(1)), collapse = "")
  ticks <- paste(vapply(x_ticks, function(value) {
    paste0(
      '<text x="', x(value), '" y="184" class="selection-round-x-tick" data-value="', value,
      '" text-anchor="middle" font-size="12" fill="#31544d">', value, "</text>"
    )
  }, character(1)), collapse = "")
  colors <- c("#176b55", "#4b6485", "#a45424", "#795495", "#527323")
  lines <- legend <- character(length(records))
  for (index in seq_along(records)) {
    record <- records[[index]]
    curve <- record$curve
    label <- names(records)[[index]]
    color <- colors[[(index - 1L) %% length(colors) + 1L]]
    path <- paste0("M", paste(paste(round(x(curve$round), 2), round(y(curve$score), 2)), collapse = " L"))
    selected <- match(record$selected_rounds, curve$round)
    marker <- if (!is.na(selected)) {
      paste0(
        '<circle data-selected-round="', record$selected_rounds, '" data-selected-score="', curve$score[[selected]],
        '" cx="', x(record$selected_rounds), '" cy="', y(curve$score[[selected]]),
        '" r="4.5" fill="', color, '" stroke="white" stroke-width="1.5"><title>',
        html_escape(paste(
          label, "selected round", record$selected_rounds,
          pretty_metric(metric), report_number(curve$score[[selected]], 6)
        )), "</title></circle>"
      )
    } else {
      ""
    }
    lines[[index]] <- paste0(
      '<path d="', path, '" class="selection-round-curve" data-rounds="', paste(curve$round, collapse = ","),
      '" data-scores="', paste(sprintf("%.17g", curve$score), collapse = ","),
      '" fill="none" stroke="', color, '" stroke-width="1.8"><title>',
      html_escape(paste(label, "inner monitoring scores")), "</title></path>", marker
    )
    legend[[index]] <- paste0(
      '<span style="display:inline-block;margin-right:1rem"><span aria-hidden="true" style="color:', color,
      '">\u25cf</span> ', html_escape(label), " \u00b7 round ", record$selected_rounds, "</span>"
    )
  }
  higher <- selection_metric_direction(metric) == "maximize"
  paste0(
    '<div class="selection-round-chart"><h4>Choosing the boosting rounds',
    explorer_help("Which data chose the stopping round?", paste(
      "Each line shows the inner monitoring score as trees were added. Dots mark the chosen rounds.",
      "The outer CV assessment rows did not choose these points. Only rounds actually tried are plotted;",
      "the maximum search cap can be much larger. Full numeric curves remain in tuning_results(result)."
    ), id = paste0(id, "-help")), "</h4>",
    '<svg viewBox="0 0 720 215" class="selection-round-plot"',
    ' data-round-min="', x_limits[[1L]], '" data-round-max="', x_limits[[2L]],
    '" data-score-min="', sprintf("%.17g", y_limits[[1L]]),
    '" data-score-max="', sprintf("%.17g", y_limits[[2L]]),
    '" style="width:100%;max-width:900px;display:block" role="img" aria-label="',
    html_escape(paste("Inner", pretty_metric(metric), "by boosting round; dots mark selected rounds")), '">',
    "<title>Inner monitoring scores and chosen rounds</title>",
    '<text x="65" y="12" class="selection-round-title" font-size="12" fill="#31544d">Inner ',
    html_escape(pretty_metric(metric)),
    if (higher) " (higher is better)" else " (lower is better)", "</text>",
    grid, ticks, paste(lines, collapse = ""),
    '<text x="380" y="209" class="selection-round-axis" text-anchor="middle" font-size="12"',
    ' fill="#31544d">Boosting round</text></svg>',
    '<p class="microcopy">', paste(legend, collapse = ""), "</p></div>"
  )
}

selection_technical_rows <- function(record, prefix = "") {
  if (!is.list(record) || !length(record)) {
    return(data.frame(Setting = prefix, Value = selection_exact_parameter_value(record), stringsAsFactors = FALSE))
  }
  fields <- names(record) %||% as.character(seq_along(record))
  rows <- lapply(seq_along(record), function(index) {
    field <- fields[[index]]
    if (field %in% c("curve", "call_reconstruction")) return(NULL)
    label <- selection_status_label(field)
    path <- if (nzchar(prefix)) paste(prefix, label, sep = " / ") else label
    selection_technical_rows(record[[index]], path)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows)) do.call(rbind, rows) else data.frame(Setting = character(), Value = character())
}

selection_fold_technical_details <- function(folds) {
  if (!nrow(folds)) return("")
  tables <- vapply(seq_len(nrow(folds)), function(index) {
    record <- list(
      requested = folds$requested_parameters[[index]], effective = folds$effective_parameters[[index]],
      seed = folds$fit_seed[[index]], optimizer = folds$optimization_status[[index]],
      learned = folds$learned[[index]]
    )
    html_table(selection_technical_rows(record), caption = paste("Fold", folds$fold[[index]], "technical record"))
  }, character(1))
  paste0(
    '<details class="advanced"><summary>Exact fold settings, seeds and training records</summary>',
    paste(tables, collapse = ""), "</details>"
  )
}
