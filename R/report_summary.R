prepare_report_uncertainty <- function(result, uncertainty = "auto") {
  if (is.logical(uncertainty) && length(uncertainty) == 1L && !is.na(uncertainty)) {
    mode <- if (uncertainty) "required" else "none"
  } else if (identical(uncertainty, "auto")) {
    mode <- "auto"
  } else {
    stop("`uncertainty` must be TRUE, FALSE, or \"auto\".", call. = FALSE)
  }
  result$performance_uncertainty <- NULL
  if (mode == "none") {
    result$.report_uncertainty <- list(status = "not_run", reason = "Paired intervals were disabled for this report.")
    return(result)
  }
  reused_for_selection <- isTRUE(result$provenance$test_used_for_validation)
  if (mode == "auto" && reused_for_selection) {
    result$.report_uncertainty <- list(status = "unavailable", reason = paste(
      "These evaluation rows were reused for model selection or stopping.",
      "The automatic fixed-model bootstrap would not account for that reuse."
    ))
    return(result)
  }
  value <- if (mode == "required") {
    performance_uncertainty(result)
  } else {
    tryCatch(performance_uncertainty(result), error = function(error) error)
  }
  if (inherits(value, "error")) {
    result$.report_uncertainty <- list(status = "unavailable", reason = conditionMessage(value))
  } else {
    if (reused_for_selection) {
      value$notes <- c(value$notes, paste(
        "These evaluation rows were reused for model selection or stopping;",
        "this descriptive interval does not account for that reuse."
      ))
    }
    result$performance_uncertainty <- value
    result$.report_uncertainty <- list(status = "computed", reason = "")
  }
  result
}

explorer_baseline_comparison <- function(result, models) {
  board <- models$table
  primary <- match(result$provenance$primary_model_id, board$model_id)
  reference_id <- result_reference_id(result)
  baseline <- match(reference_id, board$model_id)
  metric <- result$evaluation$primary_metric
  if (length(baseline) != 1L || anyNA(c(primary, baseline)) || !metric %in% names(board)) {
    return("")
  }
  difference <- board[[metric]][primary] - board[[metric]][baseline]
  value <- result$performance_uncertainty
  unit <- if (result$task == "regression") result$provenance$target_units else NULL
  label <- paste(pretty_metric(metric), unit)
  interval <- if (!is.null(value)) {
    estimate <- value$estimates[value$estimates$quantity == "difference", , drop = FALSE]
    paste0(
      format_percent(value$confidence), " paired interval: ",
      report_number(estimate$lower), " to ", report_number(estimate$upper),
      "; ", value$units, " independent ", value$unit, if (value$units != 1L) "s" else "", " assumed."
    )
  } else {
    paste0("No paired interval: ", result$.report_uncertainty$reason %||% "Not computed.")
  }
  important <- if (!is.null(value)) {
    value$notes[grepl("Fewer than|degenerate|reused", value$notes)]
  } else {
    character()
  }
  primary_role <- if (isTRUE(result$tuning$refit$fallback_used)) {
    "Refit fallback primary"
  } else if (inherits(result$tuning, "autoxplain_tuning")) {
    "Training-selected primary"
  } else {
    "Primary model"
  }
  paste0(
    '<div class="baseline-comparison" aria-label="Primary model compared with reference">',
    "<dl><div><dt>", primary_role,
    "</dt><dd>", html_escape(explorer_label(result, board$model_id[primary])),
    " <strong>", report_number(board[[metric]][primary]), "</strong><span>", html_escape(label), "</span></dd></div>",
    "<div><dt>", html_escape(explorer_label(result, reference_id)), "</dt><dd><strong>",
    report_number(board[[metric]][baseline]),
    "</strong><span>", html_escape(label), "</span></dd></div>",
    "<div><dt>Primary minus reference</dt><dd><strong>", if (difference > 0) "+" else "",
    report_number(difference), "</strong><span>",
    if (metric %in% higher_is_better_metrics()) "Positive favors the model" else "Negative favors the model",
    "</span></dd></div></dl>",
    '<p class="baseline-interval print-help">', html_escape(interval),
    if (!is.null(value)) {
      explorer_help("Scope of the paired interval", paste(
        "Evaluation-sample uncertainty for these fixed models; fitting and selection uncertainty are excluded.",
        "The interval compares the retained primary model with the reference; it does not rank alternatives."
      ))
    },
    "</p>",
    if (length(important)) {
      paste0(
        '<p class="baseline-caution">',
        html_escape(paste(important, collapse = " ")), "</p>"
      )
    },
    "</div>"
  )
}
