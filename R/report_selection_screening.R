selection_requested_tuple <- function(candidate, evidence) {
  if (identical(candidate$family, "linear")) return("No tuned controls")
  parameters <- selection_candidate_parameters(candidate, evidence)
  if (is.null(parameters)) return("Full requested settings not recorded")
  if (!length(parameters)) return("No tuned controls")
  paste(paste(names(parameters), vapply(parameters, selection_exact_parameter_value, character(1)), sep = " = "),
    collapse = "; "
  )
}

selection_exact_parameter_value <- function(value) {
  model_spec_exact_value(value)
}

selection_screening_rows <- function(candidates, evidence) {
  screening <- evidence$screening
  scores <- screening$scores
  ids <- candidates$configuration_id
  matched <- match(ids, scores$configuration_id)
  present <- !is.na(matched)
  score <- rep(NA_real_, length(ids))
  score[present] <- scores$score[matched[present]]
  successful <- present & is.finite(score)
  failed <- rep(FALSE, length(ids))
  if ("error" %in% names(scores)) {
    errors <- scores$error[matched]
    failed <- failed | (present & !is.na(errors) & nzchar(errors))
    successful <- successful & !is.na(errors) & !nzchar(errors)
  }
  if ("status" %in% names(scores)) {
    statuses <- scores$status[matched]
    complete <- statuses %in% c("ok", "success", "completed")
    failed <- failed | (present & !is.na(statuses) & !complete)
    successful <- successful & complete
  }
  promotion <- screening$promotion
  promoted <- promotion$promoted[match(ids, promotion$configuration_id)]
  status <- vapply(seq_along(ids), function(index) {
    if (!present[[index]]) return("Not attempted")
    if (failed[[index]]) return("Failed")
    if (!successful[[index]]) return("No valid score")
    if (length(promoted) < index || is.na(promoted[[index]])) return("Promotion not recorded")
    if (promoted[[index]]) "Selected for CV" else "Not advanced"
  }, character(1))
  rows <- data.frame(configuration_id = ids, candidate_index = seq_along(ids), score = score,
    successful = successful, status = status, stringsAsFactors = FALSE
  )
  metric <- screening$metric %||% evidence$metric
  rows[order(!successful, ifelse(successful, selection_metric_loss(score, metric), Inf), ids), , drop = FALSE]
}

selection_screening_preview <- function(candidate, evidence) {
  parameters <- selection_candidate_parameters(candidate, evidence)
  if (!length(parameters)) return(selection_short_parameters(candidate, evidence))
  if (candidate$family == "boosting") {
    return(paste0(
      "depth ", parameters$max_depth, "; eta ", parameters$eta,
      "; child ", parameters$min_child_weight, "; lambda ", parameters$reg_lambda,
      if (isTRUE(parameters$reg_alpha != 0)) paste0("; alpha ", parameters$reg_alpha)
    ))
  }
  if (candidate$family == "forest") {
    return(paste0(
      "mtry ", parameters$mtry, "; node ", parameters$min.node.size,
      "; sample ", parameters$sample.fraction,
      if (!identical(parameters$splitrule, "default")) paste0("; ", parameters$splitrule)
    ))
  }
  selection_short_parameters(candidate, evidence)
}

selection_screening_table <- function(candidates, evidence) {
  if (is.null(evidence$screening) || !nrow(candidates)) return("")
  ranked <- selection_screening_rows(candidates, evidence)
  metric <- evidence$screening$metric %||% evidence$metric
  higher <- selection_metric_direction(metric) == "maximize"
  rows <- vapply(seq_len(nrow(ranked)), function(index) {
    record <- ranked[index, , drop = FALSE]
    candidate <- candidates[record$candidate_index, , drop = FALSE]
    target <- paste0("selection-detail-", report_anchor(record$configuration_id))
    paste0(
      '<tr><td><a href="#', target, '" data-selection-inspect="', target, '">',
      html_escape(selection_screening_preview(candidate, evidence)), "</a><small>",
      html_escape(record$configuration_id), '</small></td><td class="number">',
      if (record$successful) report_number(record$score, 4) else "Not ranked", "</td><td>",
      html_escape(record$status), "</td></tr>"
    )
  }, character(1))
  paste0(
    '<div class="selection-screening"><h4>Screening</h4>',
    '<div class="table-wrap selection-candidate-table selection-screening-table"><table>',
    "<caption>Common training-only sample; ", if (higher) "higher is better" else "lower is better",
    ". Compare CV separately below.</caption>",
    '<thead><tr><th scope="col">Setting preview</th><th scope="col">Screening ',
    html_escape(pretty_metric(metric)), '</th><th scope="col">Screening outcome</th></tr></thead><tbody>',
    paste(rows, collapse = ""), "</tbody></table></div></div>"
  )
}
