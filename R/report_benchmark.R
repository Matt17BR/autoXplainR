prepare_report_benchmark <- function(result, benchmark = NULL) {
  benchmark <- benchmark %||% result$prediction_benchmark
  if (is.null(benchmark)) {
    return(result)
  }
  validate_prediction_benchmark(benchmark, result)
  # Attach only to the render copy. Existing fit/batch readings remain intact.
  result$.report_benchmark <- benchmark
  index <- match(result$leaderboard$model_id, benchmark$summary$model_id)
  result$leaderboard$repeated_prediction_ms_per_row <- benchmark$summary$median_ms_per_row[index]
  result
}

render_report_benchmark <- function(result) {
  benchmark <- result$.report_benchmark
  if (is.null(benchmark)) {
    return(paste0(
      "<details><summary>Measure repeated prediction costs</summary>",
      "<p>A single timer reading can be too coarse for small models. ",
      "Measure repeated predictions on a common batch to compare their costs.</p>",
      '<pre tabindex="0"><code>bench &lt;- benchmark_predictions(result)\n',
      'render_model_report(result, "report.html", benchmark = bench)</code></pre></details>'
    ))
  }
  summary <- benchmark$summary
  labels <- vapply(summary$model_id, function(id) explorer_label(result, id), character(1))
  format_cost <- function(values) vapply(values, explorer_measurement, character(1))
  interval <- ifelse(is.finite(summary$p25_ms_per_batch) & is.finite(summary$p75_ms_per_batch),
    paste(format_cost(summary$p25_ms_per_batch), "\u2013", format_cost(summary$p75_ms_per_batch)),
    "Unavailable"
  )
  display <- data.frame(
    Model = labels,
    `Median ms / row` = format_cost(summary$median_ms_per_row),
    `Median ms / batch` = format_cost(summary$median_ms_per_batch),
    interval = interval,
    Repeats = paste(summary$repetitions, "/", summary$requested_repetitions), check.names = FALSE
  )
  names(display)[[4L]] <- "25th\u201375th percentile ms / batch"
  affected <- summary$status != "computed" | nzchar(summary$reason) | nzchar(summary$warning)
  issues <- data.frame(
    Model = labels, Status = summary$status, Reason = summary$reason,
    Warnings = summary$warning, check.names = FALSE
  )[affected, , drop = FALSE]
  paste0(
    '<details class="prediction-benchmark"><summary>Repeated prediction measurements</summary><p>',
    benchmark$protocol$batch_rows, " common evaluation rows per batch. ",
    "Quartiles show variation between measured repeats; they are not confidence intervals. ",
    "Per-row cost is batch cost divided by batch size, not single-row request latency.</p>",
    '<div class="benchmark-cost-table">',
    html_table(display, caption = "Repeated costs on this machine; repeats completed / requested"), "</div>",
    if (nrow(issues)) {
      paste0(
        '<div class="benchmark-issues">',
        html_table(issues, caption = "Incomplete measurements and recorded issues"), "</div>"
      )
    } else {
      '<p class="benchmark-status">All requested repetitions completed without recorded issues.</p>'
    },
    "<details><summary>Measurement protocol</summary><p>",
    html_escape(benchmark$protocol$scope), "</p><p>",
    html_escape(benchmark$protocol$timing_interpretation), "</p>",
    html_table(data.frame(
      Setting = c("Observed clock step (ms)", "Requested repeats", "Timed budget (seconds)", "Seed"),
      Value = c(
        benchmark$protocol$observed_clock_step_ms, benchmark$protocol$n_repeats,
        benchmark$protocol$soft_budget_seconds, benchmark$protocol$seed
      )
    ), caption = "Benchmark controls"), "</details>",
    '<pre tabindex="0"><code>bench$measurements\nbench$protocol</code></pre></details>'
  )
}
