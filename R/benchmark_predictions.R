#' Measure repeated prediction cost on one common evaluation batch
#'
#' Benchmarks direct prediction through each retained explainer on the same
#' retained evaluation predictor rows. Model fitting, the guided workflow's saved
#' raw-data recipe and report caches are outside the timed call. Transformations
#' inside a custom prediction function are included. These measurements describe
#' this R process and backend, not deployment latency or a general speed ranking.
#'
#' @param result An [autoxplain()] or [evaluate_models()] result.
#' @param models Optional retained model IDs or indices; NULL uses all models.
#' @param batch_size Maximum number of evaluation rows sampled without replacement.
#' @param n_repeats Number of measured repetitions, at least two.
#' @param min_duration Target elapsed seconds for an inner block of repeated
#'   predictions. Calibration also targets at least 20 observed clock steps.
#' @param max_iterations Maximum prediction calls in any inner block.
#' @param max_seconds Soft budget for warmup, calibration and measurement. A
#'   running backend call or inner block can exceed the remaining budget.
#' @param seed Seed for the common batch and interleaved model order. The caller's
#'   RNG state is preserved. Timings themselves are not reproducible numbers.
#'
#' @details Each model receives two warmup calls. Calibration chooses a block
#'   size, then models are measured in a shuffled order in each repetition.
#'   The smallest positive step observed in bounded clock polling is recorded;
#'   it is an empirical clock observation, not a certified hardware resolution.
#'   The 20-step target is an engineering guard against resolution-scale timing,
#'   not a guarantee of stable measurements. Quartiles summarize repeated
#'   measurements and are not confidence intervals.
#'
#'   Raw warmup, calibration and measurement records remain available even if
#'   the budget expires, prediction fails, or elapsed time is not sufficiently
#'   resolved. Summary costs are omitted when fewer than two usable repetitions
#'   completed, clock resolution was not observed, any measured block is resolution
#'   limited, or final identity could not be verified. No model is refitted.
#'   A captured payload fingerprint detects accidental edits to summaries,
#'   raw timings or protocol before attachment; this is not a digital signature.
#'
#' @return An `autoxplain_prediction_benchmark` containing `summary`, raw
#'   `measurements`, `protocol` and model/evaluation identity fingerprints. Attach
#'   it explicitly when rendering or store it as `result$prediction_benchmark`.
#' @export
#' @examples
#' result <- autoxplain(mtcars, "mpg", model_set = "quick", explain = FALSE)
#' bench <- benchmark_predictions(result, n_repeats = 2, min_duration = 0.02)
#' bench$summary
benchmark_predictions <- function(result, models = NULL, batch_size = 256L,
                                  n_repeats = 7L, min_duration = 0.05,
                                  max_iterations = 1000L, max_seconds = 15,
                                  seed = 123L) {
  if (!inherits(result, "autoxplain_result")) {
    stop("`result` must be an AutoXplainR result.", call. = FALSE)
  }
  batch_size <- assert_count(batch_size, "batch_size", minimum = 1L)
  n_repeats <- assert_count(n_repeats, "n_repeats", minimum = 2L)
  max_iterations <- assert_count(max_iterations, "max_iterations", minimum = 1L)
  seed <- assert_count(seed, "seed", minimum = 0L)
  for (name in c("min_duration", "max_seconds")) {
    value <- get(name)
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value <= 0) {
      stop("`", name, "` must be a finite positive number of seconds.", call. = FALSE)
    }
  }
  with_preserved_seed(seed, {
    setup_started <- benchmark_clock()
    result$.report_context <- NULL
    ids <- names(select_models(result$models, models))
    explainers <- report_explainers(result, models = ids)
    assert_common_evaluation(explainers)
    n <- nrow(explainers[[1L]]$data)
    if (!n) stop("At least one evaluation row is needed for a prediction benchmark.", call. = FALSE)
    rows <- sort(sample.int(n, min(batch_size, n), replace = FALSE))
    data <- explainers[[1L]]$data[rows, , drop = FALSE]
    orders <- lapply(seq_len(n_repeats + 2L), function(index) ids[sample.int(length(ids))])
    fingerprints <- vapply(explainers, current_explainer_fingerprint, character(1))
    clock_step <- benchmark_clock_step()
    target <- max(min_duration, if (is.finite(clock_step)) 20 * clock_step else 0)
    setup_elapsed <- (benchmark_clock() - setup_started) * 1000
    started <- benchmark_clock()
    deadline <- started + max_seconds
    iterations <- stats::setNames(rep(NA_integer_, length(ids)), ids)
    recorded <- new.env(parent = emptyenv())
    recorded$failed <- stats::setNames(rep(FALSE, length(ids)), ids)
    recorded$measurements <- list()
    record <- function(id, phase, count, repetition = NA_integer_) {
      value <- benchmark_measure(
        function() predict(explainers[[id]], data), count, id, phase, repetition, nrow(data)
      )
      recorded$measurements[[length(recorded$measurements) + 1L]] <- value
      if (nzchar(value$error)) recorded$failed[[id]] <- TRUE
      value
    }
    expired <- function() benchmark_clock() >= deadline
    for (id in orders[[1L]]) {
      if (expired()) break
      record(id, "warmup", 2L)
    }
    for (id in orders[[2L]]) {
      if (expired()) break
      if (recorded$failed[[id]]) next
      count <- 1L
      repeat {
        if (expired()) break
        value <- record(id, "calibration", count)
        if (recorded$failed[[id]]) break
        iterations[[id]] <- count
        seconds <- value$elapsed_ms / 1000
        if (seconds >= target || count == max_iterations) break
        denominator <- max(seconds, if (is.finite(clock_step)) clock_step else 0, 1e-6)
        count <- as.integer(min(max_iterations, max(count + 1, ceiling(count * target / denominator * 1.1))))
      }
    }
    for (repetition in seq_len(n_repeats)) {
      if (expired()) break
      for (id in orders[[repetition + 2L]]) {
        if (expired()) break
        if (recorded$failed[[id]] || is.na(iterations[[id]])) next
        record(id, "measurement", iterations[[id]], repetition)
      }
    }
    elapsed <- (benchmark_clock() - started) * 1000
    measurements <- if (length(recorded$measurements)) {
      do.call(rbind, recorded$measurements)
    } else {
      empty_benchmark_measurements()
    }
    rownames(measurements) <- NULL
    summaries <- lapply(ids, function(id) {
      benchmark_model_summary(
        id, measurements, n_repeats, nrow(data), clock_step
      )
    })
    summary <- do.call(rbind, summaries)
    identity_errors <- stats::setNames(rep("", length(ids)), ids)
    current <- vapply(ids, function(id) {
      tryCatch(current_explainer_fingerprint(explainers[[id]]), error = function(condition) {
        identity_errors[[id]] <<- conditionMessage(condition)
        NA_character_
      })
    }, character(1))
    changed <- ids[is.na(current) | fingerprints != current]
    if (length(changed)) {
      affected <- summary$model_id %in% changed
      unresolved <- is.na(current[changed])
      summary$status[affected] <- ifelse(unresolved, "identity_failed", "model_changed")
      summary$reason[affected] <- ifelse(unresolved, paste(
        "Identity could not be rechecked after measurement:", identity_errors[changed]
      ), "Prediction changed captured model or prediction-function context during measurement.")
      cost_fields <- grep("^median_|^p25_|^p75_|^min_|^max_", names(summary), value = TRUE)
      summary[affected, cost_fields] <- NA_real_
    }
    benchmark <- structure(list(
      schema_version = 1L, summary = summary, measurements = measurements,
      model_fingerprints = fingerprints, evaluation_fingerprint = content_fingerprint(list(
        explainers[[1L]]$data, explainers[[1L]]$y, explainers[[1L]]$task,
        explainers[[1L]]$class_levels, explainers[[1L]]$positive
      )),
      sampling_rows = rows,
      protocol = list(
        scope = paste(
          "Repeated prediction through retained explainers on a common evaluation predictor batch.",
          "The guided workflow's saved raw-data recipe, fitting and report caches are outside the timed call.",
          "Transformations inside a custom prediction function are included."
        ),
        timing_interpretation = paste(
          "Measurements depend on this process, backend, batch and machine load.",
          "Quartiles are descriptive repeat variability, not confidence intervals or deployment guarantees."
        ),
        clock = "proc.time elapsed seconds", observed_clock_step_ms = clock_step * 1000,
        clock_step_note = paste(
          "Smallest positive step observed during bounded polling; hardware resolution is not certified."
        ),
        batch_rows = nrow(data), evaluation_rows = n, seed = seed, warmup_calls = 2L,
        n_repeats = n_repeats, requested_min_duration_seconds = min_duration,
        calibrated_target_seconds = target, max_iterations = max_iterations,
        soft_budget_seconds = max_seconds, elapsed_ms = elapsed, setup_elapsed_ms = setup_elapsed,
        budget_exceeded = elapsed > max_seconds * 1000,
        r_version = as.character(getRversion()), platform = R.version$platform,
        model_order = orders
      )
    ), class = "autoxplain_prediction_benchmark")
    benchmark$payload_fingerprint <- benchmark_payload_fingerprint(benchmark)
    benchmark
  })
}

benchmark_clock <- function() unname(proc.time()[["elapsed"]])

benchmark_clock_step <- function(max_reads = 10000L, poll_seconds = 0.02) {
  started <- previous <- benchmark_clock()
  positive <- numeric()
  for (index in seq_len(max_reads)) {
    current <- benchmark_clock()
    if (is.finite(current - previous) && current > previous) positive <- c(positive, current - previous)
    previous <- current
    if (current - started >= poll_seconds) break
  }
  if (length(positive)) min(positive) else NA_real_
}

benchmark_measure <- function(predictor, iterations, id, phase, repetition, batch_rows) {
  warnings <- character()
  error <- ""
  completed <- 0L
  started <- benchmark_clock()
  tryCatch(withCallingHandlers(
    {
      for (index in seq_len(iterations)) {
        invisible(predictor())
        completed <- completed + 1L
      }
    },
    warning = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  ), error = function(condition) error <<- conditionMessage(condition))
  elapsed <- (benchmark_clock() - started) * 1000
  if (!is.finite(elapsed) || elapsed < 0) {
    error <- paste(error, "Elapsed clock did not provide a finite nonnegative duration.")
    elapsed <- NA_real_
  }
  data.frame(
    model_id = id, phase = phase, repetition = as.integer(repetition),
    iterations = as.integer(iterations), completed = completed, batch_rows = as.integer(batch_rows),
    elapsed_ms = elapsed, ms_per_batch = if (!nzchar(error)) elapsed / iterations else NA_real_,
    ms_per_row = if (!nzchar(error)) elapsed / iterations / batch_rows else NA_real_,
    warning = paste(unique(warnings), collapse = " | "), error = error, stringsAsFactors = FALSE
  )
}

empty_benchmark_measurements <- function() {
  data.frame(
    model_id = character(), phase = character(), repetition = integer(), iterations = integer(),
    completed = integer(), batch_rows = integer(), elapsed_ms = numeric(), ms_per_batch = numeric(),
    ms_per_row = numeric(), warning = character(), error = character(), stringsAsFactors = FALSE
  )
}

benchmark_model_summary <- function(id, measurements, requested, batch_rows, clock_step) {
  records <- measurements[measurements$model_id == id, , drop = FALSE]
  measured <- records[records$phase == "measurement" & !nzchar(records$error), , drop = FALSE]
  count <- nrow(measured)
  resolution_floor <- if (is.finite(clock_step)) 20 * clock_step * 1000 else 0
  resolution_limited <- count > 0L && any(measured$elapsed_ms <= 0 | measured$elapsed_ms < resolution_floor)
  failures <- unique(records$error[nzchar(records$error)])
  status <- if (length(failures)) "failed" else if (count < requested) "partial" else "computed"
  reason <- if (length(failures)) {
    paste(failures, collapse = " | ")
  } else if (count < requested) {
    paste(count, "of", requested, "timed repetitions completed within the soft budget.")
  } else {
    ""
  }
  if (resolution_limited && !length(failures)) {
    status <- "resolution_limited"
    reason <- paste(
      "A measured block was at or near the observed clock step;",
      "raw timings are retained without a summary cost."
    )
  } else if (count > 0L && !is.finite(clock_step) && !length(failures)) {
    status <- "resolution_unknown"
    reason <- "No positive clock step was observed; raw timings are retained without a summary cost."
  } else if (count < 2L) {
    reason <- paste(reason, "At least two completed repetitions are needed for summary costs.")
  }
  values <- if (count >= 2L && !resolution_limited && is.finite(clock_step) && !length(failures)) {
    measured$ms_per_batch
  } else {
    numeric()
  }
  statistics <- if (length(values)) stats::quantile(values, c(0, .25, .5, .75, 1), names = FALSE) else rep(NA_real_, 5)
  data.frame(
    model_id = id, status = status, reason = trimws(reason), batch_rows = as.integer(batch_rows),
    repetitions = count, requested_repetitions = as.integer(requested),
    min_ms_per_batch = statistics[[1L]], p25_ms_per_batch = statistics[[2L]],
    median_ms_per_batch = statistics[[3L]], p75_ms_per_batch = statistics[[4L]], max_ms_per_batch = statistics[[5L]],
    median_ms_per_row = statistics[[3L]] / batch_rows,
    warning = paste(unique(records$warning[nzchar(records$warning)]), collapse = " | "), stringsAsFactors = FALSE
  )
}

# This detects accidental edits to captured measurements; it is not a signature.
benchmark_payload_fingerprint <- function(benchmark) {
  fields <- c(
    "schema_version", "summary", "measurements", "model_fingerprints", "evaluation_fingerprint",
    "sampling_rows", "protocol"
  )
  content_fingerprint(benchmark[fields])
}

validate_prediction_benchmark <- function(benchmark, result) {
  if (!inherits(benchmark, "autoxplain_prediction_benchmark") || !identical(benchmark$schema_version, 1L)) {
    stop("`benchmark` must be returned by `benchmark_predictions()`.", call. = FALSE)
  }
  intact_payload <- identical(benchmark$payload_fingerprint, benchmark_payload_fingerprint(benchmark))
  if (is.null(benchmark$payload_fingerprint) || !intact_payload) {
    stop(
      "Benchmark payload changed or lacks its recorded integrity fingerprint. ",
      "Run `benchmark_predictions()` again; summaries, raw timings and measurement units must remain together.",
      call. = FALSE
    )
  }
  ids <- names(benchmark$model_fingerprints)
  unidentified <- any(benchmark$summary$status %in% c("model_changed", "identity_failed"))
  if (!length(ids) || !all(ids %in% names(result$models)) || unidentified) {
    stop("The benchmark does not identify unchanged retained models.", call. = FALSE)
  }
  result$.report_context <- NULL
  explainers <- report_explainers(result, models = ids)
  current <- vapply(explainers, current_explainer_fingerprint, character(1))
  if (!identical(current, benchmark$model_fingerprints)) {
    stop("Benchmark model or evaluation content changed; run `benchmark_predictions()` again.", call. = FALSE)
  }
  invisible(TRUE)
}

#' @export
print.autoxplain_prediction_benchmark <- function(x, ...) {
  cat("<AutoXplainR repeated prediction benchmark>\n")
  cat("  batch:", x$protocol$batch_rows, "evaluation predictor rows\n")
  columns <- c("model_id", "status", "repetitions", "median_ms_per_batch", "median_ms_per_row")
  print(x$summary[columns], row.names = FALSE)
  cat(x$protocol$timing_interpretation, "\n")
  invisible(x)
}
