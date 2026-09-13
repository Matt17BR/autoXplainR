# Source common.R first. This verifier reads JSON and hashes artifacts, never RDS contents.
tabular_verify_resources <- function(destination,
                                     process = jsonlite::read_json(file.path(destination, "process.json")),
                                     cache = tabular_cache(), require_success = TRUE) {
  check <- function(value, message) {
    if (!isTRUE(value)) stop(message, call. = FALSE)
  }
  number <- function(value, label) {
    check(is.numeric(value) && length(value) == 1L && is.finite(value) && value >= 0,
      paste(label, "must be a finite nonnegative scalar."))
    as.numeric(value)
  }
  same_number <- function(actual, expected, label) {
    check(abs(number(actual, label) - expected) <= 1e-6, paste(label, "is inconsistent."))
  }
  hash <- function(path) {
    check(file.exists(path), paste("Missing resource evidence:", path))
    tabular_hash(path)
  }
  rss <- function(record) {
    keys <- intersect(c("peak_rss_kib", "sampled_child_high_water_rss_kib"), names(record))
    check(length(keys) > 0L, "A process has no observed RSS measurement.")
    max(vapply(keys, function(key) number(record[[key]], key), numeric(1)))
  }
  destination <- normalizePath(destination, mustWork = TRUE)
  check(is.logical(require_success) && length(require_success) == 1L && !is.na(require_success),
    "The success requirement must be one logical value.")
  current_path <- file.path(destination, "process.json")
  check(identical(process, jsonlite::read_json(current_path)), "Current process record changed.")
  check(process$phase %in% c("acceptance", "development"), "Unknown process phase.")
  check(number(process$threads, "Native threads") %in% c(1, 4), "Unexpected native thread count.")
  check(is.character(process$partitions_sha256) && length(process$partitions_sha256) == 1L &&
    grepl("^[a-f0-9]{64}$", process$partitions_sha256), "Missing partition-manifest identity.")
  limit <- if (identical(process$phase, "acceptance")) 7200 else 1200
  check(process$stage %in% c("fit-and-score", "fit-only", "score-only"), "Unknown process stage.")
  if (require_success) {
    check(identical(process$process_status, "ok") && identical(process$exit_code, 0L),
      "The current process must have completed successfully.")
  } else {
    check(process$process_status %in% c("failed", "timeout"),
      "Failure accounting requires a final failed or timed-out process.")
    check(is.numeric(process$exit_code) && length(process$exit_code) == 1L && is.finite(process$exit_code) &&
      process$exit_code == trunc(process$exit_code) &&
      (!identical(process$process_status, "failed") || process$exit_code != 0),
      "The failed process lacks a valid final exit code.")
  }
  validate <- function(record, successful = FALSE) {
    check(identical(record$phase, process$phase) && identical(record$case, process$case) &&
      identical(record$variant, process$variant) && identical(record$threads, process$threads),
      "A process has a different phase, case, variant or thread count.")
    check(record$process_status %in% c("ok", "failed", "timeout"),
      "An applicable process is active or lacks a final status.")
    if (successful) check(identical(record$process_status, "ok") && identical(record$exit_code, 0L),
      "The source fit must have completed successfully.")
    same_number(record$combined_wall_limit_seconds, limit, "Combined wall limit")
    same_number(record$address_space_limit_bytes, 24 * 1024^3, "Address-space limit")
    number(record$wall_limit_seconds, "Process wall limit")
    list(seconds = number(record$process_elapsed_seconds, "Process elapsed seconds"), rss_kib = rss(record))
  }
  current <- validate(process, require_success)
  total <- current$seconds
  peak <- current$rss_kib
  fit_evidence <- NULL
  attempts <- list()
  reported_scope <- NULL
  if (identical(process$stage, "score-only")) {
    check(is.character(process$fit_source) && length(process$fit_source) == 1L && nzchar(process$fit_source),
      "The scoring process lacks its source fit directory.")
    fit_directory <- normalizePath(process$fit_source, mustWork = TRUE)
    source_hashes <- setNames(vapply(c(process = "process.json", summary = "summary.json", model = "model.rds"),
      function(file) hash(file.path(fit_directory, file)), character(1)), c("process", "summary", "model"))
    for (key in names(source_hashes)) check(identical(process[[paste0("fit_source_", key, "_sha256")]], source_hashes[[key]]),
      paste("Source fit", key, "hash changed."))
    check(identical(hash(file.path(destination, "model.rds")), source_hashes[["model"]]),
      "The scored model differs from the bound source fit.")
    fit <- jsonlite::read_json(file.path(fit_directory, "process.json"))
    summary <- jsonlite::read_json(file.path(fit_directory, "summary.json"))
    check(identical(fit$stage, "fit-only") && identical(summary$stage, "fit-only") &&
      identical(summary$status, "ok") && identical(summary$phase, process$phase) &&
      identical(summary$case, process$case) && identical(summary$variant, process$variant) &&
      identical(summary$native_threads, process$threads) &&
      identical(summary$saved_model_sha256, source_hashes[["model"]]) &&
      identical(fit$partitions_sha256, process$partitions_sha256), "Source fit identity or completion evidence differs.")
    if (!is.null(fit$summary_sha256)) check(identical(fit$summary_sha256, source_hashes[["summary"]]),
      "The source process binds a different summary.")
    fit_values <- validate(fit, TRUE)
    same_number(fit$wall_limit_seconds, limit, "Source fit wall limit")
    fit_evidence <- list(directory = fit_directory, hashes = as.list(source_hashes),
      process_elapsed_seconds = fit_values$seconds, observed_high_water_rss_kib = fit_values$rss_kib)
    paths <- unique(normalizePath(Sys.glob(file.path(cache, "runs", "*", "*", "*", "process.json")), mustWork = TRUE))
    for (path in paths) {
      record <- jsonlite::read_json(path)
      if (!identical(record$fit_source_process_sha256, source_hashes[["process"]])) next
      check(identical(record$stage, "score-only"), "A source-bound attempt has an unexpected stage.")
      values <- validate(record)
      for (key in names(source_hashes)) check(identical(record[[paste0("fit_source_", key, "_sha256")]], source_hashes[[key]]),
        paste("A scoring attempt binds a different source", key, "hash."))
      check(identical(record$partitions_sha256, process$partitions_sha256), "A scoring attempt used different partitions.")
      attempts[[length(attempts) + 1L]] <- list(path = path, process_sha256 = hash(path),
        status = record$process_status, seconds = values$seconds, rss_kib = values$rss_kib,
        previous = number(record$previous_scoring_process_seconds, "Previous scoring seconds"), record = record)
    }
    check(any(vapply(attempts, function(item) identical(item$path, normalizePath(current_path)), logical(1))),
      "The current scoring process is absent from the cache run inventory.")
    attempts <- attempts[order(vapply(attempts, `[[`, numeric(1), "previous"),
      vapply(attempts, function(item) if (item$seconds == 0) 0 else 1, numeric(1)))]
    previous <- 0
    prefix_peak <- fit_values$rss_kib
    current_prefix_peak <- NULL
    for (index in seq_along(attempts)) {
      item <- attempts[[index]]
      same_number(item$previous, previous, "Recorded prior-attempt total")
      same_number(item$record$fit_process_elapsed_seconds, fit_values$seconds, "Recorded fit elapsed seconds")
      same_number(item$record$wall_limit_seconds, limit - fit_values$seconds - previous, "Remaining scoring wall limit")
      same_number(item$record$combined_process_elapsed_seconds, fit_values$seconds + previous + item$seconds,
        "Recorded combined process elapsed seconds")
      previous <- previous + item$seconds
      prefix_peak <- max(prefix_peak, item$rss_kib)
      if (identical(item$path, normalizePath(current_path))) current_prefix_peak <- prefix_peak
      attempts[[index]]$record <- NULL
    }
    total <- fit_values$seconds + previous
    peak <- max(fit_values$rss_kib, vapply(attempts, `[[`, numeric(1), "rss_kib"))
    recorded_rss <- number(process$combined_observed_high_water_rss_kib, "Reported combined RSS")
    legacy_rss <- function(record) {
      if (!is.null(record$peak_rss_kib)) record$peak_rss_kib else record$sampled_child_high_water_rss_kib
    }
    legacy_peak <- max(legacy_rss(process), legacy_rss(fit))
    check(recorded_rss %in% c(current_prefix_peak, legacy_peak),
      "Reported combined RSS matches neither the full prefix nor the historical fit/current aggregate.")
    reported_scope <- list(value_kib = recorded_rss,
      scope = if (recorded_rss == current_prefix_peak) "Matches the fit and all scoring attempts through this process; later attempts are also included in the authoritative total." else
        "Historical narrower aggregate; authoritative RSS includes the fit and every scoring attempt.")
  } else {
    same_number(process$wall_limit_seconds, limit, "Process wall limit")
  }
  check(is.finite(total), "The complete fit/scoring elapsed total is not finite.")
  within_limits <- total <= limit && current$seconds <= process$wall_limit_seconds
  if (require_success) {
    check(total <= limit, "The complete fit/scoring work exceeded its declared wall limit.")
    check(current$seconds <= process$wall_limit_seconds, "The current process exceeded its remaining wall limit.")
  }
  list(status = if (require_success) "ok" else "failed_attempt_verified", phase = process$phase, stage = process$stage,
    current_process_status = process$process_status, within_limits = within_limits,
    process_sha256 = hash(current_path), combined_wall_limit_seconds = limit,
    address_space_limit_bytes = 24 * 1024^3, current_process_elapsed_seconds = current$seconds,
    combined_process_elapsed_seconds = total, combined_observed_high_water_rss_kib = peak,
    fit = fit_evidence, scoring_attempts = attempts, process_reported_combined_rss = reported_scope,
    scope = paste(if (!require_success) "This accounted failed/timeout attempt remains an operational failure and provides no quality credit." else "",
      "Resource gate covers the complete fit process and every recorded scoring attempt for that exact saved fit, including failures.",
      "Fresh-session prediction replay and independent verification are separate and excluded from this resource gate.",
      "Only metadata and artifact hashes are read; no model or partition is deserialized."))
}
