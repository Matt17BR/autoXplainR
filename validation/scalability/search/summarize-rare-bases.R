# Retain every fixed-fold comparison, including any worse losses or failures.
cache <- Sys.getenv("AXR_SEARCH_DIR", path.expand("~/.cache/autoxplain-scale-0.7.0/search"))
operations <- jsonlite::read_json(file.path(cache, "rare-bases-operations.json"))
rows <- lapply(operations, function(operation) {
  stem <- paste("rare_1pct", operation$seed, paste0("k", operation$k), operation$solver, sep = "-")
  path <- file.path(cache, paste0(stem, ".json"))
  record <- if (file.exists(path)) jsonlite::read_json(path) else list()
  reference <- file.path(cache, paste0("rare_1pct-", operation$seed, "-k", operation$k, "-gam.rds"))
  fitted <- file.path(cache, paste0(stem, ".rds"))
  difference <- if (file.exists(reference) && file.exists(fitted)) {
    old <- readRDS(reference)
    new <- readRDS(fitted)
    stopifnot(identical(old$fold$training, new$fold$training),
              identical(old$fold$validation, new$fold$validation))
    abs(old$prediction - new$prediction)
  } else NA_real_
  value <- function(name, default = NA) if (is.null(record[[name]])) default else record[[name]]
  data.frame(case = "rare_1pct", seed = operation$seed, k = operation$k,
    solver = operation$solver, training_rows = value("training_rows"),
    validation_rows = value("validation_rows"), event_count = value("event_count"),
    coefficients = value("coefficients"), fit_seconds = value("fit_seconds"),
    log_loss = value("score"), max_probability_difference = max(difference),
    mean_probability_difference = mean(difference),
    optimization = if (is.null(record$optimization$status)) NA_character_ else record$optimization$status,
    status = value("status", operation$status),
    warning_count = length(record$warnings),
    final_nonconvergence_warning_count = sum(vapply(record$warnings, function(warning) {
      identical(warning, "algorithm did not converge")
    }, logical(1))),
    timing_context = operation$timing_context,
    error = value("error", NA_character_), stringsAsFactors = FALSE)
})
output <- do.call(rbind, rows)
utils::write.csv(output, "validation/scalability/search/rare-bases-comparison.csv", row.names = FALSE)
print(output)
