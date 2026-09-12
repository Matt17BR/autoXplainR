# Read metadata captured during the timed fits. Do not refit or query current
# engine versions as a substitute for the versions stored with those models.
cache <- Sys.getenv("AXR_SEARCH_DIR", path.expand("~/.cache/autoxplain-scale-0.7.0/search"))
.libPaths(c(file.path(cache, "candidate-library"), .libPaths()))
library(AutoXplainR)
stopifnot(as.character(packageVersion("AutoXplainR")) == "0.7.0")
destination <- "validation/scalability/search"
records <- list()
for (variant in c("baseline", "candidate", "candidate_binary_guard")) {
  for (scenario in c("friedman_noise", "bank_marketing")) {
    directory <- file.path(cache, "recommended", variant, scenario)
    if (!file.exists(file.path(directory, "result.rds")) && variant == "candidate_binary_guard") next
    stored <- readRDS(file.path(directory, "result.rds"))
    result <- stored$result
    stopifnot(identical(stored$record$status, "ok"))
    for (id in names(result$models)) {
      metadata <- attr(result$models[[id]], "autoxplain_tuning_fit")
      if (is.null(metadata)) {
        stopifnot(identical(id, "simple_baseline"))
        next
      }
      stopifnot(length(metadata$backend_version) == 1L)
      records[[length(records) + 1L]] <- data.frame(
        case = scenario, variant = variant, model_id = id,
        package_version = result$provenance$package_version,
        r_version = result$provenance$r_version,
        family = metadata$family, backend_version = metadata$backend_version,
        stringsAsFactors = FALSE
      )
    }
    # Keep raw cache logs unchanged; portable copies use LF and no trailing space.
    session_lines <- readLines(file.path(directory, "session-info.txt"), warn = FALSE)
    writeLines(sub("[[:blank:]]+$", "", session_lines),
      file.path(destination, paste0("recommended-", variant, "-", scenario, "-session.txt")))
  }
}
records <- do.call(rbind, records)
for (family in unique(records$family)) {
  stopifnot(length(unique(records$backend_version[records$family == family])) == 1L)
}
write.csv(records, file.path(destination, "recommended-engine-versions.csv"), row.names = FALSE)
cat(nrow(records), "saved-model version records agree across the paired runs.\n")
