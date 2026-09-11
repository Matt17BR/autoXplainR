# Re-render the saved wide fixtures without changing their fits or audit data.
pkgload::load_all(quiet = TRUE)
base <- Sys.getenv("AXR_STRESS_REPORTS", path.expand("~/.cache/autoxplain-stress-0.6.2/reports"))
out <- file.path(base, "after-wide")
records <- list()
for (p in c(100L, 500L)) {
  source <- file.path(base, paste0("wide-", p), "result.rds")
  stored <- readRDS(source)
  folder <- file.path(out, paste0("wide-", p))
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  for (mode in c("summary", "rows")) {
    destination <- file.path(folder, paste0(mode, ".html"))
    time <- system.time(render_model_report(stored$result, destination,
      audit = stored$audit, uncertainty = FALSE, title = paste(p, "predictors across 80 sites"),
      report_data = mode))[["elapsed"]]
    records[[paste(p, mode, sep = "-")]] <- list(seconds = time, bytes = file.info(destination)$size)
    cat(p, mode, time, "seconds", file.info(destination)$size, "bytes\n")
  }
}
jsonlite::write_json(records, file.path(out, "timings.json"), pretty = TRUE, auto_unbox = TRUE, digits = 16)
