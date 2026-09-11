# Profile a saved fit through an installed baseline or the current source.
library_path <- Sys.getenv("AXR_STRESS_LIBRARY")
if (nzchar(library_path)) {
  .libPaths(c(library_path, .libPaths()))
  library(AutoXplainR)
} else {
  pkgload::load_all(quiet = TRUE)
}
output <- Sys.getenv("AXR_STRESS_REPORTS", path.expand("~/.cache/autoxplain-stress-0.6.2/reports"))
p <- as.integer(Sys.getenv("AXR_STRESS_PROFILE_WIDTH", "100"))
label <- Sys.getenv("AXR_STRESS_PROFILE_RUN", "baseline-profile")
folder <- file.path(output, paste0("wide-", p))
stored <- readRDS(file.path(folder, "result.rds"))
results <- list()
for (mode in c("summary", "rows")) {
  destination <- file.path(folder, paste0(label, "-", mode, ".html"))
  log <- file.path(folder, paste0(label, "-", mode, ".Rprof"))
  gc()
  Rprof(log, interval = 0.01, memory.profiling = TRUE)
  time <- system.time(render_model_report(stored$result, destination,
    audit = stored$audit, uncertainty = FALSE, report_data = mode))[["elapsed"]]
  Rprof(NULL)
  profile <- summaryRprof(log, memory = "both")
  write.csv(head(profile$by.total, 40L), file.path(folder, paste0(label, "-", mode, "-total.csv")))
  write.csv(head(profile$by.self, 40L), file.path(folder, paste0(label, "-", mode, "-self.csv")))
  results[[mode]] <- list(elapsed_seconds = time, bytes = file.info(destination)$size,
    top_total = head(profile$by.total, 15L), top_self = head(profile$by.self, 15L))
  cat(label, p, mode, time, "seconds\n")
}
jsonlite::write_json(results, file.path(folder, paste0(label, ".json")), pretty = TRUE,
  auto_unbox = TRUE, digits = 16)
