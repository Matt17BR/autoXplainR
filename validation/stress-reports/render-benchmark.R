# Reuse independently fitted benchmark models; no tuning or model fit here.
library_path <- Sys.getenv("AXR_STRESS_LIBRARY")
if (nzchar(library_path)) {
  .libPaths(c(library_path, .libPaths()))
  library(AutoXplainR)
} else {
  pkgload::load_all(quiet = TRUE)
}
base <- path.expand("~/.cache/autoxplain-stress-0.6.2")
case <- Sys.getenv("AXR_STRESS_CASE", "sparse_wide")
setting <- Sys.getenv("AXR_STRESS_SETTING", "core")
source <- file.path(base, "benchmark", "baseline", case, setting, "result.rds")
folder <- file.path(base, "reports", paste(case, setting, sep = "-"))
dir.create(folder, recursive = TRUE, showWarnings = FALSE)
stored <- readRDS(source)
result <- stored$result
Rprof(file.path(folder, "explanations.Rprof"), interval = 0.02)
explanation_time <- system.time({
  result$explanations <- AutoXplainR:::prepare_model_report_data(result,
    top_features = 3L, n_repeats = 2L, max_models = 3L)
})[["elapsed"]]
Rprof(NULL)
write.csv(head(summaryRprof(file.path(folder, "explanations.Rprof"))$by.total, 30L),
  file.path(folder, "explanations-profile.csv"))
cat("Computed3-model explanations in", explanation_time, "seconds\n")
saveRDS(result, file.path(folder, "explained-result.rds"))
times <- list()
title <- switch(case,
  sparse_wide = "Many predictors, few observations",
  friedman_noise = "A nonlinear signal among irrelevant inputs",
  bank_marketing = "Bank marketing responses",
  rare_interaction = "A rare outcome driven by interacting inputs",
  gsub("_", " ", case))
for (mode in c("summary", "rows")) {
  time <- system.time(render_model_report(result, file.path(folder, paste0(mode, ".html")),
    title = title, report_data = report_data_control(mode, max_rows = 300L), uncertainty = FALSE))[["elapsed"]]
  times[[mode]] <- list(seconds = time, bytes = file.info(file.path(folder, paste0(mode, ".html")))$size)
  cat("Rendered", mode, "in", time, "seconds\n")
}
jsonlite::write_json(list(case = case, setting = setting, source = source,
  explanation_seconds = explanation_time, top_features = 3L, n_repeats = 2L, max_models = 3L,
  reports = times, leaderboard = result$leaderboard), file.path(folder, "report-evidence.json"),
  auto_unbox = TRUE, pretty = TRUE, digits = 16, na = "null", null = "null")
