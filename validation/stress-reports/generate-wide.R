# Report scaling is measured separately from fitting and permutation screening.
# Run from the package root. Files remain outside the checkout.
library_path <- Sys.getenv("AXR_STRESS_LIBRARY")
if (nzchar(library_path)) {
  .libPaths(c(library_path, .libPaths()))
  library(AutoXplainR)
} else {
  pkgload::load_all(quiet = TRUE)
}
output <- Sys.getenv("AXR_STRESS_REPORTS", path.expand("~/.cache/autoxplain-stress-0.6.2/reports"))
dir.create(output, recursive = TRUE, showWarnings = FALSE)
widths <- as.integer(strsplit(Sys.getenv("AXR_STRESS_WIDTHS", "100,500"), ",", fixed = TRUE)[[1L]])
records <- list()
for (p in widths) {
  set.seed(260912 + p)
  n <- 1200L
  data <- as.data.frame(matrix(rnorm(n * (p - 1L)), nrow = n))
  names(data) <- sprintf("sensor_%03d", seq_len(p - 1L))
  names(data)[1:3] <- c("pressure_before_stage_one", "temperature_after_compression", "vibration_amplitude")
  data$site <- factor(sample(sprintf("Site %03d with a descriptive regional name", 1:80), n, replace = TRUE))
  data$response <- 3 * sin(data[[1L]]) + 2 * data[[2L]]^2 +
    2 * data[[1L]] * data[[3L]] + rnorm(n, sd = 0.5)
  train <- data[1:900, ]
  test <- data[901:1200, ]
  folder <- file.path(output, paste0("wide-", p))
  dir.create(folder, showWarnings = FALSE)
  cat("Fitting", p, "predictors\n")
  fit_time <- system.time({
    result <- autoxplain(train, "response", test_data = test,
      learners = "tree", max_models = 1L, nfolds = 3L,
      explain = FALSE, seed = 260912L)
  })[["elapsed"]]
  cat("Fit finished in", fit_time, "seconds; auditing three specified inputs\n")
  # Three specified scientific inputs keep the report experiment separate from
  # the cost/quality of ranking 500 predictors. This is stated in the evidence.
  audit_time <- system.time({
    audit <- audit_explanations(as_explainers(result), features = names(data)[1:3],
      n_repeats = 3L, seed = 260912L)
  })[["elapsed"]]
  cat("Audit finished in", audit_time, "seconds; rendering\n")
  timings <- list()
  for (mode in c("summary", "rows")) {
    cat("Rendering", p, mode, "\n")
    file <- file.path(folder, paste0(mode, ".html"))
    elapsed <- system.time({
      report <- render_model_report(result, file, audit = audit, uncertainty = FALSE,
        title = paste(p, "predictors across 80 sites"), report_data = mode)
    })[["elapsed"]]
    timings[[mode]] <- list(elapsed_seconds = elapsed, bytes = file.info(file)$size,
      manifest = attr(report, "data_manifest"))
  }
  saveRDS(list(result = result, audit = audit), file.path(folder, "result.rds"))
  oracle <- list(predictors = p, columns = names(data), training_rows = nrow(train),
    evaluation_rows = nrow(test), signal = names(data)[1:3],
    last_predictor = paste0("sensor_", sprintf("%03d", p - 1L)),
    primary = result$provenance$primary_model_id,
    leaderboard = result$leaderboard,
    target_mean = c(training = mean(train$response), evaluation = mean(test$response)),
    last_mean = c(training = mean(train[[p - 1L]]), evaluation = mean(test[[p - 1L]])),
    site_counts = list(training = as.data.frame(table(train$site)), evaluation = as.data.frame(table(test$site))),
    target_correlations = vapply(data[-ncol(data)], function(x) {
      if (is.numeric(x)) cor(x[901:1200], test$response, method = "spearman") else NA_real_
    }, numeric(1)),
    fitting_seconds = fit_time, specified_feature_audit_seconds = audit_time, reports = timings)
  jsonlite::write_json(oracle, file.path(folder, "oracle.json"), auto_unbox = TRUE,
    pretty = TRUE, digits = 16, na = "null", null = "null")
  records[[paste0("wide-", p)]] <- oracle[c("predictors", "fitting_seconds", "specified_feature_audit_seconds", "reports")]
  cat("Finished", p, "predictors:", jsonlite::toJSON(records[[paste0("wide-", p)]], auto_unbox = TRUE), "\n")
}
jsonlite::write_json(records, file.path(output, "generation.json"), auto_unbox = TRUE,
  pretty = TRUE, digits = 16, na = "null", null = "null")
writeLines(capture.output(sessionInfo()), file.path(output, "session-info.txt"))
