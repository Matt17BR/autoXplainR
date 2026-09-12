# Render an already fitted case with explicit, bounded review settings.
args <- commandArgs(TRUE)
stopifnot(length(args) == 2L)
library_path <- normalizePath(args[[1L]])
.libPaths(c(library_path, .libPaths()))
library(AutoXplainR)
stopifnot(normalizePath(find.package("AutoXplainR")) == file.path(library_path, "AutoXplainR"))
directory <- normalizePath(args[[2L]])
result <- readRDS(file.path(directory, "result.rds"))
settings <- list(top_features = 6L, max_models = 4L, n_repeats = 5L,
  explanation_rows = 5000L, export_rows = 1000L, max_pair_rows = 10000L, seed = 824L)
file <- file.path(directory, "review-report.html")
render_model_report(result, file,
  top_features = settings$top_features, max_models = settings$max_models,
  n_repeats = settings$n_repeats, explanation_rows = settings$explanation_rows,
  report_data = report_data_control("rows", max_rows = settings$export_rows,
    max_pair_rows = settings$max_pair_rows, seed = settings$seed))
stopifnot(file.info(file)$size > 10000L)
jsonlite::write_json(list(status = "rendered", settings = settings,
  library = find.package("AutoXplainR"), version = as.character(packageVersion("AutoXplainR")),
  file = file, bytes = file.info(file)$size,
  source_rows = list(training = nrow(result$training_data), evaluation = nrow(result$test_data)),
  scope = "Saved full fit; explicit review settings. This is not a one-call default report timing."),
  file.path(directory, "review-report.json"), pretty = TRUE, auto_unbox = TRUE)
