# Fresh-process reload, full holdout prediction and optional standalone report.
args <- commandArgs(TRUE)
stopifnot(length(args) %in% c(2L, 3L))
library_path <- normalizePath(args[[1L]], mustWork = TRUE)
.libPaths(c(library_path, .libPaths()))
library(AutoXplainR)
loaded_library <- normalizePath(find.package("AutoXplainR"), mustWork = TRUE)
stopifnot(loaded_library == file.path(library_path, "AutoXplainR"))
directory <- normalizePath(args[[2L]])
stopifnot(!"glmnet" %in% loadedNamespaces(), !"xgboost" %in% loadedNamespaces())
input <- jsonlite::read_json(file.path(directory, "input.json"), simplifyVector = TRUE)
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script <- normalizePath(sub("^--file=", "", script_arg))
fixture_script <- file.path(dirname(script), "fixtures.R")
stopifnot(unname(tools::md5sum(fixture_script)) == input$fixture_source_md5)
source(fixture_script)
result <- readRDS(file.path(directory, "result.rds"))
saved <- readRDS(file.path(directory, "replay.rds"))
holdout <- scale_fixture(input$problem, input$evaluation_rows, evaluation = TRUE)
losses <- list()
factor_predictors <- setdiff(names(saved$probe)[vapply(saved$probe, is.factor, logical(1L))], "outcome")
novel_predictors <- intersect("segment", names(saved$probe))
for (id in names(saved$expected)) {
  prediction <- predict(result, saved$probe, model = id)
  stopifnot(isTRUE(all.equal(prediction, saved$expected[[id]], tolerance = 1e-12)))
  reordered <- saved$probe
  for (name in names(reordered)[vapply(reordered, is.factor, logical(1L))]) {
    if (name != "outcome") {
      reordered[[name]] <- factor(as.character(reordered[[name]]), levels = rev(levels(reordered[[name]])))
    }
  }
  stopifnot(isTRUE(all.equal(predict(result, reordered, model = id), prediction, tolerance = 1e-12)))
  unusual <- saved$probe
  unusual$x1[[1L]] <- NA_real_
  if ("segment" %in% names(unusual)) {
    unusual$segment <- as.character(unusual$segment)
    unusual$segment[[2L]] <- "new-level-for-cold-replay"
    unusual$segment[[3L]] <- NA_character_
  }
  stopifnot(all(is.finite(predict(result, unusual, model = id))))
  full <- predict(result, holdout$data, model = id)
  stopifnot(NROW(full) == input$evaluation_rows, all(is.finite(full)))
  loss <- independent_scale_loss(holdout$data$outcome, full)
  metric <- if (result$task == "regression") "rmse" else "log_loss"
  stopifnot(isTRUE(all.equal(loss, unname(result$evaluation$metrics[[id]][[metric]]), tolerance = 1e-10)))
  losses[[id]] <- loss
}
rendered <- FALSE
if (length(args) == 3L && args[[3L]] == "report") {
  render_model_report(result, file.path(directory, "reloaded-report.html"),
    top_features = 4L, max_models = 3L, n_repeats = 5L, report_data = "summary")
  rendered <- file.info(file.path(directory, "reloaded-report.html"))$size > 10000
  stopifnot(rendered)
}
jsonlite::write_json(list(status = "passed", library = loaded_library,
  version = as.character(packageVersion("AutoXplainR")), script_md5 = unname(tools::md5sum(script)),
  models = length(saved$expected),
  full_holdout_rows_per_model = input$evaluation_rows, losses = losses,
  reordered_factor_predictors = factor_predictors,
  reordered_factor_levels_preserve_predictions = if (length(factor_predictors)) TRUE else NULL,
  numeric_missing_predictors = "x1", novel_level_predictors = novel_predictors,
  predictions_with_tested_missing_and_novel_inputs_are_finite = TRUE,
  report_rendered = rendered, report_controls = if (rendered) {
    list(top_features = 4L, max_models = 3L, n_repeats = 5L, report_data = "summary")
  } else NULL), file.path(directory, "cold-replay.json"), pretty = TRUE, auto_unbox = TRUE, digits = 16)
writeLines(capture.output(sessionInfo()), file.path(directory, "cold-session-info.txt"))
cat("Cold replay passed:", length(saved$expected), "models on all", input$evaluation_rows, "holdout rows\n")
