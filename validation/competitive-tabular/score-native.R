# Called only after run-one.R validates the final acceptance freeze, if needed.
score_saved_native <- function(name, phase, variant, threads, destination,
                               fit_source, partition, declared) {
  summary <- jsonlite::read_json(file.path(fit_source, "summary.json"))
  process <- jsonlite::read_json(file.path(fit_source, "process.json"))
  stopifnot(identical(summary$status, "ok"), identical(summary$stage, "fit-only"),
    identical(process$process_status, "ok"), identical(summary$case, name),
    identical(summary$phase, phase), identical(summary$variant, variant),
    summary$native_threads == threads, is.null(summary$metrics),
    identical(as.character(packageVersion(variant)), summary$backend_versions[[variant]]),
    identical(summary$saved_model_sha256, tabular_hash(file.path(fit_source, "model.rds"))))
  saved <- readRDS(file.path(fit_source, "model.rds"))
  stopifnot(saved$threads == threads, identical(saved$family, variant))
  features <- readRDS(file.path(partition, "evaluation-features.rds"))
  truth <- readRDS(file.path(partition, "evaluation-targets.rds"))
  stopifnot(nrow(features$data) == declared$n_evaluation, length(truth) == declared$n_evaluation)
  summary$fit_completed_at <- summary$ended_at
  summary$fit_elapsed_seconds <- summary$elapsed_seconds
  summary$stage <- "score-only"
  summary$fit_source <- fit_source
  summary$fit_source_process_sha256 <- tabular_hash(file.path(fit_source, "process.json"))
  summary$started_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  started <- proc.time()[["elapsed"]]
  predictions <- tabular_native_predict(saved, features$data)
  summary$metrics <- lapply(predictions, tabular_metrics, y = truth, task = saved$task)
  saveRDS(predictions, file.path(destination, "predictions.rds"), compress = FALSE, version = 3L)
  summary$predictions_sha256 <- tabular_hash(file.path(destination, "predictions.rds"))
  summary$evaluation_files_opened <- TRUE
  summary$elapsed_seconds <- proc.time()[["elapsed"]] - started
  summary$ended_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  writeLines(capture.output(sessionInfo()), file.path(destination, "session-info.txt"))
  tabular_json(summary, file.path(destination, "summary.json"))
  cat("Scored saved", variant, "reference on", length(truth), "evaluation rows.\n")
}
