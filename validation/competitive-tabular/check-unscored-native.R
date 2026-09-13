arguments <- commandArgs(TRUE)
stopifnot(length(arguments) == 1L)
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script)), "common.R"))
destination <- normalizePath(arguments[[1L]], mustWork = TRUE)
process <- jsonlite::read_json(file.path(destination, "process.json"))
summary <- jsonlite::read_json(file.path(destination, "summary.json"))
stopifnot(identical(process$process_status, "ok"), identical(summary$status, "ok"),
  identical(summary$stage, "fit-only"), identical(summary$variant, "ranger"),
  identical(summary$native_reference_protocol, "fixed-native-forest-v1"),
  identical(summary$evaluation_files_opened, FALSE), is.null(summary$metrics),
  !file.exists(file.path(destination, "predictions.rds")))
started <- proc.time()[["elapsed"]]
saved <- readRDS(file.path(destination, "model.rds"))
parameters <- summary$selected_parameters$parameters
stopifnot(identical(saved$family, "ranger"), saved$threads == 4L,
  saved$model$num.samples == summary$training_rows,
  saved$model$num.samples == summary$native_reference$entry$full_training_rows,
  saved$model$num.trees == 500L, saved$model$forest$num.trees == 500L,
  saved$model$mtry == parameters$mtry,
  saved$model$min.node.size == parameters$min.node.size,
  identical(saved$model$splitrule, parameters$splitrule),
  saved$model$num.independent.variables == summary$predictors,
  identical(saved$model$forest$independent.variable.names, names(saved$blueprint)),
  identical(as.character(packageVersion("ranger")), summary$backend_versions$ranger),
  identical(tabular_hash(file.path(destination, "model.rds")), summary$saved_model_sha256))
record <- list(status = "ok", verified_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  process_id = Sys.getpid(), fresh_session = TRUE,
  verified_training_rows = saved$model$num.samples,
  verified_trees = saved$model$num.trees, verified_mtry = saved$model$mtry,
  verified_min_node_size = saved$model$min.node.size,
  verified_predictors = saved$model$num.independent.variables,
  native_threads = saved$threads, saved_model_sha256 = summary$saved_model_sha256,
  elapsed_seconds = proc.time()[["elapsed"]] - started,
  verification_source_sha256 = tabular_hash(script),
  scope = "Fresh-session structural verification of the saved unscored native forest. No training or evaluation partition files are opened. This is not held-out prediction replay or acceptance quality evidence.")
tabular_json(record, file.path(destination, "unscored-model-verification.json"))
cat("Verified saved unscored forest:", saved$model$num.samples, "rows and", saved$model$num.trees, "trees.\n")
