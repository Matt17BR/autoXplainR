# Usage: Rscript --vanilla check-unscored-xgboost.R FIT_DIRECTORY [NEW_OUTPUT_JSON] [PARTITIONS_JSON]
# Reads saved fit evidence and the partition manifest, never partition data.
arguments <- commandArgs(TRUE)
stopifnot(length(arguments) %in% 1:3)
script <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]]))
source(file.path(dirname(script), "common.R"))
fit_directory <- normalizePath(arguments[[1L]], mustWork = TRUE)
cache <- path.expand(Sys.getenv("AXR_TABULAR_DIR", "~/.cache/autoxplain-tabular-0.8.0"))
output_path <- if (length(arguments) >= 2L) path.expand(arguments[[2L]]) else {
  file.path(cache, "reproducibility", paste0("unscored-xgboost-audit-",
    format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC"), "-", Sys.getpid()), "verification.json")
}
if (!startsWith(output_path, "/")) stop("Use an absolute output JSON path.")
output_path <- file.path(normalizePath(dirname(output_path), mustWork = FALSE), basename(output_path))
if (file.exists(output_path) || startsWith(output_path, paste0(fit_directory, "/"))) {
  stop("Write verification to a new output outside the original fit directory.")
}
partitions_path <- if (length(arguments) == 3L) normalizePath(arguments[[3L]], mustWork = TRUE) else {
  file.path(cache, "partitions.json")
}
started <- proc.time()[["elapsed"]]
require_check <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}
record <- list(status = "failed", fit_directory = fit_directory,
  process_id = Sys.getpid(), fresh_session = TRUE,
  evaluation_files_opened = FALSE, training_partition_opened = FALSE,
  evaluation_replay_performed = FALSE,
  scope = paste("Fresh-session saved XGBoost structure, hashes and training-calibration provenance only.",
    "No training or evaluation partition files are opened. Training row counts are manifest, summary and log metadata;",
    "XGBoost does not retain the training row count. This is not evaluation prediction replay or acceptance quality evidence."),
  verification_source_sha256 = list(checker = tabular_hash(script),
    common = tabular_hash(file.path(dirname(script), "common.R"))))
tryCatch({
  process_path <- file.path(fit_directory, "process.json")
  summary_path <- file.path(fit_directory, "summary.json")
  model_path <- file.path(fit_directory, "model.rds")
  process <- jsonlite::read_json(process_path)
  summary <- jsonlite::read_json(summary_path)
  record$case <- summary$case
  record$phase <- summary$phase
  record$variant <- "xgboost"
  record$fit_process_sha256 <- tabular_hash(process_path)
  record$fit_summary_sha256 <- tabular_hash(summary_path)
  require_check(identical(process$process_status, "ok") && process$exit_code == 0L &&
    identical(summary$status, "ok") && identical(process$stage, "fit-only") &&
    identical(summary$stage, "fit-only") && identical(summary$variant, "xgboost") &&
    identical(process$variant, "xgboost") && identical(process$case, summary$case) &&
    identical(process$phase, summary$phase) && identical(summary$primary, "xgboost"),
    "The source must be a successful matching XGBoost fit-only process and summary.")
  require_check(is.null(summary$native_reference_protocol) && is.null(process$native_reference_protocol),
    "This checker supports the original two-setting XGBoost protocol only.")
  require_check(identical(summary$evaluation_files_opened, FALSE) &&
    !any(c("metrics", "predictions_sha256", "fit_source") %in% names(summary)) &&
    !file.exists(file.path(fit_directory, "predictions.rds")),
    "The fit has evaluation metrics, predictions or opened evaluation files.")
  if (!is.null(process$summary_sha256)) {
    require_check(identical(process$summary_sha256, record$fit_summary_sha256), "Saved summary hash changed.")
  }
  record$summary_hash_bound_by_process <- !is.null(process$summary_sha256)
  require_check(identical(process$partitions_sha256, tabular_hash(partitions_path)),
    "Partition manifest hash differs from the completed process.")
  partitions <- jsonlite::read_json(partitions_path)
  declared <- partitions$cases[[paste(summary$case, summary$phase, sep = "/")]]
  require_check(!is.null(declared) && identical(summary$task, declared$task) &&
    identical(summary$partition_files$training.rds, declared$files$training.rds),
    "Training partition hash metadata or declared task differs.")
  require_check(summary$training_rows == declared$n_training && summary$native_training_rows == declared$n_training &&
    summary$predictors == declared$predictors && process$threads == summary$native_threads &&
    process$threads %in% c(1L, 4L), "Training row, predictor or thread metadata differs.")
  limit <- if (identical(summary$phase, "acceptance")) 7200 else 1200
  require_check(summary$phase %in% c("development", "acceptance") &&
    process$wall_limit_seconds == limit && process$combined_wall_limit_seconds == limit &&
    process$address_space_limit_bytes == 24 * 1024^3 &&
    is.finite(process$process_elapsed_seconds) && process$process_elapsed_seconds <= limit,
    "The fit does not satisfy the original process resource limits.")
  require_check(all(c("run-one.R", "common.R", "run.py", "README.md", "native-staging.md") %in% names(process$scripts)),
    "The frozen fitting/protocol script inventory is incomplete.")
  for (filename in names(process$scripts)) {
    require_check(identical(filename, basename(filename)) &&
      identical(tabular_hash(file.path(fit_directory, "scripts", filename)), process$scripts[[filename]]),
      paste("Frozen script hash changed:", filename))
  }
  require_check(identical(process$scripts[["README.md"]], partitions$protocol_sha256),
    "Original protocol hash differs from the partition manifest.")
  record$frozen_script_hashes_verified <- process$scripts
  trace_path <- file.path(fit_directory, "process.log")
  trace <- readLines(trace_path, warn = FALSE)
  final_lines <- grep("^NATIVE_FINAL xgboost fit ", trace, value = TRUE)
  require_check(length(final_lines) == 1L &&
    identical(trimws(final_lines), paste("NATIVE_FINAL xgboost fit", declared$n_training, "rows")),
    "The original full-training NATIVE_FINAL trace is absent or inconsistent.")
  calibration_lines <- grep("^NATIVE_CALIBRATION xgboost ", trace, value = TRUE)
  calibration_rows <- floor(.2 * declared$n_training)
  expected_lines <- vapply(1:2, function(index) paste("NATIVE_CALIBRATION xgboost", index,
    "fit", declared$n_training - calibration_rows, "assess", calibration_rows, "rows"), character(1))
  require_check(identical(trimws(calibration_lines), expected_lines),
    "The two original training-calibration row traces are absent or inconsistent.")
  record$process_log_sha256 <- tabular_hash(trace_path)
  objective <- switch(summary$task, regression = "reg:squarederror", binary = "binary:logistic",
    multiclass = "multi:softprob")
  metric <- if (summary$task == "regression") "rmse" else "log_loss"
  native_metric <- switch(summary$task, regression = "rmse", binary = "logloss", multiclass = "mlogloss")
  calibrations <- summary$calibration
  require_check(length(calibrations) == 2L, "Exactly two original calibration settings are required.")
  require_check(identical(jsonlite::read_json(file.path(fit_directory, "calibration.json")), calibrations),
    "Calibration checkpoint differs from the completed summary.")
  record$calibration_checkpoint_sha256 <- tabular_hash(file.path(fit_directory, "calibration.json"))
  calibration_evidence <- vector("list", 2L)
  for (index in 1:2) {
    calibration <- calibrations[[index]]
    expected <- list(max_depth = c(6L, 10L)[[index]], eta = .05, min_child_weight = 1L,
      subsample = .8, colsample_bytree = .8, lambda = 1L, alpha = 0L,
      tree_method = "hist", max_bin = 256L, nthread = process$threads,
      objective = objective, eval_metric = native_metric, seed = declared$fit_seed, verbosity = 0L)
    if (summary$task == "multiclass") expected$num_class <- length(declared$class_levels)
    require_check(setequal(names(calibration$parameters), names(expected)) &&
      all(vapply(names(expected), function(key) isTRUE(all.equal(calibration$parameters[[key]],
        expected[[key]], tolerance = 0)), logical(1))),
      paste("Calibration parameters differ from the original controls for setting", index))
    csv_path <- file.path(fit_directory, paste0("calibration-", index, "-rounds.csv"))
    rounds <- read.csv(csv_path, check.names = FALSE)
    require_check(identical(names(rounds), c("iter", paste0("calibration_", native_metric))) &&
      nrow(rounds) >= 1L && nrow(rounds) <= 1500L && identical(rounds$iter, seq_len(nrow(rounds))) &&
      all(is.finite(rounds[[2L]])), paste("Invalid saved calibration round log for setting", index))
    best <- which.min(rounds[[2L]])
    loss <- tabular_primary(calibration$calibration_metrics, summary$task)
    require_check(calibration$selected_rounds == best, paste("Selected rounds differ from the CSV minimum for setting", index))
    require_check(nrow(rounds) == min(1500L, best + 50L),
      paste("Calibration round log violates the 1500-round/50-round-stopping controls for setting", index))
    require_check(length(loss) == 1L && is.finite(loss) && abs(loss - rounds[[2L]][best]) < 1e-5,
      paste("Calibration primary loss differs from the selected CSV round for setting", index))
    prediction_path <- file.path(fit_directory, paste0("calibration-", index, "-predictions.rds"))
    require_check(identical(calibration$calibration_predictions_sha256, tabular_hash(prediction_path)),
      paste("Training-calibration prediction hash changed for setting", index))
    calibration_evidence[[index]] <- list(parameters = calibration$parameters, selected_rounds = best,
      observed_calibration_rounds = nrow(rounds), calibration_primary_loss = loss,
      csv_minimum_loss = rounds[[2L]][best], csv_sha256 = tabular_hash(csv_path),
      calibration_predictions_sha256 = calibration$calibration_predictions_sha256)
  }
  chosen <- which.min(vapply(calibrations, function(item) tabular_primary(item$calibration_metrics, summary$task), numeric(1)))
  require_check(summary$selected_configuration == chosen &&
    identical(summary$selected_parameters, calibrations[[chosen]]),
    "The selected configuration is not the actual minimum training-calibration primary loss.")
  record$calibration <- calibration_evidence
  record$selected_configuration <- chosen
  record$selected_rounds <- calibrations[[chosen]]$selected_rounds
  record$selection_metric <- metric
  require_check(identical(tabular_hash(model_path), summary$saved_model_sha256) &&
    file.info(model_path)$size == summary$saved_model_bytes, "Saved native model hash or size changed.")
  saved <- readRDS(model_path)
  require_check(identical(saved$family, "xgboost") && inherits(saved$model, "xgb.Booster") &&
    identical(saved$task, summary$task) && saved$threads == process$threads &&
    identical(saved$class_levels, unlist(declared$class_levels, use.names = FALSE)) &&
    identical(as.character(packageVersion("xgboost")), summary$backend_versions$xgboost) &&
    identical(summary$backend_versions$xgboost, "3.2.1.1"),
    "Saved model family, task, class levels, threads or backend version differs.")
  actual_rounds <- xgboost::xgb.get.num.boosted.rounds(saved$model)
  require_check(actual_rounds == record$selected_rounds && actual_rounds == summary$native_verified_boosting_rounds,
    "Native boosted round count differs from the selected training-calibration round count.")
  configuration <- xgboost::xgb.config(saved$model)
  learner <- configuration$learner
  selected_parameters <- calibrations[[chosen]]$parameters
  native_parameters <- learner$gradient_booster$tree_train_param
  for (key in c("max_depth", "eta", "min_child_weight", "subsample", "colsample_bytree", "lambda", "alpha", "max_bin")) {
    # Native configuration writes float32 values as decimal strings.
    require_check(length(native_parameters[[key]]) == 1L &&
      abs(as.numeric(native_parameters[[key]]) - selected_parameters[[key]]) < 1e-7,
      paste("Native saved booster parameter differs:", key))
  }
  require_check(identical(learner$gradient_booster$name, "gbtree") &&
    identical(learner$generic_param$device, "cpu") &&
    identical(learner$gradient_booster$gbtree_train_param$tree_method, "hist") &&
    identical(learner$learner_train_param$objective, objective) && identical(learner$objective$name, objective) &&
    identical(learner$metrics$name, native_metric) &&
    as.integer(learner$generic_param$nthread) == process$threads &&
    as.integer(learner$generic_param$seed) == declared$fit_seed &&
    as.integer(learner$learner_model_param$num_class) == if (summary$task == "multiclass") length(saved$class_levels) else 0L,
    "Native saved booster task, controls, seed or threads differs.")
  require_check(length(saved$blueprint) == summary$predictors && !is.null(names(saved$blueprint)) &&
    !anyDuplicated(names(saved$blueprint)), "Saved feature blueprint is inconsistent.")
  # Rebuild only the encoded column names from synthetic values and the saved
  # blueprint. This does not open training data or run a prediction.
  prototype <- as.data.frame(lapply(saved$blueprint, function(levels) if (is.null(levels)) 0 else levels[[1L]]),
    optional = TRUE, stringsAsFactors = FALSE)
  encoded_names <- colnames(tabular_bake(prototype, saved$blueprint, matrix = TRUE))
  native_names <- xgboost::getinfo(saved$model, "feature_name")
  require_check(identical(native_names, encoded_names) &&
    as.integer(learner$learner_model_param$num_feature) == length(native_names),
    "Native feature names/count differ from the saved encoding blueprint.")
  require_check(as.integer(learner$gradient_booster$gbtree_model_param$num_parallel_tree) == 1L &&
    as.integer(learner$gradient_booster$gbtree_model_param$num_trees) ==
      actual_rounds * if (summary$task == "multiclass") length(saved$class_levels) else 1L,
    "Native tree count is inconsistent with the boosted rounds and task.")
  record$native_verified_boosting_rounds <- actual_rounds
  record$native_verified_tree_count <- as.integer(learner$gradient_booster$gbtree_model_param$num_trees)
  record$native_verified_encoded_predictors <- length(native_names)
  record$native_feature_names <- native_names
  record$native_configuration <- configuration
  record$native_configuration_api <- "xgboost::xgb.config"
  record$native_feature_api <- "xgboost::getinfo(model, 'feature_name')"
  record$native_round_api <- "xgboost::xgb.get.num.boosted.rounds"
  record$training_rows_metadata <- list(declared = declared$n_training, summary = summary$training_rows,
    native_training_rows_field = summary$native_training_rows, final_trace = final_lines,
    source = "Partition manifest, summary and original NATIVE_FINAL trace only; not retained by the native booster.")
  record$training_rows_native_verified <- FALSE
  record$partitions_sha256 <- process$partitions_sha256
  record$training_partition_sha256_metadata <- declared$files$training.rds$sha256
  record$native_threads <- saved$threads
  record$backend_version <- as.character(packageVersion("xgboost"))
  record$saved_model_sha256 <- summary$saved_model_sha256
  record$status <- "ok"
}, error = function(error) {
  record$error <<- conditionMessage(error)
})
record$verified_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
record$elapsed_seconds <- proc.time()[["elapsed"]] - started
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
tabular_json(record, output_path)
cat("Unscored XGBoost structural/provenance check:", record$status, "\n", output_path, "\n")
if (record$status != "ok") {
  cat(record$error, "\n", file = stderr())
  quit(status = 1L)
}
