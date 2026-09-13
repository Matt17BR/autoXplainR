# Usage: Rscript --vanilla check-unscored-ranger.R FIT_DIRECTORY [NEW_OUTPUT_JSON] [PARTITIONS_JSON]
# Inspect saved original-protocol ranger fits; never open partition data or score a model.
arguments <- commandArgs(TRUE)
stopifnot(length(arguments) %in% 1:3)
script <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]]))
source(file.path(dirname(script), "common.R"))
fit_directory <- normalizePath(arguments[[1L]], mustWork = TRUE)
cache <- path.expand(Sys.getenv("AXR_TABULAR_DIR", "~/.cache/autoxplain-tabular-0.8.0"))
output_path <- if (length(arguments) >= 2L) path.expand(arguments[[2L]]) else {
  file.path(cache, "reproducibility", paste0("unscored-ranger-audit-",
    format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC"), "-", Sys.getpid()), "verification.json")
}
# Resolve existing ancestors before creating directories, including symlink aliases.
new_path <- function(path) {
  if (!startsWith(path, "/")) stop("Use an absolute output JSON path.")
  suffix <- character()
  while (!file.exists(path)) {
    suffix <- c(basename(path), suffix)
    path <- dirname(path)
  }
  path <- normalizePath(path, mustWork = TRUE)
  for (part in suffix) path <- if (part == "..") dirname(path) else if (part == ".") path else file.path(path, part)
  path
}
output_path <- new_path(output_path)
if (file.exists(output_path) || identical(output_path, fit_directory) ||
    startsWith(output_path, paste0(fit_directory, "/"))) {
  stop("Write verification to a new output outside the original fit directory.")
}
partitions_path <- if (length(arguments) == 3L) normalizePath(arguments[[3L]], mustWork = TRUE) else {
  file.path(cache, "partitions.json")
}
started <- proc.time()[["elapsed"]]
require_check <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}
equal_value <- function(actual, expected) isTRUE(all.equal(actual, expected, tolerance = 0))
record <- list(status = "failed", fit_directory = fit_directory,
  process_id = Sys.getpid(), fresh_session = TRUE,
  evaluation_files_opened = FALSE, training_partition_opened = FALSE,
  evaluation_replay_performed = FALSE,
  scope = paste("Fresh-session saved ranger structure, hashes and original training-calibration provenance only.",
    "No training or evaluation partition files are opened and no predictions are generated.",
    "Calibration losses are saved metadata: labels are not loaded to recompute them.",
    "Threads, seed, sample fraction and factor handling are checked against literal saved fit-call controls;",
    "they are provenance, not independently reconstructed execution history or acceptance quality evidence."),
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
  record$variant <- "ranger"
  record$fit_process_sha256 <- tabular_hash(process_path)
  record$fit_summary_sha256 <- tabular_hash(summary_path)
  require_check(identical(process$process_status, "ok") && process$exit_code == 0L &&
    identical(summary$status, "ok") && identical(process$stage, "fit-only") &&
    identical(summary$stage, "fit-only") && identical(summary$variant, "ranger") &&
    identical(process$variant, "ranger") && identical(process$case, summary$case) &&
    identical(process$phase, summary$phase) && identical(summary$primary, "ranger"),
    "The source must be a successful matching ranger fit-only process and summary.")
  require_check(all(vapply(c("native_reference_protocol", "native_reference", "native_reference_plan_sha256"),
    function(key) is.null(summary[[key]]) && is.null(process[[key]]), logical(1))) &&
    !file.exists(file.path(fit_directory, "native-reference-plan.json")),
    "This checker supports the original calibrated ranger protocol only.")
  files <- list.files(fit_directory, all.files = TRUE, no.. = TRUE)
  forbidden_files <- grep("(^predictions\\.rds$|^evaluation[-.]|^held[-_]?out[-_.]|^metrics\\.|^score[-_.])", files, value = TRUE)
  require_check(identical(summary$evaluation_files_opened, FALSE) &&
    !any(c("metrics", "predictions_sha256", "fit_source") %in% names(summary)) &&
    length(forbidden_files) == 0L,
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
    summary$native_verified_training_rows == declared$n_training &&
    summary$predictors == declared$predictors && process$threads == summary$native_threads &&
    process$threads %in% c(1L, 4L), "Training row, predictor or thread metadata differs.")
  limit <- if (identical(summary$phase, "acceptance")) 7200 else 1200
  require_check(summary$phase %in% c("development", "acceptance") &&
    process$wall_limit_seconds == limit && process$combined_wall_limit_seconds == limit &&
    process$address_space_limit_bytes == 24 * 1024^3 &&
    is.finite(process$process_elapsed_seconds) && process$process_elapsed_seconds > 0 &&
    process$process_elapsed_seconds <= limit && is.finite(process$peak_rss_kib) &&
    process$peak_rss_kib > 0 && process$peak_rss_kib * 1024 <= process$address_space_limit_bytes,
    "The fit does not satisfy the original process resource limits.")
  command <- unlist(process$command, use.names = FALSE)
  script_index <- which(basename(command) == "run-one.R")
  require_check(length(script_index) == 1L && script_index > 2L &&
    identical(command[script_index - 1L], "--vanilla") &&
    identical(basename(command[script_index - 2L]), "Rscript"), "The saved Rscript command is inconsistent.")
  fit_arguments <- command[seq.int(script_index + 1L, length(command))]
  require_check(length(fit_arguments) %in% c(10L, 11L, 12L) &&
    identical(fit_arguments[1:3], c(summary$case, "ranger", summary$phase)) &&
    identical(fit_arguments[[6L]], as.character(process$threads)) &&
    identical(fit_arguments[[8L]], summary$request) && identical(summary$request, process$request) &&
    identical(fit_arguments[[9L]], "fit-only") && identical(fit_arguments[[10L]], "") &&
    (length(fit_arguments) == 10L || identical(fit_arguments[[11L]], "")),
    "The saved command case, phase, threads, stage or reference plan differs.")
  require_check(length(fit_arguments) < 12L ||
    (is.character(process$cohort) && length(process$cohort) == 1L && nzchar(process$cohort) &&
      identical(fit_arguments[[12L]], process$cohort)),
    "The saved command cohort differs from the completed process.")
  record$command_cohort_binding_present <- length(fit_arguments) == 12L
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
  final_lines <- grep("^NATIVE_FINAL ", trace, value = TRUE)
  require_check(length(final_lines) == 1L &&
    identical(trimws(final_lines), paste("NATIVE_FINAL ranger fit", declared$n_training, "rows")),
    "The original full-training NATIVE_FINAL trace is absent or inconsistent.")
  # The original runner deduplicates sqrt(p) and p/3 when they coincide.
  mtry_settings <- unique(pmax(1L, c(floor(sqrt(declared$predictors)), floor(declared$predictors / 3))))
  calibration_rows <- floor(.2 * declared$n_training)
  calibration_lines <- grep("^NATIVE_CALIBRATION ", trace, value = TRUE)
  expected_lines <- vapply(seq_along(mtry_settings), function(index) paste("NATIVE_CALIBRATION ranger", index,
    "fit", declared$n_training - calibration_rows, "assess", calibration_rows, "rows"), character(1))
  require_check(identical(trimws(calibration_lines), expected_lines) &&
    !any(grepl("^NATIVE_FIXED_REFERENCE ", trace)),
    "The original training-calibration row traces are absent or inconsistent.")
  record$process_log_sha256 <- tabular_hash(trace_path)
  calibrations <- summary$calibration
  require_check(length(calibrations) == length(mtry_settings),
    "The original sqrt(p)/p3 calibration setting count differs.")
  calibration_path <- file.path(fit_directory, "calibration.json")
  require_check(identical(jsonlite::read_json(calibration_path), calibrations),
    "Calibration checkpoint differs from the completed summary.")
  record$calibration_checkpoint_sha256 <- tabular_hash(calibration_path)
  expected_prediction_files <- paste0("calibration-", seq_along(mtry_settings), "-predictions.rds")
  require_check(setequal(grep("^calibration-.*-predictions\\.rds$", files, value = TRUE), expected_prediction_files),
    "The calibration prediction file inventory differs.")
  calibration_evidence <- vector("list", length(mtry_settings))
  calibration_source_rows <- NULL
  class_levels <- unlist(declared$class_levels, use.names = FALSE)
  for (index in seq_along(mtry_settings)) {
    calibration <- calibrations[[index]]
    expected <- list(mtry = mtry_settings[[index]], num.trees = 500L,
      min.node.size = 5L, sample.fraction = .8,
      splitrule = if (summary$task == "regression") "variance" else "gini")
    require_check(setequal(names(calibration$parameters), names(expected)) &&
      all(vapply(names(expected), function(key) equal_value(calibration$parameters[[key]], expected[[key]]), logical(1))) &&
      is.null(calibration$selected_rounds),
      paste("Calibration parameters differ from the original controls for setting", index))
    loss <- tabular_primary(calibration$calibration_metrics, summary$task)
    require_check(length(loss) == 1L && is.finite(loss) && loss >= 0 &&
      is.finite(calibration$elapsed_seconds) && calibration$elapsed_seconds >= 0,
      paste("Invalid saved calibration primary loss or duration for setting", index))
    prediction_path <- file.path(fit_directory, expected_prediction_files[[index]])
    require_check(identical(calibration$calibration_predictions_sha256, tabular_hash(prediction_path)),
      paste("Training-calibration prediction hash changed for setting", index))
    prediction <- readRDS(prediction_path)
    require_check(is.list(prediction) && setequal(names(prediction), c("predictions", "source_rows")) &&
      is.numeric(prediction$source_rows) && length(prediction$source_rows) == calibration_rows &&
      all(is.finite(prediction$source_rows)) && all(prediction$source_rows > 0) &&
      all(prediction$source_rows == as.integer(prediction$source_rows)) && !anyDuplicated(prediction$source_rows),
      paste("Invalid saved training-calibration source rows for setting", index))
    if (index == 1L) calibration_source_rows <- prediction$source_rows
    require_check(identical(prediction$source_rows, calibration_source_rows),
      "Calibration settings do not use identical saved source-row order.")
    values <- prediction$predictions
    require_check(is.numeric(values) && all(is.finite(values)),
      paste("Invalid saved training-calibration predictions for setting", index))
    if (summary$task == "regression") {
      require_check(is.null(dim(values)) && length(values) == calibration_rows,
        paste("Regression calibration prediction dimensions differ for setting", index))
    } else {
      require_check(is.matrix(values) && nrow(values) == calibration_rows &&
        ncol(values) == length(class_levels) && identical(colnames(values), class_levels) &&
        all(values >= 0 & values <= 1) && max(abs(rowSums(values) - 1)) < 1e-6,
        paste("Classification calibration probabilities or class names differ for setting", index))
    }
    calibration_evidence[[index]] <- list(parameters = calibration$parameters,
      calibration_primary_loss_metadata = loss, calibration_rows = calibration_rows,
      calibration_predictions_sha256 = calibration$calibration_predictions_sha256)
  }
  chosen <- which.min(vapply(calibrations, function(item) tabular_primary(item$calibration_metrics, summary$task), numeric(1)))
  require_check(summary$selected_configuration == chosen &&
    identical(summary$selected_parameters, calibrations[[chosen]]),
    "The selected configuration is not the minimum saved training-calibration primary loss.")
  record$calibration <- calibration_evidence
  record$calibration_metrics_recomputed <- FALSE
  record$calibration_source_rows_verified_against_training_partition <- FALSE
  record$selected_configuration <- chosen
  record$selection_metric <- if (summary$task == "regression") "rmse" else "log_loss"
  require_check(identical(tabular_hash(model_path), summary$saved_model_sha256) &&
    file.info(model_path)$size == summary$saved_model_bytes, "Saved native model hash or size changed.")
  saved <- readRDS(model_path)
  require_check(identical(saved$family, "ranger") && inherits(saved$model, "ranger") &&
    identical(saved$task, summary$task) && saved$threads == process$threads &&
    identical(saved$class_levels, class_levels) &&
    identical(as.character(packageVersion("ranger")), summary$backend_versions$ranger),
    "Saved model family, task, class levels, threads or backend version differs.")
  model <- saved$model
  parameters <- calibrations[[chosen]]$parameters
  require_check(model$num.samples == declared$n_training &&
    model$num.samples == summary$native_verified_training_rows,
    "Native saved forest training row count differs from the full training pool.")
  require_check(model$num.trees == 500L && model$forest$num.trees == 500L &&
    (is.null(summary$native_verified_tree_count) || model$num.trees == summary$native_verified_tree_count) &&
    all(lengths(model$forest[c("child.nodeIDs", "split.varIDs", "split.values")]) == 500L),
    "Native saved forest tree count differs from the original 500-tree setting.")
  require_check(model$mtry == parameters$mtry && model$min.node.size == parameters$min.node.size &&
    identical(model$splitrule, parameters$splitrule),
    "Native saved forest mtry, minimum node size or split rule differs.")
  require_check(is.list(saved$blueprint) && length(saved$blueprint) == summary$predictors &&
    !is.null(names(saved$blueprint)) && !anyDuplicated(names(saved$blueprint)) &&
    all(nzchar(names(saved$blueprint))) &&
    all(vapply(saved$blueprint, function(levels) is.null(levels) ||
      (is.character(levels) && !anyNA(levels) && !anyDuplicated(levels) &&
        length(levels) >= 2L && identical(tail(levels, 2L), c("__new__", "__missing__"))), logical(1))) &&
    model$num.independent.variables == summary$predictors &&
    identical(model$forest$independent.variable.names, names(saved$blueprint)) &&
    length(model$forest$is.ordered) == summary$predictors && all(model$forest$is.ordered),
    "Native predictor names/count or saved feature blueprint is inconsistent.")
  expected_treetype <- if (summary$task == "regression") "Regression" else "Probability estimation"
  require_check(identical(model$treetype, expected_treetype) && identical(model$forest$treetype, expected_treetype) &&
    (summary$task == "regression" || identical(model$forest$levels, class_levels)) &&
    identical(model$replace, TRUE), "Native saved forest task, classes or replacement control differs.")
  # Read literal controls only; never evaluate the stored call or inspect x/y.
  require_check(is.call(model$call), "Native saved forest fit-call metadata is absent.")
  fit_call <- as.list(model$call)
  call_expected <- c(parameters, list(probability = summary$task != "regression",
    num.threads = process$threads, seed = declared$fit_seed,
    respect.unordered.factors = "order", write.forest = TRUE))
  for (key in names(call_expected)) {
    require_check(equal_value(fit_call[[key]], call_expected[[key]]),
      paste("Saved literal forest fit-call control differs:", key))
  }
  record$native_verified_training_rows <- model$num.samples
  record$training_rows_native_verified <- TRUE
  record$native_verified_tree_count <- model$num.trees
  record$native_tree_count_recorded_in_fit_summary <- !is.null(summary$native_verified_tree_count)
  record$native_verified_mtry <- model$mtry
  record$native_verified_min_node_size <- model$min.node.size
  record$native_verified_split_rule <- model$splitrule
  record$native_verified_predictors <- model$num.independent.variables
  record$native_predictor_names <- model$forest$independent.variable.names
  record$native_tree_type <- model$treetype
  record$saved_literal_fit_call_controls <- call_expected
  record$sample_fraction_evidence <- "Saved calibration parameters and literal final ranger fit-call metadata; no independent in-bag reconstruction."
  record$partitions_sha256 <- process$partitions_sha256
  record$training_partition_sha256_metadata <- declared$files$training.rds$sha256
  record$native_threads <- saved$threads
  record$backend_version <- as.character(packageVersion("ranger"))
  record$saved_model_sha256 <- summary$saved_model_sha256
  record$saved_model_bytes <- summary$saved_model_bytes
  record$status <- "ok"
}, error = function(error) {
  record$error <<- conditionMessage(error)
})
record$verified_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
record$elapsed_seconds <- proc.time()[["elapsed"]] - started
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
tabular_json(record, output_path)
cat("Unscored ranger structural/provenance check:", record$status, "\n", output_path, "\n")
if (record$status != "ok") {
  cat(record$error, "\n", file = stderr())
  quit(status = 1L)
}
