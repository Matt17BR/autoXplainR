arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 3L) {
  stop("Usage: Rscript replay.R CACHE RECORD_DIRECTORY OUTPUT.json", call. = FALSE)
}
stopifnot(requireNamespace("ranger", quietly = TRUE))
source_path <- file.path(arguments[[1L]], "cases/yearprediction/development/training.rds")
case <- readRDS(source_path)
source_hash <- digest::digest(file = source_path, algo = "sha256")
files <- list.files(arguments[[2L]], pattern = "\\.rds$", full.names = TRUE)
stopifnot(length(files) > 0L, !file.exists(arguments[[3L]]))
records <- lapply(files, function(path) {
  saved <- readRDS(path)
  stopifnot(identical(source_hash, saved$record$source_sha256))
  fitted <- saved$training_rows
  scored <- saved$validation_rows
  stopifnot(!length(intersect(fitted, scored)),
    all(scored %in% case$calibration_rows),
    !length(intersect(fitted, case$calibration_rows)),
    saved$model$num.samples == length(fitted))
  prediction <- predict(saved$model,
    data = case$data[scored, setdiff(names(case$data), "y"), drop = FALSE],
    num.threads = 2L)$predictions
  delta <- max(abs(prediction - saved$predictions))
  rmse <- sqrt(mean((case$data$y[scored] - prediction)^2))
  stopifnot(identical(prediction, saved$predictions),
    abs(rmse - saved$record$rmse) < 1e-12)
  list(id = saved$record$id, rows_fitted = length(fitted), rows_scored = length(scored),
    native_trees = saved$model$num.trees, native_predictors = saved$model$num.independent.variables,
    prediction_arrays_identical = identical(prediction, saved$predictions),
    maximum_prediction_difference = delta, replayed_rmse = rmse,
    saved_record_sha256 = digest::digest(file = path, algo = "sha256"),
    record_bytes = unname(file.info(path)$size))
})
names(records) <- vapply(records, `[[`, character(1), "id")
pair_names <- c("sqrt_node20_128trees", "sqrt_node20_128trees_no_oob")
oob <- NULL
if (all(pair_names %in% names(records))) {
  pair <- lapply(pair_names, function(id) readRDS(file.path(arguments[[2L]], paste0(id, ".rds"))))
  stopifnot(identical(pair[[1L]]$model$forest, pair[[2L]]$model$forest),
    identical(pair[[1L]]$predictions, pair[[2L]]$predictions),
    length(pair[[1L]]$model$predictions) == 40000L,
    length(pair[[2L]]$model$predictions) == 0L)
  oob <- list(forest_structures_identical = TRUE, prediction_arrays_identical = TRUE,
    oob_prediction_rows_enabled = 40000L, oob_prediction_rows_disabled = 0L,
    timing_conclusion = "This pair does not establish an elapsed-time improvement.")
}
output <- list(phase = "development calibration replay",
  generated_at = format(Sys.time(), tz = "UTC"),
  ranger_version = as.character(packageVersion("ranger")), R_version = R.version.string,
  source_sha256 = source_hash, threads = 2L, records = records, oob_pair = oob)
jsonlite::write_json(output, arguments[[3L]], pretty = TRUE, auto_unbox = TRUE, digits = 16)
cat(length(records), "saved forests reproduced every stored calibration prediction exactly.\n")
