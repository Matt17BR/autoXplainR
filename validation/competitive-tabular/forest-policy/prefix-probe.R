# Written before this diagnostic was run. Uses training-only calibration rows.
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 3L) {
  stop("Usage: Rscript prefix-probe.R CACHE FOREST_RECORD_DIRECTORY OUTPUT_DIRECTORY", call. = FALSE)
}
stopifnot(requireNamespace("ranger", quietly = TRUE))
source_path <- file.path(arguments[[1L]], "cases/yearprediction/development/training.rds")
case <- readRDS(source_path)
source_hash <- digest::digest(file = source_path, algo = "sha256")
destination <- arguments[[3L]]
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
stopifnot(!file.exists(file.path(destination, "summary.json")))
ids <- c("sqrt_node5_500trees", "third_node20_500trees", "extra_sqrt_node5_500trees")
results <- lapply(ids, function(id) {
  saved <- readRDS(file.path(arguments[[2L]], paste0(id, ".rds")))
  stopifnot(saved$model$num.trees == 500L, identical(source_hash, saved$record$source_sha256),
    identical(saved$validation_rows, case$calibration_rows))
  y <- case$data$y[saved$validation_rows]
  x <- case$data[saved$validation_rows, setdiff(names(case$data), "y"), drop = FALSE]
  tree_prediction <- predict(saved$model, data = x, predict.all = TRUE,
    num.trees = 500L, num.threads = 2L)$predictions
  stopifnot(identical(dim(tree_prediction), c(length(y), 500L)))
  full <- rowMeans(tree_prediction)
  stopifnot(max(abs(full - saved$predictions)) < 1e-10)
  full_squared_error <- (y - full)^2
  set.seed(80781L)
  bootstrap_rows <- replicate(300L, sample.int(length(y), length(y), replace = TRUE))
  rows <- lapply(c(128L, 256L, 500L), function(count) {
    prefix <- rowMeans(tree_prediction[, seq_len(count), drop = FALSE])
    native <- predict(saved$model, data = x, num.trees = count, num.threads = 2L)$predictions
    aggregation_delta <- max(abs(prefix - native))
    stopifnot(aggregation_delta < 1e-10)
    squared_error <- (y - prefix)^2
    delta <- sqrt(mean(squared_error)) - sqrt(mean(full_squared_error))
    paired_bootstrap <- apply(bootstrap_rows, 2L, function(index) {
      sqrt(mean(squared_error[index])) - sqrt(mean(full_squared_error[index]))
    })
    set.seed(80791L)
    subset_scores <- replicate(30L, {
      trees <- sample.int(500L, count, replace = FALSE)
      sqrt(mean((y - rowMeans(tree_prediction[, trees, drop = FALSE]))^2))
    })
    list(trees = count, rmse = sqrt(mean(squared_error)),
      rmse_difference_from_500 = delta,
      paired_row_bootstrap_difference_quantiles = stats::setNames(
        as.list(as.numeric(quantile(paired_bootstrap, c(.025, .5, .975)))), c("q025", "median", "q975")),
      within_saved_forest_subset_rmse_quantiles = stats::setNames(
        as.list(as.numeric(quantile(subset_scores, c(0, .5, 1)))), c("minimum", "median", "maximum")),
      independent_aggregation_maximum_difference = aggregation_delta)
  })
  saveRDS(list(id = id, tree_predictions = tree_prediction,
    validation_rows = saved$validation_rows, rows = rows),
    file.path(destination, paste0(id, "-prefix.rds")), compress = FALSE)
  list(id = id, parameters = saved$record$parameters, rows_fitted = length(saved$training_rows),
    rows_scored = length(y), tree_prediction_bytes = as.numeric(object.size(tree_prediction)), scores = rows)
})
script_path <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())])
output <- list(phase = "development calibration only", generated_at = format(Sys.time(), tz = "UTC"),
  source_sha256 = source_hash, runner_sha256 = digest::digest(file = script_path, algo = "sha256"),
  ranger_version = as.character(packageVersion("ranger")), R_version = R.version.string,
  prediction_threads = 2L, bootstrap_seed = 80781L, bootstrap_repeats = 300L,
  tree_subset_seed = 80791L, tree_subset_repeats = 30L, records = results,
  scope = paste("Native prefix predictions and independent per-tree averages of three saved 500-tree forests.",
    "No model was fitted. Row bootstrap varies calibration rows, not training data.",
    "Tree subsets overlap and measure variation inside each saved forest, not independent fitting seeds.",
    "A 256-tree search fit can have a different parameter-derived seed; this is a fidelity diagnostic, not proof of identity."))
jsonlite::write_json(output, file.path(destination, "summary.json"), pretty = TRUE, auto_unbox = TRUE, digits = 16)
cat("Verified native 128/256/500-tree aggregation and recorded calibration fidelity for", length(ids), "forests.\n")
