arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 3L) {
  stop("Usage: Rscript precision-probe.R CACHE FOREST_RECORD_DIRECTORY OUTPUT_DIRECTORY", call. = FALSE)
}
destination <- arguments[[3L]]
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
snapshot <- file.path(destination, "source")
stopifnot(!dir.exists(snapshot))
dir.create(file.path(snapshot, "R"), recursive = TRUE)
stopifnot(all(file.copy(c("DESCRIPTION", "NAMESPACE"), snapshot)),
  all(file.copy(list.files("R", full.names = TRUE), file.path(snapshot, "R"))))
source_files <- list.files(file.path(snapshot, "R"), full.names = TRUE)
source_hashes <- stats::setNames(lapply(source_files, function(path) {
  digest::digest(file = path, algo = "sha256")
}), file.path("R", basename(source_files)))
pkgload::load_all(snapshot, quiet = TRUE, helpers = FALSE)
stopifnot(requireNamespace("ranger", quietly = TRUE))
partition <- file.path(arguments[[1L]], "cases/yearprediction/development")
feature_path <- file.path(partition, "evaluation-features.rds")
outcome_path <- file.path(partition, "evaluation-targets.rds")
evaluation <- readRDS(feature_path)
y <- readRDS(outcome_path)
data <- evaluation$data
data$y <- y
model_path <- file.path(arguments[[2L]], "third_node20_500trees.rds")
saved <- readRDS(model_path)
model <- AutoXplainR:::new_autoxplain_fitted_model(
  "forest", "ranger", saved$model, "regression", setdiff(names(data), "y"),
  saved$record$parameters, fit_details = list(threads = 2L), seed = 80711L)
explainer <- AutoXplainR::explain_model(model, data, "y", task = "regression")
objects <- list()
timings <- list()
for (rows in c(1000L, 5000L)) {
  cat("Starting importance on", rows, "rows at", format(Sys.time(), tz = "UTC"), "\n")
  started <- proc.time()[["elapsed"]]
  objects[[as.character(rows)]] <- AutoXplainR::calculate_permutation_importance(
    explainer, metric = "rmse", n_repeats = 5L, seed = 80841L,
    max_rows = rows, sample_seed = 80831L)
  timings[[as.character(rows)]] <- proc.time()[["elapsed"]] - started
  saveRDS(objects[[as.character(rows)]], file.path(destination, paste0("importance-", rows, ".rds")))
  cat("Finished", rows, "rows in", timings[[as.character(rows)]], "seconds.\n")
}
small_rows <- attr(objects[["1000"]], "sampling")$row_indices
large_rows <- attr(objects[["5000"]], "sampling")$row_indices
stopifnot(all(small_rows %in% large_rows))
large <- objects[["5000"]]
small <- objects[["1000"]][match(large$feature, objects[["1000"]]$feature), ]
comparison <- data.frame(feature = large$feature,
  importance_5000 = large$importance, importance_1000 = small$importance,
  shuffle_se_5000 = large$std_error, shuffle_se_1000 = small$std_error,
  lower_5000 = large$conf_low, upper_5000 = large$conf_high,
  lower_1000 = small$conf_low, upper_1000 = small$conf_high,
  rank_5000 = rank(-large$importance, ties.method = "min"),
  rank_1000 = rank(-small$importance, ties.method = "min"),
  sign_5000 = sign(large$importance), sign_1000 = sign(small$importance))
comparison <- comparison[order(comparison$rank_5000), ]
write.csv(comparison, file.path(destination, "importance-comparison.csv"), row.names = FALSE)
top_overlap <- function(k) {
  first <- head(large$feature[order(-large$importance)], k)
  second <- head(small$feature[order(-small$importance)], k)
  list(k = k, common = intersect(first, second), only_5000 = setdiff(first, second),
    only_1000 = setdiff(second, first))
}
script_path <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())])
summary <- list(phase = "development diagnostic, not acceptance",
  generated_at = format(Sys.time(), tz = "UTC"), rows_fitted = saved$model$num.samples,
  trees = saved$model$num.trees, assessment_rows = nrow(data), predictors = length(model$features),
  threads = 2L, repeats = 5L, sample_seed = 80831L, permutation_seed = 80841L,
  samples_nested = TRUE, timings_seconds = timings, overlap = list(top_overlap(8L), top_overlap(16L)),
  source_sha256 = source_hashes, runner_sha256 = digest::digest(file = script_path, algo = "sha256"),
  model_sha256 = digest::digest(file = model_path, algo = "sha256"),
  feature_data_sha256 = digest::digest(file = feature_path, algo = "sha256"),
  outcome_data_sha256 = digest::digest(file = outcome_path, algo = "sha256"),
  R_version = R.version.string, ranger_version = as.character(packageVersion("ranger")),
  scope = "One development forest and one nested sample pair. Shuffle intervals omit row-sampling uncertainty. No default change follows automatically.")
jsonlite::write_json(summary, file.path(destination, "summary.json"), pretty = TRUE, auto_unbox = TRUE, digits = 16)
