arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 3L) {
  stop("Usage: Rscript inference-probe.R CACHE FOREST_RECORD_DIRECTORY OUTPUT.json", call. = FALSE)
}
pkgload::load_all(".", quiet = TRUE)
stopifnot(requireNamespace("ranger", quietly = TRUE))
stopifnot(!file.exists(arguments[[3L]]))
case <- readRDS(file.path(arguments[[1L]], "cases/yearprediction/development/training.rds"))
evaluation <- case$data[case$calibration_rows, , drop = FALSE]
features <- setdiff(names(evaluation), "y")
set.seed(80811L)
sampled <- evaluation[sort(sample.int(nrow(evaluation), 5000L)), features, drop = FALSE]
permuted <- lapply(seq_len(5L), function(index) {
  value <- sampled
  value[[1L]] <- value[[1L]][sample.int(nrow(value))]
  value
})
combined <- do.call(rbind, permuted)
results <- lapply(c("sqrt_node5", "third_node20", "extra_sqrt_node5"), function(id) {
  saved <- readRDS(file.path(arguments[[2L]], paste0(id, "_500trees.rds")))
  short <- readRDS(file.path(arguments[[2L]], paste0(id, "_128trees.rds")))
  native_prefix <- predict(saved$model, data = evaluation[features],
    num.trees = 128L, num.threads = 2L)$predictions
  stopifnot(identical(native_prefix, short$predictions))
  wrapper <- AutoXplainR:::new_autoxplain_fitted_model(
    "forest", "ranger", saved$model, "regression", features, saved$record$parameters,
    fit_details = list(threads = 2L), seed = 80711L)
  invisible(predict(wrapper, sampled[1:10, , drop = FALSE]))
  timings <- list()
  record <- function(label, repetition, rows, expression) {
    gc()
    started <- proc.time()[["elapsed"]]
    value <- force(expression)
    timings[[length(timings) + 1L]] <<- list(label = label, repetition = repetition,
      predicted_rows = rows, seconds = proc.time()[["elapsed"]] - started)
    value
  }
  for (repetition in seq_len(2L)) {
    for (count in if (repetition == 1L) c(1000L, 5000L, 10000L) else c(10000L, 5000L, 1000L)) {
      record("wrapper_prediction", repetition, count,
        predict(wrapper, evaluation[seq_len(count), features, drop = FALSE]))
    }
    modes <- if (repetition == 1L) c("five_separate", "one_batched") else c("one_batched", "five_separate")
    predictions <- list()
    for (mode in modes) {
      predictions[[mode]] <- record(mode, repetition, 25000L, if (mode == "one_batched") {
        predict(wrapper, combined)
      } else {
        unlist(lapply(permuted, function(data) predict(wrapper, data)), use.names = FALSE)
      })
    }
    stopifnot(identical(predictions[[1L]], predictions[[2L]]))
  }
  explainer <- explain_model(wrapper, evaluation, "y", task = "regression")
  record("full_fingerprint_including_prediction", 1L, nrow(evaluation),
    AutoXplainR:::current_explainer_fingerprint(explainer))
  importance <- record("importance_3_features_3_repeats", 1L, 65000L,
    calculate_permutation_importance(explainer, metric = "rmse",
      features = features[1:3], n_repeats = 3L, max_rows = 5000L, seed = 80811L))
  stopifnot(nrow(importance) == 3L)
  cat(id, "inference probe complete.\n")
  list(id = id, native_trees = saved$model$num.trees, native_training_rows = saved$model$num.samples,
    prediction_threads = 2L, full_evaluation_rows = nrow(evaluation),
    saved_128_and_500_prefix_predictions_identical = TRUE,
    batched_and_separate_prediction_arrays_identical = TRUE,
    timings = timings)
})
script_path <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())])
source_paths <- c("R/fitted_model.R", "R/explainer.R", "R/evidence_contract.R",
  "R/permutation_importance.R", "R/dashboard_generation.R", "R/report_preparation.R", "R/audit.R")
manifest <- list(generated_at = format(Sys.time(), tz = "UTC"),
  runner_sha256 = digest::digest(file = script_path, algo = "sha256"),
  source_sha256 = stats::setNames(lapply(source_paths, function(path) {
    digest::digest(file = path, algo = "sha256")
  }), source_paths),
  ranger_version = as.character(packageVersion("ranger")), R_version = R.version.string,
  records = results,
  scope = paste("Year development calibration predictors and targets only. No fitting.",
    "Timings include the package model prediction wrapper at two native threads and shared-host noise.",
    "The three-feature importance probe makes nine 5000-row perturbation predictions",
    "and two 10000-row predictions for baseline and identity, totaling 65000 rows."))
jsonlite::write_json(manifest, arguments[[3L]], pretty = TRUE, auto_unbox = TRUE, digits = 16)
