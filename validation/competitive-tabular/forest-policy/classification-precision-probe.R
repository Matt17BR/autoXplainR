# Run from the repository root. All target paths explicitly select development.
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 4L) {
  stop(paste("Usage: Rscript classification-precision-probe.R CACHE",
    "NATIVE_RUN_DIRECTORY FROZEN_SOURCE OUTPUT_DIRECTORY"), call. = FALSE)
}
destination <- arguments[[4L]]
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
snapshot <- file.path(destination, "source")
stopifnot(!dir.exists(snapshot))
dir.create(file.path(snapshot, "R"), recursive = TRUE)
stopifnot(all(file.copy(file.path(arguments[[3L]], c("DESCRIPTION", "NAMESPACE")), snapshot)),
  all(file.copy(list.files(file.path(arguments[[3L]], "R"), full.names = TRUE), file.path(snapshot, "R"))))
common_path <- file.path(destination, "common.R")
stopifnot(file.copy("validation/competitive-tabular/common.R", common_path))
source(common_path)
pkgload::load_all(snapshot, quiet = TRUE, helpers = FALSE)
source_files <- c(file.path(snapshot, c("DESCRIPTION", "NAMESPACE")),
  list.files(file.path(snapshot, "R"), full.names = TRUE))
source_hashes <- stats::setNames(lapply(source_files, function(path) {
  digest::digest(file = path, algo = "sha256")
}), sub(paste0("^", snapshot, "/"), "", source_files))
stopifnot(requireNamespace("ranger", quietly = TRUE))
script_path <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())])
protocol_path <- "validation/competitive-tabular/forest-policy/classification-precision-protocol.md"
file.copy(protocol_path, file.path(destination, "protocol.md"))
run_case <- function(id) {
  case_output <- file.path(destination, id)
  dir.create(case_output)
  partition <- file.path(arguments[[1L]], "cases", id, "development")
  feature_path <- file.path(partition, "evaluation-features.rds")
  outcome_path <- file.path(partition, "evaluation-targets.rds")
  evaluation <- readRDS(feature_path)
  truth <- readRDS(outcome_path)
  model_path <- file.path(arguments[[2L]], id, "ranger/model.rds")
  saved <- readRDS(model_path)
  stopifnot(saved$model$num.trees == 500L, identical(saved$class_levels, levels(truth)))
  data <- tabular_bake(evaluation$data, saved$blueprint, matrix = FALSE)
  features <- names(data)
  data$y <- truth
  parameters <- list(num.trees = saved$model$num.trees, mtry = saved$model$mtry,
    min.node.size = saved$model$min.node.size)
  model <- AutoXplainR:::new_autoxplain_fitted_model("forest", "ranger", saved$model,
    saved$task, features, parameters, class_levels = saved$class_levels,
    fit_details = list(threads = 2L), seed = 80711L)
  explainer <- AutoXplainR::explain_model(model, data, "y", task = saved$task,
    positive = if (saved$task == "binary") "yes" else NULL)
  objects <- list()
  timings <- list()
  prediction_timings <- list()
  for (rows in c(1000L, 5000L)) {
    key <- as.character(rows)
    cat(id, "starting", rows, "rows at", format(Sys.time(), tz = "UTC"), "\n")
    started <- proc.time()[["elapsed"]]
    objects[[key]] <- AutoXplainR::calculate_permutation_importance(explainer,
      metric = "logloss", n_repeats = 5L, seed = 80841L,
      max_rows = rows, sample_seed = 80831L)
    timings[[key]] <- proc.time()[["elapsed"]] - started
    saveRDS(objects[[key]], file.path(case_output, paste0("importance-", rows, ".rds")))
    selected <- attr(objects[[key]], "sampling")$row_indices
    prediction_timings[[key]] <- lapply(seq_len(3L), function(repeat_index) {
      started <- proc.time()[["elapsed"]]
      prediction <- predict(explainer, data[selected, features, drop = FALSE])
      elapsed <- proc.time()[["elapsed"]] - started
      list(seconds = elapsed, prediction_sha256 = digest::digest(prediction, algo = "sha256"))
    })
    cat(id, "finished", rows, "rows in", timings[[key]], "seconds.\n")
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
  write.csv(comparison, file.path(case_output, "importance-comparison.csv"), row.names = FALSE)
  large_order <- order(-large$importance)
  small_order <- order(-small$importance)
  top_overlap <- function(k) {
    first <- head(large$feature[large_order], k)
    second <- head(small$feature[small_order], k)
    list(k = k, count = length(intersect(first, second)), common = intersect(first, second),
      only_5000 = setdiff(first, second), only_1000 = setdiff(second, first))
  }
  important <- head(large_order, 8L)
  important <- important[large$conf_low[important] > 0]
  selected_1000 <- head(small$feature[small_order], 16L)
  missed_positive <- large$feature[important][!large$feature[important] %in% selected_1000 |
    small$importance[important] <= 0]
  class_counts <- lapply(list(full = seq_along(truth), rows_1000 = small_rows,
    rows_5000 = large_rows), function(rows) as.list(table(truth[rows])))
  class_missing <- names(class_counts$rows_5000)[unlist(class_counts$rows_5000) >= 20L &
    unlist(class_counts$rows_1000) == 0L]
  overlap <- list(top_overlap(8L), top_overlap(16L))
  gate <- list(top8_at_least_7 = overlap[[1L]]$count >= 7L,
    top16_at_least_12 = overlap[[2L]]$count >= 12L,
    confident_top8_retained_positive_in_top16 = !length(missed_positive),
    represented_classes_retained = !length(class_missing),
    missed_positive_features = missed_positive, missing_classes = class_missing)
  summary <- list(id = id, phase = "development screening diagnostic, not acceptance",
    rows_fitted = saved$model$num.samples, trees = saved$model$num.trees,
    assessment_rows = nrow(data), predictors = length(features),
    original_fit_threads = saved$threads, prediction_threads = 2L, repeats = 5L,
    sample_seed = 80831L, permutation_seed = 80841L, samples_nested = TRUE,
    timings_seconds = timings, repeated_prediction_timings = prediction_timings,
    class_counts = class_counts, overlap = overlap, diagnostic_gate = gate,
    repeat_score_dimensions = lapply(objects, function(object) dim(attr(object, "repeat_scores"))),
    repeat_score_sha256 = lapply(objects, function(object) digest::digest(attr(object, "repeat_scores"), algo = "sha256")),
    model_sha256 = digest::digest(file = model_path, algo = "sha256"),
    feature_data_sha256 = digest::digest(file = feature_path, algo = "sha256"),
    outcome_data_sha256 = digest::digest(file = outcome_path, algo = "sha256"))
  jsonlite::write_json(summary, file.path(case_output, "summary.json"),
    pretty = TRUE, auto_unbox = TRUE, digits = 16)
  summary
}
records <- list()
for (id in c("bank", "covertype")) {
  records[[id]] <- run_case(id)
  invisible(gc())
}
output <- list(generated_at = format(Sys.time(), tz = "UTC"),
  source_sha256 = source_hashes, runner_sha256 = digest::digest(file = script_path, algo = "sha256"),
  preprocessing_source_sha256 = digest::digest(file = common_path, algo = "sha256"),
  protocol_sha256 = digest::digest(file = file.path(destination, "protocol.md"), algo = "sha256"),
  R_version = R.version.string, ranger_version = as.character(packageVersion("ranger")), records = records,
  scope = paste("One nested sample pair per saved development forest. Detailed importance remains at 5000 rows.",
    "Shuffle intervals exclude row-sampling uncertainty. Passing checks cannot establish stable feature selection.",
    "No defaults are changed by this diagnostic; acceptance targets remain unopened."))
jsonlite::write_json(output, file.path(destination, "summary.json"), pretty = TRUE, auto_unbox = TRUE, digits = 16)
