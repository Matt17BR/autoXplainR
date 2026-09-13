arguments <- commandArgs(trailingOnly = TRUE)
stopifnot(length(arguments) == 5L)
id <- match.arg(arguments[[1L]], c("yearprediction", "covertype"))
stage <- match.arg(arguments[[2L]], c("prepare", "fit", "replay", "screen", "detail", "summarize"))
model_id <- arguments[[3L]]
root <- arguments[[4L]]
cache <- arguments[[5L]]
destination <- file.path(root, id)
source(file.path(root, "scripts/common.R"))
pkgload::load_all(file.path(root, "source"), quiet = TRUE, helpers = FALSE)
stage_directory <- file.path(destination, paste0(stage,
  if (stage %in% c("screen", "detail")) paste0("-", model_id) else ""))
write_summary <- function(value) tabular_json(value, file.path(stage_directory, "summary.json"))
model_path <- function(key) file.path(destination, paste0(key, ".rds"))
read_model <- function(key) readRDS(model_path(key))
timed <- function(code) {
  begin <- proc.time()[["elapsed"]]
  value <- force(code)
  list(value = value, seconds = proc.time()[["elapsed"]] - begin)
}
native_prediction <- function(model, data) {
  value <- predict(model$fit, data = data[model$features], num.threads = 4L)$predictions
  if (model$task == "binary") value <- value[, model$class_levels[[2L]]]
  value
}
classification_detail <- function(y, prediction, task) {
  if (task == "regression") return(NULL)
  probability <- as.matrix(prediction)[, levels(y), drop = FALSE]
  actual <- as.integer(y)
  chosen <- max.col(probability, ties.method = "first")
  true_probability <- probability[cbind(seq_along(y), actual)]
  counts <- table(actual = factor(actual, levels = seq_along(levels(y)), labels = levels(y)),
    predicted = factor(chosen, levels = seq_along(levels(y)), labels = levels(y)))
  list(confusion = unclass(counts), class_counts = as.list(table(y)),
    correct_by_class = as.list(setNames(diag(counts), levels(y))),
    zero_probabilities_by_class = as.list(colSums(probability == 0)),
    true_class_zeros = sum(true_probability == 0),
    true_class_zeros_by_class = as.list(tapply(true_probability == 0, y, sum)),
    unbounded_log_loss = if (any(true_probability == 0)) "Inf" else -mean(log(true_probability)))
}
model_summary <- function(model) {
  list(trees = model$fit$num.trees, native_training_rows = model$fit$num.samples,
    effective_predictors = length(model$features), parameters = model$parameters,
    fit_seed = model$seed, fit_evidence = attr(model, "autoxplain_tuning_fit"),
    oob_prediction_error = model$fit$prediction.error,
    nodes = sum(vapply(model$fit$forest$child.nodeIDs, function(tree) length(tree[[1L]]), integer(1))))
}

if (stage == "prepare") {
  source_directory <- file.path(cache, "runs/candidate-paired-4t-v4-recovery-20260913", id, "package")
  source_path <- file.path(source_directory, "model.rds")
  result <- readRDS(source_path)
  model <- result$models$forest_model
  evidence <- attr(model, "autoxplain_tuning_fit")
  training <- result$training_data
  evaluation <- result$test_data
  prepared_path <- file.path(cache, "cases", id, "development", "training.rds")
  prepared <- readRDS(prepared_path)
  stopifnot(nrow(training) == 50000L, nrow(evaluation) == 20000L,
    model$fit$num.samples == 50000L, model$fit$num.trees == 500L,
    evidence$threads == 4L, evidence$search_seed == 80711L,
    evidence$fit_seed == AutoXplainR:::stable_configuration_seed(80711L, "forest", evidence$effective_parameters),
    identical(evidence$requested_parameters, evidence$effective_parameters),
    identical(training$y, prepared$data$y),
    identical(names(result$models), c("main_model", "forest_model", "simple_baseline")))
  parameters <- evidence$effective_parameters
  parameters$num.trees <- 256L
  configuration <- AutoXplainR:::local_tuning_plan(
    1L, 50000L, length(result$features), result$task,
    if (result$task == "regression") 1L else nlevels(training$y),
    learners = "forest", seed = evidence$search_seed,
    custom_grids = list(forest = list(parameters)))
  configuration$configuration_id <- evidence$configuration_id
  configuration$threads <- 4L
  stopifnot(configuration$seed == AutoXplainR:::stable_configuration_seed(80711L, "forest", parameters))
  context <- list(task = result$task, training = training, evaluation = evaluation,
    features = result$features, raw_predictors = ncol(prepared$data) - 1L,
    removed_constant_features = result$provenance$constant_features_removed,
    configuration = configuration, sample_seed = 80711L, permutation_seed = 80711L,
    original_predictions = readRDS(file.path(source_directory, "predictions.rds"))$forest_model)
  saveRDS(context, file.path(destination, "context.rds"), compress = FALSE, version = 3L)
  saveRDS(model, model_path("forest500"), compress = FALSE, version = 3L)
  for (key in c("main_model", "simple_baseline")) {
    saveRDS(result$models[[key]], model_path(key), compress = FALSE, version = 3L)
  }
  paths <- c(source_model = source_path,
    source_process = file.path(source_directory, "process.json"),
    source_summary = file.path(source_directory, "summary.json"),
    source_predictions = file.path(source_directory, "predictions.rds"),
    training = prepared_path,
    evaluation_features = file.path(cache, "cases", id, "development", "evaluation-features.rds"),
    evaluation_targets = file.path(cache, "cases", id, "development", "evaluation-targets.rds"))
  write_summary(list(status = "ok", phase = "development", case = id,
    source_files = as.list(paths), source_sha256 = lapply(paths, tabular_hash),
    reference500 = model_summary(model), planned256 = configuration,
    raw_predictors = context$raw_predictors, effective_predictors = length(context$features),
    removed_constant_features = context$removed_constant_features,
    reference500_bytes = file.info(model_path("forest500"))$size,
    source_freeze_sha256 = tabular_hash(file.path(root, "freeze.json"))))
  quit(save = "no")
}

context <- readRDS(file.path(destination, "context.rds"))
if (stage == "fit") {
  capture <- timed(AutoXplainR:::fit_tuning_configuration(
    context$configuration, context$training, "y", context$task))
  model <- capture$value
  stopifnot(model$fit$num.samples == 50000L, model$fit$num.trees == 256L,
    model$fit_details$oob_computed,
    model$seed == context$configuration$seed)
  saveRDS(model, model_path("forest256"), compress = FALSE, version = 3L)
  prediction <- predict(model, context$evaluation)
  native <- native_prediction(model, context$evaluation)
  stopifnot(max(abs(prediction - native)) <= 1e-12)
  saveRDS(prediction, file.path(destination, "forest256-predictions.rds"), compress = FALSE, version = 3L)
  write_summary(list(status = "ok", phase = "development diagnostic", case = id,
    fit_elapsed_seconds = capture$seconds, model = model_summary(model),
    model_bytes = file.info(model_path("forest256"))$size,
    model_sha256 = tabular_hash(model_path("forest256")),
    metrics = tabular_metrics(context$evaluation$y, prediction, context$task),
    classification = classification_detail(context$evaluation$y, prediction, context$task),
    native_maximum_absolute_difference = max(abs(prediction - native))))
} else if (stage == "replay") {
  records <- list()
  rows <- AutoXplainR:::explanation_row_sample(nrow(context$evaluation), 5000L, 80711L)$row_indices
  for (key in c("forest500", "forest256")) {
    model <- read_model(key)
    expected <- if (key == "forest500") context$original_predictions else {
      readRDS(file.path(destination, "forest256-predictions.rds"))
    }
    prediction <- predict(model, context$evaluation)
    native <- native_prediction(model, context$evaluation)
    stopifnot(max(abs(prediction - expected)) <= 1e-12, max(abs(prediction - native)) <= 1e-12)
    timings <- lapply(seq_len(3L), function(i) {
      captured <- timed(predict(model, context$evaluation[rows, , drop = FALSE]))
      list(seconds = captured$seconds, prediction_sha256 = digest::digest(captured$value, algo = "sha256"))
    })
    saveRDS(prediction, file.path(destination, paste0(key, "-replayed-predictions.rds")), compress = FALSE, version = 3L)
    records[[key]] <- list(metrics = tabular_metrics(context$evaluation$y, prediction, context$task),
      classification = classification_detail(context$evaluation$y, prediction, context$task),
      cold_replay_maximum_absolute_difference = max(abs(prediction - expected)),
      native_maximum_absolute_difference = max(abs(prediction - native)),
      prediction_rows = nrow(context$evaluation), class_names = colnames(prediction),
      prediction_timings_5000 = timings, model_bytes = file.info(model_path(key))$size,
      native = model_summary(model))
  }
  saveRDS(rows, file.path(destination, "reference-rows.rds"))
  write_summary(list(status = "ok", records = records, sample_seed = 80711L,
    reference_rows_sha256 = digest::digest(rows, algo = "sha256"),
    sampled_class_counts = if (context$task != "regression") as.list(table(context$evaluation$y[rows])) else NULL))
} else if (stage %in% c("screen", "detail")) {
  keys <- c("main_model", "simple_baseline", "forest500", "forest256")
  stopifnot(model_id %in% keys)
  features <- NULL
  unions <- NULL
  if (stage == "detail") {
    stopifnot(model_id %in% c("forest500", "forest256"))
    screens <- setNames(lapply(keys, function(key) {
      readRDS(file.path(destination, paste0("screen-", key), "importance.rds"))
    }), keys)
    leading <- lapply(screens, function(item) head(item$feature[order(-item$importance)], 8L))
    unions <- lapply(c("forest500", "forest256"), function(key) {
      unique(unlist(leading[c("main_model", key, "simple_baseline")], use.names = FALSE))
    })
    names(unions) <- c("forest500", "forest256")
    features <- unique(unlist(unions, use.names = FALSE))
    borderline <- lapply(screens[c("forest500", "forest256")], function(item) {
      valid <- !item$feature %in% features & is.finite(item$std_error) & item$std_error > 0
      candidate <- item[valid, , drop = FALSE]
      head(candidate$feature[order(abs(candidate$importance / candidate$std_error), candidate$feature)], 2L)
    })
    features <- unique(c(features, unlist(borderline, use.names = FALSE)))
  }
  model <- read_model(model_id)
  explainer <- AutoXplainR::explain_model(model, context$evaluation, "y", task = context$task,
    label = model_id)
  captured <- timed(AutoXplainR::calculate_permutation_importance(explainer,
    metric = if (context$task == "regression") "rmse" else "logloss",
    n_repeats = if (stage == "screen") 5L else 20L,
    features = features, seed = if (stage == "screen") 80711L else 80712L,
    max_rows = 5000L, sample_seed = 80711L))
  importance <- captured$value
  rows <- attr(importance, "sampling")$row_indices
  expected_rows <- readRDS(file.path(destination, "reference-rows.rds"))
  stopifnot(identical(rows, expected_rows), length(rows) == 5000L)
  saveRDS(importance, file.path(stage_directory, "importance.rds"), compress = FALSE, version = 3L)
  write.csv(importance, file.path(stage_directory, "importance.csv"), row.names = FALSE)
  write_summary(list(status = "ok", model = model_id, stage = stage,
    elapsed_seconds = captured$seconds, rows = length(rows),
    repeats = if (stage == "screen") 5L else 20L,
    features = importance$feature, ordinary_report_unions = unions,
    borderline_inputs = if (stage == "detail") borderline else NULL,
    detailed_feature_order = features,
    reference_rows_sha256 = digest::digest(rows, algo = "sha256"),
    repeat_scores_sha256 = digest::digest(attr(importance, "repeat_scores"), algo = "sha256"),
    repeat_score_dimensions = dim(attr(importance, "repeat_scores"))))
} else if (stage == "summarize") {
  comparison <- function(kind) {
    large <- readRDS(file.path(destination, paste0(kind, "-forest500"), "importance.rds"))
    small <- readRDS(file.path(destination, paste0(kind, "-forest256"), "importance.rds"))
    small <- small[match(large$feature, small$feature), ]
    data.frame(feature = large$feature, importance500 = large$importance, importance256 = small$importance,
      difference = small$importance - large$importance,
      lower500 = large$conf_low, upper500 = large$conf_high,
      lower256 = small$conf_low, upper256 = small$conf_high,
      shuffle_se500 = large$std_error, shuffle_se256 = small$std_error,
      rank500 = rank(-large$importance, ties.method = "min"),
      rank256 = rank(-small$importance, ties.method = "min"))
  }
  screening <- comparison("screen")
  detail <- comparison("detail")
  write.csv(screening, file.path(stage_directory, "screen-comparison.csv"), row.names = FALSE)
  write.csv(detail, file.path(stage_directory, "detail-comparison.csv"), row.names = FALSE)
  overlap <- lapply(c(8L, 16L), function(k) {
    first <- head(screening$feature[order(screening$rank500)], k)
    second <- head(screening$feature[order(screening$rank256)], k)
    list(k = k, common = intersect(first, second), only500 = setdiff(first, second), only256 = setdiff(second, first))
  })
  write_summary(list(status = "ok", case = id, scope = "Development diagnostic, not acceptance or a complete report.",
    overlap = overlap, sign_changes = detail$feature[sign(detail$importance500) != sign(detail$importance256)],
    clearly_positive500_now_nonpositive = detail$feature[detail$lower500 > 0 & detail$importance256 <= 0],
    source_freeze_sha256 = tabular_hash(file.path(root, "freeze.json"))))
}
