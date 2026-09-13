# Declared before inspecting prefix scores. Development assessment only.
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 3L) {
  stop("Usage: Rscript classification-prefix-probe.R CACHE NATIVE_RUN_DIRECTORY OUTPUT_DIRECTORY", call. = FALSE)
}
source("validation/competitive-tabular/common.R")
stopifnot(requireNamespace("ranger", quietly = TRUE))
destination <- arguments[[3L]]
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
stopifnot(!file.exists(file.path(destination, "summary.json")))
counts <- c(128L, 256L, 500L)
score_probabilities <- function(probability, truth) {
  classes <- levels(truth)
  stopifnot(identical(colnames(probability), classes),
    all(is.finite(probability)), all(probability >= 0), all(probability <= 1),
    max(abs(rowSums(probability) - 1)) < 1e-10)
  target <- as.integer(truth)
  actual_probability <- probability[cbind(seq_along(target), target)]
  log_losses <- -log(pmax(actual_probability, 1e-15))
  chosen <- if (length(classes) == 2L) 1L + as.integer(probability[, 2L] >= .5) else {
    max.col(probability, ties.method = "first")
  }
  indicator <- matrix(0, nrow(probability), ncol(probability))
  indicator[cbind(seq_along(target), target)] <- 1
  list(log_loss_floor_1e_15 = mean(log_losses),
    unbounded_log_loss = if (any(actual_probability == 0)) "Inf" else as.character(mean(-log(actual_probability))),
    true_class_zero_probabilities = sum(actual_probability == 0),
    all_class_zero_probabilities = sum(probability == 0),
    brier = if (length(classes) == 2L) mean((probability[, 2L] - indicator[, 2L])^2) else {
      mean(rowSums((probability - indicator)^2))
    },
    accuracy = mean(chosen == target),
    class_recall = stats::setNames(lapply(seq_along(classes), function(index) {
      list(rows = sum(target == index), recall = mean(chosen[target == index] == index))
    }), classes), case_log_losses = log_losses)
}
records <- lapply(c("bank", "covertype"), function(id) {
  partition <- file.path(arguments[[1L]], "cases", id, "development")
  x_path <- file.path(partition, "evaluation-features.rds")
  y_path <- file.path(partition, "evaluation-targets.rds")
  evaluation <- readRDS(x_path)
  truth <- readRDS(y_path)
  model_path <- file.path(arguments[[2L]], id, "ranger/model.rds")
  saved <- readRDS(model_path)
  original <- readRDS(file.path(arguments[[2L]], id, "ranger/predictions.rds"))$ranger
  stopifnot(saved$model$num.trees == 500L, identical(saved$class_levels, levels(truth)))
  x <- tabular_bake(evaluation$data, saved$blueprint, matrix = FALSE)
  probabilities <- stats::setNames(lapply(counts, function(count) {
    matrix(NA_real_, nrow(x), length(levels(truth)), dimnames = list(NULL, levels(truth)))
  }), as.character(counts))
  maximum_chunk_bytes <- 0
  for (start in seq.int(1L, nrow(x), by = 1000L)) {
    rows <- seq.int(start, min(start + 999L, nrow(x)))
    tree_probability <- predict(saved$model, data = x[rows, , drop = FALSE],
      predict.all = TRUE, num.trees = 500L, num.threads = 2L)$predictions
    stopifnot(identical(dim(tree_probability), c(length(rows), length(levels(truth)), 500L)))
    maximum_chunk_bytes <- max(maximum_chunk_bytes, as.numeric(object.size(tree_probability)))
    for (count in counts) {
      probabilities[[as.character(count)]][rows, ] <- rowMeans(
        tree_probability[, , seq_len(count), drop = FALSE], dims = 2L)
    }
  }
  full_score <- score_probabilities(probabilities[["500"]], truth)
  set.seed(80801L)
  bootstrap_rows <- replicate(300L, sample.int(nrow(x), nrow(x), replace = TRUE))
  scores <- lapply(counts, function(count) {
    native <- predict(saved$model, data = x, num.trees = count, num.threads = 2L)$predictions
    oracle <- probabilities[[as.character(count)]]
    stopifnot(identical(colnames(native), levels(truth)))
    delta <- max(abs(native - oracle))
    stopifnot(delta < 1e-10)
    score <- score_probabilities(oracle, truth)
    differences <- score$case_log_losses - full_score$case_log_losses
    paired <- apply(bootstrap_rows, 2L, function(rows) mean(differences[rows]))
    score$case_log_losses <- NULL
    list(trees = count, metrics = score,
      log_loss_difference_from_500 = mean(differences),
      paired_row_bootstrap_difference_quantiles = stats::setNames(
        as.list(as.numeric(quantile(paired, c(.025, .5, .975)))), c("q025", "median", "q975")),
      independent_aggregation_maximum_difference = delta,
      original_500_predictions_maximum_difference = if (count == 500L) max(abs(native - original)) else NULL)
  })
  stopifnot(scores[[3L]]$original_500_predictions_maximum_difference == 0)
  saveRDS(list(id = id, predictions = probabilities, evaluation_source_rows = evaluation$source_rows,
    scores = scores), file.path(destination, paste0(id, "-prefix.rds")), compress = FALSE)
  cat(id, ":", paste(vapply(scores, function(score) {
    paste0(score$trees, " trees LL=", signif(score$metrics$log_loss_floor_1e_15, 8))
  }, character(1)), collapse = "; "), "\n")
  list(id = id, phase = "development assessment, already used for iterative diagnosis",
    native_training_rows = saved$model$num.samples, scored_rows = nrow(x),
    native_predictors = saved$model$num.independent.variables,
    original_fit_threads = saved$threads, prediction_threads = 2L,
    maximum_tree_prediction_chunk_bytes = maximum_chunk_bytes,
    model_sha256 = digest::digest(file = model_path, algo = "sha256"),
    feature_data_sha256 = digest::digest(file = x_path, algo = "sha256"),
    outcome_data_sha256 = digest::digest(file = y_path, algo = "sha256"), scores = scores)
})
script_path <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())])
output <- list(generated_at = format(Sys.time(), tz = "UTC"),
  runner_sha256 = digest::digest(file = script_path, algo = "sha256"),
  preprocessing_source_sha256 = digest::digest(file = "validation/competitive-tabular/common.R", algo = "sha256"),
  ranger_version = as.character(packageVersion("ranger")), R_version = R.version.string,
  records = records, scope = paste("Two saved native forests, no new fitting or parameter selection.",
    "Independent probability aggregation, log loss, Brier score and per-class recall checks.",
    "Row bootstrap describes this assessment sample only; models are not refitted.",
    "All targets are development assessment targets. Acceptance targets remain unopened."))
jsonlite::write_json(output, file.path(destination, "summary.json"), pretty = TRUE, auto_unbox = TRUE, digits = 16)
