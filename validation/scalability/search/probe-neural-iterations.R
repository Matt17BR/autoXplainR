# Evidence-only probe. No final evaluation dataset is constructed or opened.
# Compare the published native fitting procedure at two fixed iteration caps.
arguments <- commandArgs(TRUE)
fold_ids <- if (length(arguments)) as.integer(arguments) else 2L
stopifnot(all(fold_ids %in% 1:5))
cache <- path.expand("~/.cache/autoxplain-scale-0.7.0")
destination <- file.path(cache, "search", "neural-iterations")
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(file.path(cache, "search", "baseline-library"), .libPaths()))
library(AutoXplainR)
stopifnot(as.character(packageVersion("AutoXplainR")) == "0.6.2")
source("validation/scalability/million/fixtures.R")
training <- scale_fixture("regression", 10000L)$data
saved <- readRDS(file.path(cache, "baseline", "regression-10000-default-fit", "tuning-evidence.rds"))
set.seed(824L)
assignment <- AutoXplainR:::tuning_fold_assignment(training$outcome, "regression", 5L)
fit_original <- AutoXplainR:::fit_tuned_neural_network
function_text <- paste(deparse(fit_original), collapse = "\n")
stopifnot(sum(grepl("maxit = 500L", strsplit(function_text, "\n")[[1L]], fixed = TRUE)) == 1L)
fit_extended <- eval(parse(text = sub("maxit = 500L", "maxit = 2000L", function_text, fixed = TRUE)),
  envir = environment(fit_original))
configuration_ids <- c("neural_02", "neural_03", "neural_04", "neural_05", "neural_07")
records <- list()
preprocessing <- list(enable_target_handling = TRUE, enable_character_to_factors = TRUE,
  enable_ordered_factors = FALSE, enable_ordinal_factors = FALSE, enable_id_removal = FALSE,
  missing_value_strategy = "impute", novel_level_strategy = "mode", verbose = FALSE)
for (fold_id in fold_ids) {
  fold <- AutoXplainR:::prepare_tuning_fold(training, "outcome", "regression", assignment$id,
    fold_id, TRUE, preprocessing)
  for (configuration in configuration_ids) {
    reference <- saved$fold_scores[saved$fold_scores$configuration_id == configuration &
      saved$fold_scores$fold == fold_id, ]
    stopifnot(nrow(reference) == 1L, nrow(fold$training) == reference$training_rows,
      nrow(fold$validation) == reference$validation_rows)
    parameters <- reference$requested_parameters[[1L]]
    for (maxit in c(500L, 2000L)) {
      set.seed(reference$fit_seed)
      started <- proc.time()[["elapsed"]]
      fit <- (if (maxit == 500L) fit_original else fit_extended)(
        fold$training, "outcome", "regression", parameters$size, parameters$decay)
      fit_seconds <- proc.time()[["elapsed"]] - started
      prediction <- predict(fit, fold$validation)
      loss <- sqrt(mean((fold$validation$outcome - prediction)^2))
      stopifnot(length(prediction) == 2000L, all(is.finite(prediction)))
      if (maxit == 500L) {
        stopifnot(identical(fit$model$convergence == 0L,
          reference$optimization_status == "converged"))
        if (is.finite(reference$score)) {
          stopifnot(isTRUE(all.equal(loss, reference$score, tolerance = 1e-10)))
        }
      }
      record <- list(fold = fold_id, configuration = configuration, size = parameters$size,
        decay = parameters$decay, fit_seed = reference$fit_seed, maxit = maxit,
        convergence = fit$model$convergence, training_objective = fit$model$value,
        cv_rmse = loss, fit_seconds = fit_seconds)
      records[[length(records) + 1L]] <- record
      stem <- paste0(configuration, "-fold", fold_id, "-maxit", maxit)
      saveRDS(list(model = fit, validation_prediction = prediction, record = record),
        file.path(destination, paste0(stem, ".rds")))
      jsonlite::write_json(records, file.path(destination, paste0("folds-", paste(fold_ids, collapse = "-"), ".json")),
        pretty = TRUE, auto_unbox = TRUE, digits = 16)
      cat(stem, "convergence", record$convergence, "CV RMSE", format(loss, digits = 7),
        "seconds", fit_seconds, "\n")
      flush.console()
    }
  }
}
writeLines(capture.output(sessionInfo()), file.path(destination, "session-info.txt"))
