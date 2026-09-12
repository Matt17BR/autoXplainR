# Compare the complete saved recommended searches after the binary solver guard.
# This reads existing fits and predictions; it performs no fitting or selection.
cache <- Sys.getenv("AXR_SEARCH_DIR", path.expand("~/.cache/autoxplain-scale-0.7.0/search"))
.libPaths(c(file.path(cache, "candidate-binary-guard-library"), .libPaths()))
library(AutoXplainR)
stopifnot(as.character(packageVersion("AutoXplainR")) == "0.7.0")
directories <- file.path(cache, "recommended", c("baseline", "candidate_binary_guard"), "bank_marketing")
stored <- lapply(directories, function(path) readRDS(file.path(path, "result.rds")))
predictions <- lapply(directories, function(path) readRDS(file.path(path, "predictions.rds")))
before <- stored[[1L]]$result
after <- stored[[2L]]$result
stopifnot(identical(stored[[1L]]$record$case_sha256, stored[[2L]]$record$case_sha256),
  identical(stored[[1L]]$record$seed, stored[[2L]]$record$seed),
  nrow(after$tuning$candidates) == 30L,
  all(after$tuning$candidates$status == "ok"),
  all(is.finite(after$tuning$fold_scores$score)))
planned <- after$tuning$input_policy$additive$configurations
stopifnot(length(planned) == 5L, all(vapply(planned, function(value) {
  identical(value$solver, "gam") && identical(value$task, "binary")
}, logical(1L))), identical(after$models$additive_model$fit_details$computation$solver, "gam"))
model_ids <- names(predictions[[1L]])
stopifnot(identical(model_ids, names(predictions[[2L]])))
model_checks <- lapply(model_ids, function(id) list(
  model = id, predictions_identical = identical(predictions[[1L]][[id]], predictions[[2L]][[id]]),
  maximum_absolute_difference = max(abs(predictions[[1L]][[id]] - predictions[[2L]][[id]]))))
fold_columns <- c("configuration_id", "fold", "score")
candidate_columns <- c("configuration_id", "cv_score", "folds_completed", "selected", "status")
record <- list(
  case = "bank_marketing", training_rows = nrow(stored[[2L]]$case$training),
  selected_configuration_identical = identical(before$tuning$final_configuration, after$tuning$final_configuration),
  configuration_scores_and_statuses_identical = identical(before$tuning$candidates[candidate_columns],
    after$tuning$candidates[candidate_columns]),
  all_fold_scores_identical = identical(before$tuning$fold_scores[fold_columns], after$tuning$fold_scores[fold_columns]),
  configurations = nrow(after$tuning$candidates), completed_folds = nrow(after$tuning$fold_scores),
  failed_folds = sum(!is.finite(after$tuning$fold_scores$score)),
  planned_additive_solver = "gam", retained_additive_solver = "gam", models = model_checks,
  source_result_sha256 = as.list(vapply(directories, function(path) {
    digest::digest(file = file.path(path, "result.rds"), algo = "sha256")
  }, character(1L))),
  scope = "Full native prediction arrays and all CV scores are checked. Timing, metadata and unrelated serialized fields are not asserted identical."
)
jsonlite::write_json(record, file.path(cache, "bank-policy-parity.json"),
  pretty = TRUE, auto_unbox = TRUE, digits = 16)
stopifnot(record$selected_configuration_identical, record$configuration_scores_and_statuses_identical,
  record$all_fold_scores_identical, all(vapply(model_checks, function(x) x$predictions_identical, logical(1L))))
cat("All 150 validation scores, 30 configuration outcomes and seven full prediction arrays match the published Bank run.\n")
