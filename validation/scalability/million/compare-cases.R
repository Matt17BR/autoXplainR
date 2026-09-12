# Compare unchanged-search before/after cases. Explicitly different search spaces
# (for example native categorical boosting) need quality comparisons instead.
args <- commandArgs(TRUE)
stopifnot(length(args) == 3L)
before_dir <- normalizePath(args[[1L]])
after_dir <- normalizePath(args[[2L]])
metadata <- jsonlite::read_json(file.path(after_dir, "input.json"), simplifyVector = TRUE)
library_path <- normalizePath(dirname(metadata$library), mustWork = TRUE)
.libPaths(c(library_path, .libPaths()))
library(AutoXplainR)
loaded_library <- normalizePath(find.package("AutoXplainR"), mustWork = TRUE)
stopifnot(loaded_library == file.path(library_path, "AutoXplainR"))
script <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)))
fixture <- file.path(dirname(script), "fixtures.R")
stopifnot(unname(tools::md5sum(fixture)) == metadata$fixture_source_md5)
source(fixture)
holdout <- scale_fixture(metadata$problem, metadata$evaluation_rows, evaluation = TRUE)$data
before <- readRDS(file.path(before_dir, "result.rds"))
after <- readRDS(file.path(after_dir, "result.rds"))
stopifnot(identical(before$training_data, after$training_data),
          identical(before$test_data, after$test_data),
          identical(names(before$models), names(after$models)))
failures <- character()
metadata_changes <- character()
# 0.6.2 always used the numeric matrix path. The new version names that choice
# explicitly. Normalize only this documented equivalence, and keep the original
# unmodified RDS evidence. Native encoding or any changed numeric setting still
# fails this parity check.
if (!is.null(before$tuning)) {
  for (i in seq_len(nrow(after$tuning$candidates))) {
    row <- after$tuning$candidates[i, ]
    previous <- before$tuning$candidates[before$tuning$candidates$configuration_id == row$configuration_id, ]
    if (row$family == "boosting" && nrow(previous) == 1L &&
        identical(row$hyperparameters, paste0(previous$hyperparameters, ", input encoding = matrix"))) {
      after$tuning$candidates$hyperparameters[[i]] <- previous$hyperparameters
      metadata_changes <- c(metadata_changes, paste(row$configuration_id, "now names matrix input encoding"))
    }
  }
  for (i in seq_len(nrow(after$tuning$fold_scores))) {
    row <- after$tuning$fold_scores[i, ]
    previous <- before$tuning$fold_scores[
      before$tuning$fold_scores$configuration_id == row$configuration_id &
        before$tuning$fold_scores$fold == row$fold, ]
    family <- after$tuning$candidates$family[
      match(row$configuration_id, after$tuning$candidates$configuration_id)]
    if (family != "boosting" || nrow(previous) != 1L) next
    for (kind in c("requested", "effective")) {
      field <- paste0(kind, "_parameters")
      key_field <- paste0(kind, "_parameter_key")
      old_parameters <- previous[[field]][[1L]]
      parameters <- row[[field]][[1L]]
      if (is.null(old_parameters$encoding) && identical(parameters$encoding, "matrix")) {
        parameters$encoding <- NULL
        after$tuning$fold_scores[[field]][[i]] <- parameters
        # Remove only the exact new suffix; any other changed key survives.
        after$tuning$fold_scores[[key_field]][[i]] <- sub(
          "\\|encoding=character:matrix$", "", row[[key_field]])
        metadata_changes <- c(metadata_changes,
          paste(row$configuration_id, "fold", row$fold, kind, "matrix encoding recorded"))
      }
    }
  }
}
compare <- function(label, x, y) {
  difference <- all.equal(x, y, tolerance = 1e-10)
  if (!isTRUE(difference)) failures <<- c(failures, paste(label, paste(difference, collapse = "; ")))
}
compare("held-out metrics", before$evaluation$metrics, after$evaluation$metrics)
compare("stored primary/baseline held-out predictions", before$evaluation$predictions, after$evaluation$predictions)
for (id in names(after$models)) {
  old_prediction <- predict(before, holdout, model = id)
  new_prediction <- predict(after, holdout, model = id)
  stopifnot(NROW(old_prediction) == nrow(before$test_data),
    NROW(new_prediction) == nrow(after$test_data))
  compare(paste("all held-out predictions", id), old_prediction, new_prediction)
}
compare("selected configuration", before$tuning$selected_configuration, after$tuning$selected_configuration)
compare("final configuration", before$tuning$final_configuration, after$tuning$final_configuration)
if (!is.null(before$tuning)) {
  fields <- c("configuration_id", "family", "hyperparameters", "cv_score", "cv_sd", "cv_se",
              "folds_completed", "evaluated_rows", "selected", "status")
  order_candidates <- function(x) {
    x <- x[order(x$configuration_id), fields, drop = FALSE]
    rownames(x) <- NULL
    x
  }
  compare("candidate evidence", order_candidates(before$tuning$candidates),
          order_candidates(after$tuning$candidates))
  fields <- c("configuration_id", "fold", "requested_parameter_key", "effective_parameter_key",
              "fit_seed", "training_rows", "score", "validation_rows", "validation_rows_omitted",
              "novel_levels_mapped", "requested_parameters", "effective_parameters")
  order_folds <- function(x) {
    x <- x[order(x$configuration_id, x$fold), fields, drop = FALSE]
    rownames(x) <- NULL
    x
  }
  compare("fold evidence", order_folds(before$tuning$fold_scores), order_folds(after$tuning$fold_scores))
  compare("fold preprocessing", before$tuning$fold_preprocessing, after$tuning$fold_preprocessing)
}
before_replay <- readRDS(file.path(before_dir, "replay.rds"))
after_replay <- readRDS(file.path(after_dir, "replay.rds"))
compare("raw new-data predictions", before_replay, after_replay)
jsonlite::write_json(list(
  status = if (length(failures)) "difference" else "identical_within_tolerance",
  tolerance = 1e-10, before = before_dir, after = after_dir,
  library = loaded_library, version = as.character(packageVersion("AutoXplainR")),
  training_rows = nrow(after$training_data), evaluation_rows = nrow(after$test_data),
  models = length(after$models), full_holdout_predictions_checked_per_model = nrow(after$test_data),
  expected_metadata_changes = metadata_changes, failures = failures
), args[[3L]], pretty = TRUE, auto_unbox = TRUE)
stopifnot(!length(failures))
