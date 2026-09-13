arguments <- commandArgs(TRUE)
stopifnot(length(arguments) == 1L)
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script)), "common.R"))
source(file.path(dirname(normalizePath(script)), "evidence-common.R"))
started <- proc.time()[["elapsed"]]
destination <- normalizePath(arguments[[1L]], mustWork = TRUE)
summary <- jsonlite::read_json(file.path(destination, "summary.json"))
# Preserve the explicit development guard. Acceptance uses check-acceptance.R.
stopifnot(identical(summary$status, "ok"), identical(summary$phase, "development"),
  identical(summary$variant, "package"))
partition <- tabular_verify_partition(destination, summary)
training <- readRDS(file.path(partition$path, "training.rds"))
y <- readRDS(file.path(partition$path, "evaluation-targets.rds"))
predictions <- readRDS(file.path(destination, "predictions.rds"))
result <- readRDS(file.path(destination, "model.rds"))
heldout <- tabular_verify_package(result, predictions, summary, training, y)
cv <- tabular_verify_cv(result, summary, training)
record <- c(list(case = summary$case, phase = summary$phase, status = "ok", heldout_rows = length(y),
  heldout_models = length(heldout$ids), model_ids = heldout$ids, forest_ids = heldout$forest_ids,
  heldout_score_differences = heldout$differences, final_training_rows = heldout$final_training_rows), cv,
  list(verification_source_sha256 = list(check_evidence = tabular_hash(script),
    common = tabular_hash(file.path(dirname(normalizePath(script)), "common.R")),
    evidence_common = tabular_hash(file.path(dirname(normalizePath(script)), "evidence-common.R"))),
    saved_model_sha256 = tabular_hash(file.path(destination, "model.rds")),
    predictions_sha256 = tabular_hash(file.path(destination, "predictions.rds")),
    elapsed_seconds = proc.time()[["elapsed"]] - started,
    scope = "Independent development verification. Verification time is separate from the fitting/scoring process budget."))
tabular_json(record, file.path(destination, "evidence-verification.json"))
cat("Verified all retained predictions, reported metrics, full training rows, OOF folds and selected configuration.\n")
