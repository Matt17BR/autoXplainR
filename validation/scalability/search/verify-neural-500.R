# Fixed-procedure parity after adding the public budget and compact native call.
# The saved references came from published 0.6.2 on training-validation fold 2.
pkgload::load_all(".", quiet = TRUE)
source("validation/scalability/million/fixtures.R")
cache <- path.expand("~/.cache/autoxplain-scale-0.7.0/search/neural-iterations")
training <- scale_fixture("regression", 10000L)$data
set.seed(824L)
assignment <- AutoXplainR:::tuning_fold_assignment(training$outcome, "regression", 5L)
preprocessing <- list(enable_target_handling = TRUE, enable_character_to_factors = TRUE,
  enable_ordered_factors = FALSE, enable_ordinal_factors = FALSE, enable_id_removal = FALSE,
  missing_value_strategy = "impute", novel_level_strategy = "mode", verbose = FALSE)
fold <- AutoXplainR:::prepare_tuning_fold(training, "outcome", "regression", assignment$id,
  2L, TRUE, preprocessing)
files <- list.files(cache, pattern = "-fold2-maxit500[.]rds$", full.names = TRUE)
stopifnot(length(files) == 5L)
records <- lapply(files, function(path) {
  reference <- readRDS(path)
  record <- reference$record
  fitted <- withr::with_seed(record$fit_seed, AutoXplainR:::fit_tuned_neural_network(
    fold$training, "outcome", "regression", record$size, record$decay, maxit = 500L
  ))
  prediction <- predict(fitted, fold$validation)
  stopifnot(identical(prediction, reference$validation_prediction))
  fields <- setdiff(names(reference$model$model), "call")
  stopifnot(identical(fitted$model[fields], reference$model$model[fields]))
  destination <- sub("-maxit500[.]rds$", "-candidate-maxit500.rds", path)
  saveRDS(fitted, destination)
  stopifnot(identical(predict(readRDS(destination), fold$validation), prediction))
  data.frame(configuration = record$configuration, fold = 2L, seed = record$fit_seed,
    maxit = 500L, predictions_identical = TRUE, native_fields_except_call_identical = TRUE,
    serialized_call_bytes_before = length(serialize(reference$model$model$call, NULL)),
    serialized_call_bytes_after = length(serialize(fitted$model$call, NULL)))
})
output <- do.call(rbind, records)
utils::write.csv(output, "validation/scalability/search/neural-500-parity.csv", row.names = FALSE)
print(output)
