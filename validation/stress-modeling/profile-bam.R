# Exploratory native-engine comparison, separate from the public package benchmark.
# It never changes an adapter or opens the outer evaluation sample.
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script)), "common.R"))
arguments <- commandArgs(TRUE)
stopifnot(length(arguments) == 2L, arguments[[1L]] %in% c("1", "2", "5"),
  arguments[[2L]] %in% c("discrete", "continuous"))
configuration <- as.integer(arguments[[1L]])
discrete <- arguments[[2L]] == "discrete"
output <- stress_directory()
directory <- file.path(output, "additive-profile")
stopifnot(requireNamespace("mgcv", quietly = TRUE))
original <- readRDS(file.path(directory, paste0("configuration-", configuration, "-inputs-30.rds")))
case <- readRDS(file.path(output, "cases", "friedman_noise.rds"))
validation <- case$training[case$folds == 1L, original$features, drop = FALSE]
names(validation) <- unname(original$fit_details$feature_map)
original_predictions <- predict(original$fit, validation, type = "response")
record <- list(configuration = configuration, inputs = 30L,
  training_rows = nrow(original$fit$model), validation_rows = nrow(validation),
  engine = "mgcv::bam", method = "fREML", discrete = discrete, nthreads = 1L,
  parameters = original$parameters,
  scope = "Exploratory first training-fold comparison against mgcv::gam REML; no outer evaluation and no package engine change.")
start <- proc.time()[["elapsed"]]
tryCatch({
  candidate <- mgcv::bam(formula(original$fit), data = original$fit$model,
    family = stats::gaussian(), method = "fREML", discrete = discrete,
    select = original$parameters$select, gamma = original$parameters$gamma,
    nthreads = 1L)
  record$elapsed_seconds <- proc.time()[["elapsed"]] - start
  predictions <- predict(candidate, validation, type = "response")
  record$validation_rmse <- sqrt(mean((case$training$y[case$folds == 1L] - predictions)^2))
  record$max_prediction_difference_from_gam <- max(abs(predictions - original_predictions))
  record$mean_prediction_difference_from_gam <- mean(abs(predictions - original_predictions))
  record$coefficient_count <- length(coef(candidate))
  record$total_edf <- sum(candidate$edf)
  record$status <- "ok"
}, error = function(error) {
  record$status <<- "failed"
  record$error <<- conditionMessage(error)
  record$elapsed_seconds <<- proc.time()[["elapsed"]] - start
})
write_json(record, file.path(directory,
  paste0("bam-", arguments[[2L]], "-configuration-", configuration, ".json")))
print(record)
if (record$status != "ok") quit(status = 1L)
