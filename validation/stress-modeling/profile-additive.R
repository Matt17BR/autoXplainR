# Locate GAM fitting cost on one declared training fold; never use the final holdout.
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script)), "common.R"))
arguments <- as.integer(commandArgs(TRUE))
stopifnot(length(arguments) == 2L, arguments[[1L]] %in% seq_len(5L),
  arguments[[2L]] %in% c(5L, 10L, 30L))
configuration <- arguments[[1L]]
inputs <- arguments[[2L]]
output <- stress_directory()
.libPaths(c(Sys.getenv("AXR_STRESS_LIBRARY"), .libPaths()))
library(AutoXplainR)
case <- readRDS(file.path(output, "cases", "friedman_noise.rds"))
columns <- c(paste0("x", seq_len(inputs)), "y")
training <- case$training[case$folds != 1L, columns, drop = FALSE]
validation <- case$training[case$folds == 1L, columns, drop = FALSE]
parameters <- getFromNamespace("additive_learner_grid", "AutoXplainR")(
  nrow(training), inputs, "regression", 0L)[[configuration]]
directory <- file.path(output, "additive-profile")
dir.create(directory, recursive = TRUE, showWarnings = FALSE)
prefix <- file.path(directory, paste0("configuration-", configuration, "-inputs-", inputs))
record <- list(configuration = configuration, inputs = inputs, training_rows = nrow(training),
  validation_rows = nrow(validation), parameters = parameters,
  source_rows = "Only first declared training fold; final evaluation rows remain unopened.",
  package_version = as.character(packageVersion("AutoXplainR")))
write_json(record, paste0(prefix, "-started.json"))
start <- proc.time()[["elapsed"]]
tryCatch({
  model <- getFromNamespace("fit_additive_learner", "AutoXplainR")(
    training, "y", "regression", parameters, case$fit_seed)
  record$elapsed_seconds <- proc.time()[["elapsed"]] - start
  record$coefficient_count <- length(coef(model$fit))
  record$total_edf <- sum(model$fit$edf)
  prediction <- predict(model, validation)
  record$validation_rmse <- stress_metrics(validation$y, prediction, "regression")[["rmse"]]
  record$status <- "ok"
  saveRDS(model, paste0(prefix, ".rds"))
}, error = function(error) {
  record$status <<- "failed"
  record$error <<- conditionMessage(error)
  record$elapsed_seconds <<- proc.time()[["elapsed"]] - start
})
write_json(record, paste0(prefix, ".json"))
print(record)
if (record$status != "ok") quit(status = 1L)
