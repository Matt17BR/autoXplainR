# Compare native tuning behavior with an installed release in separate processes.
# Usage: Rscript streaming-check.R <library-path-or-source> <output.rds>
arguments <- commandArgs(TRUE)
stopifnot(length(arguments) == 2L)
if (arguments[[1L]] == "source") {
  pkgload::load_all(quiet = TRUE)
} else {
  library(AutoXplainR, lib.loc = arguments[[1L]])
}
set.seed(915)
training <- data.frame(x = rnorm(120), z = rnorm(120), group = factor(rep(letters[1:3], 40)))
training$y <- sin(training$x) + training$z + rnorm(120, sd = .2)
training$x[seq(3, 120, 7)] <- NA_real_
evaluation <- data.frame(x = rnorm(23), z = rnorm(23), group = factor(rep(letters[1:3], length.out = 23)))
evaluation$y <- sin(evaluation$x) + evaluation$z + rnorm(23, sd = .2)
folds <- rep(c("A", "B", "C"), c(24, 36, 60))
output <- list()
without_times <- function(data) data[!grepl("time|elapsed", names(data))]
# Hold the neural optimizer budget at the published 500 iterations. Its new
# default is a separate modeling change, not part of the streaming comparison.
neural_grid <- list(list(size = 1L, decay = .1), list(size = 2L, decay = .03))
if ("maxit" %in% names(AutoXplainR:::tuning_parameter_contracts()$neural)) {
  neural_grid <- lapply(neural_grid, function(parameters) c(parameters, list(maxit = 500L)))
}
for (task in c("regression", "binary", "multiclass")) {
  train <- training
  test <- evaluation
  metrics <- if (task == "regression") c("rmse", "mae") else c("log_loss", "brier")
  if (task != "regression") {
    classes <- if (task == "binary") c("no", "yes") else c("north", "south", "west")
    train$y <- factor(rep(classes, length.out = nrow(train)), classes)
    test$y <- factor(rep(classes, length.out = nrow(test)), classes)
  }
  for (metric in metrics) {
    result <- autoxplain(train, "y", test_data = test, task = task,
      learners = c("linear", "tree", "neural"), max_models = 6,
      tuning_control = tuning_control(fold_ids = folds, metric = metric, grids = list(neural = neural_grid)),
      seed = 63, explain = FALSE)
    output[[paste(task, metric, sep = "/")]] <- list(
      selected = result$tuning$selected_configuration,
      final = result$tuning$final_configuration,
      candidates = without_times(result$tuning$candidates),
      folds = without_times(result$tuning$fold_scores),
      oof = result$tuning$out_of_fold_predictions,
      omissions = result$tuning$omitted_rows,
      preprocessing = result$tuning$fold_preprocessing,
      predictions = lapply(names(result$models), function(id) predict(result, test, model = id))
    )
  }
}
saveRDS(output, arguments[[2L]])
cat("Wrote", length(output), "task/metric workflows to", arguments[[2L]], "\n")
