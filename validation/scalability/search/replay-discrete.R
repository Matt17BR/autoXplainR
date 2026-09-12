# Replay saved training folds at a finer resolution. The first comparison and
# its integration failures remain untouched in solver-comparison.json.
output <- Sys.getenv("AXR_SEARCH_DIR", path.expand("~/.cache/autoxplain-scale-0.7.0/search"))
pkgload::load_all(".", quiet = TRUE)
paths <- sort(list.files(output, pattern = "-fold-[0-9]+-gam[.]rds$", full.names = TRUE))
stopifnot(length(paths) == 14L)
records <- list()
for (path in paths) {
  reference <- readRDS(path)
  task <- reference$model$task
  if (is.null(task)) task <- if (is.factor(reference$fold$validation$y)) "binary" else "regression"
  parameters <- list(k = 5L, gamma = 1, select = TRUE,
    solver = "bam_discrete", discrete_bins = 10000L)
  seed <- 608L + as.integer(sub(".*-fold-([0-9]+)-gam[.]rds$", "\\1", basename(path)))
  warnings <- character()
  record <- list(reference = basename(path), parameters = parameters, task = task, seed = seed)
  started <- proc.time()[["elapsed"]]
  tryCatch(withCallingHandlers({
    set.seed(seed)
    model <- AutoXplainR:::fit_additive_learner(reference$fold$training, "y", task,
      parameters, seed)
    record$fit_seconds <- proc.time()[["elapsed"]] - started
    prediction <- predict(model, reference$fold$validation)
    y <- reference$fold$validation$y
    loss <- function(p) {
      stopifnot(all(is.finite(p)), length(p) == length(y))
      if (task == "regression") return(sqrt(mean((y - p)^2)))
      p <- pmax(1e-15, pmin(1 - 1e-15, p))
      event <- as.integer(y == levels(y)[[2L]])
      -mean(event * log(p) + (1 - event) * log1p(-p))
    }
    record$score <- loss(prediction)
    record$reference_score <- loss(reference$prediction)
    record$max_prediction_difference <- max(abs(prediction - reference$prediction))
    record$mean_prediction_difference <- mean(abs(prediction - reference$prediction))
    record$computation <- model$fit_details$computation
    record$optimization <- AutoXplainR:::model_optimization_record(model)
    record$status <- if (record$optimization$status == "converged") "ok" else "check_convergence"
    saveRDS(list(model = model, prediction = prediction, fold = reference$fold),
      sub("-gam[.]rds$", "-bam-discrete-10000.rds", path))
  }, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }), error = function(e) {
    record$status <<- "failed"
    record$error <<- conditionMessage(e)
  })
  record$elapsed_seconds <- proc.time()[["elapsed"]] - started
  record$warnings <- warnings
  records[[length(records) + 1L]] <- record
  jsonlite::write_json(records, file.path(output, "discrete-10000-comparison.json"),
    auto_unbox = TRUE, pretty = TRUE, digits = 16, na = "null")
  cat(basename(path), record$status, round(record$elapsed_seconds, 3), "s\n")
}
stopifnot(all(vapply(records, function(record) record$status == "ok", logical(1))))
