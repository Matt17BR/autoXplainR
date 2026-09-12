# Fixed training folds compare computational methods, never the final holdout.
output <- Sys.getenv("AXR_SEARCH_DIR", path.expand("~/.cache/autoxplain-scale-0.7.0/search"))
dir.create(output, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(file.path(output, "baseline-library"), .libPaths()))
library(AutoXplainR)
stopifnot(as.character(packageVersion("AutoXplainR")) == "0.6.2")
stopifnot(requireNamespace("mgcv", quietly = TRUE))
cache <- path.expand("~/.cache/autoxplain-stress-0.6.2")
cases <- list()
for (replicate in c("benchmark", "benchmark-replicate")) {
  for (scenario in c("friedman_noise", "rare_interaction")) {
    case <- readRDS(file.path(cache, replicate, "cases", paste0(scenario, ".rds")))
    cases[[paste(scenario, replicate, sep = "_")]] <- case[c("training", "folds", "task")]
  }
}
bank <- readRDS(file.path(cache, "benchmark/cases/bank_marketing.rds"))
cases$bank <- bank[c("training", "folds", "task")]
for (seed in c(719L, 720L)) {
  set.seed(seed)
  data <- as.data.frame(matrix(rnorm(1600L * 9L), 1600L, 9L))
  names(data) <- paste0("x", seq_len(9L))
  data$x1 <- exp(1.3 * data$x1)
  data$`segment / label` <- factor(sample(c("ordinary", "rare", "a:b"), 1600L, TRUE,
    prob = c(.7, .03, .27)))
  data$y <- 2 * log1p(data$x1) + sin(data$x2) + data$x3 * data$x4 +
    as.integer(data$`segment / label` == "rare") + rnorm(1600L, sd = .3 + .2 * data$x1)
  cases[[paste0("skew_factor_", seed)]] <- list(training = data,
    folds = sample(rep(1:5, length.out = nrow(data))), task = "regression")
}
saveRDS(cases, file.path(output, "solver-cases.rds"))
records <- list()
write_records <- function() jsonlite::write_json(records, file.path(output, "solver-comparison.json"),
  pretty = TRUE, auto_unbox = TRUE, digits = 16, na = "null")
score <- function(y, prediction, task) {
  stopifnot(length(prediction) == length(y), all(is.finite(prediction)))
  if (task == "regression") return(sqrt(mean((y - prediction)^2)))
  p <- pmax(1e-15, pmin(1 - 1e-15, prediction))
  event <- as.integer(y == levels(y)[[2L]])
  -mean(event * log(p) + (1 - event) * log1p(-p))
}
for (name in names(cases)) {
  case <- cases[[name]]
  for (fold_id in 1:2) {
    fold <- AutoXplainR:::prepare_tuning_fold(case$training, "y", case$task,
      case$folds, fold_id, TRUE, list())
    parameters <- list(k = 5L, gamma = 1, select = TRUE)
    original <- NULL
    original_prediction <- NULL
    for (solver in c("gam", "bam", "bam_discrete")) {
      warnings <- character()
      started <- proc.time()[["elapsed"]]
      record <- list(case = name, fold = fold_id, solver = solver, task = case$task,
        training_rows = nrow(fold$training), validation_rows = nrow(fold$validation),
        inputs = ncol(fold$training) - 1L, parameters = parameters)
      tryCatch(withCallingHandlers({
        set.seed(608L + fold_id)
        if (solver == "gam") {
          model <- AutoXplainR:::fit_additive_learner(fold$training, "y", case$task, parameters, 608L + fold_id)
          native <- model$fit
          original <- model
        } else {
          stopifnot(!is.null(original))
          native <- mgcv::bam(formula(original$fit), data = original$fit$model,
            family = if (case$task == "regression") gaussian() else binomial(),
            method = "fREML", discrete = solver == "bam_discrete", nthreads = 1L,
            select = parameters$select, gamma = parameters$gamma)
          model <- original
          model$fit <- native
        }
        record$fit_seconds <- proc.time()[["elapsed"]] - started
        prediction <- predict(model, fold$validation)
        if (solver == "gam") original_prediction <- prediction
        record$score <- score(fold$validation$y, prediction, case$task)
        record$max_prediction_difference <- max(abs(prediction - original_prediction))
        record$mean_prediction_difference <- mean(abs(prediction - original_prediction))
        record$edf <- sum(native$edf)
        record$coefficients <- length(coef(native))
        record$optimization <- AutoXplainR:::model_optimization_record(model)
        record$native_convergence <- list(converged = native$converged,
          outer = native$outer.info$conv, mgcv = native$mgcv.conv)
        record$status <- "ok"
        saveRDS(list(model = model, prediction = prediction, fold = fold),
          file.path(output, paste0(name, "-fold-", fold_id, "-", solver, ".rds")))
      }, warning = function(warning) {
        warnings <<- c(warnings, conditionMessage(warning))
        invokeRestart("muffleWarning")
      }), error = function(error) {
        record$status <<- "failed"
        record$error <<- conditionMessage(error)
      })
      record$elapsed_seconds <- proc.time()[["elapsed"]] - started
      record$warnings <- warnings
      records[[length(records) + 1L]] <- record
      write_records()
      elapsed <- if (is.null(record$fit_seconds)) NA_real_ else record$fit_seconds
      cat(name, "fold", fold_id, solver, record$status, round(elapsed, 3), "s\n")
      flush.console()
    }
  }
}
writeLines(capture.output(sessionInfo()), file.path(output, "session-info.txt"))
