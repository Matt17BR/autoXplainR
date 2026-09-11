script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script)), "common.R"))
arguments <- commandArgs(TRUE)
stopifnot(length(arguments) == 2L)
scenario <- arguments[[1L]]
variant <- match.arg(arguments[[2L]], c("core", "stronger", "recommended", "references"))
output <- stress_directory()
library_path <- Sys.getenv("AXR_STRESS_LIBRARY", file.path(output, "library-baseline"))
.libPaths(c(normalizePath(library_path, mustWork = TRUE), .libPaths()))
library(AutoXplainR)
case <- readRDS(file.path(output, "cases", paste0(scenario, ".rds")))
plan <- jsonlite::read_json(file.path(output, "benchmark-plan.json"))
declared <- Filter(function(item) identical(item$name, scenario), plan$cases)
stopifnot(length(declared) == 1L,
  identical(declared[[1L]]$sha256,
    digest::digest(file = file.path(output, "cases", paste0(scenario, ".rds")), algo = "sha256")))
destination <- file.path(output, Sys.getenv("AXR_STRESS_RUN", "baseline"), scenario, variant)
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
if (file.exists(file.path(destination, "summary.json"))) stop("Use a fresh AXR_STRESS_RUN; this result already exists.")
writeLines(capture.output(sessionInfo()), file.path(destination, "session-info.txt"))
warnings <- character()
begin <- proc.time()
summary <- list(scenario = scenario, variant = variant,
  package_version = as.character(packageVersion("AutoXplainR")),
  case_sha256 = digest::digest(file = file.path(output, "cases", paste0(scenario, ".rds")), algo = "sha256"),
  library = find.package("AutoXplainR"), task = case$task)
capture <- function(expression) withCallingHandlers(expression, warning = function(warning) {
  warnings <<- c(warnings, conditionMessage(warning))
  invokeRestart("muffleWarning")
})

tryCatch({
  if (variant != "references") {
    learners <- if (variant == "stronger") c("regularized", "forest", "boosting") else NULL
    portfolio <- if (variant == "recommended") "recommended" else "core"
    result <- capture(autoxplain(case$training, "y", test_data = case$evaluation,
      task = case$task, learners = learners, portfolio = portfolio, seed = case$fit_seed,
      tuning_control = tuning_control(fold_ids = case$folds),
      evaluation_role = "test", explain = FALSE))
    fitted_time <- proc.time() - begin
    # Predictions are independently rescored from raw held-out records, not copied from the leaderboard.
    predictions <- lapply(names(result$models), function(id) predict(result, case$evaluation, model = id))
    names(predictions) <- names(result$models)
    scores <- lapply(predictions, function(prediction) as.list(stress_metrics(case$evaluation$y, prediction, case$task)))
    summary$primary <- result$provenance$primary_model_id
    summary$primary_family <- result$tuning$candidates$family[
      match(result$tuning$final_configuration, result$tuning$candidates$configuration_id)]
    summary$metrics <- scores
    summary$configurations <- result$tuning$candidates
    summary$families_failed <- result$tuning$refit$families_resampling_failed
    summary$configuration_count <- nrow(result$tuning$candidates)
    summary$model_count <- length(result$models)
    summary$fit_elapsed_seconds <- unname(fitted_time[["elapsed"]])
    saveRDS(list(result = result, case = case), file.path(destination, "result.rds"), version = 3L)
    saveRDS(predictions, file.path(destination, "predictions.rds"), version = 3L)
  } else {
    # This independent blueprint learns categorical levels exclusively from training rows.
    train <- case$training
    test <- case$evaluation
    for (name in setdiff(names(train), "y")) {
      if (is.character(train[[name]]) || is.factor(train[[name]])) {
        known <- sort(unique(as.character(train[[name]])))
        train[[name]] <- factor(train[[name]], levels = known)
        stopifnot(all(as.character(test[[name]]) %in% known))
        test[[name]] <- factor(test[[name]], levels = known)
      }
    }
    x <- model.matrix(~ . - 1, train[setdiff(names(train), "y")])
    xt <- model.matrix(~ . - 1, test[setdiff(names(test), "y")])
    stopifnot(identical(colnames(x), colnames(xt)))
    binary <- case$task == "binary"
    y <- if (binary) as.integer(train$y == "yes") else train$y
    predictions <- list()
    records <- list()
    for (reference in c("xgboost", "ranger", "glmnet")) {
      model_start <- proc.time()[["elapsed"]]
      failure <- NULL
      fit <- tryCatch(capture({
        if (reference == "xgboost") {
          model <- xgboost::xgb.train(params = list(
            objective = if (binary) "binary:logistic" else "reg:squarederror",
            eta = 0.05, max_depth = 3L, min_child_weight = 1, subsample = 0.8,
            colsample_bytree = 0.8, lambda = 1, alpha = 0, nthread = 1L,
            seed = case$fit_seed, verbosity = 0L),
            data = xgboost::xgb.DMatrix(x, label = y), nrounds = 400L, verbose = 0L)
          predictions[[reference]] <- as.numeric(predict(model, xgboost::xgb.DMatrix(xt)))
        } else if (reference == "ranger") {
          model <- ranger::ranger(x = as.data.frame(x), y = train$y,
            probability = binary, num.trees = 500L, num.threads = 1L, seed = case$fit_seed)
          predicted <- predict(model, data = as.data.frame(xt))$predictions
          predictions[[reference]] <- if (binary) predicted[, "yes"] else as.numeric(predicted)
        } else {
          model <- glmnet::cv.glmnet(x, y, family = if (binary) "binomial" else "gaussian",
            alpha = 1, foldid = case$folds, parallel = FALSE,
            type.measure = if (binary) "deviance" else "mse")
          predictions[[reference]] <- as.numeric(predict(model, xt, s = "lambda.1se", type = "response"))
        }
        model
      }), error = function(error) { failure <<- conditionMessage(error); NULL })
      records[[reference]] <- list(error = failure,
        elapsed_seconds = proc.time()[["elapsed"]] - model_start,
        metrics = if (!is.null(fit)) as.list(stress_metrics(test$y, predictions[[reference]], case$task)) else NULL)
      if (!is.null(fit)) saveRDS(fit, file.path(destination, paste0(reference, ".rds")), version = 3L)
    }
    summary$references <- records
    if (!is.null(case$oracle)) summary$oracle <- as.list(stress_metrics(test$y, case$oracle, case$task))
    saveRDS(predictions, file.path(destination, "predictions.rds"), version = 3L)
  }
  summary$status <- if (variant == "references" &&
    any(vapply(summary$references, function(item) !is.null(item$error), logical(1)))) "partial" else "ok"
}, error = function(error) {
  summary$status <<- "failed"
  summary$error <<- conditionMessage(error)
})
summary$elapsed_seconds <- unname((proc.time() - begin)[["elapsed"]])
summary$warnings <- unique(warnings)
summary$backend_versions <- as.list(vapply(
  c("nnet", "rpart", "glmnet", "ranger", "xgboost", "mgcv", "Matrix"),
  function(package) tryCatch(as.character(utils::packageVersion(package)),
    error = function(error) NA_character_), character(1)))
writeLines(capture.output(sessionInfo()), file.path(destination, "session-info.txt"))
write_json(summary, file.path(destination, "summary.json"))
cat(scenario, variant, summary$status, "elapsed", summary$elapsed_seconds, "seconds\n")
if (!identical(summary$status, "ok")) quit(status = 1L)
