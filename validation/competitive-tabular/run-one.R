arguments <- commandArgs(TRUE)
stopifnot(length(arguments) %in% c(10L, 11L, 12L))
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script)), "common.R"))
name <- arguments[[1L]]
variant <- match.arg(arguments[[2L]], c("package", "xgboost", "ranger"))
phase <- match.arg(arguments[[3L]], c("development", "acceptance"))
destination <- normalizePath(arguments[[4L]], mustWork = TRUE)
library_path <- arguments[[5L]]
threads <- as.integer(arguments[[6L]])
freeze_path <- arguments[[7L]]
request <- match.arg(arguments[[8L]], c("paired", "tabular", "public-tabular"))
stage <- match.arg(arguments[[9L]], c("fit-and-score", "fit-only", "score-only"))
fit_source <- arguments[[10L]]
native_plan_path <- if (length(arguments) >= 11L) arguments[[11L]] else ""
cohort <- if (length(arguments) == 12L) arguments[[12L]] else ""
fixed_native <- nzchar(native_plan_path)
fit_only <- identical(stage, "fit-only")
if (fixed_native && (variant != "ranger" || !fit_only || threads != 4L)) {
  stop("A fixed native reference plan requires ranger fit-only with four threads.")
}
if (variant == "package" && stage != "fit-and-score") stop("Staging is native-reference only.")
stopifnot(threads >= 1L, threads <= 4L)
if (variant == "package") {
  .libPaths(c(normalizePath(library_path, mustWork = TRUE), .libPaths()))
  library(AutoXplainR)
  stopifnot(identical(normalizePath(find.package("AutoXplainR")),
    normalizePath(file.path(library_path, "AutoXplainR"), mustWork = TRUE)))
}
data.table::setDTthreads(threads)
cache <- tabular_cache()
partition <- file.path(cache, "cases", name, phase)
plan <- jsonlite::read_json(file.path(cache, "partitions.json"))
if (phase == "acceptance" && !fit_only) {
  if (!nzchar(freeze_path) || !file.exists(freeze_path)) stop("Acceptance scoring is locked; a frozen candidate manifest is required.")
  if (!grepl("^[a-zA-Z0-9][a-zA-Z0-9._-]*$", cohort)) {
    stop("Acceptance scoring requires an explicit cohort argument.")
  }
  freeze <- jsonlite::read_json(freeze_path)
  if (!isTRUE(freeze$acceptance_authorized) || !nzchar(freeze$candidate_source_sha256) ||
      !identical(freeze$partitions_sha256, tabular_hash(file.path(cache, "partitions.json"))) ||
      !identical(freeze$protocol_sha256, plan$protocol_sha256)) {
    stop("Invalid acceptance freeze manifest or changed partition/protocol.")
  }
  entries <- Filter(function(entry) identical(entry$case, name) &&
    identical(entry$variant, variant) && entry$threads == threads &&
    identical(if (is.null(entry$request)) "paired" else entry$request, request) &&
    identical(entry$stage, stage) && identical(entry$cohort, cohort), freeze$allowed_runs)
  if (length(entries) != 1L) stop("This exact acceptance process was not authorized.")
  required_protocols <- c("README.md", "additional-cohorts.md", "forest-family-acceptance.md",
    "covertype-multicore.md", "native-staging.md", "staged-scoring-retries.md",
    "forest-validation-budget-v2.md", "boosting-anchor-amendment-v2.md", "public-one-call.md",
    "forest-tree-budget-v3.md", "fixed-native-forest-v1.md", "fixed-native-forest-v1.json",
    "native-bank-original-1t-20260913.md")
  if (!all(required_protocols %in% names(freeze$protocol_files))) {
    stop("Acceptance freeze lacks a required protocol amendment.")
  }
  if (!identical(freeze$protocol_files[["README.md"]], plan$protocol_sha256)) {
    stop("Original protocol changed after preparation; use explicit amendments.")
  }
  for (file in names(freeze$protocol_files)) {
    if (!identical(file, basename(file)) ||
        !identical(tabular_hash(file.path(dirname(script), file)), freeze$protocol_files[[file]])) {
      stop("Acceptance protocol or amendment changed: ", file)
    }
  }
  if (variant == "package") {
    package <- find.package("AutoXplainR")
    files <- list.files(package, recursive = TRUE, all.files = TRUE, no.. = TRUE)
    files <- files[!file.info(file.path(package, files))$isdir]
    expected <- entries[[1L]]$package_files
    if (!identical(sort(files), sort(names(expected)))) stop("Installed candidate file inventory changed.")
    for (file in files) {
      if (!identical(tabular_hash(file.path(package, file)), expected[[file]])) {
        stop("Installed candidate file changed: ", file)
      }
    }
  }
  if (stage == "score-only") {
    if (!identical(tabular_hash(file.path(fit_source, "process.json")), entries[[1L]]$fit_process_sha256) ||
        !identical(tabular_hash(file.path(fit_source, "model.rds")), entries[[1L]]$fit_model_sha256) ||
        !identical(tabular_hash(file.path(fit_source, "summary.json")), entries[[1L]]$fit_summary_sha256)) {
      stop("Saved native fit does not match the final acceptance freeze.")
    }
  }
}
declared <- plan$cases[[paste(name, phase, sep = "/")]]
if (is.null(declared)) stop("Case is not declared.")
native_reference <- NULL
if (fixed_native) {
  # Repeat provenance validation inside the bounded process before reading data.
  # The validator opens JSON evidence only and selects exclusively by calibration.
  verified_path <- tempfile("native-reference-verified-", tmpdir = destination, fileext = ".json")
  validation_status <- system2("python3", c(shQuote(file.path(dirname(script), "native_reference_plan.py")),
    "--plan", shQuote(native_plan_path), "--cache", shQuote(cache),
    "--case", shQuote(name), "--phase", shQuote(phase)), stdout = verified_path)
  if (validation_status != 0L) stop("Fixed native reference provenance validation failed.")
  native_reference <- jsonlite::read_json(verified_path)
  unlink(verified_path)
  supervisor <- jsonlite::read_json(file.path(destination, "process.json"))
  stopifnot(identical(supervisor$native_reference_protocol, native_reference$protocol),
    identical(supervisor$native_reference_plan_sha256, native_reference$plan_sha256),
    identical(tabular_hash(native_plan_path), native_reference$plan_sha256))
}
partition_files_to_read <- if (fit_only) "training.rds" else names(declared$files)
for (filename in partition_files_to_read) {
  stopifnot(identical(tabular_hash(file.path(partition, filename)), declared$files[[filename]]$sha256))
}
if (stage == "score-only") {
  source(file.path(dirname(normalizePath(script)), "score-native.R"))
  score_saved_native(name, phase, variant, threads, destination, fit_source, partition, declared)
  quit(status = 0L)
}
training <- readRDS(file.path(partition, "training.rds"))
evaluation <- if (!fit_only) readRDS(file.path(partition, "evaluation-features.rds")) else NULL
y_evaluation <- if (!fit_only) readRDS(file.path(partition, "evaluation-targets.rds")) else NULL
train <- training$data
features <- setdiff(names(train), "y")
task <- training$metadata$task
class_levels <- if (is.factor(train$y)) levels(train$y) else NULL
fit_seed <- training$metadata$fit_seed
if (fixed_native) {
  stopifnot(fit_seed == native_reference$entry$seed,
    nrow(train) == declared$n_training, nrow(train) == native_reference$entry$full_training_rows,
    identical(task, declared$task), length(features) == declared$predictors)
}
warnings <- character()
summary <- list(case = name, phase = phase, variant = variant, request = request, stage = stage,
  training_rows = nrow(train), evaluation_rows = declared$n_evaluation, predictors = length(features),
  task = task, native_threads = threads, partition_files = declared$files,
  started_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  package_version = if (variant == "package") as.character(packageVersion("AutoXplainR")) else NULL,
  package_library = if (variant == "package") find.package("AutoXplainR") else NULL,
  backend_versions = as.list(vapply(c("xgboost", "ranger", "Matrix", "data.table"),
    function(package) as.character(packageVersion(package)), character(1))))
if (fixed_native) {
  summary$native_reference_protocol <- native_reference$protocol
  summary$native_reference_plan_sha256 <- native_reference$plan_sha256
  summary$native_reference <- native_reference
  summary$new_calibration_fits <- 0L
  summary$fit_seed <- fit_seed
}
writeLines(capture.output(sessionInfo()), file.path(destination, "session-info.txt"))
tabular_json(summary, file.path(destination, "started.json"))
begin <- proc.time()[["elapsed"]]
capture <- function(expression) withCallingHandlers(expression, warning = function(warning) {
  warnings <<- c(warnings, conditionMessage(warning))
  cat("WARNING", conditionMessage(warning), "\n")
  invokeRestart("muffleWarning")
})

xgb_prediction <- function(fit, matrix, best_rounds = NULL) {
  input <- xgboost::xgb.DMatrix(matrix, nthread = threads)
  result <- if (is.null(best_rounds)) predict(fit, input) else {
    # XGBoost 3.2.1.1's R method subtracts one from BEGIN, leaving END unchanged
    # for C's exclusive endpoint. Thus this R interval includes rounds 1:best.
    predict(fit, input, iterationrange = c(1L, best_rounds))
  }
  if (task == "multiclass") {
    if (is.null(dim(result))) result <- matrix(result, ncol = length(class_levels), byrow = TRUE)
    colnames(result) <- class_levels
    return(result)
  }
  as.numeric(result)
}

tryCatch({
  if (variant == "package") {
    control_arguments <- list(fold_ids = training$folds)
    if ("threads" %in% names(formals(tuning_control))) {
      control_arguments$threads <- threads
    } else if (threads != 1L) {
      stop("This package version does not expose native thread controls.")
    }
    # Tracing only records the actual rows entering native adapter fits. It does
    # not change data, arguments, seeds, fit order, or scores.
    for (function_name in c("fit_forest_learner", "fit_boosting_learner")) {
      trace(function_name, where = asNamespace("AutoXplainR"), print = FALSE,
        tracer = substitute({
          cat("NATIVE_FIT", LABEL, nrow(data), "rows\n")
          cat("NATIVE_PARAMETERS", LABEL,
            jsonlite::toJSON(list(rows = nrow(data), seed = seed, parameters = parameters,
              threads = attr(parameters, "autoxplain_threads")),
              auto_unbox = TRUE, null = "null", digits = 17), "\n")
        },
          list(LABEL = function_name)))
    }
    test_data <- evaluation$data
    test_data$y <- y_evaluation
    public_workflow <- identical(request, "public-tabular")
    if (public_workflow) {
      call_arguments <- list(data = train, target_column = "y", test_data = test_data,
        portfolio = "tabular", evaluation_role = "test",
        report = file.path(destination, "report.html"))
      for (function_name in c("prepare_model_report_data", "render_model_report")) {
        trace(function_name, where = asNamespace("AutoXplainR"), print = FALSE,
          tracer = substitute(cat("WORKFLOW_STAGE", LABEL, "enter", proc.time()[["elapsed"]], "\n"),
            list(LABEL = function_name)),
          exit = substitute(cat("WORKFLOW_STAGE", LABEL, "exit", proc.time()[["elapsed"]], "\n"),
            list(LABEL = function_name)))
      }
    } else {
      call_arguments <- list(data = train, target_column = "y", test_data = test_data,
        task = task, nfolds = 5L, seed = fit_seed,
        tuning_control = do.call(tuning_control, control_arguments),
        evaluation_role = "test", explain = FALSE)
      if (request == "paired") {
        call_arguments$learners <- c("forest", "boosting")
        call_arguments$tuning_rule <- "best"
      } else {
        call_arguments$portfolio <- "tabular"
      }
    }
    summary$public_request <- list(request = request, learners = call_arguments$learners,
      portfolio = call_arguments$portfolio, explicit_tuning_rule = call_arguments$tuning_rule,
      nfolds = call_arguments$nfolds, seed = call_arguments$seed,
      threads = if (public_workflow) NULL else threads, custom_grids = FALSE,
      explicit_arguments = names(call_arguments), default_explanations_and_report = public_workflow)
    fit_start <- proc.time()[["elapsed"]]
    result <- capture(do.call(autoxplain, call_arguments))
    if (public_workflow) {
      summary$workflow_elapsed_seconds <- proc.time()[["elapsed"]] - fit_start
      for (function_name in c("prepare_model_report_data", "render_model_report")) {
        untrace(function_name, where = asNamespace("AutoXplainR"))
      }
      stopifnot(identical(result$task, task), !is.null(result$explanations),
        file.exists(result$report_file), length(result$tuning$control$threads) == 1L,
        is.finite(result$tuning$control$threads), result$tuning$control$threads >= 1L,
        result$tuning$control$threads <= threads)
      summary$resolved_public_defaults <- list(seed = result$provenance$seed,
        nfolds = length(unique(result$tuning$fold_assignment$fold)),
        max_models = result$tuning$control$max_models,
        threads = result$tuning$control$threads, threads_requested = result$tuning$control$threads_requested,
        fold_source = result$tuning$control$fold_source, task = result$task,
        explanations = result$explanations$config)
      summary$report <- list(filename = basename(result$report_file),
        bytes = file.info(result$report_file)$size, sha256 = tabular_hash(result$report_file),
        scope = "Written inside the unmodified public autoxplain workflow, within the same process budget.")
      summary$explanation_feature_union <- result$explanations$audit$config$features
    } else {
      summary$fit_elapsed_seconds <- proc.time()[["elapsed"]] - fit_start
    }
    for (function_name in c("fit_forest_learner", "fit_boosting_learner")) {
      untrace(function_name, where = asNamespace("AutoXplainR"))
    }
    predictions <- lapply(names(result$models), function(id) {
      predict(result, evaluation$data, model = id)
    })
    names(predictions) <- names(result$models)
    summary$primary <- result$provenance$primary_model_id
    summary$final_configuration <- result$tuning$final_configuration
    summary$candidates <- result$tuning$candidates
    summary$fold_scores <- result$tuning$fold_scores
    summary$selection <- result$tuning$selection
    summary$search_plan <- result$tuning$plan
    summary$search_space <- result$tuning$search_space
    summary$input_policy <- result$tuning$input_policy
    summary$resources <- result$tuning$resources
    summary$search <- result$tuning$search
    if (!is.null(result$tuning$screening)) {
      summary$screening <- result$tuning$screening
    }
    summary$control <- result$tuning$control
    summary$refit <- result$tuning$refit
    summary$leaderboard <- result$leaderboard
    summary$metrics <- lapply(predictions, tabular_metrics, y = y_evaluation, task = task)
    saveRDS(result, file.path(destination, "model.rds"), compress = FALSE, version = 3L)
  } else {
    if (fixed_native) {
      records <- NULL
      selected <- native_reference$entry$source_selected_configuration
      settings <- list(parameters = native_reference$entry$parameters)
      cat("NATIVE_FIXED_REFERENCE", native_reference$protocol, "source",
        native_reference$entry$source_cohort, "configuration", selected, "new calibration fits 0\n")
    } else {
    calibration <- training$calibration_rows
    fitting <- setdiff(seq_len(nrow(train)), calibration)
    blueprint <- tabular_blueprint(train[fitting, features, drop = FALSE])
    x <- tabular_bake(train[fitting, features, drop = FALSE], blueprint, matrix = variant == "xgboost")
    x_cal <- tabular_bake(train[calibration, features, drop = FALSE], blueprint, matrix = variant == "xgboost")
    y <- train$y[fitting]
    y_cal <- train$y[calibration]
    settings <- if (variant == "xgboost") list(list(max_depth = 6L), list(max_depth = 10L)) else {
      lapply(unique(pmax(1L, c(floor(sqrt(length(features))), floor(length(features) / 3)))),
        function(mtry) list(mtry = mtry))
    }
    records <- vector("list", length(settings))
    for (i in seq_along(settings)) {
      cat("NATIVE_CALIBRATION", variant, i, "fit", length(fitting), "assess", length(calibration), "rows\n")
      started <- proc.time()[["elapsed"]]
      records[[i]] <- capture({
        if (variant == "xgboost") {
          stopifnot(as.character(packageVersion("xgboost")) == "3.2.1.1")
          parameters <- c(settings[[i]], list(eta = .05, min_child_weight = 1,
            subsample = .8, colsample_bytree = .8, lambda = 1, alpha = 0,
            tree_method = "hist", max_bin = 256L, nthread = threads,
            objective = switch(task, regression = "reg:squarederror", binary = "binary:logistic", multiclass = "multi:softprob"),
            eval_metric = switch(task, regression = "rmse", binary = "logloss", multiclass = "mlogloss"),
            seed = fit_seed, verbosity = 0L))
          if (task == "multiclass") parameters$num_class <- length(class_levels)
          numeric_y <- if (task == "regression") y else as.integer(y) - 1L
          numeric_cal <- if (task == "regression") y_cal else as.integer(y_cal) - 1L
          fit <- xgboost::xgb.train(params = parameters,
            data = xgboost::xgb.DMatrix(x, label = numeric_y, nthread = threads),
            nrounds = 1500L, evals = list(calibration = xgboost::xgb.DMatrix(x_cal, label = numeric_cal, nthread = threads)),
            early_stopping_rounds = 50L, maximize = FALSE, verbose = 0L)
          log <- as.data.frame(attr(fit, "evaluation_log"))
          best_rounds <- which.min(log[[2L]])
          prediction <- xgb_prediction(fit, x_cal, best_rounds)
          metrics <- tabular_metrics(y_cal, prediction, task)
          stopifnot(abs(tabular_primary(metrics, task) - log[[2L]][best_rounds]) < 1e-5)
          write.csv(log, file.path(destination, paste0("calibration-", i, "-rounds.csv")), row.names = FALSE)
        } else {
          parameters <- c(settings[[i]], list(num.trees = 500L, min.node.size = 5L,
            sample.fraction = .8, splitrule = if (task == "regression") "variance" else "gini"))
          fit <- do.call(ranger::ranger, c(list(x = x, y = y,
            probability = task != "regression", num.threads = threads,
            seed = fit_seed, respect.unordered.factors = "order", write.forest = TRUE), parameters))
          prediction <- predict(fit, data = x_cal, num.threads = threads)$predictions
          metrics <- tabular_metrics(y_cal, prediction, task)
          best_rounds <- NULL
        }
        list(parameters = parameters, selected_rounds = best_rounds,
          calibration_metrics = metrics, elapsed_seconds = proc.time()[["elapsed"]] - started)
      })
      calibration_path <- file.path(destination, paste0("calibration-", i, "-predictions.rds"))
      saveRDS(list(predictions = prediction, source_rows = training$source_rows[calibration]),
        calibration_path, compress = FALSE, version = 3L)
      records[[i]]$calibration_predictions_sha256 <- tabular_hash(calibration_path)
      tabular_json(records[seq_len(i)], file.path(destination, "calibration.json"))
      rm(fit)
      gc()
    }
    selected <- which.min(vapply(records, function(record) tabular_primary(record$calibration_metrics, task), numeric(1)))
    settings <- records[[selected]]
    }
    blueprint <- tabular_blueprint(train[features])
    x <- tabular_bake(train[features], blueprint, matrix = variant == "xgboost")
    x_eval <- if (!fit_only) tabular_bake(evaluation$data, blueprint, matrix = variant == "xgboost") else NULL
    cat("NATIVE_FINAL", variant, "fit", nrow(train), "rows\n")
    refit_start <- proc.time()[["elapsed"]]
    fit <- capture({
      if (variant == "xgboost") {
        y <- if (task == "regression") train$y else as.integer(train$y) - 1L
        model <- xgboost::xgb.train(params = settings$parameters,
          data = xgboost::xgb.DMatrix(x, label = y, nthread = threads),
          nrounds = settings$selected_rounds, verbose = 0L)
        if (!fit_only) predictions <- setNames(list(xgb_prediction(model, x_eval)), variant)
      } else {
        model <- do.call(ranger::ranger, c(list(x = x, y = train$y,
          probability = task != "regression", num.threads = threads,
          seed = fit_seed, respect.unordered.factors = "order", write.forest = TRUE), settings$parameters))
        stopifnot(model$num.samples == nrow(train))
        if (fixed_native) stopifnot(model$num.trees == 500L, model$forest$num.trees == 500L)
        if (!fit_only) predictions <- setNames(list(predict(model, data = x_eval, num.threads = threads)$predictions), variant)
      }
      model
    })
    summary$refit_elapsed_seconds <- proc.time()[["elapsed"]] - refit_start
    summary$calibration <- records
    summary$selected_configuration <- selected
    summary$selected_parameters <- settings
    summary$primary <- variant
    if (!fit_only) summary$metrics <- lapply(predictions, tabular_metrics, y = y_evaluation, task = task)
    summary$native_training_rows <- nrow(train)
    if (variant == "ranger") {
      summary$native_verified_training_rows <- fit$num.samples
      summary$native_verified_tree_count <- fit$num.trees
    }
    if (variant == "xgboost") summary$native_verified_boosting_rounds <- xgboost::xgb.get.num.boosted.rounds(fit)
    saveRDS(list(model = fit, blueprint = blueprint, task = task, class_levels = class_levels,
      family = variant, threads = threads), file.path(destination, "model.rds"), compress = FALSE, version = 3L)
  }
  if (!fit_only) saveRDS(predictions, file.path(destination, "predictions.rds"), compress = FALSE, version = 3L)
  summary$saved_model_bytes <- file.info(file.path(destination, "model.rds"))$size
  summary$saved_model_sha256 <- tabular_hash(file.path(destination, "model.rds"))
  if (!fit_only) summary$predictions_sha256 <- tabular_hash(file.path(destination, "predictions.rds"))
  if (fit_only) summary$evaluation_files_opened <- FALSE
  summary$status <- "ok"
}, error = function(error) {
  summary$status <<- "failed"
  summary$error <<- conditionMessage(error)
})
summary$elapsed_seconds <- proc.time()[["elapsed"]] - begin
summary$warnings <- unique(warnings)
summary$ended_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
tabular_json(summary, file.path(destination, "summary.json"))
cat(name, phase, variant, summary$status, "elapsed", summary$elapsed_seconds, "seconds\n")
if (summary$status != "ok") quit(status = 1L)
