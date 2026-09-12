# Run only from a bounded fresh process; see supervise.py.
args <- commandArgs(TRUE)
stopifnot(length(args) == 5L)
library_path <- normalizePath(args[[1L]])
problem <- args[[2L]]
n <- as.integer(args[[3L]])
mode <- args[[4L]]
output <- normalizePath(args[[5L]], mustWork = FALSE)
dir.create(output, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(library_path, .libPaths()))
library(AutoXplainR)
stopifnot(normalizePath(find.package("AutoXplainR")) == file.path(library_path, "AutoXplainR"))
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script <- normalizePath(sub("^--file=", "", script_arg))
source(file.path(dirname(script), "fixtures.R"))
started <- proc.time()[["elapsed"]]
events_file <- file.path(output, "stages.jsonl")
xgb_traced <- FALSE
stage_event <- function(stage, boundary, details = NULL) {
  status <- readLines("/proc/self/status", warn = FALSE)
  memory <- function(name) {
    line <- grep(paste0("^", name, ":"), status, value = TRUE)
    if (!length(line)) return(NA_real_)
    as.numeric(sub(paste0("^", name, ":\\s*([0-9]+).*"), "\\1", line))
  }
  line <- jsonlite::toJSON(c(list(
    stage = stage, boundary = boundary, elapsed = proc.time()[["elapsed"]] - started,
    rss_kib = memory("VmRSS"), peak_rss_kib = memory("VmHWM"), vsize_kib = memory("VmSize")
  ), details), auto_unbox = TRUE, null = "null")
  cat(line, "\n", file = events_file, append = TRUE)
  if (stage == "fit_boosting_learner" && boundary == "enter" && !xgb_traced) {
    xgb_traced <<- TRUE
    trace("xgb.train", where = asNamespace("xgboost"), print = FALSE,
      tracer = quote(.GlobalEnv$stage_event("xgb.train", "enter",
        list(training_rows = nrow(data), training_columns = ncol(data), training_matrix_class = class(data)))),
      exit = quote(.GlobalEnv$stage_event("xgb.train", "exit")))
  }
}
tracked <- c(
  "fit_guided_base", "check_evaluation_row_overlap", "preprocess_guided_split",
  "fit_base_candidates", "tune_supervised_candidates", "prepare_tuning_folds",
  "fit_regularized_learner", "fit_boosting_learner", "fit_linear_learner",
  "fit_tree_learner", "fit_neural_learner", "evaluate_candidates", "capture_data_context",
  "finalize_autoxplain", "prepare_model_report_data", "prepare_report_context",
  "render_model_report", "report_data_payload", "prepare_report_rows"
)
namespace <- asNamespace("AutoXplainR")
tracked <- tracked[vapply(tracked, exists, logical(1L), envir = namespace, inherits = FALSE)]
for (name in tracked) {
  trace(name, where = namespace, print = FALSE,
    tracer = bquote(.GlobalEnv$stage_event(.(name), "enter")),
    exit = bquote(.GlobalEnv$stage_event(.(name), "exit")))
}
metadata <- list(
  package = as.character(packageVersion("AutoXplainR")), library = find.package("AutoXplainR"),
  problem = problem, training_rows = n, evaluation_rows = 20000L, mode = mode,
  R = R.version.string, platform = R.version$platform, rng = RNGkind(),
  installed_code_md5 = as.list(tools::md5sum(file.path(find.package("AutoXplainR"),
    c("DESCRIPTION", "R/AutoXplainR.rdb", "R/AutoXplainR.rdx")))),
  engines = lapply(c("glmnet", "xgboost", "Matrix", "nnet", "rpart"), function(package) {
    list(package = package, version = as.character(packageVersion(package)))
  }),
  fixture_source_md5 = unname(tools::md5sum(file.path(dirname(script), "fixtures.R"))),
  runner_source_md5 = unname(tools::md5sum(script)),
  instrumented_stages = tracked, native_threads = 1L
)
jsonlite::write_json(metadata, file.path(output, "input.json"), pretty = TRUE, auto_unbox = TRUE)

run <- function() {
  stage_event("generate_data", "enter")
  training <- scale_fixture(problem, n)
  evaluation <- scale_fixture(problem, 20000L, evaluation = TRUE)
  jsonlite::write_json(list(
    training = training$provenance, evaluation = evaluation$provenance,
    training_bytes = as.numeric(object.size(training$data)),
    evaluation_bytes = as.numeric(object.size(evaluation$data)),
    training_classes = if (is.factor(training$data$outcome)) as.list(table(training$data$outcome)) else NULL,
    evaluation_classes = if (is.factor(evaluation$data$outcome)) as.list(table(evaluation$data$outcome)) else NULL
  ), file.path(output, "data.json"), pretty = TRUE, auto_unbox = TRUE)
  stopifnot(!anyNA(training$data$outcome), !anyNA(evaluation$data$outcome))
  stage_event("generate_data", "exit")
  full <- grepl("_full$", mode)
  arguments <- list(data = training$data, target_column = "outcome",
    test_data = evaluation$data, seed = 824L, explain = full,
    report = if (full) file.path(output, "report.html") else NULL)
  if (grepl("^controlled_", mode)) {
    arguments <- c(arguments, list(learners = c("regularized", "boosting"),
      nfolds = 2L, max_models = 2L,
      tuning_control = tuning_control(retain_oof = FALSE)))
  } else if (grepl("^stronger_", mode)) {
    # A separate capacity experiment, not a replacement for the fixed controls.
    # Both larger trees can represent the known interactions; parameters are
    # fixed before observing the independent evaluation losses.
    boosting_grid <- data.frame(nrounds = c(300L, 600L), eta = c(0.05, 0.05),
      max_depth = c(4L, 6L), min_child_weight = c(3, 5),
      subsample = c(0.8, 0.8), colsample_bytree = c(1, 1),
      reg_alpha = c(0, 0), reg_lambda = c(1, 1))
    arguments <- c(arguments, list(learners = c("regularized", "boosting"),
      nfolds = 2L, tuning_control = tuning_control(
        grids = list(boosting = boosting_grid),
        family_budgets = c(regularized = 1L, boosting = 2L), retain_oof = FALSE)))
  } else if (grepl("^quick_", mode)) {
    arguments$model_set <- "quick"
  } else {
    stopifnot(grepl("^default_", mode))
  }
  stage_event("public_autoxplain", "enter")
  fit_start <- proc.time()[["elapsed"]]
  result <- do.call(autoxplain, arguments)
  elapsed <- proc.time()[["elapsed"]] - fit_start
  stage_event("public_autoxplain", "exit")
  jsonlite::write_json(list(status = "public_call_returned", elapsed_seconds = elapsed,
    training_rows = result$provenance$training_rows, evaluation_rows = result$provenance$evaluation_rows,
    models = names(result$models), leaderboard = result$leaderboard),
    file.path(output, "public-call.json"), pretty = TRUE, auto_unbox = TRUE, na = "null")
  stopifnot(nrow(result$training_data) == n, nrow(result$test_data) == 20000L,
            result$provenance$training_rows == n, result$provenance$evaluation_rows == 20000L)
  native_rows <- lapply(result$models, function(model) {
    if (inherits(model, "autoxplain_tuned_nnet")) {
      count <- NROW(model$model$fitted.values)
      stopifnot(count == n)
      return(list(native_fitted_rows = count))
    }
    if (inherits(model, "autoxplain_fitted_model")) {
      if (model$family %in% c("regularized", "boosting")) {
        recorded_rows <- model$blueprint$training_rows
        if (is.null(recorded_rows)) recorded_rows <- model$fit_details$computation$rows
        stopifnot(identical(recorded_rows, n))
      }
      count <- if (is.list(model$fit)) model$fit$nobs else NULL
      if (!is.null(count)) stopifnot(count == n)
      return(list(blueprint_rows = model$blueprint$training_rows,
                  native_nobs = count))
    }
    count <- tryCatch(stats::nobs(model), error = function(error) {
      if (!is.null(model$fitted.values)) NROW(model$fitted.values) else NULL
    })
    if (!is.null(count)) stopifnot(count == n)
    list(native_nobs = count)
  })
  stage_records <- lapply(readLines(events_file), jsonlite::fromJSON)
  training_matrices <- Filter(function(event) {
    event$stage == "xgb.train" && event$boundary == "enter"
  }, stage_records)
  retains_boosting <- any(vapply(result$models, function(model) {
    inherits(model, "autoxplain_fitted_model") && identical(model$family, "boosting")
  }, logical(1L)))
  if (retains_boosting) {
    actual_rows <- vapply(training_matrices, function(event) event$training_rows, numeric(1L))
    stopifnot(length(actual_rows) > 0L, any(actual_rows == n))
  }
  if (!is.null(result$tuning)) {
    candidates <- result$tuning$candidates
    successful <- candidates$configuration_id[candidates$status == "ok"]
    stopifnot(length(successful) > 0L,
              all(candidates$evaluated_rows[candidates$status == "ok"] == n))
    scores <- result$tuning$fold_scores
    for (configuration in successful) {
      rows <- scores[scores$configuration_id == configuration, ]
      stopifnot(sum(rows$validation_rows) == n,
                all(rows$training_rows + rows$validation_rows == n))
    }
    if (grepl("^(controlled|stronger)_", mode)) stopifnot(is.null(result$tuning$out_of_fold_predictions))
    saveRDS(list(candidates = candidates, fold_scores = scores, plan = result$tuning$plan,
                 fold_preprocessing = result$tuning$fold_preprocessing,
                 control = result$tuning$control, input_policy = result$tuning$input_policy,
                 refit = result$tuning$refit),
            file.path(output, "tuning-evidence.rds"))
    jsonlite::write_json(list(candidates = candidates, fold_scores = scores,
      control = result$tuning$control, input_policy = result$tuning$input_policy), file.path(output, "tuning.json"),
      pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null")
  }
  stage_event("verify_full_holdout", "enter")
  metrics <- lapply(names(result$models), function(id) {
    prediction <- predict(result, evaluation$data, model = id)
    stopifnot(NROW(prediction) == 20000L, all(is.finite(prediction)))
    # XGBoost returns float32 probabilities. Enforce the public probability
    # contract (1e-6), while retaining the actual normalization error.
    sum_error <- if (is.matrix(prediction)) max(abs(rowSums(prediction) - 1)) else NULL
    if (!is.null(sum_error)) stopifnot(sum_error <= 1e-6)
    loss <- independent_scale_loss(evaluation$data$outcome, prediction)
    key <- if (result$task == "regression") "rmse" else "log_loss"
    reported <- unname(result$evaluation$metrics[[id]][[key]])
    stopifnot(isTRUE(all.equal(loss, reported, tolerance = 1e-10)))
    list(model_id = id, metric = key, loss = loss, recorded_loss = reported,
         maximum_probability_sum_error = sum_error)
  })
  sampling <- if (full) result$explanations$audit$config$sampling else NULL
  if (!is.null(sampling)) {
    stopifnot(sampling$rows_available == 20000L,
      sampling$rows_used == length(sampling$row_indices),
      !anyDuplicated(sampling$row_indices),
      all(sampling$row_indices %in% seq_len(20000L)))
    expected <- min(20000L, result$explanations$config$explanation_rows)
    stopifnot(sampling$rows_used == expected)
    for (id in names(result$explanations$audit$importance_objects)) {
      importance <- result$explanations$audit$importance_objects[[id]]
      stopifnot(identical(attr(importance, "sampling")$row_indices, sampling$row_indices))
      metric <- result$explanations$audit$config$metric
      stopifnot(isTRUE(all.equal(attr(importance, "full_baseline_score"),
        unname(result$evaluation$metrics[[id]][[metric]]), tolerance = 1e-10)))
    }
    sampling$row_indices <- NULL
    if (!is.null(sampling$class_counts)) sampling$class_counts <- as.list(sampling$class_counts)
  }
  oracle <- independent_scale_loss(evaluation$data$outcome, evaluation$truth)
  stage_event("verify_full_holdout", "exit")
  stage_event("save_model", "enter")
  probe <- evaluation$data[unique(c(1:32, seq(1000, 20000, length.out = 32))), , drop = FALSE]
  expected <- lapply(names(result$models), function(id) predict(result, probe, model = id))
  names(expected) <- names(result$models)
  saveRDS(list(probe = probe, expected = expected, task = result$task), file.path(output, "replay.rds"))
  saveRDS(result, file.path(output, "result.rds"), compress = FALSE)
  stage_event("save_model", "exit")
  list(status = "passed", public_call_seconds = elapsed, metrics = metrics,
    oracle_loss_before_missingness = oracle, training_rows = n, evaluation_rows = 20000L,
    predictors = length(result$features), models = length(result$models),
    object_bytes = as.numeric(object.size(result)),
    saved_bytes = file.info(file.path(output, "result.rds"))$size,
    report_bytes = if (full) file.info(file.path(output, "report.html"))$size else NULL,
    full_training_and_holdout_verified = TRUE,
    native_training_rows = native_rows, explanation_sampling = sampling,
    fitted_model_configuration = lapply(result$models, function(model) {
      if (inherits(model, "autoxplain_tuned_nnet")) {
        return(list(family = "neural", backend = "nnet", size = model$size,
          decay = model$decay, maxit = model$maxit, native_convergence = model$model$convergence))
      }
      if (!inherits(model, "autoxplain_fitted_model")) return(NULL)
      list(family = model$family, backend = model$backend, parameters = model$parameters,
        computation = model$fit_details$computation,
        native_call_inputs = model$fit_details$native_call_inputs)
    }),
    actual_xgboost_training_matrices = training_matrices,
    tuning_input_policy = result$tuning$input_policy,
    configurations_attempted = nrow(result$tuning$candidates),
    configurations_failed = if (!is.null(result$tuning)) {
      result$tuning$candidates[result$tuning$candidates$status != "ok",
        c("configuration_id", "family", "hyperparameters", "folds_completed", "optimization_issues", "status"),
        drop = FALSE]
    } else NULL,
    selected_configuration = result$tuning$selected_configuration,
    final_configuration = result$tuning$final_configuration,
    families_resampling_failed = result$tuning$families_resampling_failed,
    families_refit_failed = result$tuning$refit$families_refit_failed)
}
answer <- tryCatch(run(), error = function(error) {
  stage_event("caught_error", "exit")
  list(status = "error", message = conditionMessage(error), call = deparse(conditionCall(error)))
})
jsonlite::write_json(answer, file.path(output, "result.json"), pretty = TRUE, auto_unbox = TRUE,
                     null = "null", na = "null")
writeLines(capture.output(sessionInfo()), file.path(output, "session-info.txt"))
quit(status = if (answer$status == "passed") 0L else 1L)
