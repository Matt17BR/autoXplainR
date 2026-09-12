# One public recommended-portfolio fit. The final test set only scores the
# training-selected model and retained alternatives; it never chooses settings.
arguments <- commandArgs(TRUE)
stopifnot(length(arguments) == 2L)
scenario <- match.arg(arguments[[1L]], c("friedman_noise", "bank_marketing"))
variant <- match.arg(arguments[[2L]], c("baseline", "candidate", "candidate_binary_guard", "candidate_fixed_gam"))
output <- Sys.getenv("AXR_SEARCH_DIR", path.expand("~/.cache/autoxplain-scale-0.7.0/search"))
library_path <- file.path(output, if (variant == "baseline") "baseline-library" else if
  (variant == "candidate_binary_guard") "candidate-binary-guard-library" else "candidate-library")
.libPaths(c(library_path, .libPaths()))
library(AutoXplainR)
stopifnot(as.character(packageVersion("AutoXplainR")) == if (variant == "baseline") "0.6.2" else "0.7.0")
cache <- path.expand("~/.cache/autoxplain-stress-0.6.2/benchmark")
case_path <- file.path(cache, "cases", paste0(scenario, ".rds"))
case <- readRDS(case_path)
plan <- jsonlite::read_json(file.path(cache, "benchmark-plan.json"))
declared <- Filter(function(item) identical(item$name, scenario), plan$cases)
stopifnot(length(declared) == 1L, identical(declared[[1L]]$sha256,
  digest::digest(file = case_path, algo = "sha256")))
destination <- file.path(output, "recommended", variant, scenario)
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
stopifnot(!file.exists(file.path(destination, "summary.json")))
writeLines(capture.output(sessionInfo()), file.path(destination, "session-info.txt"))
grids <- if (variant == "candidate_fixed_gam") list(additive = data.frame(
  k = c(5L, 8L, 5L, 8L, 10L), gamma = c(1, 1, 1.4, 1, 1.4),
  select = c(TRUE, TRUE, TRUE, FALSE, TRUE), solver = "gam")) else NULL
control <- tuning_control(grids = grids, fold_ids = case$folds)
record <- list(case = scenario, variant = variant,
  package_version = as.character(packageVersion("AutoXplainR")), library = find.package("AutoXplainR"),
  case_sha256 = declared[[1L]]$sha256, seed = case$fit_seed)
warnings <- character()
started <- proc.time()[["elapsed"]]
tryCatch(withCallingHandlers({
  result <- autoxplain(case$training, "y", test_data = case$evaluation, task = case$task,
    portfolio = "recommended", tuning_control = control, evaluation_role = "test",
    explain = FALSE, seed = case$fit_seed)
  record$fit_seconds <- proc.time()[["elapsed"]] - started
  # Persist the call timer before prediction, summarization, or serialization of
  # complex evidence objects. A later bookkeeping failure must not lose it.
  jsonlite::write_json(list(case = scenario, variant = variant,
    fit_seconds = record$fit_seconds, package_version = record$package_version),
    file.path(destination, "fit-timer.json"), auto_unbox = TRUE, pretty = TRUE, digits = 16)
  saveRDS(list(result = result, case = case, record = record), file.path(destination, "result.rds"))
  predictions <- lapply(names(result$models), function(id) predict(result, case$evaluation, model = id))
  names(predictions) <- names(result$models)
  scores <- lapply(predictions, function(p) {
    y <- case$evaluation$y
    stopifnot(length(p) == length(y), all(is.finite(p)))
    if (case$task == "regression") return(list(rmse = sqrt(mean((y - p)^2)), mae = mean(abs(y - p))))
    event <- as.integer(y == "yes")
    stopifnot(all(p >= 0 & p <= 1))
    bounded <- pmax(1e-15, pmin(1 - 1e-15, p))
    list(log_loss = -mean(event * log(bounded) + (1 - event) * log1p(-bounded)),
      brier = mean((p - event)^2),
      auc = (sum(rank(p)[event == 1L]) - sum(event) * (sum(event) + 1) / 2) /
        (sum(event) * sum(1 - event)))
  })
  record$metrics <- scores
  record$primary <- result$provenance$primary_model_id
  record$selected_configuration <- result$tuning$final_configuration
  record$model_count <- length(result$models)
  record$configuration_count <- nrow(result$tuning$candidates)
  record$configuration_status <- as.list(table(result$tuning$candidates$status))
  record$candidates <- result$tuning$candidates[, c("configuration_id", "family",
    "hyperparameters", "cv_score", "folds_completed", "selected", "status")]
  record$input_policy <- result$tuning$input_policy
  record$refit <- result$tuning$refit
  folds <- result$tuning$fold_scores
  record$failed_folds <- folds[!is.finite(folds$score), c("configuration_id", "fold",
    "optimization_status", "optimization_message", "warning", "error")]
  families <- result$tuning$candidates$family[match(folds$configuration_id,
    result$tuning$candidates$configuration_id)]
  record$fold_fit_seconds_by_family <- aggregate(folds$elapsed_ms / 1000,
    list(family = families), sum)
  record$status <- "ok"
  saveRDS(list(result = result, case = case, record = record), file.path(destination, "result.rds"))
  saveRDS(predictions, file.path(destination, "predictions.rds"))
}, warning = function(w) {
  warnings <<- c(warnings, conditionMessage(w))
  invokeRestart("muffleWarning")
}), error = function(e) {
  record$status <<- "failed"
  record$error <<- conditionMessage(e)
})
record$elapsed_seconds <- proc.time()[["elapsed"]] - started
record$warnings <- as.list(unname(warnings))
jsonlite::write_json(record, file.path(destination, "summary.json"),
  pretty = TRUE, auto_unbox = TRUE, digits = 16, null = "null", na = "null")
cat(scenario, variant, record$status, record$elapsed_seconds, "seconds\n")
if (record$status != "ok") quit(status = 1L)
