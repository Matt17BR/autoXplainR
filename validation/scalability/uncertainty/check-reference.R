# Functional verification only. Never run alongside a timed workload.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L)
source_directory <- normalizePath(args[[1L]], mustWork = TRUE)
reference_file <- normalizePath(args[[2L]], mustWork = TRUE)
output_file <- args[[3L]]
stopifnot(unname(tools::md5sum(reference_file)) == "72817afcf96f6cc8cf59940135bb10e7")
pkgload::load_all(source_directory, quiet = TRUE)
namespace <- asNamespace("AutoXplainR")
before <- new.env(parent = namespace)
sys.source(reference_file, envir = before)

# Load only the literal-data fixture/oracle functions, not the test bodies.
fixtures <- new.env(parent = globalenv())
fixture_file <- file.path(source_directory, "tests/testthat/test-uncertainty-contributions.R")
for (expression in parse(fixture_file)) {
  if (is.call(expression) && identical(expression[[1L]], quote(`<-`)) &&
      as.character(expression[[2L]]) %in% c("uncertainty_probability_fixture", "literal_bootstrap_score")) {
    eval(expression, envir = fixtures)
  }
}
capture_run <- function(fun, result) {
  state <- new.env(parent = namespace)
  state$drawn_units <- list()
  state$metric_calls <- 0L
  state$prediction_batches <- 0L
  state$sample.int <- function(...) {
    value <- base::sample.int(...)
    state$drawn_units[[length(state$drawn_units) + 1L]] <- value
    value
  }
  state$metric_score <- function(...) {
    state$metric_calls <- state$metric_calls + 1L
    get("metric_score", envir = namespace)(...)
  }
  state$report_predictions <- function(...) {
    state$prediction_batches <- state$prediction_batches + 1L
    get("report_predictions", envir = namespace)(...)
  }
  environment(fun) <- state
  set.seed(984L)
  rng <- .Random.seed
  value <- fun(result, n_boot = 37L, seed = 123L, confidence = 0.95)
  stopifnot(identical(rng, .Random.seed), length(state$drawn_units) == 37L,
    state$prediction_batches == 1L)
  list(value = value, units = state$drawn_units, metric_calls = state$metric_calls,
    prediction_batches = state$prediction_batches)
}
verdicts <- list()
for (task in c("regression", "binary", "multiclass")) {
  fixture <- fixtures$uncertainty_probability_fixture(task)
  metrics <- if (task == "regression") c("rmse", "mae") else c("log_loss", "brier_score")
  for (grouped in c(FALSE, TRUE)) {
    result <- fixture$result
    labels <- if (grouped) rep(c("site-z", "site-a", "site-9", "site-b"), c(3, 9, 7, 28)) else NULL
    if (grouped) result$validation <- list(method = "group", evaluation_groups = labels,
      evaluation_row_names = rownames(result$test_data))
    for (metric in metrics) {
      result$evaluation$primary_metric <- metric
      result <- AutoXplainR:::seal_evaluation_result(result)
      old <- capture_run(before$performance_uncertainty, result)
      new <- capture_run(performance_uncertainty, result)
      stopifnot(identical(old$value, new$value), identical(old$units, new$units),
        old$metric_calls == 76L, new$metric_calls == 0L)
      oracle <- t(vapply(new$units, function(draw) {
        rows <- if (grouped) unlist(lapply(unique(labels)[draw], function(label) which(labels == label)),
          use.names = FALSE) else draw
        fixtures$literal_bootstrap_score(rows, fixture, metric)
      }, numeric(3L)))
      colnames(oracle) <- c("primary", "baseline", "difference")
      stopifnot(identical(new$value$draws, as.data.frame(oracle)))
      verdicts[[length(verdicts) + 1L]] <- list(task = task, metric = metric, grouped = grouped,
        rows = length(fixture$outcome), draws = nrow(new$value$draws), exact_output = TRUE,
        exact_sampled_units = TRUE, independent_literal_rows = TRUE, caller_rng_preserved = TRUE,
        old_metric_calls = old$metric_calls, new_metric_calls = new$metric_calls,
        prediction_batches = new$prediction_batches)
    }
  }
}
jsonlite::write_json(list(status = "passed", reference_md5 = unname(tools::md5sum(reference_file)),
  source_md5 = as.list(tools::md5sum(file.path(source_directory,
    c("R/performance_uncertainty.R", "R/permutation_importance.R", "tests/testthat/test-uncertainty-contributions.R")))),
  cases = verdicts, session = capture.output(sessionInfo())), output_file,
  pretty = TRUE, auto_unbox = TRUE, digits = NA)
cat("All 12 cases preserve every draw, interval, note and sampling-unit sequence exactly.\n")
