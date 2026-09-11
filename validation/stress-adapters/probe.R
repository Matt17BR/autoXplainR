# Bounded adapter stress probes. Run from the package root with Rscript.
# Failures are recorded rather than treated as success; inspect summary.json.
options(warn = 1)
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
library_path <- Sys.getenv("AXR_ADAPTER_LIBRARY")
if (nzchar(library_path)) {
  .libPaths(c(library_path, .libPaths()))
  library(AutoXplainR)
} else {
  pkgload::load_all(quiet = TRUE)
}
output <- Sys.getenv("AXR_ADAPTER_OUTPUT", path.expand("~/.cache/autoxplain-stress-0.6.2/adapters/before"))
dir.create(output, recursive = TRUE, showWarnings = FALSE)
set.seed(71502)

make_regression <- function(n = 180L, p = 5L) {
  data <- as.data.frame(matrix(rnorm(n * p), n, p))
  names(data) <- paste0("x", seq_len(p))
  data$y <- 2 * sin(data$x1) + data$x2^2 + rnorm(n, sd = 0.3)
  data
}
regular <- make_regression()
wide <- make_regression(75L, 100L)
correlated <- regular
correlated$x_copy <- correlated$x1
correlated$x_nearly <- correlated$x1 + rnorm(nrow(correlated), sd = 1e-12)
correlated$constant <- 1
correlated$constant_factor <- factor(rep("only", nrow(correlated)))
categorical <- make_regression(420L)
categorical$category <- factor(sample(sprintf("level%03d", 1:180), 420L, replace = TRUE))
rare <- make_regression(150L)
rare$y <- factor(c(rep("common", 146L), rep("rare", 4L)))
multi <- make_regression(180L)
multi$y <- factor(c(rep("a", 85L), rep("b", 85L), rep("c", 10L)))
discrete <- regular
for (name in setdiff(names(discrete), "y")) discrete[[name]] <- round(discrete[[name]])
large_scale <- regular
large_scale$x1 <- large_scale$x1 * 1e160
small_scale <- regular
small_scale$x1 <- small_scale$x1 * 1e-160
large_outcome <- regular
large_outcome$y <- large_outcome$y * 1e30
cases <- list(
  high_dimension = wide, correlated_and_constant = correlated,
  many_factor_levels = categorical, rare_binary = rare,
  rare_multiclass = multi, low_unique_numeric = discrete,
  large_predictor_units = large_scale, small_predictor_units = small_scale,
  large_outcome_units = large_outcome
)
only_cases <- Sys.getenv("AXR_ADAPTER_CASES")
if (nzchar(only_cases)) cases <- cases[strsplit(only_cases, ",", fixed = TRUE)[[1L]]]
trace(
  "fit_tuning_configuration", where = asNamespace("AutoXplainR"), print = FALSE,
  tracer = quote(cat(
    format(Sys.time(), "%H:%M:%S"), "fitting", configuration$family[[1L]],
    "with", nrow(data), "rows and", ncol(data) - 1L, "inputs\n"
  ))
)
if (identical(Sys.getenv("AXR_ADAPTER_PROFILE"), "true")) {
  Rprof(file.path(output, "Rprof.out"), interval = 0.02, memory.profiling = TRUE)
}
families <- c("regularized", "additive", "forest", "boosting", "kernel", "neighbors", "mars")
records <- list()
for (case in names(cases)) {
  data <- cases[[case]]
  selected <- families
  if (case == "rare_multiclass") selected <- setdiff(selected, c("additive", "mars"))
  start <- proc.time()[["elapsed"]]
  cat("Starting", case, "with", nrow(data), "rows and", ncol(data) - 1L, "predictors\n")
  warnings <- character()
  result <- tryCatch(withCallingHandlers(autoxplain(
    data, "y", learners = selected, max_models = length(selected),
    nfolds = 3L, seed = 5103L, explain = FALSE,
    task = if (case == "rare_binary") "binary" else if (case == "rare_multiclass") "multiclass" else "regression"
  ), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }), error = identity)
  elapsed <- proc.time()[["elapsed"]] - start
  if (inherits(result, "error")) {
    record <- list(
      case = case, success = FALSE, elapsed_seconds = elapsed,
      error = conditionMessage(result), warnings = unique(warnings)
    )
  } else {
    path <- file.path(output, paste0(case, ".rds"))
    saveRDS(result, path)
    reloaded <- readRDS(path)
    checks <- lapply(names(result$models), function(model) {
      first <- tryCatch(predict(result, data[seq_len(11L), ], model = model), error = identity)
      second <- tryCatch(predict(reloaded, data[seq_len(11L), ], model = model), error = identity)
      list(
        model = model, prediction_success = !inherits(first, "error"),
        error = if (inherits(first, "error")) conditionMessage(first) else NULL,
        finite = if (inherits(first, "error")) FALSE else all(is.finite(first)),
        saved_prediction_identical = identical(first, second),
        prediction_range = if (inherits(first, "error")) NULL else range(first)
      )
    })
    record <- list(
      case = case, success = TRUE, elapsed_seconds = elapsed,
      retained_models = names(result$models), warnings = unique(warnings),
      leaderboard = result$leaderboard,
      candidates = result$tuning$candidates,
      folds = result$tuning$fold_scores,
      prediction_checks = checks,
      source_result = normalizePath(path), rds_bytes = file.info(path)$size
    )
  }
  records[[case]] <- record
  jsonlite::write_json(
    records, file.path(output, "summary.json"), pretty = TRUE,
    auto_unbox = TRUE, null = "null", na = "null", digits = 16
  )
  cat(case, "completed:", record$success, "in", round(elapsed, 3), "seconds\n")
}
writeLines(capture.output(sessionInfo()), file.path(output, "session-info.txt"))
Rprof(NULL)
