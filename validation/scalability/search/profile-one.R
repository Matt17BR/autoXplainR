# One bounded process per case/solver. Invoke under timeout and one native thread.
arguments <- commandArgs(TRUE)
stopifnot(length(arguments) == 4L)
case_name <- arguments[[1L]]
solver <- match.arg(arguments[[2L]], c("gam", "bam", "bam_discrete"))
k <- as.integer(arguments[[3L]])
seed <- as.integer(arguments[[4L]])
output <- Sys.getenv("AXR_SEARCH_DIR", path.expand("~/.cache/autoxplain-scale-0.7.0/search"))
if (solver == "gam") {
  .libPaths(c(file.path(output, "baseline-library"), .libPaths()))
  library(AutoXplainR)
  stopifnot(as.character(packageVersion("AutoXplainR")) == "0.6.2")
} else if (nzchar(Sys.getenv("AXR_PROFILE_LIBRARY"))) {
  .libPaths(c(Sys.getenv("AXR_PROFILE_LIBRARY"), .libPaths()))
  library(AutoXplainR)
  stopifnot(as.character(packageVersion("AutoXplainR")) == "0.7.0")
} else pkgload::load_all(".", quiet = TRUE)
set.seed(seed)
if (case_name %in% c("bank", "friedman")) {
  name <- if (case_name == "bank") "bank" else "friedman_noise_benchmark"
  case <- readRDS(file.path(output, "solver-cases.rds"))[[name]]
} else if (case_name %in% c("rare_1pct", "regression_10000", "binary_10000")) {
  # Five folds leave exactly 10,000 training rows in the larger Gaussian case.
  n <- if (case_name == "rare_1pct") 2000L else 12500L
  p <- if (case_name == "rare_1pct") 10L else 5L
  data <- as.data.frame(matrix(rnorm(n * p), n, p))
  names(data) <- paste0("x", seq_len(p))
  data$group <- factor(sample(c("ordinary", "rare", "a:b"), n, TRUE, c(.7, .02, .28)))
  signal <- sin(data$x1) + .5 * data$x2^2 + .2 * data$x3 * data$x4 +
    .5 * (data$group == "rare")
  if (case_name == "regression_10000") {
    data$y <- signal + rnorm(n)
    task <- "regression"
  } else {
    intercept <- if (case_name == "rare_1pct") {
      uniroot(function(value) mean(plogis(value + signal)) - .01, c(-15, 5))$root
    } else -1
    probability <- plogis(intercept + signal)
    data$y <- factor(ifelse(runif(n) < probability, "yes", "no"), levels = c("no", "yes"))
    task <- "binary"
  }
  folds <- integer(n)
  strata <- if (task == "regression") list(seq_len(n)) else split(seq_len(n), data$y)
  for (rows in strata) folds[rows] <- sample(rep(1:5, length.out = length(rows)))
  case <- list(training = data, task = task, folds = folds)
} else stop("Unknown case")
fold <- AutoXplainR:::prepare_tuning_fold(case$training, "y", case$task, case$folds, 1L, TRUE, list())
parameters <- list(k = k, gamma = if (k == 10L) 1.4 else 1, select = TRUE)
if (solver != "gam") parameters <- c(parameters, list(solver = solver, discrete_bins = 10000L))
record <- list(case = case_name, solver = solver, seed = seed, parameters = parameters,
  timing_context = Sys.getenv("AXR_TIMING_CONTEXT", "exclusive single-process timing"),
  task = case$task, training_rows = nrow(fold$training), validation_rows = nrow(fold$validation),
  event_count = if (case$task == "binary") sum(fold$training$y == "yes") else NULL,
  package_version = as.character(packageVersion("AutoXplainR")),
  library = find.package("AutoXplainR"))
stem <- paste(case_name, seed, paste0("k", k), solver, sep = "-")
warnings <- character()
started <- proc.time()[["elapsed"]]
tryCatch(withCallingHandlers({
  model <- AutoXplainR:::fit_additive_learner(fold$training, "y", case$task, parameters, seed)
  record$fit_seconds <- proc.time()[["elapsed"]] - started
  prediction <- predict(model, fold$validation)
  y <- fold$validation$y
  stopifnot(all(is.finite(prediction)), length(prediction) == length(y))
  record$score <- if (case$task == "regression") sqrt(mean((y - prediction)^2)) else {
    p <- pmax(1e-15, pmin(1 - 1e-15, prediction))
    event <- as.integer(y == "yes")
    -mean(event * log(p) + (1 - event) * log1p(-p))
  }
  record$computation <- model$fit_details$computation
  record$coefficients <- length(coef(model$fit))
  record$optimization <- AutoXplainR:::model_optimization_record(model)
  record$status <- if (record$optimization$status == "converged") "ok" else "check_convergence"
  saveRDS(list(model = model, prediction = prediction, fold = fold), file.path(output, paste0(stem, ".rds")))
}, warning = function(w) {
  warnings <<- c(warnings, conditionMessage(w))
  invokeRestart("muffleWarning")
}), error = function(e) {
  record$status <<- "failed"
  record$error <<- conditionMessage(e)
})
record$elapsed_seconds <- proc.time()[["elapsed"]] - started
record$warnings <- warnings
jsonlite::write_json(record, file.path(output, paste0(stem, ".json")),
  auto_unbox = TRUE, pretty = TRUE, digits = 16, na = "null")
cat(stem, record$status, record$elapsed_seconds, "s\n")
if (record$status != "ok") quit(status = 1L)
