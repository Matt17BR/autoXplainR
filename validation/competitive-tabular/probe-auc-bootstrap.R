# Run in separate processes for each method. This is a synthetic evaluation
# bootstrap probe, not a model-fitting or competition-performance benchmark.
# Rscript validation/competitive-tabular/probe-auc-bootstrap.R optimized /path/to/output
# Rscript validation/competitive-tabular/probe-auc-bootstrap.R rerank /path/to/output
arguments <- commandArgs(trailingOnly = TRUE)
stopifnot(length(arguments) == 2L, arguments[[1L]] %in% c("optimized", "rerank"))
method <- arguments[[1L]]
output <- arguments[[2L]]
dir.create(output, recursive = TRUE, showWarnings = FALSE)
pkgload::load_all(quiet = TRUE)

set.seed(1941)
rows <- 102000L
data <- data.frame(x = rnorm(rows), z = rnorm(rows))
data$y <- factor(ifelse(runif(rows) < plogis(1.3 * data$x - .6 * data$z), "yes", "no"))
result <- autoxplain(data[seq_len(2000L), ], "y", test_data = data[-seq_len(2000L), ],
  learners = "linear", max_models = 1L, nfolds = 2L, explain = FALSE,
  tuning_control = tuning_control(metric = "auc", search = "grid")
)
prediction <- result$evaluation$predictions
truth <- as.character(result$test_data$y) == "yes"
n <- nrow(prediction)
n_boot <- 1000L
invisible(gc())
elapsed <- system.time({
  if (method == "optimized") {
    value <- performance_uncertainty(result, n_boot = n_boot, seed = 123L)
    draws <- as.matrix(value$draws)
    point <- value$estimates$estimate
    record <- value$bootstrap
  } else {
    point <- c(
      AutoXplainR:::selection_binary_auc(truth, prediction$primary_probability),
      AutoXplainR:::selection_binary_auc(truth, prediction$baseline_probability)
    )
    point <- c(point, point[[1L]] - point[[2L]])
    draws <- withr::with_seed(123L, t(replicate(n_boot, {
      sampled <- sample.int(n, n, replace = TRUE)
      primary <- AutoXplainR:::selection_binary_auc(truth[sampled], prediction$primary_probability[sampled])
      baseline <- AutoXplainR:::selection_binary_auc(truth[sampled], prediction$baseline_probability[sampled])
      c(primary = primary, baseline = baseline, difference = primary - baseline)
    })))
    record <- list(requested = n_boot, retained = nrow(draws), discarded = 0L)
  }
})[["elapsed"]]
stopifnot(all(is.finite(draws)), nrow(draws) == n_boot)
saveRDS(draws, file.path(output, paste0(method, "-draws.rds")))
sources <- c(
  "R/auc_bootstrap.R", "R/performance_uncertainty.R", "R/selection_metrics.R",
  "R/guided_workflow.R", "validation/competitive-tabular/probe-auc-bootstrap.R"
)
source_hashes <- setNames(vapply(sources, function(path) {
  digest::digest(file = path, algo = "sha256")
}, character(1)), sources)
summary <- list(
  scope = "Synthetic fixed-model evaluation bootstrap; 2,000 training rows and 100,000 evaluation rows.",
  method = method, evaluation_rows = n, requested_draws = n_boot,
  elapsed_seconds = elapsed,
  point_estimates = as.list(setNames(point, c("primary", "baseline", "difference"))),
  bootstrap = record,
  r_version = R.version.string, source_sha256 = as.list(source_hashes),
  prediction_sha256 = digest::digest(prediction[c("observed", "primary_probability", "baseline_probability")],
    algo = "sha256", serializeVersion = 2L
  ),
  warning = "This is a local cost probe of evaluation uncertainty. It does not establish fitting scalability."
)
jsonlite::write_json(summary, file.path(output, paste0(method, ".json")),
  pretty = TRUE, auto_unbox = TRUE, digits = NA
)
cat(method, ":", elapsed, "seconds;", n, "evaluation rows;", nrow(draws), "paired draws\n")
