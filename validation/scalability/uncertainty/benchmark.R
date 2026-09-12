# Run through supervise.py. Both variants use the same installed package;
# "before" substitutes only the frozen pre-optimization public function.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 6L)
library_path <- normalizePath(args[[1L]], mustWork = TRUE)
variant <- match.arg(args[[2L]], c("before", "after"))
problem <- match.arg(args[[3L]], c("regression", "multiclass_brier"))
n <- as.integer(args[[4L]])
output <- normalizePath(args[[5L]], mustWork = TRUE)
reference_file <- normalizePath(args[[6L]], mustWork = TRUE)
stopifnot(n %in% c(20000L, 200000L, 1000000L))
.libPaths(c(library_path, .libPaths()))
library(AutoXplainR)
stopifnot(identical(normalizePath(find.package("AutoXplainR")),
  normalizePath(file.path(library_path, "AutoXplainR"))))
stopifnot(unname(tools::md5sum(reference_file)) == "72817afcf96f6cc8cf59940135bb10e7")
before <- new.env(parent = asNamespace("AutoXplainR"))
sys.source(reference_file, envir = before)
bootstrap <- if (variant == "before") before$performance_uncertainty else performance_uncertainty
index <- seq_len(n)
data <- data.frame(x = sin(index / 101), z = cos(index / 59))
if (problem == "regression") {
  data$outcome <- 2 * data$x + data$z^2 + sin(index / 17) * 0.7
  models <- list(primary = list(slope = 1.8, intercept = 0.5), reference = list(slope = 0, intercept = 0.5))
  adapter <- function(model, newdata) model$intercept + model$slope * newdata$x
  metric <- "rmse"
} else {
  classes <- paste0("class", seq_len(8L))
  data$outcome <- factor(classes[(index %% 8L) + 1L], levels = classes)
  models <- list(primary = list(scale = 1.5, classes = classes), reference = list(scale = 0, classes = classes))
  adapter <- function(model, newdata) {
    probability <- vapply(seq_along(model$classes), function(k) {
      exp(model$scale * (sin(k) * newdata$x + cos(k) * newdata$z))
    }, numeric(nrow(newdata)))
    probability <- probability / rowSums(probability)
    colnames(probability) <- model$classes
    probability
  }
  metric <- "brier_score"
}
preparation_seconds <- system.time({
  result <- evaluate_models(models, data, "outcome",
    predict_functions = list(primary = adapter, reference = adapter), reference = "reference")
  result$evaluation$primary_metric <- metric
  result <- AutoXplainR:::seal_evaluation_result(result)
})[["elapsed"]]
jsonlite::write_json(list(stage = "prepared", rows = n, metric = metric,
  preparation_seconds = preparation_seconds), file.path(output, "prepared.json"), auto_unbox = TRUE, pretty = TRUE)
gc()
set.seed(984L)
rng <- .Random.seed
seconds <- system.time(interval <- bootstrap(result, n_boot = 1000L, seed = 123L))[["elapsed"]]
stopifnot(identical(.Random.seed, rng), interval$units == n, nrow(interval$draws) == 1000L)
saveRDS(interval, file.path(output, "interval.rds"), version = 3L)
measurement <- list(variant = variant, problem = problem, rows = n, draws = 1000L,
  public_call_seconds = seconds, preparation_seconds = preparation_seconds,
  metric = metric, unit = interval$unit, units = interval$units,
  estimates = interval$estimates, package_version = as.character(packageVersion("AutoXplainR")),
  package_path = normalizePath(find.package("AutoXplainR")),
  reference_md5 = unname(tools::md5sum(reference_file)),
  runner_md5 = unname(tools::md5sum(sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())]))),
  bootstrap_body = deparse(body(bootstrap)), session = capture.output(sessionInfo()))
jsonlite::write_json(measurement, file.path(output, "measurement.json"), pretty = TRUE, auto_unbox = TRUE, digits = NA)
cat(jsonlite::toJSON(measurement[c("variant", "problem", "rows", "public_call_seconds")], auto_unbox = TRUE), "\n")
