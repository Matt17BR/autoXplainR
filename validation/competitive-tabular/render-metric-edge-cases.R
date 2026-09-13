# Reproduce undefined evaluation scores without losing the fitted models or
# their reports. Run from any directory; an optional argument sets the output.
# Rscript validation/competitive-tabular/render-metric-edge-cases.R [output_directory]
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) > 1L) {
  stop("Usage: Rscript render-metric-edge-cases.R [output_directory]", call. = FALSE)
}
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
project <- normalizePath(file.path(dirname(script), "../.."), mustWork = TRUE)
output <- if (length(arguments)) {
  arguments[[1L]]
} else {
  "~/.cache/autoxplain-tabular-0.8.0/integration"
}
dir.create(path.expand(output), recursive = TRUE, showWarnings = FALSE)
output <- normalizePath(path.expand(output), mustWork = TRUE)
setwd(project)

source_files <- sort(c(
  list.files("R", pattern = "[.]R$", full.names = TRUE),
  list.files("inst/report", full.names = TRUE),
  "DESCRIPTION", "validation/competitive-tabular/render-metric-edge-cases.R"
))
hash_files <- function(paths) {
  stats::setNames(vapply(paths, function(path) {
    digest::digest(file = path, algo = "sha256")
  }, character(1)), paths)
}
source_hashes <- hash_files(source_files)
pkgload::load_all(project, quiet = TRUE)

# A linear fit can score valid nonnegative training outcomes during CV, then
# extrapolate to negative predictions on otherwise valid evaluation rows.
rmsle_training <- data.frame(x = seq(1, 10, length.out = 80L))
rmsle_training$y <- 2 + rmsle_training$x + sin(rmsle_training$x) / 10
rmsle_evaluation <- data.frame(x = c(-20, -15, -10), y = c(3, 4, 5))
rmsle <- autoxplain(
  rmsle_training, "y", test_data = rmsle_evaluation, learners = "linear",
  max_models = 1L, nfolds = 2L, seed = 123L, explain = FALSE,
  tuning_control = tuning_control(metric = "rmsle", search = "grid")
)

# Both classes occur in the training folds. A held-out slice can nevertheless
# contain only one, leaving its AUC undefined while log loss remains usable.
auc_training <- data.frame(
  x = seq_len(100L), y = factor(ifelse(seq_len(100L) %% 5L == 0L, "yes", "no"))
)
auc_evaluation <- data.frame(
  x = seq(1.5, 30.5, length.out = 30L), y = factor(rep("no", 30L), levels = c("no", "yes"))
)
auc <- autoxplain(
  auc_training, "y", test_data = auc_evaluation, learners = "tree",
  max_models = 1L, nfolds = 2L, seed = 123L, explain = FALSE,
  tuning_control = tuning_control(metric = "auc", search = "grid")
)
stopifnot(
  all(predict(rmsle, rmsle_evaluation) < 0),
  all(is.finite(rmsle$tuning$fold_scores$score)),
  is.na(rmsle$evaluation$metrics$main_model[["rmsle"]]),
  is.finite(rmsle$evaluation$metrics$main_model[["rmse"]]),
  all(is.finite(auc$tuning$fold_scores$score)),
  is.na(auc$evaluation$metrics$main_model[["roc_auc"]]),
  is.finite(auc$evaluation$metrics$main_model[["log_loss"]]),
  is.na(rmsle$evaluation$beats_baseline), is.na(auc$evaluation$beats_baseline)
)

reports <- c(rmsle = "rmsle-unavailable-report.html", auc = "auc-unavailable-report.html")
results <- list(rmsle = rmsle, auc = auc)
for (name in names(results)) {
  render_model_report(results[[name]], file.path(output, reports[[name]]), n_repeats = 2L)
}
inputs <- list(
  rmsle = list(training = rmsle_training, evaluation = rmsle_evaluation),
  auc = list(training = auc_training, evaluation = auc_evaluation)
)
saveRDS(results, file.path(output, "unavailable-metric-results.rds"), version = 2L)
saveRDS(inputs, file.path(output, "metric-edge-case-inputs.rds"), version = 2L)
dput(inputs, file = file.path(output, "metric-edge-case-inputs.R"),
  control = c("keepNA", "keepInteger", "niceNames", "showAttributes", "hexNumeric")
)
invisible(file.copy("validation/competitive-tabular/render-metric-edge-cases.R",
  file.path(output, "render-metric-edge-cases.R"), overwrite = TRUE
))
if (!identical(source_hashes, hash_files(source_files))) {
  stop("Package sources changed while rendering. Rerun to produce a consistent manifest.", call. = FALSE)
}

artifact_names <- c(
  unname(reports), "unavailable-metric-results.rds", "metric-edge-case-inputs.rds",
  "metric-edge-case-inputs.R", "render-metric-edge-cases.R"
)
artifacts <- lapply(artifact_names, function(name) {
  path <- file.path(output, name)
  list(file = name, bytes = file.info(path)$size, sha256 = digest::digest(file = path, algo = "sha256"))
})
manifest <- list(
  status = "Development report-domain regression fixtures, not performance benchmarks.",
  generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  command = "Rscript validation/competitive-tabular/render-metric-edge-cases.R [output_directory]",
  fit_seed = 123L, report_permutations = 2L,
  r_version = R.version.string,
  session_info = utils::capture.output(utils::sessionInfo()),
  package_version = as.character(utils::packageVersion("AutoXplainR")),
  source_bundle_sha256 = digest::digest(
    paste(names(source_hashes), source_hashes, collapse = "\n"), algo = "sha256", serialize = FALSE
  ),
  source_sha256 = as.list(source_hashes),
  cases = lapply(names(results), function(name) {
    result <- results[[name]]
    list(
      id = name, training_rows = nrow(inputs[[name]]$training),
      evaluation_rows = nrow(inputs[[name]]$evaluation),
      primary_metric = result$evaluation$primary_metric,
      metric_availability = result$evaluation$metric_availability,
      evaluation_scores = lapply(result$evaluation$metrics, as.list),
      prediction_sha256 = digest::digest(predict(result, inputs[[name]]$evaluation),
        algo = "sha256", serializeVersion = 2L
      ),
      report = reports[[name]]
    )
  }),
  artifacts = artifacts,
  reproduction = paste(
    "Exact inputs are retained as R dput text and RDS; the renderer is copied beside this manifest.",
    "Source and report hashes bind these generated files to one code state.",
    "Timing, serialized model internals and generated timestamps can vary between runs."
  )
)
jsonlite::write_json(manifest, file.path(output, "metric-edge-cases-manifest.json"),
  pretty = TRUE, auto_unbox = TRUE, digits = NA, na = "null"
)
cat("Rendered metric edge cases and manifest in", output, "\n")
