# Run from the repository root. Native models are fitted once, then both source
# snapshots prepare evidence from the same serialized results in separate R sessions.
arguments <- commandArgs(trailingOnly = TRUE)
script_path <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())])
if (length(arguments) && identical(arguments[[1L]], "--worker")) {
  stopifnot(length(arguments) == 4L)
  pkgload::load_all(arguments[[2L]], quiet = TRUE, helpers = FALSE)
  fixtures <- readRDS(arguments[[3L]])
  output <- lapply(fixtures, function(result) {
    AutoXplainR:::prepare_model_report_data(result, top_features = 3L,
      n_repeats = 4L, max_models = 2L, explanation_rows = 31L)
  })
  saveRDS(output, arguments[[4L]], compress = FALSE)
  quit(status = 0L)
}
if (!length(arguments) %in% c(2L, 3L)) {
  stop("Usage: Rscript parity-reports.R FROZEN_PRE_REUSE_SOURCE OUTPUT_DIRECTORY [SAVED_FIXTURES]", call. = FALSE)
}
destination <- normalizePath(arguments[[2L]], mustWork = FALSE)
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
snapshot_source <- function(from, to) {
  stopifnot(!dir.exists(to))
  dir.create(file.path(to, "R"), recursive = TRUE)
  files <- c("DESCRIPTION", "NAMESPACE", file.path("R", list.files(file.path(from, "R"))))
  before <- stats::setNames(vapply(file.path(from, files), function(path) {
    digest::digest(file = path, algo = "sha256")
  }, character(1)), files)
  stopifnot(all(file.copy(file.path(from, files), file.path(to, files))))
  loaded <- stats::setNames(vapply(file.path(to, files), function(path) {
    digest::digest(file = path, algo = "sha256")
  }, character(1)), files)
  after <- stats::setNames(vapply(file.path(from, files), function(path) {
    digest::digest(file = path, algo = "sha256")
  }, character(1)), files)
  stopifnot(identical(before, loaded), identical(after, loaded))
  as.list(loaded)
}
baseline_path <- file.path(destination, "baseline-source")
current_path <- file.path(destination, "current-source")
baseline_hashes <- snapshot_source(arguments[[1L]], baseline_path)
current_hashes <- snapshot_source(".", current_path)
pkgload::load_all(current_path, quiet = TRUE, helpers = FALSE)
stopifnot(requireNamespace("ranger", quietly = TRUE), requireNamespace("xgboost", quietly = TRUE))
fixture_path <- file.path(destination, "fixtures.rds")
if (length(arguments) == 3L) {
  stopifnot(file.copy(arguments[[3L]], fixture_path))
  fixtures <- readRDS(fixture_path)
} else {
  set.seed(80871L)
  data <- data.frame(x = rnorm(210), z = rnorm(210),
    tied = sample(-2:2, 210, replace = TRUE),
    category = factor(rep(c("a", "b", "c"), 70)))
  signal <- data$x + .3 * ifelse(is.na(data$z), 0, data$z) + as.integer(data$category)
  outcomes <- list(
    regression = 2 * signal + rnorm(nrow(data), sd = .5),
    binary = factor(ifelse(runif(nrow(data)) < plogis(signal - 2), "yes", "no"), levels = c("no", "yes")),
    multiclass = factor(c("a", "b", "c")[max.col(cbind(
      signal + rnorm(nrow(data)), -signal + 3 + rnorm(nrow(data)),
      .5 * data$tied + 2 + rnorm(nrow(data))), ties.method = "first")], levels = c("a", "b", "c"))
  )
  fixtures <- lapply(names(outcomes), function(task) {
    target_data <- data
    target_data$y <- outcomes[[task]]
    training <- target_data[1:140, , drop = FALSE]
    evaluation <- target_data[141:210, , drop = FALSE]
    forest_parameters <- list(num.trees = 32L, mtry = 2L, min.node.size = 3L,
      sample.fraction = .8, splitrule = "default")
    attr(forest_parameters, "autoxplain_threads") <- 1L
    forest <- AutoXplainR:::fit_forest_learner(training, "y", task, forest_parameters, 80873L)
    boosting_parameters <- AutoXplainR:::boosting_learner_grid(140L, 4L, task, 1L)[[1L]]
    boosting_parameters$nrounds <- 24L
    boosting_parameters$max_depth <- 2L
    attr(boosting_parameters, "autoxplain_threads") <- 1L
    boosting <- AutoXplainR:::fit_boosting_learner(training, "y", task, boosting_parameters, 80879L)
    result <- AutoXplainR::evaluate_models(list(forest = forest, boosting = boosting),
      evaluation, "y", task = task, training_data = training,
      positive = if (task == "binary") "no" else NULL, seed = 80881L)
    stopifnot(forest$fit$num.samples == 140L, forest$fit$num.trees == 32L)
    result
  })
  names(fixtures) <- names(outcomes)
  saveRDS(fixtures, fixture_path, compress = FALSE)
}
outputs <- c(baseline = file.path(destination, "baseline-evidence.rds"),
  current = file.path(destination, "current-evidence.rds"))
for (id in names(outputs)) {
  source_path <- if (id == "baseline") baseline_path else current_path
  log <- file.path(destination, paste0(id, ".log"))
  code <- system2(file.path(R.home("bin"), "Rscript"),
    args = vapply(c(script_path, "--worker", source_path, fixture_path, outputs[[id]]), shQuote, character(1)),
    stdout = log, stderr = log)
  if (code != 0L) stop("Evidence worker failed: ", log, call. = FALSE)
}
without_creation_time <- function(value) {
  if (is.list(value)) {
    if (!is.null(names(value)) && "created_at" %in% names(value)) value["created_at"] <- NULL
    for (index in seq_along(value)) value[index] <- list(without_creation_time(value[[index]]))
  }
  value
}
baseline <- without_creation_time(readRDS(outputs[["baseline"]]))
current <- without_creation_time(readRDS(outputs[["current"]]))
checks <- lapply(names(fixtures), function(task) {
  old <- baseline[[task]]
  new <- current[[task]]
  identical_fields <- stats::setNames(vapply(names(old), function(field) {
    identical(old[[field]], new[[field]])
  }, logical(1)), names(old))
  same <- identical(old, new)
  list(task = task, identical_complete_evidence = same, fields_identical = as.list(identical_fields),
    difference = if (same) NULL else as.character(all.equal(old, new, tolerance = 0)),
    models = names(fixtures[[task]]$models),
    audited_inputs = old$audit$config$features,
    effect_status = old$effect_status,
    audited_repeat_matrices = lapply(old$audit$importance_objects, function(item) dim(attr(item, "repeat_scores"))))
})
manifest <- list(generated_at = format(Sys.time(), tz = "UTC"),
  purpose = "Independent complete report evidence parity across immutable pre-reuse and current sources",
  ignored_fields = "created_at only; fixtures already share every other creation timestamp",
  native_models = "One 32-tree ranger and one 24-round XGBoost model per task, fitted once on 140 rows, evaluated on 70 rows",
  native_threads = 1L, explanation_rows = 31L, repeats = 4L, top_features = 3L,
  fixture_sha256 = digest::digest(file = fixture_path, algo = "sha256"),
  runner_sha256 = digest::digest(file = script_path, algo = "sha256"),
  baseline_source_sha256 = baseline_hashes, current_source_sha256 = current_hashes,
  R_version = R.version.string, ranger_version = as.character(packageVersion("ranger")),
  xgboost_version = as.character(packageVersion("xgboost")), checks = checks)
jsonlite::write_json(manifest, file.path(destination, "summary.json"), pretty = TRUE, auto_unbox = TRUE, digits = 16)
stopifnot(all(vapply(checks, `[[`, logical(1), "identical_complete_evidence")))
cat("Complete report evidence is identical for regression, binary and multiclass after removing created_at.\n")
