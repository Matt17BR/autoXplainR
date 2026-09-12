# Refit the retained Gaussian additive model after the binary-only policy guard.
# The check uses training predictions and complete native fitted contents.
cache <- Sys.getenv("AXR_SEARCH_DIR", path.expand("~/.cache/autoxplain-scale-0.7.0/search"))
mode <- if (length(commandArgs(TRUE))) match.arg(commandArgs(TRUE)[[1L]], c("source", "installed")) else "installed"
source_files <- c("R/learner_backends.R", "R/tuning.R", "R/tuning_control.R", "R/tuning_evidence.R")
source_md5 <- if (mode == "source") as.list(tools::md5sum(source_files)) else NULL
if (mode == "source") {
  pkgload::load_all(quiet = TRUE)
} else {
  .libPaths(c(file.path(cache, "candidate-binary-guard-library"), .libPaths()))
  library(AutoXplainR)
}
stopifnot(as.character(packageVersion("AutoXplainR")) == "0.7.0")
reference <- file.path(cache, "recommended", "candidate", "friedman_noise", "result.rds")
stored <- readRDS(reference)
old <- stored$result$models$additive_model
training <- stored$case$training
parameters <- old$parameters
attr(parameters, "autoxplain_additive_policy") <- NULL
parameters$solver <- "auto"
plan <- AutoXplainR:::local_tuning_plan(1L, nrow(training), ncol(training) - 1L,
  "regression", 1L, learners = "additive", custom_grids = list(additive = parameters),
  additive_planning_data = training, additive_target = "y")
new <- AutoXplainR:::with_preserved_seed(old$seed, AutoXplainR:::fit_additive_learner(
  training, "y", "regression", plan$parameters[[1L]], old$seed))
stopifnot(identical(new$fit_details$computation$solver, old$fit_details$computation$solver),
  identical(new$fit_details$effective_parameters, old$fit_details$effective_parameters))
predictions_identical <- identical(predict(new, training), predict(old, training))
native_fields_identical <- identical(serialize(new$fit, NULL), serialize(old$fit, NULL))
stopifnot(predictions_identical, native_fields_identical,
  identical(new$fit_details$computation$task, "regression"))
stopifnot(is.null(source_md5) || identical(source_md5, as.list(tools::md5sum(source_files))))
record <- list(case = "friedman_noise", training_rows = nrow(training), mode = mode, source_md5 = source_md5,
  source_model = "recommended/candidate/friedman_noise/result.rds:additive_model",
  source_result_sha256 = digest::digest(file = reference, algo = "sha256"),
  solver = new$fit_details$computation$solver,
  predictions_identical = predictions_identical, native_fields_identical = native_fields_identical,
  metadata_scope = "Task and policy rationale were added; full wrapper metadata is not asserted identical.")
jsonlite::write_json(record, file.path(cache, "gaussian-policy-parity.json"), pretty = TRUE, auto_unbox = TRUE)
cat("Gaussian automatic routing, training predictions and all native fitted fields are unchanged.\n")
