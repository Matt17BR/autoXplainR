# Explicitly authorized acceptance verification only. This script never fits a model.
arguments <- commandArgs(TRUE)
stopifnot(length(arguments) == 3L)
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
script_dir <- dirname(normalizePath(script))
source(file.path(script_dir, "common.R"))
source(file.path(script_dir, "evidence-common.R"))
source(file.path(script_dir, "acceptance-resources.R"))
started <- proc.time()[["elapsed"]]
destination <- normalizePath(arguments[[1L]], mustWork = TRUE)
library_path <- arguments[[2L]]
freeze_path <- arguments[[3L]]
authorized <- tabular_verify_freeze(destination, freeze_path, library_path)
for (filename in c("check-acceptance.R", "evidence-common.R", "acceptance-resources.R", "common.R")) {
  tabular_same_hash(file.path(script_dir, filename), authorized$freeze$protocol_files[[filename]])
}
summary <- jsonlite::read_json(file.path(destination, "summary.json"))
partition <- tabular_verify_partition(destination, summary)
resources <- tabular_verify_resources(destination)
# All authorization, immutable-file and resource checks precede opening outcomes.
y <- readRDS(file.path(partition$path, "evaluation-targets.rds"))
tabular_scalar(length(y), "acceptance evaluation rows", partition$declared$n_evaluation, 1)
saved <- readRDS(file.path(destination, "model.rds"))
predictions <- readRDS(file.path(destination, "predictions.rds"))
if (identical(summary$variant, "package")) {
  baseline <- identical(authorized$entry$role, "published-baseline")
  training <- readRDS(file.path(partition$path, "training.rds"))
  evidence <- tabular_verify_package(saved, predictions, summary, training, y)
  cv <- tabular_verify_cv(saved, summary, training)
  if (baseline) {
    stopifnot(identical(summary$package_version, "0.7.0"), identical(summary$request, "paired"),
      identical(authorized$entry$published_archive_sha256, "bc6ad22bee49a9a3fee2ba7a092d975372815e07629295b3a012871782ac2532"),
      identical(authorized$process$threads, 1L), is.null(saved$explanations),
      saved$tuning$control$max_models == 10L, saved$provenance$seed == 80711L)
    quality <- NULL
  } else {
  stopifnot(identical(summary$request, "public-tabular"))
  actual_threads <- saved$tuning$control$threads
  tabular_scalar(actual_threads, "resolved package threads", minimum = 1)
  stopifnot(actual_threads <= authorized$process$threads,
    identical(summary$resolved_public_defaults$threads, actual_threads))
  stopifnot(!is.null(saved$explanations), isTRUE(summary$public_request$default_explanations_and_report))
  tabular_scalar(summary$report$bytes, "public report bytes", minimum = 1)
  stopifnot(identical(summary$report$filename, basename(summary$report$filename)))
  tabular_same_hash(file.path(destination, summary$report$filename), summary$report$sha256)
  references <- Filter(function(entry) identical(entry$case, summary$case) &&
    entry$variant %in% c("ranger", "xgboost") && identical(entry$stage, "score-only") &&
    identical(entry$threads, actual_threads), authorized$freeze$allowed_runs)
  if (!length(references)) stop("No matched native references were frozen.")
  native <- failures <- list()
  for (entry in references) {
    reference_path <- file.path(tabular_cache(), "runs", entry$cohort, entry$case, entry$variant)
    process_path <- file.path(reference_path, "process.json")
    if (!file.exists(process_path)) {
      # A freeze may authorize a standby retry cohort. Require at least one
      # actual attempt for each distinct saved fit, not every unused retry.
      siblings <- Filter(function(other) identical(other$fit_process_sha256, entry$fit_process_sha256), references)
      attempted <- vapply(siblings, function(other) file.exists(file.path(tabular_cache(), "runs",
        other$cohort, other$case, other$variant, "process.json")), logical(1))
      if (!any(attempted)) stop("A predeclared native saved fit has no scoring attempt: ", reference_path)
      next
    }
    process <- jsonlite::read_json(process_path)
    key <- paste(entry$cohort, entry$variant, sep = "/")
    if (process$process_status %in% c("failed", "timeout")) {
      verified_reference <- tabular_verify_freeze(reference_path, freeze_path, require_success = FALSE)
      stopifnot(identical(verified_reference$entry, entry))
      failed_resources <- tabular_verify_resources(reference_path, require_success = FALSE)
      failures[[key]] <- list(status = process$process_status, process_sha256 = tabular_hash(process_path),
        resources = failed_resources, scope = "Verified failure evidence; no quality score or acceptance credit.")
      next
    }
    verified_reference <- tabular_verify_freeze(reference_path, freeze_path)
    stopifnot(identical(verified_reference$entry, entry))
    native_summary <- jsonlite::read_json(file.path(reference_path, "summary.json"))
    native_partition <- tabular_verify_partition(reference_path, native_summary)
    native_resources <- tabular_verify_resources(reference_path)
    native_model <- readRDS(file.path(reference_path, "model.rds"))
    native_predictions <- readRDS(file.path(reference_path, "predictions.rds"))
    checked <- tabular_verify_native(native_model, native_predictions, native_summary, native_partition$declared, y)
    native_replay <- tabular_verify_replay(reference_path, checked$ids, length(y))
    native[[key]] <- list(variant = entry$variant, metrics = checked$metrics[[entry$variant]],
      resources = native_resources, replay = native_replay, process_sha256 = tabular_hash(process_path),
      row_evidence_scope = checked$row_evidence_scope)
    rm(native_model, native_predictions)
    gc()
  }
  quality <- tabular_quality_gates(evidence$metrics, summary$primary, evidence$forest_ids, native, summary$task)
  quality$references <- native
  quality$failed_references <- failures
  quality$actual_package_threads <- actual_threads
  quality$supervisor_threads <- authorized$process$threads
  quality$published_comparison <- tabular_verify_published_comparison(summary$case, authorized$freeze,
    freeze_path, y, evidence$metrics[[summary$primary]], native, summary$task)
  }
} else {
  evidence <- tabular_verify_native(saved, predictions, summary, partition$declared, y)
  cv <- quality <- NULL
}
replay <- tabular_verify_replay(destination, evidence$ids, length(y))
passed <- is.null(quality) || (quality$primary_pass && quality$forest_pass &&
  !identical(quality$published_comparison$non_regression_pass, FALSE))
record <- list(status = if (passed) "ok" else "quality_failed", case = summary$case, variant = summary$variant,
  phase = "acceptance", model_ids = evidence$ids, evidence = evidence, cv = cv, quality = quality,
  resources = resources, replay = replay, elapsed_seconds = proc.time()[["elapsed"]] - started,
  acceptance_freeze_sha256 = tabular_hash(freeze_path), process_sha256 = tabular_hash(file.path(destination, "process.json")),
  verification_source_sha256 = tabular_hash(script),
  scope = paste("Frozen numerical, full-row, replay and resource verification; no overall release approval.",
    "Published-0.7 comparison and offline report usability gates remain separately required.",
    "Replay and verification times are separate from the fitting/scoring process budget."))
tabular_json(record, file.path(destination, "acceptance-verification.json"))
if (!passed) stop("The selected model or retained forest failed its predeclared quality gate.")
cat("Verified frozen", summary$variant, "evidence, complete predictions, replay and resource accounting.\n")
