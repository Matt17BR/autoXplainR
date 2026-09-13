arguments <- commandArgs(TRUE)
stopifnot(length(arguments) %in% c(2L, 3L))
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script)), "common.R"))
source(file.path(dirname(normalizePath(script)), "evidence-common.R"))
started <- proc.time()[["elapsed"]]
destination <- normalizePath(arguments[[1L]], mustWork = TRUE)
summary <- jsonlite::read_json(file.path(destination, "summary.json"))
stopifnot(identical(summary$status, "ok"))
if (identical(summary$phase, "acceptance")) {
  if (length(arguments) != 3L) stop("Acceptance replay is locked; supply the final freeze as the third argument.")
  authorized <- tabular_verify_freeze(destination, arguments[[3L]], arguments[[2L]])
  for (filename in c("replay.R", "evidence-common.R", "common.R")) {
    tabular_same_hash(file.path(dirname(normalizePath(script)), filename), authorized$freeze$protocol_files[[filename]])
  }
} else stopifnot(identical(summary$phase, "development"))
partition <- tabular_verify_partition(destination, summary, include_targets = FALSE)
if (summary$variant == "package") {
  .libPaths(c(normalizePath(arguments[[2L]], mustWork = TRUE), .libPaths()))
  library(AutoXplainR)
  stopifnot(identical(as.character(packageVersion("AutoXplainR")), summary$package_version),
    identical(normalizePath(find.package("AutoXplainR")), normalizePath(file.path(arguments[[2L]], "AutoXplainR"))))
}
features <- readRDS(file.path(partition$path, "evaluation-features.rds"))$data
tabular_scalar(nrow(features), "replay evaluation rows", partition$declared$n_evaluation, 1)
expected <- readRDS(file.path(destination, "predictions.rds"))
saved <- readRDS(file.path(destination, "model.rds"))
ids <- tabular_verify_prediction_ids(saved, expected, summary$variant)
expected <- expected[ids]
if (summary$variant == "package") {
  actual <- lapply(ids, function(id) predict(saved, features, model = id))
  names(actual) <- ids
} else {
  actual <- tabular_native_predict(saved, features)
}
stopifnot(identical(names(expected), names(actual)))
differences <- Map(function(old, new) {
  stopifnot(identical(dim(old), dim(new)), identical(colnames(old), colnames(new)),
    length(old) == length(new), length(new) > 0L, all(is.finite(old)), all(is.finite(new)))
  tabular_scalar(if (is.null(dim(new))) length(new) else nrow(new), "replay prediction rows", nrow(features), 1)
  difference <- max(abs(old - new))
  stopifnot(difference <= 1e-12)
  difference
}, expected, actual)
record <- list(status = "ok", case = summary$case, phase = summary$phase, variant = summary$variant,
  process_id = Sys.getpid(), max_absolute_prediction_difference = differences,
  predictions_sha256 = tabular_hash(file.path(destination, "predictions.rds")),
  saved_model_sha256 = tabular_hash(file.path(destination, "model.rds")),
  verified_at = format(Sys.time(), tz = "UTC", usetz = TRUE), fresh_session = TRUE,
  model_ids = ids, evaluation_rows = nrow(features),
  process_sha256 = tabular_hash(file.path(destination, "process.json")),
  verification_source_sha256 = list(replay = tabular_hash(script),
    common = tabular_hash(file.path(dirname(normalizePath(script)), "common.R")),
    evidence_common = tabular_hash(file.path(dirname(normalizePath(script)), "evidence-common.R"))),
  elapsed_seconds = proc.time()[["elapsed"]] - started,
  scope = "Fresh-session prediction replay; elapsed time is separate from the fitting/scoring process budget.")
tabular_json(record, file.path(destination, "cold-replay.json"))
cat("Fresh-session predictions agree for", length(actual), "models and", nrow(features), "evaluation rows.\n")
