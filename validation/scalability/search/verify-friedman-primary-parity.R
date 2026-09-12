# Read the original prediction vectors saved by each timed worker. No native
# model is loaded, refitted or asked to create a replacement reference.
cache <- path.expand("~/.cache/autoxplain-scale-0.7.0")
search <- file.path(cache, "search")
variants <- c("baseline", "candidate")
folders <- file.path(search, "recommended", variants, "friedman_noise")
records <- lapply(folders, function(path) jsonlite::read_json(file.path(path, "summary.json")))
vectors <- lapply(folders, function(path) readRDS(file.path(path, "predictions.rds")))
stopifnot(all(vapply(records, function(x) identical(x$status, "ok"), logical(1L))),
  identical(records[[1L]]$case_sha256, records[[2L]]$case_sha256),
  identical(records[[1L]]$seed, records[[2L]]$seed),
  identical(records[[1L]]$selected_configuration, records[[2L]]$selected_configuration),
  identical(records[[1L]]$primary, records[[2L]]$primary))
model_id <- records[[1L]]$primary
before <- vectors[[1L]][[model_id]]
after <- vectors[[2L]][[model_id]]
stopifnot(is.numeric(before), length(before) > 0L, all(is.finite(before)),
  is.numeric(after), length(after) == length(before), all(is.finite(after)))
sources <- lapply(seq_along(variants), function(i) {
  files <- c("predictions.rds", "summary.json")
  list(variant = variants[[i]], package_version = records[[i]]$package_version,
    directory = folders[[i]], public_call_seconds = records[[i]]$fit_seconds,
    source_sha256 = as.list(stats::setNames(vapply(files, function(name) {
      digest::digest(file = file.path(folders[[i]], name), algo = "sha256")
    }, character(1L)), files)))
})
answer <- list(
  case = "friedman_noise", case_sha256 = records[[1L]]$case_sha256,
  fit_seed = records[[1L]]$seed, model_id = model_id,
  selected_configuration = records[[1L]]$selected_configuration,
  rows = length(before), values_identical = identical(before, after),
  serialized_vectors_identical = identical(serialize(before, NULL), serialize(after, NULL)),
  maximum_absolute_difference = max(abs(before - after)),
  sources = sources,
  scope = "Complete original saved primary holdout vectors from the timed published and accepted Gaussian-policy candidate searches. No model reload, prediction regeneration or fitting. This does not claim identity of alternate models or all later package metadata."
)
jsonlite::write_json(answer, file.path(cache, "release-verification", "recommended-friedman-parity", "verdict.json"),
  pretty = TRUE, auto_unbox = TRUE, digits = 16)
stopifnot(answer$values_identical, answer$serialized_vectors_identical)
cat("All", length(before), "original primary holdout values and their complete serialized vectors are identical.\n")
