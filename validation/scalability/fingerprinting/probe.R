# Run via supervise.py. This isolates content fingerprinting, not model fitting.
arguments <- commandArgs(trailingOnly = TRUE)
stopifnot(length(arguments) == 4L)
source_path <- normalizePath(arguments[[1L]], mustWork = TRUE)
variant <- match.arg(arguments[[2L]], c("before", "after"))
output <- normalizePath(arguments[[3L]], mustWork = TRUE)
vector_mib <- as.integer(arguments[[4L]])
stopifnot(!is.na(vector_mib), vector_mib >= 1L)
stopifnot(requireNamespace("digest", quietly = TRUE), requireNamespace("jsonlite", quietly = TRUE))
implementation <- new.env(parent = baseenv())
sys.source(source_path, envir = implementation)

# The eight list entries share one materialized numeric vector in memory. R's
# version-2 serialization repeats ordinary vector values, exceeding that footprint.
values <- rep(0.125, vector_mib * 1024^2 / 8)
payload <- rep(list(values), 8L)
set.seed(903L)
seed <- .Random.seed
directories <- list.dirs(tempdir(), recursive = FALSE, full.names = TRUE)
gc()
started <- proc.time()[["elapsed"]]
fingerprint <- tryCatch(implementation$content_fingerprint(payload), error = identity)
seconds <- proc.time()[["elapsed"]] - started
result <- list(
  variant = variant, status = if (inherits(fingerprint, "error")) "error" else "success",
  fingerprint_seconds = unname(seconds), vector_mib = vector_mib, list_entries = length(payload),
  list_object_size_bytes = as.numeric(object.size(payload)),
  fingerprint = if (is.character(fingerprint)) fingerprint else NULL,
  error = if (inherits(fingerprint, "error")) conditionMessage(fingerprint) else NULL,
  rng_unchanged = identical(seed, .Random.seed),
  temporary_directories_removed = identical(directories, list.dirs(tempdir(), recursive = FALSE, full.names = TRUE)),
  digest_version = as.character(utils::packageVersion("digest")), R_version = R.version.string
)

if (identical(variant, "after") && identical(result$status, "success")) {
  # Independent byte producer: do not call the package's serialization helper.
  connection <- file(file.path(output, "oracle-v2.bin"), open = "wb")
  tryCatch(serialize(payload, connection, ascii = FALSE, xdr = TRUE, version = 2L),
           finally = close(connection))
  result$oracle_file_bytes <- unname(file.info(file.path(output, "oracle-v2.bin"))$size)
  payload[[8L]][length(payload[[8L]])] <- 0.25
  result$changed_final_value_fingerprint <- implementation$content_fingerprint(payload)
  result$changed_final_value_detected <- !identical(result$fingerprint, result$changed_final_value_fingerprint)
}
stopifnot(result$rng_unchanged, result$temporary_directories_removed)
jsonlite::write_json(result, file.path(output, "measurement.json"), auto_unbox = TRUE,
                     pretty = TRUE, digits = NA, null = "null")
writeLines(capture.output(sessionInfo()), file.path(output, "session.txt"))
print(result)
