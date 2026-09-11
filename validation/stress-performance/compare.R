# Compare an immutable installed package with only the report preparation and
# serialization functions from this checkout. The saved fit is never refitted.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L) {
  stop("Usage: Rscript compare.R BASELINE_LIBRARY SAVED_FIT OUTPUT_DIR [summary|rows] [REPETITIONS]")
}
baseline_library <- normalizePath(args[1L], mustWork = TRUE)
fixture <- normalizePath(args[2L], mustWork = TRUE)
output <- args[3L]
mode <- if (length(args) >= 4L) match.arg(args[4L], c("summary", "rows")) else "rows"
repetitions <- if (length(args) >= 5L) as.integer(args[5L]) else 1L
stopifnot(length(repetitions) == 1L, !is.na(repetitions), repetitions > 0L)
dir.create(output, recursive = TRUE, showWarnings = FALSE)
source_files <- c("R/data_profile.R", "R/report_payload.R")
stopifnot(all(file.exists(source_files)))
source_md5 <- as.list(tools::md5sum(source_files))
baseline <- loadNamespace("AutoXplainR", lib.loc = baseline_library)
candidate <- new.env(parent = baseline)
for (file in source_files) sys.source(file, envir = candidate)
stored <- readRDS(fixture)
expected_export <- expected_html <- NULL
observations <- list()
for (implementation in c("baseline", "candidate")) {
  environment <- if (implementation == "baseline") baseline else candidate
  prepare <- get("prepare_data_explorer", envir = environment)
  encode <- get("report_json_script", envir = environment)
  times <- vector("list", repetitions)
  for (i in seq_len(repetitions)) {
    gc()
    preparation <- system.time(export <- prepare(stored$result, mode))[["elapsed"]]
    if (is.null(expected_export)) expected_export <- export
    stopifnot(identical(export, expected_export))
    serialization <- system.time({
      payload <- if (implementation == "candidate") candidate$report_data_payload(export) else export
      html <- encode(payload, "axr-data-payload")
    })[["elapsed"]]
    if (is.null(expected_html)) expected_html <- html
    stopifnot(identical(html, expected_html))
    times[[i]] <- list(prepare_seconds = preparation, serialization_with_escaping_seconds = serialization)
    cat(implementation, i, "preparation", preparation, "serialization", serialization, "seconds\n")
  }
  body <- sub("^<script[^>]*>", "", sub("</script>$", "", html))
  reference_body <- sub("^<script[^>]*>", "", sub("</script>$", "", expected_html))
  stopifnot(identical(jsonlite::fromJSON(body, simplifyVector = FALSE),
                      jsonlite::fromJSON(reference_body, simplifyVector = FALSE)))
  observations[[implementation]] <- list(
    timings = times, entire_R_export_identical = TRUE, escaped_script_byte_identical = TRUE,
    decoded_full_payload_identical = TRUE,
    R_export_bytes = as.numeric(object.size(export)), embedded_script_bytes = nchar(html, type = "bytes")
  )
}
record <- list(
  baseline_version = as.character(utils::packageVersion("AutoXplainR", lib.loc = baseline_library)),
  candidate_version = read.dcf("DESCRIPTION", fields = "Version")[[1L]],
  mode = mode, predictors = length(stored$result$features),
  training_rows = nrow(stored$result$training_data), evaluation_rows = nrow(stored$result$test_data),
  fixture_sha256 = digest::digest(file = fixture, algo = "sha256"),
  source_md5 = source_md5,
  observations = observations,
  scope = paste("Same saved fitted result and host. These times cover preparation and embedded JSON,",
                "not model fitting, explanation computation, the rest of HTML generation or browser rendering.",
                "Timings can vary with other work on the host. Object and file sizes are not peak process memory.")
)
stopifnot(identical(source_md5, as.list(tools::md5sum(source_files))))
jsonlite::write_json(record, file.path(output, "comparison.json"), pretty = TRUE,
                     auto_unbox = TRUE, digits = 16)
writeLines(capture.output(sessionInfo()), file.path(output, "session-info.txt"))
