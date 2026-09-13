arguments <- commandArgs(trailingOnly = TRUE)
stopifnot(length(arguments) == 2L)
root <- arguments[[1L]]
output <- arguments[[2L]]
records <- list()
for (case in c("yearprediction", "covertype")) {
  directory <- file.path(root, case)
  expected_rows <- readRDS(file.path(directory, "reference-rows.rds"))
  stopifnot(length(expected_rows) == 5000L, !anyDuplicated(expected_rows))
  matrices <- list()
  checked <- list()
  for (kind in c("screen", "detail")) {
    keys <- if (kind == "screen") {
      c("main_model", "simple_baseline", "forest500", "forest256")
    } else c("forest500", "forest256")
    for (key in keys) {
      stage <- paste(kind, key, sep = "-")
      path <- file.path(directory, stage, "importance.rds")
      value <- readRDS(path)
      summary <- jsonlite::fromJSON(file.path(directory, stage, "summary.json"))
      scores <- attr(value, "repeat_scores")
      rows <- attr(value, "sampling")$row_indices
      repeats <- if (kind == "screen") 5L else 20L
      stopifnot(is.matrix(scores), all(is.finite(scores)),
        identical(rownames(scores), value$feature), ncol(scores) == repeats,
        identical(rows, expected_rows), !anyDuplicated(value$feature),
        identical(digest::digest(scores, algo = "sha256"), summary$repeat_scores_sha256),
        identical(digest::digest(rows, algo = "sha256"), summary$reference_rows_sha256))
      mean <- rowMeans(scores)
      standard_error <- apply(scores, 1L, stats::sd) / sqrt(repeats)
      critical <- stats::qt(0.975, repeats - 1L)
      differences <- c(value$importance - mean, value$std_error - standard_error,
        value$conf_low - (mean - critical * standard_error),
        value$conf_high - (mean + critical * standard_error))
      stopifnot(max(abs(differences)) <= 1e-12)
      checked[[stage]] <- list(dimensions = dim(scores),
        maximum_summary_difference = max(abs(differences)),
        repeat_scores_sha256 = digest::digest(scores, algo = "sha256"))
      matrices[[stage]] <- scores
    }
    before <- matrices[[paste0(kind, "-forest500")]]
    after <- matrices[[paste0(kind, "-forest256")]]
    stopifnot(setequal(rownames(before), rownames(after)),
      identical(colnames(before), colnames(after)))
    delta <- after[rownames(before), , drop = FALSE] - before
    comparison <- data.frame(feature = rownames(delta),
      mean_difference_256_minus500 = rowMeans(delta),
      paired_shuffle_standard_error = apply(delta, 1L, stats::sd) / sqrt(ncol(delta)),
      minimum_repeat_difference = apply(delta, 1L, min),
      maximum_repeat_difference = apply(delta, 1L, max), row.names = NULL)
    write.csv(comparison,
      file.path(output, paste0("final-tree-", case, "-", kind, "-paired-repeats.csv")),
      row.names = FALSE)
  }
  records[[case]] <- checked
}
jsonlite::write_json(list(status = "ok", scope = "Saved development importance arrays only; no prediction or fitting.",
  checked_arrays = 12L, records = records),
  file.path(output, "final-tree-importance-check.json"), pretty = TRUE, auto_unbox = TRUE)
cat("Checked 12 saved importance arrays, reference-row identities, summaries, and paired differences.\n")
