pkgload::load_all(".", quiet = TRUE)
cache <- path.expand("~/.cache/autoxplain-scale-0.7.0/search")
cases <- data.frame(seed = c(912L, 912L, 913L, 913L), k = c(10L, 8L, 10L, 8L))
rows <- lapply(seq_len(nrow(cases)), function(index) {
  stem <- paste0("rare_1pct-", cases$seed[[index]], "-k", cases$k[[index]], "-bam")
  before <- readRDS(file.path(cache, paste0(stem, ".rds")))
  after <- readRDS(file.path(cache, "rare-diagnostic-replay", paste0(stem, ".rds")))
  stopifnot(identical(before$prediction, after$prediction))
  fields <- setdiff(names(before$model$fit), "call")
  # Separately restored binomial-family closures have distinct environment
  # identities. Their complete serialized contents, including those closures,
  # must still agree exactly, as must all other native fitted fields.
  stopifnot(identical(serialize(before$model$fit[fields], NULL), serialize(after$model$fit[fields], NULL)))
  records <- after$model$fit_details$optimizer_warnings
  final <- Filter(function(warning) identical(warning$message, "algorithm did not converge"), records)
  expected <- if (index %in% c(1L, 4L)) "not_converged" else "converged"
  status <- AutoXplainR:::model_optimization_record(after$model)$status
  stopifnot(identical(status, expected))
  if (index %in% c(1L, 4L)) stopifnot(length(final) == 1L, identical(final[[1L]]$stage, "bgam.fit"))
  if (index == 2L) stopifnot(any(vapply(records, function(warning) {
    grepl("Possible divergence", warning$message, fixed = TRUE)
  }, logical(1))))
  if (index == 3L) stopifnot(length(records) == 0L)
  data.frame(seed = cases$seed[[index]], k = cases$k[[index]], predictions_identical = TRUE,
    serialized_native_fields_except_call_identical = TRUE, optimizer_status = status,
    warning_count = length(records), final_pirls_warning_stage = if (length(final)) final[[1L]]$stage else NA,
    final_pirls_warning_call = if (length(final)) final[[1L]]$call else NA)
})
output <- do.call(rbind, rows)
utils::write.csv(output, "validation/scalability/search/rare-diagnostic-replay.csv", row.names = FALSE)
print(output)
