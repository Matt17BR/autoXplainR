explanation_row_sample <- function(n, max_rows, seed) {
  if (!is.null(max_rows)) max_rows <- assert_count(max_rows, "max_rows", minimum = 2L)
  seed <- assert_count(seed, "seed", minimum = 0L)
  indices <- if (!is.null(max_rows) && n > max_rows) {
    sort(with_preserved_seed(seed, sample.int(n, max_rows)))
  } else {
    seq_len(n)
  }
  list(
    rows_available = n, rows_used = length(indices), sampled = length(indices) < n,
    method = "Uniform sample without replacement", seed = seed, row_indices = indices
  )
}

explanation_sampling_note <- function(sampling) {
  if (!isTRUE(sampling$sampled)) {
    return("")
  }
  paste0(
    "Explanations use ", format(sampling$rows_used, big.mark = ",", trim = TRUE),
    " sampled evaluation rows out of ", format(sampling$rows_available, big.mark = ",", trim = TRUE),
    ". Model scores use all evaluation rows. Shuffle intervals do not include row-sampling uncertainty.",
    if (length(sampling$missed_classes %||% sampling$missing_classes)) {
      paste0(
        " No sampled observations belong to: ",
        paste(sampling$missed_classes %||% sampling$missing_classes, collapse = ", "),
        ". Increase the explanation row limit to inspect these classes."
      )
    },
    if (length(sampling$absent_evaluation_classes)) {
      paste0(
        " The full evaluation set has no observations of: ", paste(sampling$absent_evaluation_classes, collapse = ", "),
        ". Assessing these classes requires additional evaluation observations."
      )
    }
  )
}
