# The fitted probabilities and their tie groups stay fixed in a case bootstrap.
# Sorting once lets each draw score row multiplicities without reranking a
# repeated data set. Ties receive half credit in every draw, including when a
# positive and a negative share the same fitted probability.
auc_bootstrap_plan <- function(truth, probability) {
  estimate <- selection_binary_auc(truth, probability)
  index <- order(probability)
  sorted <- probability[index]
  ends <- c(which(diff(sorted) != 0), length(sorted))
  list(order = index, positive = truth[index], ends = ends, rows = length(truth), estimate = estimate)
}

auc_bootstrap_score <- function(plan, row_counts) {
  counts <- row_counts[plan$order]
  positive <- cumsum(counts * plan$positive)[plan$ends]
  negative <- cumsum(counts * !plan$positive)[plan$ends]
  positives <- positive[[length(positive)]]
  negatives <- negative[[length(negative)]]
  if (!positives || !negatives) return(NA_real_)
  negative_below <- c(0, head(negative, -1L))
  positive_groups <- diff(c(0, positive))
  negative_groups <- negative - negative_below
  sum(positive_groups * (negative_below + negative_groups / 2)) / (as.double(positives) * negatives)
}

auc_bootstrap_complete_draws <- function(draws, requested) {
  complete <- apply(draws, 1L, function(row) all(is.finite(row)))
  kept <- sum(complete)
  minimum <- max(20L, ceiling(.8 * requested))
  if (kept < minimum) {
    stop(
      "AUC bootstrap retained ", kept, " of ", requested, " draws with both outcome classes; ",
      "at least ", minimum, " are required (20 draws and 80% of the requested draws). ",
      "The evaluation sample does not support this automatic interval.", call. = FALSE
    )
  }
  list(
    draws = draws[complete, , drop = FALSE],
    record = list(
      requested = requested, retained = kept, discarded = sum(!complete),
      retained_draw_ids = which(complete), discarded_draw_ids = which(!complete),
      reason = "AUC is undefined when a sampled evaluation set contains only one outcome class.",
      method = "Paired bootstrap of observations or whole groups; fixed-score tie groups use row multiplicities."
    )
  )
}
