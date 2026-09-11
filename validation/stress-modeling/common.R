stress_directory <- function() {
  path <- Sys.getenv("AXR_STRESS_DIR", path.expand("~/.cache/autoxplain-stress-0.6.2/benchmark"))
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path)
}

write_json <- function(value, path) {
  jsonlite::write_json(value, path, auto_unbox = TRUE, pretty = TRUE, digits = 16,
    null = "null", na = "null")
}

stress_metrics <- function(observed, predicted, task) {
  stopifnot(length(predicted) == length(observed), all(is.finite(predicted)))
  if (task == "regression") {
    return(c(rmse = sqrt(mean((observed - predicted)^2)),
      mae = mean(abs(observed - predicted)),
      r_squared = 1 - sum((observed - predicted)^2) / sum((observed - mean(observed))^2)))
  }
  event <- as.integer(observed == "yes")
  stopifnot(all(predicted >= 0), all(predicted <= 1), length(unique(event)) == 2L)
  bounded <- pmax(1e-15, pmin(1 - 1e-15, predicted))
  rank_sum <- sum(rank(predicted, ties.method = "average")[event == 1L])
  positives <- sum(event)
  negatives <- sum(1 - event)
  # Group equal scores before computing the stepwise precision-recall integral.
  ordering <- order(predicted, decreasing = TRUE)
  ordered_event <- event[ordering]
  ends <- cumsum(rle(predicted[ordering])$lengths)
  true_positives <- cumsum(ordered_event)[ends]
  recall <- true_positives / positives
  precision <- true_positives / ends
  average_precision <- sum(diff(c(0, recall)) * precision)
  called <- predicted >= 0.5
  c(log_loss = -mean(event * log(bounded) + (1 - event) * log1p(-bounded)),
    brier = mean((predicted - event)^2),
    auc = (rank_sum - positives * (positives + 1) / 2) / (positives * negatives),
    average_precision = average_precision,
    prevalence = mean(event), accuracy_at_0_5 = mean(called == event),
    recall_at_0_5 = sum(called & event == 1L) / positives,
    precision_at_0_5 = if (any(called)) sum(called & event == 1L) / sum(called) else NA_real_)
}

stress_folds <- function(y, task, seed) {
  set.seed(seed)
  folds <- integer(length(y))
  groups <- if (task == "regression") list(seq_along(y)) else split(seq_along(y), y)
  for (rows in groups) folds[rows] <- sample(rep(seq_len(5L), length.out = length(rows)))
  folds
}
