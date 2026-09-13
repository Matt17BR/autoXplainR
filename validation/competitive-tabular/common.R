tabular_cache <- function() {
  path <- Sys.getenv("AXR_TABULAR_DIR", path.expand("~/.cache/autoxplain-tabular-0.8.0"))
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path)
}

tabular_json <- function(value, path) {
  jsonlite::write_json(value, path, auto_unbox = TRUE, pretty = TRUE,
    digits = 16, null = "null", na = "null")
}

tabular_hash <- function(path) digest::digest(file = path, algo = "sha256")

tabular_sample <- function(rows, y, size, seed) {
  stopifnot(length(size) == 1L, size <= length(rows), size > 0L,
    !anyDuplicated(rows), length(y) >= max(rows))
  set.seed(seed)
  if (!is.factor(y)) return(sample(rows, size))
  groups <- split(rows, y[rows], drop = TRUE)
  quotas <- lengths(groups) / length(rows) * size
  counts <- floor(quotas)
  remainder <- size - sum(counts)
  if (remainder > 0L) {
    add <- order(quotas - counts, decreasing = TRUE)[seq_len(remainder)]
    counts[add] <- counts[add] + 1L
  }
  chosen <- unlist(Map(function(group, count) {
    if (count) group[sample.int(length(group), count)] else integer()
  }, groups, counts), use.names = FALSE)
  stopifnot(length(chosen) == size)
  chosen[sample.int(length(chosen))]
}

tabular_folds <- function(y, seed = 80701L, k = 5L) {
  set.seed(seed)
  out <- integer(length(y))
  groups <- if (is.factor(y)) split(seq_along(y), y, drop = TRUE) else list(seq_along(y))
  for (group in groups) out[group] <- sample(rep(seq_len(k), length.out = length(group)))
  out
}

tabular_metrics <- function(y, prediction, task) {
  stopifnot(length(y) > 1L, all(is.finite(prediction)))
  if (task == "regression") {
    stopifnot(is.numeric(prediction), is.null(dim(prediction)), length(y) == length(prediction))
    residual <- y - prediction
    return(list(rmse = sqrt(mean(residual^2)), mae = mean(abs(residual)),
      r_squared = 1 - sum(residual^2) / sum((y - mean(y))^2)))
  }
  stopifnot(is.factor(y))
  if (task == "binary" && is.null(dim(prediction))) {
    stopifnot(length(prediction) == length(y))
    probabilities <- cbind(1 - prediction, prediction)
    colnames(probabilities) <- levels(y)
  } else {
    probabilities <- as.matrix(prediction)
    stopifnot(nrow(probabilities) == length(y),
      identical(sort(colnames(probabilities)), sort(levels(y))))
    probabilities <- probabilities[, levels(y), drop = FALSE]
  }
  stopifnot(all(probabilities >= 0), all(probabilities <= 1),
    max(abs(rowSums(probabilities) - 1)) < 1e-6)
  actual <- as.integer(y)
  chosen <- if (task == "binary") 1L + as.integer(probabilities[, 2L] >= .5) else {
    max.col(probabilities, ties.method = "first")
  }
  indicator <- matrix(0, nrow(probabilities), ncol(probabilities))
  indicator[cbind(seq_along(y), actual)] <- 1
  metrics <- list(log_loss = -mean(log(pmax(1e-15,
      probabilities[cbind(seq_along(y), actual)]))),
    brier = mean(rowSums((probabilities - indicator)^2)),
    accuracy = mean(chosen == actual),
    recall_by_class = as.list(setNames(vapply(seq_along(levels(y)), function(index) {
      mean(chosen[actual == index] == index)
    }, numeric(1)), levels(y))))
  if (task == "binary") {
    # A probability matrix stored inside a data frame can carry base R's
    # AsIs class. Its class is not part of the numeric ranking contract.
    p <- as.numeric(probabilities[, 2L])
    event <- actual == 2L
    positives <- sum(event)
    negatives <- sum(!event)
    stopifnot(positives > 0L, negatives > 0L)
    metrics$brier <- mean((p - as.numeric(event))^2)
    metrics$auc <- (sum(rank(p, ties.method = "average")[event]) -
      positives * (positives + 1) / 2) / (positives * negatives)
    ordering <- order(p, decreasing = TRUE)
    ends <- cumsum(rle(p[ordering])$lengths)
    tp <- cumsum(event[ordering])[ends]
    metrics$average_precision <- sum(diff(c(0, tp / positives)) * (tp / ends))
    metrics$prevalence <- mean(event)
  }
  metrics
}

tabular_primary <- function(metrics, task) {
  if (task == "regression") metrics$rmse else metrics$log_loss
}

# A native reference's encoding is learned from its fitting partition. The
# explicit new-value bucket avoids silently dropping a calibration row.
tabular_blueprint <- function(data) {
  result <- lapply(data, function(column) {
    if (is.character(column) || is.factor(column)) {
      known <- sort(unique(as.character(column[!is.na(column)])))
      stopifnot(!any(c("__new__", "__missing__") %in% known))
      c(known, "__new__", "__missing__")
    } else NULL
  })
  result
}

tabular_bake <- function(data, blueprint, matrix = FALSE) {
  stopifnot(identical(names(data), names(blueprint)))
  for (name in names(data)) {
    known <- blueprint[[name]]
    if (is.null(known)) next
    values <- as.character(data[[name]])
    values[is.na(values)] <- "__missing__"
    values[!values %in% known] <- "__new__"
    data[[name]] <- factor(values, levels = known)
  }
  if (!matrix) return(data)
  as.matrix(stats::model.matrix(~ . - 1, data,
    contrasts.arg = lapply(data[vapply(data, is.factor, logical(1))],
      function(x) contrasts(x, contrasts = FALSE))))
}

tabular_native_predict <- function(saved, features) {
  if (saved$family == "xgboost") {
    x <- tabular_bake(features, saved$blueprint, matrix = TRUE)
    prediction <- predict(saved$model, xgboost::xgb.DMatrix(x, nthread = saved$threads))
    if (saved$task == "multiclass") {
      if (is.null(dim(prediction))) {
        prediction <- matrix(prediction, ncol = length(saved$class_levels), byrow = TRUE)
      }
      colnames(prediction) <- saved$class_levels
    } else prediction <- as.numeric(prediction)
    return(list(xgboost = prediction))
  }
  stopifnot(identical(saved$family, "ranger"), requireNamespace("ranger", quietly = TRUE))
  x <- tabular_bake(features, saved$blueprint)
  list(ranger = predict(saved$model, data = x, num.threads = saved$threads)$predictions)
}
