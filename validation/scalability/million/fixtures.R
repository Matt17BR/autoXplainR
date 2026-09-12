# Synthetic scaling fixtures. Each column has its own fixed random stream, so
# increasing training size preserves all earlier observations. Evaluation uses
# independent streams and always contains the same 20,000 rows.
scale_fixture <- function(problem, n, evaluation = FALSE) {
  stopifnot(problem %in% c("regression", "rare_binary", "multiclass", "wide"))
  seed_offset <- if (evaluation) 100000L else 0L
  random <- function(stream, expression) {
    set.seed(7200L + seed_offset + stream)
    force(expression)
  }
  p <- if (problem == "wide") 128L else 20L
  columns <- lapply(seq_len(p), function(j) random(j, rnorm(n)))
  names(columns) <- paste0("x", seq_len(p))
  data <- as.data.frame(columns)
  data$x2 <- 0.8 * data$x1 + 0.6 * data$x2
  if (problem %in% c("rare_binary", "multiclass", "wide")) {
    levels <- if (problem == "wide") 1000L else 32L
    index <- random(300L, sample.int(levels, n, replace = TRUE))
    data$segment <- factor(index, levels = seq_len(levels))
    data$channel <- factor(random(301L, sample.int(5L, n, replace = TRUE)), levels = 1:5)
    segment_effect <- sin(index * 0.37)
  } else {
    segment_effect <- rep(0, n)
  }
  if (problem == "rare_binary") {
    score <- -5.8 + 1.4 * data$x1 + 0.8 * data$x2 +
      0.6 * data$x3 * data$x4 + 0.6 * segment_effect
    truth <- plogis(score)
    data$outcome <- factor(ifelse(random(400L, runif(n)) < truth, "event", "none"),
                           levels = c("none", "event"))
  } else if (problem == "multiclass") {
    scores <- cbind(
      1.5 * data$x1 + sin(2 * data$x3),
      -data$x1 + data$x2 * data$x4 + segment_effect,
      0.6 * data$x2 - data$x3 - 0.5,
      -1.5 + 0.8 * data$x5 + 0.5 * data$x6 * data$x7
    )
    row_max <- pmax(scores[, 1L], scores[, 2L], scores[, 3L], scores[, 4L])
    scores <- exp(scores - row_max)
    truth <- scores / rowSums(scores)
    cumulative <- cbind(truth[, 1L], truth[, 1L] + truth[, 2L],
                         truth[, 1L] + truth[, 2L] + truth[, 3L])
    draw <- random(400L, runif(n))
    selected <- 1L + rowSums(cumulative < draw)
    data$outcome <- factor(LETTERS[selected], levels = LETTERS[1:4])
    colnames(truth) <- LETTERS[1:4]
  } else {
    truth <- 2 * sin(data$x1) + 0.75 * (data$x2^2 - 1) +
      1.5 * data$x3 * data$x4 + 0.8 * data$x5 + segment_effect
    data$outcome <- truth + random(400L, rnorm(n, sd = 0.7))
  }
  # Missingness is independent of the noise/draw used for the outcome. Every
  # row remains eligible; the public workflow must learn imputation in folds.
  for (j in c(3L, 8L)) {
    missing <- random(500L + j, runif(n)) < 0.02
    data[[paste0("x", j)]][missing] <- NA_real_
  }
  if ("segment" %in% names(data)) {
    missing <- random(550L, runif(n)) < 0.01
    data$segment[missing] <- NA
  }
  list(data = data, truth = truth, provenance = list(
    kind = "deterministic synthetic data", version = "1", problem = problem,
    seed_base = 7200L, independent_seed_offset = seed_offset,
    stream_contract = "Independent per-column seeds; training prefixes are nested.",
    rows = n, predictors = ncol(data) - 1L,
    source_rows = c(first = 1L, last = n),
    role = if (evaluation) "fixed independent evaluation" else "training",
    noise_sd = if (problem %in% c("regression", "wide")) 0.7 else NULL
  ))
}

independent_scale_loss <- function(outcome, prediction) {
  if (is.numeric(outcome)) return(sqrt(mean((outcome - as.numeric(prediction))^2)))
  if (nlevels(outcome) == 2L) {
    p <- as.numeric(prediction)
    p <- pmin(1 - 1e-15, pmax(1e-15, p))
    event <- as.integer(outcome == levels(outcome)[[2L]])
    return(-mean(event * log(p) + (1 - event) * log1p(-p)))
  }
  p <- as.matrix(prediction)
  stopifnot(identical(colnames(p), levels(outcome)))
  observed <- p[cbind(seq_along(outcome), as.integer(outcome))]
  -mean(log(pmax(observed, 1e-15)))
}
