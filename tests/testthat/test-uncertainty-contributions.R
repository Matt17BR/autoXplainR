uncertainty_probability_fixture <- function(task) {
  n <- 47L
  index <- seq_len(n)
  if (task == "regression") {
    predictions <- list(primary = sin(index / 4) + index / 50, reference = rep(0.3, n))
    outcome <- predictions$primary + cos(index / 3) * 0.4
  } else if (task == "binary") {
    # The event is deliberately the first factor level. Exact boundaries and
    # sub-epsilon probabilities distinguish clipping and arithmetic contracts.
    predictions <- list(
      primary = rep(c(0, 1, 1e-20, 1 - 1e-16, 0.5, 0.8, 0.03), length.out = n),
      reference = rep(c(0.1, 0.9, 1, 0, 0.6), length.out = n)
    )
    outcome <- factor(rep(c("yes", "no", "no"), length.out = n), levels = c("yes", "no"))
  } else {
    classes <- c("first", "second", "third", "absent")
    probability <- cbind(index %% 5 + 1, index %% 3 + 1, index %% 7 + 1, rep(2, n))
    probability <- probability / rowSums(probability)
    probability[1:3, ] <- rbind(c(0, 1, 0, 0), c(1, 0, 0, 0), c(0, 0, 1, 0))
    colnames(probability) <- classes
    predictions <- list(primary = probability[, c(4, 2, 1, 3)],
                        reference = probability[n:1, c(3, 1, 4, 2)])
    outcome <- factor(rep(classes[1:3], length.out = n), levels = classes)
  }
  data <- data.frame(index = index, outcome = outcome)
  rownames(data) <- paste0("source-", index * 7L)
  adapter <- function(model, newdata) {
    if (is.matrix(model)) model[newdata$index, , drop = FALSE] else model[newdata$index]
  }
  result <- evaluate_models(predictions, data, "outcome", task = task,
    predict_functions = list(primary = adapter, reference = adapter),
    reference = "reference", positive = if (task == "binary") "yes" else NULL
  )
  list(result = result, outcome = outcome, predictions = predictions)
}

# This oracle resamples literal outcomes and probabilities, then recalculates
# losses. It intentionally does not use the package's contribution or metric
# helpers, and does not precompute errors before resampling.
literal_bootstrap_score <- function(rows, fixture, metric) {
  y <- fixture$outcome[rows]
  values <- vapply(fixture$predictions, function(prediction) {
    p <- if (is.matrix(prediction)) prediction[rows, , drop = FALSE] else prediction[rows]
    if (metric == "rmse") return(sqrt(mean((y - p)^2)))
    if (metric == "mae") return(mean(abs(y - p)))
    if (!is.matrix(p)) {
      event <- as.numeric(y == "yes")
      if (metric == "brier_score") return(mean((p - event)^2))
      p[p < 1e-15] <- 1e-15
      p[p > 1 - 1e-15] <- 1 - 1e-15
      return(-mean(event * log(p) + (1 - event) * log(1 - p)))
    }
    if (metric == "brier_score") {
      p <- p[, levels(y), drop = FALSE]
      observed <- outer(as.character(y), levels(y), `==`)
      return(mean(rowSums((p - observed)^2)))
    }
    correct_class <- p[cbind(seq_along(y), match(as.character(y), colnames(p)))]
    correct_class[correct_class < 1e-15] <- 1e-15
    -mean(log(correct_class))
  }, numeric(1))
  c(primary = values[[1L]], baseline = values[[2L]], difference = values[[1L]] - values[[2L]])
}

test_that("every supported bootstrap loss agrees with literal row and whole-group resampling", {
  for (task in c("regression", "binary", "multiclass")) {
    fixture <- uncertainty_probability_fixture(task)
    metrics <- if (task == "regression") c("rmse", "mae") else c("log_loss", "brier_score")
    for (grouped in c(FALSE, TRUE)) {
      labels <- if (grouped) rep(c("site-z", "site-a", "site-9", "site-b"), c(3, 9, 7, 28)) else NULL
      result <- fixture$result
      if (grouped) {
        result$validation <- list(method = "group", evaluation_groups = labels,
                                  evaluation_row_names = rownames(result$test_data))
      }
      for (metric in metrics) {
        # Select another already recorded metric; models and assessment rows
        # stay unchanged. The public workflow has no supplied-model metric switch.
        result$evaluation$primary_metric <- metric
        result <- AutoXplainR:::seal_evaluation_result(result)
        set.seed(551)
        caller_rng <- .Random.seed
        output <- performance_uncertainty(result, n_boot = 29L, seed = 87L, confidence = 0.75)
        expect_identical(.Random.seed, caller_rng)
        oracle <- withr::with_seed(87L, t(replicate(29L, {
          rows <- if (grouped) {
            selected <- sample(unique(labels), length(unique(labels)), replace = TRUE)
            unlist(lapply(selected, function(label) which(labels == label)), use.names = FALSE)
          } else {
            sample.int(length(fixture$outcome), length(fixture$outcome), replace = TRUE)
          }
          literal_bootstrap_score(rows, fixture, metric)
        })))
        expect_identical(output$draws, as.data.frame(oracle))
        expected_interval <- apply(oracle, 2L, quantile, probs = c(0.125, 0.875), names = FALSE, type = 7)
        expect_identical(output$estimates$estimate,
                         unname(literal_bootstrap_score(seq_along(fixture$outcome), fixture, metric)))
        expect_identical(output$estimates$lower, unname(expected_interval[1L, ]))
        expect_identical(output$estimates$upper, unname(expected_interval[2L, ]))
        expect_identical(output$unit, if (grouped) "group" else "observation")
        expect_identical(output$units, if (grouped) 4L else 47L)
        expect_identical(output$reference_model_id, "reference")
      }
    }
  }
})

test_that("bootstrap keeps degenerate comparisons and restores an absent RNG", {
  fixture <- uncertainty_probability_fixture("regression")
  fixture$result$models$reference <- fixture$result$models$primary
  # Re-evaluate the deliberately identical models through the public boundary.
  result <- evaluate_models(fixture$result$models, fixture$result$test_data, "outcome",
    predict_functions = lapply(fixture$result$prediction_contracts, `[[`, "predict_function"),
    reference = "reference"
  )
  original <- if (exists(".Random.seed", globalenv(), inherits = FALSE)) get(".Random.seed", globalenv()) else NULL
  withr::defer({
    if (is.null(original)) {
      if (exists(".Random.seed", globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
    } else {
      assign(".Random.seed", original, envir = globalenv()) # nolint: object_name_linter. R's RNG state binding.
    }
  })
  if (exists(".Random.seed", globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  output <- performance_uncertainty(result, n_boot = 20L)
  expect_false(exists(".Random.seed", globalenv(), inherits = FALSE))
  expect_identical(output$draws$difference, rep(0, 20L))
  expect_true(any(grepl("degenerate", output$notes, fixed = TRUE)))
})

test_that("perfect multiclass log loss retains its negative zero", {
  classes <- c("a", "b", "c")
  data <- data.frame(index = seq_len(9L), outcome = factor(rep(classes, 3L), levels = classes))
  probability <- diag(3L)[rep(seq_len(3L), 3L), , drop = FALSE]
  colnames(probability) <- classes
  adapter <- function(model, newdata) model[newdata$index, , drop = FALSE]
  result <- evaluate_models(list(primary = probability, reference = probability), data, "outcome",
    predict_functions = list(primary = adapter, reference = adapter), reference = "reference"
  )
  output <- performance_uncertainty(result, n_boot = 20L)
  expect_identical(1 / output$draws$primary, rep(-Inf, 20L))
  expect_identical(1 / output$draws$baseline, rep(-Inf, 20L))
  expect_identical(1 / output$draws$difference, rep(Inf, 20L))
  expect_identical(1 / output$estimates$estimate, c(-Inf, -Inf, Inf))
})
