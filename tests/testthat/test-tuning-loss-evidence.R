test_that("binary case losses apply the fold-scoring clipping convention", {
  probabilities <- c(0, 1, 1e-18, 1 - 1e-14, .5, .25)
  outcome <- factor(c("event", "other", "event", "other", "other", "event"), c("other", "event"))
  contract <- list(y = outcome, task = "binary", class_levels = levels(outcome), positive = "event")
  configuration <- data.frame(configuration_id = "tree_01", family = "tree", backend = "rpart")
  fold <- list(validation_row = seq_along(outcome), source_row = paste0("case-", seq_along(outcome)))
  epsilon <- 1e-15
  upper <- 1 - epsilon
  expected <- list(
    log_loss = c(-log(epsilon), -log(1 - upper), -log(epsilon), -log(1 - (1 - 1e-14)), -log(.5), -log(.25)),
    brier_score = c((epsilon - 1)^2, upper^2, (epsilon - 1)^2, (1 - 1e-14)^2, .5^2, (.25 - 1)^2)
  )
  for (metric in names(expected)) {
    records <- AutoXplainR:::format_tuning_predictions(
      configuration, fold, contract, probabilities, "binary", 1L, metric
    )
    expect_equal(records$case_loss, expected[[metric]], tolerance = 1e-15)
    if (metric == "brier_score") expect_identical(records$case_loss, expected[[metric]])
    expect_identical(as.numeric(records$probabilities[, "event"]), probabilities)
    expect_equal(mean(records$case_loss), AutoXplainR:::tuning_prediction_loss(
      outcome, probabilities, metric, contract
    ), tolerance = 1e-15)
  }
})

test_that("pooled binary case losses reproduce CV with pure leaves and omitted rows", {
  training <- data.frame(x = rep(c(-1, 1), 24))
  training$y <- factor(ifelse(training$x < 0, "negative", "positive"))
  training$y[48] <- "negative"
  omitted <- c(2L, 5L, 14L, 18L, 35L, 37L)
  training$x[omitted] <- NA_real_
  evaluation <- data.frame(x = c(-.9, -.8, .8, .9), y = factor(
    c("negative", "negative", "positive", "positive"), levels = levels(training$y)
  ))
  for (metric in c("log_loss", "brier")) {
    result <- autoxplain(training, "y", test_data = evaluation, learners = "tree", explain = FALSE,
      preprocessing_config = list(missing_value_strategy = "drop_rows"),
      tuning_control = tuning_control(
        fold_ids = rep(1:3, c(12, 16, 20)), metric = metric,
        grids = list(tree = list(cp = 0, minsplit = 2L, maxdepth = 2L)), family_budgets = c(tree = 1L)
      )
    )
    records <- result$tuning$out_of_fold_predictions
    expect_identical(result$tuning$omitted_rows$training_row, omitted)
    expect_setequal(records$training_row, setdiff(seq_len(nrow(training)), omitted))
    expect_true(any(records$probabilities == 0))
    expect_true(any(records$probabilities == 1))
    expect_equal(mean(records$case_loss), result$tuning$candidates$cv_score, tolerance = 1e-15)
  }
})

test_that("binary OOF ties follow the public positive-class threshold", {
  training <- data.frame(x = seq_len(48), y = factor(rep(c("negative", "positive"), 24)))
  evaluation <- data.frame(x = c(.2, .4, .6, .8), y = factor(
    c("negative", "positive", "negative", "positive"), levels = levels(training$y)
  ))
  result <- autoxplain(training, "y", test_data = evaluation, learners = "tree", explain = FALSE,
    tuning_control = tuning_control(
      fold_ids = rep(1:3, each = 16),
      grids = list(tree = list(cp = 1, minsplit = 20L, maxdepth = 1L)), family_budgets = c(tree = 1L)
    )
  )
  records <- result$tuning$out_of_fold_predictions
  expect_true(all(records$probabilities == .5))
  expect_identical(records$predicted_class, rep("positive", nrow(records)))
  expect_true(all(predict(result, evaluation, type = "class") == "positive"))
  expect_identical(predict(result, evaluation), rep(.5, nrow(evaluation)))
})
