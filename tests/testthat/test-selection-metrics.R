test_that("AUC scores have the natural orientation and half credit for ties", {
  truth <- c(FALSE, FALSE, TRUE, TRUE)
  auc <- AutoXplainR:::selection_binary_auc
  expect_equal(auc(truth, c(.1, .4, .35, .8)), .75)
  expect_equal(auc(truth, c(.2, .2, .2, .8)), .75)
  expect_equal(auc(truth, c(.9, .6, .65, .2)), .25)
  expect_equal(auc(truth, rep(.5, 4)), .5)
  expect_equal(auc(truth, c(0, 0, 1, 1)), 1)
  expect_equal(auc(truth, c(1, 1, 0, 0)), 0)

  # Clipping before ranking would turn these strict orderings into ties.
  expect_equal(auc(c(TRUE, FALSE), c(1 - 1e-16, 1 - 2e-16)), 1)
  expect_equal(auc(c(TRUE, FALSE), c(1e-300, 0)), 1)
})

test_that("AUC agrees with explicit positive-negative pair comparisons", {
  truth <- c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)
  probability <- c(.8, .8, .2, .2, .4, 1, 0, .2, .5)
  positive <- probability[truth]
  negative <- probability[!truth]
  pairs <- outer(positive, negative, `-`)
  expected <- (sum(pairs > 0) + sum(pairs == 0) / 2) / length(pairs)
  expect_equal(AutoXplainR:::selection_binary_auc(truth, probability), expected)
  expect_equal(AutoXplainR:::selection_binary_auc(truth, probability^2), expected)
  expect_equal(AutoXplainR:::selection_binary_auc(rep(truth, 2), rep(probability, 2)), expected)
})

test_that("AUC rejects undefined folds and invalid probability inputs", {
  auc <- AutoXplainR:::selection_binary_auc
  expect_error(auc(rep(TRUE, 3), c(.1, .4, .8)), "both outcome classes")
  expect_error(auc(c(TRUE, FALSE, NA), c(.1, .4, .8)), "nonmissing logical")
  expect_error(auc(c(1, 0), c(.1, .4)), "nonmissing logical")
  expect_error(auc(c(TRUE, FALSE), c(.1, .4, .8)), "same length")
  expect_error(auc(c(TRUE, FALSE), c(.1, Inf)), "finite numeric")
  expect_error(auc(c(TRUE, FALSE), c(NA, .2)), "finite numeric")
  expect_error(auc(c(TRUE, FALSE), c(.1, -.001)), "between zero and one")
  expect_error(auc(c(TRUE, FALSE), c(.1, 1.001)), "between zero and one")
  expect_error(auc(c(TRUE, FALSE), factor(c(.1, .2))), "finite numeric")
})

test_that("RMSLE matches known log-space errors and preserves very large targets", {
  observed <- expm1(c(1, 2, 3))
  predicted <- expm1(c(2, 2, 1))
  expect_equal(AutoXplainR:::selection_rmsle_case_loss(observed, predicted), c(1, 0, 4))
  expect_equal(AutoXplainR:::selection_rmsle(observed, predicted), sqrt(5 / 3))
  expect_equal(AutoXplainR:::selection_rmsle(c(0, 1, 3), c(1, 3, 7)), log(2))
  expect_equal(AutoXplainR:::selection_rmsle(c(0, 1e308), c(0, 1e308)), 0)
})

test_that("RMSLE rejects negatives instead of changing the prediction function", {
  rmsle <- AutoXplainR:::selection_rmsle
  expect_error(rmsle(c(-.1, 1), c(0, 1)), "nonnegative outcomes.*not clipped")
  expect_error(rmsle(c(0, 1), c(-1e-300, 1)), "nonnegative predictions.*not clipped")
  expect_error(rmsle(c(0, NA), c(0, 1)), "outcomes.*finite numeric")
  expect_error(rmsle(c(0, 1), c(0, Inf)), "predictions.*finite numeric")
  expect_error(rmsle(1:3, 1:2), "same length")
  expect_error(rmsle(factor(c(0, 1)), c(0, 1)), "outcomes.*finite numeric")
  expect_error(rmsle(numeric(), numeric()), "nonempty")
  expect_error(rmsle(c(0, 1), matrix(c(0, 1))), "numeric vector")
})

test_that("RMSLE fold aggregation pools squared log error with unequal row counts", {
  # Two rows have log error 1, six rows have log error 2: MSLE is 26 / 8.
  summary <- AutoXplainR:::selection_fold_summary(c(1, 2), c(2L, 6L), "rmsle")
  expect_equal(summary$score, sqrt(26 / 8))
  expect_equal(summary$sd, sqrt(4.5) / (2 * sqrt(26 / 8)))
  expect_equal(summary$se, sqrt(4.5 * 5 / 8) / (2 * sqrt(26 / 8)))
  zero <- AutoXplainR:::selection_fold_summary(c(0, 0), c(2L, 6L), "rmsle")
  expect_identical(zero, list(score = 0, sd = 0, se = 0))
})

test_that("AUC fold aggregation is a row-weighted within-fold average", {
  # First fold has an inverted pair, second fold has perfect within-fold order.
  # A pooled ROC AUC would also compare predictions from different fitted models.
  first_truth <- c(FALSE, TRUE)
  first_score <- c(.2, .1)
  second_truth <- c(FALSE, FALSE, TRUE, TRUE)
  second_score <- c(.7, .8, .9, 1)
  scores <- c(
    AutoXplainR:::selection_binary_auc(first_truth, first_score),
    AutoXplainR:::selection_binary_auc(second_truth, second_score)
  )
  summary <- AutoXplainR:::selection_fold_summary(scores, c(2L, 4L), "roc_auc")
  pooled_auc <- AutoXplainR:::selection_binary_auc(
    c(first_truth, second_truth), c(first_score, second_score)
  )
  expect_equal(summary$score, 2 / 3)
  expect_equal(pooled_auc, 2 / 3)
  # Moving scores within the first fold changes cross-fold rankings but no
  # within-fold ordering. This prevents accidentally substituting pooled AUC.
  shifted_auc <- AutoXplainR:::selection_binary_auc(
    c(first_truth, second_truth), c(.95, .94, second_score)
  )
  expect_equal(shifted_auc, 7 / 9)
  expect_equal(summary$sd, sqrt(.5))
  expect_equal(summary$se, sqrt(5 / 18))
})

test_that("score direction governs ordering and the one-SE eligibility boundary", {
  loss <- AutoXplainR:::selection_metric_loss
  threshold <- AutoXplainR:::selection_metric_threshold
  eligible <- AutoXplainR:::selection_metric_eligible
  expect_identical(AutoXplainR:::selection_metric_direction("roc_auc"), "maximize")
  expect_identical(AutoXplainR:::selection_metric_direction("rmsle"), "minimize")
  expect_identical(order(loss(c(.7, .9, .8), "roc_auc")), c(2L, 3L, 1L))
  expect_identical(order(loss(c(.7, .9, .8), "rmsle")), c(1L, 3L, 2L))
  expect_equal(threshold(.9, .05, "roc_auc"), .85)
  expect_identical(eligible(c(.8, .85, .9, NA), .85, "roc_auc"), c(FALSE, TRUE, TRUE, FALSE))
  expect_equal(threshold(.1, .05, "rmsle"), .15)
  expect_identical(eligible(c(.1, .15, .2, NA), .15, "rmsle"), c(TRUE, TRUE, FALSE, FALSE))
})

test_that("fold summaries do not silently discard failed scores or rows", {
  summary <- AutoXplainR:::selection_fold_summary
  expect_identical(summary(numeric(), integer(), "roc_auc"),
    list(score = NA_real_, sd = NA_real_, se = NA_real_)
  )
  expect_identical(summary(.8, 2L, "roc_auc"), list(score = .8, sd = NA_real_, se = NA_real_))
  expect_error(summary(c(.8, NA), c(2L, 3L), "roc_auc"), "finite scores")
  expect_error(summary(c(.8, .9), c(2L, 0L), "roc_auc"), "positive integer")
  expect_error(summary(c(.8, .9), c(2, 2.5), "roc_auc"), "positive integer")
  expect_error(summary(c(.8, .9), c(2L, 3L, 4L), "roc_auc"), "matching")
})
