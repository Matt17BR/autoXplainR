test_that("binary calibration compares positive-class probability with frequency", {
  set.seed(440)
  data <- data.frame(x = rnorm(240), z = rnorm(240))
  data$accepted <- factor(ifelse(
    stats::plogis(1.4 * data$x - 0.6 * data$z) > runif(240),
    "yes",
    "no"
  ))
  result <- autoxplain(data, "accepted", test_fraction = 0.4, seed = 17)
  diagnostic <- calibration_diagnostics(result, bins = 5)

  expect_s3_class(diagnostic, "autoxplain_calibration")
  expect_equal(diagnostic$model_id, "main_model")
  expect_equal(diagnostic$task, "binary")
  expect_equal(sum(diagnostic$groups$rows), nrow(result$test_data))
  expect_lte(diagnostic$n_groups, 5L)
  expect_true(all(diagnostic$groups$mean_probability >= 0 &
                    diagnostic$groups$mean_probability <= 1))
  expect_equal(
    diagnostic$calibration_error,
    weighted.mean(diagnostic$groups$calibration_gap, diagnostic$groups$rows)
  )
  expect_equal(
    result$evaluation$metrics$main_model[["calibration_error"]],
    diagnostic$calibration_error
  )
  expect_output(print(diagnostic), "binned gap")
})

test_that("multiclass calibration checks predicted-class confidence", {
  result <- autoxplain(iris, "Species", test_fraction = 0.5, seed = 91)
  diagnostic <- calibration_diagnostics(result, bins = 4)

  expect_equal(diagnostic$task, "multiclass")
  expect_match(diagnostic$event_label, "predicted class")
  expect_equal(diagnostic$observed_rate, mean(result$evaluation$predictions$correct))
  expect_equal(
    diagnostic$mean_probability,
    mean(result$evaluation$predictions$primary_confidence)
  )
  expect_true("calibration_error" %in% names(result$leaderboard))
  expect_equal(
    result$evaluation$diagnostics$calibration$calibration_error,
    diagnostic$calibration_error
  )
})

test_that("calibration handles ties and rejects unsupported requests", {
  grouped <- AutoXplainR:::calibration_groups(rep(0.5, 20), rep(c(TRUE, FALSE), 10), 5)
  expect_equal(nrow(grouped), 1L)
  expect_equal(grouped$calibration_gap, 0)

  regression <- autoxplain(mtcars, "mpg", seed = 7)
  expect_error(calibration_diagnostics(regression), "classification probabilities")
  classification <- autoxplain(iris, "Species", seed = 7)
  expect_error(calibration_diagnostics(classification, bins = 0), "bins")
  expect_error(calibration_diagnostics(classification, model = c(1, 2)), "exactly one")
  expect_error(
    AutoXplainR:::calibration_groups(c(0.2, 1.2), c(TRUE, FALSE), 2),
    "finite probabilities"
  )
})
