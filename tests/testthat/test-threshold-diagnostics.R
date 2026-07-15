test_that("threshold table computes binary decision trade-offs exactly", {
  performance <- AutoXplainR:::threshold_performance(
    truth = c(TRUE, TRUE, FALSE, FALSE),
    probability = c(0.9, 0.4, 0.6, 0.1),
    thresholds = c(0.3, 0.5),
    false_positive_cost = 1,
    false_negative_cost = 2
  )
  half <- performance[performance$threshold == 0.5, ]

  expect_equal(half$sensitivity, 0.5)
  expect_equal(half$specificity, 0.5)
  expect_equal(half$precision, 0.5)
  expect_equal(half$negative_predictive_value, 0.5)
  expect_equal(half$accuracy, 0.5)
  expect_equal(half$f1, 0.5)
  expect_equal(half$false_positives, 1)
  expect_equal(half$false_negatives, 1)
  expect_equal(half$expected_cost, 0.75)
})

test_that("public threshold diagnostics preserve the held-out model contract", {
  set.seed(501)
  data <- data.frame(x = rnorm(240), z = rnorm(240))
  data$accepted <- factor(ifelse(
    stats::plogis(1.5 * data$x - data$z) > runif(240), "yes", "no"
  ))
  result <- autoxplain(data, "accepted", test_fraction = 0.4, seed = 22)
  diagnostic <- threshold_diagnostics(
    result,
    thresholds = c(0.8, 0.2, 0.5, 0.5),
    false_positive_cost = 1,
    false_negative_cost = 3
  )

  expect_s3_class(diagnostic, "autoxplain_thresholds")
  expect_equal(diagnostic$performance$threshold, c(0.2, 0.5, 0.8))
  expect_equal(diagnostic$positive_class, levels(result$training_data$accepted)[[2L]])
  expect_equal(diagnostic$rows, nrow(result$test_data))
  expect_true(all(diff(diagnostic$performance$predicted_positive_rate) <= 0))
  expect_true(all(diff(diagnostic$performance$sensitivity) <= 0))
  expect_true(all(diff(diagnostic$performance$specificity) >= 0))
  expect_equal(
    diagnostic$lowest_observed_cost,
    min(diagnostic$performance$expected_cost)
  )
  expect_match(diagnostic$scope_note, "different data", fixed = TRUE)
  expect_output(print(diagnostic), "descriptive only")
})

test_that("threshold diagnostics reject unsupported or ambiguous requests", {
  regression <- autoxplain(mtcars, "mpg", seed = 3)
  binary_data <- transform(mtcars, am = factor(am, labels = c("auto", "manual")))
  binary <- autoxplain(binary_data, "am", model_set = "comparison", seed = 3)

  expect_error(threshold_diagnostics(regression), "binary-classification")
  expect_error(threshold_diagnostics(binary, thresholds = numeric()), "thresholds")
  expect_error(threshold_diagnostics(binary, thresholds = c(-0.1, 0.5)), "thresholds")
  expect_error(
    threshold_diagnostics(binary, false_positive_cost = -1),
    "false_positive_cost"
  )
  expect_error(
    threshold_diagnostics(
      binary, false_positive_cost = 0, false_negative_cost = 0
    ),
    "greater than zero"
  )
  expect_error(
    threshold_diagnostics(binary, model = c("main_model", "small_tree")),
    "exactly one"
  )
})

test_that("guided binary reports explain cutoff sensitivity without optimizing it", {
  set.seed(88)
  data <- data.frame(x = rnorm(160), z = rnorm(160))
  data$event <- factor(ifelse(data$x + rnorm(160) > 0, "yes", "no"))
  binary <- autoxplain(data, "event", seed = 17)
  binary_path <- tempfile(fileext = ".html")
  render_model_report(binary, binary_path, top_features = 1, n_repeats = 2)
  binary_html <- paste(readLines(binary_path, warn = FALSE), collapse = "\n")

  expect_match(
    binary_html,
    "What changes when the decision cutoff moves?",
    fixed = TRUE
  )
  expect_match(binary_html, "No cutoff is recommended here", fixed = TRUE)
  expect_match(binary_html, "False positives", fixed = TRUE)

  regression <- autoxplain(mtcars, "mpg", seed = 17)
  regression_path <- tempfile(fileext = ".html")
  render_model_report(regression, regression_path, top_features = 1, n_repeats = 2)
  regression_html <- paste(readLines(regression_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("decision cutoff moves", regression_html, fixed = TRUE))
})
