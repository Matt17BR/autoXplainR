test_that("weighted efficiency validates and warns about subjectivity", {
  expect_warning(
    score <- calculate_weighted_efficiency(c(0.7, 0.8, 0.9), c(1, 3, 8)),
    "candidate-set-relative"
  )
  expect_true(all(score >= 0 & score <= 1))
  expect_error(calculate_weighted_efficiency(1, 2), "at least two")
  expect_error(calculate_weighted_efficiency(c(1, NA), c(1, 2)), "finite")
})

test_that("guided models expose useful engine-neutral metadata", {
  result <- autoxplain(mtcars, "mpg", seed = 11)
  metadata <- result$model_characteristics

  expect_s3_class(metadata, "autoxplainr_model_characteristics")
  expect_equal(length(metadata), 2L)
  expect_equal(metadata[[1L]]$algorithm, "linear regression")
  expect_true(is.finite(metadata[[1L]]$size_bytes))
  expect_true(is.finite(metadata[[1L]]$training_time_s))
  expect_true(is.finite(metadata[[1L]]$complexity))
  expect_match(metadata[[1L]]$hyperparameters$formula, "mpg")
  expect_null(metadata[[1L]]$native_variable_importance)
  expect_output(print(metadata), "linear regression")
  expect_named(summary(metadata), c("summary", "models"))

  minimal <- extract_model_characteristics(
    result,
    include_hyperparams = FALSE,
    include_performance = FALSE,
    include_varimp = FALSE
  )
  expect_false("hyperparameters" %in% names(minimal[[1L]]))
  expect_false("performance_metrics" %in% names(minimal[[1L]]))
})

test_that("metadata reports are escaped and validated", {
  result <- autoxplain(mtcars, "mpg", seed = 12)
  metadata <- result$model_characteristics
  names(metadata)[[1L]] <- "<unsafe>"
  metadata[[1L]]$model_id <- "<unsafe>"
  path <- tempfile(fileext = ".html")
  output <- create_model_comparison_report(metadata, path)
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")

  expect_match(html, "&lt;unsafe&gt;", fixed = TRUE)
  expect_false(grepl("<unsafe>", html, fixed = TRUE))
  expect_error(create_model_comparison_report(list(), path), "must be returned")
  expect_error(create_model_comparison_report(metadata, tempfile(fileext = ".txt")), "html")
  expect_error(extract_model_characteristics(list()), "must be returned")
  expect_error(extract_model_characteristics(result, include_varimp = NA), "TRUE or FALSE")
})

test_that("model metadata helpers handle supported families", {
  binary <- glm(am ~ wt, mtcars, family = binomial())
  expect_equal(AutoXplainR:::friendly_model_type(binary), "logistic regression")
  expect_equal(AutoXplainR:::friendly_model_type(list()), "list")
  expect_equal(AutoXplainR:::base_model_hyperparameters(binary)$family, "binomial")
  expect_equal(AutoXplainR:::minmax(c(2, 2)), c(0.5, 0.5))
})
