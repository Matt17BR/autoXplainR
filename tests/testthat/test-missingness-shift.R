test_that("missingness shift preserves and compares pre-imputation rates", {
  set.seed(812)
  training <- data.frame(x = rnorm(120), z = rnorm(120))
  training$y <- 2 * training$x - training$z + rnorm(120, sd = 0.3)
  evaluation <- data.frame(x = rnorm(60), z = rnorm(60))
  evaluation$y <- 2 * evaluation$x - evaluation$z + rnorm(60, sd = 0.3)
  training$x[seq_len(6)] <- NA
  evaluation$x[seq_len(30)] <- NA

  result <- autoxplain(training, "y", test_data = evaluation, seed = 5)
  diagnostic <- missingness_shift(result)
  x_row <- diagnostic$features[diagnostic$features$feature == "x", ]

  expect_s3_class(diagnostic, "autoxplain_missingness_shift")
  expect_equal(x_row$training_missing_rate, 0.05)
  expect_equal(x_row$evaluation_missing_rate, 0.5)
  expect_equal(x_row$absolute_shift, 0.45)
  expect_true(x_row$flagged)
  expect_true(x_row$used_by_model)
  expect_equal(diagnostic$n_flagged_model_features, 1L)
  expect_false(anyNA(result$training_data$x))
  expect_false(anyNA(result$test_data$x))
  expect_s3_class(
    result$evaluation$diagnostics$missingness_shift,
    "autoxplain_missingness_shift"
  )
  expect_true("missingness_shift" %in% result$evaluation$notes$code)
  expect_output(print(diagnostic), "not a statistical test")
})

test_that("missingness threshold is explicit and no-missing reports stay concise", {
  result <- autoxplain(mtcars, "mpg", seed = 19)
  diagnostic <- missingness_shift(result)

  expect_equal(diagnostic$n_with_missing, 0L)
  expect_equal(diagnostic$n_flagged, 0L)
  expect_equal(missingness_shift(result, threshold = 1)$n_flagged, 0L)
  expect_error(missingness_shift(result, threshold = -0.1), "threshold")

  path <- tempfile(fileext = ".html")
  render_model_report(result, path, top_features = 1, n_repeats = 2)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_false(grepl("Did missing data change", html, fixed = TRUE))
})

test_that("guided report explains missingness shift without calling it drift proof", {
  set.seed(92)
  training <- data.frame(x = rnorm(100), z = rnorm(100))
  training$y <- training$x + rnorm(100)
  evaluation <- data.frame(x = rnorm(50), z = rnorm(50))
  evaluation$y <- evaluation$x + rnorm(50)
  training$x[1:10] <- NA
  evaluation$x[1:20] <- NA
  result <- autoxplain(training, "y", test_data = evaluation, seed = 20)
  path <- tempfile(fileext = ".html")
  render_model_report(result, path, top_features = 1, n_repeats = 2)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(html, "Did missing data change between fitting and evaluation?", fixed = TRUE)
  expect_match(html, "before the configured", fixed = TRUE)
  expect_match(html, "not a statistical test", fixed = TRUE)
  expect_match(html, "Flagged model inputs", fixed = TRUE)
})

test_that("missingness shift requests retained per-column metadata", {
  result <- autoxplain(mtcars, "mpg", seed = 4)
  old_result <- result
  old_result$preprocessing_metadata$training_data$original_info$
    missing_fraction_by_column <- NULL
  expect_error(missingness_shift(old_result), "predates per-column")

  no_evaluation <- result
  no_evaluation$preprocessing_metadata$test_data <- NULL
  expect_error(missingness_shift(no_evaluation), "requires retained")
})
