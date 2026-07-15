test_that("preprocessing is conservative by default", {
  data <- data.frame(id = 1:5, category = c("a", "b", "a", "b", "a"), y = 1:5)
  result <- preprocess_data(data, "y")
  expect_true("id" %in% names(result$data))
  expect_true(is.factor(result$data$category))

  compatibility_result <- preprocess_for_h2o(data, "y")
  expect_equal(compatibility_result, result)

  removed <- preprocess_for_h2o(data, "y", enable_id_removal = TRUE)
  expect_false("id" %in% names(removed$data))
  expect_equal(removed$recipe$removed_columns, "id")
  expect_output(print(removed), "missing strategy")
})

test_that("training imputations and levels are reused", {
  train <- data.frame(group = c("a", "a", "b", NA), x = c(1, 2, NA, 4), y = 1:4)
  fitted <- preprocess_for_h2o(train, "y", missing_value_strategy = "impute")
  test <- data.frame(group = c("a", NA), x = c(NA, 10), y = c(5, 6))
  applied <- AutoXplainR:::apply_preprocessing_recipe(test, fitted$recipe, "y")
  expect_false(anyNA(applied$data))
  expect_equal(levels(applied$data$group), levels(fitted$data$group))

  unseen <- data.frame(group = "c", x = 1, y = 1)
  expect_error(
    AutoXplainR:::apply_preprocessing_recipe(unseen, fitted$recipe, "y"),
    "unseen levels"
  )
})

test_that("missing strategies and inputs are validated", {
  data <- data.frame(x = c(1, NA, 3), y = 1:3)
  expect_equal(nrow(preprocess_for_h2o(data, "y", missing_value_strategy = "remove_rows")$data), 2)
  expect_error(preprocess_for_h2o(data, "y", missing_value_strategy = "invent"))
  expect_error(preprocess_for_h2o(data, "missing"), "name one column")
})
