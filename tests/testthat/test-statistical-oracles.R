test_that("PDP follows the closed-form additive model and null inputs stay null", {
  data <- data.frame(x = seq(-2, 2, length.out = 101), z = sin(seq_len(101)))
  data$y <- 3 * data$x - 2 * data$z
  model <- explain_model(list(), data, "y", task = "regression",
                         predict_function = function(newdata) 3 * newdata$x - 2 * newdata$z)
  pdp <- calculate_partial_dependence(model, feature = "x", n_points = 9)
  expect_equal(pdp$partial_dependence, 3 * pdp$x - 2 * mean(data$z), tolerance = 1e-10)
  ale <- calculate_accumulated_local_effects(model, feature = "x", n_points = 9)
  expect_equal(diff(ale$accumulated_effect), 3 * diff(ale$x), tolerance = 1e-10)
  null <- explain_model(list(), data, "y", task = "regression",
                        predict_function = function(newdata) 3 * newdata$x)
  importance <- calculate_permutation_importance(null, features = "z", n_repeats = 10)
  expect_equal(importance$importance, 0)
  expect_equal(importance$std_error, 0)
})

test_that("permutation draws match direct loss calculations", {
  data <- data.frame(x = seq_len(12), y = 2 * seq_len(12))
  model <- explain_model(list(), data, "y", task = "regression",
                         predict_function = function(newdata) 2 * newdata$x)
  importance <- calculate_permutation_importance(model, n_repeats = 20, seed = 61)
  direct <- withr::with_seed(61, replicate(20, {
    shuffled <- sample.int(nrow(data))
    sqrt(mean((data$y - 2 * data$x[shuffled])^2))
  }))
  expect_equal(as.numeric(attr(importance, "repeat_scores")), direct)
  expect_equal(importance$importance, mean(direct))
  expect_equal(importance$std_error, sd(direct) / sqrt(20))
})

test_that("binary AUC handles ties and agrees with pairwise concordance", {
  truth <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
  probability <- c(0.5, 0.5, 0.9, 0.2, 0.1)
  pairs <- outer(probability[truth], probability[!truth], "-")
  expected <- mean((pairs > 0) + 0.5 * (pairs == 0))
  expect_equal(AutoXplainR:::guided_binary_auc(truth, probability), expected)
})

test_that("imputation learns from complete training columns before evaluation missingness", {
  train <- data.frame(x = 1:20, group = rep(c("a", "b"), 10), y = sin(1:20))
  test <- data.frame(x = c(NA, 3, 1000), group = c(NA, "a", "b"), y = c(2, 4, 1))
  result <- autoxplain(train, "y", test_data = test, explain = FALSE)
  expect_equal(result$test_data$x[1], median(train$x))
  expect_false(anyNA(result$test_data))
  expect_equal(result$preprocessing_metadata$training_data$preprocessing_log$missing_values$imputed_columns,
               character())
})

test_that("evidence export excludes case values and model objects", {
  data <- transform(mtcars, case_id = paste0("private-case-", seq_len(nrow(mtcars))))
  result <- autoxplain(data, "mpg", validation = validation_split(group = "case_id"))
  summary <- evidence_summary(result)
  text <- paste(capture.output(str(summary, max.level = 10)), collapse = "\n")
  expect_identical(summary$schema_version, "2.0")
  expect_false(grepl("private-case-", text, fixed = TRUE))
  expect_false("models" %in% names(summary))
  expect_equal(summary$evaluation$metrics, result$evaluation$metrics)
})
