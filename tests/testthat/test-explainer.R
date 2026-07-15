test_that("regression explainers validate and predict", {
  fixture <- make_regression_fixture()
  explainer <- explain_model(
    fixture$model, fixture$test, "y", label = "linear",
    metadata = list(evaluation_role = "test")
  )

  expect_s3_class(explainer, "autoxplain_explainer")
  expect_equal(explainer$task, "regression")
  expect_false("y" %in% names(explainer$data))
  expect_length(predict(explainer, explainer$data), nrow(fixture$test))
  expect_output(print(explainer), "AutoXplainR explainer")
})

test_that("numeric two-level outcomes are binary", {
  fixture <- make_binary_fixture()
  explainer <- explain_model(fixture$model, fixture$test, "y")

  expect_equal(explainer$task, "binary")
  expect_equal(explainer$positive, "1")
  prediction <- predict(explainer, explainer$data)
  expect_true(all(prediction >= 0 & prediction <= 1))
})

test_that("custom prediction functions support one or two arguments", {
  data <- data.frame(x = 1:5, y = 2 * (1:5))
  model <- list(coefficient = 2)
  one <- explain_model(
    model, data, "y",
    predict_function = function(newdata) 2 * newdata$x
  )
  two <- explain_model(
    model, data, "y",
    predict_function = function(model, newdata) model$coefficient * newdata$x
  )
  expect_equal(predict(one, one$data), data$y)
  expect_equal(predict(two, two$data), data$y)
})

test_that("multiclass probability contracts are validated", {
  data <- data.frame(x = rep(1:3, each = 3), y = factor(rep(c("a", "b", "c"), 3)))
  probability_function <- function(newdata) {
    probability <- matrix(0.1, nrow(newdata), 3L,
                          dimnames = list(NULL, c("a", "b", "c")))
    probability[cbind(seq_len(nrow(newdata)), newdata$x)] <- 0.8
    probability
  }
  explainer <- explain_model(
    list(), data, "y", task = "multiclass",
    predict_function = probability_function
  )
  importance <- calculate_permutation_importance(
    explainer, metric = "logloss", n_repeats = 3, seed = 4
  )

  expect_equal(explainer$task, "multiclass")
  expect_equal(dim(predict(explainer, explainer$data)), c(nrow(data), 3L))
  expect_s3_class(importance, "autoxplain_importance")
  expect_error(
    explain_model(
      list(), data, "y", task = "multiclass",
      predict_function = function(newdata) matrix(0.5, nrow(newdata), 3L)
    ),
    "named"
  )
})

test_that("explainer errors are targeted", {
  model <- lm(mpg ~ wt, mtcars)
  expect_error(explain_model(model, mtcars, "missing"), "one value")
  expect_error(explain_model(model, mtcars, y = 1:3), "one value")
  expect_error(
    explain_model(model, mtcars, "mpg", predict_function = function(newdata) 1),
    "predictions"
  )
  explainer <- explain_model(model, mtcars, "mpg")
  expect_error(predict(explainer, data.frame(nope = 1)), "missing required")
})
