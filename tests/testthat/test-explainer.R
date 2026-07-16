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

test_that("regression matrix predictions require an identifiable prediction column", {
  expect_equal(
    AutoXplainR:::normalize_predictions(
      data.frame(extra = c(100, 200), predict = c(1.5, 2.5)),
      "regression"
    ),
    c(1.5, 2.5)
  )
  expect_equal(
    AutoXplainR:::normalize_predictions(matrix(c(1.5, 2.5), ncol = 1L), "regression"),
    c(1.5, 2.5)
  )
  expect_error(
    AutoXplainR:::normalize_predictions(
      matrix(c(1, 2, 3, 4), nrow = 2L),
      "regression"
    ),
    "multiple columns must include a column named `predict`"
  )
})

test_that("explicit explainer tasks must match the observed outcome", {
  categorical <- data.frame(
    x = seq_len(6),
    y = factor(rep(c("a", "b", "c"), 2))
  )
  expect_error(
    explain_model(
      list(),
      categorical,
      "y",
      task = "regression",
      predict_function = function(newdata) rep(1, nrow(newdata))
    ),
    "finite numeric target"
  )

  binary <- data.frame(
    x = seq_len(6),
    y = factor(rep(c("no", "yes"), 3))
  )
  expect_error(
    explain_model(
      list(),
      binary,
      "y",
      task = "multiclass",
      predict_function = function(newdata) {
        matrix(
          0.5,
          nrow = nrow(newdata),
          ncol = 2L,
          dimnames = list(NULL, c("no", "yes"))
        )
      }
    ),
    "at least three observed outcome classes"
  )

  non_finite <- data.frame(x = seq_len(4), y = c(1, 2, 3, Inf))
  expect_error(
    explain_model(
      list(),
      non_finite,
      "y",
      task = "regression",
      predict_function = function(newdata) rep(1, nrow(newdata))
    ),
    "finite numeric target"
  )
})

test_that("numeric two-level outcomes are binary", {
  fixture <- make_binary_fixture()
  explainer <- explain_model(fixture$model, fixture$test, "y")

  expect_equal(explainer$task, "binary")
  expect_equal(explainer$positive, "1")
  prediction <- predict(explainer, explainer$data)
  expect_true(all(prediction >= 0 & prediction <= 1))
})

test_that("binary probability matrices identify the configured positive class", {
  reversed <- matrix(
    c(0.8, 0.2, 0.3, 0.7),
    nrow = 2L,
    byrow = TRUE,
    dimnames = list(NULL, c("yes", "no"))
  )
  expect_equal(
    AutoXplainR:::normalize_predictions(reversed, "binary", positive = "yes"),
    c(0.8, 0.3)
  )

  h2o_like <- data.frame(
    predict = factor(c("0", "1")),
    p0 = c(0.75, 0.2),
    p1 = c(0.25, 0.8)
  )
  expect_equal(
    AutoXplainR:::normalize_predictions(h2o_like, "binary", positive = "1"),
    c(0.25, 0.8)
  )

  expect_equal(
    AutoXplainR:::normalize_predictions(c(0.2, 0.9), "binary", positive = "yes"),
    c(0.2, 0.9)
  )
  expect_equal(
    AutoXplainR:::normalize_predictions(
      matrix(c(0.2, 0.9), ncol = 1L),
      "binary",
      positive = "yes"
    ),
    c(0.2, 0.9)
  )

  expect_error(
    AutoXplainR:::normalize_predictions(
      matrix(c(0.8, 0.2, 0.3, 0.7), nrow = 2L, byrow = TRUE),
      "binary",
      positive = "yes"
    ),
    "must identify the configured positive class `yes`"
  )
  expect_error(
    AutoXplainR:::normalize_predictions(
      matrix(
        c(0.8, 0.2, 0.3, 0.7),
        nrow = 2L,
        byrow = TRUE,
        dimnames = list(NULL, c("negative", "other"))
      ),
      "binary",
      positive = "yes"
    ),
    "must identify the configured positive class `yes`"
  )
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

test_that("multiclass adapters preserve numeric probabilities beside hard labels", {
  class_levels <- c("class one", "class-two", "third")
  raw <- data.frame(
    predict = factor(c("class one", "class-two"), levels = class_levels),
    class.one = c(0.7, 0.1),
    `pclass-two` = c(0.2, 0.8),
    third = c(0.1, 0.1),
    check.names = FALSE
  )

  normalized <- AutoXplainR:::normalize_predictions(
    raw,
    task = "multiclass",
    class_levels = class_levels,
    n = 2L
  )

  expect_true(is.numeric(normalized))
  expect_identical(colnames(normalized), class_levels)
  expect_equal(unname(normalized[1L, ]), c(0.7, 0.2, 0.1))
  expect_invisible(AutoXplainR:::validate_predictions(
    normalized,
    n = 2L,
    task = "multiclass",
    class_levels = class_levels
  ))
})

test_that("one-row multiclass vectors require and align complete class names", {
  class_levels <- c("first", "second", "third")
  aligned <- AutoXplainR:::normalize_predictions(
    c(third = 0.1, first = 0.7, second = 0.2),
    task = "multiclass",
    class_levels = class_levels,
    n = 1L
  )

  expect_identical(colnames(aligned), class_levels)
  expect_equal(unname(aligned[1L, ]), c(0.7, 0.2, 0.1))
  expect_error(
    AutoXplainR:::normalize_predictions(
      c(0.7, 0.2, 0.1),
      task = "multiclass",
      class_levels = class_levels,
      n = 1L
    ),
    "must name every outcome class"
  )
  expect_error(
    AutoXplainR:::normalize_predictions(
      c(first = 0.7, second = 0.2, wrong = 0.1),
      task = "multiclass",
      class_levels = class_levels,
      n = 1L
    ),
    "must name every outcome class"
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
