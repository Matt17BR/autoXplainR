test_that("forest native calls keep settings without embedding the training frame", {
  skip_if_package_unavailable("ranger")
  set.seed(491)
  data <- data.frame(x = rnorm(240L), z = rnorm(240L))
  data$y <- data$x - data$z + rnorm(240L, sd = 0.2)
  parameters <- list(num.trees = 20L, mtry = 2L, min.node.size = 5L,
                     sample.fraction = 0.8, splitrule = "extratrees")
  fit <- AutoXplainR:::fit_forest_learner(data, "y", "regression", parameters, 51L)
  expect_lt(length(serialize(fit$fit$call, NULL)), 2048L)
  expect_identical(fit$fit$call[[1L]], quote(ranger::ranger))
  expect_identical(fit$fit$call$x, quote(x))
  expect_identical(fit$fit$call$y, quote(y))
  expect_identical(fit$fit$call$splitrule, "extratrees")
  expect_identical(fit$fit$call$num.random.splits, 5L)
  expect_equal(fit$fit$num.samples, nrow(data))
})

test_that("compact forest calls reconstruct all tasks and public reload predictions", {
  skip_if_package_unavailable("ranger")
  set.seed(497)
  data <- data.frame(
    x = rnorm(120L), z = rnorm(120L), group = factor(rep(c("a", "b", "c"), 40L))
  )
  outcomes <- list(
    regression = data$x - data$z + rnorm(120L, sd = 0.2),
    binary = factor(
      ifelse(data$x + rnorm(120L) > 0, "event", "none"), levels = c("none", "event")
    ),
    multiclass = factor(rep(c("A", "B", "C"), 40L))
  )
  parameters <- list(num.trees = 20L, mtry = 2L, min.node.size = 5L,
                     sample.fraction = 0.8, splitrule = "default")
  for (task in names(outcomes)) {
    data$y <- outcomes[[task]]
    result <- autoxplain(
      data[1:90, ], "y", test_data = data[91:120, ], learners = "forest",
      max_models = 1L, nfolds = 2L, explain = FALSE,
      tuning_control = tuning_control(grids = list(forest = parameters), retain_oof = FALSE)
    )
    id <- names(result$models)[vapply(result$models, function(model) {
      inherits(model, "autoxplain_fitted_model") && model$family == "forest"
    }, logical(1L))]
    expect_length(id, 1L)
    model <- result$models[[id]]
    rebuilt <- eval(model$fit$call, envir = list(
      x = result$training_data[model$features], y = result$training_data$y
    ))
    expect_identical(rebuilt$forest, model$fit$forest)
    expect_identical(rebuilt$predictions, model$fit$predictions)
    expect_identical(rebuilt$prediction.error, model$fit$prediction.error)
    expected <- predict(result, data[91:120, ], model = id)
    restored <- unserialize(serialize(result, NULL))
    expect_identical(predict(restored, data[91:120, ], model = id), expected)
    expect_true(all(is.finite(expected)))
  }
})
