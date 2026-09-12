test_that("MARS native calls record settings without embedding training data", {
  skip_if_package_unavailable("earth")
  set.seed(607)
  data <- data.frame(x = rnorm(180L), z = rnorm(180L))
  data$y <- abs(data$x) - data$z + rnorm(180L, sd = 0.2)
  fit <- AutoXplainR:::fit_mars_learner(
    data, "y", "regression", list(degree = 1L, nprune = 5L), 51L
  )
  expect_lt(length(serialize(fit$fit$call, NULL)), 2048L)
  expect_identical(fit$fit$call[[1L]], quote(earth::earth))
  expect_identical(fit$fit$call$x, quote(x))
  expect_identical(fit$fit$call$y, quote(y))
  expect_identical(fit$fit$call$degree, 1L)
  expect_equal(fit$fit$call$nprune, fit$fit_details$effective_parameters$nprune)
})

test_that("compact MARS calls reconstruct both supported tasks and reload predictions", {
  skip_if_package_unavailable("earth")
  set.seed(613)
  data <- data.frame(
    x = rnorm(180L), z = rnorm(180L), group = factor(rep(c("a", "b", "c"), 60L))
  )
  outcomes <- list(
    regression = abs(data$x) - data$z + rnorm(180L, sd = 0.2),
    binary = factor(
      ifelse(data$x + rnorm(180L) > 0, "event", "none"), levels = c("none", "event")
    )
  )
  for (task in names(outcomes)) {
    data$y <- outcomes[[task]]
    result <- autoxplain(
      data[1:140, ], "y", test_data = data[141:180, ], learners = "mars",
      max_models = 1L, nfolds = 2L, explain = FALSE,
      tuning_control = tuning_control(
        grids = list(mars = list(degree = 1L, nprune = 5L)), retain_oof = FALSE
      )
    )
    id <- names(result$models)[vapply(result$models, function(model) {
      inherits(model, "autoxplain_fitted_model") && model$family == "mars"
    }, logical(1L))]
    expect_length(id, 1L)
    model <- result$models[[id]]
    x <- AutoXplainR:::bake_matrix_blueprint(model$blueprint, result$training_data)
    y <- if (task == "binary") as.numeric(result$training_data$y == "event") else result$training_data$y
    rebuilt <- eval(model$fit$call, envir = list(x = x, y = y))
    expect_lt(length(serialize(model$fit$call, NULL)), 2048L)
    expect_identical(stats::coef(rebuilt), stats::coef(model$fit))
    expect_identical(rebuilt$selected.terms, model$fit$selected.terms)
    expect_identical(rebuilt$fitted.values, model$fit$fitted.values)
    expect_identical(stats::predict(rebuilt, x), stats::predict(model$fit, x))
    if (task == "binary") {
      expect_identical(model$fit$call$glm, quote(list(family = stats::binomial())))
    }
    expected <- predict(result, data[141:180, ], model = id)
    restored <- unserialize(serialize(result, NULL))
    expect_identical(predict(restored, data[141:180, ], model = id), expected)
    expect_true(all(is.finite(expected)))
  }
})
