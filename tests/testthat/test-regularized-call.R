test_that("regularized native calls retain settings without embedding training data", {
  skip_if_package_unavailable("glmnet")
  set.seed(206)
  data <- as.data.frame(matrix(rnorm(2400L * 16L), 2400L, 16L))
  data$y <- 2 * data$V1 - data$V2 + rnorm(2400L, sd = 0.2)
  fit <- AutoXplainR:::fit_regularized_learner(
    data, "y", "regression", list(alpha = 0.5, path_fraction = 0.55), 17L
  )
  # This limit catches an embedded matrix, response vector or function body.
  # It does not constrain the legitimate coefficient path or blueprint.
  expect_lt(length(serialize(fit$fit$call, NULL)), 2048L)
  expect_identical(fit$fit$call[[1L]], quote(glmnet::glmnet))
  expect_identical(fit$fit$call$x, quote(x))
  expect_identical(fit$fit$call$y, quote(y))
  expect_identical(fit$fit$call$family, "gaussian")
  expect_identical(fit$fit$call$alpha, 0.5)
  expect_equal(fit$fit$nobs, nrow(data))
})

test_that("compact glmnet calls reconstruct all tasks and public reload predictions", {
  skip_if_package_unavailable("glmnet")
  set.seed(297)
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
  for (task in names(outcomes)) {
    # Also cover the single-input workaround, whose excluded dummy column must
    # be present when the recorded native call is reconstructed.
    for (features in list("x", c("x", "z", "group"))) {
      frame <- data[features]
      frame$y <- outcomes[[task]]
      result <- autoxplain(
        frame[1:90, ], "y", test_data = frame[91:120, ],
        learners = "regularized", max_models = 1L, nfolds = 2L, explain = FALSE
      )
      id <- names(result$models)[vapply(result$models, function(model) {
        inherits(model, "autoxplain_fitted_model") && model$family == "regularized"
      }, logical(1L))]
      expect_length(id, 1L)
      model <- result$models[[id]]
      x <- AutoXplainR:::bake_matrix_blueprint(model$blueprint, result$training_data)
      if (!is.null(model$fit_details$dummy_column)) {
        dummy <- matrix(
          0, nrow(x), 1L, dimnames = list(NULL, model$fit_details$dummy_column)
        )
        x <- cbind(x, dummy)
      }
      y <- if (task == "binary") as.numeric(result$training_data$y == "event") else result$training_data$y
      rebuilt <- eval(model$fit$call, envir = list(x = x, y = y))
      expect_equal(rebuilt$lambda, model$fit$lambda, tolerance = 1e-12)
      expect_equal(
        stats::coef(rebuilt, s = model$fit_details$lambda),
        stats::coef(model$fit, s = model$fit_details$lambda), tolerance = 1e-12
      )
      expected <- predict(result, frame[91:120, ], model = id)
      restored <- unserialize(serialize(result, NULL))
      expect_identical(predict(restored, frame[91:120, ], model = id), expected)
      expect_true(all(is.finite(expected)))
    }
  }
})
