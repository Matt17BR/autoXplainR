test_that("standardization preserves geometry across extreme finite input units", {
  base <- data.frame(x = c(-2, -1, 1, 2), constant = rep(7, 4))
  expected <- as.numeric(scale(base$x))
  for (units in c(1e160, 1e-160, 1e-200)) {
    training <- base
    training$x <- training$x * units
    blueprint <- AutoXplainR:::fit_matrix_blueprint(training, center = TRUE, scale = TRUE)
    baked <- AutoXplainR:::bake_matrix_blueprint(blueprint, training)
    expect_equal(unname(baked[, "x"]), expected, tolerance = 1e-12)
    expect_identical(blueprint$zero_variance_columns, "constant")
    expect_equal(unname(baked[, "constant"]), rep(0, 4))
    expect_equal(blueprint$scale[["x"]] / units, sd(base$x), tolerance = 1e-12)
  }
})

test_that("standardization catches unrepresentable new-data values", {
  data <- data.frame(x = c(-2, -1, 1, 2) * 1e-200)
  blueprint <- AutoXplainR:::fit_matrix_blueprint(data, center = TRUE, scale = TRUE)
  expect_error(
    AutoXplainR:::bake_matrix_blueprint(blueprint, data.frame(x = 1e200)),
    "Standardized predictor values exceed the finite numeric range"
  )
})

test_that("standardization avoids overflowing a representable centered result", {
  training <- data.frame(x = c(-1, -1, -1, 1) * 1e308)
  blueprint <- AutoXplainR:::fit_matrix_blueprint(training, center = TRUE, scale = TRUE)
  actual <- AutoXplainR:::bake_matrix_blueprint(blueprint, data.frame(x = 1.7e308))
  expect_equal(as.numeric(actual), 2.2, tolerance = 1e-12)
})

test_that("generated input names keep distinct original features", {
  data <- data.frame(x = factor(c("a", "b", "a", "b")), xb = c(2, 4, 6, 8))
  blueprint <- AutoXplainR:::fit_matrix_blueprint(data)
  baked <- AutoXplainR:::bake_matrix_blueprint(blueprint, data)
  expect_identical(colnames(baked), c("xb", "xb.1"))
  expect_equal(unname(baked), cbind(c(0, 1, 0, 1), c(2, 4, 6, 8)))
  expect_identical(unname(blueprint$column_predictors), c("x", "xb"))
  expect_equal(AutoXplainR:::bake_matrix_blueprint(
    unserialize(serialize(blueprint, NULL)), data[4:1, ]
  ), baked[4:1, , drop = FALSE])

  # Previously saved dense blueprints did not rename colliding columns.
  old <- blueprint
  old$unique_column_names <- NULL
  old$columns <- c("xb", "xb")
  expect_identical(colnames(AutoXplainR:::bake_matrix_blueprint(old, data)), old$columns)
})

test_that("sparse encoding agrees with dense values and training schema", {
  skip_if_package_unavailable("Matrix")
  data <- data.frame(
    check.names = FALSE,
    x = factor(c("a", "b", "c", "a", "b", "c")),
    xb = c(0, -3, 2, 0, 5, -1),
    "odd: name" = seq_len(6),
    "quoted`factor" = factor(c("B", "A", "B", "A", "B", "A")),
    rank = ordered(c("low", "high", "middle", "high", "low", "middle")),
    enabled = c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE)
  )
  for (encoding in c("treatment", "one_hot")) {
    dense <- AutoXplainR:::fit_matrix_blueprint(data, categorical_encoding = encoding)
    sparse <- AutoXplainR:::fit_matrix_blueprint(
      data, categorical_encoding = encoding, output = "sparse"
    )
    expect_s4_class(AutoXplainR:::bake_matrix_blueprint(sparse, data), "dgCMatrix")
    expect_equal(
      as.matrix(AutoXplainR:::bake_matrix_blueprint(sparse, data)),
      AutoXplainR:::bake_matrix_blueprint(dense, data), tolerance = 1e-12
    )
    expect_identical(sparse$column_predictors, dense$column_predictors)
    restored <- unserialize(serialize(sparse, NULL))
    newdata <- data[c(5L, 2L), ]
    newdata$x <- as.character(newdata$x)
    expect_equal(
      as.matrix(AutoXplainR:::bake_matrix_blueprint(restored, newdata)),
      AutoXplainR:::bake_matrix_blueprint(dense, newdata), tolerance = 1e-12
    )
    newdata$x[[1L]] <- "unseen"
    expect_error(AutoXplainR:::bake_matrix_blueprint(restored, newdata), "unseen levels")
  }
})

test_that("high-cardinality sparse encoding stores observations, not a dense grid", {
  skip_if_package_unavailable("Matrix")
  data <- data.frame(group = factor(rep(seq_len(240L), 5L)), x = seq_len(1200L))
  blueprint <- AutoXplainR:::fit_matrix_blueprint(data, output = "sparse")
  encoded <- AutoXplainR:::bake_matrix_blueprint(blueprint, data)
  dense <- AutoXplainR:::bake_matrix_blueprint(AutoXplainR:::fit_matrix_blueprint(data), data)
  expect_equal(as.matrix(encoded), dense)
  expect_lt(as.numeric(object.size(encoded)), as.numeric(object.size(dense)) / 10)
  expect_equal(nrow(encoded), nrow(data))
})

test_that("glmnet sparse adapter agrees with an independent dense native fit", {
  skip_if_package_unavailable("glmnet")
  data <- data.frame(
    x = seq(-2, 2, length.out = 120L),
    group = factor(rep(c("a", "b", "c"), 40L))
  )
  data$y <- 2 * data$x + rep(c(-1, 0.2, 1), 40L) + sin(seq_len(120L)) / 10
  parameters <- list(alpha = 0.5, path_fraction = 0.55)
  fitted <- AutoXplainR:::fit_regularized_learner(data, "y", "regression", parameters, 41L)
  native_x <- model.matrix(~ x + group, data)[, -1L, drop = FALSE]
  native <- glmnet::glmnet(
    native_x, data$y, alpha = 0.5, family = "gaussian",
    nlambda = 80L, lambda.min.ratio = 0.001, standardize = TRUE, intercept = TRUE
  )
  index <- round(1 + 0.55 * (length(native$lambda) - 1L))
  native_prediction <- as.numeric(predict(native, native_x, s = native$lambda[[index]]))
  expect_identical(fitted$blueprint$output, "sparse")
  expect_equal(fitted$fit_details$lambda, native$lambda[[index]], tolerance = 1e-10)
  expect_equal(predict(fitted, data), native_prediction, tolerance = 1e-8)
  expect_equal(
    predict(unserialize(serialize(fitted, NULL)), data), native_prediction,
    tolerance = 1e-8
  )
})

test_that("MARS remains usable when a factor dummy and numeric input share a name", {
  skip_if_package_unavailable("earth")
  set.seed(12)
  data <- data.frame(x = factor(rep(c("a", "b"), 60L)), xb = rnorm(120L))
  data$y <- as.numeric(data$x) + data$xb + rnorm(120L, sd = 0.1)
  result <- autoxplain(
    data, "y", learners = c("linear", "mars"),
    max_models = 2L, nfolds = 3L, explain = FALSE
  )
  candidate <- result$tuning$candidates[result$tuning$candidates$family == "mars", ]
  expect_identical(candidate$status, "ok")
  mars <- names(result$models)[vapply(result$models, function(x) {
    inherits(x, "autoxplain_fitted_model") && identical(x$family, "mars")
  }, logical(1))]
  expect_length(mars, 1L)
  expect_true(all(is.finite(predict(result, data, model = mars))))
})

test_that("automatic GAM capacity policy is exact and precedes native fitting", {
  skip_if_package_unavailable("mgcv")
  data <- as.data.frame(matrix(seq_len(42L * 12L), 42L, 12L))
  data$y <- sin(seq_len(42L))
  parameters <- list(k = 5L, gamma = 1, select = TRUE)
  native_called <- FALSE
  local_mocked_bindings(gam = function(...) {
    native_called <<- TRUE
    structure(list(), class = "gam")
  }, .package = "mgcv")
  expect_error(AutoXplainR:::fit_additive_learner(
    data[1:12, ], "y", "regression", parameters, 1L
  ), "12 terms, 12 rows", fixed = TRUE)
  expect_false(native_called)
  expect_error(AutoXplainR:::fit_additive_learner(
    data[1:11, ], "y", "regression", parameters, 1L
  ), "conservative capacity policy")
  expect_false(native_called)
  AutoXplainR:::fit_additive_learner(data[1:13, ], "y", "regression", parameters, 1L)
  expect_true(native_called)
  native_called <- FALSE
  AutoXplainR:::fit_additive_learner(data[1:40, ], "y", "regression", parameters, 1L)
  expect_true(native_called)
})

test_that("a too-wide GAM fails within its fold while useful families survive", {
  skip_if_package_unavailable("mgcv")
  skip_if_package_unavailable("glmnet")
  set.seed(513)
  data <- as.data.frame(matrix(rnorm(50L * 40L), 50L, 40L))
  data$y <- data$V1 + 0.2 * data$V2 + rnorm(50L, sd = 0.1)
  result <- autoxplain(
    data, "y", learners = c("regularized", "additive"),
    max_models = 2L, nfolds = 2L, explain = FALSE
  )
  expect_identical(result$tuning$families_resampling_failed, "additive")
  folds <- result$tuning$fold_scores
  configuration <- result$tuning$candidates$configuration_id[
    result$tuning$candidates$family == "additive"
  ]
  additive <- folds[folds$configuration_id %in% configuration, ]
  expect_equal(nrow(additive), 2L)
  expect_true(all(grepl("40 terms, 20 rows", additive$error, fixed = TRUE)))
  expect_true(all(additive$optimization_status == "unknown"))
  expect_true(all(is.finite(predict(result, data[1:4, ]))))
  expect_true("regularized" %in% result$leaderboard$family)
})
