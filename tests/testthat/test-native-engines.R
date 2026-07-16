engine_test_data <- function() {
  set.seed(2026)
  regression <- data.frame(
    check.names = FALSE,
    "curved value" = runif(90, -2, 2),
    segment = factor(rep(c("a", "b", "c"), 30)),
    noise = rnorm(90)
  )
  regression$y <- with(
    regression,
    2 * `curved value`^2 + as.numeric(segment) + 0.2 * noise + rnorm(90, sd = 0.15)
  )
  binary <- regression
  probability <- stats::plogis(
    0.5 * binary[["curved value"]] + 0.3 * binary$noise - 0.2
  )
  binary$y <- factor(ifelse(stats::runif(nrow(binary)) < probability, "yes", "no"))
  multiclass <- iris
  list(regression = regression, binary = binary, multiclass = multiclass)
}

fit_engine_configuration <- function(family, data, target, task) {
  plan <- AutoXplainR:::local_tuning_plan(
    max_models = 1L,
    n = nrow(data),
    p = ncol(data) - 1L,
    task = task,
    n_classes = if (task == "regression") 1L else nlevels(data[[target]]),
    learners = family,
    seed = 41L
  )
  AutoXplainR:::fit_tuning_configuration(plan, data, target, task)
}

expect_engine_prediction_contract <- function(family, package) {
  skip_if_not_installed(package)
  data <- engine_test_data()

  regression <- fit_engine_configuration(family, data$regression, "y", "regression")
  regression_prediction <- predict(regression, data$regression[1:7, ])
  expect_s3_class(regression, "autoxplain_fitted_model")
  expect_length(regression_prediction, 7L)
  expect_true(all(is.finite(regression_prediction)))

  path <- tempfile(fileext = ".rds")
  withr::defer(unlink(path))
  saveRDS(regression, path)
  restored <- readRDS(path)
  expect_s3_class(restored, "autoxplain_fitted_model")
  expect_equal(
    predict(restored, data$regression[1:7, ]),
    regression_prediction,
    tolerance = 1e-10
  )

  binary <- fit_engine_configuration(family, data$binary, "y", "binary")
  binary_probability <- predict(binary, data$binary[1:7, ])
  expect_length(binary_probability, 7L)
  expect_true(all(is.finite(binary_probability)))
  expect_true(all(binary_probability >= 0 & binary_probability <= 1))

  if ("multiclass" %in% AutoXplainR:::learner_definition(family)$tasks) {
    multiclass <- fit_engine_configuration(family, data$multiclass, "Species", "multiclass")
    probability <- predict(multiclass, data$multiclass[1:7, ])
    expect_equal(dim(probability), c(7L, 3L))
    expect_identical(colnames(probability), levels(data$multiclass$Species))
    expect_equal(unname(rowSums(probability)), rep(1, 7L), tolerance = 1e-6)

    one_probability <- predict(multiclass, data$multiclass[1L, , drop = FALSE])
    expect_equal(dim(one_probability), c(1L, 3L))
    expect_identical(colnames(one_probability), levels(data$multiclass$Species))
    expect_equal(unname(rowSums(one_probability)), 1, tolerance = 1e-6)
  }
}

test_that("linear multiclass models keep one-row probability dimensions", {
  model <- fit_engine_configuration("linear", iris, "Species", "multiclass")
  explainer <- explain_model(model, iris, "Species", task = "multiclass")
  probability <- predict(explainer, explainer$data[1L, , drop = FALSE])

  expect_equal(dim(probability), c(1L, 3L))
  expect_identical(colnames(probability), levels(iris$Species))
  expect_equal(unname(rowSums(probability)), 1, tolerance = 1e-7)
})

test_that("glmnet adapter satisfies every task prediction contract", {
  expect_engine_prediction_contract("regularized", "glmnet")
})

test_that("glmnet supports one encoded predictor", {
  skip_if_not_installed("glmnet")
  set.seed(2027)
  regression <- data.frame(x = seq(-2, 2, length.out = 60))
  regression$y <- 1.5 * regression$x + stats::rnorm(60, sd = 0.1)
  regression_model <- fit_engine_configuration(
    "regularized", regression, "y", "regression"
  )
  expect_true(all(is.finite(predict(regression_model, regression))))
  expect_true(nzchar(regression_model$fit_details$dummy_column))

  binary <- regression["x"]
  binary$y <- factor(ifelse(binary$x + stats::rnorm(60, sd = 0.5) > 0, "yes", "no"))
  binary_model <- fit_engine_configuration("regularized", binary, "y", "binary")
  probability <- predict(binary_model, binary)
  expect_true(all(is.finite(probability)))
  expect_true(all(probability >= 0 & probability <= 1))
})

test_that("ranger adapter satisfies every task prediction contract", {
  expect_engine_prediction_contract("forest", "ranger")
})

test_that("ranger clamps mtry after fold-specific feature removal", {
  skip_if_not_installed("ranger")
  data <- engine_test_data()$regression[c("curved value", "noise", "y")]
  parameters <- AutoXplainR:::forest_learner_grid(
    nrow(data), 50L, "regression", 1L
  )[[2L]]
  expect_gt(parameters$mtry, 2L)
  model <- AutoXplainR:::fit_forest_learner(
    data, "y", "regression", parameters, seed = 19L
  )
  expect_identical(model$fit_details$requested_mtry, parameters$mtry)
  expect_identical(model$fit_details$effective_mtry, 2L)
  expect_true(all(is.finite(predict(model, data[1:4, ]))))
})

test_that("XGBoost adapter satisfies every task prediction contract", {
  expect_engine_prediction_contract("boosting", "xgboost")
})

test_that("e1071 adapter satisfies every task prediction contract", {
  expect_engine_prediction_contract("kernel", "e1071")
})

test_that("mgcv adapter satisfies regression and binary contracts", {
  expect_engine_prediction_contract("additive", "mgcv")
})

test_that("earth adapter satisfies regression and binary contracts", {
  expect_engine_prediction_contract("mars", "earth")
})

test_that("kknn adapter satisfies every task prediction contract", {
  expect_engine_prediction_contract("neighbors", "kknn")
})

test_that("kknn caps neighbor counts to small resampling training sets", {
  skip_if_not_installed("kknn")
  data <- data.frame(
    x = c(-2, -1, 1, 2),
    group = factor(c("a", "b", "a", "b")),
    y = factor(c("no", "yes", "no", "yes"))
  )
  model <- AutoXplainR:::fit_neighbors_learner(
    data,
    target = "y",
    task = "binary",
    parameters = list(k = 7L, distance = 2, kernel = "optimal"),
    seed = 19L
  )

  expect_equal(model$fit_details$requested_k, 7L)
  expect_equal(model$fit_details$effective_k, 3L)
  expect_equal(model$parameters$k, 3L)
  expect_equal(AutoXplainR:::fitted_model_complexity(model), 4 / 3)
  expect_true(all(is.finite(predict(model, data))))
})

test_that("smoother neighbor settings have a lower flexibility proxy", {
  small_k <- AutoXplainR:::neighbors_learner_complexity(
    list(k = 3L), n = 90L, p = 4L, task = "regression", n_classes = 1L
  )
  large_k <- AutoXplainR:::neighbors_learner_complexity(
    list(k = 15L), n = 90L, p = 4L, task = "regression", n_classes = 1L
  )
  expect_lt(large_k, small_k)
})

test_that("kknn rejects a one-row training set clearly", {
  skip_if_not_installed("kknn")
  data <- data.frame(x = 1, y = 2)
  expect_error(
    AutoXplainR:::fit_neighbors_learner(
      data,
      target = "y",
      task = "regression",
      parameters = list(k = 1L, distance = 2, kernel = "optimal"),
      seed = 29L
    ),
    "at least two training rows",
    fixed = TRUE
  )
})

test_that("kknn supports one-row multiclass prediction after serialization", {
  skip_if_not_installed("kknn")
  data <- engine_test_data()$multiclass
  model <- fit_engine_configuration("neighbors", data, "Species", "multiclass")
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(model, path)
  restored <- readRDS(path)

  probability <- predict(restored, data[1L, , drop = FALSE])
  expect_equal(dim(probability), c(1L, 3L))
  expect_identical(colnames(probability), levels(data$Species))
  expect_equal(unname(rowSums(probability)), 1, tolerance = 1e-6)
  expect_equal(probability, predict(model, data[1L, , drop = FALSE]))
})

test_that("recommended portfolio retains and displays every family", {
  for (package in c("glmnet", "mgcv", "ranger", "xgboost")) {
    skip_if_not_installed(package)
  }
  data <- engine_test_data()$regression
  result <- autoxplain(
    data,
    "y",
    model_set = "tuned",
    portfolio = "recommended",
    max_models = 6,
    nfolds = 2,
    tuning_rule = "best",
    seed = 55
  )

  expect_setequal(
    result$tuning$learners,
    c("linear", "regularized", "additive", "tree", "forest", "boosting")
  )
  expect_setequal(
    result$leaderboard$family,
    c("linear", "regularized", "additive", "tree", "forest", "boosting", "baseline")
  )
  expect_true(all(c("family", "backend") %in% names(result$leaderboard)))
  expect_equal(sum(!is.na(result$tuning$candidates$retained_model_id)), 6L)
  forest_id <- result$tuning$candidates$retained_model_id[
    result$tuning$candidates$family == "forest" &
      !is.na(result$tuning$candidates$retained_model_id)
  ]
  boosting_id <- result$tuning$candidates$retained_model_id[
    result$tuning$candidates$family == "boosting" &
      !is.na(result$tuning$candidates$retained_model_id)
  ]
  expect_s3_class(result$models[[forest_id]], "autoxplain_fitted_model")
  expect_s3_class(result$models[[boosting_id]], "autoxplain_fitted_model")
  expect_s3_class(tuning_results(result), "autoxplain_tuning")
})

test_that("recommended portfolio is integrated for binary and multiclass tasks", {
  for (package in c("glmnet", "mgcv", "ranger", "xgboost")) {
    skip_if_not_installed(package)
  }
  data <- engine_test_data()
  binary <- autoxplain(
    data$binary, "y", model_set = "tuned", portfolio = "recommended",
    max_models = 6, nfolds = 2, tuning_rule = "best", seed = 71
  )
  expect_setequal(
    binary$leaderboard$family,
    c("linear", "regularized", "additive", "tree", "forest", "boosting", "baseline")
  )
  binary_explainers <- as_explainers(binary)
  expect_true(all(vapply(binary_explainers, function(explainer) {
    probability <- predict(explainer, explainer$data)
    all(is.finite(probability)) && all(probability >= 0 & probability <= 1)
  }, logical(1))))

  multiclass <- autoxplain(
    data$multiclass, "Species", model_set = "tuned", portfolio = "recommended",
    max_models = 5, nfolds = 2, tuning_rule = "best", seed = 72
  )
  expect_setequal(
    multiclass$leaderboard$family,
    c("linear", "regularized", "tree", "forest", "boosting", "baseline")
  )
  multiclass_explainers <- as_explainers(multiclass)
  expect_true(all(vapply(multiclass_explainers, function(explainer) {
    probability <- predict(explainer, explainer$data)
    is.matrix(probability) &&
      identical(colnames(probability), levels(data$multiclass$Species)) &&
      max(abs(rowSums(probability) - 1)) < 1e-6
  }, logical(1))))
})

test_that("extended portfolio adds neural, kernel, neighbors, and MARS behavior", {
  for (package in c("glmnet", "mgcv", "ranger", "xgboost", "e1071", "earth", "kknn")) {
    skip_if_not_installed(package)
  }
  data <- engine_test_data()$regression
  result <- autoxplain(
    data, "y", model_set = "tuned", portfolio = "extended",
    max_models = 10, nfolds = 2, tuning_rule = "best", seed = 81
  )
  expect_setequal(
    result$leaderboard$family,
    c(
      "linear", "regularized", "additive", "tree", "forest", "boosting",
      "neural", "kernel", "neighbors", "mars", "baseline"
    )
  )
  expect_setequal(result$tuning$learner_manifest$family, result$tuning$learners)
  expect_true(all(nzchar(result$tuning$learner_manifest$strengths)))
  expect_true(all(nzchar(result$tuning$learner_manifest$cautions)))
})

test_that("large explicit search budgets are not silently capped", {
  for (package in c("glmnet", "mgcv", "ranger", "xgboost", "e1071", "earth", "kknn")) {
    skip_if_not_installed(package)
  }
  families <- AutoXplainR:::portfolio_learner_families("extended", "regression")
  plan <- AutoXplainR:::local_tuning_plan(
    max_models = 60L,
    n = 500L,
    p = 12L,
    task = "regression",
    n_classes = 1L,
    learners = families,
    seed = 144L
  )
  expect_equal(nrow(plan), 60L)
})
