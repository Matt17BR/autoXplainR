test_that("small-table forest anchors preserve the published choices", {
  grid <- AutoXplainR:::forest_learner_grid(1000L, 12L, "regression", 1L)
  expect_identical(vapply(grid[1:4], `[[`, integer(1), "mtry"), c(3L, 12L, 4L, 3L))
  expect_identical(vapply(grid[1:4], `[[`, integer(1), "min.node.size"), c(10L, 5L, 20L, 5L))
  expect_true(all(vapply(grid, `[[`, integer(1), "num.trees") == 500L))
})

test_that("large-work forests retain useful subset choices without full-width automatic fits", {
  policy <- AutoXplainR:::forest_search_policy(50000L, 90L, "regression")
  expect_identical(policy$mtry_values, c(9L, 16L, 30L))
  expect_identical(policy$node_values, c(5L, 20L, 50L))
  expect_identical(policy$mtry_upper, 30L)
  expect_true(policy$large_work)
  grids <- list(
    AutoXplainR:::forest_learner_grid(50000L, 90L, "regression", 1L),
    AutoXplainR:::adaptive_parameter_grids("forest", 50000L, 90L, "regression", 1L, 73L, 80L)$forest
  )
  for (index in seq_along(grids)) {
    grid <- grids[[index]]
    mtry <- vapply(grid, `[[`, integer(1), "mtry")
    nodes <- vapply(grid, `[[`, integer(1), "min.node.size")
    expect_true(all(mtry >= 1L & mtry <= 30L))
    expect_true(all(nodes >= 5L))
    expect_true(all(vapply(grid, `[[`, integer(1), "num.trees") == c(500L, 256L)[[index]]))
    expect_setequal(vapply(grid[1:4], `[[`, character(1), "splitrule"), c("default", "extratrees"))
    expect_identical(mtry[1:4], c(9L, 30L, 16L, 9L))
    expect_identical(nodes[1:4], c(20L, 20L, 50L, 5L))
  }
  expect_match(policy$scope, "Explicit grids")
  expect_match(policy$scope, "not a leaf-size guarantee")
})

test_that("forest size policy grows node sizes gradually and respects narrow tables", {
  policy <- AutoXplainR:::forest_search_policy
  before <- policy(9999L, 100L, "regression")
  after <- policy(10000L, 100L, "regression")
  expect_false(before$large_work)
  expect_identical(before$mtry_upper, 100L)
  expect_true(after$large_work)
  expect_identical(after$mtry_upper, 34L)
  expect_identical(policy(200000L, 90L, "regression")$node_values, c(10L, 40L, 100L))
  expect_identical(policy(200000L, 90L, "binary")$node_values, c(2L, 10L, 20L))
  expect_identical(policy(200000L, 90L, "multiclass")$node_values, c(2L, 10L, 20L))
  for (p in c(1L, 2L, 3L, 4L)) {
    narrow <- policy(1000000L, p, "regression")
    expect_true(all(narrow$mtry_values >= 1L & narrow$mtry_values <= p))
    expect_lte(narrow$mtry_upper, p)
  }
})

test_that("explicit forest grids retain expensive and small-node choices", {
  parameters <- list(
    num.trees = 700L, mtry = 90L, min.node.size = 1L,
    sample.fraction = 1, splitrule = "default"
  )
  normalized <- AutoXplainR:::normalize_family_grid(list(parameters), "forest")
  expect_identical(normalized[[1L]], parameters)
  skip_if_package_unavailable("ranger")
  plan <- AutoXplainR:::local_tuning_plan(
    max_models = 1L, n = 100000L, p = 90L, task = "regression", n_classes = 1L,
    learners = "forest", seed = 716L, custom_grids = list(forest = normalized)
  )
  expect_identical(plan$parameters[[1L]], parameters)
  expect_identical(attr(plan, "search_space")$families$origin, "user_grid")
})

test_that("forest screening adjusts only declared full-population node growth", {
  parameters <- list(
    num.trees = 500L, mtry = 30L, min.node.size = 60L,
    sample.fraction = .8, splitrule = "default"
  )
  reduced <- AutoXplainR:::adaptive_screen_parameters(
    parameters, "forest", training_rows = 20000L, planned_rows = 400000L, planned_predictors = 90L
  )
  expect_identical(reduced$min.node.size, 21L)
  expect_identical(reduced$num.trees, 128L)
  expect_identical(parameters$min.node.size, 60L)
  expect_identical(
    reduced[c("mtry", "sample.fraction", "splitrule")], parameters[c("mtry", "sample.fraction", "splitrule")]
  )
  narrow <- AutoXplainR:::adaptive_screen_parameters(
    parameters, "forest", training_rows = 20000L, planned_rows = 100000L, planned_predictors = 1L
  )
  expect_identical(narrow$min.node.size, 60L)
  unspecified <- AutoXplainR:::adaptive_screen_parameters(parameters, "forest")
  expect_identical(unspecified$min.node.size, 60L)
  no_predictor_count <- AutoXplainR:::adaptive_screen_parameters(
    parameters, "forest", training_rows = 20000L, planned_rows = 400000L
  )
  expect_identical(no_predictor_count$min.node.size, 60L)
  expect_error(AutoXplainR:::adaptive_screen_parameters(
    parameters, "forest", training_rows = 60000L, planned_rows = 40000L, planned_predictors = 90L
  ), "more training rows")
})

test_that("disposable forest fits omit only unused OOB calculations", {
  skip_if_package_unavailable("ranger")
  set.seed(924)
  data <- data.frame(x = runif(180), z = rnorm(180), segment = factor(rep(letters[1:3], 60)))
  for (task in c("regression", "binary", "multiclass")) {
    data$y <- switch(task,
      regression = data$x^2 + data$z,
      binary = factor(rep(c("no", "yes"), 90)),
      multiclass = factor(rep(c("a", "b", "c"), 60))
    )
    parameters <- list(
      num.trees = 40L, mtry = 2L, min.node.size = 5L,
      sample.fraction = .8, splitrule = "default"
    )
    final <- AutoXplainR:::fit_forest_learner(data, "y", task, parameters, 392L)
    reference <- ranger::ranger(
      x = data[c("x", "z", "segment")], y = data$y, num.trees = 40L,
      mtry = 2L, min.node.size = 5L, sample.fraction = .8,
      probability = task != "regression", respect.unordered.factors = "order",
      num.threads = 1L, seed = 392L, oob.error = TRUE
    )
    expect_identical(final$fit_details$oob_computed, TRUE)
    expect_identical(final$fit$prediction.error, reference$prediction.error)
    reference_prediction <- predict(reference, data = data)$predictions
    if (task == "binary") reference_prediction <- reference_prediction[, levels(data$y)[[2L]]]
    for (scope in c("resampling_fold", "screening")) {
      temporary_parameters <- parameters
      attr(temporary_parameters, "autoxplain_fit_scope") <- scope
      temporary <- AutoXplainR:::fit_forest_learner(data, "y", task, temporary_parameters, 392L)
      expect_identical(temporary$fit_details$oob_computed, FALSE)
      expect_length(temporary$fit$predictions, 0L)
      expect_equal(predict(temporary, data), predict(final, data), tolerance = 0)
      expect_equal(predict(temporary, data), reference_prediction, tolerance = 0, ignore_attr = TRUE)
      expect_identical(temporary$fit$forest, final$fit$forest)
    }
  }
})
