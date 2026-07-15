test_that("permutation importance recovers known signal", {
  fixture <- make_regression_fixture()
  explainer <- explain_model(
    fixture$model, fixture$test, "y",
    metadata = list(evaluation_role = "test")
  )
  importance <- calculate_permutation_importance(
    explainer, n_repeats = 25, seed = 11
  )

  expect_s3_class(importance, "autoxplain_importance")
  expect_equal(importance$feature[[1]], "x1")
  expect_gt(importance$importance[importance$feature == "x2"], 0)
  expect_equal(dim(attr(importance, "repeat_scores")), c(3, 25))
  expect_match(attr(importance, "interval_type"), "Monte Carlo")
  expect_output(print(importance), "Monte Carlo")
})

test_that("feature groups are permuted jointly", {
  fixture <- make_regression_fixture()
  explainer <- explain_model(fixture$model, fixture$test, "y")
  importance <- calculate_permutation_importance(
    explainer,
    feature_groups = list(signal = c("x1", "x2"), null = "noise"),
    n_repeats = 5,
    seed = 12
  )
  expect_equal(sort(importance$feature), c("null", "signal"))
  expect_gt(importance$importance[importance$feature == "signal"],
            importance$importance[importance$feature == "null"])
})

test_that("blocked permutations and random state are reproducible", {
  fixture <- make_regression_fixture()
  explainer <- explain_model(fixture$model, fixture$test, "y")
  set.seed(999)
  before <- .Random.seed
  first <- calculate_permutation_importance(
    explainer, features = "x1", within = "x2", n_repeats = 4, seed = 10
  )
  expect_equal(.Random.seed, before)
  second <- calculate_permutation_importance(
    explainer, features = "x1", within = "x2", n_repeats = 4, seed = 10
  )
  expect_equal(first, second)
})

test_that("binary metrics are finite and correctly directed", {
  fixture <- make_binary_fixture()
  explainer <- explain_model(fixture$model, fixture$test, "y")
  for (metric in c("logloss", "accuracy", "auc")) {
    importance <- calculate_permutation_importance(
      explainer, metric = metric, n_repeats = 4, seed = 15
    )
    expect_true(all(is.finite(importance$importance)))
    expect_equal(attr(importance, "metric"), metric)
  }
})

test_that("importance validates groups, metrics, and counts", {
  fixture <- make_regression_fixture()
  explainer <- explain_model(fixture$model, fixture$test, "y")
  expect_error(calculate_permutation_importance(explainer, n_repeats = 0), "whole number")
  expect_error(calculate_permutation_importance(explainer, metric = "auc"), "Regression")
  expect_error(calculate_permutation_importance(explainer, features = "missing"), "Unknown")
  expect_error(
    calculate_permutation_importance(explainer, feature_groups = list(group = "missing")),
    "known feature"
  )
})
