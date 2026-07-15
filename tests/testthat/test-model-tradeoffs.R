test_that("model trade-offs expose a candidate-set-relative Pareto frontier", {
  result <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 2026)
  tradeoffs <- model_tradeoffs(result)

  expect_s3_class(tradeoffs, "autoxplain_model_tradeoffs")
  expect_equal(attr(tradeoffs, "performance_metric"), "rmse")
  expect_equal(attr(tradeoffs, "complexity_metric"), "model_size_kb")
  expect_false(attr(tradeoffs, "higher_is_better"))
  expect_true(any(tradeoffs$pareto_optimal))
  expect_true(any(!tradeoffs$pareto_optimal))
  expect_match(attr(tradeoffs, "scope_note"), "supplied models")
  expect_output(print(tradeoffs), "Pareto set")

  without_baseline <- model_tradeoffs(
    result,
    complexity_metric = "complexity",
    include_baseline = FALSE
  )
  expect_false(any(without_baseline$role == "baseline"))
  expect_equal(attr(without_baseline, "complexity_metric"), "complexity")
})

test_that("Pareto helpers handle higher-is-better metrics, ties, and bad inputs", {
  result <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 7)
  tradeoffs <- model_tradeoffs(result, performance_metric = "r_squared")
  expect_true(attr(tradeoffs, "higher_is_better"))

  expect_equal(
    AutoXplainR:::pareto_nondominated(c(1, 2, 1), c(2, 1, 2)),
    c(TRUE, TRUE, TRUE)
  )
  expect_error(model_tradeoffs(list()), "returned by")
  expect_error(model_tradeoffs(result, include_baseline = NA), "TRUE or FALSE")
  expect_error(model_tradeoffs(result, performance_metric = "missing"),
               "performance_metric")
  expect_error(model_tradeoffs(result, complexity_metric = "missing"),
               "complexity_metric")

  incomplete <- result
  incomplete$leaderboard$model_size_kb <- NA_real_
  incomplete$model_characteristics <- NULL
  expect_error(model_tradeoffs(incomplete), "At least two models")
})
