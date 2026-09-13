test_that("model trade-offs expose a candidate-set-relative Pareto frontier", {
  result <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 2026)
  tradeoffs <- model_tradeoffs(result, complexity_metric = "model_size_kb")

  expect_s3_class(tradeoffs, "autoxplain_model_tradeoffs")
  expect_equal(attr(tradeoffs, "performance_metric"), "rmse")
  expect_equal(attr(tradeoffs, "complexity_metric"), "model_size_kb")
  expect_equal(attr(tradeoffs, "secondary_metric"), "model_size_kb")
  expect_equal(attr(tradeoffs, "secondary_metric_kind"), "resource proxy")
  expect_false(attr(tradeoffs, "higher_is_better"))
  expect_true(any(tradeoffs$pareto_optimal))
  expect_true(any(!tradeoffs$pareto_optimal))
  expect_match(attr(tradeoffs, "scope_note"), "supplied models")
  expect_output(print(tradeoffs), "resource proxy")

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
  expect_equal(attr(model_tradeoffs(incomplete), "complexity_metric"), "training_time_ms")
  expect_error(model_tradeoffs(incomplete, complexity_metric = "model_size_kb"), "At least two models")
})

test_that("default costs require comparable score pairs and preserve size-only results", {
  board <- data.frame(
    model_id = c("a", "b", "baseline"), model = c("A", "B", "Baseline"),
    role = c("primary", "alternative", "baseline"), rmse = c(1, 2, 3),
    training_time_ms = c(100, 200, 300), prediction_time_ms = c(.125, 0, 124800),
    model_size_kb = c(100, 30, 60)
  )
  result <- structure(list(task = "regression", evaluation = list(primary_metric = "rmse"),
                           leaderboard = board), class = "autoxplain_result")
  original <- result
  expect_identical(attr(model_tradeoffs(result), "secondary_metric"), "training_time_ms")
  expect_identical(result, original)
  result$leaderboard$repeated_prediction_ms_per_row <- c(.1, .2, NA)
  expect_identical(attr(model_tradeoffs(result), "secondary_metric"), "repeated_prediction_ms_per_row")
  result$leaderboard$repeated_prediction_ms_per_row <- c(.1, NA, NA)
  expect_identical(attr(model_tradeoffs(result), "secondary_metric"), "training_time_ms")
  result$leaderboard$training_time_ms <- c(100, NA, 300)
  expect_identical(attr(model_tradeoffs(result, include_baseline = FALSE), "secondary_metric"), "prediction_time_ms")
  result$leaderboard$rmse <- c(NA, 2, 3)
  expect_identical(attr(model_tradeoffs(result), "secondary_metric"), "prediction_time_ms")
  result$leaderboard$prediction_time_ms <- NA_real_
  expect_identical(attr(model_tradeoffs(result), "secondary_metric"), "model_size_kb")
  result$leaderboard$training_time_ms <- result$leaderboard$prediction_time_ms <- NULL
  result$leaderboard$repeated_prediction_ms_per_row <- NULL
  restored <- unserialize(serialize(result, NULL))
  expect_identical(attr(model_tradeoffs(restored), "secondary_metric"), "model_size_kb")
  restored$leaderboard$model_size_kb <- c(1, NA, NA)
  expect_error(model_tradeoffs(restored), "At least two models")
})

test_that("cost display rounding cannot change numeric tradeoffs or a log frontier", {
  costs <- c(.125, 1000, 124800, 124801)
  losses <- c(4, 3, 2, 2)
  frontier <- AutoXplainR:::pareto_nondominated(losses, costs)
  expect_identical(frontier, c(TRUE, TRUE, TRUE, FALSE))
  expect_identical(AutoXplainR:::pareto_nondominated(losses, log10(costs)), frontier)
  expect_identical(AutoXplainR:::report_resource_value(costs[3], "training_time_ms"), "2 min 5 s")
  expect_identical(AutoXplainR:::report_resource_value(costs[4], "training_time_ms"), "2 min 5 s")
  expect_false(identical(
    AutoXplainR:::report_resource_value(costs[3], "training_time_ms", TRUE),
    AutoXplainR:::report_resource_value(costs[4], "training_time_ms", TRUE)
  ))
})

test_that("the Plotly comparison keeps numeric costs behind duration tick labels", {
  skip_if_not_installed("plotly")
  board <- data.frame(
    model_id = c("a", "b", "c"), model = c("A", "B", "C"), role = rep("candidate", 3),
    rmse = c(3, 2, 1), training_time_ms = c(1, 124800, 3661000), model_size_kb = c(5, 6, 7)
  )
  result <- structure(list(task = "regression", evaluation = list(primary_metric = "rmse"),
                           leaderboard = board), class = "autoxplain_result")
  plot <- plotly::plotly_build(plot_model_comparison(result))
  expect_identical(sort(as.numeric(plot$x$data[[1L]]$x)), sort(board$training_time_ms))
  expect_true(any(grepl("min| h", plot$x$layout$xaxis$ticktext)))
  expect_true(any(grepl("recorded: 124800 ms", plot$x$data[[1L]]$customdata, fixed = TRUE)))
  expect_identical(result$leaderboard, board)
})
