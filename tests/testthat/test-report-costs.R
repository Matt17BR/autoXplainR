test_that("comparison defaults use finite score/cost pairs and expose size scope", {
  result <- structure(list(
    task = "regression",
    evaluation = list(primary_metric = "rmse", metric_definitions = c(rmse = "Root mean squared error")),
    leaderboard = data.frame(
      model_id = c("a", "b", "c"), model = c("A", "B", "C"),
      rmse = c(1, NA, 3), training_time_ms = c(100, 200, NA),
      prediction_time_ms = c(.125, NA, 0), model_size_kb = c(10, 20, 30)
    )
  ), class = "autoxplain_result")
  local_mocked_bindings(explorer_label = function(result, id) id, .package = "AutoXplainR")
  models <- AutoXplainR:::explorer_models(result)
  expect_identical(models$resources[[1L]], "prediction_time_ms")
  result$leaderboard$prediction_time_ms <- NA_real_
  models <- AutoXplainR:::explorer_models(result)
  expect_identical(models$resources[[1L]], "model_size_kb")
  html <- AutoXplainR:::explorer_tradeoffs(result, models)
  expect_match(html, 'value="model_size_kb" selected', fixed = TRUE)
  expect_match(html, "R object size (KiB)", fixed = TRUE)
  expect_match(html, "Native allocations are excluded; this is not deployment memory.", fixed = TRUE)
})

test_that("comparison table formats durations but sorts and preserves raw measurements", {
  result <- autoxplain(mtcars, "mpg", model_set = "quick", explain = FALSE)
  models_before <- serialize(result$models, NULL)
  result$leaderboard$training_time_ms <- c(124800, .125)
  result$leaderboard$prediction_time_ms <- c(0, 1250)
  # Old saved results need no new fitted-model fields to render the new table.
  restored <- unserialize(serialize(result, NULL))
  models <- AutoXplainR:::explorer_models(restored)
  html <- AutoXplainR:::explorer_model_table(restored, models)
  expect_identical(models$resources[[1L]], "training_time_ms")
  expect_match(html, 'data-value-training_time_ms="124800"', fixed = TRUE)
  expect_match(html, 'title="124800 ms">2 min 5 s</span>', fixed = TRUE)
  expect_match(html, 'title="0.125 ms">0.125 ms</span>', fixed = TRUE)
  expect_match(html, 'title="0 ms">~0 ms</span>', fixed = TRUE)
  expect_identical(serialize(restored$models, NULL), models_before)
  expect_identical(restored$leaderboard, result$leaderboard)
})

test_that("search elapsed labels are readable with exact original seconds", {
  html <- AutoXplainR:::selection_screening_details(list(
    resources = list(
      search_elapsed_seconds = 124.8, search_time_limit = 3600,
      validation_fits = 15L, limitation = "Search scope remains unchanged."
    )
  ))
  expect_match(html, "2 min 5 s (recorded:", fixed = TRUE)
  expect_match(html, sprintf("%.17g s)", 124.8), fixed = TRUE)
  expect_match(html, "1 h (recorded: 3600 s)", fixed = TRUE)
  expect_match(html, "Search scope remains unchanged.", fixed = TRUE)
})
