test_that("benchmark report uses normalized costs without changing original timing records", {
  result <- autoxplain(mtcars, "mpg", explain = FALSE)
  original <- result
  clock <- 0
  local_mocked_bindings(
    benchmark_clock = function() {
      clock <<- clock + .01
      clock
    },
    benchmark_clock_step = function(...) .0001, .package = "AutoXplainR"
  )
  bench <- benchmark_predictions(result, batch_size = 3L, n_repeats = 3L, min_duration = .005)
  rendered <- AutoXplainR:::prepare_report_benchmark(result, bench)
  expect_identical(result, original)
  expect_equal(rendered$leaderboard$repeated_prediction_ms_per_row, rep(10 / 3, nrow(result$leaderboard)))
  expect_identical(rendered$leaderboard$prediction_time_ms, result$leaderboard$prediction_time_ms)
  html <- AutoXplainR:::render_report_benchmark(rendered)
  expect_false(grepl("sampling_rows|model_fingerprints", html))
  expect_true(grepl("3 common evaluation rows", html, fixed = TRUE))
  expect_identical(AutoXplainR:::explorer_models(rendered)$resources[[1L]], "repeated_prediction_ms_per_row")
  result$test_data$mpg[[1L]] <- -100
  expect_error(AutoXplainR:::prepare_report_benchmark(result, bench), "Stored evaluation evidence no longer matches")
})
