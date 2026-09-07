benchmark_fixture <- function() {
  autoxplain(mtcars, "mpg", model_set = "quick", explain = FALSE)
}

test_that("benchmark arithmetic uses repeated blocks and descriptive quartiles", {
  times <- c(10, 10.12)
  local_mocked_bindings(benchmark_clock = function() {
    value <- times[[1L]]
    times <<- times[-1L]
    value
  }, .package = "AutoXplainR")
  calls <- 0L
  measured <- AutoXplainR:::benchmark_measure(function() calls <<- calls + 1L, 4L, "a", "measurement", 1L, 3L)
  expect_identical(calls, 4L)
  expect_equal(measured$elapsed_ms, 120)
  expect_equal(measured$ms_per_batch, 30)
  expect_equal(measured$ms_per_row, 10)
  rows <- measured[rep(1L, 4L), ]
  rows$ms_per_batch <- c(10, 20, 30, 80)
  rows$ms_per_row <- rows$ms_per_batch / 3
  rows$elapsed_ms <- rows$ms_per_batch * 4
  output <- AutoXplainR:::benchmark_model_summary("a", rows, 4L, 3L, .001)
  expect_identical(output$status, "computed")
  expect_equal(unname(unlist(output[c(
    "min_ms_per_batch", "p25_ms_per_batch", "median_ms_per_batch",
    "p75_ms_per_batch", "max_ms_per_batch"
  )])), c(10, 17.5, 25, 42.5, 80))
  expect_equal(output$median_ms_per_row, 25 / 3)
})

test_that("failed or unresolved blocks never supply a benchmark cost", {
  clock <- 1
  local_mocked_bindings(benchmark_clock = function() clock, .package = "AutoXplainR")
  zero <- AutoXplainR:::benchmark_measure(function() NULL, 2L, "a", "measurement", 1L, 5L)
  output <- AutoXplainR:::benchmark_model_summary("a", zero[rep(1L, 3L), ], 3L, 5L, .001)
  expect_identical(output$status, "resolution_limited")
  expect_true(is.na(output$median_ms_per_batch))
  failed <- AutoXplainR:::benchmark_measure(function() {
    warning("backend warning")
    stop("backend failed")
  }, 2L, "a", "warmup", NA_integer_, 5L)
  expect_identical(failed$completed, 0L)
  expect_identical(failed$warning, "backend warning")
  expect_identical(failed$error, "backend failed")
  output <- AutoXplainR:::benchmark_model_summary("a", failed, 3L, 5L, .001)
  expect_identical(output$status, "failed")
  expect_true(is.na(output$median_ms_per_batch))
  partial <- AutoXplainR:::benchmark_model_summary("a", AutoXplainR:::empty_benchmark_measurements(), 3L, 5L, .001)
  expect_identical(partial$status, "partial")
  expect_true(grepl("0 of 3", partial$reason, fixed = TRUE))
})

test_that("common batches, recorded model orders and caller RNG are reproducible without refitting", {
  result <- benchmark_fixture()
  original_predict <- AutoXplainR:::predict.autoxplain_explainer
  batches <- list()
  clock <- 0
  local_mocked_bindings(
    benchmark_clock = function() {
      clock <<- clock + .01
      clock
    },
    benchmark_clock_step = function(...) .0001,
    predict.autoxplain_explainer = function(object, newdata, ...) {
      if (nrow(newdata) == 3L) batches[[length(batches) + 1L]] <<- newdata
      original_predict(object, newdata, ...)
    },
    fit_base_candidates = function(...) stop("A benchmark must not refit"),
    fit_tuning_configuration = function(...) stop("A benchmark must not refit"),
    .package = "AutoXplainR"
  )
  set.seed(501)
  state <- .Random.seed
  one <- benchmark_predictions(result, batch_size = 3L, n_repeats = 3L, min_duration = .005, seed = 19)
  expect_identical(.Random.seed, state)
  expect_true(length(batches) > 0L)
  expect_true(all(vapply(batches, identical, logical(1), batches[[1L]])))
  expect_equal(batches[[1L]], result$test_data[one$sampling_rows, names(batches[[1L]]), drop = FALSE])
  expect_identical(one$summary$repetitions, c(3L, 3L))
  expect_true(all(one$summary$status == "computed"))
  expect_identical(one$measurements$iterations[one$measurements$phase == "warmup"], c(2L, 2L))
  expect_true(all(one$measurements$iterations[one$measurements$phase == "measurement"] == 1L))
  two <- benchmark_predictions(result, batch_size = 3L, n_repeats = 3L, min_duration = .005, seed = 19)
  expect_identical(one$sampling_rows, two$sampling_rows)
  expect_identical(one$protocol$model_order, two$protocol$model_order)
  expect_true(grepl("evaluation predictor batch", one$protocol$scope, fixed = TRUE))
  expect_true(grepl("not confidence intervals", one$protocol$timing_interpretation, fixed = TRUE))
})

test_that("soft budget completion and iteration caps remain explicit", {
  result <- benchmark_fixture()
  clock <- 0
  local_mocked_bindings(
    benchmark_clock = function() {
      clock <<- clock + .1
      clock
    },
    benchmark_clock_step = function(...) .001,
    .package = "AutoXplainR"
  )
  none <- benchmark_predictions(result, max_seconds = .05)
  expect_true(all(none$summary$repetitions == 0L))
  expect_true(all(is.na(none$summary$median_ms_per_batch)))
  expect_true(none$protocol$budget_exceeded)
  capped <- benchmark_predictions(result,
    models = 1L, n_repeats = 2L, min_duration = 2,
    max_iterations = 3L, max_seconds = 20
  )
  expect_identical(capped$measurements$iterations[capped$measurements$phase == "calibration"], c(1L, 3L))
  expect_true(all(capped$measurements$iterations[capped$measurements$phase == "measurement"] == 3L))
})

test_that("benchmark identity survives serialization and rejects changed outcomes or model state", {
  result <- benchmark_fixture()
  clock <- 0
  local_mocked_bindings(
    benchmark_clock = function() {
      clock <<- clock + .1
      clock
    },
    benchmark_clock_step = function(...) .001,
    .package = "AutoXplainR"
  )
  output <- benchmark_predictions(result, n_repeats = 2L)
  copy <- unserialize(serialize(list(result = result, benchmark = output), NULL))
  expect_true(AutoXplainR:::validate_prediction_benchmark(copy$benchmark, copy$result))
  copy$result$test_data$mpg[[1L]] <- copy$result$test_data$mpg[[1L]] + 1
  mismatch <- "content changed|Stored evaluation evidence no longer matches"
  expect_error(AutoXplainR:::validate_prediction_benchmark(copy$benchmark, copy$result), mismatch)
  result$models$main_model$coefficients[[1L]] <- result$models$main_model$coefficients[[1L]] + 1
  expect_error(AutoXplainR:::validate_prediction_benchmark(output, result), mismatch)
  expect_error(benchmark_predictions(result, batch_size = 0), "batch_size")
  expect_error(benchmark_predictions(result, n_repeats = 1), "n_repeats")
  expect_error(benchmark_predictions(result, min_duration = Inf), "min_duration")
})

test_that("a native bounded run retains measured blocks without asserting machine speed", {
  result <- benchmark_fixture()
  output <- benchmark_predictions(result, n_repeats = 2L, max_seconds = 2, min_duration = .02)
  expect_s3_class(output, "autoxplain_prediction_benchmark")
  expect_true(all(output$measurements$completed <= output$measurements$iterations))
  expect_true(all(output$summary$status %in% c("computed", "partial", "resolution_limited", "resolution_unknown")))
  measured <- output$measurements[output$measurements$phase == "measurement", ]
  expect_equal(measured$ms_per_batch, measured$elapsed_ms / measured$iterations)
  expect_equal(measured$ms_per_row, measured$ms_per_batch / measured$batch_rows)
  expect_true(AutoXplainR:::validate_prediction_benchmark(output, result))
})

test_that("unknown clock resolution withholds a summary cost", {
  rows <- data.frame(
    model_id = "a", phase = "measurement", elapsed_ms = rep(1, 3),
    ms_per_batch = 1, error = "", warning = ""
  )
  output <- AutoXplainR:::benchmark_model_summary("a", rows, 3L, 1L, NA_real_)
  expect_identical(output$status, "resolution_unknown")
  expect_true(is.na(output$median_ms_per_batch))
  expect_true(grepl("No positive clock step", output$reason, fixed = TRUE))
})

test_that("a permanent sampled-batch backend failure retains raw records when identity recheck also fails", {
  state <- new.env(parent = emptyenv())
  state$failed <- FALSE
  predictor <- function(model, newdata) {
    if (identical(newdata$x, c(2L, 3L, 6L))) state$failed <- TRUE
    if (state$failed) stop("Backend permanently failed")
    newdata$x
  }
  result <- evaluate_models(list(custom = list(coefficient = 1)), data.frame(x = 1:6, y = 1:6), "y",
    predict_functions = list(custom = predictor)
  )
  output <- benchmark_predictions(result, batch_size = 3L, n_repeats = 2L, max_seconds = 1)
  expect_identical(output$summary$status, "identity_failed")
  expect_true(any(grepl("Backend permanently failed", output$measurements$error, fixed = TRUE)))
  expect_true(is.na(output$summary$median_ms_per_batch))
  expect_true(grepl("Backend permanently failed", output$summary$reason, fixed = TRUE))
  expect_error(AutoXplainR:::validate_prediction_benchmark(output, result), "unchanged retained models")
})

test_that("summary, raw timing, unit and sampling edits invalidate the captured benchmark payload", {
  result <- benchmark_fixture()
  clock <- 0
  local_mocked_bindings(
    benchmark_clock = function() {
      clock <<- clock + .1
      clock
    },
    benchmark_clock_step = function(...) .001,
    .package = "AutoXplainR"
  )
  original <- benchmark_predictions(result, n_repeats = 2L)
  mutations <- list(
    function(x) {
      x$summary$median_ms_per_row[[1L]] <- 999
      x
    },
    function(x) {
      x$measurements$elapsed_ms[[1L]] <- 999
      x
    },
    function(x) {
      x$protocol$batch_rows <- 999L
      x
    },
    function(x) {
      x$sampling_rows <- rev(x$sampling_rows)
      x
    }
  )
  for (mutate in mutations) {
    expect_error(AutoXplainR:::validate_prediction_benchmark(mutate(original), result), "Benchmark payload changed")
  }
  copy <- unserialize(serialize(original, NULL))
  expect_true(AutoXplainR:::validate_prediction_benchmark(copy, result))
})
