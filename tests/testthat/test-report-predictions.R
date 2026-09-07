test_that("binary cutoff records match independent counted decisions including ties", {
  observed <- factor(c("no", "yes", "no", "yes", "yes", "no"), levels = c("no", "yes"))
  probability <- c(0, .2, .5, .5, .9, 1)
  records <- AutoXplainR:::prediction_cutoff_records(observed, probability, "yes")
  expect_length(records, 101)
  for (index in c(1, 21, 50, 51, 91, 101)) {
    record <- records[[index]]
    decision <- factor(ifelse(probability >= record$threshold, "yes", "no"), levels = c("no", "yes"))
    counted <- table(observed, decision)
    expect_equal(
      unname(c(record$tn, record$fp, record$fn, record$tp)),
      unname(c(counted[1, 1], counted[1, 2], counted[2, 1], counted[2, 2]))
    )
    expect_equal(record$accuracy, mean(observed == decision))
  }
  expect_equal(unlist(records[[51]][c("tp", "fp", "tn", "fn")]), c(tp = 2, fp = 2, tn = 1, fn = 1))
  absent <- AutoXplainR:::prediction_cutoff_records(rep("no", 6), probability, "yes")
  expect_true(is.na(absent[[51]]$sensitivity))
  expect_equal(absent[[51]]$specificity, 2 / 6)
  expect_identical(records[[51]]$display$accuracy, "50.0%")
  expect_identical(records[[51]]$display$fp, "2")
})

test_that("regression summaries conserve all rows and use the signed residual definition", {
  records <- AutoXplainR:::prediction_regression_records(c(0, 2, 4, 8), c(1, 1, 3, 5))
  expect_equal(records$metrics, c(rmse = sqrt(3), mae = 1.5, bias = 1))
  expect_equal(sum(records$density$n), 4)
  expect_equal(sum(records$residual_histogram$n), 4)
  expect_equal(sum(records$bias$n), 4)
  expect_true(all(is.na(records$bias$mean_residual[records$bias$n == 1])))
  finite_means <- ifelse(is.na(records$bias$mean_residual), 0, records$bias$mean_residual)
  expect_equal(sum(records$bias$n * finite_means), 0)
  constant <- AutoXplainR:::prediction_regression_records(rep(3, 8), rep(3, 8))
  expect_equal(constant$metrics, c(rmse = 0, mae = 0, bias = 0))
  expect_equal(sum(constant$density$n), 8)
  expect_true(all(is.finite(constant$density$predicted_low)))
})

test_that("classification confusion retains absent classes and calibration has explicit scope", {
  cells <- AutoXplainR:::prediction_confusion(c("b", "b", "a"), c("a", "b", "a"), c("a", "b", "c"))
  expect_equal(vapply(cells, `[[`, integer(1), "count"), c(1L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L))
  expect_null(cells[[9]]$rate)
  expect_equal(cells[[4]]$rate, .5)
  probability <- matrix(c(.8, .1, .1, .1, .7, .2, .1, .2, .7),
    nrow = 3, byrow = TRUE,
    dimnames = list(NULL, c("a", "b", "c"))
  )
  calibration <- AutoXplainR:::prediction_calibration_records(c("a", "c", "c"), probability, c("a", "b", "c"))
  expect_match(calibration$scope, "Confidence calibration", fixed = TRUE)
  expect_equal(sum(vapply(calibration$bins, `[[`, integer(1), "n")), 3)
  expect_equal(sum(vapply(calibration$bins, `[[`, integer(1), "correct_or_events")), 2)
  expect_false(any(c("probabilities", "row_key", "predictions") %in% names(calibration)))
  binary <- AutoXplainR:::prediction_calibration_records(
    c("no", "yes", "yes"), c(.11, .12, .99), c("no", "yes"), "yes"
  )
  expect_length(binary$bins, 1)
  expect_equal(binary$bins[[1]]$mean_probability, mean(c(.11, .12, .99)))
  expect_equal(binary$bins[[1]]$observed_rate, 2 / 3)
  expect_equal(binary$bins[[1]]$n, 3)
  expect_equal(binary$bins[[1]]$low, .11)
  expect_equal(binary$bins[[1]]$high, .99)
})

test_that("reported calibration uses canonical rank groups and reconstructs the leaderboard gap", {
  probability <- rep(c(.1, .3, .9), each = 10)
  observed <- c(rep(c("yes", "no"), c(2, 8)), rep(c("yes", "no"), c(4, 6)), rep(c("yes", "no"), c(9, 1)))
  calibration <- AutoXplainR:::prediction_calibration_records(observed, probability, c("yes", "no"), "yes")
  expect_length(calibration$bins, 3)
  expect_equal(vapply(calibration$bins, `[[`, numeric(1), "mean_probability"), c(.1, .3, .9))
  expect_equal(vapply(calibration$bins, `[[`, numeric(1), "observed_rate"), c(.2, .4, .9))
  expect_equal(vapply(calibration$bins, `[[`, integer(1), "n"), rep(10L, 3))
  expect_equal(calibration$calibration_error, 2 / 30)
  d <- data.frame(row = seq_len(30), outcome = factor(observed, levels = c("yes", "no")))
  fit <- evaluate_models(list(score = probability), d, "outcome", positive = "yes",
    predict_functions = list(score = function(model, newdata) model[newdata$row])
  )
  expect_equal(fit$leaderboard$calibration_error, calibration$calibration_error)
  expect_equal(calibration_diagnostics(fit)$calibration_error, calibration$calibration_error)
  tied <- AutoXplainR:::prediction_calibration_records(rep("yes", 40), rep(.37, 40), c("no", "yes"), "yes")
  expect_length(tied$bins, 1)
  expect_equal(tied$bins[[1]]$n, 40)
  expect_equal(tied$bins[[1]]$low, .37)
  expect_equal(tied$bins[[1]]$high, .37)
})

test_that("individual prediction export follows only explicit retained evaluation mapping", {
  rows <- list(
    list(partition = "training", retained = TRUE, processed_position = 1L, row_key = "data:1"),
    list(partition = "evaluation", retained = FALSE, processed_position = NA_integer_, row_key = "test:3"),
    list(
      partition = "evaluation", retained = TRUE, processed_position = 3L,
      row_key = "test:800", source = "test", source_row = 800L
    )
  )
  result <- list(.report_export = list(mode = "rows", rows = rows))
  records <- AutoXplainR:::prediction_exported_cases(result, c(10, 20, 30), c(11, 17, 25))
  expect_length(records, 1)
  expect_identical(records[[1]]$row_key, "test:800")
  expect_identical(records[[1]]$source_row, 800L)
  expect_equal(records[[1]]$observed, 30)
  expect_equal(records[[1]]$predicted, 25)
  expect_equal(records[[1]]$residual, 5)
  for (mode in c("summary", "none")) {
    result$.report_export$mode <- mode
    expect_null(AutoXplainR:::prediction_exported_cases(result, c(10, 20, 30), c(11, 17, 25)))
  }
  result$.report_export$mode <- "rows"
  result$.report_export$rows[[3]]$processed_position <- 10L
  expect_error(AutoXplainR:::prediction_exported_cases(result, c(10, 20, 30), c(11, 17, 25)), "not aligned")
})

test_that("summary prediction payload and charts never embed per-case values", {
  d <- data.frame(x = seq_len(80), y = (seq_len(80) + .123456789)^2)
  result <- autoxplain(d, "y", model_set = "quick", seed = 88, explain = FALSE)
  result$.report_context <- AutoXplainR:::prepare_report_context(result)
  result$.report_export <- list(mode = "summary", rows = NULL)
  local_mocked_bindings(make_prediction_adapter = function(...) stop("new prediction"), .package = "AutoXplainR")
  view <- AutoXplainR:::prepare_prediction_view(result, names(result$models))
  expect_true(all(vapply(view$models, function(model) is.null(model$cases), logical(1))))
  html <- AutoXplainR:::explorer_predictions(result, AutoXplainR:::explorer_models(result))
  expect_false(grepl("data-case-row|data-select-row|processed_position|source_row", html))
  for (sentinel in as.character(result$test_data$y)) {
    expect_false(grepl(sentinel, html, fixed = TRUE))
  }
  expect_match(html, "Occupied evaluation bins and row counts", fixed = TRUE)
  expect_match(html, "Distribution of errors", fixed = TRUE)
})

test_that("exported classification cases prioritize confident errors and respect class names", {
  probability <- cbind(cat = c(.6, .03, .1), dog = c(.3, .02, .8), bird = c(.1, .95, .1))
  observed <- factor(c("cat", "dog", "cat"), levels = c("cat", "dog", "bird"))
  result <- list(.report_export = list(mode = "rows", rows = lapply(1:3, function(i) {
    list(
      row_key = paste0("test:", i), partition = "evaluation", retained = TRUE,
      processed_position = i, source = "test", source_row = i
    )
  })))
  records <- AutoXplainR:::prediction_exported_cases(result, observed, probability[, 3:1], levels(observed))
  ordered <- AutoXplainR:::prediction_ordered_cases(records, "multiclass")
  expect_equal(vapply(ordered, `[[`, integer(1), "processed_position"), c(2L, 3L, 1L))
  expect_equal(vapply(ordered, `[[`, character(1), "predicted"), c("bird", "dog", "cat"))
  expect_equal(vapply(ordered, `[[`, numeric(1), "observed_probability"), c(.02, .1, .6))
  expect_equal(vapply(ordered, `[[`, numeric(1), "predicted_probability"), c(.95, .8, .6))
  expect_identical(records, AutoXplainR:::prediction_exported_cases(result, observed, probability, levels(observed)))
  binary <- AutoXplainR:::prediction_exported_cases(
    result, c("no", "yes", "no"), c(.5, .01, .1), c("no", "yes"), "yes"
  )
  ordered <- AutoXplainR:::prediction_ordered_cases(binary, "binary")
  expect_equal(vapply(ordered, `[[`, integer(1), "processed_position"), c(2L, 1L, 3L))
  expect_equal(vapply(ordered, `[[`, numeric(1), "observed_probability"), c(.01, .5, .9))
  expect_equal(vapply(ordered, `[[`, numeric(1), "predicted_probability"), c(.99, .5, .9))
  html <- AutoXplainR:::prediction_case_table(binary, "binary")
  expect_match(html, "P(observed)", fixed = TRUE)
  expect_match(html, "P(predicted)", fixed = TRUE)
})
