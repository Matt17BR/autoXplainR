test_that("metadata never turns unavailable training timings into a zero total", {
  fit <- lm(mpg ~ wt, mtcars[1:20, ])
  result <- evaluate_models(list(first = fit, second = fit), mtcars[21:32, ], "mpg", features = "wt")
  metadata <- attr(extract_model_characteristics(result), "summary")
  expect_true(is.na(metadata$total_final_refit_time_s))
  expect_true(is.na(metadata$dataset_info$n_rows))
  expect_equal(metadata$measured_final_refits, 0L)
  result$model_diagnostics$training_time_ms <- c(1500, NA)
  expect_true(is.na(attr(extract_model_characteristics(result), "summary")$total_final_refit_time_s))
  result$model_diagnostics$training_time_ms <- c(1500, 500)
  expect_equal(attr(extract_model_characteristics(result), "summary")$total_final_refit_time_s, 2)
})

test_that("narrative uses the declared event and reference without inventing an intercept baseline", {
  data <- data.frame(x = 1:4, outcome = factor(c("yes", "yes", "yes", "no"), levels = c("yes", "no")))
  constant <- function(model, newdata) rep(model$probability, nrow(newdata))
  result <- evaluate_models(
    list(current = list(probability = .8), historical = list(probability = .2)),
    data, "outcome", positive = "yes", reference = "historical",
    labels = c(current = "Current rule", historical = "Historical rule"),
    predict_functions = list(current = constant, historical = constant)
  )
  context <- AutoXplainR:::prepare_narrative_context(result)
  expect_identical(context$positive_class, "yes")
  expect_identical(context$baseline_label, "Historical rule")
  expect_equal(context$baseline_performance, -(3 * log(.2) + log(.8)) / 4)
  memo <- generate_natural_language_report(result)
  expect_match(memo, "Historical rule", fixed = TRUE)
  expect_match(memo, "refer to `yes`", fixed = TRUE)
  expect_false(grepl("intercept-only baseline|second training outcome level", memo))
  expect_output(print(result), "supplied prediction adapter")
})
