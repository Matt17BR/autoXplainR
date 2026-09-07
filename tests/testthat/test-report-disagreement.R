test_that("disagreement matches hand calculations and never exports unrequested records", {
  data <- data.frame(x = 1:4, y = c(1, 2, 3, 4))
  rownames(data) <- paste0("PRIVATE-ROW-", 1:4)
  rule <- function(model, newdata) model$offset + newdata$x
  result <- evaluate_models(
    list(a = list(offset = 0), b = list(offset = 2), c = list(offset = -1)),
    data, "y", predict_functions = list(a = rule, b = rule, c = rule), features = "x"
  )
  original <- result
  for (mode in c("summary", "none")) {
    result$.report_export <- AutoXplainR:::prepare_data_explorer(result, report_data_control(mode))
    view <- AutoXplainR:::report_disagreement_view(result)
    pairs <- view$pairs
    expect_equal(pairs$mean_prediction_distance, c(3, 2, 1))
    expect_equal(pairs$p90_prediction_distance, c(3, 2, 1))
    expect_null(view$cases)
    html <- AutoXplainR:::explorer_disagreement(result)
    expect_false(grepl("PRIVATE-ROW-|data-select-row", html))
  }
  result$.report_export <- AutoXplainR:::prepare_data_explorer(result, report_data_control("rows", max_rows = 2L))
  view <- AutoXplainR:::report_disagreement_view(result)
  expect_length(view$cases, 2L)
  expect_true(all(vapply(view$cases, `[[`, numeric(1), "gap") == 3))
  keys <- vapply(result$.report_export$rows, `[[`, character(1), "row_key")
  expect_true(all(vapply(view$cases, `[[`, character(1), "row_key") %in% keys))
  result$.report_export <- NULL
  expect_identical(result, original)
})

test_that("disagreement uses the recorded event, reference and original source positions", {
  data <- data.frame(x = 1:4, event = factor(c("yes", "no", "yes", "no"), levels = c("yes", "no")))
  rule <- function(model, newdata) model$p[newdata$x]
  result <- evaluate_models(
    list(a = list(p = c(.5, .2, .8, .1)), b = list(p = c(.4, .3, .9, .2)), ref = list(p = rep(.5, 4))),
    data, "event", positive = "yes", reference = "ref", features = "x",
    predict_functions = list(a = rule, b = rule, ref = rule)
  )
  view <- AutoXplainR:::report_disagreement_view(result)
  expect_equal(view$pairs$class_disagreement_rate, .25)
  expect_equal(view$pairs$mean_prediction_distance, .1)
  expect_setequal(view$performance$model_id, c("a", "b"))
  result$models$b <- NULL
  expect_null(AutoXplainR:::report_disagreement_view(result))
})
