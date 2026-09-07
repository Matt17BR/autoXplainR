test_that("copyable prediction code quotes model IDs and class labels as R literals", {
  id <- 'fit "A" \\ route'
  positive <- 'yes"; stop("injected") #'
  negative <- "no\nline"
  labels <- c(positive, negative)
  data <- data.frame(x = 1:4, outcome = factor(c(positive, negative, positive, negative), levels = labels))
  probability <- function(model, newdata) model$p[newdata$x]
  result <- evaluate_models(
    stats::setNames(list(list(p = c(.57, .56, .2, .9))), id), data, "outcome",
    predict_functions = stats::setNames(list(probability), id), positive = positive, features = "x"
  )
  model <- AutoXplainR:::prepare_prediction_view(result, id)$models[[1L]]
  scope <- list2env(list(result = result, new_data = data), parent = environment())
  expect_no_error(eval(parse(text = AutoXplainR:::prediction_r_example(model)), envir = scope))
  expect_equal(scope$probability, c(.57, .56, .2, .9))
  expect_identical(levels(scope$predicted_class), labels)
  expect_identical(as.character(scope$predicted_class), c(positive, positive, negative, positive))
  code <- model$r_code
  explored <- paste0(code$prediction, "\n", code$cutoff_prefix, "0.57", code$cutoff_suffix)
  expect_no_error(eval(parse(text = explored), envir = scope))
  expect_identical(as.character(scope$predicted_class), c(positive, negative, negative, positive))
})
