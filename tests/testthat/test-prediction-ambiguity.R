test_that("probability distance is the largest pairwise total variation", {
  probabilities <- list(
    c(no = 0.8, yes = 0.2),
    c(no = 0.5, yes = 0.5),
    c(no = 0.1, yes = 0.9)
  )

  expect_equal(AutoXplainR:::case_probability_distance(probabilities), 0.7)
  expect_equal(AutoXplainR:::case_probability_distance(probabilities[1]), 0)
})

test_that("regression ambiguity retains case-level candidate ranges", {
  result <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 2026)
  ambiguity <- prediction_ambiguity(result)

  expect_s3_class(ambiguity, "autoxplain_prediction_ambiguity")
  expect_equal(ambiguity$task, "regression")
  expect_setequal(
    ambiguity$model_ids,
    c("main_model", "small_tree", "flexible_tree")
  )
  expect_false(any(ambiguity$model_performance$model_id == "simple_baseline"))
  expect_equal(nrow(ambiguity$rows), nrow(result$test_data))
  expect_true(all(ambiguity$rows$prediction_range >= 0))
  expect_equal(
    ambiguity$rows$prediction_range,
    ambiguity$rows$prediction_max - ambiguity$rows$prediction_min
  )
  expect_equal(
    ambiguity$max_prediction_range,
    max(ambiguity$rows$prediction_range)
  )
  expect_match(ambiguity$scope_note, "not a confidence interval", fixed = TRUE)
  expect_output(print(ambiguity), "median gap")
})

test_that("classification ambiguity separates labels from probability distance", {
  set.seed(622)
  data <- data.frame(x = rnorm(260), z = rnorm(260))
  data$event <- factor(ifelse(data$x + 0.4 * data$z + rnorm(260) > 0, "yes", "no"))
  result <- autoxplain(data, "event", model_set = "comparison", seed = 9)
  ambiguity <- prediction_ambiguity(result)

  expect_equal(ambiguity$task, "binary")
  expect_equal(
    ambiguity$probability_distance,
    "positive-class probability range"
  )
  expect_true(all(ambiguity$rows$probability_distance >= 0 &
                    ambiguity$rows$probability_distance <= 1))
  expect_equal(
    ambiguity$class_disagreement_rate,
    mean(ambiguity$rows$class_disagreement)
  )
  expect_true(all(ambiguity$rows$n_predicted_classes %in% c(1L, 2L)))
  expect_output(print(ambiguity), "class split")

  multiclass <- autoxplain(iris, "Species", model_set = "comparison", seed = 9)
  multiclass_ambiguity <- prediction_ambiguity(multiclass)
  expect_equal(
    multiclass_ambiguity$probability_distance,
    "maximum pairwise total-variation distance"
  )
  expect_true(all(multiclass_ambiguity$rows$probability_distance <= 1 + 1e-12))
})

test_that("ambiguity selection exposes performance filtering boundaries", {
  result <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 2026)
  filtered <- prediction_ambiguity(result, performance_tolerance = 1)

  expect_equal(filtered$n_models, 3L)
  expect_true(all(filtered$model_performance$included))
  expect_error(
    prediction_ambiguity(result, performance_tolerance = 0.01),
    "fewer than two"
  )
  expect_error(
    prediction_ambiguity(result, performance_tolerance = -1),
    "performance_tolerance"
  )

  quick <- autoxplain(mtcars, "mpg", seed = 2026)
  expect_error(prediction_ambiguity(quick), "model_set")
  expect_error(prediction_ambiguity(list()), "returned by")
})

test_that("comparison reports show ambiguity beside candidate performance", {
  result <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 2026)
  path <- tempfile(fileext = ".html")
  render_model_report(result, path, top_features = 1, n_repeats = 2)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(html, "Where did supplied model choices disagree?", fixed = TRUE)
  expect_match(html, "Disagreement is a review signal, not an error bar", fixed = TRUE)
  expect_match(html, "Largest prediction range", fixed = TRUE)

  quick <- autoxplain(mtcars, "mpg", seed = 2026)
  quick_path <- tempfile(fileext = ".html")
  render_model_report(quick, quick_path, top_features = 1, n_repeats = 2)
  quick_html <- paste(readLines(quick_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("model choices disagree", quick_html, fixed = TRUE))
})
