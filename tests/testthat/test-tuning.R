test_that("local tuning is reproducible and isolated from the outer holdout", {
  set.seed(91)
  data <- data.frame(x = runif(180, -2, 2), z = rnorm(180))
  data$y <- data$x^2 + 0.4 * data$z + rnorm(180, sd = 0.12)
  set.seed(812)
  state <- .Random.seed

  first <- autoxplain(
    data, "y", model_set = "tuned", max_models = 7, nfolds = 4, seed = 2026
  )
  second <- autoxplain(
    data, "y", model_set = "tuned", max_models = 7, nfolds = 4, seed = 2026
  )

  expect_identical(.Random.seed, state)
  expect_s3_class(first$tuning, "autoxplain_tuning")
  expect_equal(first$tuning$selected_configuration, second$tuning$selected_configuration)
  expect_equal(first$tuning$candidates, second$tuning$candidates, tolerance = 1e-8)
  expect_equal(first$evaluation$metrics, second$evaluation$metrics, tolerance = 1e-8)
  expect_equal(first$tuning$folds_used, 4L)
  expect_equal(nrow(first$tuning$candidates), 7L)
  expect_setequal(
    unique(first$tuning$candidates$family),
    c("linear", "tree", "neural")
  )
  expect_true(all(first$tuning$candidates$folds_completed == 4L))
  expect_true(all(first$tuning$candidates$evaluated_rows == nrow(first$training_data)))
  expect_match(first$tuning$scope_note, "held-out evaluation rows were untouched")
  expect_match(first$provenance$candidate_selection, "training-only resampling")
  expect_true(first$tuning$candidates$selected[1L])
  expect_equal(
    first$tuning$candidates$retained_model_id[first$tuning$candidates$selected],
    "main_model"
  )
  expect_equal(length(first$models), 4L)
  expect_true(all(c("main_model", "simple_baseline") %in% names(first$models)))
  expect_equal(sum(names(first$models) %in% c(
    "linear_model", "tree_model", "neural_model"
  )), 2L)
  expect_output(print(first), "training-resampled configurations")
  expect_output(print(tuning_results(first)), "outer training rows")
  expect_equal(
    as_explainers(first, models = "main_model")$main_model$metadata$source,
    "guided local tuning workflow"
  )
})

test_that("best-score tuning rule chooses the minimum resampled loss", {
  set.seed(22)
  data <- data.frame(x = runif(150, -3, 3), z = rnorm(150))
  data$y <- sin(2 * data$x) + data$z^2 + rnorm(150, sd = 0.1)
  result <- autoxplain(
    data,
    "y",
    model_set = "tuned",
    max_models = 8,
    nfolds = 3,
    tuning_rule = "best",
    seed = 33
  )
  candidates <- result$tuning$candidates
  expect_equal(
    candidates$configuration_id[candidates$selected],
    candidates$configuration_id[[which.min(candidates$cv_score)]]
  )
  expect_output(print(result$tuning), "lowest resampled error")
})

test_that("tuned classification models retain valid probability contracts", {
  set.seed(41)
  binary <- data.frame(x = rnorm(180), z = rnorm(180))
  binary$event <- factor(ifelse(binary$x^2 + binary$z > 0.7, "yes", "no"))
  binary_fit <- autoxplain(
    binary, "event", model_set = "tuned", max_models = 6, nfolds = 3, seed = 17
  )
  binary_explainer <- as_explainers(binary_fit, models = "main_model")$main_model
  binary_probability <- predict(binary_explainer, binary_explainer$data)
  expect_true(all(is.finite(binary_probability)))
  expect_true(all(binary_probability >= 0 & binary_probability <= 1))
  expect_true(all(vapply(binary_fit$evaluation$metrics, function(metrics) {
    is.finite(metrics[["log_loss"]])
  }, logical(1))))

  multiclass_fit <- autoxplain(
    iris, "Species", model_set = "tuned", max_models = 6, nfolds = 3, seed = 19
  )
  multiclass_explainer <- as_explainers(multiclass_fit, models = "main_model")$main_model
  probability <- predict(multiclass_explainer, multiclass_explainer$data)
  expect_true(is.matrix(probability))
  expect_equal(colnames(probability), levels(iris$Species))
  expect_equal(unname(rowSums(probability)), rep(1, nrow(probability)), tolerance = 1e-6)
})

test_that("tuning refits preprocessing inside folds and records novel-level mappings", {
  set.seed(9)
  data <- data.frame(
    x = rnorm(80),
    group = c(rep("common", 76), rep("rare", 4))
  )
  data$y <- 2 * data$x + rnorm(80, sd = 0.2)
  result <- autoxplain(
    data, "y", model_set = "tuned", max_models = 3, nfolds = 4, seed = 7
  )
  expect_true(all(result$tuning$fold_scores$validation_rows >= 2L))
  expect_true(all(result$tuning$fold_scores$novel_levels_mapped >= 0L))
  expect_equal(
    length(unique(result$tuning$fold_scores$fold)),
    result$tuning$folds_used
  )
})

test_that("outer evaluation values cannot change training-only tuning", {
  set.seed(15)
  training <- data.frame(x = rnorm(90), group = rep(c("a", "b", "c"), 30))
  training$y <- 2 * training$x + as.numeric(factor(training$group)) + rnorm(90, sd = 0.2)
  test_one <- data.frame(x = rnorm(20), group = rep(c("a", "b"), 10), y = rnorm(20))
  test_two <- transform(test_one, x = x * 1e6, y = y * -1e8)

  first <- autoxplain(
    training, "y", test_data = test_one, model_set = "tuned",
    max_models = 5, nfolds = 3, seed = 818
  )
  second <- autoxplain(
    training, "y", test_data = test_two, model_set = "tuned",
    max_models = 5, nfolds = 3, seed = 818
  )

  expect_identical(first$tuning$plan, second$tuning$plan)
  stable_columns <- setdiff(names(first$tuning$fold_scores), "elapsed_ms")
  expect_equal(
    first$tuning$fold_scores[stable_columns],
    second$tuning$fold_scores[stable_columns],
    tolerance = 1e-12
  )
  expect_identical(first$tuning$selected_configuration, second$tuning$selected_configuration)
  expect_false(identical(first$evaluation$metrics, second$evaluation$metrics))
})

test_that("tuning validation is actionable", {
  expect_error(
    autoxplain(mtcars, "mpg", model_set = "tuned", max_models = 2),
    "at least 3"
  )
  expect_error(
    autoxplain(mtcars, "mpg", model_set = "tuned", nfolds = 1),
    "at least 2"
  )
  expect_error(tuning_results(autoxplain(mtcars, "mpg")), "No local tuning")

  scarce <- data.frame(
    x = 1:12,
    y = factor(c(rep("a", 10), "b", "b"))
  )
  result <- autoxplain(
    scarce,
    "y",
    model_set = "tuned",
    max_models = 3,
    nfolds = 5,
    test_data = scarce[c(1L, 11L), ],
    seed = 2
  )
  expect_equal(result$tuning$folds_used, 2L)
})

test_that("guided report explains tuning separately from final evaluation", {
  set.seed(61)
  data <- data.frame(x = runif(120, -2, 2), z = rnorm(120))
  data$y <- data$x^2 + data$z + rnorm(120, sd = 0.2)
  result <- autoxplain(
    data, "y", model_set = "tuned", max_models = 5, nfolds = 3, seed = 10
  )
  path <- tempfile(fileext = ".html")
  render_model_report(result, path, top_features = 2, n_repeats = 2)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(html, "How was the primary model selected", fixed = TRUE)
  expect_match(html, "Preprocessing was learned again inside every fold", fixed = TRUE)
  expect_match(html, "Do not quote the resampled tuning score", fixed = TRUE)
  expect_match(html, "primary model was chosen before this holdout", fixed = TRUE)
  expect_match(html, result$tuning$selected_configuration, fixed = TRUE)

  context <- AutoXplainR:::prepare_analysis_context(result)
  prompt <- AutoXplainR:::context_to_text(context)
  expect_match(prompt, "Automatic tuning:", fixed = TRUE)
  expect_match(prompt, "training-only folds", fixed = TRUE)
  expect_match(prompt, "Tuning boundary:", fixed = TRUE)

  memo <- generate_natural_language_report(result)
  expect_match(memo, "How automatic tuning selected the model", fixed = TRUE)
  expect_match(memo, "it is not the final performance estimate", fixed = TRUE)
})
