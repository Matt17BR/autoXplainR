test_that("local tuning is reproducible and isolated from the outer holdout", {
  set.seed(91)
  data <- data.frame(x = runif(180, -2, 2), z = rnorm(180))
  data$y <- data$x^2 + 0.4 * data$z + rnorm(180, sd = 0.12)
  set.seed(812)
  state <- .Random.seed

  first <- autoxplain(
    data, "y", model_set = "tuned", portfolio = "core",
    max_models = 7, nfolds = 4, seed = 2026
  )
  second <- autoxplain(
    data, "y", model_set = "tuned", portfolio = "core",
    max_models = 7, nfolds = 4, seed = 2026
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
  expect_output(print(first), "compare_model_behavior")
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
    portfolio = "core",
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

test_that("one-SE selection never compares raw complexity units across families", {
  candidates <- data.frame(
    configuration_id = c("a_simple", "a_complex", "b_simple", "b_complex"),
    family = c("a", "a", "b", "b"),
    simplicity_rank = 2L,
    complexity_proxy = c(1000, 2000, 1, 2),
    cv_score = c(0.18, 0.10, 0.20, 0.11),
    stringsAsFactors = FALSE
  )
  selected <- AutoXplainR:::select_one_se_candidate(
    candidates,
    rep(TRUE, nrow(candidates))
  )

  # The least-flexible candidate is chosen inside each family first. Their CV
  # scores then break the cross-family tie; raw proxy units never compete.
  expect_identical(candidates$configuration_id[[selected]], "a_simple")
})

test_that("tuned classification models retain valid probability contracts", {
  set.seed(41)
  binary <- data.frame(x = rnorm(180), z = rnorm(180))
  binary$event <- factor(ifelse(binary$x^2 + binary$z > 0.7, "yes", "no"))
  binary_fit <- autoxplain(
    binary, "event", model_set = "tuned", portfolio = "core",
    max_models = 6, nfolds = 3, seed = 17
  )
  binary_explainer <- as_explainers(binary_fit, models = "main_model")$main_model
  binary_probability <- predict(binary_explainer, binary_explainer$data)
  expect_true(all(is.finite(binary_probability)))
  expect_true(all(binary_probability >= 0 & binary_probability <= 1))
  expect_true(all(vapply(binary_fit$evaluation$metrics, function(metrics) {
    is.finite(metrics[["log_loss"]])
  }, logical(1))))

  multiclass_fit <- autoxplain(
    iris, "Species", model_set = "tuned", portfolio = "core",
    max_models = 6, nfolds = 3, seed = 19
  )
  multiclass_explainer <- as_explainers(multiclass_fit, models = "main_model")$main_model
  probability <- predict(multiclass_explainer, multiclass_explainer$data)
  expect_true(is.matrix(probability))
  expect_equal(colnames(probability), levels(iris$Species))
  expect_equal(unname(rowSums(probability)), rep(1, nrow(probability)), tolerance = 1e-6)
})

test_that("out-of-fold predictions use medians learned from each training partition", {
  x <- c(1:20, 101:120, 1001:1020)
  data <- data.frame(x = x, y = 2 * x + sin(seq_along(x)))
  data$x[seq(1, 60, by = 4)] <- NA_real_
  ids <- rep(c("low", "middle", "high"), each = 20)
  result <- autoxplain(
    data, "y", test_data = data.frame(x = -4:-1, y = -8:-5),
    learners = "linear", max_models = 1, explain = FALSE,
    tuning_control = tuning_control(fold_ids = ids), seed = 7
  )
  # This reference uses only base R and explicit partitions. Neither the
  # package's recipe helper nor its fold-scoring code computes the answer.
  expected <- leaked <- numeric(nrow(data))
  for (fold in unique(ids)) {
    held_out <- ids == fold
    for (learn_globally in c(FALSE, TRUE)) {
      training <- data[!held_out, ]
      validation <- data[held_out, ]
      median <- stats::median(if (learn_globally) data$x else training$x, na.rm = TRUE)
      training$x[is.na(training$x)] <- median
      validation$x[is.na(validation$x)] <- median
      prediction <- predict(lm(y ~ x, data = training), validation)
      if (learn_globally) leaked[held_out] <- prediction else expected[held_out] <- prediction
    }
  }
  # Ensure this fixture can distinguish fold-local from leaked imputation.
  expect_gt(max(abs(expected - leaked)), 100)
  oof <- result$tuning$out_of_fold_predictions
  expect_equal(oof$estimate[order(oof$training_row)], expected, tolerance = 1e-10)
  fold <- result$tuning$fold_scores
  expected_rmse <- vapply(fold$fold, function(index) {
    rows <- result$tuning$fold_assignment$training_row[result$tuning$fold_assignment$fold == index]
    sqrt(mean((data$y[rows] - expected[rows])^2))
  }, numeric(1))
  expect_equal(fold$score, unname(expected_rmse), tolerance = 1e-10)
})

test_that("outer evaluation values cannot change training-only tuning", {
  set.seed(15)
  training <- data.frame(x = rnorm(90), group = rep(c("a", "b", "c"), 30))
  training$y <- 2 * training$x + as.numeric(factor(training$group)) + rnorm(90, sd = 0.2)
  test_one <- data.frame(x = rnorm(20), group = rep(c("a", "b"), 10), y = rnorm(20))
  test_two <- transform(test_one, x = x * 1e6, y = y * -1e8)

  first <- autoxplain(
    training, "y", test_data = test_one, model_set = "tuned", portfolio = "core",
    max_models = 5, nfolds = 3, seed = 818
  )
  second <- autoxplain(
    training, "y", test_data = test_two, model_set = "tuned", portfolio = "core",
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
    autoxplain(mtcars, "mpg", model_set = "tuned", portfolio = "core", max_models = 2),
    "number of requested learner families"
  )
  expect_error(
    autoxplain(mtcars, "mpg", model_set = "tuned", portfolio = "core", nfolds = 1),
    "at least 2"
  )
  expect_error(tuning_results(autoxplain(model_set = "quick", mtcars, "mpg")), "No local tuning")

  scarce <- data.frame(
    x = 1:12,
    y = factor(c(rep("a", 10), "b", "b"))
  )
  result <- autoxplain(
    scarce,
    "y",
    model_set = "tuned",
    portfolio = "core",
    max_models = 3,
    nfolds = 5,
    test_data = make_disjoint_evaluation(scarce, "y", c(1L, 11L)),
    seed = 2
  )
  expect_equal(result$tuning$folds_used, 2L)
})

test_that("guided report explains tuning separately from final evaluation", {
  set.seed(61)
  data <- data.frame(x = runif(120, -2, 2), z = rnorm(120))
  data$y <- data$x^2 + data$z + rnorm(120, sd = 0.2)
  result <- autoxplain(
    data, "y", model_set = "tuned", portfolio = "core",
    max_models = 5, nfolds = 3, seed = 10
  )
  path <- tempfile(fileext = ".html")
  render_model_report(result, path, top_features = 2, n_repeats = 2)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(html, "How was the primary model selected", fixed = TRUE)
  expect_match(html, "Preprocessing was learned again inside every fold", fixed = TRUE)
  expect_match(html, "Do not quote the resampled tuning score", fixed = TRUE)
  expect_match(
    html,
    "evaluation rows did not select this model",
    fixed = TRUE
  )
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
