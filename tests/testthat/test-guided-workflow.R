test_that("guided regression uses a reproducible untouched holdout", {
  set.seed(19)
  data <- data.frame(x = rnorm(120), z = rnorm(120))
  data$y <- 4 * data$x - data$z + rnorm(120, sd = 0.2)
  set.seed(77)
  state <- .Random.seed

  first <- autoxplain(data, "y", seed = 902)
  second <- autoxplain(data, "y", seed = 902)

  expect_identical(.Random.seed, state)
  expect_s3_class(first, "autoxplain_result")
  expect_equal(first$engine, "base")
  expect_equal(first$task, "regression")
  expect_identical(rownames(first$test_data), rownames(second$test_data))
  expect_equal(nrow(first$training_data) + nrow(first$test_data), nrow(data))
  expect_setequal(names(first$models), c("main_model", "simple_baseline"))
  expect_true(first$evaluation$beats_baseline)
  expect_lt(first$evaluation$metrics$main_model[["rmse"]], 0.5)
  expect_match(first$evaluation$metric_definitions[["rmse"]], "lower is better")
  expect_true(all(c("observed", "primary_prediction", "error", "absolute_error") %in%
                    names(first$evaluation$predictions)))
  expect_true(all(c("mean_error", "median_absolute_error", "p90_absolute_error") %in%
                    names(first$evaluation$diagnostics)))
  expect_output(print(first), "improvement")
  expect_named(summary(first), c(
    "task", "target", "n_models", "n_features", "leaderboard", "evaluation", "provenance"
  ))
})

test_that("guided binary evaluation is stratified and probability-aware", {
  set.seed(41)
  data <- data.frame(x = rnorm(180), group = sample(c("a", "b"), 180, TRUE))
  data$clicked <- factor(ifelse(data$x + rnorm(180, sd = 0.5) > 0, "yes", "no"))
  result <- autoxplain(data, "clicked", seed = 8)

  expect_equal(result$task, "binary")
  expect_setequal(unique(result$test_data$clicked), levels(result$training_data$clicked))
  expect_true(all(c("log_loss", "brier_score", "accuracy", "balanced_accuracy", "roc_auc") %in%
                    names(result$evaluation$metrics$main_model)))
  expect_true(result$evaluation$metrics$main_model[["roc_auc"]] > 0.8)
  expect_true(all(result$evaluation$predictions$primary_probability >= 0 &
                    result$evaluation$predictions$primary_probability <= 1))
  expect_true(all(c("observed", "predicted", "rows") %in%
                    names(result$evaluation$diagnostics$confusion_matrix)))
  expect_equal(result$provenance$split_method, "reproducible stratified holdout")
  explainers <- as_explainers(result, models = "main_model")
  expect_s3_class(explainers$main_model, "autoxplain_explainer")
  expect_equal(explainers$main_model$metadata$evaluation_role, "held-out test")
  expect_equal(explainers$main_model$metadata$source, "guided base workflow")
})

test_that("guided multiclass evaluation returns normalized class probabilities", {
  result <- autoxplain(iris, "Species", seed = 15)
  explainer <- as_explainers(result, models = 1)$main_model
  probability <- predict(explainer, explainer$data)

  expect_equal(result$task, "multiclass")
  expect_equal(ncol(probability), 3L)
  expect_equal(unname(rowSums(probability)), rep(1, nrow(probability)), tolerance = 1e-7)
  expect_true(all(c("log_loss", "brier_score", "accuracy", "macro_recall") %in%
                    names(result$evaluation$metrics$main_model)))
  expect_true(all(result$evaluation$predictions$primary_confidence >= 0 &
                    result$evaluation$predictions$primary_confidence <= 1))
})

test_that("comparison mode fits approachable local candidates without changing the primary", {
  regression <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 2026)

  expect_setequal(
    names(regression$models),
    c("main_model", "small_tree", "flexible_tree", "simple_baseline")
  )
  expect_equal(regression$provenance$model_set, "comparison")
  expect_match(regression$provenance$candidate_selection, "pre-specified")
  expect_equal(regression$leaderboard$role[regression$leaderboard$model_id == "main_model"],
               "primary")
  expect_true(all(c(
    "training_time_ms", "prediction_time_ms", "model_size_kb", "complexity"
  ) %in% names(regression$leaderboard)))
  expect_true(all(regression$leaderboard$model_size_kb > 0))
  expect_output(print(regression), "comparison set")

  binary_data <- transform(mtcars, am = factor(am, labels = c("automatic", "manual")))
  binary <- autoxplain(binary_data, "am", model_set = "comparison", seed = 2026)
  expect_equal(length(binary$models), 4L)
  expect_true("model_fit_warning" %in% binary$evaluation$notes$code)
  expect_true(nzchar(binary$model_diagnostics$fit_warning[
    binary$model_diagnostics$model_id == "main_model"
  ]))
  expect_true(all(vapply(binary$evaluation$metrics, function(x) {
    is.finite(x[["log_loss"]])
  }, logical(1))))

  multiclass <- autoxplain(iris, "Species", model_set = "comparison", seed = 2026)
  expect_equal(length(multiclass$models), 4L)
  expect_true(all(vapply(multiclass$evaluation$metrics, function(x) {
    is.finite(x[["log_loss"]])
  }, logical(1))))
})

test_that("preprocessing learns imputations only from training data", {
  train <- data.frame(
    x = c(1, 2, 3, NA, 5, 6, 7, 8, 9, 10),
    category = rep(c("a", "b"), 5),
    y = 1:10
  )
  test <- data.frame(x = c(NA, 100), category = c("a", "b"), y = c(11, 12))
  result <- autoxplain(train, "y", test_data = test)

  expected <- stats::median(train$x, na.rm = TRUE)
  expect_equal(result$test_data$x[[1]], expected)
  expect_equal(result$preprocessing_metadata$training_data$recipe$imputations$x, expected)
  expect_equal(result$provenance$split_method, "user-supplied test data")
  expect_equal(result$provenance$evaluation_rows, 2L)
})

test_that("automatic splitting keeps categorical levels learnable", {
  data <- data.frame(
    category = c(rep("common", 18), "rare", "rare"),
    x = seq_len(20),
    y = seq_len(20) + c(rep(0, 19), 1)
  )
  found <- NULL
  for (seed in seq_len(100)) {
    candidate <- autoxplain(data, "y", seed = seed, test_fraction = 0.4)
    if (candidate$provenance$rows_moved_for_unseen_levels > 0L) {
      found <- candidate
      break
    }
  }
  expect_false(is.null(found))
  expect_true(all(as.character(found$test_data$category) %in%
                    as.character(found$training_data$category)))
})

test_that("guided workflow reports actionable input errors", {
  data <- data.frame(x = 1:10, y = 1:10)
  missing_target <- data
  missing_target$y[[1]] <- NA
  expect_error(autoxplain(missing_target, "y"), "target contains missing")
  expect_error(autoxplain(data, "y", test_fraction = 1), "greater than zero")
  expect_error(
    autoxplain(transform(data, x = replace(x, 1, NA)), "y", enable_preprocessing = FALSE),
    "Missing predictor values require preprocessing"
  )
  expect_error(
    autoxplain(data.frame(x = I(replicate(10, list(1))), y = 1:10), "y"),
    "supports numeric"
  )
  expect_error(
    autoxplain(data.frame(when = as.Date("2020-01-01") + 0:9, y = 1:10), "y"),
    "unsupported columns"
  )
  infinite_target <- data
  infinite_target$y[[1L]] <- Inf
  expect_error(autoxplain(infinite_target, "y"), "finite numeric target")
  expect_error(autoxplain(data, "y", task = "binary"), "exactly two")
  rare <- data.frame(x = 1:10, y = factor(c("rare", rep("common", 9))))
  expect_error(autoxplain(rare, "y"), "at least two rows")
})

test_that("binary AUC handles ties and single-class evaluation", {
  expect_equal(AutoXplainR:::guided_binary_auc(c(FALSE, TRUE), c(0.5, 0.5)), 0.5)
  expect_true(is.na(AutoXplainR:::guided_binary_auc(c(TRUE, TRUE), c(0.2, 0.8))))
})

test_that("guided evaluation flags fragile score contexts", {
  result <- autoxplain(mtcars, "mpg", seed = 10)
  expect_true(all(c("small_evaluation_set", "few_rows_per_feature") %in%
                    result$evaluation$notes$code))

  set.seed(4)
  data <- data.frame(x = rnorm(100), y = factor(c(rep("rare", 5), rep("common", 95))))
  classification <- autoxplain(data, "y", test_fraction = 0.2, seed = 4)
  expect_true("few_rows_in_class" %in% classification$evaluation$notes$code)

  training <- data.frame(x = seq_len(500), y = seq_len(500))
  evaluation <- data.frame(x = seq_len(100), y = seq_len(100))
  poor_summary <- list(
    beats_baseline = FALSE,
    metrics = list(main_model = c(r_squared = -0.2))
  )
  poor_notes <- AutoXplainR:::guided_evaluation_notes(
    training, evaluation, "y", "regression", poor_summary
  )
  expect_true(all(c("baseline_not_beaten", "negative_heldout_r_squared") %in%
                    poor_notes$code))

  good_summary <- list(
    beats_baseline = TRUE,
    metrics = list(main_model = c(r_squared = 0.8))
  )
  good_notes <- AutoXplainR:::guided_evaluation_notes(
    training, evaluation, "y", "regression", good_summary
  )
  expect_equal(nrow(good_notes), 0L)
})

test_that("constant inputs are removed, recorded, and reused in new evaluations", {
  data <- data.frame(
    x = c(NA, seq_len(29)),
    constant_number = 1,
    constant_group = factor("same"),
    y = seq_len(30) + rnorm(30, sd = 0.1)
  )
  result <- autoxplain(data, "y", seed = 13)

  expect_false(any(c("constant_number", "constant_group") %in% result$features))
  expect_true("constant_inputs_removed" %in% result$evaluation$notes$code)
  expect_true(all(c("constant_number", "constant_group") %in%
                    result$preprocessing_metadata$training_data$recipe$removed_columns))

  new_data <- data.frame(
    x = c(NA, 50),
    constant_number = 1,
    constant_group = factor("same"),
    y = c(40, 50)
  )
  explainer <- as_explainers(result, data = new_data, models = "main_model")$main_model
  expect_equal(names(explainer$data), "x")
  expect_false(anyNA(explainer$data))

  holdout <- data.frame(
    x = c(40, 50),
    constant_number = 2,
    constant_group = factor("different"),
    y = c(40, 50)
  )
  supplied <- autoxplain(data, "y", test_data = holdout, seed = 13)
  expect_equal(supplied$features, "x")
  expect_true(all(c("constant_number", "constant_group") %in%
                    supplied$provenance$constant_features_removed))
})

test_that("evaluation outcomes must match the training outcome contract", {
  train <- data.frame(x = 1:20, y = rep(c(0, 1), 10))
  unseen <- data.frame(x = 21:22, y = c(0, 2))
  missing <- data.frame(x = 21:22, y = c(0, NA))

  expect_error(autoxplain(train, "y", test_data = unseen), "classes absent")
  expect_error(autoxplain(train, "y", test_data = missing), "missing values")
})
