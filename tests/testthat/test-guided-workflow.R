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
  expect_true(all(c("log_loss", "accuracy", "balanced_accuracy", "roc_auc") %in%
                    names(result$evaluation$metrics$main_model)))
  expect_true(result$evaluation$metrics$main_model[["roc_auc"]] > 0.8)
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
  expect_true(all(c("log_loss", "accuracy", "macro_recall") %in%
                    names(result$evaluation$metrics$main_model)))
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
    "list-like predictors"
  )
  rare <- data.frame(x = 1:10, y = factor(c("rare", rep("common", 9))))
  expect_error(autoxplain(rare, "y"), "at least two rows")
})

test_that("binary AUC handles ties and single-class evaluation", {
  expect_equal(AutoXplainR:::guided_binary_auc(c(FALSE, TRUE), c(0.5, 0.5)), 0.5)
  expect_true(is.na(AutoXplainR:::guided_binary_auc(c(TRUE, TRUE), c(0.2, 0.8))))
})
