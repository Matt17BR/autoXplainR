test_that("permutation importance works for classification", {
  skip_if_not_installed("h2o")
  
  # Load breast cancer dataset
  bc_path <- system.file("extdata", "breast-cancer.csv", package = "AutoXplainR")
  breast_cancer <- read.csv(bc_path, stringsAsFactors = FALSE)
  bc_sample <- breast_cancer[sample(nrow(breast_cancer), 200), ]
  
  # Create a classification model
  result <- autoxplain(bc_sample, "diagnosis", max_models = 1, max_runtime_secs = 60)
  model <- result$models[[1]]
  
  # Test permutation importance
  expect_no_error({
    importance <- calculate_permutation_importance(model, bc_sample, "diagnosis", 
                                                  metric = "logloss", n_permutations = 2)
  })
  
  importance <- calculate_permutation_importance(model, bc_sample, "diagnosis", 
                                                metric = "logloss", n_permutations = 2)
  
  # Check result structure
  expect_s3_class(importance, "data.frame")
  expect_named(importance, c("feature", "importance", "std_dev"))
  expect_true(nrow(importance) > 0)
  expect_true(nrow(importance) <= ncol(bc_sample) - 1)  # All features except target
  
  # Check attributes
  expect_equal(attr(importance, "metric"), "logloss")
  expect_equal(attr(importance, "n_permutations"), 2)
  expect_true(is.numeric(attr(importance, "baseline_score")))
  
  # Check that results are sorted by importance
  expect_true(all(diff(importance$importance) <= 0))
})

test_that("permutation importance works for regression", {
  skip_if_not_installed("h2o")
  
  # Load housing dataset
  housing_path <- system.file("extdata", "california-housing.csv", package = "AutoXplainR")
  housing <- read.csv(housing_path, stringsAsFactors = FALSE)
  housing_sample <- housing[sample(nrow(housing), 300), ]
  
  # Create a regression model
  result <- autoxplain(housing_sample, "median_house_value", max_models = 1, max_runtime_secs = 60)
  model <- result$models[[1]]
  
  # Test permutation importance
  expect_no_error({
    importance <- calculate_permutation_importance(model, housing_sample, "median_house_value", 
                                                  metric = "rmse", n_permutations = 2)
  })
  
  importance <- calculate_permutation_importance(model, housing_sample, "median_house_value", 
                                                metric = "rmse", n_permutations = 2)
  
  # Check result structure
  expect_s3_class(importance, "data.frame")
  expect_named(importance, c("feature", "importance", "std_dev"))
  expect_equal(nrow(importance), ncol(housing_sample) - 1)  # All features except target
  
  # Check attributes
  expect_equal(attr(importance, "metric"), "rmse")
  expect_equal(attr(importance, "n_permutations"), 2)
  expect_true(is.numeric(attr(importance, "baseline_score")))
})

test_that("permutation importance auto-detects metrics", {
  skip_if_not_installed("h2o")
  
  # Test regression auto-detection
  wine_path <- system.file("extdata", "winequality-red.csv", package = "AutoXplainR")
  wine <- read.csv(wine_path, stringsAsFactors = FALSE)
  wine_sample <- wine[sample(nrow(wine), 200), ]
  
  result <- autoxplain(wine_sample, "quality", max_models = 1, max_runtime_secs = 60)
  model <- result$models[[1]]
  
  importance <- calculate_permutation_importance(model, wine_sample, "quality", 
                                                metric = "auto", n_permutations = 2)
  expect_equal(attr(importance, "metric"), "rmse")
  
  # Test classification auto-detection
  pima_path <- system.file("extdata", "pima-diabetes.csv", package = "AutoXplainR")
  pima <- read.csv(pima_path, stringsAsFactors = FALSE)
  pima_sample <- pima[sample(nrow(pima), 200), ]
  
  result <- autoxplain(pima_sample, "Outcome", max_models = 1, max_runtime_secs = 60)
  model <- result$models[[1]]
  
  importance <- calculate_permutation_importance(model, pima_sample, "Outcome", 
                                                metric = "auto", n_permutations = 2)
  expect_equal(attr(importance, "metric"), "logloss")
})

test_that("permutation importance input validation works", {
  skip_if_not_installed("h2o")
  
  bc_path <- system.file("extdata", "breast-cancer.csv", package = "AutoXplainR")
  breast_cancer <- read.csv(bc_path, stringsAsFactors = FALSE)
  bc_sample <- breast_cancer[1:100, ]
  
  result <- autoxplain(bc_sample, "diagnosis", max_models = 1, max_runtime_secs = 60)
  model <- result$models[[1]]
  
  # Test invalid inputs
  expect_error(calculate_permutation_importance(model, "not_dataframe", "diagnosis"))
  expect_error(calculate_permutation_importance(model, bc_sample, "nonexistent_column"))
  expect_error(calculate_permutation_importance(model, bc_sample, "diagnosis", n_permutations = 0))
  expect_error(calculate_permutation_importance(model, bc_sample, "diagnosis", metric = "invalid_metric"))
})

test_that("permutation importance handles different metrics correctly", {
  skip_if_not_installed("h2o")
  
  # Binary classification for AUC
  bc_path <- system.file("extdata", "breast-cancer.csv", package = "AutoXplainR")
  breast_cancer <- read.csv(bc_path, stringsAsFactors = FALSE)
  bc_sample <- breast_cancer[sample(nrow(breast_cancer), 200), ]
  
  result <- autoxplain(bc_sample, "diagnosis", max_models = 1, max_runtime_secs = 60)
  model <- result$models[[1]]
  
  # Test AUC metric
  expect_no_error({
    importance_auc <- calculate_permutation_importance(model, bc_sample, "diagnosis", 
                                                      metric = "auc", n_permutations = 2)
  })
  
  importance_auc <- calculate_permutation_importance(model, bc_sample, "diagnosis", 
                                                    metric = "auc", n_permutations = 2)
  expect_equal(attr(importance_auc, "metric"), "auc")
  
  # Test accuracy metric
  expect_no_error({
    importance_acc <- calculate_permutation_importance(model, bc_sample, "diagnosis", 
                                                      metric = "accuracy", n_permutations = 2)
  })
  
  importance_acc <- calculate_permutation_importance(model, bc_sample, "diagnosis", 
                                                    metric = "accuracy", n_permutations = 2)
  expect_equal(attr(importance_acc, "metric"), "accuracy")
})

test_that("permutation importance works with missing data", {
  skip_if_not_installed("h2o")
  
  # Use adult income dataset with missing values
  adult_path <- system.file("extdata", "adult_income.csv", package = "AutoXplainR")
  adult <- read.csv(adult_path, stringsAsFactors = FALSE, na.strings = "?")
  adult_sample <- adult[sample(nrow(adult), 200), ]
  
  # Train model with preprocessing
  result <- autoxplain(
    adult_sample, 
    "income", 
    max_models = 1, 
    max_runtime_secs = 60,
    preprocessing_config = list(handle_missing = TRUE)
  )
  model <- result$models[[1]]
  
  # Calculate importance
  expect_no_error({
    importance <- calculate_permutation_importance(
      model = model,
      data = adult_sample,
      target_column = "income",
      n_permutations = 2
    )
  })
})