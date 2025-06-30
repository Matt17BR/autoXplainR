test_that("autoxplain function works with classification data", {
  skip_if_not_installed("h2o")
  
  # Load real breast cancer dataset
  bc_path <- system.file("extdata", "breast-cancer.csv", package = "AutoXplainR")
  breast_cancer <- read.csv(bc_path, stringsAsFactors = FALSE)
  
  # Use smaller subset for faster testing
  bc_sample <- breast_cancer[sample(nrow(breast_cancer), 200), ]
  
  # Test basic functionality
  expect_no_error({
    result <- autoxplain(bc_sample, "diagnosis", max_models = 2, max_runtime_secs = 60)
  })
  
  # Test input validation
  expect_error(autoxplain("not_a_dataframe", "diagnosis"))
  expect_error(autoxplain(bc_sample, "nonexistent_column"))
  expect_error(autoxplain(bc_sample, "diagnosis", max_models = 0))
  expect_error(autoxplain(bc_sample, "diagnosis", max_runtime_secs = 0))
})

test_that("autoxplain result structure is correct", {
  skip_if_not_installed("h2o")
  
  # Use breast cancer dataset
  bc_path <- system.file("extdata", "breast-cancer.csv", package = "AutoXplainR")
  breast_cancer <- read.csv(bc_path, stringsAsFactors = FALSE)
  bc_sample <- breast_cancer[sample(nrow(breast_cancer), 200), ]
  
  result <- autoxplain(bc_sample, "diagnosis", max_models = 2, max_runtime_secs = 60)
  
  # Check result structure
  expect_s3_class(result, "autoxplain_result")
  expect_named(result, c("models", "leaderboard", "training_data", "test_data", 
                        "target_column", "features", "automl_object", "model_characteristics"))
  
  # Check models
  expect_type(result$models, "list")
  expect_true(length(result$models) >= 1)
  expect_true(length(result$models) <= 2)
  
  # Check data consistency
  expect_equal(nrow(result$training_data), nrow(bc_sample))
  expect_equal(result$target_column, "diagnosis")
  expect_equal(result$features, setdiff(colnames(bc_sample), "diagnosis"))
  
  # Check model characteristics
  expect_type(result$model_characteristics, "list")
  expect_s3_class(result$model_characteristics, "autoxplainr_model_characteristics")
})

test_that("autoxplain works with test data", {
  skip_if_not_installed("h2o")
  
  # Use Pima diabetes dataset
  pima_path <- system.file("extdata", "pima-diabetes.csv", package = "AutoXplainR")
  pima <- read.csv(pima_path, stringsAsFactors = FALSE)
  
  # Split data
  train_idx <- sample(nrow(pima), 0.7 * nrow(pima))
  train_data <- pima[train_idx, ]
  test_data <- pima[-train_idx, ]
  
  expect_no_error({
    result <- autoxplain(train_data, "Outcome", max_models = 2, 
                        max_runtime_secs = 60, test_data = test_data)
  })
  
  # Check test data is stored
  result <- autoxplain(train_data, "Outcome", max_models = 2, 
                      max_runtime_secs = 60, test_data = test_data)
  expect_equal(nrow(result$test_data), nrow(test_data))
})

test_that("autoxplain handles regression tasks", {
  skip_if_not_installed("h2o")
  
  # Use California housing dataset
  housing_path <- system.file("extdata", "california-housing.csv", package = "AutoXplainR")
  housing <- read.csv(housing_path, stringsAsFactors = FALSE)
  
  # Use smaller subset for testing
  housing_sample <- housing[sample(nrow(housing), 500), ]
  
  expect_no_error({
    result <- autoxplain(housing_sample, "median_house_value", max_models = 2, max_runtime_secs = 60)
  })
  
  result <- autoxplain(housing_sample, "median_house_value", max_models = 2, max_runtime_secs = 60)
  expect_s3_class(result, "autoxplain_result")
  expect_equal(result$target_column, "median_house_value")
})

test_that("autoxplain handles preprocessing correctly", {
  skip_if_not_installed("h2o")
  
  # Use adult income dataset with missing values
  adult_path <- system.file("extdata", "adult_income.csv", package = "AutoXplainR")
  adult <- read.csv(adult_path, stringsAsFactors = FALSE, na.strings = "?")
  
  # Use small subset
  adult_sample <- adult[sample(nrow(adult), 300), ]
  
  # Test with preprocessing
  expect_no_error({
    result <- autoxplain(
      adult_sample, 
      "income", 
      max_models = 2, 
      max_runtime_secs = 60,
      preprocessing_config = list(
        handle_missing = TRUE,
        convert_characters = TRUE,
        remove_id_columns = TRUE
      )
    )
  })
})

test_that("autoxplain handles edge cases", {
  skip_if_not_installed("h2o")
  
  # Create minimal dataset
  minimal_data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50),
    y = sample(c("A", "B"), 50, replace = TRUE)
  )
  
  # Test with minimal data
  expect_no_error({
    result <- autoxplain(minimal_data, "y", max_models = 1, max_runtime_secs = 30)
  })
  
  # Test with single feature
  single_feature <- data.frame(
    feature = rnorm(100),
    target = rnorm(100)
  )
  
  expect_no_error({
    result <- autoxplain(single_feature, "target", max_models = 1, max_runtime_secs = 30)
  })
})