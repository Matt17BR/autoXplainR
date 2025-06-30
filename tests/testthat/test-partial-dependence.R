test_that("partial dependence works for regression", {
  skip_if_not_installed("h2o")
  
  # Load California housing dataset
  housing_path <- system.file("extdata", "california-housing.csv", package = "AutoXplainR")
  housing <- read.csv(housing_path, stringsAsFactors = FALSE)
  housing_sample <- housing[sample(nrow(housing), 300), ]
  
  # Create a regression model
  result <- autoxplain(housing_sample, "median_house_value", max_models = 1, max_runtime_secs = 60)
  model <- result$models[[1]]
  
  # Test PDP calculation
  expect_no_error({
    pdp <- calculate_partial_dependence(model, housing_sample, "median_income", grid_size = 10)
  })
  
  pdp <- calculate_partial_dependence(model, housing_sample, "median_income", grid_size = 10)
  
  # Check result structure
  expect_s3_class(pdp, "data.frame")
  expect_named(pdp, c("median_income", "partial_dependence"))
  expect_equal(nrow(pdp), 10)
  expect_true(all(is.numeric(pdp$median_income)))
  expect_true(all(is.numeric(pdp$partial_dependence)))
  
  # Check attributes
  expect_equal(attr(pdp, "feature"), "median_income")
  expect_equal(attr(pdp, "grid_size"), 10)
  expect_true(is.numeric(attr(pdp, "feature_range")))
  
  # Check that feature values are within data range
  income_range <- range(housing_sample$median_income)
  expect_true(all(pdp$median_income >= income_range[1] & pdp$median_income <= income_range[2]))
})

test_that("partial dependence works for classification", {
  skip_if_not_installed("h2o")
  
  # Load breast cancer dataset
  bc_path <- system.file("extdata", "breast-cancer.csv", package = "AutoXplainR")
  breast_cancer <- read.csv(bc_path, stringsAsFactors = FALSE)
  bc_sample <- breast_cancer[sample(nrow(breast_cancer), 200), ]
  
  # Create a classification model
  result <- autoxplain(bc_sample, "diagnosis", max_models = 1, max_runtime_secs = 60)
  model <- result$models[[1]]
  
  # Test PDP calculation
  expect_no_error({
    pdp <- calculate_partial_dependence(model, bc_sample, "radius_mean", grid_size = 10)
  })
  
  pdp <- calculate_partial_dependence(model, bc_sample, "radius_mean", grid_size = 10)
  
  # Check result structure
  expect_s3_class(pdp, "data.frame")
  expect_named(pdp, c("radius_mean", "partial_dependence"))
  expect_equal(nrow(pdp), 10)
  expect_true(all(is.numeric(pdp$radius_mean)))
  expect_true(all(is.numeric(pdp$partial_dependence)))
  
  # Check attributes
  expect_equal(attr(pdp, "feature"), "radius_mean")
  expect_equal(attr(pdp, "grid_size"), 10)
})

test_that("partial dependence multi-feature function works", {
  skip_if_not_installed("h2o")
  
  # Load wine quality dataset
  wine_path <- system.file("extdata", "winequality-red.csv", package = "AutoXplainR")
  wine <- read.csv(wine_path, stringsAsFactors = FALSE)
  wine_sample <- wine[sample(nrow(wine), 300), ]
  
  result <- autoxplain(wine_sample, "quality", max_models = 1, max_runtime_secs = 60)
  model <- result$models[[1]]
  
  # Test multi-feature PDP
  features <- c("alcohol", "sulphates", "pH")
  expect_no_error({
    pdp_multi <- calculate_partial_dependence_multi(model, wine_sample, features, grid_size = 10)
  })
  
  pdp_multi <- calculate_partial_dependence_multi(model, wine_sample, features, grid_size = 10)
  
  # Check result structure
  expect_type(pdp_multi, "list")
  expect_named(pdp_multi, features)
  expect_equal(length(pdp_multi), 3)
  
  # Check each PDP result
  for (feature in features) {
    expect_s3_class(pdp_multi[[feature]], "data.frame")
    expect_named(pdp_multi[[feature]], c(feature, "partial_dependence"))
    expect_equal(nrow(pdp_multi[[feature]]), 10)
    expect_equal(attr(pdp_multi[[feature]], "feature"), feature)
  }
})

test_that("partial dependence input validation works", {
  skip_if_not_installed("h2o")
  
  # Use pima diabetes dataset
  pima_path <- system.file("extdata", "pima-diabetes.csv", package = "AutoXplainR")
  pima <- read.csv(pima_path, stringsAsFactors = FALSE)
  pima_sample <- pima[1:100, ]
  
  result <- autoxplain(pima_sample, "Outcome", max_models = 1, max_runtime_secs = 60)
  model <- result$models[[1]]
  
  # Test invalid inputs
  expect_error(calculate_partial_dependence(model, "not_dataframe", "Glucose"))
  expect_error(calculate_partial_dependence(model, pima_sample, "nonexistent_feature"))
  expect_error(calculate_partial_dependence(model, pima_sample, "Glucose", grid_size = 1))
  
  # Test multi-feature validation
  expect_error(calculate_partial_dependence_multi(model, pima_sample, character(0)))  # Empty features
  expect_error(calculate_partial_dependence_multi(model, pima_sample, c("Glucose", "nonexistent")))  # Missing feature
})

test_that("partial dependence handles categorical features", {
  skip_if_not_installed("h2o")
  
  # Use adult income dataset with categorical features
  adult_path <- system.file("extdata", "adult_income.csv", package = "AutoXplainR")
  adult <- read.csv(adult_path, stringsAsFactors = FALSE, na.strings = "?")
  adult_sample <- adult[sample(nrow(adult), 300), ]
  
  result <- autoxplain(
    adult_sample, 
    "income", 
    max_models = 1, 
    max_runtime_secs = 60,
    preprocessing_config = list(
      handle_missing = TRUE,
      convert_characters = TRUE
    )
  )
  model <- result$models[[1]]
  
  # Test PDP for categorical feature
  pdp_cat <- calculate_partial_dependence(model, adult_sample, "workclass")
  
  # Check that grid includes unique values
  expect_true(nrow(pdp_cat) <= length(unique(adult_sample$workclass)))
  expect_true(all(pdp_cat$workclass %in% unique(adult_sample$workclass)))
})

test_that("get_feature_grid handles different data types", {
  # Test numeric data
  numeric_data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  grid_numeric <- AutoXplainR:::get_feature_grid(numeric_data, 5)
  expect_length(grid_numeric, 5)
  expect_true(all(is.numeric(grid_numeric)))
  expect_true(min(grid_numeric) >= min(numeric_data))
  expect_true(max(grid_numeric) <= max(numeric_data))
  
  # Test factor data
  factor_data <- factor(c("A", "B", "C", "A", "B", "A", "A"))
  grid_factor <- AutoXplainR:::get_feature_grid(factor_data, 5)
  expect_true(all(grid_factor %in% levels(factor_data)))
  
  # Test character data
  char_data <- c("X", "Y", "Z", "X", "Y", "X", "X")
  grid_char <- AutoXplainR:::get_feature_grid(char_data, 5)
  expect_true(all(grid_char %in% unique(char_data)))
})

test_that("partial dependence handles edge cases", {
  skip_if_not_installed("h2o")
  
  # Create minimal dataset
  minimal_data <- data.frame(
    x = rnorm(50),
    y = rnorm(50)
  )
  
  result <- autoxplain(minimal_data, "y", max_models = 1, max_runtime_secs = 30)
  model <- result$models[[1]]
  
  # Test with very few points
  pdp_few <- calculate_partial_dependence(model, minimal_data, "x", grid_size = 2)
  expect_equal(nrow(pdp_few), 2)
  
  # Test with many points
  expect_no_error({
    pdp_many <- calculate_partial_dependence(model, minimal_data, "x", grid_size = 50)
  })
  
  # Should handle data with equal values at max grid_size
  expect_equal(nrow(pdp_many), 50)
})