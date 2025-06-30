test_that("plot_permutation_importance works correctly", {
  skip_if_not_installed("h2o")
  skip_if_not_installed("plotly")
  
  # Load breast cancer dataset
  bc_path <- system.file("extdata", "breast-cancer.csv", package = "AutoXplainR")
  breast_cancer <- read.csv(bc_path, stringsAsFactors = FALSE)
  bc_sample <- breast_cancer[sample(nrow(breast_cancer), 100), ]
  
  # Get model and importance
  result <- autoxplain(bc_sample, "diagnosis", max_models = 1, max_runtime_secs = 30)
  importance <- calculate_permutation_importance(
    result$models[[1]], 
    bc_sample, 
    "diagnosis",
    n_permutations = 2
  )
  
  # Test basic plotting
  expect_no_error({
    p <- plot_permutation_importance(importance)
  })
  
  p <- plot_permutation_importance(importance)
  
  # Check that it returns a plotly object
  expect_s3_class(p, "plotly")
  expect_s3_class(p, "htmlwidget")
  
  # Test with custom parameters
  expect_no_error({
    p_custom <- plot_permutation_importance(importance, 
                                           title = "Custom Title",
                                           max_features = 5,
                                           color = "#FF0000")
  })
  
  expect_s3_class(p_custom, "plotly")
})

test_that("plot_partial_dependence works correctly", {
  skip_if_not_installed("h2o")
  skip_if_not_installed("plotly")
  
  # Load housing dataset
  housing_path <- system.file("extdata", "california-housing.csv", package = "AutoXplainR")
  housing <- read.csv(housing_path, stringsAsFactors = FALSE)
  housing_sample <- housing[sample(nrow(housing), 200), ]
  
  # Get model and PDP
  result <- autoxplain(housing_sample, "median_house_value", max_models = 1, max_runtime_secs = 30)
  model <- result$models[[1]]
  pdp <- calculate_partial_dependence(model, housing_sample, "median_income", grid_size = 10)
  
  # Test basic plotting
  expect_no_error({
    p <- plot_partial_dependence(pdp)
  })
  
  p <- plot_partial_dependence(pdp)
  
  # Check that it returns a plotly object
  expect_s3_class(p, "plotly")
  expect_s3_class(p, "htmlwidget")
  
  # Test with custom parameters
  expect_no_error({
    p_custom <- plot_partial_dependence(pdp, 
                                       title = "Custom PDP",
                                       color = "#00FF00")
  })
})

test_that("plot_partial_dependence_multi works correctly", {
  skip_if_not_installed("h2o")
  skip_if_not_installed("plotly")
  
  # Create test data
  data(mtcars)
  result <- autoxplain(mtcars, "mpg", max_models = 1, max_runtime_secs = 60)
  model <- result$models[[1]]
  
  features <- c("wt", "hp")
  pdp_multi <- calculate_partial_dependence_multi(model, mtcars, features, n_points = 10)
  
  # Test basic plotting
  expect_no_error({
    p <- plot_partial_dependence_multi(pdp_multi)
  })
  
  p <- plot_partial_dependence_multi(pdp_multi)
  
  # Check that it returns a plotly object
  expect_s3_class(p, "plotly")
  expect_s3_class(p, "htmlwidget")
  
  # Test with custom parameters
  expect_no_error({
    p_custom <- plot_partial_dependence_multi(pdp_multi,
                                             title = "Multi PDP",
                                             colors = c("#FF0000", "#0000FF"),
                                             ncol = 1)
  })
})


test_that("plot_model_comparison works correctly", {
  skip_if_not_installed("h2o")
  skip_if_not_installed("plotly")
  
  # Create test data with multiple models
  data(iris)
  result <- autoxplain(iris, "Species", max_models = 3, max_runtime_secs = 120)
  
  # Test basic plotting
  expect_no_error({
    p <- plot_model_comparison(result)
  })
  
  p <- plot_model_comparison(result)
  
  # Check that it returns a plotly object
  expect_s3_class(p, "plotly")
  expect_s3_class(p, "htmlwidget")
  
  # Test with regression data
  data(mtcars)
  result_reg <- autoxplain(mtcars, "mpg", max_models = 2, max_runtime_secs = 120)
  
  expect_no_error({
    p_reg <- plot_model_comparison(result_reg)
  })
  
  # Test with custom parameters
  expect_no_error({
    p_custom <- plot_model_comparison(result, 
                                     title = "Custom Comparison")
  })
})

test_that("plotting functions input validation works", {
  skip_if_not_installed("plotly")
  
  # Test invalid inputs for plot_permutation_importance
  expect_error(plot_permutation_importance("not_dataframe"))
  expect_error(plot_permutation_importance(data.frame(wrong = 1, columns = 2)))
  
  # Test invalid inputs for plot_partial_dependence
  expect_error(plot_partial_dependence("not_dataframe"))
  expect_error(plot_partial_dependence(data.frame(only_one_column = 1)))
  
  # Test invalid inputs for plot_partial_dependence_multi
  expect_error(plot_partial_dependence_multi("not_list"))
  expect_error(plot_partial_dependence_multi(list()))  # Empty list
  
})

test_that("extract_model_type works correctly", {
  # Test different H2O model ID patterns
  model_ids <- c(
    "GBM_model_1234",
    "RandomForest_model_5678", 
    "DRF_model_9012",
    "GLM_model_3456",
    "DeepLearning_model_7890",
    "XGBoost_model_2468",
    "StackedEnsemble_model_1357",
    "SomeOther_model_9999"
  )
  
  types <- AutoXplainR:::extract_model_type(model_ids)
  
  expected_types <- c("GBM", "Random Forest", "Random Forest", "GLM", 
                     "Deep Learning", "XGBoost", "Ensemble", "SomeOther")
  
  expect_equal(types, expected_types)
  expect_length(types, length(model_ids))
})

test_that("null coalescing operator works correctly", {
  # Test %||% operator
  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_equal(NA %||% "default", NA)
  expect_equal(0 %||% "default", 0)
})

test_that("plotting functions handle edge cases", {
  skip_if_not_installed("h2o")
  skip_if_not_installed("plotly")
  
  # Create minimal test data
  data(iris)
  result <- autoxplain(iris, "Species", max_models = 1, max_runtime_secs = 60)
  model <- result$models[[1]]
  
  # Test with single feature importance
  importance_single <- data.frame(
    feature = "test_feature",
    importance = 0.5,
    stringsAsFactors = FALSE
  )
  
  expect_no_error({
    p <- plot_permutation_importance(importance_single)
  })
  
  # Test with single point PDP
  pdp_single <- data.frame(
    test_feature = 1.0,
    partial_dependence = 0.5,
    stringsAsFactors = FALSE
  )
  
  expect_no_error({
    p <- plot_partial_dependence(pdp_single)
  })
  
  
  # Test model comparison with single model
  expect_no_error({
    p <- plot_model_comparison(result)
  })
})