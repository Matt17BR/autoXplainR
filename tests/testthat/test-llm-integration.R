test_that("LLM integration input validation works", {
  skip_if_not_installed("h2o")
  
  # Create test data
  data(iris)
  result <- autoxplain(iris, "Species", max_models = 1, max_runtime_secs = 60)
  
  # Test invalid autoxplain_result
  expect_error(generate_natural_language_report("not_valid_result"))
  expect_error(generate_natural_language_report(list(not = "valid")))
  
  # Test missing API key
  old_key <- Sys.getenv("GEMINI_API_KEY")
  Sys.setenv(GEMINI_API_KEY = "")
  expect_error(generate_natural_language_report(result), 
               "Google Generative AI API key not found")
  
  # Restore environment
  Sys.setenv(GEMINI_API_KEY = old_key)
})

test_that("LLM integration creates fallback report when API fails", {
  skip_if_not_installed("h2o")
  
  # Create test data with explanations
  data(iris)
  result <- autoxplain(iris, "Species", max_models = 1, max_runtime_secs = 60)
  model <- result$models[[1]]
  
  importance <- calculate_permutation_importance(model, iris, "Species", n_repeats = 2)
  
  # Test with invalid API key (should trigger fallback)
  report <- generate_natural_language_report(result, 
                                           importance_data = importance,
                                           api_key = "invalid_key")
  
  # Check that we get a fallback report
  expect_type(report, "character")
  expect_true(nchar(report) > 100)  # Should be a substantial report
  expect_true(grepl("AutoML Analysis Report", report))
  expect_true(grepl("Species", report))  # Should mention target column
  expect_true(grepl("classification", report))  # Should mention task type
})

test_that("prepare_analysis_context works correctly", {
  skip_if_not_installed("h2o")
  
  # Create test data
  data(iris)
  result <- autoxplain(iris, "Species", max_models = 2, max_runtime_secs = 60)
  model <- result$models[[1]]
  
  importance <- calculate_permutation_importance(model, iris, "Species", n_repeats = 2)
  pdp_data <- calculate_partial_dependence_multi(model, iris, c("Petal.Length", "Sepal.Length"), n_points = 5)
  
  # Test context preparation
  context <- AutoXplainR:::prepare_analysis_context(result, importance, pdp_data)
  
  # Check context structure
  expect_type(context, "list")
  expect_named(context, c("task_type", "target_column", "n_features", "n_models", 
                         "best_model_type", "best_performance", "best_metric",
                         "importance_summary", "pdp_summary"))
  
  # Check values
  expect_equal(context$task_type, "classification")
  expect_equal(context$target_column, "Species")
  expect_equal(context$n_features, 4)
  expect_equal(context$n_models, 2)
  expect_type(context$best_model_type, "character")
  
  # Check summaries
  expect_type(context$importance_summary, "list")
  expect_named(context$importance_summary, c("top_features", "metric"))
  expect_length(context$importance_summary$top_features, 3)
  
  expect_type(context$pdp_summary, "list")
  expect_named(context$pdp_summary, c("features_analyzed", "n_features"))
})

test_that("create_report_prompt generates comprehensive prompts", {
  # Test with minimal context
  context <- list(
    task_type = "classification",
    target_column = "Species",
    n_features = 4,
    n_models = 2,
    best_model_type = "RandomForest",
    best_performance = 0.95,
    best_metric = "AUC",
    importance_summary = list(
      top_features = c("Petal.Length", "Petal.Width", "Sepal.Length"),
      metric = "permutation_importance"
    ),
    pdp_summary = list(
      features_analyzed = c("Petal.Length", "Petal.Width"),
      n_features = 2
    )
  )
  
  prompt <- AutoXplainR:::create_report_prompt(context)
  
  expect_type(prompt, "character")
  expect_true(nchar(prompt) > 200)
  expect_true(grepl("classification", prompt))
  expect_true(grepl("Species", prompt))
  expect_true(grepl("RandomForest", prompt))
  expect_true(grepl("Petal.Length", prompt))
})

test_that("create_fallback_report generates structured reports", {
  # Test with minimal context
  context <- list(
    task_type = "regression", 
    target_column = "mpg",
    n_features = 5,
    n_models = 3,
    best_model_type = "GBM",
    best_performance = 0.85,
    best_metric = "rmse",
    importance_summary = list(
      top_features = c("wt", "hp", "disp"),
      metric = "permutation_importance"
    ),
    pdp_summary = NULL
  )
  
  report <- AutoXplainR:::create_fallback_report(context)
  
  expect_type(report, "character")
  expect_true(nchar(report) > 300)
  expect_true(grepl("AutoML Analysis Report", report))
  expect_true(grepl("regression", report))
  expect_true(grepl("mpg", report))
  expect_true(grepl("GBM", report))
  expect_true(grepl("wt", report))
  
  # Test with full context
  context_full <- context
  context_full$pdp_summary <- list(
    features_analyzed = c("wt", "hp"),
    n_features = 2
  )
  
  report_full <- AutoXplainR:::create_fallback_report(context_full)
  expect_true(grepl("Partial Dependence", report_full))
})