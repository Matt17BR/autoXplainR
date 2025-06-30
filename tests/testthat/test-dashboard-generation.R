test_that("generate_dashboard creates HTML file", {
  skip_if_not_installed("h2o")
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("flexdashboard")
  
  # Load breast cancer dataset
  bc_path <- system.file("extdata", "breast-cancer.csv", package = "AutoXplainR")
  breast_cancer <- read.csv(bc_path, stringsAsFactors = FALSE)
  bc_sample <- breast_cancer[sample(nrow(breast_cancer), 150), ]
  
  # Create AutoML result
  result <- autoxplain(bc_sample, "diagnosis", max_models = 2, max_runtime_secs = 60)
  
  # Test dashboard generation without LLM
  temp_file <- tempfile(fileext = ".html")
  
  expect_no_error({
    output_path <- generate_dashboard(
      autoxplain_result = result,
      output_file = temp_file,
      top_features = 3,
      include_llm_report = FALSE,
      open_browser = FALSE
    )
  })
  
  # Check file was created
  expect_true(file.exists(temp_file))
  
  # Check return value
  expect_equal(output_path, temp_file)
  
  # Clean up
  unlink(temp_file)
})

test_that("dashboard generation input validation", {
  skip_if_not_installed("h2o")
  
  # Create minimal result
  bc_path <- system.file("extdata", "breast-cancer.csv", package = "AutoXplainR")
  breast_cancer <- read.csv(bc_path, stringsAsFactors = FALSE)
  bc_sample <- breast_cancer[1:50, ]
  result <- autoxplain(bc_sample, "diagnosis", max_models = 1, max_runtime_secs = 30)
  
  # Test invalid inputs
  expect_error(generate_dashboard(
    autoxplain_result = "not_an_autoxplain_result",
    output_file = "test.html"
  ))
  
  expect_error(generate_dashboard(
    autoxplain_result = result,
    output_file = 123  # Not a character
  ))
  
  expect_error(create_simple_dashboard(
    autoxplain_result = "not_an_autoxplain_result",
    output_file = "test.html"
  ))
})

test_that("create_simple_dashboard creates HTML file", {
  skip_if_not_installed("h2o")
  
  # Use wine quality dataset
  wine_path <- system.file("extdata", "winequality-red.csv", package = "AutoXplainR")
  wine <- read.csv(wine_path, stringsAsFactors = FALSE)
  wine_sample <- wine[sample(nrow(wine), 200), ]
  
  # Create AutoML result
  result <- autoxplain(wine_sample, "quality", max_models = 2, max_runtime_secs = 60)
  
  # Test simple dashboard
  temp_file <- tempfile(fileext = ".html")
  
  expect_no_error({
    output_path <- create_simple_dashboard(
      autoxplain_result = result,
      output_file = temp_file,
      top_features = 5
    )
  })
  
  # Check file was created
  expect_true(file.exists(temp_file))
  
  # Check content is HTML
  content <- readLines(temp_file, n = 10)
  expect_true(any(grepl("<!DOCTYPE html>", content, ignore.case = TRUE)))
  
  # Clean up
  unlink(temp_file)
})

test_that("dashboard handles missing data appropriately", {
  skip_if_not_installed("h2o")
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("flexdashboard")
  
  # Use adult income dataset with missing values
  adult_path <- system.file("extdata", "adult_income.csv", package = "AutoXplainR")
  adult <- read.csv(adult_path, stringsAsFactors = FALSE, na.strings = "?")
  adult_sample <- adult[sample(nrow(adult), 200), ]
  
  # Create AutoML result with preprocessing
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
  
  # Test dashboard generation
  temp_file <- tempfile(fileext = ".html")
  
  expect_no_error({
    output_path <- generate_dashboard(
      autoxplain_result = result,
      output_file = temp_file,
      top_features = 3,
      include_llm_report = FALSE,
      open_browser = FALSE
    )
  })
  
  expect_true(file.exists(temp_file))
  unlink(temp_file)
})

test_that("prepare_dashboard_data works correctly", {
  skip_if_not_installed("h2o")
  
  # Create minimal dataset
  bc_path <- system.file("extdata", "breast-cancer.csv", package = "AutoXplainR")
  breast_cancer <- read.csv(bc_path, stringsAsFactors = FALSE)
  bc_sample <- breast_cancer[1:50, ]
  
  result <- autoxplain(bc_sample, "diagnosis", max_models = 2, max_runtime_secs = 30)
  
  # Test data preparation
  dashboard_data <- AutoXplainR:::prepare_dashboard_data(result, top_features = 3)
  
  # Check structure
  expect_type(dashboard_data, "list")
  expect_true(all(c("importance", "importance_list", "pdp_data", "pdp_list",
                   "models_analyzed", "top_features", "feature_types",
                   "model_characteristics", "fitting_diagnostics",
                   "correlation_insights_html") %in% names(dashboard_data)))
  
  # Check data integrity
  expect_s3_class(dashboard_data$importance, "data.frame")
  expect_type(dashboard_data$importance_list, "list")
  expect_type(dashboard_data$pdp_list, "list")
  expect_length(dashboard_data$top_features, 3)
})

test_that("dashboard handles both classification and regression", {
  skip_if_not_installed("h2o")
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("flexdashboard")
  
  # Test classification
  pima_path <- system.file("extdata", "pima-diabetes.csv", package = "AutoXplainR")
  pima <- read.csv(pima_path, stringsAsFactors = FALSE)
  pima_sample <- pima[1:100, ]
  
  result_class <- autoxplain(pima_sample, "Outcome", max_models = 1, max_runtime_secs = 30)
  
  temp_file_class <- tempfile(fileext = ".html")
  expect_no_error({
    generate_dashboard(
      result_class, 
      temp_file_class, 
      top_features = 2,
      include_llm_report = FALSE,
      open_browser = FALSE
    )
  })
  expect_true(file.exists(temp_file_class))
  unlink(temp_file_class)
  
  # Test regression
  housing_path <- system.file("extdata", "california-housing.csv", package = "AutoXplainR")
  housing <- read.csv(housing_path, stringsAsFactors = FALSE)
  housing_sample <- housing[1:200, ]
  
  result_reg <- autoxplain(housing_sample, "median_house_value", max_models = 1, max_runtime_secs = 30)
  
  temp_file_reg <- tempfile(fileext = ".html")
  expect_no_error({
    generate_dashboard(
      result_reg, 
      temp_file_reg,
      top_features = 2,
      include_llm_report = FALSE,
      open_browser = FALSE
    )
  })
  expect_true(file.exists(temp_file_reg))
  unlink(temp_file_reg)
})

test_that("generate_dashboard handles LLM report generation", {
  skip_if_not_installed("h2o")
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("flexdashboard")
  skip_if(Sys.getenv("GEMINI_API_KEY") == "", "GEMINI_API_KEY not set")
  
  # Use smaller dataset for speed
  bc_path <- system.file("extdata", "breast-cancer.csv", package = "AutoXplainR")
  breast_cancer <- read.csv(bc_path, stringsAsFactors = FALSE)
  bc_sample <- breast_cancer[1:100, ]
  
  # Create AutoML result
  result <- autoxplain(bc_sample, "diagnosis", max_models = 1, max_runtime_secs = 30)
  
  # Test with LLM report
  temp_file <- tempfile(fileext = ".html")
  
  expect_no_error({
    output_path <- generate_dashboard(
      autoxplain_result = result,
      output_file = temp_file,
      top_features = 2,
      include_llm_report = TRUE,
      open_browser = FALSE
    )
  })
  
  # Check file was created
  expect_true(file.exists(temp_file))
  
  # Clean up
  unlink(temp_file)
})