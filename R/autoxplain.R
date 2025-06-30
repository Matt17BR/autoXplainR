#' AutoXplainR: Automated Machine Learning Explanation and Comparative Reporting
#'
#' @param data A data.frame containing the training data
#' @param target_column Character. Name of the target column
#' @param max_models Integer. Maximum number of models to train (default: 50)
#' @param max_runtime_secs Integer. Maximum runtime in seconds (default: 300)
#' @param seed Integer. Random seed for reproducibility (default: 123)
#' @param test_data Optional data.frame for testing. If NULL, uses train/validation split
#' @param enable_preprocessing Logical. Enable automatic data preprocessing for H2O compatibility (default: TRUE)
#' @param preprocessing_config List. Configuration for preprocessing steps (see details)
#' @return A list containing trained models, explanations, and preprocessing metadata
#' @export
#' @importFrom h2o h2o.init h2o.automl as.h2o h2o.getModel
#' @examples
#' \dontrun{
#' data(mtcars)
#' result <- autoxplain(mtcars, "mpg", max_models = 3)
#' }
autoxplain <- function(data, 
                      target_column, 
                      max_models = 50, 
                      max_runtime_secs = 300,
                      seed = 123,
                      test_data = NULL,
                      enable_preprocessing = TRUE,
                      preprocessing_config = list()) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data.frame")
  }
  
  if (!target_column %in% colnames(data)) {
    stop("target_column '", target_column, "' not found in data")
  }
  
  if (!is.numeric(max_models) || max_models < 1) {
    stop("max_models must be a positive integer")
  }
  
  if (!is.numeric(max_runtime_secs) || max_runtime_secs < 1) {
    stop("max_runtime_secs must be a positive integer")
  }
  
  # Initialize H2O
  tryCatch({
    h2o.init(nthreads = -1, max_mem_size = "4G")
  }, error = function(e) {
    message("H2O already initialized or initialization failed: ", e$message)
  })
  
  # Preprocess data for H2O compatibility
  preprocessing_result <- NULL
  if (enable_preprocessing) {
    # Set default preprocessing configuration
    default_config <- list(
      enable_target_handling = TRUE,
      enable_character_to_factors = TRUE,
      enable_ordered_factors = TRUE,
      enable_ordinal_factors = TRUE,
      enable_id_removal = TRUE,
      missing_value_strategy = "remove_rows"
    )
    
    # Merge user config with defaults
    final_config <- modifyList(default_config, preprocessing_config)
    
    # Apply preprocessing
    preprocessing_result <- preprocess_for_h2o(
      data = data,
      target_column = target_column,
      enable_target_handling = final_config$enable_target_handling,
      enable_character_to_factors = final_config$enable_character_to_factors,
      enable_ordered_factors = final_config$enable_ordered_factors,
      enable_ordinal_factors = final_config$enable_ordinal_factors,
      enable_id_removal = final_config$enable_id_removal,
      missing_value_strategy = final_config$missing_value_strategy
    )
    
    # Use preprocessed data
    data <- preprocessing_result$data
  }
  
  # Convert data to H2O format
  h2o_data <- as.h2o(data)
  
  # Prepare features and target
  features <- setdiff(colnames(data), target_column)
  
  # Handle test data
  h2o_test <- NULL
  test_preprocessing_result <- NULL
  if (!is.null(test_data)) {
    if (!is.data.frame(test_data)) {
      stop("test_data must be a data.frame")
    }
    if (!target_column %in% colnames(test_data)) {
      stop("target_column '", target_column, "' not found in test_data")
    }
    
    # Apply same preprocessing to test data
    if (enable_preprocessing) {
      test_preprocessing_result <- preprocess_for_h2o(
        data = test_data,
        target_column = target_column,
        enable_ordered_factors = final_config$enable_ordered_factors,
        enable_ordinal_factors = final_config$enable_ordinal_factors,
        enable_id_removal = final_config$enable_id_removal,
        missing_value_strategy = final_config$missing_value_strategy
      )
      test_data <- test_preprocessing_result$data
    }
    
    h2o_test <- as.h2o(test_data)
  }
  
  # Run AutoML with comprehensive algorithm exploration
  aml_result <- h2o.automl(
    x = features,
    y = target_column,
    training_frame = h2o_data,
    validation_frame = h2o_test,
    max_models = max_models,
    max_runtime_secs = max_runtime_secs,
    seed = seed,
    sort_metric = "AUTO",
    # Enable all available algorithms for maximum diversity
    exclude_algos = NULL,
    # Enable stacking with more base models
    max_after_balance_size = 5.0,
    # Allow more runtime for stacking
    max_runtime_secs_per_model = 60,
    # Include more cross-validation folds for better estimates
    nfolds = 5,
    # Enable automatic feature engineering
    preprocessing = list("target_encoding")
  )
  
  # Extract top models
  leaderboard <- aml_result@leaderboard
  model_ids <- as.vector(leaderboard$model_id)[1:min(max_models, nrow(leaderboard))]
  
  # Get model objects
  models <- lapply(model_ids, function(id) {
    tryCatch({
      h2o.getModel(id)
    }, error = function(e) {
      warning("Failed to retrieve model ", id, ": ", e$message)
      NULL
    })
  })
  
  # Remove NULL models
  models <- models[!sapply(models, is.null)]
  names(models) <- sapply(models, function(m) m@model_id)
  
  if (length(models) == 0) {
    stop("No models were successfully trained")
  }
  
  # Return results
  result <- list(
    models = models,
    leaderboard = leaderboard,
    training_data = data,
    test_data = test_data,
    target_column = target_column,
    features = features,
    automl_object = aml_result,
    preprocessing_metadata = list(
      enabled = enable_preprocessing,
      training_data = preprocessing_result,
      test_data = test_preprocessing_result
    )
  )
  
  class(result) <- "autoxplain_result"
  return(result)
}