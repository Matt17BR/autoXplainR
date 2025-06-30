#' Calculate Permutation Feature Importance
#'
#' Calculates feature importance by measuring the decrease in model performance
#' when individual features are randomly permuted.
#'
#' @param model An H2O model object
#' @param data A data.frame containing the test data
#' @param target_column Character. Name of the target column
#' @param metric Character. Metric to use for importance calculation ("rmse", "mae", "auc", "logloss", "accuracy")
#' @param n_repeats Integer. Number of permutation repeats for stability (default: 5)
#' @param seed Integer. Random seed for reproducibility (default: 123)
#' @return A data.frame with feature names and their importance scores
#' @export
#' @importFrom h2o as.h2o h2o.predict h2o.performance h2o.auc h2o.logloss
calculate_permutation_importance <- function(model, 
                                           data, 
                                           target_column, 
                                           metric = "auto",
                                           n_repeats = 5,
                                           seed = 123) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data.frame")
  }
  
  if (!target_column %in% colnames(data)) {
    stop("target_column '", target_column, "' not found in data")
  }
  
  if (!is.numeric(n_repeats) || n_repeats < 1) {
    stop("n_repeats must be a positive integer")
  }
  
  set.seed(seed)
  
  # Convert to H2O format
  h2o_data <- as.h2o(data)
  
  # Get features (excluding target)
  features <- setdiff(colnames(data), target_column)
  
  # Determine metric automatically if needed
  if (metric == "auto") {
    # Check if target is numeric (regression) or factor (classification)
    if (is.numeric(data[[target_column]])) {
      metric <- "rmse"
    } else {
      # Check number of unique values for classification
      n_classes <- length(unique(data[[target_column]]))
      if (n_classes == 2) {
        metric <- "auc"
      } else {
        metric <- "logloss"
      }
    }
  }
  
  # Calculate baseline performance
  baseline_score <- calculate_metric_score(model, h2o_data, target_column, metric)
  
  # Calculate importance for each feature
  importance_scores <- data.frame(
    feature = character(),
    importance = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (feature in features) {
    # Permute feature multiple times and average the result
    permuted_scores <- numeric(n_repeats)
    
    for (i in 1:n_repeats) {
      # Create copy of data
      permuted_data <- data
      
      # Permute the feature
      permuted_data[[feature]] <- sample(permuted_data[[feature]], 
                                        size = nrow(permuted_data), 
                                        replace = FALSE)
      
      # Convert to H2O and calculate performance
      h2o_permuted <- as.h2o(permuted_data)
      permuted_scores[i] <- calculate_metric_score(model, h2o_permuted, target_column, metric)
    }
    
    # Calculate importance as difference in performance
    # For metrics where lower is better (rmse, mae, logloss), importance is positive when performance degrades
    # For metrics where higher is better (auc, accuracy), importance is positive when performance improves
    avg_permuted_score <- mean(permuted_scores)
    
    if (metric %in% c("rmse", "mae", "logloss")) {
      importance <- avg_permuted_score - baseline_score  # Higher values indicate more important features
    } else {
      importance <- baseline_score - avg_permuted_score  # Higher values indicate more important features
    }
    
    importance_scores <- rbind(importance_scores, 
                              data.frame(feature = feature, 
                                        importance = importance,
                                        stringsAsFactors = FALSE))
  }
  
  # Sort by importance (descending)
  importance_scores <- importance_scores[order(importance_scores$importance, decreasing = TRUE), ]
  rownames(importance_scores) <- NULL
  
  # Add metadata
  attr(importance_scores, "metric") <- metric
  attr(importance_scores, "baseline_score") <- baseline_score
  attr(importance_scores, "n_repeats") <- n_repeats
  
  return(importance_scores)
}

#' Calculate metric score for a model and dataset
#'
#' @param model H2O model object
#' @param h2o_data H2O frame
#' @param target_column Character. Name of target column
#' @param metric Character. Metric to calculate
#' @return Numeric. The metric score
#' @keywords internal
calculate_metric_score <- function(model, h2o_data, target_column, metric) {
  
  # Get predictions
  predictions <- h2o.predict(model, h2o_data)
  
  # Extract actual values
  actual <- h2o_data[[target_column]]
  
  if (metric == "rmse") {
    # Root Mean Square Error
    pred_values <- as.vector(predictions$predict)
    actual_values <- as.vector(actual)
    return(sqrt(mean((actual_values - pred_values)^2)))
    
  } else if (metric == "mae") {
    # Mean Absolute Error
    pred_values <- as.vector(predictions$predict)
    actual_values <- as.vector(actual)
    return(mean(abs(actual_values - pred_values)))
    
  } else if (metric == "auc") {
    # Area Under Curve (binary classification)
    perf <- h2o.performance(model, h2o_data)
    return(as.numeric(h2o.auc(perf)))
    
  } else if (metric == "logloss") {
    # Log Loss (multi-class classification)
    perf <- h2o.performance(model, h2o_data)
    return(as.numeric(h2o.logloss(perf)))
    
  } else if (metric == "accuracy") {
    # Accuracy (classification)
    pred_values <- as.vector(predictions$predict)
    actual_values <- as.vector(actual)
    return(mean(actual_values == pred_values))
    
  } else {
    stop("Unsupported metric: ", metric)
  }
}