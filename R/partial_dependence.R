#' Calculate Partial Dependence Plot Data
#'
#' Generates data for partial dependence plots by calculating model predictions
#' across a range of values for a specific feature while marginalizing over all other features.
#'
#' @param model An H2O model object
#' @param data A data.frame containing the training/reference data
#' @param feature Character. Name of the feature to calculate PDP for
#' @param n_points Integer. Number of points to evaluate along the feature range (default: 50)
#' @param quantile_range Numeric vector of length 2. Quantile range to evaluate (default: c(0.05, 0.95))
#' @return A data.frame with feature values and corresponding partial dependence values
#' @export
#' @importFrom h2o as.h2o h2o.predict
calculate_partial_dependence <- function(model, 
                                       data, 
                                       feature, 
                                       n_points = 50,
                                       quantile_range = c(0.05, 0.95)) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data.frame")
  }
  
  if (!feature %in% colnames(data)) {
    stop("feature '", feature, "' not found in data")
  }
  
  if (!is.numeric(n_points) || n_points < 2) {
    stop("n_points must be a numeric value >= 2")
  }
  
  if (!is.numeric(quantile_range) || length(quantile_range) != 2 || 
      quantile_range[1] >= quantile_range[2] || 
      any(quantile_range < 0) || any(quantile_range > 1)) {
    stop("quantile_range must be a numeric vector of length 2 with values between 0 and 1, where first < second")
  }
  
  # Get feature values to evaluate
  feature_values <- get_feature_grid(data[[feature]], n_points, quantile_range)
  
  # Optimize: Use smaller sample for PDP calculation to improve speed
  max_sample_size <- 100  # Limit to 100 rows for PDP calculation
  if (nrow(data) > max_sample_size) {
    # Sample representative rows
    sample_indices <- sample(nrow(data), max_sample_size)
    pdp_data <- data[sample_indices, ]
  } else {
    pdp_data <- data
  }
  
  # Calculate partial dependence for each feature value
  pdp_values <- numeric(length(feature_values))
  
  for (i in seq_along(feature_values)) {
    # Create modified dataset where all instances have the same feature value
    modified_data <- pdp_data
    modified_data[[feature]] <- feature_values[i]
    
    # Convert to H2O and predict
    h2o_modified <- as.h2o(modified_data)
    predictions <- h2o.predict(model, h2o_modified)
    
    # Calculate average prediction (marginalizing over all other features)
    if (ncol(predictions) == 1) {
      # Regression: single prediction column
      avg_pred <- mean(as.vector(predictions$predict))
    } else {
      # Classification: multiple columns, use predicted probabilities or classes
      if ("predict" %in% colnames(predictions)) {
        # Use the main prediction column for classification
        if (is.numeric(as.vector(predictions$predict))) {
          # Numeric predictions (e.g., binary classification probabilities)
          avg_pred <- mean(as.vector(predictions$predict))
        } else {
          # Categorical predictions - convert to numeric representation
          pred_values <- as.vector(predictions$predict)
          avg_pred <- mean(as.numeric(as.factor(pred_values)))
        }
      } else {
        # Multi-class: use first probability column or handle appropriately
        prob_cols <- grep("^p[0-9]+", colnames(predictions))
        if (length(prob_cols) > 0) {
          avg_pred <- mean(as.vector(predictions[[prob_cols[1]]]))
        } else {
          stop("Unable to determine prediction values from model output")
        }
      }
    }
    
    pdp_values[i] <- avg_pred
  }
  
  # Create result data frame
  result <- data.frame(
    feature_value = feature_values,
    partial_dependence = pdp_values,
    stringsAsFactors = FALSE
  )
  
  names(result)[1] <- feature  # Name the first column after the feature
  
  # Add metadata
  attr(result, "feature") <- feature
  attr(result, "n_points") <- n_points
  attr(result, "quantile_range") <- quantile_range
  # Add data range (only for numeric features)
  if (is.numeric(data[[feature]])) {
    attr(result, "data_range") <- range(data[[feature]], na.rm = TRUE)
  } else {
    attr(result, "data_range") <- c(NA, NA)
  }
  
  return(result)
}

#' Calculate Partial Dependence for Multiple Features
#'
#' Convenience function to calculate partial dependence for multiple features at once.
#'
#' @param model An H2O model object
#' @param data A data.frame containing the training/reference data
#' @param features Character vector. Names of features to calculate PDP for
#' @param n_points Integer. Number of points to evaluate along each feature range (default: 50)
#' @param quantile_range Numeric vector of length 2. Quantile range to evaluate (default: c(0.05, 0.95))
#' @return A named list of data.frames, one for each feature
#' @export
calculate_partial_dependence_multi <- function(model, 
                                             data, 
                                             features, 
                                             n_points = 50,
                                             quantile_range = c(0.05, 0.95)) {
  
  if (!is.character(features) || length(features) == 0) {
    stop("features must be a non-empty character vector")
  }
  
  # Check all features exist
  missing_features <- setdiff(features, colnames(data))
  if (length(missing_features) > 0) {
    stop("Features not found in data: ", paste(missing_features, collapse = ", "))
  }
  
  # Calculate PDP for each feature
  pdp_results <- vector("list", length(features))
  names(pdp_results) <- features
  
  for (feature in features) {
    pdp_results[[feature]] <- calculate_partial_dependence(
      model = model,
      data = data,
      feature = feature,
      n_points = n_points,
      quantile_range = quantile_range
    )
  }
  
  return(pdp_results)
}

#' Generate feature grid for partial dependence calculation
#'
#' @param feature_data Numeric vector of feature values
#' @param n_points Integer. Number of points to generate
#' @param quantile_range Numeric vector of length 2. Quantile range
#' @return Numeric vector of feature values to evaluate
#' @keywords internal
get_feature_grid <- function(feature_data, n_points, quantile_range) {
  
  if (is.numeric(feature_data)) {
    # For numeric features, create evenly spaced grid within quantile range
    q_min <- quantile(feature_data, quantile_range[1], na.rm = TRUE)
    q_max <- quantile(feature_data, quantile_range[2], na.rm = TRUE)
    
    if (q_min == q_max) {
      # If all values are the same, just return that value
      return(rep(q_min, n_points))
    }
    
    return(seq(q_min, q_max, length.out = n_points))
    
  } else if (is.factor(feature_data) || is.character(feature_data)) {
    # For categorical features, use all unique values (up to n_points most frequent)
    value_counts <- table(feature_data)
    unique_values <- names(sort(value_counts, decreasing = TRUE))
    
    # Limit to n_points most frequent values
    if (length(unique_values) > n_points) {
      unique_values <- unique_values[1:n_points]
    }
    
    return(unique_values)
    
  } else {
    stop("Unsupported feature type: ", class(feature_data))
  }
}