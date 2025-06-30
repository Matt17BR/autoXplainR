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
#' @param return_all_classes Logical. For classification, return probabilities for all classes (default: FALSE)
#' @return A data.frame with feature values and partial dependence values (single column for regression, multiple columns for multi-class when return_all_classes=TRUE)
#' @export
#' @importFrom h2o as.h2o h2o.predict
calculate_partial_dependence <- function(model, 
                                       data, 
                                       feature, 
                                       n_points = 50,
                                       quantile_range = c(0.05, 0.95),
                                       return_all_classes = FALSE) {
  
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
  pdp_values <- if (return_all_classes) {
    list()  # Will store class probabilities for each feature value
  } else {
    numeric(length(feature_values))  # Single value per feature value
  }
  
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
      # Classification: use predicted probabilities
      # H2O returns columns: "predict" (class), and one column per class with probabilities
      prob_cols <- setdiff(colnames(predictions), "predict")
      
      if (length(prob_cols) > 0) {
        if (return_all_classes) {
          # Return probabilities for all classes
          class_probs <- sapply(prob_cols, function(class_name) {
            mean(as.vector(predictions[[class_name]]))
          })
          avg_pred <- class_probs
        } else {
          # Original behavior - return single class probability
          if (length(prob_cols) == 2) {
            # Binary classification - use last column (typically positive class)
            positive_class_col <- prob_cols[length(prob_cols)]
            avg_pred <- mean(as.vector(predictions[[positive_class_col]]))
          } else {
            # Multi-class - average probability of first class
            avg_pred <- mean(as.vector(predictions[[prob_cols[1]]]))
          }
        }
      } else {
        stop("Unable to find probability predictions in model output. Expected probability columns in H2O predictions.")
      }
    }
    
    if (return_all_classes && is.numeric(avg_pred) && length(avg_pred) > 1) {
      # Store class probabilities for each feature value
      pdp_values[[i]] <- avg_pred
    } else {
      # Store single value
      pdp_values[i] <- avg_pred
    }
  }
  
  # Create result data frame
  if (return_all_classes && is.list(pdp_values)) {
    # Multi-class case: create columns for each class
    class_names <- names(pdp_values[[1]])
    result <- data.frame(feature_value = feature_values, stringsAsFactors = FALSE)
    names(result)[1] <- feature
    
    # Add a column for each class
    for (class_name in class_names) {
      result[[paste0("prob_", class_name)]] <- sapply(pdp_values, function(x) x[class_name])
    }
  } else {
    # Single value case (regression or single class)
    result <- data.frame(
      feature_value = feature_values,
      partial_dependence = pdp_values,
      stringsAsFactors = FALSE
    )
    names(result)[1] <- feature  # Name the first column after the feature
  }
  
  # Add metadata
  attr(result, "feature") <- feature
  attr(result, "n_points") <- n_points
  attr(result, "quantile_range") <- quantile_range
  attr(result, "return_all_classes") <- return_all_classes
  if (return_all_classes && is.list(pdp_values)) {
    attr(result, "class_names") <- names(pdp_values[[1]])
  }
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
#' @param return_all_classes Logical. For classification, return probabilities for all classes (default: FALSE)
#' @return A named list of data.frames, one for each feature
#' @export
calculate_partial_dependence_multi <- function(model, 
                                             data, 
                                             features, 
                                             n_points = 50,
                                             quantile_range = c(0.05, 0.95),
                                             return_all_classes = FALSE) {
  
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
      quantile_range = quantile_range,
      return_all_classes = return_all_classes
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