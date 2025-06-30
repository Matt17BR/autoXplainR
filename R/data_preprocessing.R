# Convert ordered factors to appropriate types for H2O
#
# @param data A data.frame
# @param target_column Character. Name of target column to preserve
# @return List with processed data and conversion metadata
convert_ordered_factors <- function(data, target_column) {
  conversions <- list()
  
  for (col in names(data)) {
    if (col != target_column) {  # Preserve target column
      if (is.ordered(data[[col]])) {
        # Convert ordered factors to numeric, preserving order
        original_levels <- levels(data[[col]])
        data[[col]] <- as.numeric(data[[col]])
        conversions[[col]] <- list(
          type = "ordered_factor",
          original_levels = original_levels,
          action = "converted_to_numeric"
        )
        message("✓ Converted ordered factor '", col, "' to numeric (preserving order)")
      }
    }
  }
  
  return(list(data = data, conversions = conversions))
}

#' Handle target variable type conversion for H2O compatibility  
#'
#' @param data A data.frame
#' @param target_column Character. Name of target column
#' @return List with processed data and conversion metadata
handle_target_variable <- function(data, target_column) {
  conversions <- list()
  
  if (target_column %in% names(data)) {
    target_data <- data[[target_column]]
    
    if (is.character(target_data)) {
      # For character targets, convert to factor for classification
      unique_vals <- length(unique(target_data[!is.na(target_data)]))
      data[[target_column]] <- as.factor(target_data)
      conversions[[target_column]] <- list(
        type = "target_character_to_factor",
        unique_values = unique_vals,
        action = "converted_to_factor_for_classification"
      )
      message("✓ Converted target variable '", target_column, "' to factor for classification (", unique_vals, " levels)")
    }
  }
  
  return(list(data = data, conversions = conversions))
}

#' Convert character variables to factors for H2O compatibility
#'
#' @param data A data.frame
#' @param target_column Character. Name of target column to preserve
#' @return List with processed data and conversion metadata
convert_character_to_factors <- function(data, target_column) {
  conversions <- list()
  
  for (col in names(data)) {
    if (col != target_column && is.character(data[[col]])) {
      # Convert character variables to factors so H2O can use them
      unique_vals <- length(unique(data[[col]][!is.na(data[[col]])]))
      data[[col]] <- as.factor(data[[col]])
      conversions[[col]] <- list(
        type = "character_to_factor",
        unique_values = unique_vals,
        action = "converted_to_factor"
      )
      message("✓ Converted character variable '", col, "' to factor (", unique_vals, " levels)")
    }
  }
  
  return(list(data = data, conversions = conversions))
}

#' Convert regular factors that appear to be ordinal to numeric
#'
#' @param data A data.frame
#' @param target_column Character. Name of target column to preserve
#' @return List with processed data and conversion metadata
convert_ordinal_factors <- function(data, target_column) {
  conversions <- list()
  
  for (col in names(data)) {
    if (col != target_column && is.factor(data[[col]])) {
      levels_vals <- levels(data[[col]])
      
      # Check if factor levels are numeric strings (like "1", "2", "3"...)
      if (all(grepl("^[0-9]+$", levels_vals))) {
        original_levels <- levels_vals
        data[[col]] <- as.numeric(as.character(data[[col]]))
        conversions[[col]] <- list(
          type = "ordinal_factor",
          original_levels = original_levels,
          action = "converted_to_numeric"
        )
        message("✓ Converted ordinal factor '", col, "' to numeric")
      }
    }
  }
  
  return(list(data = data, conversions = conversions))
}

#' Remove problematic columns (like ID columns)
#'
#' @param data A data.frame
#' @param target_column Character. Name of target column to preserve
#' @return List with processed data and removal metadata
remove_id_columns <- function(data, target_column) {
  removals <- list()
  cols_to_remove <- c()
  
  for (col in names(data)) {
    # Check for ID-like columns
    if (col != target_column && 
        (tolower(col) %in% c("id", "identifier", "row_id", "index") ||
         grepl("^id$|^.*_id$|^id_.*$", tolower(col)) ||
         # Check if column has unique values for every row (likely an ID)
         (is.character(data[[col]]) && length(unique(data[[col]])) == nrow(data)))) {
      
      cols_to_remove <- c(cols_to_remove, col)
      removals[[col]] <- list(
        type = "id_column",
        reason = "appears_to_be_identifier",
        action = "removed"
      )
      message("✓ Removed ID-like column '", col, "'")
    }
  }
  
  if (length(cols_to_remove) > 0) {
    data <- data[, !names(data) %in% cols_to_remove, drop = FALSE]
  }
  
  return(list(data = data, removals = removals))
}

#' Handle missing values
#'
#' @param data A data.frame
#' @param target_column Character. Name of target column
#' @param strategy Character. How to handle missing values ("remove_rows", "remove_columns", "impute")
#' @return List with processed data and missing value metadata
handle_missing_values <- function(data, target_column, strategy = "remove_rows") {
  missing_info <- list()
  
  # Check for missing values
  missing_counts <- sapply(data, function(x) sum(is.na(x)))
  cols_with_missing <- names(missing_counts[missing_counts > 0])
  
  if (length(cols_with_missing) > 0) {
    missing_info$original_missing <- missing_counts[cols_with_missing]
    
    if (strategy == "remove_rows") {
      original_nrow <- nrow(data)
      data <- data[complete.cases(data), ]
      removed_rows <- original_nrow - nrow(data)
      
      if (removed_rows > 0) {
        missing_info$action <- "removed_incomplete_rows"
        missing_info$rows_removed <- removed_rows
        message("✓ Removed ", removed_rows, " rows with missing values")
      }
    } else if (strategy == "remove_columns") {
      # Remove columns with high percentage of missing values (>50%)
      high_missing_cols <- names(missing_counts[missing_counts > nrow(data) * 0.5])
      if (length(high_missing_cols) > 0 && !target_column %in% high_missing_cols) {
        data <- data[, !names(data) %in% high_missing_cols, drop = FALSE]
        missing_info$action <- "removed_high_missing_columns"
        missing_info$columns_removed <- high_missing_cols
        message("✓ Removed ", length(high_missing_cols), " columns with >50% missing values")
      }
    }
  }
  
  return(list(data = data, missing_info = missing_info))
}

#' Main preprocessing function that orchestrates all preprocessing steps
#'
#' @param data A data.frame to preprocess
#' @param target_column Character. Name of the target column
#' @param enable_target_handling Logical. Handle target variable type conversion (default: TRUE)
#' @param enable_character_to_factors Logical. Convert character variables to factors (default: TRUE)
#' @param enable_ordered_factors Logical. Convert ordered factors to numeric (default: TRUE)
#' @param enable_ordinal_factors Logical. Convert ordinal factors to numeric (default: TRUE)
#' @param enable_id_removal Logical. Remove ID-like columns (default: TRUE)
#' @param missing_value_strategy Character. How to handle missing values (default: "remove_rows")
#' @return List with preprocessed data and comprehensive metadata
#' @export
preprocess_for_h2o <- function(data, 
                               target_column,
                               enable_target_handling = TRUE,
                               enable_character_to_factors = TRUE,
                               enable_ordered_factors = TRUE,
                               enable_ordinal_factors = TRUE, 
                               enable_id_removal = TRUE,
                               missing_value_strategy = "remove_rows") {
  
  message("🔄 Starting data preprocessing for H2O compatibility...")
  
  # Store original data info
  original_info <- list(
    nrows = nrow(data),
    ncols = ncol(data),
    column_names = names(data),
    column_types = sapply(data, class)
  )
  
  preprocessing_log <- list()
  
  # Step 1: Remove ID columns
  if (enable_id_removal) {
    id_result <- remove_id_columns(data, target_column)
    data <- id_result$data
    preprocessing_log$id_removal <- id_result$removals
  }
  
  # Step 2: Handle missing values
  missing_result <- handle_missing_values(data, target_column, missing_value_strategy)
  data <- missing_result$data
  preprocessing_log$missing_values <- missing_result$missing_info
  
  # Step 3: Handle target variable type conversion
  if (enable_target_handling) {
    target_result <- handle_target_variable(data, target_column)
    data <- target_result$data
    preprocessing_log$target_handling <- target_result$conversions
  }
  
  # Step 4: Convert character variables to factors
  if (enable_character_to_factors) {
    character_result <- convert_character_to_factors(data, target_column)
    data <- character_result$data
    preprocessing_log$character_to_factors <- character_result$conversions
  }
  
  # Step 5: Convert ordered factors
  if (enable_ordered_factors) {
    ordered_result <- convert_ordered_factors(data, target_column)
    data <- ordered_result$data
    preprocessing_log$ordered_factors <- ordered_result$conversions
  }
  
  # Step 6: Convert ordinal factors
  if (enable_ordinal_factors) {
    ordinal_result <- convert_ordinal_factors(data, target_column)
    data <- ordinal_result$data
    preprocessing_log$ordinal_factors <- ordinal_result$conversions
  }
  
  # Final validation
  final_info <- list(
    nrows = nrow(data),
    ncols = ncol(data),
    column_names = names(data),
    column_types = sapply(data, class)
  )
  
  message("✅ Preprocessing complete! Dataset: ", nrow(data), " rows × ", ncol(data), " columns")
  
  return(list(
    data = data,
    preprocessing_log = preprocessing_log,
    original_info = original_info,
    final_info = final_info
  ))
}