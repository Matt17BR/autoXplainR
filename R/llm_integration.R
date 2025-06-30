#' Generate Natural Language Report using Google Generative AI
#'
#' Creates a natural language summary of AutoML results and explanations using
#' Google's Gemini API with the gemma-3-27b-it model.
#'
#' @param autoxplain_result Object from autoxplain() function
#' @param importance_data Data.frame from calculate_permutation_importance() (optional)
#' @param pdp_data List from calculate_partial_dependence_multi() (optional)
#' @param model_characteristics List from extract_model_characteristics() (optional)
#' @param api_key Character. Google Generative AI API key. If NULL, uses GEMINI_API_KEY environment variable
#' @param model Character. Model to use (default: "gemma-3-27b-it")
#' @param max_tokens Integer. Maximum tokens in response (default: 1000)
#' @param temperature Numeric. Sampling temperature (default: 0.3)
#' @return Character. Generated natural language report
#' @export
#' @importFrom httr POST content add_headers timeout
#' @importFrom jsonlite toJSON fromJSON
generate_natural_language_report <- function(autoxplain_result,
                                            importance_data = NULL,
                                            pdp_data = NULL,
                                            model_characteristics = NULL,
                                            api_key = NULL,
                                            model = "gemma-3-27b-it",
                                            max_tokens = 1500,
                                            temperature = 0.3) {
  
  # Input validation
  if (!inherits(autoxplain_result, "autoxplain_result")) {
    stop("autoxplain_result must be an object from autoxplain() function")
  }
  
  # Get API key
  if (is.null(api_key)) {
    api_key <- Sys.getenv("GEMINI_API_KEY")
    if (api_key == "") {
      stop("Google Generative AI API key not found. Please set GEMINI_API_KEY environment variable or provide api_key parameter.")
    }
  }
  
  # Prepare context summary
  context <- prepare_analysis_context(autoxplain_result, importance_data, pdp_data, model_characteristics)
  
  # Create prompt
  prompt <- create_report_prompt(context)
  
  # Call Google Generative AI API
  tryCatch({
    response <- call_gemini_api(prompt, api_key, model, max_tokens, temperature)
    return(response)
  }, error = function(e) {
    warning("Failed to generate natural language report: ", e$message)
    return(create_fallback_report(context))
  })
}

#' Prepare analysis context summary
#'
#' @param autoxplain_result AutoML results
#' @param importance_data Feature importance data
#' @param pdp_data Partial dependence data
#' @return List with context information
#' @keywords internal
prepare_analysis_context <- function(autoxplain_result, importance_data, pdp_data, model_characteristics = NULL) {
  
  # Extract leaderboard data safely
  leaderboard <- tryCatch({
    as.data.frame(autoxplain_result$leaderboard)
  }, error = function(e) {
    # Create minimal leaderboard if H2O data not available
    data.frame(
      model_id = names(autoxplain_result$models),
      stringsAsFactors = FALSE
    )
  })
  
  target_column <- autoxplain_result$target_column
  n_features <- length(autoxplain_result$features)
  n_models <- length(autoxplain_result$models)
  
  # Determine task type
  sample_data <- autoxplain_result$training_data
  is_classification <- !is.numeric(sample_data[[target_column]])
  
  # Best model info
  best_model_id <- as.vector(leaderboard$model_id)[1]
  best_model_type <- extract_model_type_simple(best_model_id)
  
  # Do not pass specific performance metrics to avoid hallucination
  best_performance <- NULL
  best_metric <- NULL
  
  # Feature importance summary
  importance_summary <- NULL
  if (!is.null(importance_data)) {
    top_features <- head(importance_data$feature, 3)
    importance_summary <- list(
      top_features = top_features,
      metric = attr(importance_data, "metric") %||% "unknown"
    )
  }
  
  # PDP summary
  pdp_summary <- NULL
  if (!is.null(pdp_data) && length(pdp_data) > 0) {
    pdp_features <- names(pdp_data)
    pdp_summary <- list(
      features_analyzed = pdp_features,
      n_features = length(pdp_features)
    )
  }
  
  
  # Model characteristics summary
  model_char_summary <- NULL
  if (!is.null(model_characteristics)) {
    summary_info <- attr(model_characteristics, "summary")
    if (!is.null(summary_info)) {
      model_char_summary <- list(
        total_models = summary_info$total_models,
        algorithms_used = summary_info$algorithms_used,
        total_training_time_s = summary_info$total_training_time_s,
        models_details = head(model_characteristics, 3)  # Top 3 models
      )
    }
  }
  
  return(list(
    task_type = if (is_classification) "classification" else "regression",
    target_column = target_column,
    n_features = n_features,
    n_models = n_models,
    best_model_type = best_model_type,
    best_performance = best_performance,
    best_metric = best_metric %||% "performance",
    importance_summary = importance_summary,
    pdp_summary = pdp_summary,
    model_characteristics = model_char_summary
  ))
}

#' Create prompt for natural language report generation
#'
#' @param context List with analysis context
#' @return Character. Formatted prompt
#' @keywords internal
create_report_prompt <- function(context) {
  
  prompt <- paste0(
    "You are an AI assistant helping to explain machine learning model results. ",
    "Create a clear, educational summary that helps people understand the analysis.\n\n",
    "CRITICAL INSTRUCTIONS:\n",
    "- NEVER make up numbers, metrics, or statistics that are not explicitly provided below\n",
    "- If a specific number is not given, do NOT invent one - describe concepts instead\n",
    "- Focus on explaining WHAT the different model types are and HOW they work\n",
    "- Explain hyperparameters in practical terms (e.g., 'number of decision trees', 'how deep each tree goes')\n",
    "- Emphasize understanding the methods rather than citing performance figures\n",
    "- Only use numbers that are explicitly listed in the data below\n",
    "- Use clear markdown formatting for better readability (headers, bold, italic, code formatting)\n\n",
    
    "## Analysis Overview\n",
    "- **Task Type**: ", context$task_type, "\n",
    "- **Target Variable**: ", context$target_column, "\n",
    "- **Number of Features**: ", context$n_features, "\n",
    "- **Number of Models Trained**: ", context$n_models, "\n",
    "- **Best Model Type**: ", context$best_model_type, "\n"
  )
  
  if (!is.null(context$best_performance)) {
    # Add metric explanation
    metric_explanation <- switch(tolower(context$best_metric),
      "rmse" = "Root Mean Square Error - measures average prediction error; lower is better",
      "mae" = "Mean Absolute Error - average absolute difference between predictions and actual values; lower is better", 
      "r2" = "R-squared - proportion of variance explained by the model; higher is better (0-1 scale)",
      "mse" = "Mean Square Error - average squared prediction error; lower is better",
      "auc" = "Area Under Curve - model's ability to distinguish between classes; higher is better (0-1 scale)",
      "logloss" = "Logarithmic Loss - penalizes confident wrong predictions; lower is better",
      "mean_per_class_error" = "Classification Error Rate - percentage of incorrect predictions; lower is better",
      "accuracy" = "Accuracy - percentage of correct predictions; higher is better",
      paste("Performance metric:", context$best_metric)
    )
    
    prompt <- paste0(prompt, 
      "- **Best Model Performance**: ", round(context$best_performance, 4), 
      " (", context$best_metric, " - ", metric_explanation, ")\n"
    )
  } else {
    prompt <- paste0(prompt, "- **Performance**: Results available but specific numbers not provided\n")
  }
  
  if (!is.null(context$importance_summary)) {
    prompt <- paste0(prompt, "\n## Feature Importance Analysis\n",
      "The most important features for prediction are: ",
      paste(context$importance_summary$top_features, collapse = ", "), ".\n",
      "Feature importance was calculated using: ", context$importance_summary$metric, ".\n"
    )
  }
  
  if (!is.null(context$pdp_summary)) {
    prompt <- paste0(prompt, "\n## Partial Dependence Analysis\n",
      "Partial dependence plots were generated for ", context$pdp_summary$n_features, " features: ",
      paste(context$pdp_summary$features_analyzed, collapse = ", "), ".\n"
    )
  }
  
  
  if (!is.null(context$model_characteristics)) {
    char <- context$model_characteristics
    
    # Only include reasonable total training times
    training_time_text <- ""
    if (!is.null(char$total_training_time_s)) {
      total_time <- as.numeric(char$total_training_time_s)
      if (!is.na(total_time) && total_time > 0 && total_time < 7200) {  # Less than 2 hours
        training_time_text <- paste0("Total Training Time: ", round(total_time, 2), " seconds\n")
      }
    }
    
    prompt <- paste0(prompt, "\n## Model Characteristics & Hyperparameters\n",
      training_time_text,
      "Algorithms Used: ", paste(char$algorithms_used, collapse = ", "), "\n\n"
    )
    
    # Add detailed model info for top models
    if (!is.null(char$models_details) && length(char$models_details) > 0) {
      prompt <- paste0(prompt, "**Top Model Details:**\n")
      for (i in 1:min(3, length(char$models_details))) {
        model <- char$models_details[[i]]
        prompt <- paste0(prompt, "Model ", i, " (", model$algorithm, "):\n")
        
        if (!is.null(model$training_time_s)) {
          # Only include reasonable training times (between 0.001 and 3600 seconds)
          time_val <- as.numeric(model$training_time_s)
          if (!is.na(time_val) && time_val > 0.001 && time_val < 3600) {
            prompt <- paste0(prompt, "- Training time: ", round(time_val, 2), " seconds\n")
          }
        }
        
        if (!is.null(model$hyperparameters)) {
          prompt <- paste0(prompt, "- Key hyperparameters: ")
          hyperparams <- head(names(model$hyperparameters), 3)
          for (param in hyperparams) {
            value <- model$hyperparameters[[param]]
            if (nchar(as.character(value)) > 20) {
              value <- paste0(substr(as.character(value), 1, 17), "...")
            }
            prompt <- paste0(prompt, param, " = ", value, "; ")
          }
          prompt <- paste0(prompt, "\n")
        }
        
        # Do not include specific performance metrics to prevent number hallucination
        prompt <- paste0(prompt, "\n")
      }
    }
  }
  
  prompt <- paste0(prompt, 
    "\n## Task\n",
    "Create an educational summary that explains how the machine learning analysis works and what it found. ",
    "**ABSOLUTELY CRITICAL: Do NOT make up any numbers. If you don't see a specific metric value above, don't mention numbers for it.**\n\n",
    "Structure your response like this:\n",
    "1. **Understanding the Best Performing Models**: Explain what the different algorithm types are (like Random Forest, Gradient Boosting, etc.) and what their hyperparameters mean in simple terms\n",
    "2. **Model Performance Interpretation**: If performance metrics are provided above, explain what they mean in practical terms (e.g., 'An RMSE of X means the model's predictions are typically off by X units', 'An AUC of Y means the model correctly distinguishes between classes Y% of the time')\n",
    "3. **What Drives the Predictions**: Explain the most important features and what they mean practically\n",
    "4. **How the Models Work**: Describe the algorithms in everyday language (e.g., 'Random Forest is like asking many experts and averaging their opinions')\n",
    "5. **Practical Insights**: What this analysis tells us for decision-making and how good the model performance is for real-world use\n",
    "6. **Reliability & Limitations**: General guidance about model reliability and when to trust the predictions\n\n",
    "**Focus on education and understanding. When performance metrics are provided, explain what they mean for practical use of the model. Only mention specific values if they are explicitly provided in the data above.**\n\n",
    "Use a helpful, educational tone that explains concepts clearly. Use markdown formatting like `backticks` for technical terms, **bold** for emphasis, and proper headers."
  )
  
  return(prompt)
}

#' Call Google Generative AI API
#'
#' @param prompt Character. Input prompt
#' @param api_key Character. API key
#' @param model Character. Model name
#' @param max_tokens Integer. Maximum tokens
#' @param temperature Numeric. Temperature parameter
#' @return Character. Generated text
#' @keywords internal
call_gemini_api <- function(prompt, api_key, model, max_tokens, temperature) {
  
  # Prepare API endpoint
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model, ":generateContent?key=", api_key)
  
  # Prepare request body
  request_body <- list(
    contents = list(
      list(
        parts = list(
          list(text = prompt)
        )
      )
    ),
    generationConfig = list(
      temperature = temperature,
      maxOutputTokens = max_tokens,
      topP = 0.8,
      topK = 10
    )
  )
  
  # Make API call
  response <- POST(
    url = url,
    body = toJSON(request_body, auto_unbox = TRUE),
    add_headers(`Content-Type` = "application/json"),
    timeout(30)
  )
  
  # Check response status
  if (response$status_code != 200) {
    stop("API request failed with status code: ", response$status_code, 
         ". Response: ", content(response, "text"))
  }
  
  # Parse response
  response_content <- content(response, "text", encoding = "UTF-8")
  response_json <- fromJSON(response_content)
  
  # Extract generated text with robust error handling for data.frame structure
  tryCatch({
    if (!is.null(response_json$candidates) && 
        length(response_json$candidates) > 0) {
      
      # Handle data.frame structure (what the API actually returns)
      if (is.data.frame(response_json$candidates)) {
        candidate <- response_json$candidates[1, ]
        
        # Navigate the nested data.frame structure
        if (!is.null(candidate$content) && is.data.frame(candidate$content)) {
          content_row <- candidate$content[1, ]
          if (!is.null(content_row$parts) && is.list(content_row$parts)) {
            parts_list <- content_row$parts[[1]]
            if (is.data.frame(parts_list) && !is.null(parts_list$text)) {
              return(as.character(parts_list$text[1]))
            }
          }
        }
      } else {
        # Handle list structure (fallback)
        candidate <- response_json$candidates[[1]]
        
        # Check for different response structures
        if (!is.null(candidate$content$parts[[1]]$text)) {
          return(candidate$content$parts[[1]]$text)
        } else if (!is.null(candidate$content) && !is.null(candidate$content$text)) {
          return(candidate$content$text)
        } else if (!is.null(candidate$text)) {
          return(candidate$text)
        }
      }
    }
    
    # Check for error messages in response
    if (!is.null(response_json$error)) {
      stop("API error: ", response_json$error$message)
    }
    
    stop("Unable to extract text from API response structure")
    
  }, error = function(e) {
    # Enhanced error message with response details
    stop("Failed to parse API response: ", e$message, 
         ". Raw response: ", substr(response_content, 1, 500))
  })
}

#' Create fallback report when API fails
#'
#' @param context List with analysis context
#' @return Character. Fallback report
#' @keywords internal
create_fallback_report <- function(context) {
  
  report <- paste0(
    "# AutoML Analysis Report\n\n",
    "## Executive Summary\n",
    "An automated machine learning analysis was performed on the dataset targeting the '", 
    context$target_column, "' variable for ", context$task_type, ".\n\n",
    
    "## Model Performance\n",
    "- **Models Trained**: ", context$n_models, " different algorithms were evaluated\n",
    "- **Best Model**: ", context$best_model_type, "\n"
  )
  
  if (!is.null(context$best_performance)) {
    report <- paste0(report,
      "- **Performance**: ", round(context$best_performance, 4), " (", context$best_metric, ")\n"
    )
  }
  
  if (!is.null(context$importance_summary)) {
    report <- paste0(report, "\n## Key Findings\n",
      "The analysis identified the most influential features for prediction: ",
      paste(context$importance_summary$top_features, collapse = ", "), ".\n"
    )
  }
  
  report <- paste0(report, "\n## Technical Details\n",
    "- **Features Used**: ", context$n_features, " input variables\n",
    "- **Analysis Type**: ", stringr::str_to_title(context$task_type), " modeling\n"
  )
  
  if (!is.null(context$pdp_summary)) {
    report <- paste0(report,
      "- **Partial Dependence**: Analyzed ", context$pdp_summary$n_features, " key features\n"
    )
  }
  
  
  report <- paste0(report, 
    "\n## Recommendations\n",
    "The model shows promise for ", context$task_type, " tasks. ",
    "Further validation and domain expert review are recommended before deployment.\n"
  )
  
  return(report)
}

#' Extract simplified model type from H2O model ID
#'
#' @param model_id Character. H2O model ID
#' @return Character. Simplified model type
#' @keywords internal
extract_model_type_simple <- function(model_id) {
  if (grepl("GBM", model_id, ignore.case = TRUE)) {
    return("Gradient Boosting")
  } else if (grepl("RandomForest|DRF", model_id, ignore.case = TRUE)) {
    return("Random Forest")
  } else if (grepl("GLM", model_id, ignore.case = TRUE)) {
    return("Linear Model")
  } else if (grepl("DeepLearning", model_id, ignore.case = TRUE)) {
    return("Neural Network")
  } else if (grepl("XGBoost", model_id, ignore.case = TRUE)) {
    return("XGBoost")
  } else if (grepl("StackedEnsemble", model_id, ignore.case = TRUE)) {
    return("Ensemble Model")
  } else {
    return("Advanced Model")
  }
}