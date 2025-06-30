#' Extract Detailed Model Characteristics
#'
#' Provides comprehensive model analysis including hyperparameters, training times,
#' model complexity metrics, and algorithm-specific details for AutoML comparison.
#'
#' @param autoxplain_result Object from autoxplain() function
#' @param include_hyperparams Logical. Whether to extract hyperparameters (default: TRUE)
#' @param include_performance Logical. Whether to extract performance metrics (default: TRUE)
#' @param include_varimp Logical. Whether to extract variable importance (default: TRUE)
#' @return List with detailed model characteristics for each model
#' @export
#' @importFrom h2o h2o.varimp
#' @examples
#' \dontrun{
#' result <- autoxplain(iris, "Species", max_models = 3)
#' model_details <- extract_model_characteristics(result)
#' print(model_details)
#' }
extract_model_characteristics <- function(autoxplain_result, 
                                        include_hyperparams = TRUE,
                                        include_performance = TRUE,
                                        include_varimp = TRUE) {
  
  # Input validation
  if (!inherits(autoxplain_result, "autoxplain_result")) {
    stop("autoxplain_result must be an object from autoxplain() function")
  }
  
  models <- autoxplain_result$models
  if (length(models) == 0) {
    stop("No models found in autoxplain_result")
  }
  
  model_characteristics <- list()
  
  for (i in seq_along(models)) {
    model <- models[[i]]
    
    # Basic model information
    model_info <- list(
      rank = i,
      model_id = model@model_id,
      algorithm = model@algorithm
    )
    
    # Training time and resources
    if (!is.null(model@model$run_time)) {
      model_info$training_time_ms <- model@model$run_time
      model_info$training_time_s <- model@model$run_time / 1000
    }
    
    # Model size and complexity
    if (!is.null(model@model$output) && !is.null(model@model$output$model_size_in_bytes)) {
      model_info$size_bytes <- model@model$output$model_size_in_bytes
      model_info$size_mb <- model@model$output$model_size_in_bytes / (1024^2)
    }
    
    # Hyperparameters (algorithm-specific)
    if (include_hyperparams && !is.null(model@allparameters)) {
      params <- model@allparameters
      
      # Extract algorithm-specific important parameters
      important_params <- switch(model@algorithm,
        "glm" = c("alpha", "lambda", "family", "solver", "standardize"),
        "gbm" = c("ntrees", "max_depth", "learn_rate", "sample_rate", "col_sample_rate", "min_rows"),
        "drf" = c("ntrees", "max_depth", "sample_rate", "mtries", "min_rows"),
        "xgboost" = c("ntrees", "max_depth", "learn_rate", "sample_rate", "reg_alpha", "reg_lambda", "subsample", "colsample_bytree"),
        "deeplearning" = c("hidden", "epochs", "activation", "input_dropout_ratio", "l1", "l2"),
        "stackedensemble" = c("base_models", "metalearner_algorithm"),
        names(params)  # fallback: all params
      )
      
      extracted_params <- list()
      for (param in important_params) {
        if (param %in% names(params) && !is.null(params[[param]])) {
          value <- params[[param]]
          
          # Special handling for base_models parameter (H2O Key objects)
          if (param == "base_models" && is.list(value)) {
            # Extract model IDs from H2O Key objects, filtering out "Key" objects
            model_ids <- character()
            for (item in value) {
              if (is.list(item)) {
                # Try to find the model ID in the list structure
                # Look for character strings that look like model IDs
                for (element in item) {
                  if (is.character(element) && length(element) == 1 && 
                      nchar(element) > 10 && grepl("AutoML", element) &&
                      !grepl("^Key", element)) {  # Filter out "Key" objects
                    model_ids <- c(model_ids, element)
                    break
                  }
                }
              } else if (is.character(item) && length(item) == 1 && 
                         nchar(item) > 10 && grepl("AutoML", item) &&
                         !grepl("^Key", item)) {  # Filter out "Key" objects
                # Direct character element
                model_ids <- c(model_ids, item)
              }
            }
            if (length(model_ids) > 0) {
              # Clean up model IDs for better readability
              clean_ids <- sapply(model_ids, function(id) {
                # Extract algorithm and short identifier
                if (grepl("GLM", id)) {
                  return("GLM")
                } else if (grepl("XGBoost", id)) {
                  return("XGBoost")
                } else if (grepl("GBM", id)) {
                  return("GBM")
                } else if (grepl("DRF", id)) {
                  return("RandomForest")
                } else if (grepl("DeepLearning", id)) {
                  return("DeepLearning")
                } else {
                  # Return first part before underscore
                  parts <- strsplit(id, "_")[[1]]
                  return(parts[1])
                }
              })
              value <- paste(clean_ids, collapse = ", ")
            } else {
              value <- paste(length(value), "base models")
            }
          } else if (is.list(value) && length(value) > 0) {
            # For other list parameters, handle more carefully
            # Check if it's a simple list of values
            if (all(sapply(value, function(x) is.atomic(x) && length(x) == 1))) {
              value <- paste(unlist(value), collapse = ", ")
            } else {
              # For complex nested lists, provide a summary
              value <- paste("Complex parameter with", length(value), "elements")
            }
          }
          
          # Handle multiple values
          if (length(value) > 1) {
            value <- paste(value, collapse = ", ")
          }
          
          # Don't truncate here - let the dashboard handle display formatting
          # This preserves all data for the dashboard's intelligent expandable display
          value_str <- as.character(value)
          
          extracted_params[[param]] <- value
        }
      }
      model_info$hyperparameters <- extracted_params
    }
    
    # Performance metrics
    if (include_performance && !is.null(model@model$training_metrics)) {
      metrics <- model@model$training_metrics
      if (!is.null(metrics@metrics)) {
        performance_metrics <- list()
        for (metric_name in names(metrics@metrics)) {
          metric_value <- metrics@metrics[[metric_name]]
          if (is.numeric(metric_value) && length(metric_value) == 1) {
            performance_metrics[[metric_name]] <- round(metric_value, 6)
          }
        }
        model_info$performance_metrics <- performance_metrics
      }
    }
    
    # Variable importance
    if (include_varimp) {
      tryCatch({
        varimp <- h2o.varimp(model, use_pandas = FALSE)
        if (!is.null(varimp) && nrow(varimp) > 0) {
          # Store top 10 important variables
          top_vars <- head(varimp, 10)
          model_info$variable_importance <- list(
            variables = top_vars$variable,
            relative_importance = round(top_vars$relative_importance, 6),
            scaled_importance = round(top_vars$scaled_importance, 6)
          )
        }
      }, error = function(e) {
        model_info$variable_importance <- NULL
      })
    }
    
    model_characteristics[[i]] <- model_info
  }
  
  # Add summary statistics
  attr(model_characteristics, "summary") <- list(
    total_models = length(models),
    algorithms_used = unique(sapply(model_characteristics, function(x) x$algorithm)),
    total_training_time_s = sum(sapply(model_characteristics, function(x) 
      if(!is.null(x$training_time_s)) x$training_time_s else 0)),
    dataset_info = list(
      target_column = autoxplain_result$target_column,
      n_rows = nrow(autoxplain_result$training_data),
      n_features = ncol(autoxplain_result$training_data) - 1
    )
  )
  
  class(model_characteristics) <- c("autoxplainr_model_characteristics", "list")
  return(model_characteristics)
}

#' Print Method for Model Characteristics
#'
#' @param x Object of class autoxplainr_model_characteristics
#' @param ... Additional arguments (unused)
#' @export
print.autoxplainr_model_characteristics <- function(x, ...) {
  summary_info <- attr(x, "summary")
  
  cat("AutoXplainR Model Characteristics\n")
  cat("=================================\n\n")
  
  cat("Dataset Information:\n")
  cat("- Target variable:", summary_info$dataset_info$target_column, "\n")
  cat("- Rows:", summary_info$dataset_info$n_rows, "\n")
  cat("- Features:", summary_info$dataset_info$n_features, "\n\n")
  
  cat("AutoML Summary:\n")
  cat("- Total models:", summary_info$total_models, "\n")
  cat("- Algorithms used:", paste(summary_info$algorithms_used, collapse = ", "), "\n")
  cat("- Total training time:", round(summary_info$total_training_time_s, 2), "seconds\n\n")
  
  cat("Model Rankings:\n")
  cat(sprintf("%-4s %-20s %-12s %-10s %-15s\n", "Rank", "Algorithm", "Time(s)", "Size(MB)", "Top Feature"))
  cat(strrep("-", 70), "\n")
  
  for (i in seq_along(x)) {
    model <- x[[i]]
    algo <- substr(model$algorithm, 1, 18)
    time_s <- if (!is.null(model$training_time_s)) round(model$training_time_s, 2) else "N/A"
    size_mb <- if (!is.null(model$size_mb)) round(model$size_mb, 3) else "N/A"
    top_feature <- if (!is.null(model$variable_importance)) {
      substr(model$variable_importance$variables[1], 1, 13)
    } else "N/A"
    
    cat(sprintf("%-4d %-20s %-12s %-10s %-15s\n", i, algo, time_s, size_mb, top_feature))
  }
  
  cat("\nUse summary() for detailed hyperparameter information.\n")
  invisible(x)
}

#' Summary Method for Model Characteristics
#'
#' @param object Object of class autoxplainr_model_characteristics
#' @param ... Additional arguments (unused)
#' @export
summary.autoxplainr_model_characteristics <- function(object, ...) {
  print(object)
  
  cat("\nDetailed Model Information:\n")
  cat("===========================\n")
  
  for (i in seq_along(object)) {
    model <- object[[i]]
    cat("\nModel", i, ":", model$algorithm, "\n")
    cat("- Model ID:", model$model_id, "\n")
    
    if (!is.null(model$hyperparameters)) {
      cat("- Key Hyperparameters:\n")
      for (param in names(model$hyperparameters)) {
        cat("  ", param, ":", model$hyperparameters[[param]], "\n")
      }
    }
    
    if (!is.null(model$performance_metrics)) {
      cat("- Performance Metrics:\n")
      # Show top 3 most relevant metrics
      metrics <- model$performance_metrics
      important_metrics <- c("rmse", "mse", "logloss", "auc", "r2", "mae")
      shown_metrics <- intersect(important_metrics, names(metrics))
      if (length(shown_metrics) == 0) shown_metrics <- names(metrics)[1:min(3, length(metrics))]
      
      for (metric in shown_metrics) {
        if (metric %in% names(metrics)) {
          cat("  ", metric, ":", metrics[[metric]], "\n")
        }
      }
    }
    
    if (!is.null(model$variable_importance)) {
      cat("- Top 3 Important Features:\n")
      for (j in 1:min(3, length(model$variable_importance$variables))) {
        cat("  ", j, ".", model$variable_importance$variables[j], 
            "(", round(model$variable_importance$relative_importance[j], 4), ")\n")
      }
    }
  }
  invisible(object)
}

#' Create Model Comparison Report
#'
#' Generates a detailed comparison report of all models including hyperparameters,
#' performance metrics, and training characteristics.
#'
#' @param model_characteristics Object from extract_model_characteristics()
#' @param output_file Character. Path for output file (default: "model_comparison_report.html")
#' @param include_plots Logical. Whether to include comparison plots (default: TRUE)
#' @return Character. Path to generated report file
#' @export
create_model_comparison_report <- function(model_characteristics, 
                                         output_file = "model_comparison_report.html",
                                         include_plots = TRUE) {
  
  if (!inherits(model_characteristics, "autoxplainr_model_characteristics")) {
    stop("model_characteristics must be from extract_model_characteristics()")
  }
  
  summary_info <- attr(model_characteristics, "summary")
  
  # Create HTML report
  html_content <- c(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "  <title>AutoXplainR Model Comparison Report</title>",
    "  <style>",
    "    body { font-family: Arial, sans-serif; margin: 20px; }",
    "    table { border-collapse: collapse; width: 100%; margin: 20px 0; }",
    "    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }",
    "    th { background-color: #f2f2f2; }",
    "    .model-section { margin: 30px 0; padding: 20px; border: 1px solid #ccc; }",
    "    .hyperparams { background-color: #f9f9f9; padding: 10px; }",
    "  </style>",
    "</head>",
    "<body>",
    "  <h1>AutoXplainR Model Comparison Report</h1>",
    "",
    "  <h2>Dataset Summary</h2>",
    paste0("  <p><strong>Target Variable:</strong> ", summary_info$dataset_info$target_column, "</p>"),
    paste0("  <p><strong>Dataset Size:</strong> ", summary_info$dataset_info$n_rows, " rows × ", summary_info$dataset_info$n_features, " features</p>"),
    paste0("  <p><strong>Models Trained:</strong> ", summary_info$total_models, "</p>"),
    paste0("  <p><strong>Total Training Time:</strong> ", round(summary_info$total_training_time_s, 2), " seconds</p>"),
    "",
    "  <h2>Model Comparison Table</h2>",
    "  <table>",
    "    <tr><th>Rank</th><th>Algorithm</th><th>Model ID</th><th>Training Time (s)</th><th>Model Size (MB)</th></tr>"
  )
  
  # Add model rows
  for (i in seq_along(model_characteristics)) {
    model <- model_characteristics[[i]]
    time_s <- if (!is.null(model$training_time_s)) round(model$training_time_s, 3) else "N/A"
    size_mb <- if (!is.null(model$size_mb)) round(model$size_mb, 3) else "N/A"
    
    html_content <- c(html_content,
      paste0("    <tr><td>", i, "</td><td>", model$algorithm, "</td><td>", model$model_id, 
             "</td><td>", time_s, "</td><td>", size_mb, "</td></tr>")
    )
  }
  
  html_content <- c(html_content, "  </table>")
  
  # Add detailed model sections
  for (i in seq_along(model_characteristics)) {
    model <- model_characteristics[[i]]
    
    html_content <- c(html_content,
      "",
      paste0("  <div class='model-section'>"),
      paste0("    <h3>Model ", i, ": ", model$algorithm, "</h3>"),
      paste0("    <p><strong>Model ID:</strong> ", model$model_id, "</p>")
    )
    
    # Hyperparameters
    if (!is.null(model$hyperparameters)) {
      html_content <- c(html_content,
        "    <h4>Hyperparameters</h4>",
        "    <div class='hyperparams'>"
      )
      
      for (param in names(model$hyperparameters)) {
        html_content <- c(html_content,
          paste0("      <p><strong>", param, ":</strong> ", model$hyperparameters[[param]], "</p>")
        )
      }
      
      html_content <- c(html_content, "    </div>")
    }
    
    # Performance metrics
    if (!is.null(model$performance_metrics)) {
      html_content <- c(html_content,
        "    <h4>Performance Metrics</h4>",
        "    <ul>"
      )
      
      for (metric in names(model$performance_metrics)) {
        html_content <- c(html_content,
          paste0("      <li><strong>", metric, ":</strong> ", model$performance_metrics[[metric]], "</li>")
        )
      }
      
      html_content <- c(html_content, "    </ul>")
    }
    
    html_content <- c(html_content, "  </div>")
  }
  
  html_content <- c(html_content,
    "",
    "  <footer>",
    paste0("    <p>Generated by AutoXplainR on ", Sys.Date(), "</p>"),
    "  </footer>",
    "</body>",
    "</html>"
  )
  
  # Write HTML file
  writeLines(html_content, output_file)
  
  message("Model comparison report generated: ", output_file)
  return(output_file)
}