#' Generate AutoXplainR Dashboard
#'
#' Creates a comprehensive HTML dashboard with comparative model explanations,
#' visualizations, and natural language summaries.
#'
#' @param autoxplain_result Object from autoxplain() function
#' @param output_file Character. Path for output HTML file (default: "autoxplainr_dashboard.html")
#' @param top_features Integer. Number of top features to analyze in detail (default: 5)
#' @param sample_instances Integer. DEPRECATED - no longer used (default: 3)
#' @param include_llm_report Logical. Whether to include LLM-generated report (default: TRUE)
#' @param llm_api_key Character. Google Generative AI API key (optional)
#' @param open_browser Logical. Whether to open dashboard in browser (default: TRUE)
#' @return Character. Path to generated HTML file
#' @export
#' @importFrom rmarkdown render
generate_dashboard <- function(autoxplain_result,
                              output_file = "autoxplainr_dashboard.html",
                              top_features = 5,
                              sample_instances = 3,
                              include_llm_report = TRUE,
                              llm_api_key = NULL,
                              open_browser = TRUE) {
  
  # Input validation
  if (!inherits(autoxplain_result, "autoxplain_result")) {
    stop("autoxplain_result must be an object from autoxplain() function")
  }
  
  if (!is.character(output_file) || length(output_file) != 1) {
    stop("output_file must be a single character string")
  }
  
  # Prepare dashboard data
  dashboard_data <- prepare_dashboard_data(autoxplain_result, top_features, sample_instances)
  
  # Generate LLM report if requested
  llm_report <- NULL
  if (include_llm_report) {
    tryCatch({
      llm_report <- generate_natural_language_report(
        autoxplain_result,
        importance_data = dashboard_data$importance,
        pdp_data = dashboard_data$pdp_data,
        model_characteristics = dashboard_data$model_characteristics,
        api_key = llm_api_key
      )
    }, error = function(e) {
      warning("Failed to generate LLM report: ", e$message)
      llm_report <<- NULL
    })
  }
  
  # Save serializable data to temporary files
  temp_dir <- tempdir()
  data_files <- list(
    importance = file.path(temp_dir, "importance.rds"),
    importance_list = file.path(temp_dir, "importance_list.rds"),
    pdp_data = file.path(temp_dir, "pdp_data.rds"),
    pdp_list = file.path(temp_dir, "pdp_list.rds"),
    models_analyzed = file.path(temp_dir, "models_analyzed.rds"),
    top_features = file.path(temp_dir, "top_features.rds"),
    feature_types = file.path(temp_dir, "feature_types.rds"),
    metadata = file.path(temp_dir, "metadata.rds"),
    model_characteristics = file.path(temp_dir, "model_characteristics.rds"),
    fitting_diagnostics = file.path(temp_dir, "fitting_diagnostics.rds"),
    correlation_insights_html = file.path(temp_dir, "correlation_insights_html.rds"),
    llm_report = if(!is.null(llm_report)) file.path(temp_dir, "llm_report.txt") else NULL
  )
  
  # Save the data
  saveRDS(dashboard_data$importance, data_files$importance)
  saveRDS(dashboard_data$importance_list, data_files$importance_list)
  saveRDS(dashboard_data$pdp_data, data_files$pdp_data)
  saveRDS(dashboard_data$pdp_list, data_files$pdp_list)
  saveRDS(dashboard_data$models_analyzed, data_files$models_analyzed)
  saveRDS(dashboard_data$top_features, data_files$top_features)
  saveRDS(dashboard_data$feature_types, data_files$feature_types)
  saveRDS(dashboard_data$model_characteristics, data_files$model_characteristics)
  saveRDS(dashboard_data$fitting_diagnostics, data_files$fitting_diagnostics)
  saveRDS(dashboard_data$correlation_insights_html, data_files$correlation_insights_html)
  saveRDS(list(
    model_count = length(autoxplain_result$models),
    target_column = autoxplain_result$target_column,
    task_type = if (!is.numeric(autoxplain_result$training_data[[autoxplain_result$target_column]])) "Classification" else "Regression"
  ), data_files$metadata)
  
  # Save LLM report if available
  if (!is.null(llm_report)) {
    writeLines(llm_report, data_files$llm_report)
  }
  
  # Create temporary Rmd file  
  rmd_content <- create_dashboard_rmd(data_files)
  temp_rmd <- tempfile(fileext = ".Rmd")
  writeLines(rmd_content, temp_rmd)
  
  # Render dashboard
  tryCatch({
    output_path <- render(
      input = temp_rmd,
      output_file = basename(output_file),
      output_dir = dirname(output_file),
      quiet = TRUE,
      envir = new.env()
    )
    
    # Clean up
    unlink(temp_rmd)
    
    # Open in browser if requested
    if (open_browser && interactive()) {
      browseURL(output_path)
    }
    
    message("Dashboard generated successfully: ", output_path)
    return(output_path)
    
  }, error = function(e) {
    unlink(temp_rmd)
    stop("Failed to render dashboard: ", e$message)
  })
}

#' Calculate Correlation Insights HTML
#'
#' Pre-calculates model correlation insights to avoid H2O progress bar leakage in dashboard.
#'
#' @param autoxplain_result Object from autoxplain() function
#' @param model_names Character vector of model names to analyze
#' @return Character string with HTML formatted correlation insights
#' @keywords internal
calculate_correlation_insights <- function(autoxplain_result, model_names) {
  
  # Initialize insights HTML structure
  insights_html <- '<div style="font-family: Arial, sans-serif; line-height: 1.4; padding: 15px; background-color: #f8f9fa; border-radius: 8px; border-left: 4px solid #1976d2;">'
  insights_html <- paste0(insights_html, '<h4 style="color: #1976d2; margin-top: 0;">🎯 Model Correlation Insights</h4>')
  
  # Always show correlation explanation content
  insights_html <- paste0(insights_html, 
    '<div style="margin: 6px 0; padding: 8px; background: white; border-radius: 4px; border-left: 3px solid #1976d2;">',
    '<strong style="color: #1976d2;">📊 Understanding Model Correlations</strong><br>',
    '<span style="font-size: 0.9em; color: #666;">',
    'Model correlations show how similarly different algorithms predict on your data.',
    '</span>',
    '</div>')
  
  # Apply same model family filtering as prepare_dashboard_data to reduce clutter
  models <- autoxplain_result$models
  
  # Filter to best model from each family to avoid heatmap clutter
  model_families <- sapply(names(models), function(name) {
    if (grepl("StackedEnsemble_AllModels", name)) "StackedEnsemble_AllModels"
    else if (grepl("StackedEnsemble_BestOfFamily", name)) "StackedEnsemble_BestOfFamily"  
    else if (grepl("XGBoost", name)) "XGBoost"
    else if (grepl("GBM", name)) "GBM"
    else if (grepl("GLM", name)) "GLM"
    else if (grepl("DeepLearning", name)) "DeepLearning"
    else if (grepl("DRF", name)) "RandomForest"
    else "Other"
  })
  
  # Keep only best model from each family, up to 6 total for clean heatmap
  selected_models <- c()
  selected_families <- c()
  
  for (i in seq_along(models)) {
    model_name <- names(models)[i]
    family <- model_families[i]
    
    if (!family %in% selected_families && length(selected_models) < 6) {
      selected_models <- c(selected_models, model_name)
      selected_families <- c(selected_families, family)
    }
  }
  
  # Use filtered model names for correlation calculation
  filtered_model_names <- selected_models
  
  # Calculate and show specific model pair correlations
  if (!is.null(filtered_model_names) && length(filtered_model_names) >= 2) {
    tryCatch({
      # Use simplified model names (for filtered models only)
      simplified_names <- sapply(filtered_model_names, function(name) {
        if (grepl('StackedEnsemble_AllModels', name)) {
          'All-Model Ensemble'
        } else if (grepl('StackedEnsemble_BestOfFamily', name)) {
          'Best-Family Ensemble'
        } else if (grepl('XGBoost', name)) {
          'XGBoost'
        } else if (grepl('GBM', name)) {
          'Gradient Boosting'
        } else if (grepl('GLM', name)) {
          'Linear Model'
        } else if (grepl('DeepLearning', name)) {
          'Neural Network'
        } else if (grepl('DRF', name)) {
          'Random Forest'
        } else {
          gsub('_.*', '', name)
        }
      })
      
      # Calculate actual correlation insights with real data
      n_models <- length(simplified_names)
      if (n_models >= 2) {
        # Calculate actual correlations between models
        tryCatch({
          # Get predictions from filtered models only
          model_list <- autoxplain_result$models[filtered_model_names]
          data_for_corr <- autoxplain_result$training_data
          target_col <- autoxplain_result$target_column
          
          # Remove target column from prediction data
          if (target_col %in% colnames(data_for_corr)) {
            pred_data <- data_for_corr[, !colnames(data_for_corr) %in% target_col]
          } else {
            pred_data <- data_for_corr
          }
          
          # Convert to H2O frame if needed
          if (!inherits(pred_data, 'H2OFrame') && !is.null(data_for_corr)) {
            pred_data <- h2o::as.h2o(pred_data)
          }
          
          # Calculate correlations between filtered model predictions only
          cor_values <- c()
          cor_pairs <- c()
          
          model_names_list <- names(model_list)
          for (i in 1:(length(model_list)-1)) {
            for (j in (i+1):length(model_list)) {
              # Comprehensive H2O output suppression (critical for avoiding progress bars)
              invisible({
                capture.output({
                  suppressMessages({
                    pred1 <- h2o.predict(model_list[[i]], pred_data)
                    pred2 <- h2o.predict(model_list[[j]], pred_data)
                  })
                }, type = 'output')
              })
              
              pred1_val <- as.vector(pred1[, 1])
              pred2_val <- as.vector(pred2[, 1])
              
              cor_val <- cor(pred1_val, pred2_val, use = 'complete.obs')
              if (!is.na(cor_val)) {
                # Use simplified, readable model names
                model1_name <- if(i <= length(simplified_names)) simplified_names[i] else substr(gsub('_.*', '', model_names_list[i]), 1, 12)
                model2_name <- if(j <= length(simplified_names)) simplified_names[j] else substr(gsub('_.*', '', model_names_list[j]), 1, 12)
                cor_values <- c(cor_values, cor_val)
                cor_pairs <- c(cor_pairs, paste(model1_name, 'vs', model2_name))
              }
            }
          }
          
          # Analyze correlation patterns
          if (length(cor_values) > 0) {
            highest_idx <- which.max(cor_values)
            lowest_idx <- which.min(cor_values)
            avg_corr <- mean(cor_values)
            
            insights_html <- paste0(insights_html, 
              '<div style="margin: 6px 0; padding: 8px; background: white; border-radius: 4px; border-left: 3px solid #2e7d32;">',
              '<strong style="color: #2e7d32;">📊 Actual Correlation Analysis</strong><br>',
              '<div style="margin-top: 5px;">',
              '<span style="color: #1b5e20; font-weight: bold;">Highest Similarity:</span><br>',
              '<span style="font-size: 0.85em; margin-left: 10px;">• ', cor_pairs[highest_idx], ': ', sprintf('%.3f', cor_values[highest_idx]), '</span><br>',
              '</div>',
              '<div style="margin-top: 5px;">',
              '<span style="color: #c62828; font-weight: bold;">Most Diverse Pair:</span><br>',
              '<span style="font-size: 0.85em; margin-left: 10px;">• ', cor_pairs[lowest_idx], ': ', sprintf('%.3f', cor_values[lowest_idx]), '</span><br>',
              '</div>',
              '<div style="margin-top: 8px; padding: 5px; background: #e8f5e9; border-radius: 3px;">',
              '<span style="color: #2e7d32; font-size: 0.85em;"><strong>Average Correlation:</strong> ', sprintf('%.3f', avg_corr), 
              if (avg_corr > 0.9) ' - Very similar models' else if (avg_corr > 0.7) ' - Moderately similar' else ' - Good diversity', '</span>',
              '</div>',
              '</div>')
          }
        }, error = function(e) {
          # Fallback to simple message if correlation calculation fails
          insights_html <<- paste0(insights_html, 
            '<div style="margin: 6px 0; padding: 8px; background: white; border-radius: 4px; border-left: 3px solid #2e7d32;">',
            '<strong style="color: #2e7d32;">📊 Model Analysis</strong><br>',
            '<span style="font-size: 0.9em; color: #666;">Analyzing correlation patterns between ', n_models, ' models. Check the heatmap above for detailed relationships.</span>',
            '</div>')
        })
      }
    }, error = function(e) {
      # Fallback: Show educational message when correlation calculation fails
      insights_html <<- paste0(insights_html, 
        '<div style="margin: 6px 0; padding: 8px; background: white; border-radius: 4px; border-left: 3px solid #2e7d32;">',
        '<strong style="color: #2e7d32;">🎯 Model Correlation Analysis</strong><br>',
        '<span style="font-size: 0.9em; color: #666;">Real correlation values are shown in the heatmap above.</span><br>',
        '<span style="font-size: 0.85em; margin-top: 5px;">Use the visualization to assess model diversity for ensemble building.</span>',
        '</div>')
    })
  }
  
  # Add concise interpretation guide
  insights_html <- paste0(insights_html, 
    '<div style="margin-top: 10px; padding: 8px; background: #e3f2fd; border-radius: 4px;">',
    '<strong style="color: #1976d2; font-size: 0.9em;">📊 Reading the Heatmap:</strong><br>',
    '<span style="font-size: 0.8em; color: #666;">Blue = different patterns, Red = similar predictions</span>',
    '</div>',
    '</div>')
  
  return(insights_html)
}

#' Prepare data for dashboard generation
#'
#' @param autoxplain_result AutoML results
#' @param top_features Number of top features
#' @param sample_instances Number of sample instances
#' @return List with prepared data
#' @keywords internal
prepare_dashboard_data <- function(autoxplain_result, top_features, sample_instances) {
  
  data <- autoxplain_result$training_data
  target_column <- autoxplain_result$target_column
  models <- autoxplain_result$models
  
  # Calculate feature importance for ALL models to enable model selection
  importance_list <- list()
  pdp_list <- list()
  
  # Select top 6 models, keeping only best from each family
  model_families <- sapply(names(models), function(name) {
    if (grepl("StackedEnsemble_AllModels", name)) "Ensemble_All"
    else if (grepl("StackedEnsemble_BestOfFamily", name)) "Ensemble_Best"
    else if (grepl("XGBoost", name)) "XGBoost"
    else if (grepl("GBM", name)) "GBM"
    else if (grepl("GLM", name)) "GLM"
    else if (grepl("DeepLearning", name)) "DeepLearning"
    else if (grepl("DRF", name)) "RandomForest"
    else "Other"
  })
  
  # Keep only best model from each family, up to 6 total
  selected_models <- c()
  selected_families <- c()
  
  for (i in seq_along(models)) {
    model_name <- names(models)[i]
    family <- model_families[i]
    
    if (!family %in% selected_families && length(selected_models) < 6) {
      selected_models <- c(selected_models, model_name)
      selected_families <- c(selected_families, family)
    }
  }
  
  models_to_analyze <- models[selected_models]
  model_names <- selected_models
  
  for (i in seq_along(models_to_analyze)) {
    model <- models_to_analyze[[i]]
    model_name <- model_names[i]
    
    # Calculate importance for each model
    tryCatch({
      importance_list[[model_name]] <- calculate_permutation_importance(
        model, data, target_column, n_repeats = 2
      )
    }, error = function(e) {
      warning("Failed to calculate importance for model ", model_name, ": ", e$message)
    })
  }
  
  # Get importance for best model (default selection)
  importance <- NULL
  if (length(importance_list) > 0) {
    importance <- importance_list[[1]]
  } else {
    # Fallback with minimal repeats
    importance <- calculate_permutation_importance(models[[1]], data, target_column, n_repeats = 1)
  }
  
  top_feature_names <- head(importance$feature, top_features)
  
  # Calculate PDP for top features with optimized function
  for (i in seq_along(models_to_analyze)) {
    model <- models_to_analyze[[i]]
    model_name <- model_names[i]
    
    tryCatch({
      # Use top features for calculation with optimized PDP function
      top_pdp_features <- head(top_feature_names, min(top_features, length(top_feature_names)))
      pdp_list[[model_name]] <- calculate_partial_dependence_multi(
        model, data, top_pdp_features, n_points = 10
      )
    }, error = function(e) {
      warning("Failed to calculate PDP for model ", model_name, ": ", e$message)
    })
  }
  
  # Get PDP for best model (default selection) with improved fallback
  pdp_data <- NULL
  if (length(pdp_list) > 0) {
    # Find first successful PDP calculation
    for (model_result in pdp_list) {
      if (!is.null(model_result) && length(model_result) > 0) {
        pdp_data <- model_result
        break
      }
    }
  }
  
  # Debug information
  cat("PDP calculation summary:\n")
  cat("- Models attempted:", length(models_to_analyze), "\n")
  cat("- PDP results available:", length(pdp_list), "\n")
  cat("- Final pdp_data available:", !is.null(pdp_data), "\n")
  if (!is.null(pdp_data)) {
    cat("- PDP features calculated:", length(pdp_data), "\n")
  }
  
  # Determine feature types for better explanations
  feature_types <- sapply(top_feature_names, function(feature_name) {
    if (feature_name %in% colnames(data)) {
      col_data <- data[[feature_name]]
      # Check if categorical (character, factor, or low-cardinality numeric)
      if (is.character(col_data) || is.factor(col_data)) {
        return("categorical")
      } else if (is.numeric(col_data)) {
        # Consider numeric variables with few unique values as categorical
        unique_vals <- length(unique(col_data[!is.na(col_data)]))
        if (unique_vals <= 10 && unique_vals < nrow(data) * 0.05) {
          return("categorical")
        } else {
          return("numerical")
        }
      } else {
        return("numerical")  # Default to numerical
      }
    } else {
      return("numerical")  # Default if feature not found
    }
  })
  names(feature_types) <- top_feature_names
  
  
  # Extract model characteristics
  model_characteristics <- NULL
  tryCatch({
    model_characteristics <- extract_model_characteristics(autoxplain_result)
  }, error = function(e) {
    warning("Failed to extract model characteristics: ", e$message)
  })
  
  # Collect diagnostic information about the fitting process
  fitting_diagnostics <- list(
    total_models_requested = length(autoxplain_result$models),
    models_successfully_trained = length(autoxplain_result$models),
    algorithms_attempted = unique(sapply(autoxplain_result$models, function(m) m@algorithm)),
    importance_calculation_success = !is.null(importance),
    pdp_calculation_success = !is.null(pdp_data),
    model_characteristics_extracted = !is.null(model_characteristics),
    dataset_size = paste0(nrow(data), " rows × ", ncol(data), " features"),
    target_column = target_column,
    warnings_encountered = length(warnings())
  )
  
  # Calculate correlation insights (outside R markdown to avoid progress bar leakage)
  correlation_insights_html <- calculate_correlation_insights(autoxplain_result, model_names)
  
  return(list(
    autoxplain_result = autoxplain_result,
    importance = importance,
    importance_list = importance_list,
    pdp_data = pdp_data,
    pdp_list = pdp_list,
    models_analyzed = model_names,
    top_features = top_feature_names,
    feature_types = feature_types,
    model_characteristics = model_characteristics,
    fitting_diagnostics = fitting_diagnostics,
    correlation_insights_html = correlation_insights_html
  ))
}

#' Create flexdashboard Rmd content
#'
#' @param data_files List of file paths to data RDS files
#' @return Character vector with Rmd content
#' @keywords internal
create_dashboard_rmd <- function(data_files) {
  
  rmd_content <- c(
    "---",
    "title: \"AutoXplainR: Automated ML Explanation Dashboard\"",
    "subtitle: \"Automated Machine Learning Analysis\"",
    "output:",
    "  flexdashboard::flex_dashboard:",
    "    theme: bootstrap", 
    "    orientation: columns",
    "    vertical_layout: fill",
    "    source_code: embed",
    "    navbar:",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "library(flexdashboard)",
    "library(plotly)",
    "library(DT)",
    "library(knitr)",
    "library(AutoXplainR)",
    "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
    "",
    "# Add global CSS for thin scrollbars",
    "htmltools::tags$head(",
    "  htmltools::tags$style(",
    "    htmltools::HTML(",
    "      '::-webkit-scrollbar { width: 12px; height: 12px; }',",
    "      '::-webkit-scrollbar-track { background: #f1f1f1; border-radius: 4px; }',",
    "      '::-webkit-scrollbar-thumb { background: #888; border-radius: 4px; }',",
    "      '::-webkit-scrollbar-thumb:hover { background: #555; }',",
    "      '* { scrollbar-width: thin; scrollbar-color: #888 #f1f1f1; }'",
    "    )",
    "  )",
    ")",
    "",
    "# Load dashboard data from RDS files",
    paste0("importance <- readRDS('", data_files$importance, "')"),
    paste0("pdp_data <- readRDS('", data_files$pdp_data, "')"),
    paste0("top_features <- readRDS('", data_files$top_features, "')"),
    paste0("metadata <- readRDS('", data_files$metadata, "')"),
    paste0("model_characteristics <- readRDS('", data_files$model_characteristics, "')"),
    paste0("fitting_diagnostics <- readRDS('", data_files$fitting_diagnostics, "')"),
    "model_count <- metadata$model_count",
    "target_column <- metadata$target_column",
    "task_type <- metadata$task_type",
    "```",
    "",
    "# Overview {data-icon=\"fa-dashboard\"}",
    "",
    "## Column {data-width=500}",
    "",
    "### 🏆 Performance vs Training Time {data-height=550}",
    "",
    "```{r model-comparison}",
    "# Create meaningful model performance comparison",
    "if (!is.null(model_characteristics) && length(model_characteristics) > 0) {",
    "  # Extract performance data from model characteristics",
    "  perf_data <- data.frame(",
    "    Rank = integer(),",
    "    Algorithm = character(),",
    "    Performance = numeric(),",
    "    Training_Time = numeric(),",
    "    stringsAsFactors = FALSE",
    "  )",
    "  ",
    "  for (i in seq_along(model_characteristics)) {",
    "    model <- model_characteristics[[i]]",
    "    # Get actual performance metric - prioritize RMSE for consistency",
    "    perf_score <- if (!is.null(model$performance_metrics)) {",
    "      perf_metrics <- model$performance_metrics",
    "      metric_names <- names(perf_metrics)",
    "      ",
    "      # Determine task type for proper metric selection",
    "      is_classification <- !is.numeric(autoxplain_result$training_data[[autoxplain_result$target_column]])",
    "      ",
    "      if (!is_classification) {",
    "        # For regression: prioritize RMSE for interpretability",
    "        selected_metric <- if ('RMSE' %in% metric_names) {",
    "          'RMSE'",
    "        } else if ('rmse' %in% metric_names) {",
    "          'rmse'",
    "        } else if ('MAE' %in% metric_names) {",
    "          'MAE'",
    "        } else if ('mae' %in% metric_names) {",
    "          'mae'",
    "        } else if ('MSE' %in% metric_names) {",
    "          'MSE'",
    "        } else {",
    "          metric_names[1]",
    "        }",
    "      } else {",
    "        # For classification: prioritize AUC",
    "        selected_metric <- if ('AUC' %in% metric_names) {",
    "          'AUC'",
    "        } else if ('auc' %in% metric_names) {",
    "          'auc'",
    "        } else {",
    "          metric_names[1]",
    "        }",
    "      }",
    "      ",
    "      round(perf_metrics[[selected_metric]], 4)",
    "    } else { NA }  # No fallback - use actual performance only",
    "    ",
    "    training_time <- if (!is.null(model$training_time_s)) {",
    "      max(model$training_time_s, 0.01)  # Ensure minimum size for visibility",
    "    } else { 0.1 }",
    "    ",
    "    perf_data <- rbind(perf_data, data.frame(",
    "      Rank = i,",
    "      Algorithm = model$algorithm,",
    "      Performance = perf_score,",
    "      Training_Time = training_time",
    "    ))",
    "  }",
    "  ",
    "  # Clean data - remove rows with missing critical values and ensure all fields are valid",
    "  perf_data <- perf_data[!is.na(perf_data$Performance) & !is.na(perf_data$Training_Time), ]",
    "  perf_data <- perf_data[!is.infinite(perf_data$Performance) & !is.infinite(perf_data$Training_Time), ]",
    "  perf_data <- perf_data[!is.na(perf_data$Algorithm) & nchar(as.character(perf_data$Algorithm)) > 0, ]",
    "  perf_data <- perf_data[!is.na(perf_data$Rank), ]",
    "  ",
    "  # Convert to character to ensure proper display",
    "  perf_data$Algorithm <- as.character(perf_data$Algorithm)",
    "  ",
    "  # Ensure minimum data quality",
    "  if (nrow(perf_data) == 0) {",
    "    stop('No valid performance data available for plotting')",
    "  }",
    "  ",
    "  # Get actual performance metric name - prioritize RMSE for regression",
    "  is_classification <- !is.numeric(autoxplain_result$training_data[[autoxplain_result$target_column]])",
    "  metric_name <- if (!is.null(model_characteristics[[1]]$performance_metrics)) {",
    "    perf_metrics <- model_characteristics[[1]]$performance_metrics",
    "    metric_names <- names(perf_metrics)",
    "    ",
    "    # For regression: prioritize RMSE over MSE for interpretability",
    "    if (!is_classification) {",
    "      if ('RMSE' %in% metric_names) {",
    "        'RMSE'",
    "      } else if ('rmse' %in% metric_names) {",
    "        'rmse'",
    "      } else if ('MAE' %in% metric_names) {",
    "        'MAE'",
    "      } else if ('mae' %in% metric_names) {",
    "        'mae'",
    "      } else if ('MSE' %in% metric_names) {",
    "        'MSE'  # Fallback to MSE if no RMSE",
    "      } else {",
    "        metric_names[1]  # First available metric",
    "      }",
    "    } else {",
    "      # For classification: prioritize AUC",
    "      if ('AUC' %in% metric_names) {",
    "        'AUC'",
    "      } else if ('auc' %in% metric_names) {",
    "        'auc'",
    "      } else {",
    "        metric_names[1]",
    "      }",
    "    }",
    "  } else { 'Performance' }",
    "  ",
    "  # Create concise metric label for Y-axis",
    "  metric_explanation <- switch(tolower(metric_name),",
    "    'auc' = 'AUC',",
    "    'logloss' = 'LogLoss',",
    "    'rmse' = 'RMSE',",
    "    'mae' = 'MAE',",
    "    'mse' = 'MSE',",
    "    'scoring_time' = 'Time (s)',",
    "    'mean_per_class_error' = 'Error Rate',",
    "    toupper(metric_name)",
    "  )",
    "  ",
    "  lower_is_better <- grepl('loss|error|mse|rmse|mae|time', tolower(metric_name))",
    "  perf_direction <- if (lower_is_better) 'Lower is Better' else 'Higher is Better'",
    "  ",
    "  # Normalize performance for better Y-axis readability",
    "  perf_range <- range(perf_data$Performance)",
    "  perf_spread <- perf_range[2] - perf_range[1]",
    "  ",
    "  # For timing metrics or very small differences, use relative performance",
    "  if (grepl('time|timing|speed', tolower(metric_name)) || perf_spread < 0.01) {",
    "    # Special handling for timing metrics",
    "    if (grepl('time|timing|speed', tolower(metric_name))) {",
    "      fastest_time <- min(perf_data$Performance)",
    "      perf_data$Performance_Relative <- (fastest_time / perf_data$Performance) * 100",
    "      y_axis_var <- ~Performance_Relative",
    "      y_axis_title <- 'Relative Speed (% of Fastest Model)'",
    "      hover_perf <- ~paste('Time:', round(Performance, 4), 's<br>Relative Speed:', round(Performance_Relative, 1), '%')",
    "    } else {",
    "      # For other small differences: show percentage improvement",
    "      worst_score <- if (lower_is_better) max(perf_data$Performance) else min(perf_data$Performance)",
    "      perf_data$Performance_Pct <- if (lower_is_better) {",
    "        ((worst_score - perf_data$Performance) / worst_score) * 100",
    "      } else {",
    "        ((perf_data$Performance - worst_score) / worst_score) * 100",
    "      }",
    "      y_axis_var <- ~Performance_Pct",
    "      y_axis_title <- '% Improvement over Worst Model'",
    "      hover_perf <- ~paste('Original Score:', round(Performance, 6), '<br>% Better:', round(Performance_Pct, 2), '%')",
    "    }",
    "  } else {",
    "    y_axis_var <- ~Performance",
    "    y_axis_title <- metric_explanation",
    "    hover_perf <- ~paste('Score:', round(Performance, 4))",
    "  }",
    "  ",
    "  # Calculate true Pareto front - points not dominated by any other point",
    "  calculate_pareto_front <- function(data, lower_perf_is_better) {",
    "    n <- nrow(data)",
    "    is_pareto <- rep(TRUE, n)",
    "    ",
    "    for (i in 1:n) {",
    "      for (j in 1:n) {",
    "        if (i != j) {",
    "          # Check if point j dominates point i",
    "          time_better <- data$Training_Time[j] <= data$Training_Time[i]  # Lower time is always better",
    "          perf_better <- if(lower_perf_is_better) {",
    "            data$Performance[j] <= data$Performance[i]  # Lower performance is better",
    "          } else {",
    "            data$Performance[j] >= data$Performance[i]  # Higher performance is better",
    "          }",
    "          ",
    "          # Point j dominates point i if j is better or equal in both dimensions,",
    "          # and strictly better in at least one dimension",
    "          time_strictly_better <- data$Training_Time[j] < data$Training_Time[i]",
    "          perf_strictly_better <- if(lower_perf_is_better) {",
    "            data$Performance[j] < data$Performance[i]",
    "          } else {",
    "            data$Performance[j] > data$Performance[i]",
    "          }",
    "          ",
    "          if (time_better && perf_better && (time_strictly_better || perf_strictly_better)) {",
    "            is_pareto[i] <- FALSE",
    "            break",
    "          }",
    "        }",
    "      }",
    "    }",
    "    ",
    "    return(data[is_pareto, ])",
    "  }",
    "  ",
    "  # Calculate Pareto optimal points (no connecting lines)",
    "  pareto_points <- calculate_pareto_front(perf_data, lower_is_better)",
    "  ",
    "  # Determine axis ranges with better padding",
    "  x_min <- min(perf_data$Training_Time)",
    "  x_max <- max(perf_data$Training_Time)",
    "  x_padding <- (x_max - x_min) * 0.05  # 5% padding horizontally",
    "  # Start x-axis closer to actual minimum, not way left",
    "  x_range <- c(max(x_min * 0.9, x_min - x_padding), x_max + x_padding)",
    "  ",
    "  y_min <- min(perf_data$Performance)",
    "  y_max <- max(perf_data$Performance)",
    "  y_padding <- abs(y_max - y_min) * 0.15  # 15% padding vertically",
    "  y_range <- if(lower_is_better) {",
    "    c(y_min - y_padding, y_max + y_padding)",
    "  } else {",
    "    c(max(0, y_min - y_padding), y_max + y_padding)",
    "  }",
    "  ",
    "  # Use log scale for x-axis if training times span multiple orders of magnitude",
    "  use_log_scale <- (max(perf_data$Training_Time) / min(perf_data$Training_Time)) > 10",
    "  ",
    "  # Create base plot structure - add Pareto line first if exists, then points on top",
    "  if (nrow(pareto_points) > 1) {",
    "    # Sort Pareto points by training time for proper line connection",
    "    pareto_sorted <- pareto_points[order(pareto_points$Training_Time), ]",
    "    ",
    "    # Create plot with Pareto line first (background layer)",
    "    p <- plotly::plot_ly() %>%",
    "      plotly::add_trace(",
    "        data = pareto_sorted,",
    "        x = ~Training_Time,",
    "        y = ~Performance,",
    "        type = 'scatter',",
    "        mode = 'lines',",
    "        line = list(",
    "          color = '#FF6B6B',",
    "          width = 3,",
    "          dash = 'dash'",
    "        ),",
    "        name = 'Pareto Front',",
    "        hoverinfo = 'skip',",
    "        showlegend = FALSE",
    "      ) %>%",
    "      # Add all model points on top (foreground layer)",
    "      plotly::add_trace(",
    "        data = perf_data,",
    "        x = ~Training_Time,",
    "        y = ~Performance,",
    "        color = ~Algorithm,",
    "        type = 'scatter',",
    "        mode = 'markers',",
    "        marker = list(size = 12, opacity = 0.7),",
    "        text = ~paste0('<b>', Algorithm, '</b><br>',",
    "                       'Performance: ', round(Performance, 4), '<br>',",
    "                       'Training Time: ', round(Training_Time, 3), 's'),",
    "        hoverinfo = 'text',",
    "        name = 'Models'",
    "      )",
    "  } else {",
    "    # No Pareto front, just create regular scatter plot",
    "    p <- plotly::plot_ly(",
    "      data = perf_data,",
    "      x = ~Training_Time,",
    "      y = ~Performance,",
    "      color = ~Algorithm,",
    "      type = 'scatter',",
    "      mode = 'markers',",
    "      marker = list(size = 12, opacity = 0.7),",
    "      text = ~paste0('<b>', Algorithm, '</b><br>',",
    "                     'Performance: ', round(Performance, 4), '<br>',",
    "                     'Training Time: ', round(Training_Time, 3), 's'),",
    "      hoverinfo = 'text',",
    "      name = 'Models'",
    "    )",
    "  }",
    "  ",
    "  p <- p %>%",
    "    plotly::layout(",
    "      title = list(",
    "        text = paste0('Model Performance vs Training Time<br><span style=\"font-size: 12px; color: #666;\">🔴 Dashed line shows Pareto front</span>'),",
    "        font = list(size = 16)",
    "      ),",
    "      xaxis = list(",
    "        title = paste0('Training Time (seconds)', if(use_log_scale) ' (Log Scale)' else '', '<br><span style=\"font-size: 11px; color: #666;\">(Lower is Better)</span>'),",
    "        titlefont = list(size = 14),",
    "        range = if(use_log_scale) log10(x_range) else x_range,",
    "        type = if(use_log_scale) 'log' else 'linear',",
    "        # Fix log scale tick formatting - only show clean powers of 10",
    "        tickmode = if(use_log_scale) 'array' else 'auto',",
    "        tickvals = if(use_log_scale) {",
    "          # Generate clean tick values (powers of 10)",
    "          log_min <- floor(log10(x_range[1]))",
    "          log_max <- ceiling(log10(x_range[2]))",
    "          10^(log_min:log_max)",
    "        } else NULL,",
    "        ticktext = if(use_log_scale) {",
    "          log_min <- floor(log10(x_range[1]))",
    "          log_max <- ceiling(log10(x_range[2]))",
    "          paste0(10^(log_min:log_max))",
    "        } else NULL",
    "      ),",
    "      yaxis = list(",
    "        title = paste0(metric_explanation, '<br><span style=\"font-size: 11px; color: #666;\">(', perf_direction, ')</span>'),",
    "        titlefont = list(size = 14),",
    "        range = y_range",
    "      ),",
    "      showlegend = FALSE,  # Hide legend for cleaner look",
    "      margin = list(l = 60, r = 30, t = 70, b = 50),",
    "      autosize = TRUE",
    "    ) %>%",
    "    plotly::config(displayModeBar = FALSE, responsive = TRUE)",
    "  ",
    "  p",
    "} else {",
    "  # Fallback simple chart",
    "  model_data <- data.frame(",
    "    Model = paste('Model', 1:model_count),",
    "    Rank = 1:model_count",
    "  )",
    "  p <- plotly::plot_ly(model_data, x = ~Rank, y = ~Model, type = 'scatter', mode = 'markers',",
    "                       marker = list(size = 15, color = 'steelblue')) %>%",
    "       plotly::layout(title = 'Models Trained', xaxis = list(title = 'Rank'),",
    "                      yaxis = list(title = 'Model')) %>%",
    "       plotly::config(displayModeBar = FALSE, responsive = TRUE)",
    "  p",
    "}",
    "```",
    "",
    "",
    "### 🎯 Permutation Feature Importance (Degradation when feature values are shuffled) {data-height=450}",
    "",
    "```{r feature-importance}",    "if (!is.null(importance)) {",
    "  # Create colorful importance plot based on quantiles",
    "  importance_data <- importance",
    "  ",
    "  # Calculate quantiles for color coding",
    "  quantiles <- quantile(importance_data$importance, probs = c(0, 0.25, 0.5, 0.75, 1.0))",
    "  ",
    "  # Check if quantiles are unique, if not use simple color assignment",
    "  if (length(unique(quantiles)) < 5) {",
    "    # All values are similar, use single color",
    "    importance_data$color_group <- 'Medium'",
    "  } else {",
    "    # Assign colors based on importance quantiles",
    "    importance_data$color_group <- cut(importance_data$importance, ",
    "                                      breaks = quantiles, ",
    "                                      labels = c('Low', 'Medium-Low', 'Medium-High', 'High'),",
    "                                      include.lowest = TRUE)",
    "  }",
    "  ",
    "  # Define color palette (blue gradient for cleaner professional look)",
    "  color_palette <- c('Low' = '#E3F2FD',           # Very light blue",
    "                     'Medium-Low' = '#90CAF9',     # Light blue", 
    "                     'Medium' = '#42A5F5',         # Medium blue",
    "                     'Medium-High' = '#1E88E5',    # Darker blue",
    "                     'High' = '#0D47A1')           # Deep blue",
    "  ",
    "  importance_data$colors <- color_palette[importance_data$color_group]",
    "  ",
    "  # Create clean, readable importance plot",
    "  p <- plotly::plot_ly(",
    "    data = importance_data, ",
    "    x = ~importance, ",
    "    y = ~reorder(feature, importance),",
    "    type = 'bar',",
    "    orientation = 'h',",
    "    marker = list(",
    "      color = ~colors,",
    "      line = list(color = 'white', width = 1)",
    "    ),",
    "    hovertemplate = '<b>%{y}</b><br>Importance: %{x:.3f}<extra></extra>'",
    "  ) %>%",
    "  plotly::layout(",
    "    title = list(",
    "      text = 'Feature Importance Ranking (Best Model)',",
    "      font = list(size = 16)",
    "    ),",
    "    xaxis = list(",
    "      title = '',",
    "      titlefont = list(size = 14),",
    "      side = 'top'",
    "    ),",
    "    yaxis = list(",
    "      title = '',",
    "      tickfont = list(size = 12)",
    "    ),",
    "    showlegend = FALSE,",
    "    margin = list(l = 80, r = 20, t = 40, b = 40),",
    "    height = max(450, nrow(importance_data) * 30)  # Dynamic height for scrolling",
    "  ) %>%",
    "  plotly::config(displayModeBar = FALSE, responsive = TRUE, scrollZoom = TRUE)",
    "  ",
    "  # Create scrollable container",
    "  htmltools::div(",
    "    style = 'height: 275px; max-height: 100vh; overflow-y: auto; overflow-x: hidden; border: 1px solid #e0e0e0; border-radius: 6px; background: white;',",
    "    p",
    "  )",
    "} else {",
    "  plotly::plot_ly() %>% plotly::add_text(x = 0.5, y = 0.5, text = 'Feature importance data not available', textposition = 'middle center') %>% plotly::config(displayModeBar = FALSE)",
    "}",
    "```"
  )
  
  # Add AI-Generated Summary section if available - make it more prominent
  if (!is.null(data_files$llm_report)) {
    rmd_content <- c(rmd_content,
      "",
      "## Column {data-width=500}",
      "",
      "### 🤖 AI Summary {data-height=675}",
      "",
      "```{r llm-report}",
      paste0("report_text <- readLines('", data_files$llm_report, "')"),
      "# Format as proper markdown",
      "formatted_report <- paste(report_text, collapse = '\n')",
      "# Convert markdown to HTML for proper rendering",
      "if (requireNamespace('markdown', quietly = TRUE)) {",
      "  html_report <- markdown::renderMarkdown(text = formatted_report)",
      "  # Add compact styling with better space usage",
      "  styled_report <- paste0(",
      "    '<div style=\"font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5; padding: 15px; background-color: #f8f9fa; border-radius: 8px; border: 1px solid #e9ecef; scrollbar-width: thin; scrollbar-color: #888 #f1f1f1;\">',",
      "    html_report,",
      "    '</div>'",
      "  )",
      "  htmltools::HTML(styled_report)",
      "} else {",
      "  # Enhanced fallback without markdown package", 
      "  # Comprehensive markdown-to-HTML conversion",
      "  html_content <- formatted_report",
      "  ",
      "  # Convert headers first (order matters)",
      "  html_content <- gsub('(^|\\\\n)### ([^\\\\n]+)', '\\\\1<h4>\\\\2</h4>', html_content, perl = TRUE)",
      "  html_content <- gsub('(^|\\\\n)## ([^\\\\n]+)', '\\\\1<h3>\\\\2</h3>', html_content, perl = TRUE)", 
      "  html_content <- gsub('(^|\\\\n)# ([^\\\\n]+)', '\\\\1<h2>\\\\2</h2>', html_content, perl = TRUE)",
      "  ",
      "  # Convert inline code (backticks) - must come before bold/italic",
      "  html_content <- gsub('`([^`]+)`', '<code style=\"background-color: #f1f3f4; padding: 2px 4px; border-radius: 3px; font-size: 90%; font-family: monospace;\">\\\\1</code>', html_content)",
      "  ",
      "  # Convert bold and italic",
      "  html_content <- gsub('\\\\*\\\\*([^*]+)\\\\*\\\\*', '<strong>\\\\1</strong>', html_content)",
      "  html_content <- gsub('(?<!\\\\*)\\\\*([^*]+)\\\\*(?!\\\\*)', '<em>\\\\1</em>', html_content, perl = TRUE)",
      "  ",
      "  # Convert numbered lists",
      "  html_content <- gsub('(^|\\\\n)([0-9]+\\\\. )', '\\\\1<br>\\\\2', html_content, perl = TRUE)",
      "  ",
      "  # Convert line breaks and paragraphs",
      "  html_content <- gsub('\\\\n\\\\n', '</p><p>', html_content)",
      "  html_content <- gsub('\\\\n', '<br>', html_content)",
      "  html_content <- paste0('<p>', html_content, '</p>')",
      "  ",
      "  # Clean up any double paragraph tags",
      "  html_content <- gsub('<p></p>', '', html_content)",
      "  html_content <- gsub('<p><br>', '<p>', html_content)",
      "  ",
      "  # Fallback without markdown package",
      "  styled_report <- paste0(",
      "    '<div style=\"font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5; padding: 15px; background-color: #f8f9fa; border-radius: 8px; border: 1px solid #e9ecef; scrollbar-width: thin; scrollbar-color: #888 #f1f1f1;\">',",
      "    html_content,",
      "    '</div>'",
      "  )",
      "  htmltools::HTML(styled_report)",
      "}",
      "```",
      "",
      "### 📋 Pipeline Summary {data-height=325}",
      "",
      "```{r pipeline-summary-right}",
      "# Compact pipeline summary for right column",
      "if (!is.null(model_characteristics)) {",
      "  summary_info <- attr(model_characteristics, 'summary')",
      "  if (!is.null(summary_info)) {",
      "    best_algorithm <- if (!is.null(model_characteristics[[1]]$algorithm)) {",
      "      model_characteristics[[1]]$algorithm",
      "    } else { 'Unknown' }",
      "    ",
      "    model_count <- length(model_characteristics)",
      "    total_training_time <- if (!is.null(summary_info$total_training_time_s)) {",
      "      round(summary_info$total_training_time_s, 2)",
      "    } else { 'Unknown' }",
      "    ",
      "    # Get best model performance",
      "    best_score_info <- list(name = '', value = 'N/A')",
      "    if (!is.null(model_characteristics[[1]]$performance_metrics)) {",
      "      perf_metrics <- model_characteristics[[1]]$performance_metrics",
      "      metric_names <- names(perf_metrics)",
      "      is_classification <- !is.numeric(autoxplain_result$training_data[[autoxplain_result$target_column]])",
      "      ",
      "      if (!is_classification && 'RMSE' %in% metric_names) {",
      "        best_score_info <- list(name = 'RMSE', value = round(perf_metrics[['RMSE']], 4))",
      "      } else if (!is_classification && 'rmse' %in% metric_names) {",
      "        best_score_info <- list(name = 'RMSE', value = round(perf_metrics[['rmse']], 4))",
      "      } else if (is_classification && 'AUC' %in% metric_names) {",
      "        best_score_info <- list(name = 'AUC', value = round(perf_metrics[['AUC']], 4))",
      "      } else if (is_classification && 'auc' %in% metric_names) {",
      "        best_score_info <- list(name = 'AUC', value = round(perf_metrics[['auc']], 4))",
      "      } else {",
      "        best_score_info <- list(name = metric_names[1], value = round(perf_metrics[[1]], 4))",
      "      }",
      "    }",
      "    ",
      "    # Create compact summary HTML",
      "    summary_html <- paste0(",
      "      '<div style=\"background-color: #f8f9fa; padding: 6px; border-radius: 6px; border-left: 4px solid #28a745; font-family: Arial, sans-serif; font-size: 14px; line-height: 1.25;\">',",
      "      '<div style=\"margin: 6px 0;\"><strong>🎯 Target:</strong> ', autoxplain_result$target_column, '</div>',",
      "      '<div style=\"margin: 6px 0;\"><strong>📊 Dataset:</strong> ', nrow(autoxplain_result$training_data), ' rows × ', (ncol(autoxplain_result$training_data) - 1), ' features</div>',",
      "      '<div style=\"margin: 6px 0;\"><strong>🏆 Best Model:</strong> ', gsub('_.*', '', best_algorithm), '</div>',",
      "      '<div style=\"margin: 6px 0;\"><strong>📈 Performance:</strong> ', best_score_info$name, ' ', best_score_info$value, '</div>',",
      "      '<div style=\"margin: 6px 0;\"><strong>🤖 Total Models:</strong> ', model_count, ' trained</div>',",
      "      '<div style=\"margin: 6px 0;\"><strong>⏱️ Training Time:</strong> ', total_training_time, 's</div>',",
      "      '</div>'",
      "    )",
      "  } else {",
      "    summary_html <- '<div style=\"padding: 12px; color: #666; font-style: italic;\">Pipeline summary not available</div>'",
      "  }",
      "} else {",
      "  summary_html <- '<div style=\"padding: 12px; color: #666; font-style: italic;\">Model data not available</div>'",
      "}",
      "",
      "htmltools::HTML(summary_html)",
      "```"
    )
  } else {
    # Add explanation section when LLM report is not available
    rmd_content <- c(rmd_content,
      "",
      "## Column {data-width=500}",
      "",
      "### 📖 Understanding Your Results {data-height=675}",
      "",
      "```{r results-explanation}",
      "explanation_html <- paste0(",
      "  '<div style=\"background-color: #f8f9fa; padding: 20px; border-radius: 10px; border-left: 5px solid #2E86AB; font-family: Arial, sans-serif; line-height: 1.6;\">',",
      "  '<h4 style=\"color: #2E86AB; margin-top: 0;\">🎯 What This Analysis Shows</h4>',",
      "  '<p><strong>Model Performance:</strong> The scatter plot above shows how different algorithms performed. ',",
      "  'Each bubble represents a trained model, with the best performing model ranked #1.</p>',",
      "  '<p><strong>Feature Importance:</strong> The colored bar chart shows which features have the most impact on predictions. ',",
      "  'Green bars indicate high-impact features that strongly influence the models decisions.</p>',",
      "  '<p><strong>Algorithm Diversity:</strong> We trained multiple types of models (e.g., Random Forest, XGBoost) ',",
      "  'to find the best approach for your specific dataset.</p>',",
      "  '<h4 style=\"color: #2E86AB;\">💡 Key Insights</h4>',",
      "  '<ul>',",
      "  '<li><strong>Best Model:</strong> Rank #1 model achieved the best performance and should be used for predictions</li>',",
      "  '<li><strong>Important Features:</strong> Focus on the green-colored features as they drive model decisions</li>',",
      "  '<li><strong>Model Reliability:</strong> Multiple algorithms were tested to ensure robust results</li>',",
      "  '</ul>',",
      "  '</div>'",
      ")",
      "htmltools::HTML(explanation_html)",
      "```",
      "",
      "### 📋 Pipeline Summary {data-height=325}",
      "",
      "```{r pipeline-summary-fallback}",
      "# Compact pipeline summary for fallback case",
      "if (!is.null(model_characteristics)) {",
      "  summary_info <- attr(model_characteristics, 'summary')",
      "  if (!is.null(summary_info)) {",
      "    best_algorithm <- if (!is.null(model_characteristics[[1]]$algorithm)) {",
      "      model_characteristics[[1]]$algorithm",
      "    } else { 'Unknown' }",
      "    ",
      "    model_count <- length(model_characteristics)",
      "    total_training_time <- if (!is.null(summary_info$total_training_time_s)) {",
      "      round(summary_info$total_training_time_s, 2)",
      "    } else { 'Unknown' }",
      "    ",
      "    # Get best model performance",
      "    best_score_info <- list(name = '', value = 'N/A')",
      "    if (!is.null(model_characteristics[[1]]$performance_metrics)) {",
      "      perf_metrics <- model_characteristics[[1]]$performance_metrics",
      "      metric_names <- names(perf_metrics)",
      "      is_classification <- !is.numeric(autoxplain_result$training_data[[autoxplain_result$target_column]])",
      "      ",
      "      if (!is_classification && 'RMSE' %in% metric_names) {",
      "        best_score_info <- list(name = 'RMSE', value = round(perf_metrics[['RMSE']], 4))",
      "      } else if (!is_classification && 'rmse' %in% metric_names) {",
      "        best_score_info <- list(name = 'RMSE', value = round(perf_metrics[['rmse']], 4))",
      "      } else if (is_classification && 'AUC' %in% metric_names) {",
      "        best_score_info <- list(name = 'AUC', value = round(perf_metrics[['AUC']], 4))",
      "      } else if (is_classification && 'auc' %in% metric_names) {",
      "        best_score_info <- list(name = 'AUC', value = round(perf_metrics[['auc']], 4))",
      "      } else {",
      "        best_score_info <- list(name = metric_names[1], value = round(perf_metrics[[1]], 4))",
      "      }",
      "    }",
      "    ",
      "    # Create compact summary HTML",
      "    summary_html <- paste0(",
      "      '<div style=\"background-color: #f8f9fa; padding: 10px; border-radius: 8px; border-left: 4px solid #28a745; font-family: Arial, sans-serif; font-size: 14px; line-height: 1.4;\">',",
      "      '<div style=\"margin: 6px 0;\"><strong>🎯 Target:</strong> ', autoxplain_result$target_column, '</div>',",
      "      '<div style=\"margin: 6px 0;\"><strong>📊 Dataset:</strong> ', nrow(autoxplain_result$training_data), ' rows × ', (ncol(autoxplain_result$training_data) - 1), ' features</div>',",
      "      '<div style=\"margin: 6px 0;\"><strong>🏆 Best Model:</strong> ', gsub('_.*', '', best_algorithm), '</div>',",
      "      '<div style=\"margin: 6px 0;\"><strong>📈 Performance:</strong> ', best_score_info$name, ' ', best_score_info$value, '</div>',",
      "      '<div style=\"margin: 6px 0;\"><strong>🤖 Total Models:</strong> ', model_count, ' trained</div>',",
      "      '<div style=\"margin: 6px 0;\"><strong>⏱️ Training Time:</strong> ', total_training_time, 's</div>',",
      "      '</div>'",
      "    )",
      "  } else {",
      "    summary_html <- '<div style=\"padding: 12px; color: #666; font-style: italic;\">Pipeline summary not available</div>'",
      "  }",
      "} else {",
      "  summary_html <- '<div style=\"padding: 12px; color: #666; font-style: italic;\">Model data not available</div>'",
      "}",
      "",
      "htmltools::HTML(summary_html)",
      "```"
    )
  }
  
  # Add Enhanced Partial Dependence page
  rmd_content <- c(rmd_content,
    "",
    "# Feature Effects {data-icon=\"fa-line-chart\"}",
    "",
    "## Column {data-width=700}",
    "",
    "### 📈 How Features Impact Predictions",
    "",
    "```{r feature-effects-plots}",
    "# Load models for dropdown",
    paste0("models_analyzed <- readRDS('", data_files$models_analyzed, "')"),
    paste0("pdp_list <- readRDS('", data_files$pdp_list, "')"),
    paste0("metadata <- readRDS('", data_files$metadata, "')"),
    "",
    "# Create simple model names for dropdown",
    "model_display_names <- sapply(models_analyzed, function(name) {",
    "  if (grepl('StackedEnsemble_AllModels', name)) {",
    "    'Stacked Ensemble (All Models)'",
    "  } else if (grepl('StackedEnsemble_BestOfFamily', name)) {",
    "    'Stacked Ensemble (Best of Family)'", 
    "  } else if (grepl('XGBoost', name)) {",
    "    'XGBoost'",
    "  } else if (grepl('GBM', name)) {",
    "    'Gradient Boosting Machine'",
    "  } else if (grepl('GLM', name)) {",
    "    'Generalized Linear Model'",
    "  } else if (grepl('DeepLearning', name)) {",
    "    'Deep Learning Neural Network'",
    "  } else if (grepl('DRF', name)) {",
    "    'Distributed Random Forest'",
    "  } else {",
    "    paste('Model:', gsub('_.*', '', name))",
    "  }",
    "})",
    "",
    "# Create compact dropdown selector at the top",
    "htmltools::div(",
    "  style = 'margin-bottom: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 6px;',",
    "  htmltools::div(",
    "    style = 'display: flex; align-items: center; gap: 10px;',",
    "    htmltools::strong('Choose Model to Analyze:', style = 'color: #2E86AB; flex-shrink: 0;'),",
    "    htmltools::tags$select(",
    "      id = 'model-selector-pdp',",
    "      onchange = 'updatePDPPlots()',",
    "      style = 'flex-grow: 1; padding: 6px; border: 1px solid #ddd; border-radius: 4px;',",
    "      c(",
    "        list(htmltools::tags$option(value = 'ALL_MODELS', selected = 'selected', 'Top 3 Models')),",
    "        lapply(seq_along(models_analyzed), function(i) {",
    "          htmltools::tags$option(",
    "            value = models_analyzed[i],",
    "            model_display_names[i]",
    "          )",
    "        })",
    "      )",
    "    )",
    "  ),",
    "  htmltools::tags$script(htmltools::HTML('function updatePDPPlots() { var selector = document.getElementById(\"model-selector-pdp\"); var selectedModel = selector.value; var allModelsContainer = document.getElementById(\"pdp-all-models\"); var allIndividualContainers = document.querySelectorAll(\"[id^=pdp-model-]\"); if (selectedModel === \"ALL_MODELS\") { if (allModelsContainer) allModelsContainer.style.display = \"block\"; for (var i = 0; i < allIndividualContainers.length; i++) { allIndividualContainers[i].style.display = \"none\"; } } else { if (allModelsContainer) allModelsContainer.style.display = \"none\"; for (var i = 0; i < allIndividualContainers.length; i++) { allIndividualContainers[i].style.display = \"none\"; } var cleanModelName = selectedModel.replace(/[^a-zA-Z0-9]/g, \"-\"); var targetContainer = document.getElementById(\"pdp-model-\" + cleanModelName); if (targetContainer) { targetContainer.style.display = \"block\"; } } }'))",
    ")",
    "# Get target_column from metadata if not already available",
    "if (!exists('target_column') || is.null(target_column)) {",
    "  target_column <- metadata$target_column",
    "}",
    "if (is.null(target_column)) {",
    "  target_column <- 'Target'",
    "}",
    "",
    "# Define color palette for different models",
    "model_colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf')",
    "",
    "# First, create the combined 'All Models' view",
    "all_models_container <- NULL",
    "if (length(pdp_list) > 0) {",
    "  # Get unique features across all models",
    "  all_features <- unique(unlist(lapply(pdp_list, names)))",
    "  combined_plots <- list()",
    "  ",
    "  for (feature_idx in seq_along(all_features)[1:min(4, length(all_features))]) {",
    "    feature_name <- all_features[feature_idx]",
    "    ",
    "    # Create multi-model plot for this feature",
    "    p <- plotly::plot_ly()",
    "    ",
    "    # Determine if this feature is categorical by checking first available data",
    "    is_categorical <- FALSE",
    "    for (model_name in names(pdp_list)) {",
    "      if (!is.null(pdp_list[[model_name]][[feature_name]])) {",
    "        sample_data <- pdp_list[[model_name]][[feature_name]]",
    "        if (!is.null(sample_data) && nrow(sample_data) > 0) {",
    "          is_categorical <- is.factor(sample_data[,1]) || is.character(sample_data[,1])",
    "          break",
    "        }",
    "      }",
    "    }",
    "    ",
    "    # Add traces for top 3 models only in combined view",
    "    top_3_models <- names(pdp_list)[1:min(3, length(pdp_list))]",
    "    for (model_idx in seq_along(top_3_models)) {",
    "      model_name <- top_3_models[model_idx]",
    "      model_pdp <- pdp_list[[model_name]]",
    "      ",
    "      if (!is.null(model_pdp[[feature_name]])) {",
    "        data <- model_pdp[[feature_name]]",
    "        if (!is.null(data) && nrow(data) > 1) {",
    "          # Create friendly model name for legend",
    "          display_name <- if (grepl('StackedEnsemble_AllModels', model_name)) {",
    "            'Ensemble (All)'",
    "          } else if (grepl('StackedEnsemble_BestOfFamily', model_name)) {",
    "            'Ensemble (Best)'",
    "          } else if (grepl('XGBoost', model_name)) {",
    "            'XGBoost'",
    "          } else if (grepl('GBM', model_name)) {",
    "            'GBM'",
    "          } else if (grepl('GLM', model_name)) {",
    "            'GLM'",
    "          } else if (grepl('DeepLearning', model_name)) {",
    "            'DeepLearning'",
    "          } else if (grepl('DRF', model_name)) {",
    "            'Random Forest'",
    "          } else {",
    "            substr(gsub('_.*', '', model_name), 1, 15)",
    "          }",
    "          ",
    "          # Use different plot types based on feature type",
    "          if (is_categorical) {",
    "            p <- p %>% plotly::add_trace(",
    "              x = data[,1], y = data[,2],",
    "              type = 'bar',",
    "              name = display_name,",
    "              marker = list(color = model_colors[model_idx %% length(model_colors) + 1], opacity = 0.7),",
    "              hovertemplate = paste0('<b>', display_name, '</b><br>',",
    "                                    'Category: %{x}<br>',",
    "                                    'Predicted ', ifelse(is.null(target_column), 'Target', target_column), ': %{y:.3f}<extra></extra>')",
    "            )",
    "          } else {",
    "            p <- p %>% plotly::add_trace(",
    "              x = data[,1], y = data[,2],",
    "              type = 'scatter', mode = 'lines+markers',",
    "              name = display_name,",
    "              line = list(color = model_colors[model_idx %% length(model_colors) + 1], width = 2),",
    "              marker = list(color = model_colors[model_idx %% length(model_colors) + 1], size = 4),",
    "              hovertemplate = paste0('<b>', display_name, '</b><br>',",
    "                                    feature_name, ': %{x}<br>',",
    "                                    'Predicted ', ifelse(is.null(target_column), 'Target', target_column), ': %{y:.3f}<extra></extra>')",
    "            )",
    "          }",
    "        }",
    "      }",
    "    }",
    "    ",
    "    # Finalize plot layout - more compact for grid",
    "    xaxis_config <- if (is_categorical) {",
    "      list(title = feature_name, titlefont = list(size = 9), tickangle = -45, tickfont = list(size = 8))",
    "    } else {",
    "      list(title = feature_name, titlefont = list(size = 9))",
    "    }",
    "    ",
    "    margin_config <- if (is_categorical) {",
    "      list(l = 40, r = 10, t = 30, b = 50)  # More top margin to push plot area down",
    "    } else {",
    "      list(l = 40, r = 10, t = 30, b = 50)",
    "    }",
    "    ",
    "    legend_config <- if (is_categorical) {",
    "      list(orientation = 'h', x = 0.5, y = 1.1, xanchor = 'center', font = list(size = 8))  # Higher position for categorical",
    "    } else {",
    "      list(orientation = 'h', x = 0.5, y = -0.35, xanchor = 'center', font = list(size = 8))  # Bottom position for numerical",
    "    }",
    "    ",
    "    p <- p %>% plotly::layout(",
    "      title = if (is_categorical) {",
    "        list(text = paste0('<b>', feature_name, '</b>'), font = list(size = 12), y = 1.2)  # Push title up for categorical",
    "      } else {",
    "        list(text = paste0('<b>', feature_name, '</b>'), font = list(size = 12))",
    "      },",
    "      xaxis = xaxis_config,",
    "      yaxis = if (is_categorical) {",
    "        # Calculate 20% padding above max value for categorical plots",
    "        max_val <- max(data[,2], na.rm = TRUE)",
    "        list(",
    "          title = paste('Predicted', ifelse(is.null(target_column), 'Target', target_column)), ",
    "          titlefont = list(size = 9),",
    "          range = c(0, max_val * 1.2)  # 20% extra space at top",
    "        )",
    "      } else {",
    "        list(title = paste('Predicted', ifelse(is.null(target_column), 'Target', target_column)), titlefont = list(size = 9))",
    "      },",
    "      legend = legend_config,",
    "      margin = margin_config,",
    "      height = 220,",
    "      width = 350,",
    "      hovermode = 'x unified'",
    "    ) %>%",
    "    plotly::config(displayModeBar = FALSE, responsive = TRUE)",
    "    ",
    "    combined_plots[[feature_idx]] <- p",
    "  }",
    "  ",
    "  # Create container for all models view with 2x2 grid",
    "  all_models_container <- htmltools::div(",
    "    id = 'pdp-all-models',",
    "    style = 'display: block;',",
    "    htmltools::h4('Comparing Top 3 Models - Top 4 Features', style = 'color: #2E86AB; margin-bottom: 15px;'),",
    "    htmltools::div(",
    "      style = 'display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 15px; width: 100%;',",
    "      htmltools::tagList(combined_plots)",
    "    )",
    "  )",
    "}",
    "",
    "# Then create individual model containers",
    "individual_containers <- list()",
    "for (model_idx in seq_along(models_analyzed)) {",
    "  model_name <- models_analyzed[model_idx]",
    "  pdp_data <- if(!is.null(pdp_list[[model_name]])) pdp_list[[model_name]] else NULL",
    "  ",
    "  # Create clean model name for HTML ID",
    "  clean_model_name <- gsub('[^a-zA-Z0-9]', '-', model_name)",
    "  container_id <- paste0('pdp-model-', clean_model_name)",
    "  ",
    "  if (!is.null(pdp_data) && length(pdp_data) > 0) {",
    "    # Create plots for individual model",
    "    feature_names <- names(pdp_data)",
    "    individual_plots <- list()",
    "    ",
    "    for (i in 1:min(4, length(feature_names))) {",
    "      feature_name <- feature_names[i]",
    "      data <- pdp_data[[feature_name]]",
    "      ",
    "      if (!is.null(data) && nrow(data) > 1) {",
    "        # Determine if feature is categorical or numerical",
    "        is_categorical <- is.factor(data[,1]) || is.character(data[,1])",
    "        ",
    "        if (is_categorical) {",
    "          # Use bar plot for categorical features",
    "          p <- plotly::plot_ly(x = data[,1], y = data[,2], ",
    "                              type = 'bar',",
    "                              marker = list(color = model_colors[model_idx %% length(model_colors) + 1], opacity = 0.8),",
    "                              hovertemplate = paste0('<b>', feature_name, '</b><br>',",
    "                                                    'Category: %{x}<br>',",
    "                                                    'Predicted ', ifelse(is.null(target_column), 'Target', target_column), ': %{y:.3f}<extra></extra>')) %>%",
    "            plotly::layout(",
    "              xaxis = list(title = feature_name, titlefont = list(size = 9), ",
    "                          tickangle = -45, tickfont = list(size = 8)),",
    "              margin = list(b = 80)",
    "            )",
    "        } else {",
    "          # Use line plot for numerical features",
    "          p <- plotly::plot_ly(x = data[,1], y = data[,2], ",
    "                              type = 'scatter', mode = 'lines+markers',",
    "                              line = list(color = model_colors[model_idx %% length(model_colors) + 1], width = 2),",
    "                              marker = list(color = model_colors[model_idx %% length(model_colors) + 1], size = 4),",
    "                              hovertemplate = paste0('<b>', feature_name, '</b><br>',",
    "                                                    'Value: %{x}<br>',",
    "                                                    'Predicted ', ifelse(is.null(target_column), 'Target', target_column), ': %{y:.3f}<extra></extra>')) %>%",
    "            plotly::layout(",
    "              xaxis = list(title = feature_name, titlefont = list(size = 9))",
    "            )",
    "        }",
    "        ",
    "        # Apply common layout settings",
    "        # Adjust legend position based on feature type",
    "        legend_pos <- if (is_categorical) {",
    "          list(orientation = 'h', x = 0.5, y = 1.35, xanchor = 'center', font = list(size = 8))  # Higher position for categorical",
    "        } else {",
    "          list(orientation = 'h', x = 0.5, y = -0.35, xanchor = 'center', font = list(size = 8))  # Bottom for numerical",
    "        }",
    "        ",
    "        margin_settings <- if (is_categorical) {",
    "          list(l = 40, r = 10, t = 30, b = 100)  # Increased top margin for legend clearance",
    "        } else {",
    "          list(l = 40, r = 10, t = 30, b = 100)",
    "        }",
    "        ",
    "        p <- p %>%",
    "          plotly::layout(",
    "            title = list(text = paste0('<b>', feature_name, '</b>'), font = list(size = 12)),",
    "            yaxis = list(title = paste('Predicted', ifelse(is.null(target_column), 'Target', target_column)), titlefont = list(size = 9)),",
    "            legend = legend_pos,",
    "            margin = margin_settings,",
    "            height = 220,",
    "            width = 350,",
    "            hovermode = 'x unified'",
    "          ) %>%",
    "          plotly::config(displayModeBar = FALSE, responsive = TRUE)",
    "        ",
    "        individual_plots[[i]] <- p",
    "      }",
    "    }",
    "    ",
    "    individual_containers[[model_idx]] <- htmltools::div(",
    "      id = container_id,",
    "      style = 'display: none;',  # Hidden by default",
    "      htmltools::h4(paste('Model:', gsub('_.*', '', model_name)), style = 'color: #2E86AB; margin-bottom: 15px;'),",
    "      htmltools::div(",
    "        style = 'display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 15px; width: 100%;',",
    "        htmltools::tagList(individual_plots)",
    "      )",
    "    )",
    "  }",
    "}",
    "",
    "# Display all containers",
    "htmltools::tagList(",
    "  all_models_container,",
    "  individual_containers",
    ")",
    "```",
    "",
    "## Column {data-width=400}",
    "",
    "### 🔍 Feature Impact Analysis {data-height=500}",
    "",
    "```{r feature-insights}",
    paste0("pdp_data <- readRDS('", data_files$pdp_data, "')"),
    paste0("feature_types <- readRDS('", data_files$feature_types, "')"),
    "if (!is.null(pdp_data) && length(pdp_data) > 0) {",
    "  # Generate comprehensive insights",
    "  insights_html <- '<div style=\"font-family: Arial, sans-serif; line-height: 1.4; padding: 15px; background-color: #f8f9fa; border-radius: 8px; border-left: 4px solid #2196f3;\">'",
    "  insights_html <- paste0(insights_html, '<h4 style=\"color: #1976d2; margin-top: 0;\">🎯 Key Insights</h4>')",
    "  ",
    "  feature_names <- names(pdp_data)",
    "  n_features <- length(feature_names)",
    "  ",
    "  for (i in 1:min(4, n_features)) {",
    "    feature_name <- feature_names[i]",
    "    data <- pdp_data[[feature_name]]",
    "    ",
    "    if (!is.null(data) && nrow(data) > 1) {",
    "      y_range <- max(data[,2]) - min(data[,2])",
    "      direction <- ifelse(data[nrow(data), 2] > data[1, 2], 'increase', 'decrease')",
    "      magnitude <- round(y_range, 2)",
    "      ",
    "      # Determine impact and recommendations",
    "      if (y_range > 2) {",
    "        impact_desc <- 'CRITICAL DRIVER'",
    "        recommendation <- 'High-impact feature'",
    "        color <- '#d32f2f'",
    "      } else if (y_range > 0.5) {",
    "        impact_desc <- 'IMPORTANT FACTOR'",
    "        recommendation <- 'Moderate impact'",
    "        color <- '#f57c00'",
    "      } else {",
    "        impact_desc <- 'MINOR INFLUENCE'",
    "        recommendation <- 'Low impact'",
    "        color <- '#388e3c'",
    "      }",
    "      ",
    "      # Get feature type and create appropriate explanation",
    "      feature_type <- ifelse(feature_name %in% names(feature_types), feature_types[[feature_name]], 'numerical')",
    "      ",
    "      # Create explanation based on feature type",
    "      if (feature_type == 'categorical') {",
    "        explanation <- paste0('Different categories show varying impact on predictions (range: ~', magnitude, ' units)')",
    "      } else {",
    "        explanation <- paste0('As values increase by one unit, predictions <strong>', direction, '</strong> by ~', magnitude, ' units')",
    "      }",
    "      ",
    "      insights_html <- paste0(insights_html, ",
    "        '<div style=\"margin: 4px 0; padding: 6px; background: white; border-radius: 4px; border-left: 3px solid ', color, ';\">',",
    "        '<strong style=\"color: ', color, ';\">📌 ', feature_name, '</strong> <span style=\"font-size: 0.8em; color: #888;\">(', feature_type, ')</span><br>',",
    "        explanation,",
    "        '</div>')",
    "    }",
    "  }",
    "  ",
    "  insights_html <- paste0(insights_html, '</div>')",
    "  ",
    "  htmltools::HTML(insights_html)",
    "} else {",
    "  htmltools::HTML('<div style=\"text-align: center; padding: 40px; color: #666;\">📊<br><br>Feature impact analysis not available for this dataset.</div>')",
    "}",
    "```",
    "",
    "### 📋 Feature Summary {data-height=200}",
    "",
    "These are **Partial Dependence Plots (PDPs)** showing how each feature affects model predictions on average. Steep curves indicate high feature impact (small changes in feature values lead to large prediction changes), while flat curves suggest low impact. Use these insights to identify which features are most influential, understand non-linear relationships, guide feature engineering, and validate that models have learned sensible patterns."
  )
  
  # Add Model Correlations page
  rmd_content <- c(rmd_content,
    "",
    "# Model Correlations {data-icon=\"fa-sitemap\"}",
    "",
    "## Column {data-width=700}",
    "",
    "### 📊 Model Prediction Correlation Heatmap",
    "```{r model-correlations}",
    "# Load the autoxplain result to generate correlations",
    "tryCatch({",
    "  # Create correlation heatmap with suppressed H2O output",
    "  invisible(capture.output({",
    "    correlation_plot <- plot_model_correlations(autoxplain_result)",
    "  }))",
    "  correlation_plot",
    "}, error = function(e) {",
    "  plotly::plot_ly() %>%",
    "    plotly::add_text(x = 0.5, y = 0.5, ",
    "                     text = paste('Model correlation analysis unavailable:', e$message),",
    "                     textposition = 'middle center',",
    "                     textfont = list(size = 14, color = '#666')) %>%",
    "    plotly::layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),",
    "                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%",
    "    plotly::config(displayModeBar = FALSE)",
    "})",
    "```",
    "",
    "## Column {data-width=400}",
    "",
    "### 🔍 Correlation Analysis Insights {data-height=500}",
    "",
    "```{r correlation-insights, echo=FALSE, results='asis', message=FALSE, warning=FALSE}",
    "# Display pre-calculated correlation insights (no H2O operations in R markdown)",
    "correlation_insights_html <- readRDS(file.path(temp_dir, 'correlation_insights_html.rds'))",
    "htmltools::HTML(correlation_insights_html)",
    "```",
    "",
    "### 📋 Ensemble Strategy {data-height=200}",
    "",
    "This **correlation heatmap** shows prediction similarity between models (Pearson correlation of predictions on validation data). Low correlation (< 0.7) indicates diverse models that capture different patterns - ideal for ensemble methods as they can compensate for each other's weaknesses. High correlation (> 0.9) suggests models make similar predictions. Use this to understand which algorithms learn similar patterns from your data.")
  
  
  # Add Model Characteristics page
  rmd_content <- c(rmd_content,
    "",
    "# Model Characteristics {data-icon=\"fa-cogs\"}",
    "",
    "## Column {data-width=600}",
    "",
    "### 📊 Model Comparison & Hyperparameters {data-height=520}",
    "",
    "```{r model-characteristics}",
    "# Helper functions for model characteristics",
    "get_important_hyperparams <- function(algorithm, hyperparams) {",
    "  # Select most important hyperparameters based on algorithm type",
    "  important_keys <- switch(tolower(algorithm),",
    "    'xgboost' = c('ntrees', 'max_depth', 'learn_rate', 'sample_rate'),",
    "    'gbm' = c('ntrees', 'max_depth', 'learn_rate', 'sample_rate'),",
    "    'randomforest' = c('ntrees', 'max_depth', 'sample_rate'),",
    "    'drf' = c('ntrees', 'max_depth', 'sample_rate'),",
    "    'glm' = c('family', 'alpha', 'lambda'),",
    "    'deeplearning' = c('epochs', 'hidden', 'activation', 'l1', 'l2'),",
    "    'stackedensemble' = c('base_models', 'metalearner_algorithm'),",
    "    head(names(hyperparams), 3)  # Default to first 3 parameters",
    "  )",
    "  available_params <- intersect(names(hyperparams), important_keys)",
    "  if (length(available_params) == 0) {",
    "    return(head(hyperparams, 3))",
    "  }",
    "  return(hyperparams[available_params])",
    "}",
    "",
    "filter_relevant_metrics <- function(metrics, is_classification) {",
    "  if (is_classification) {",
    "    # Classification: AUC + accuracy/logloss + precision/recall (don't overdo it)",
    "    priority_metrics <- c('AUC', 'auc', 'accuracy', 'logloss', 'precision', 'recall', 'f1', 'mean_per_class_error')",
    "  } else {",
    "    # Regression: Core performance + additional metric like MAE",
    "    priority_metrics <- c('RMSE', 'rmse', 'mae', 'MAE', 'mean_residual_deviance', 'r2', 'R2', 'AIC', 'aic')",
    "  }",
    "  available_metrics <- intersect(names(metrics), priority_metrics)",
    "  if (length(available_metrics) == 0) {",
    "    return(head(metrics, 2))",
    "  }",
    "  # Prioritize performance metrics, then additional ones",
    "  primary_metrics <- if (is_classification) {",
    "    intersect(available_metrics, c('AUC', 'auc', 'accuracy', 'logloss'))",
    "  } else {",
    "    intersect(available_metrics, c('RMSE', 'rmse', 'mae', 'MAE', 'r2', 'R2'))",
    "  }",
    "  additional_metrics <- if (is_classification) {",
    "    intersect(available_metrics, c('precision', 'recall', 'f1'))",
    "  } else {",
    "    intersect(available_metrics, c('AIC', 'aic'))",
    "  }",
    "  ",
    "  # Combine metrics: up to 3 performance + 1 additional",
    "  selected_metrics <- c(head(primary_metrics, 3), head(additional_metrics, 1))",
    "  result_metrics <- metrics[selected_metrics[selected_metrics != '']]",
    "  ",
    "  # Remove redundant metrics (keep RMSE, remove MSE)",
    "  if ('RMSE' %in% names(result_metrics) && 'MSE' %in% names(result_metrics)) {",
    "    result_metrics <- result_metrics[!names(result_metrics) %in% 'MSE']",
    "  }",
    "  if ('rmse' %in% names(result_metrics) && 'mse' %in% names(result_metrics)) {",
    "    result_metrics <- result_metrics[!names(result_metrics) %in% 'mse']",
    "  }",
    "  ",
    "  return(result_metrics)",
    "}",
    "",
    "clean_algorithm_name <- function(algorithm) {",
    "  switch(tolower(algorithm),",
    "    'stackedensemble_allmodels' = 'Stacked Ensemble (All)',",
    "    'stackedensemble_bestoffamily' = 'Stacked Ensemble (Best)',",
    "    'stackedensemble' = 'Stacked Ensemble',",
    "    'xgboost' = 'XGBoost',",
    "    'gbm' = 'Gradient Boosting',",
    "    'randomforest' = 'Random Forest',",
    "    'drf' = 'Random Forest',",
    "    'glm' = 'Linear Model',",
    "    'deeplearning' = 'Neural Network',",
    "    algorithm",
    "  )",
    "}",
    "",
    "if (!is.null(model_characteristics)) {",
    "  # Create comprehensive model characteristics table with proper ranking",
    "  char_data <- data.frame(",
    "    Algorithm = character(),",
    "    Training_Time_s = numeric(),",
    "    Key_Hyperparameters = character(),",
    "    Performance = character(),",
    "    Performance_Score = numeric(),",
    "    Training_Score = numeric(),",
    "    stringsAsFactors = FALSE",
    "  )",
    "  ",
    "  # Determine task type for proper metric selection",
    "  is_classification <- !is.numeric(autoxplain_result$training_data[[autoxplain_result$target_column]])",
    "  primary_metric <- if (is_classification) 'auc' else 'rmse'",
    "  metric_direction <- if (is_classification) 'desc' else 'asc'  # AUC higher is better, RMSE lower is better",
    "  ",
    "  for (i in seq_along(model_characteristics)) {",
    "    model <- model_characteristics[[i]]",
    "    ",
    "    # Extract key hyperparameters with cleaner formatting",
    "    hyperparams_text <- '<span style=\"color: #888;\">Default parameters</span>'",
    "    if (!is.null(model$hyperparameters)) {",
    "      # Select most important hyperparameters based on algorithm",
    "      important_params <- get_important_hyperparams(model$algorithm, model$hyperparameters)",
    "      if (length(important_params) > 0) {",
    "        param_strs <- character()",
    "        for (param in names(important_params)) {",
    "          value <- important_params[[param]]",
    "          # Clean value formatting",  
    "          if (is.numeric(value)) {",
    "            if (abs(value) < 0.001 && value != 0) {",
    "              value_str <- format(value, scientific = TRUE, digits = 2)",
    "            } else if (abs(value) > 1000000) {",
    "              value_str <- paste0(round(value/1000000, 1), 'M')",
    "            } else if (abs(value) > 1000) {",
    "              value_str <- format(round(value), big.mark = ',')",
    "            } else {",
    "              value_str <- as.character(round(value, 3))",
    "            }",
    "          } else {",
    "            value_str <- as.character(value)",
    "            # Handle long strings with expandable display - show ALL values with context",
    "            if (nchar(value_str) > 30) {",
    "              if (grepl(',', value_str)) {",
    "                # For comma-separated values, create expandable display",
    "                parts <- trimws(strsplit(value_str, ',')[[1]])",
    "                ",
    "                # Check if parts are mostly numeric (like lambda values)",
    "                numeric_parts <- suppressWarnings(as.numeric(parts))",
    "                mostly_numeric <- sum(!is.na(numeric_parts)) > (length(parts) * 0.7)",
    "                ",
    "                if (mostly_numeric) {",
    "                  # For numeric lists, format numbers and show with parameter context",
    "                  formatted_parts <- sapply(parts, function(p) {",
    "                    num_val <- suppressWarnings(as.numeric(p))",
    "                    if (!is.na(num_val)) {",
    "                      if (abs(num_val) < 0.001 && num_val != 0) {",
    "                        format(num_val, scientific = TRUE, digits = 2)",
    "                      } else {",
    "                        format(round(num_val, 3), nsmall = 0)",
    "                      }",
    "                    } else {",
    "                      as.character(p)",
    "                    }",
    "                  })",
    "                  ",
    "                  if (length(formatted_parts) > 4) {",
    "                    visible_parts <- paste(formatted_parts[1:3], collapse = ', ')",
    "                    hidden_parts <- paste(formatted_parts[4:length(formatted_parts)], collapse = ', ')",
    "                    value_str <- paste0(visible_parts, '<br><details style=\"margin-top:2px;\"><summary style=\"cursor:pointer;color:#007bff;font-size:11px;\">Show ', length(formatted_parts)-3, ' more ', param, ' values...</summary><div style=\"font-size:11px;margin-top:2px;\">', hidden_parts, '</div></details>')",
    "                  } else {",
    "                    value_str <- paste(formatted_parts, collapse = ', ')",
    "                  }",
    "                } else {",
    "                  # For non-numeric lists (like model names), don't truncate in expansion",
    "                  if (length(parts) > 3) {",
    "                    visible_parts <- paste(parts[1:2], collapse = ', ')",
    "                    # Don't truncate the hidden parts - show ALL remaining values",
    "                    remaining_parts <- parts[3:length(parts)]",
    "                    # Break long model names into multiple lines for readability",
    "                    hidden_display <- paste(remaining_parts, collapse = '<br>')",
    "                    value_str <- paste0(visible_parts, '<br><details style=\"margin-top:2px;\"><summary style=\"cursor:pointer;color:#007bff;font-size:11px;\">Show ', length(remaining_parts), ' more ', param, ' values...</summary><div style=\"font-size:11px;margin-top:2px;line-height:1.3;\">', hidden_display, '</div></details>')",
    "                  } else {",
    "                    value_str <- paste(parts, collapse = '<br>')",
    "                  }",
    "                }",
    "              } else {",
    "                # For other long strings, create expandable section without truncation",
    "                short_str <- substr(value_str, 1, 25)",
    "                remaining_str <- substr(value_str, 26, nchar(value_str))",
    "                value_str <- paste0(short_str, '<details style=\"display:inline;\"><summary style=\"cursor:pointer;color:#007bff;font-size:11px;\">...</summary><span style=\"font-size:11px;\">', remaining_str, '</span></details>')",
    "              }",
    "            }",
    "          }",
    "          param_strs <- c(param_strs, paste0('<b>', param, ':</b> ', value_str))",
    "        }",
    "        hyperparams_text <- paste(param_strs, collapse = '<br>')",
    "      }",
    "    }",
    "    ",
    "    # Extract and clean performance metrics",
    "    performance_text <- '<span style=\"color: #888;\">No metrics available</span>'",
    "    perf_score <- NA",
    "    training_time <- if(!is.null(model$training_time_s)) model$training_time_s else 0.1",
    "    ",
    "    if (!is.null(model$performance_metrics)) {",
    "      # Filter and prioritize metrics based on task type",
    "      relevant_metrics <- filter_relevant_metrics(model$performance_metrics, is_classification)",
    "      metric_strs <- character()",
    "      ",
    "      for (metric_name in names(relevant_metrics)) {",
    "        value <- relevant_metrics[[metric_name]]",
    "        # Store primary metric for ranking (including fallback to information criteria)",
    "        if (tolower(metric_name) == primary_metric || ",
    "            (primary_metric == 'auc' && tolower(metric_name) == 'auc') || ",
    "            (primary_metric == 'rmse' && tolower(metric_name) == 'rmse')) {",
    "          perf_score <- value",
    "        } else if (is.na(perf_score) && !is_classification && tolower(metric_name) %in% c('aic', 'bic')) {",
    "          # Use information criteria as fallback ranking metric for regression",
    "          perf_score <- value",
    "        }",
    "        # Enhanced formatting for different metric types with consistent colors",
    "        is_primary <- tolower(metric_name) %in% c('auc', 'rmse')",
    "        ",
    "        formatted_value <- if (is_primary) {",
    "          paste0('<b>', round(value, 4), '</b>')  # Bold primary metrics",
    "        } else {",
    "          as.character(round(value, 4))  # Consistent formatting for all additional metrics",
    "        }",
    "        metric_strs <- c(metric_strs, paste0('<b>', metric_name, ':</b> ', formatted_value))",
    "      }",
    "      performance_text <- paste(metric_strs, collapse = '<br>')",
    "    }",
    "    ",
    "    char_data <- rbind(char_data, data.frame(",
    "      Algorithm = clean_algorithm_name(model$algorithm),",
    "      Training_Time_s = if(!is.null(model$training_time_s)) round(model$training_time_s, 3) else NA,",
    "      Key_Hyperparameters = hyperparams_text,",
    "      Performance = performance_text,",
    "      Performance_Score = if(!is.na(perf_score)) perf_score else (if (is_classification) 0.5 else 10),",
    "      Training_Score = training_time,",
    "      stringsAsFactors = FALSE",
    "    ))",
    "  }",
    "  ",
    "  # Sort by performance first, then by training time for tiebreaker",
    "  if (metric_direction == 'desc') {",
    "    # For classification (AUC): higher performance is better, lower training time for ties",
    "    char_data <- char_data[order(-char_data$Performance_Score, char_data$Training_Score), ]",
    "  } else {",
    "    # For regression (RMSE): lower error is better, lower training time for ties",  
    "    char_data <- char_data[order(char_data$Performance_Score, char_data$Training_Score), ]",
    "  }",
    "  ",
    "  # Add proper ranking based on sorted performance",
    "  char_data$Rank <- seq_len(nrow(char_data))",
    "  ",
    "  # Create display data with clean column order",
    "  display_data <- char_data[, c('Rank', 'Algorithm', 'Performance', 'Training_Time_s', 'Key_Hyperparameters')]",
    "  # Rename columns for better display",
    "  colnames(display_data) <- c('Rank', 'Algorithm', 'Performance', 'Training Time (s)', 'Key Hyperparameters')",
    "  ",
    "  DT::datatable(display_data, ",
    "                options = list(",
    "                  scrollX = TRUE, ",
    "                  pageLength = 15,  # Show more models",
    "                  dom = 'ftip',  # Remove length selector",
    "                  columnDefs = list(",
    "                    list(width = '50px', targets = 0, className = 'dt-center'),    # Rank",
    "                    list(width = '120px', targets = 1),                            # Algorithm",
    "                    list(width = '180px', targets = 2),                            # Performance",
    "                    list(width = '90px', targets = 3, className = 'dt-center'),    # Training Time",
    "                    list(width = '250px', targets = 4)                             # Hyperparameters",
    "                  ),",
    "                  autoWidth = FALSE,",
    "                  searching = TRUE,",
    "                  ordering = TRUE,",
    "                  order = list(list(0, 'asc'))  # Default sort by rank",
    "                ),",
    "                escape = FALSE,  # Allow HTML formatting",
    "                caption = 'Model Performance Ranking: Sorted by primary metric, then training efficiency',",
    "                rownames = FALSE) %>%",
    "    DT::formatStyle(0, backgroundColor = '#f8f9fa', fontWeight = 'bold')  # Style rank column",
    "} else {",
    "  DT::datatable(data.frame(Message = 'Model characteristics not available'))",
    "}",
    "```",
    "",
    "## Column {data-width=400}",
    "",
    "### ⚙️ Training Summary",
    "",
    "```{r training-summary}",
    "if (!is.null(model_characteristics)) {",
    "  summary_info <- attr(model_characteristics, 'summary')",
    "  if (!is.null(summary_info)) {",
    "    # Get H2O validation details from first model",
    "    h2o_validation_info <- tryCatch({",
    "      h2o_leader <- autoxplain_result$models[[1]]",
    "      ",
    "      # Get validation method",
    "      validation_method <- if (!is.null(h2o_leader@model$cross_validation_metrics)) {",
    "        nfolds <- tryCatch(h2o_leader@parameters$nfolds, error = function(e) 5)",
    "        paste0(nfolds, '-fold Cross-Validation')",
    "      } else {",
    "        'Holdout Validation'",
    "      }",
    "      ",
    "      # Get fold assignment method",
    "      fold_assignment <- tryCatch({",
    "        if (!is.null(h2o_leader@model$cross_validation_metrics)) {",
    "          fold_column <- h2o_leader@parameters$fold_column",
    "          if (!is.null(fold_column)) 'Custom Fold Column' else 'Random Assignment'",
    "        } else {",
    "          'N/A (Holdout)'",
    "        }",
    "      }, error = function(e) 'Random Assignment')",
    "      ",
    "      list(validation_method = validation_method, fold_assignment = fold_assignment)",
    "    }, error = function(e) {",
    "      list(validation_method = '5-fold Cross-Validation', fold_assignment = 'Random Assignment')",
    "    })",
    "    ",
    "    summary_data <- data.frame(",
    "      Metric = c('Total Models Trained', 'Algorithms Used', 'Total Training Time (s)', ",
    "                 'Dataset Rows', 'Dataset Features', 'Validation Method', 'Fold Assignment'),",
    "      Value = c(summary_info$total_models,",
    "                paste(summary_info$algorithms_used, collapse = ', '),",
    "                round(summary_info$total_training_time_s, 2),",
    "                summary_info$dataset_info$n_rows,",
    "                summary_info$dataset_info$n_features,",
    "                h2o_validation_info$validation_method,",
    "                h2o_validation_info$fold_assignment),",
    "      stringsAsFactors = FALSE",
    "    )",
    "  } else {",
    "    # Get H2O validation details even without summary_info",
    "    h2o_validation_info <- tryCatch({",
    "      h2o_leader <- autoxplain_result$models[[1]]",
    "      validation_method <- if (!is.null(h2o_leader@model$cross_validation_metrics)) {",
    "        nfolds <- tryCatch(h2o_leader@parameters$nfolds, error = function(e) 5)",
    "        paste(nfolds, '-fold Cross-Validation')",
    "      } else {",
    "        'Holdout Validation'",
    "      }",
    "      fold_assignment <- tryCatch({",
    "        if (!is.null(h2o_leader@model$cross_validation_metrics)) {",
    "          fold_column <- h2o_leader@parameters$fold_column",
    "          if (!is.null(fold_column)) 'Custom Fold Column' else 'Random Assignment'",
    "        } else {",
    "          'N/A (Holdout)'",
    "        }",
    "      }, error = function(e) 'Random Assignment')",
    "      list(validation_method = validation_method, fold_assignment = fold_assignment)",
    "    }, error = function(e) {",
    "      list(validation_method = '5-fold Cross-Validation', fold_assignment = 'Random Assignment')",
    "    })",
    "    ",
    "    summary_data <- data.frame(",
    "      Metric = c('Total Models Trained', 'Validation Method', 'Fold Assignment'),",
    "      Value = c(model_count, h2o_validation_info$validation_method, h2o_validation_info$fold_assignment)",
    "    )",
    "  }",
    "} else {",
    "  # Extract H2O AutoML technical details",
    "  tryCatch({",
    "    h2o_leader <- autoxplain_result$models[[1]]",
    "    ",
    "    # Get comprehensive cross-validation and resampling information",
    "    cv_details <- 'Not Available'",
    "    validation_method <- 'Holdout Validation'",
    "    fold_assignment <- 'Random'",
    "    ",
    "    # Check for cross-validation metrics",
    "    if (!is.null(h2o_leader@model$cross_validation_metrics)) {",
    "      # Get nfolds parameter",
    "      nfolds <- tryCatch(h2o_leader@parameters$nfolds, error = function(e) 5)",
    "      if (!is.null(nfolds) && nfolds > 1) {",
    "        validation_method <- paste(nfolds, '-fold Cross-Validation')",
    "        ",
    "        # Get fold assignment method",
    "        fold_assignment_method <- tryCatch({",
    "          fold_column <- h2o_leader@parameters$fold_column",
    "          if (!is.null(fold_column)) {",
    "            'Custom Fold Column'",
    "          } else {",
    "            'Random Assignment'",
    "          }",
    "        }, error = function(e) 'Random Assignment')",
    "        ",
    "        fold_assignment <- fold_assignment_method",
    "        ",
    "        # Additional CV details",
    "        cv_details <- paste('Stratified sampling with', nfolds, 'folds')",
    "      }",
    "    }",
    "    ",
    "    # Get stopping criteria and training details",
    "    stopping_criteria <- tryCatch({",
    "      stopping_rounds <- h2o_leader@parameters$stopping_rounds",
    "      stopping_metric <- h2o_leader@parameters$stopping_metric",
    "      if (!is.null(stopping_rounds) && stopping_rounds > 0) {",
    "        paste('Early stopping after', stopping_rounds, 'rounds')",
    "      } else {",
    "        'Performance-based convergence'",
    "      }",
    "    }, error = function(e) 'Performance-based convergence')",
    "    ",
    "    # Get metric computation details",
    "    metric_computation <- tryCatch({",
    "      if (!is.null(h2o_leader@model$cross_validation_metrics)) {",
    "        'CV holdout predictions aggregated'",
    "      } else {",
    "        'Validation set evaluation'",
    "      }",
    "    }, error = function(e) 'Standard H2O validation')",
    "    ",
    "    summary_data <- data.frame(",
    "      Metric = c('Analysis Date', 'Target Column', 'Task Type', 'Models Generated', ",
    "                 'Validation Method', 'Fold Assignment', 'Metric Computation', ",
    "                 'Cross-Validation Details', 'Stopping Criteria', 'H2O Version'),",
    "      Value = c(as.character(Sys.Date()), target_column, task_type, as.character(model_count),",
    "                validation_method, fold_assignment, metric_computation, cv_details,",
    "                stopping_criteria, as.character(h2o::h2o.getVersion()$version_number)),",
    "      stringsAsFactors = FALSE",
    "    )",
    "  }, error = function(e) {",
    "    summary_data <- data.frame(",
    "      Metric = c('Analysis Date', 'Target Column', 'Task Type', 'Models Generated'),",
    "      Value = c(as.character(Sys.Date()), target_column, task_type, as.character(model_count))",
    "    )",
    "  })",
    "}",
    "DT::datatable(summary_data, options = list(dom = 't'), rownames = FALSE)",
    "```",
    "",
    "### 💡 Model Insights",
    "",
    "```{r model-insights}",
    "if (!is.null(model_characteristics) && length(model_characteristics) > 0) {",
    "  # Create performance insights visualization with clear ranking criteria",
    "  insights_data <- data.frame(",
    "    Criterion = c('Top Performer', 'Fastest Training', 'Most Efficient', 'Best Balance'),",
    "    Model = character(4),",
    "    Explanation = character(4),",
    "    stringsAsFactors = FALSE",
    "  )",
    "  ",
    "  # Find top performing model (best primary metric)",
    "  performance_rank <- if(is_classification) {",
    "    order(-char_data$Performance_Score)",
    "  } else {",
    "    order(char_data$Performance_Score)",
    "  }",
    "  top_model_idx <- performance_rank[1]",
    "  insights_data$Model[1] <- char_data$Algorithm[top_model_idx]",
    "  primary_metric <- if(is_classification) 'AUC' else 'RMSE'",
    "  insights_data$Explanation[1] <- paste('Best', primary_metric, 'score:', round(char_data$Performance_Score[top_model_idx], 3))",
    "  ",
    "  # Find fastest training model (minimum training time)",
    "  fastest_idx <- which.min(char_data$Training_Score)",
    "  insights_data$Model[2] <- char_data$Algorithm[fastest_idx]",
    "  insights_data$Explanation[2] <- paste('Shortest training time:', round(char_data$Training_Score[fastest_idx], 3), 'seconds')",
    "  ",
    "  # Find most efficient model (best performance-to-training-time efficiency)",
    "  efficiency_scores <- if(is_classification) {",
    "    # For classification: AUC/time (higher AUC is better, lower time is better)",
    "    char_data$Performance_Score / char_data$Training_Score",
    "  } else {",
    "    # For regression: 1/(RMSE * time) - rewards both low RMSE and low time",
    "    1 / (char_data$Performance_Score * char_data$Training_Score)",
    "  }",
    "  most_efficient_idx <- which.max(efficiency_scores)",
    "  insights_data$Model[3] <- char_data$Algorithm[most_efficient_idx]",
    "  ",
    "  # Create more meaningful explanation based on task type",
    "  if(is_classification) {",
    "    insights_data$Explanation[3] <- paste('Highest AUC/time ratio:', round(efficiency_scores[most_efficient_idx], 4))",
    "  } else {",
    "    # For regression, show the actual values to make it more interpretable",
    "    rmse_val <- char_data$Performance_Score[most_efficient_idx]",
    "    time_val <- char_data$Training_Score[most_efficient_idx]",
    "    insights_data$Explanation[3] <- paste0('Lowest 1/(RMSE * time) - RMSE: ', round(rmse_val, 3), ', Time: ', round(time_val, 3), 's')",
    "  }",
    "  ",
    "  # Best balance (among top 3 performers, pick fastest)",
    "  top3_indices <- head(performance_rank, 3)",
    "  balanced_idx <- top3_indices[which.min(char_data$Training_Score[top3_indices])]",
    "  insights_data$Model[4] <- char_data$Algorithm[balanced_idx]",
    "  insights_data$Explanation[4] <- paste('Top-3 performer with fastest training (rank', which(performance_rank == balanced_idx), 'in performance)')",
    "  ",
    "  DT::datatable(insights_data, ",
    "                options = list(",
    "                  dom = 't',  # Only table, no search/pagination",
    "                  columnDefs = list(",
    "                    list(width = '120px', targets = 0, className = 'dt-left'),",
    "                    list(width = '150px', targets = 1, className = 'dt-center'),",
    "                    list(width = '300px', targets = 2, className = 'dt-left')",
    "                  ),",
    "                  autoWidth = FALSE",
    "                ),",
    "                escape = FALSE,",
    "                rownames = FALSE)",
    "} else {",
    "  DT::datatable(data.frame(Message = 'Model insights not available'), options = list(dom = 't'))",
    "}",
    "```"
  )
  
  
  
  return(rmd_content)
}

#' Create simple dashboard without flexdashboard dependency
#'
#' @param autoxplain_result Object from autoxplain() function
#' @param output_file Character. Path for output HTML file
#' @param top_features Integer. Number of top features to analyze
#' @return Character. Path to generated HTML file
#' @export
create_simple_dashboard <- function(autoxplain_result,
                                   output_file = "simple_dashboard.html",
                                   top_features = 5) {
  
  # Input validation
  if (!inherits(autoxplain_result, "autoxplain_result")) {
    stop("autoxplain_result must be an object from autoxplain() function")
  }
  
  # Prepare data
  data <- autoxplain_result$training_data
  target_column <- autoxplain_result$target_column
  best_model <- autoxplain_result$models[[1]]
  
  # Calculate explanations
  importance <- calculate_permutation_importance(best_model, data, target_column, n_repeats = 2)
  top_feature_names <- head(importance$feature, top_features)
  
  # Create plots
  imp_plot <- plot_permutation_importance(importance)
  model_plot <- plot_model_comparison(autoxplain_result)
  
  # Generate HTML
  html_content <- create_simple_html(
    imp_plot = imp_plot,
    model_plot = model_plot,
    autoxplain_result = autoxplain_result,
    importance = importance
  )
  
  # Write to file
  writeLines(html_content, output_file)
  message("Simple dashboard generated: ", output_file)
  
  return(output_file)
}

#' Create simple HTML content
#'
#' @param imp_plot Plotly importance plot
#' @param model_plot Plotly model comparison plot  
#' @param autoxplain_result AutoML results
#' @param importance Feature importance data
#' @return Character vector with HTML content
#' @keywords internal
create_simple_html <- function(imp_plot, model_plot, autoxplain_result, importance) {
  
  # Convert plots to HTML
  imp_html <- plotly::plotly_json(imp_plot, pretty = FALSE)
  model_html <- plotly::plotly_json(model_plot, pretty = FALSE)
  
  target <- autoxplain_result$target_column
  is_classification <- !is.numeric(autoxplain_result$training_data[[target]])
  task_type <- if (is_classification) "Classification" else "Regression"
  
  html_content <- c(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "  <title>AutoXplainR Dashboard</title>",
    "  <script src='https://cdn.plot.ly/plotly-latest.min.js'></script>",
    "  <style>",
    "    body { font-family: Arial, sans-serif; margin: 20px; }",
    "    .header { text-align: center; margin-bottom: 30px; }",
    "    .plot-container { margin: 20px 0; }",
    "    .summary { background: #f8f9fa; padding: 15px; margin: 20px 0; border-radius: 5px; }",
    "  </style>",
    "</head>",
    "<body>",
    "  <div class='header'>",
    "    <h1>AutoXplainR: Automated ML Explanation Dashboard</h1>",
    paste0("    <h3>", task_type, " Analysis for '", target, "'</h3>"),
    "  </div>",
    "  ",
    "  <div class='summary'>",
    "    <h3>Analysis Summary</h3>",
    paste0("    <p><strong>Target Variable:</strong> ", target, "</p>"),
    paste0("    <p><strong>Task Type:</strong> ", task_type, "</p>"),
    paste0("    <p><strong>Features:</strong> ", length(autoxplain_result$features), "</p>"),
    paste0("    <p><strong>Observations:</strong> ", nrow(autoxplain_result$training_data), "</p>"),
    paste0("    <p><strong>Models Trained:</strong> ", length(autoxplain_result$models), "</p>"),
    "  </div>",
    "  ",
    "  <div class='plot-container'>",
    "    <h3>Model Performance Comparison</h3>",
    "    <div id='model-plot'></div>",
    "  </div>",
    "  ",
    "  <div class='plot-container'>",
    "    <h3>Feature Importance (Best Model)</h3>",
    "    <div id='importance-plot'></div>",
    "  </div>",
    "  ",
    "  <script>",
    paste0("    Plotly.newPlot('model-plot', ", model_html, ");"),
    paste0("    Plotly.newPlot('importance-plot', ", imp_html, ");"),
    "  </script>",
    "</body>",
    "</html>"
  )
  
  return(html_content)
}