#' Plot Permutation Feature Importance
#'
#' Creates an interactive bar chart showing feature importance scores.
#'
#' @param importance_data Data.frame from calculate_permutation_importance()
#' @param title Character. Plot title (default: "Permutation Feature Importance")
#' @param max_features Integer. Maximum number of features to display (default: 15)
#' @param color Character. Bar color (default: "#2E86AB")
#' @return A plotly object
#' @export
#' @importFrom plotly plot_ly layout config
plot_permutation_importance <- function(importance_data,
                                       title = "Permutation Feature Importance",
                                       max_features = 15,
                                       color = "#2E86AB") {
  
  # Input validation
  if (!is.data.frame(importance_data) || !all(c("feature", "importance") %in% colnames(importance_data))) {
    stop("importance_data must be a data.frame with 'feature' and 'importance' columns")
  }
  
  # Limit number of features and ensure positive values for plotting
  plot_data <- head(importance_data, max_features)
  
  # Create hover text
  hover_text <- paste0(
    "<b>", plot_data$feature, "</b><br>",
    "Importance: ", round(plot_data$importance, 4), "<br>",
    "Metric: ", attr(importance_data, "metric") %||% "unknown"
  )
  
  # Create bar plot
  p <- plot_ly(
    data = plot_data,
    x = ~importance,
    y = ~reorder(feature, importance),
    type = "bar",
    orientation = "h",
    marker = list(color = color),
    hovertemplate = hover_text,
    name = "Importance"
  ) %>%
    layout(
      title = list(text = title, font = list(size = 16)),
      xaxis = list(title = "Importance Score"),
      yaxis = list(title = "Features"),
      margin = list(l = 150, r = 50, t = 50, b = 50),
      showlegend = FALSE,
      hovermode = "closest"
    ) %>%
    config(displayModeBar = FALSE)
  
  return(p)
}

#' Plot Partial Dependence
#'
#' Creates an interactive line plot showing partial dependence values.
#'
#' @param pdp_data Data.frame from calculate_partial_dependence()
#' @param title Character. Plot title (default: auto-generated)
#' @param color Character. Line color (default: "#A23B72")
#' @param height Numeric. Plot height in pixels (default: 220)
#' @param width Numeric. Plot width in pixels (default: 350)
#' @return A plotly object
#' @export
#' @importFrom plotly plot_ly layout config add_trace
plot_partial_dependence <- function(pdp_data,
                                   title = NULL,
                                   color = "#A23B72",
                                   height = 220,
                                   width = 350) {
  
  # Input validation
  if (!is.data.frame(pdp_data) || ncol(pdp_data) < 2) {
    stop("pdp_data must be a data.frame with at least 2 columns")
  }
  
  feature_name <- colnames(pdp_data)[1]
  
  # Check if this is multi-class PDP (has prob_* columns)
  prob_cols <- grep("^prob_", colnames(pdp_data), value = TRUE)
  is_multiclass <- length(prob_cols) > 0
  
  if (is.null(title)) {
    if (is_multiclass) {
      title <- paste("Class Probabilities vs", feature_name)
    } else {
      title <- paste("Partial Dependence Plot -", feature_name)
    }
  }
  
  if (is_multiclass) {
    # Multi-class PDP: create multiple lines
    p <- plot_ly()
    
    # Define colors for different classes
    class_colors <- c("#e74c3c", "#3498db", "#2ecc71", "#f39c12", "#9b59b6", "#1abc9c")
    
    for (i in seq_along(prob_cols)) {
      class_name <- gsub("^prob_", "", prob_cols[i])
      p <- p %>% add_trace(
        x = pdp_data[[feature_name]],
        y = pdp_data[[prob_cols[i]]],
        type = "scatter",
        mode = "lines+markers",
        name = paste0("P(", class_name, ")"),
        line = list(color = class_colors[i %% length(class_colors) + 1], width = 3),
        marker = list(color = class_colors[i %% length(class_colors) + 1], size = 6),
        hovertemplate = paste0(
          "<b>P(", class_name, ")</b><br>",
          feature_name, ": %{x}<br>",
          "Probability: %{y:.3f}<extra></extra>"
        )
      )
    }
    
    y_title <- "Class Probability"
    y_range <- c(0, 1)
    show_legend <- TRUE
    
  } else {
    # Single value PDP: original behavior
    hover_text <- paste0(
      "<b>", feature_name, "</b>: ", pdp_data[[1]], "<br>",
      "<b>Partial Dependence</b>: ", round(pdp_data$partial_dependence, 4)
    )
    
    p <- plot_ly(
      data = pdp_data,
      x = ~get(feature_name),
      y = ~partial_dependence,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = color, width = 3),
      marker = list(color = color, size = 6),
      hovertemplate = hover_text,
      name = "Partial Dependence"
    )
    
    y_title <- "Partial Dependence"
    y_range <- NULL
    show_legend <- FALSE
  }
  
  # Apply common layout
  if (show_legend) {
    # Classification: legend at bottom, match regression "Top 3 Models" style
    p <- p %>%
      layout(
        title = list(text = title, font = list(size = 16)),
        xaxis = list(title = feature_name, titlefont = list(size = 9)),  # Match regression x-axis style
        yaxis = if (is.null(y_range)) {
          list(title = y_title)
        } else {
          list(title = y_title, range = y_range)
        },
        margin = list(l = 50, r = 50, t = 50, b = 50),  # Match regression margin
        height = height,
        width = width,
        showlegend = TRUE,
        legend = list(
          orientation = "h",  # Horizontal legend
          x = 0.5,           # Center horizontally
          xanchor = "center",
          y = -0.35,         # Position further down (match regression)
          yanchor = "top",
          font = list(size = 8)  # Match regression legend font size
        ),
        hovermode = "closest"
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  } else {
    # Regression: no legend, standard margins
    p <- p %>%
      layout(
        title = list(text = title, font = list(size = 16)),
        xaxis = list(title = feature_name),
        yaxis = if (is.null(y_range)) {
          list(title = y_title)
        } else {
          list(title = y_title, range = y_range)
        },
        margin = list(l = 50, r = 50, t = 50, b = 50),
        height = height,
        width = width,
        showlegend = FALSE,
        hovermode = "closest"
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  }
  
  return(p)
}

#' Plot Multiple Partial Dependence Plots
#'
#' Creates a subplot with multiple partial dependence plots.
#'
#' @param pdp_list Named list of data.frames from calculate_partial_dependence_multi()
#' @param title Character. Overall plot title (default: "Partial Dependence Plots")
#' @param colors Character vector. Colors for each plot (default: predefined palette)
#' @param ncol Integer. Number of columns in subplot layout (default: 2)
#' @return A plotly object
#' @export
#' @importFrom plotly subplot
plot_partial_dependence_multi <- function(pdp_list,
                                         title = "Partial Dependence Plots",
                                         colors = NULL,
                                         ncol = 2) {
  
  # Input validation
  if (!is.list(pdp_list) || length(pdp_list) == 0) {
    stop("pdp_list must be a non-empty list")
  }
  
  # Set default colors
  if (is.null(colors)) {
    colors <- c("#A23B72", "#F18F01", "#C73E1D", "#2E86AB", "#F24236", "#593E2C")
  }
  colors <- rep(colors, length.out = length(pdp_list))
  
  # Create individual plots
  plots <- list()
  for (i in seq_along(pdp_list)) {
    feature_name <- names(pdp_list)[i]
    plots[[i]] <- plot_partial_dependence(
      pdp_list[[i]], 
      title = feature_name,
      color = colors[i]
    )
  }
  
  # Create subplot
  nrow <- ceiling(length(plots) / ncol)
  p <- subplot(plots, nrows = nrow, shareY = FALSE, shareX = FALSE, 
               titleX = TRUE, titleY = TRUE, margin = 0.05) %>%
    layout(
      title = list(text = title, font = list(size = 18)),
      margin = list(l = 50, r = 50, t = 80, b = 50)
    ) %>%
    config(displayModeBar = FALSE)
  
  return(p)
}


#' Create Model Correlation Heatmap
#'
#' Creates a correlation heatmap showing how similar different models' predictions are
#'
#' @param autoxplain_result AutoXplainR result object
#' @param test_data Optional test data. If NULL, uses training data
#' @return plotly heatmap object
#' @export
plot_model_correlations <- function(autoxplain_result, test_data = NULL) {
  
  # Revolutionary approach: Model Selection Intelligence Dashboard
  # Focus on actionable insights rather than raw correlation visualization
  tryCatch({
    # Use test data if provided, otherwise use training data
    data_to_use <- if (!is.null(test_data)) {
      test_data
    } else {
      autoxplain_result$training_data
    }
    
    # Convert to H2O frame if not already
    if (!inherits(data_to_use, "H2OFrame")) {
      data_to_use <- h2o::as.h2o(data_to_use)
    }
    
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
    
    # Use filtered models for correlation heatmap
    models <- models[selected_models]
    model_names <- names(models)
    
    if (length(models) < 2) {
      return(
        plotly::plot_ly() %>%
          plotly::add_text(x = 0.5, y = 0.5, 
                           text = "Need at least 2 models for ensemble analysis",
                           textposition = "middle center",
                           textfont = list(size = 14, color = "#666")) %>%
          plotly::layout(
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
          ) %>%
          plotly::config(displayModeBar = FALSE)
      )
    }
    
    # Get model performance from leaderboard
    leaderboard <- tryCatch({
      as.data.frame(autoxplain_result$leaderboard)
    }, error = function(e) {
      data.frame(
        model_id = model_names,
        stringsAsFactors = FALSE
      )
    })
    
    # Get predictions for correlation analysis
    predictions <- data.frame(row.names = seq_len(nrow(data_to_use)))
    target_col <- autoxplain_result$target_column
    sample_target <- autoxplain_result$training_data[[target_col]]
    is_classification <- !is.numeric(sample_target)
    
    
    # Collect predictions and performance
    model_data <- list()
    for (i in seq_along(models)) {
      model_name <- model_names[i]
      tryCatch({
        # Get predictions
        pred_data <- data_to_use
        if (target_col %in% colnames(pred_data)) {
          pred_data <- pred_data[, !colnames(pred_data) %in% target_col]
        }
        
        # Comprehensive H2O output suppression
        invisible({
          capture.output({
            suppressMessages({
              pred <- h2o.predict(models[[i]], pred_data)
            })
          }, type = 'output')
        })
        
        # Extract appropriate prediction values for correlation analysis
        if (is_classification) {
          # For classification: store the complete probability matrix for each model
          # We'll compute correlations between these matrices later
          if (ncol(pred) > 2) {
            # Multi-class: exclude the "predict" column and get all probability columns
            all_cols <- colnames(pred)
            prob_cols <- all_cols[all_cols != "predict"]  # All columns except "predict"
            prob_cols <- sort(prob_cols)  # Ensure consistent ordering across models
            
            if (length(prob_cols) > 0) {
              # Store the complete probability matrix for this model
              prob_matrix <- matrix(nrow = nrow(pred), ncol = length(prob_cols))
              for (j in seq_along(prob_cols)) {
                prob_matrix[, j] <- as.vector(pred[, prob_cols[j]])
              }
              pred_values <- prob_matrix  # Store the full matrix
            } else {
              pred_values <- as.vector(pred[, 2])  # Fallback to second column
            }
          } else {
            # Binary classification: use probability of positive class
            pred_values <- as.vector(pred[, 2])  # Column 2 is typically the probability
          }
        } else {
          # For regression: use the predicted value
          pred_values <- as.vector(pred[, 1])
        }
        predictions[[model_name]] <- pred_values
        
        # Get performance (safely extract numeric performance metric)
        performance <- NA  # no default - use actual performance only
        tryCatch({
          perf_row <- which(leaderboard$model_id == model_name)
          if (length(perf_row) > 0) {
            numeric_cols <- sapply(leaderboard, is.numeric)
            if (any(numeric_cols)) {
              perf_val <- leaderboard[perf_row[1], names(numeric_cols)[1]]
              if (is.numeric(perf_val) && !is.na(perf_val)) {
                performance <- as.numeric(perf_val)
              }
            }
          }
        }, error = function(e) {
          # Keep NA if extraction fails - no fake data
          performance <<- NA
        })
        
        model_data[[model_name]] <- list(
          predictions = pred_values,
          performance = performance,
          display_name = create_model_display_name(model_name)
        )
      }, error = function(e) {
        cat("Failed to process model ", model_name, ": ", e$message, "\n")
      })
    }
    
    if (length(model_data) < 2) {
      return(
        plotly::plot_ly() %>%
          plotly::add_text(x = 0.5, y = 0.5, 
                           text = "Could not analyze model relationships",
                           textposition = "middle center",
                           textfont = list(size = 14, color = "#666")) %>%
          plotly::layout(
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
          ) %>%
          plotly::config(displayModeBar = FALSE)
      )
    }
    
    # Create properly designed correlation heatmap
    return(create_clean_correlation_heatmap(model_data, is_classification))
    
  }, error = function(e) {
    return(
      plotly::plot_ly() %>%
        plotly::add_text(x = 0.5, y = 0.5, 
                         text = paste("Model analysis error:", e$message),
                         textposition = "middle center",
                         textfont = list(size = 14, color = "#666")) %>%
        plotly::layout(
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    )
  })
}

# Helper function to create intelligent model display names
create_model_display_name <- function(model_name) {
  if (grepl("StackedEnsemble_AllModels", model_name)) {
    "🎯 All-Model Ensemble"
  } else if (grepl("StackedEnsemble_BestOfFamily", model_name)) {
    "⭐ Best-of-Family Ensemble"
  } else if (grepl("XGBoost", model_name)) {
    "🚀 XGBoost"
  } else if (grepl("GBM", model_name)) {
    "🌳 Gradient Boosting"
  } else if (grepl("GLM", model_name)) {
    "📊 Linear Model"
  } else if (grepl("DeepLearning", model_name)) {
    "🧠 Neural Network"
  } else if (grepl("DRF", model_name)) {
    "🌲 Random Forest"
  } else {
    paste("🔮", substr(gsub("_.*", "", model_name), 1, 12))
  }
}

# Clean, properly-sized correlation heatmap with dashboard consistency
create_clean_correlation_heatmap <- function(model_data, is_classification) {
  
  model_names <- names(model_data)
  n_models <- length(model_names)
  
  # Create correlation matrix
  cor_matrix <- matrix(1, nrow = n_models, ncol = n_models,
                      dimnames = list(model_names, model_names))
  
  # Calculate pairwise correlations
  for (i in 1:(n_models-1)) {
    for (j in (i+1):n_models) {
      model1 <- model_names[i]
      model2 <- model_names[j]
      
      # Calculate correlation with robust error handling
      correlation <- tryCatch({
        pred1 <- model_data[[model1]]$predictions
        pred2 <- model_data[[model2]]$predictions
        
        # Handle different prediction types (vectors for regression, matrices for classification)
        if (is.matrix(pred1) && is.matrix(pred2)) {
          # Classification: compute correlation between flattened probability matrices
          if (nrow(pred1) == nrow(pred2) && ncol(pred1) == ncol(pred2)) {
            flat1 <- as.vector(pred1)
            flat2 <- as.vector(pred2)
            corr_val <- cor(flat1, flat2, use = "complete.obs")
            if (is.numeric(corr_val) && !is.na(corr_val)) {
              corr_val
            } else {
              NA
            }
          } else {
            NA  # Incompatible matrix dimensions
          }
        } else {
          # Regression: convert to numeric vectors and correlate
          pred1_vec <- as.numeric(pred1)
          pred2_vec <- as.numeric(pred2)
          
          if (length(pred1_vec) == length(pred2_vec) && length(pred1_vec) > 0) {
            corr_val <- cor(pred1_vec, pred2_vec, use = "complete.obs")
            if (is.numeric(corr_val) && !is.na(corr_val)) {
              corr_val
            } else {
              NA
            }
          } else {
            NA
          }
        }
      }, error = function(e) {
        NA  # Return NA on error instead of fake value
      })
      
      cor_matrix[i, j] <- correlation
      cor_matrix[j, i] <- correlation
    }
  }
  
  # Create clean, shortened display names
  display_names <- sapply(model_names, function(name) {
    if (grepl("StackedEnsemble_AllModels", name)) {
      "All-Model Ensemble"
    } else if (grepl("StackedEnsemble_BestOfFamily", name)) {
      "Best-Family Ensemble"
    } else if (grepl("XGBoost", name)) {
      "XGBoost"
    } else if (grepl("GBM", name)) {
      "Gradient Boosting"
    } else if (grepl("GLM", name)) {
      "Linear Model"
    } else if (grepl("DeepLearning", name)) {
      "Neural Network"
    } else if (grepl("DRF", name)) {
      "Random Forest"
    } else {
      # Extract clean algorithm name
      clean_name <- gsub("_.*", "", name)
      substr(clean_name, 1, 15)
    }
  })
  
  # Dynamic color scale based on actual data range (excluding diagonal)
  actual_correlations <- cor_matrix[cor_matrix < 1 & !is.na(cor_matrix)]
  
  # Robust range calculation with validation
  if(length(actual_correlations) > 0) {
    min_corr <- min(actual_correlations)
    max_corr <- max(actual_correlations)
    
    # Ensure valid range for plotly domain
    if (is.infinite(min_corr) || is.infinite(max_corr) || is.na(min_corr) || is.na(max_corr)) {
      min_corr <- -1
      max_corr <- 1
    } else if (min_corr == max_corr) {
      # If all correlations are the same, create a small range
      min_corr <- max(min_corr - 0.1, -1)
      max_corr <- min(max_corr + 0.1, 1)
    }
  } else {
    # No valid correlations found - use full correlation range
    min_corr <- -1
    max_corr <- 1
  }
  
  # Create intuitive color scale optimized for correlation range
  color_scale <- list(
    c(0, "#4683f4"),      # Low correlation - Blue
    c(0.5, "#aa15fb"),    # Middle range - Very light gray  
    c(1, "#960000")       # High correlation - Red
  )
  
  # Create hover text - now using correlation for both types
  metric_name <- "Correlation"
  hover_template <- paste0(
    "<b>%{y}</b> vs <b>%{x}</b><br>",
    "<b>", metric_name, ":</b> %{z:.3f}<br>",
    "<extra></extra>"
  )
  
  # Create masked correlation matrix for lower triangle display only
  # CRITICAL: Use same dimnames as original matrix to avoid indexing bugs
  masked_matrix <- matrix(NA, nrow = n_models, ncol = n_models,
                         dimnames = list(model_names, model_names))
  
  # Fill only the lower triangle (below diagonal)
  for (i in 1:n_models) {
    for (j in 1:n_models) {
      if (i > j) {  # Only show lower triangle
        masked_matrix[i, j] <- cor_matrix[i, j]
      }
    }
  }
  
  # Now update dimnames to display names AFTER copying values
  dimnames(masked_matrix) <- list(display_names, display_names)
  
  # Create text annotations only for lower triangle with proper positioning
  # CRITICAL: Use zero-based indexing for plotly positioning
  text_annotations <- list()
  for (i in 1:n_models) {
    for (j in 1:n_models) {
      if (i > j) {  # Only show text for lower triangle
        corr_val <- cor_matrix[i, j]
        if (!is.na(corr_val)) {  # Only show text for valid correlations
          text_annotations[[length(text_annotations) + 1]] <- list(
            x = j - 1,  # Zero-based indexing for plotly
            y = i - 1,  # Zero-based indexing for plotly
            text = sprintf("%.3f", corr_val),
            showarrow = FALSE,
            font = list(
              size = 13,
              color = "#ffffff",
              family = "Arial, sans-serif", 
              weight = "bold"
            ),
            xref = "x",
            yref = "y"
          )
        }
      }
    }
  }
  
  # Check if we have any valid data to plot
  if (all(is.na(masked_matrix))) {
    # Return error plot if no valid correlations
    return(
      plotly::plot_ly() %>%
        plotly::add_text(x = 0.5, y = 0.5, 
                         text = "Unable to calculate model correlations\n(Need at least 2 models with valid predictions)",
                         textposition = "middle center",
                         textfont = list(size = 14, color = "#666")) %>%
        plotly::layout(
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    )
  }
  
  # Create the clean, properly-sized heatmap with better square separation
  p <- plotly::plot_ly(
    x = display_names,
    y = display_names,
    z = masked_matrix,
    type = "heatmap",
    colorscale = color_scale,
    zmin = min_corr,
    zmax = max_corr,
    hovertemplate = hover_template,
    showscale = TRUE,
    xgap = 4,  # Larger gap between squares for better separation
    ygap = 4,  # Larger gap between squares for better separation
    colorbar = list(
      title = list(
        text = metric_name,
        font = list(size = 14, color = "#333", family = "Arial", weight = "bold")
      ),
      titleside = "right",
      thickness = 20,  # Much thicker colorbar
      len = 1.0,       # Full height colorbar
      x = 1.02
    )
  ) %>%
    plotly::layout(
      title = list(
        text = paste("Model", metric_name, "Matrix"),
        font = list(size = 18, color = "#333", family = "Arial", weight = "bold"),
        x = 0.5,
        xanchor = "center"
      ),
      xaxis = list(
        title = "",
        tickfont = list(size = 12, color = "#333", family = "Arial"),
        tickangle = -45,
        showgrid = FALSE,
        side = "bottom"
      ),
      yaxis = list(
        title = "",
        tickfont = list(size = 12, color = "#333", family = "Arial"), 
        showgrid = FALSE,
        autorange = "reversed"  # Standard heatmap orientation
      ),
      annotations = text_annotations,  # Add correlation numbers inside squares
      margin = list(l = 120, r = 80, t = 60, b = 120),  # More generous margins
      height = 550,  # Expanded vertical space
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      font = list(family = "Arial", color = "#333")
    ) %>%
    plotly::config(
      displayModeBar = FALSE, 
      responsive = TRUE,
      displaylogo = FALSE
    )
  
  return(p)
}

#' Plot Model Performance Comparison
#'
#' Creates a scatter plot comparing model performance vs complexity.
#'
#' @param autoxplain_result Object from autoxplain() function
#' @param performance_metric Character. Metric to use for y-axis (default: "auc" for classification, "rmse" for regression)
#' @param complexity_metric Character. Metric to use for x-axis (default: "model_size")
#' @param title Character. Plot title (default: auto-generated)
#' @return A plotly object
#' @export
plot_model_comparison <- function(autoxplain_result,
                                 performance_metric = NULL,
                                 complexity_metric = "model_size",
                                 title = NULL) {
  
  # Input validation
  if (!inherits(autoxplain_result, "autoxplain_result")) {
    stop("autoxplain_result must be an object from autoxplain() function")
  }
  
  # Extract leaderboard data safely
  tryCatch({
    leaderboard <- as.data.frame(autoxplain_result$leaderboard)
  }, error = function(e) {
    # If H2O data is not available, create a minimal leaderboard
    leaderboard <- data.frame(
      model_id = names(autoxplain_result$models),
      stringsAsFactors = FALSE
    )
  })
  
  # Auto-detect performance metric if not specified
  if (is.null(performance_metric)) {
    # Check available metrics in leaderboard
    available_metrics <- colnames(leaderboard)
    if ("auc" %in% available_metrics) {
      performance_metric <- "auc"
    } else if ("rmse" %in% available_metrics) {
      performance_metric <- "rmse"
    } else if ("logloss" %in% available_metrics) {
      performance_metric <- "logloss"
    } else {
      # Use first numeric column after model_id
      numeric_cols <- sapply(leaderboard, is.numeric)
      performance_metric <- names(numeric_cols)[numeric_cols][1]
    }
  }
  
  if (!performance_metric %in% colnames(leaderboard)) {
    stop("performance_metric '", performance_metric, "' not found in leaderboard")
  }
  
  # Create complexity proxy if model_size not available
  if (!"model_size" %in% colnames(leaderboard)) {
    # Use model rank as complexity proxy
    leaderboard$model_size <- seq_len(nrow(leaderboard))
    complexity_metric <- "model_size"
  }
  
  if (is.null(title)) {
    title <- paste("Model Performance vs Complexity")
  }
  
  # Extract model types from model IDs
  leaderboard$model_type <- extract_model_type(as.vector(leaderboard$model_id))
  
  # Create hover text
  hover_text <- paste0(
    "<b>Model:</b> ", leaderboard$model_type, "<br>",
    "<b>", performance_metric, ":</b> ", round(leaderboard[[performance_metric]], 4), "<br>",
    "<b>", complexity_metric, ":</b> ", leaderboard[[complexity_metric]]
  )
  
  # Create scatter plot
  p <- plot_ly(
    data = leaderboard,
    x = ~get(complexity_metric),
    y = ~get(performance_metric),
    color = ~model_type,
    type = "scatter",
    mode = "markers",
    marker = list(size = 10),
    hovertemplate = hover_text,
    text = ~model_type
  ) %>%
    layout(
      title = list(text = title, font = list(size = 16)),
      xaxis = list(title = complexity_metric),
      yaxis = list(title = performance_metric),
      margin = list(l = 50, r = 50, t = 50, b = 50),
      hovermode = "closest"
    ) %>%
    config(displayModeBar = FALSE)
  
  return(p)
}

#' Extract model type from H2O model ID
#'
#' @param model_ids Character vector of H2O model IDs
#' @return Character vector of simplified model types
#' @keywords internal
extract_model_type <- function(model_ids) {
  
  # Extract algorithm name from H2O model IDs
  types <- character(length(model_ids))
  
  for (i in seq_along(model_ids)) {
    id <- model_ids[i]
    
    if (grepl("GBM", id, ignore.case = TRUE)) {
      types[i] <- "GBM"
    } else if (grepl("RandomForest|DRF", id, ignore.case = TRUE)) {
      types[i] <- "Random Forest"
    } else if (grepl("GLM", id, ignore.case = TRUE)) {
      types[i] <- "GLM"
    } else if (grepl("DeepLearning", id, ignore.case = TRUE)) {
      types[i] <- "Deep Learning"
    } else if (grepl("XGBoost", id, ignore.case = TRUE)) {
      types[i] <- "XGBoost"
    } else if (grepl("StackedEnsemble", id, ignore.case = TRUE)) {
      types[i] <- "Ensemble"
    } else {
      # Extract first part of model ID as fallback
      parts <- strsplit(id, "_")[[1]]
      types[i] <- parts[1]
    }
  }
  
  return(types)
}

#' Null-coalescing operator
#'
#' Returns the first argument if it is not NULL, otherwise returns the second argument.
#' This is useful for providing default values.
#'
#' @param x First value to check
#' @param y Default value to use if x is NULL
#' @return x if not NULL, otherwise y
#' @name grapes-or-or-grapes
#' @keywords internal
#' @examples
#' \dontrun{
#' # Returns "default"
#' NULL %||% "default"
#' 
#' # Returns "value"
#' "value" %||% "default"
#' }
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}