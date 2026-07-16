#' Plot permutation importance with uncertainty
#'
#' @param importance_data Data frame returned by
#'   [calculate_permutation_importance()].
#' @param title Plot title.
#' @param max_features Maximum features displayed.
#' @param color Positive-importance color.
#' @param negative_color Negative-importance color.
#'
#' @return A Plotly htmlwidget.
#' @export
plot_permutation_importance <- function(importance_data,
                                        title = "Permutation Importance Evidence",
                                        max_features = 15L,
                                        color = "#237a57",
                                        negative_color = "#b24747") {
  require_optional("plotly", "interactive importance plots")
  if (!is.data.frame(importance_data) ||
        !all(c("feature", "importance") %in% names(importance_data))) {
    stop("`importance_data` must contain `feature` and `importance` columns.",
         call. = FALSE)
  }
  max_features <- assert_count(max_features, "max_features")
  plot_data <- importance_data[order(importance_data$importance, decreasing = TRUE), , drop = FALSE]
  plot_data <- head(plot_data, max_features)
  plot_data$feature <- factor(plot_data$feature, levels = rev(plot_data$feature))
  plot_data$bar_color <- ifelse(plot_data$importance < 0, negative_color, color)
  if (all(c("conf_low", "conf_high") %in% names(plot_data))) {
    error_x <- list(
      type = "data",
      symmetric = FALSE,
      array = pmax(0, plot_data$conf_high - plot_data$importance),
      arrayminus = pmax(0, plot_data$importance - plot_data$conf_low),
      color = "#384641",
      thickness = 1.2
    )
  } else {
    error_x <- NULL
  }
  hover <- paste0(
    "<b>", plot_data$feature, "</b><br>",
    "Importance: ", signif(plot_data$importance, 4),
    if ("sign_stability" %in% names(plot_data)) {
      paste0("<br>Sign stability: ", round(100 * plot_data$sign_stability, 1), "%")
    } else {
      ""
    },
    "<extra></extra>"
  )
  plotly::plot_ly(
    plot_data,
    x = ~importance,
    y = ~feature,
    type = "bar",
    orientation = "h",
    marker = list(color = plot_data$bar_color),
    error_x = error_x,
    hovertemplate = hover
  ) |>
    plotly::layout(
      title = list(text = title),
      xaxis = list(title = "Performance degradation after permutation", zeroline = TRUE),
      yaxis = list(title = ""),
      margin = list(l = 150, r = 35, t = 55, b = 55),
      showlegend = FALSE
    ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

#' @export
plot.autoxplain_importance <- function(x, ...) plot_permutation_importance(x, ...)

#' Plot a PDP or ALE feature effect
#'
#' @param pdp_data Data frame returned by [explain_effect()].
#' @param title Optional title.
#' @param color Effect-line color.
#' @param height,width Widget dimensions.
#'
#' @return A Plotly htmlwidget.
#' @export
plot_partial_dependence <- function(pdp_data,
                                    title = NULL,
                                    color = "#6f3f86",
                                    height = 320,
                                    width = NULL) {
  require_optional("plotly", "interactive feature-effect plots")
  if (!is.data.frame(pdp_data) || ncol(pdp_data) < 2L) {
    stop("`pdp_data` must be a feature-effect data frame.", call. = FALSE)
  }
  feature <- names(pdp_data)[[1L]]
  effect_column <- if ("accumulated_effect" %in% names(pdp_data)) {
    "accumulated_effect"
  } else if ("partial_dependence" %in% names(pdp_data)) {
    "partial_dependence"
  } else {
    names(pdp_data)[[2L]]
  }
  method <- if (effect_column == "accumulated_effect") "ALE" else "PDP"
  title <- title %||% paste(method, "-", feature)
  hover <- paste0(
    "<b>", feature, "</b>: %{x}<br>", method, ": %{y:.4f}",
    if ("support" %in% names(pdp_data)) "<br>Relative support: %{customdata:.2f}" else "",
    "<extra></extra>"
  )
  ribbon <- all(c("conf_low", "conf_high") %in% names(pdp_data))
  plot <- plotly::plot_ly(width = width, height = height)
  if (ribbon) {
    plot <- plot |>
      plotly::add_ribbons(
        x = pdp_data[[feature]],
        ymin = pdp_data$conf_low,
        ymax = pdp_data$conf_high,
        fillcolor = "rgba(111,63,134,0.14)",
        line = list(color = "transparent"),
        hoverinfo = "skip",
        name = "descriptive interval",
        showlegend = FALSE
      )
  }
  plot |>
    plotly::add_lines(
      x = pdp_data[[feature]],
      y = pdp_data[[effect_column]],
      customdata = if ("support" %in% names(pdp_data)) pdp_data$support else NULL,
      line = list(color = color, width = 3),
      hovertemplate = hover,
      name = method
    ) |>
    plotly::layout(
      title = list(text = title),
      xaxis = list(title = feature),
      yaxis = list(title = if (method == "ALE") "Centered accumulated effect" else "Mean prediction"),
      margin = list(l = 60, r = 25, t = 55, b = 55),
      showlegend = FALSE
    ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

#' @export
plot.autoxplain_effect <- function(x, ...) plot_partial_dependence(x, ...)

#' Plot multiple feature effects
#'
#' @param pdp_list Named list of feature effects.
#' @param title Overall title.
#' @param colors Line colors.
#' @param ncol Number of subplot columns.
#'
#' @return A Plotly htmlwidget.
#' @export
plot_partial_dependence_multi <- function(pdp_list,
                                          title = "Feature Effects",
                                          colors = NULL,
                                          ncol = 2L) {
  require_optional("plotly", "interactive feature-effect plots")
  if (!is.list(pdp_list) || !length(pdp_list)) {
    stop("`pdp_list` must be a non-empty list.", call. = FALSE)
  }
  ncol <- assert_count(ncol, "ncol")
  colors <- colors %||% c("#6f3f86", "#c27025", "#237a57", "#315c9b")
  colors <- rep(colors, length.out = length(pdp_list))
  plots <- lapply(seq_along(pdp_list), function(index) {
    plot_partial_dependence(
      pdp_list[[index]], title = names(pdp_list)[[index]] %||% paste("Feature", index),
      color = colors[[index]], height = 300
    )
  })
  plotly::subplot(
    plots,
    nrows = ceiling(length(plots) / ncol),
    shareX = FALSE,
    shareY = FALSE,
    titleX = TRUE,
    titleY = TRUE,
    margin = 0.06
  ) |>
    plotly::layout(title = list(text = title), margin = list(t = 70)) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

#' Plot prediction agreement among retained AutoML models
#'
#' @param autoxplain_result An `autoxplain_result`.
#' @param test_data Optional evaluation data.
#'
#' @return A Plotly heatmap.
#' @export
plot_model_correlations <- function(autoxplain_result, test_data = NULL) {
  require_optional("plotly", "interactive model-agreement plots")
  explainers <- as_explainers(autoxplain_result, data = test_data)
  if (length(explainers) < 2L) {
    stop("At least two models are required for an agreement heatmap.", call. = FALSE)
  }
  predictions <- lapply(explainers, function(explainer) {
    value <- predict(explainer, explainer$data)
    if (is.matrix(value)) max.col(value, ties.method = "first") else value
  })
  prediction_matrix <- do.call(cbind, predictions)
  colnames(prediction_matrix) <- names(explainers)
  agreement <- suppressWarnings(stats::cor(
    prediction_matrix, method = "spearman", use = "pairwise.complete.obs"
  ))
  plotly::plot_ly(
    x = colnames(agreement),
    y = rownames(agreement),
    z = agreement,
    type = "heatmap",
    zmin = -1,
    zmax = 1,
    colors = c("#b84b4b", "#f6f7f2", "#237a57"),
    hovertemplate = "%{y} / %{x}<br>Spearman: %{z:.3f}<extra></extra>"
  ) |>
    plotly::layout(
      title = "Prediction Rank Agreement",
      xaxis = list(title = ""), yaxis = list(title = ""),
      margin = list(l = 150, b = 130, t = 55, r = 20)
    ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

#' Plot model performance and resource trade-offs
#'
#' @param autoxplain_result An `autoxplain_result`.
#' @param performance_metric Leaderboard metric; automatically selected when
#'   `NULL`.
#' @param complexity_metric Optional numeric leaderboard or model-metadata
#'   column. Model size is preferred when `NULL`.
#' @param title Plot title.
#'
#' @return A Plotly scatter plot.
#' @export
plot_model_comparison <- function(autoxplain_result,
                                  performance_metric = NULL,
                                  complexity_metric = NULL,
                                  title = "Model Trade-off Landscape") {
  require_optional("plotly", "interactive model comparison plots")
  tradeoffs <- model_tradeoffs(
    autoxplain_result,
    performance_metric = performance_metric,
    complexity_metric = complexity_metric
  )
  performance_metric <- attr(tradeoffs, "performance_metric")
  complexity_metric <- attr(tradeoffs, "complexity_metric")
  performance_label <- pretty_metric(performance_metric)
  resource_label <- pretty_complexity(complexity_metric)
  tradeoffs$pareto_status <- ifelse(tradeoffs$pareto_optimal, "Pareto-efficient", "Dominated")
  plot <- plotly::plot_ly(
    tradeoffs,
    x = tradeoffs[[complexity_metric]],
    y = tradeoffs[[performance_metric]],
    color = ~role,
    symbol = ~pareto_status,
    text = ~model,
    type = "scatter",
    mode = "markers",
    marker = list(size = 12, line = list(width = 1, color = "#ffffff")),
    hovertemplate = paste0(
      "<b>%{text}</b><br>", resource_label, ": %{x}<br>",
      performance_label, ": %{y:.5f}<br>%{customdata}<extra></extra>"
    ),
    customdata = ~pareto_status
  )
  frontier <- tradeoffs[tradeoffs$pareto_optimal, , drop = FALSE]
  frontier <- frontier[order(frontier[[complexity_metric]]), , drop = FALSE]
  if (nrow(frontier) > 1L) {
    plot <- plotly::add_lines(
      plot,
      data = frontier,
      x = frontier[[complexity_metric]],
      y = frontier[[performance_metric]],
      inherit = FALSE,
      line = list(color = "#176b4d", width = 2, dash = "dot"),
      hoverinfo = "skip",
      name = "Pareto frontier"
    )
  }
  plot |>
    plotly::layout(
      title = title,
      xaxis = list(title = resource_label),
      yaxis = list(title = performance_label),
      margin = list(l = 70, r = 25, t = 55, b = 65),
      legend = list(orientation = "h", x = 0, y = -0.2)
    ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

extract_model_type <- function(model_ids) {
  unname(vapply(model_ids, function(id) {
    if (grepl("StackedEnsemble", id, ignore.case = TRUE)) return("Ensemble")
    if (grepl("RandomForest|DRF", id, ignore.case = TRUE)) return("Random Forest")
    if (grepl("DeepLearning", id, ignore.case = TRUE)) return("Deep Learning")
    if (grepl("XGBoost", id, ignore.case = TRUE)) return("XGBoost")
    if (grepl("GBM", id, ignore.case = TRUE)) return("GBM")
    if (grepl("GLM", id, ignore.case = TRUE)) return("GLM")
    sub("_.*$", "", id)
  }, character(1)))
}

`%||%` <- function(x, y) if (is.null(x)) y else x
