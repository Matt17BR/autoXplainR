#' Generate a guided model report
#'
#' Creates a standalone report that starts with the modeling question,
#' held-out performance, a simple-baseline comparison, and metric definitions.
#' Fitted patterns and the more technical explanation evidence audit follow by
#' progressive disclosure.
#'
#' @param autoxplain_result An `autoxplain_result`.
#' @param output_file Destination HTML file.
#' @param top_features Maximum number of features included in the audit. The
#'   initial ranking is obtained from repeated permutation importance on the
#'   leading model, never from impurity importance.
#' @param sample_instances Retained for backward compatibility; no longer used.
#' @param include_llm_report Whether to request an optional narrative. The
#'   evidence report remains complete without it.
#' @param narrative_provider Provider passed to
#'   [generate_natural_language_report()] when `include_llm_report = TRUE`.
#'   It defaults to the deterministic local provider.
#' @param narrative_args Optional named arguments forwarded to
#'   [generate_natural_language_report()].
#' @param open_browser Open the result interactively.
#' @param performance_weight Deprecated and ignored. Performance and
#'   explanation reliability are reported separately rather than collapsed
#'   into an arbitrary weighted score.
#' @param n_repeats Permutation repeats in the final audit.
#' @param max_models Maximum retained models to compare.
#'
#' @return The normalized dashboard path, invisibly.
#' @export
generate_dashboard <- function(autoxplain_result,
                               output_file = "autoxplain-dashboard.html",
                               top_features = 8L,
                               sample_instances = 3L,
                               include_llm_report = FALSE,
                               narrative_provider = "local",
                               narrative_args = list(),
                               open_browser = FALSE,
                               performance_weight = NULL,
                               n_repeats = 20L,
                               max_models = 5L) {
  if (!inherits(autoxplain_result, "autoxplain_result")) {
    stop("`autoxplain_result` must be returned by `autoxplain()`.", call. = FALSE)
  }
  top_features <- assert_count(top_features, "top_features")
  assert_count(sample_instances, "sample_instances")
  n_repeats <- assert_count(n_repeats, "n_repeats")
  max_models <- assert_count(max_models, "max_models")
  if (!is.list(narrative_args) || length(narrative_args) && is.null(names(narrative_args))) {
    stop("`narrative_args` must be a named list.", call. = FALSE)
  }
  if (!is.null(performance_weight)) {
    warning(
      "`performance_weight` is deprecated. Performance and evidence quality are no longer collapsed into one score.",
      call. = FALSE
    )
  }
  report_data <- prepare_model_report_data(
    autoxplain_result,
    top_features = top_features,
    n_repeats = n_repeats,
    max_models = max_models
  )

  narrative <- NULL
  if (isTRUE(include_llm_report)) {
    narrative <- tryCatch(
      do.call(
        generate_natural_language_report,
        utils::modifyList(
          list(
            autoxplain_result = autoxplain_result,
            importance_data = report_data$screening,
            model_characteristics = autoxplain_result$model_characteristics,
            provider = narrative_provider
          ),
          narrative_args
        )
      ),
      error = function(error) {
        warning("Optional narrative was not generated: ", conditionMessage(error), call. = FALSE)
        NULL
      }
    )
  }
  render_model_report(
    autoxplain_result,
    output_file = output_file,
    title = paste("Understanding", autoxplain_result$target_column),
    audit = report_data$audit,
    effects = report_data$effects,
    narrative = narrative,
    open = open_browser
  )
}

prepare_model_report_data <- function(autoxplain_result,
                                      top_features = 8L,
                                      n_repeats = 20L,
                                      max_models = 5L) {
  selected <- seq_len(min(max_models, length(autoxplain_result$models)))
  explainers <- as_explainers(autoxplain_result, models = selected)
  importance_metric <- result_importance_metric(autoxplain_result)
  screen_repeats <- min(5L, n_repeats)
  screening <- calculate_permutation_importance(
    explainers[[1L]],
    metric = importance_metric,
    n_repeats = screen_repeats,
    seed = autoxplain_result$provenance$seed %||% 123L
  )
  features <- head(
    screening$feature[order(screening$importance, decreasing = TRUE)],
    min(top_features, nrow(screening))
  )
  audit <- audit_explanations(
    explainers,
    features = features,
    metric = importance_metric,
    n_repeats = n_repeats,
    seed = autoxplain_result$provenance$seed %||% 123L
  )
  audit$provenance$automl_created_at <- autoxplain_result$provenance$created_at
  audit$provenance$automl_target <- autoxplain_result$target_column
  audit$provenance$automl_task <- autoxplain_result$task
  effect_features <- head(features, min(3L, length(features)))
  effects <- lapply(effect_features, function(feature) {
    method <- if (is.numeric(explainers[[1L]]$data[[feature]])) "ale" else "pdp"
    tryCatch(
      explain_effect(
        explainers[[1L]],
        feature = feature,
        method = method,
        n_points = 16L,
        seed = autoxplain_result$provenance$seed %||% 123L,
        class = if (autoxplain_result$task == "multiclass") {
          explainers[[1L]]$class_levels[[1L]]
        } else {
          NULL
        }
      ),
      error = function(error) NULL
    )
  })
  names(effects) <- effect_features
  effects <- effects[!vapply(effects, is.null, logical(1))]
  list(screening = screening, audit = audit, effects = effects)
}

#' Create a lightweight AutoXplainR dashboard
#'
#' `create_simple_dashboard()` now uses the same dependency-free evidence
#' report as [generate_dashboard()].
#'
#' @inheritParams generate_dashboard
#' @param ... Additional arguments forwarded to [generate_dashboard()].
#' @return The normalized output path, invisibly.
#' @export
create_simple_dashboard <- function(autoxplain_result,
                                    output_file = "autoxplain-dashboard.html",
                                    top_features = 8L,
                                    sample_instances = 3L,
                                    open_browser = FALSE,
                                    n_repeats = 10L,
                                    ...) {
  generate_dashboard(
    autoxplain_result = autoxplain_result,
    output_file = output_file,
    top_features = top_features,
    sample_instances = sample_instances,
    include_llm_report = FALSE,
    open_browser = open_browser,
    n_repeats = n_repeats,
    ...
  )
}

prepare_dashboard_data <- function(autoxplain_result,
                                   top_features = 8L,
                                   sample_instances = 3L,
                                   n_repeats = 10L,
                                   max_models = 5L) {
  if (!inherits(autoxplain_result, "autoxplain_result")) {
    stop("`autoxplain_result` must be returned by `autoxplain()`.", call. = FALSE)
  }
  top_features <- assert_count(top_features, "top_features")
  assert_count(sample_instances, "sample_instances")
  selected <- seq_len(min(assert_count(max_models, "max_models"),
                          length(autoxplain_result$models)))
  explainers <- as_explainers(autoxplain_result, models = selected)
  importance_metric <- result_importance_metric(autoxplain_result)
  importance_list <- lapply(seq_along(explainers), function(index) {
    calculate_permutation_importance(
      explainers[[index]], metric = importance_metric, n_repeats = n_repeats,
      seed = (autoxplain_result$provenance$seed %||% 123L) + index - 1L
    )
  })
  names(importance_list) <- names(explainers)
  importance <- importance_list[[1L]]
  features <- head(importance$feature, min(top_features, nrow(importance)))
  pdp_list <- lapply(explainers, function(explainer) {
    calculate_partial_dependence_multi(
      explainer, features = features, n_points = 20L,
      seed = autoxplain_result$provenance$seed %||% 123L
    )
  })
  feature_types <- vapply(
    explainers[[1L]]$data[features],
    function(x) if (is.numeric(x)) "numeric" else "categorical",
    character(1)
  )
  audit <- audit_explanations(
    explainers, features = features, metric = importance_metric,
    n_repeats = n_repeats,
    seed = autoxplain_result$provenance$seed %||% 123L
  )
  list(
    importance = importance,
    importance_list = importance_list,
    pdp_data = pdp_list[[1L]],
    pdp_list = pdp_list,
    models_analyzed = names(explainers),
    top_features = features,
    feature_types = feature_types,
    model_characteristics = autoxplain_result$model_characteristics,
    fitting_diagnostics = audit$performance,
    correlation_insights_html = calculate_correlation_insights(
      autoxplain_result, names(explainers)
    ),
    audit = audit
  )
}

calculate_correlation_insights <- function(autoxplain_result, model_names = NULL) {
  explainers <- as_explainers(autoxplain_result, models = model_names)
  if (length(explainers) < 2L) return("One model supplied; model-agreement diagnostics are not available.")
  predictions <- do.call(cbind, lapply(explainers, function(x) {
    prediction <- predict(x, x$data)
    if (is.matrix(prediction)) max.col(prediction, ties.method = "first") else prediction
  }))
  correlation <- suppressWarnings(stats::cor(predictions, method = "spearman"))
  mean_correlation <- mean(correlation[lower.tri(correlation)], na.rm = TRUE)
  paste0(
    "Mean pairwise prediction rank correlation is ",
    format(mean_correlation, digits = 3),
    ". This describes the supplied models only."
  )
}

# Legacy internal helper retained so downstream code fails gracefully rather
# than depending on generated R Markdown implementation details.
create_dashboard_rmd <- function(data_files, performance_weight = NULL) {
  warning(
    "`create_dashboard_rmd()` is retired. Use `render_explanation_report()` for a standalone report.",
    call. = FALSE
  )
  c(
    "---",
    "title: 'Retired AutoXplainR dashboard template'",
    "output: html_document",
    "---",
    "Use `render_explanation_report()` to create the current evidence dashboard."
  )
}

create_simple_html <- function(imp_plot = NULL,
                               model_plot = NULL,
                               autoxplain_result,
                               importance = NULL) {
  if (!inherits(autoxplain_result, "autoxplain_result")) {
    stop("`autoxplain_result` must be returned by `autoxplain()`.", call. = FALSE)
  }
  audit <- audit_explanations(
    as_explainers(autoxplain_result),
    metric = result_importance_metric(autoxplain_result),
    n_repeats = 5L
  )
  explanation_report_html(audit, "AutoXplainR Explanation Evidence Report")
}

result_importance_metric <- function(result) {
  importance_metric_from_primary(result$evaluation$primary_metric %||% NULL) %||% "auto"
}
