test_that("optional plotting methods return widgets", {
  skip_if_package_unavailable("plotly")
  fixture <- make_regression_fixture()
  explainer <- explain_model(fixture$model, fixture$test, "y")
  importance <- calculate_permutation_importance(
    explainer, features = c("x1", "x2"), n_repeats = 3
  )
  effect <- explain_effect(explainer, "x1", n_points = 5)
  expect_s3_class(plot_permutation_importance(importance), "plotly")
  expect_s3_class(plot_partial_dependence(effect), "plotly")
  expect_s3_class(plot(importance), "plotly")
  expect_s3_class(plot(effect), "plotly")
  expect_s3_class(
    plot_partial_dependence_multi(list(x1 = effect, x1_again = effect), ncol = 1),
    "plotly"
  )

  no_interval <- importance[c("feature", "importance")]
  expect_s3_class(plot_permutation_importance(no_interval), "plotly")
  no_support <- effect[setdiff(names(effect), c("support", "conf_low", "conf_high"))]
  expect_s3_class(plot_partial_dependence(no_support), "plotly")

  result <- autoxplain(model_set = "quick", fixture$train, "y", test_data = fixture$test)
  expect_s3_class(plot_model_correlations(result), "plotly")
  expect_s3_class(plot_model_comparison(result), "plotly")
  expect_s3_class(plot_model_comparison(result, performance_metric = "mae"), "plotly")

  expect_error(plot_permutation_importance(data.frame(x = 1)), "must contain")
  expect_error(plot_partial_dependence(data.frame(x = 1)), "feature-effect")
  expect_error(plot_partial_dependence_multi(list()), "non-empty")
  expect_error(plot_model_comparison(list()), "must be returned")

  one_model <- evaluate_models(list(fitted = fixture$model), fixture$test, "y")
  expect_error(plot_model_correlations(one_model), "At least two")
  expect_error(plot_model_comparison(result, performance_metric = "unknown_metric"), "metric")
})

test_that("model type extraction remains stable", {
  ids <- c("GBM_model_1", "DRF_model_1", "GLM_model_1", "StackedEnsemble_1")
  expect_equal(
    AutoXplainR:::extract_model_type(ids),
    c("GBM", "Random Forest", "GLM", "Ensemble")
  )
  expect_equal(
    AutoXplainR:::extract_model_type(c("DeepLearning_1", "XGBoost_1", "unknown_1")),
    c("Deep Learning", "XGBoost", "unknown")
  )
})

test_that("classification heatmap agrees on labels despite probability column order", {
  skip_if_package_unavailable("plotly")
  observed <- factor(c("a", "a", "b", "c"), levels = c("a", "b", "c"))
  data <- data.frame(row = seq_along(observed), y = observed)
  probabilities <- function(labels) {
    p <- matrix(0.05, length(labels), 3, dimnames = list(NULL, c("a", "b", "c")))
    p[cbind(seq_along(labels), match(labels, colnames(p)))] <- 0.9
    p
  }
  first <- probabilities(c("a", "a", "b", "c"))
  second <- probabilities(c("b", "a", "c", "c"))
  make <- function(p) {
    explain_model(list(), data, "y", task = "multiclass",
      predict_function = function(model, newdata) p[newdata$row, , drop = FALSE]
    )
  }
  fixtures <- list(first = make(first), second = make(second))
  local_mocked_bindings(as_explainers = function(...) fixtures)
  plot <- plotly::plotly_build(plot_model_correlations(list(task = "multiclass")))
  expect_equal(unname(plot$x$data[[1L]]$z[1, 2]), 0.5)
  fixtures$second <- make(second[, c("c", "a", "b")])
  reordered <- plotly::plotly_build(plot_model_correlations(list(task = "multiclass")))
  expect_equal(unname(reordered$x$data[[1L]]$z), unname(plot$x$data[[1L]]$z))
  expect_equal(plot$x$data[[1L]]$zmin, 0)
})

test_that("categorical effects are separate estimates with matching uncertainty", {
  skip_if_package_unavailable("plotly")
  effect <- data.frame(
    service = factor(c("economy", "priority")), partial_dependence = c(14, 9),
    conf_low = c(12, 8), conf_high = c(17, 11)
  )
  traces <- plotly::plotly_build(plot_partial_dependence(effect))$x$data
  expect_length(traces, 1L)
  expect_identical(traces[[1L]]$mode, "markers")
  expect_equal(as.vector(traces[[1L]]$y), c(14, 9))
  expect_equal(as.vector(traces[[1L]]$error_y$array), c(3, 2))
  expect_equal(as.vector(traces[[1L]]$error_y$arrayminus), c(2, 1))
  expect_null(traces[[1L]]$fill)
})
