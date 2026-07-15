test_that("optional plotting methods return widgets", {
  skip_if_not_installed("plotly")
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

  result <- autoxplain(fixture$train, "y", test_data = fixture$test)
  expect_s3_class(plot_model_correlations(result), "plotly")
  expect_s3_class(plot_model_comparison(result), "plotly")
  expect_s3_class(plot_model_comparison(result, performance_metric = "mae"), "plotly")

  expect_error(plot_permutation_importance(data.frame(x = 1)), "must contain")
  expect_error(plot_partial_dependence(data.frame(x = 1)), "feature-effect")
  expect_error(plot_partial_dependence_multi(list()), "non-empty")
  expect_error(plot_model_comparison(list()), "must be returned")

  one_model <- result
  one_model$models <- one_model$models[1L]
  expect_error(plot_model_correlations(one_model), "At least two")
  no_metric <- result
  no_metric$leaderboard <- data.frame(model_id = names(result$models))
  expect_error(plot_model_comparison(no_metric), "No suitable")
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
