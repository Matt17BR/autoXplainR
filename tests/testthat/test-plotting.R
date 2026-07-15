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
})

test_that("model type extraction remains stable", {
  ids <- c("GBM_model_1", "DRF_model_1", "GLM_model_1", "StackedEnsemble_1")
  expect_equal(
    AutoXplainR:::extract_model_type(ids),
    c("GBM", "Random Forest", "GLM", "Ensemble")
  )
})
