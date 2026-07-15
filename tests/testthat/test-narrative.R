test_that("local narratives are deterministic and limitation-first", {
  fixture <- make_regression_fixture()
  explainer <- explain_model(fixture$model, fixture$test, "y",
                             metadata = list(evaluation_role = "test"))
  audit <- audit_explanations(explainer, features = c("x1", "x2"), n_repeats = 4)
  first <- generate_natural_language_report(audit, use_remote = FALSE)
  second <- generate_natural_language_report(audit, use_remote = FALSE)
  expect_identical(first, second)
  expect_match(first, "not a certification", ignore.case = TRUE)
  expect_match(first, "not be read as causal", ignore.case = TRUE)
})

test_that("evidence prompt constrains unsupported claims", {
  context <- list(
    task_type = "regression", target_column = "y", n_models = 2,
    n_features = 3, best_model_type = "lm", best_metric = "rmse",
    best_performance = 1.2, disclosure = "aggregated"
  )
  prompt <- AutoXplainR:::create_report_prompt(context)
  expect_match(prompt, "Never imply causality")
  expect_match(prompt, "never certification")
  expect_false(grepl("GEMINI_API_KEY", prompt, fixed = TRUE))
})
