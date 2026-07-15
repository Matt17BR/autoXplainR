test_that("H2O AutoML integrates through the model-agnostic contract", {
  skip_if_not(identical(tolower(Sys.getenv("AUTOXPLAIN_RUN_H2O")), "true"))
  skip_if_not_installed("h2o")

  set.seed(101)
  data <- data.frame(x1 = rnorm(100), x2 = rnorm(100))
  data$y <- as.integer(data$x1 + data$x2 > 0)
  result <- autoxplain(
    data[1:75, ], "y", test_data = data[76:100, ], max_models = 1,
    max_runtime_secs = 30, nfolds = 0, seed = 101, verbosity = "quiet",
    engine = "h2o"
  )
  on.exit(try(h2o::h2o.shutdown(prompt = FALSE), silent = TRUE), add = TRUE)

  expect_s3_class(result, "autoxplain_result")
  expect_equal(result$task, "binary")
  expect_equal(result$provenance$evaluation_role, "test")
  explainers <- as_explainers(result)
  expect_s3_class(explainers[[1]], "autoxplain_explainer")
  audit <- audit_explanations(explainers, n_repeats = 2)
  expect_s3_class(audit, "autoxplain_audit")
})
