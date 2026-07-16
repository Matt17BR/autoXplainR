test_that("learner catalog separates families, backends, and behavior", {
  catalog <- learner_catalog()
  expect_s3_class(catalog, "autoxplain_learner_catalog")
  expect_setequal(catalog$family, c("linear", "tree", "neural"))
  expect_true(all(c(
    "backend", "supported_tasks", "available", "nonlinearity",
    "interactions", "strengths", "cautions"
  ) %in% names(catalog)))
  expect_true(all(catalog$available))
  expect_output(print(catalog), "families")

  regression <- learner_catalog("regression")
  expect_true(all(grepl("regression", regression$supported_tasks, fixed = TRUE)))
  expect_error(learner_catalog("survival"), "arg")
})

test_that("generic tuning plans are deterministic and family explicit", {
  first <- AutoXplainR:::local_tuning_plan(
    max_models = 7, n = 100, p = 5, task = "regression", n_classes = 1,
    learners = c("linear", "tree", "neural"), seed = 91
  )
  second <- AutoXplainR:::local_tuning_plan(
    max_models = 7, n = 100, p = 5, task = "regression", n_classes = 1,
    learners = c("linear", "tree", "neural"), seed = 91
  )
  expect_identical(first, second)
  expect_equal(nrow(first), 7L)
  expect_true(is.list(first$parameters))
  expect_true(all(c("family", "backend", "simplicity_rank", "seed") %in% names(first)))
  expect_equal(first$seed, 91:97)
  expect_error(
    AutoXplainR:::local_tuning_plan(
      3, 100, 5, "regression", 1, learners = c("linear", "unknown")
    ),
    "Unknown learner"
  )
  expect_error(
    AutoXplainR:::local_tuning_plan(
      2, 100, 5, "regression", 1, learners = c("linear", "tree", "neural")
    ),
    "number of learner families"
  )
})
