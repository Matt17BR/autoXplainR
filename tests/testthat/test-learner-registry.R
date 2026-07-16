test_that("learner catalog separates families, backends, and behavior", {
  catalog <- learner_catalog()
  expect_s3_class(catalog, "autoxplain_learner_catalog")
  expect_setequal(catalog$family, c(
    "linear", "regularized", "additive", "tree", "forest", "boosting",
    "neural", "kernel", "neighbors", "mars"
  ))
  expect_true(all(c(
    "backend", "supported_tasks", "available", "nonlinearity",
    "interactions", "strengths", "cautions", "one_se_priority",
    "complexity_proxy", "dependency_status", "installed_version", "status_note"
  ) %in% names(catalog)))
  expect_identical(anyDuplicated(catalog$one_se_priority), 0L)
  expect_true(all(catalog$available[catalog$family %in% c("linear", "tree", "neural")]))
  expect_output(print(catalog), "families")

  regression <- learner_catalog("regression")
  expect_true(all(grepl("regression", regression$supported_tasks, fixed = TRUE)))
  multiclass <- learner_catalog("multiclass")
  expect_false(any(multiclass$family %in% c("additive", "mars")))
  expect_true(all(grepl("multiclass", multiclass$supported_tasks, fixed = TRUE)))
  expect_output(print(multiclass[c("family", "backend")]), "families")
  expect_error(learner_catalog("survival"), "arg")
})

test_that("dependency status distinguishes core, missing, and outdated backends", {
  core <- AutoXplainR:::learner_dependency_status(list(package = NULL))
  expect_identical(core$status, "core")
  expect_true(core$available)

  missing <- AutoXplainR:::learner_dependency_status(list(
    package = "an_autoxplain_package_that_does_not_exist",
    minimum_version = "1.0"
  ))
  expect_identical(missing$status, "missing")
  expect_false(missing$installed)
  expect_match(missing$reason, "not installed")

  outdated <- AutoXplainR:::learner_dependency_status(list(
    package = "stats",
    minimum_version = "9999.0"
  ))
  expect_identical(outdated$status, "outdated")
  expect_true(outdated$installed)
  expect_match(outdated$reason, "requires")
})

test_that("portfolio dependency checks are explicit and reproducible", {
  core <- AutoXplainR:::resolve_tuning_learners("core", NULL, "regression")
  expect_identical(core, c("linear", "tree", "neural"))

  catalog <- learner_catalog("regression")
  missing <- catalog$dependency[!catalog$available & grepl("recommended", catalog$portfolios)]
  missing <- setdiff(unique(missing), "AutoXplainR core")
  expect_message(
    dry_run <- install_model_engines("recommended", dry_run = TRUE),
    "Dry run only; no packages were installed"
  )
  expect_setequal(dry_run, intersect(missing, c("glmnet", "mgcv", "ranger", "xgboost")))
  expect_error(install_model_engines(dry_run = NA), "TRUE or FALSE")

  if (length(dry_run)) {
    expect_error(
      AutoXplainR:::resolve_tuning_learners("recommended", NULL, "regression"),
      "requested model portfolio is unavailable"
    )
  } else {
    expect_setequal(
      AutoXplainR:::resolve_tuning_learners("recommended", NULL, "regression"),
      c("linear", "regularized", "additive", "tree", "forest", "boosting")
    )
  }
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
  expect_true(all(c(
    "family", "backend", "simplicity_rank", "complexity_definition", "seed"
  ) %in% names(first)))
  expect_true(all(is.finite(first$seed)))

  reordered <- AutoXplainR:::local_tuning_plan(
    max_models = 7, n = 100, p = 5, task = "regression", n_classes = 1,
    learners = c("neural", "tree", "linear"), seed = 91
  )
  shared <- intersect(first$configuration_id, reordered$configuration_id)
  expect_equal(
    first$seed[match(shared, first$configuration_id)],
    reordered$seed[match(shared, reordered$configuration_id)]
  )
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

test_that("small recommended budgets cover important tuning dimensions", {
  additive <- AutoXplainR:::additive_learner_grid(200, 12, "regression", 1)[1:4]
  expect_gt(length(unique(vapply(additive, `[[`, logical(1), "select"))), 1L)
  expect_gt(length(unique(vapply(additive, `[[`, numeric(1), "gamma"))), 1L)
  expect_gt(length(unique(vapply(additive, `[[`, integer(1), "k"))), 1L)

  forest <- AutoXplainR:::forest_learner_grid(200, 12, "regression", 1)[1:4]
  expect_gt(length(unique(vapply(forest, `[[`, numeric(1), "mtry"))), 1L)
  expect_gt(length(unique(vapply(forest, `[[`, numeric(1), "min.node.size"))), 1L)
  expect_gt(length(unique(vapply(forest, `[[`, numeric(1), "sample.fraction"))), 1L)
  expect_gt(length(unique(vapply(forest, `[[`, character(1), "splitrule"))), 1L)
})

test_that("count validation rejects non-finite and overflowing values", {
  expect_error(AutoXplainR:::assert_count(Inf, "max_models"), "whole number")
  expect_error(
    AutoXplainR:::assert_count(.Machine$integer.max + 1, "max_models"),
    "whole number"
  )
  expect_error(AutoXplainR:::assert_count(1.5, "max_models"), "whole number")
  expect_identical(AutoXplainR:::assert_count(0, "seed", 0L), 0L)
})

test_that("automatic tuning budgets are portfolio-aware and custom-family aware", {
  expect_identical(AutoXplainR:::default_local_tuning_budget("core"), 15L)
  expect_identical(AutoXplainR:::default_local_tuning_budget("recommended"), 30L)
  expect_identical(AutoXplainR:::default_local_tuning_budget("extended"), 40L)
  expect_identical(
    AutoXplainR:::default_local_tuning_budget("recommended", c("linear", "tree")),
    10L
  )
})
