test_that("automatic intervals compare the training choice despite a different holdout winner", {
  result <- autoxplain(mtcars, "mpg", max_models = 4, nfolds = 3, seed = 2026, explain = FALSE)
  expect_false(identical(result$evaluation$winner, result$provenance$primary_model_id))
  expected <- result$evaluation$metrics[[result$provenance$primary_model_id]][["rmse"]] -
    result$evaluation$metrics$simple_baseline[["rmse"]]
  set.seed(73)
  state <- .Random.seed
  prepared <- AutoXplainR:::prepare_report_uncertainty(result)
  expect_identical(.Random.seed, state)
  expect_identical(prepared$.report_uncertainty$status, "computed")
  expect_equal(prepared$performance_uncertainty$estimates$estimate[3], expected)
  expect_identical(prepared$performance_uncertainty$units, nrow(result$test_data))
  html <- AutoXplainR:::explorer_baseline_comparison(prepared, AutoXplainR:::explorer_models(prepared))
  expect_true(grepl("Training-selected primary", html, fixed = TRUE))
  expect_true(grepl("fitting and selection uncertainty are excluded", html, fixed = TRUE))
  expect_true(grepl("Fewer than 20 sampling units", html, fixed = TRUE))
  expect_true(grepl("independent observations assumed", html, fixed = TRUE))
  prepared$tuning$refit$fallback_used <- TRUE
  html <- AutoXplainR:::explorer_baseline_comparison(prepared, AutoXplainR:::explorer_models(prepared))
  expect_true(grepl("Refit fallback primary", html, fixed = TRUE))
  custom_result <- evaluate_models(
    result$models[c("main_model", "linear_model")], result$test_data, "mpg",
    primary = "main_model", reference = "linear_model",
    labels = c(main_model = "Primary tree", linear_model = "Linear regression")
  )
  custom <- AutoXplainR:::prepare_report_uncertainty(custom_result)
  expect_identical(custom$performance_uncertainty$reference_model_id, "linear_model")
  expect_equal(
    custom$performance_uncertainty$estimates$estimate[3],
    result$evaluation$metrics$main_model[["rmse"]] - result$evaluation$metrics$linear_model[["rmse"]]
  )
  html <- AutoXplainR:::explorer_baseline_comparison(custom, AutoXplainR:::explorer_models(custom))
  expect_true(grepl("Linear regression", html, fixed = TRUE))
  expect_false(grepl("Intercept-only baseline", html, fixed = TRUE))
})

test_that("group bootstrap aligns dropped rows and samples surviving whole groups", {
  set.seed(442)
  data <- data.frame(site = rep(seq_len(20), rep(3:6, 5)), x = rnorm(90), y = rnorm(90))
  seed <- 77L
  held_out <- withr::with_seed(seed, sample.int(20, 4))
  data$x[data$site == held_out[[1]]] <- NA_real_
  other <- which(data$site == held_out[[2]])
  data$x[other[[1]]] <- NA_real_
  result <- autoxplain(data, "y",
    model_set = "quick", seed = seed, explain = FALSE,
    validation = validation_split(group = "site"),
    preprocessing_config = list(missing_value_strategy = "drop_rows")
  )
  expected_rows <- which(data$site %in% held_out & !is.na(data$x))
  expect_identical(result$validation$evaluation_rows, expected_rows)
  expect_identical(result$validation$evaluation_groups, as.character(data$site[expected_rows]))
  expect_identical(result$evaluation_context$site, data$site[expected_rows])
  prepared <- AutoXplainR:::prepare_report_uncertainty(result)
  output <- prepared$performance_uncertainty
  expect_identical(prepared$.report_uncertainty$status, "computed")
  expect_identical(output$unit, "group")
  expect_identical(output$units, 3L)
  predictions <- result$evaluation$predictions
  labels <- data$site[expected_rows]
  groups <- lapply(unique(labels), function(label) which(labels == label))
  oracle <- withr::with_seed(123, replicate(1000, {
    rows <- unlist(groups[sample.int(length(groups), length(groups), replace = TRUE)], use.names = FALSE)
    cases <- predictions[rows, ]
    sqrt(mean((cases$observed - cases$primary_prediction)^2)) -
      sqrt(mean((cases$observed - cases$baseline_prediction)^2))
  }))
  expect_equal(output$draws$difference, oracle)
  expect_true(all(result$data_context$row_map$source == "data"))
  expect_silent(AutoXplainR:::validate_data_context(result$data_context, result$training_data, result$test_data))
})

test_that("auto, explicit, disabled, unsupported and reused-evaluation states differ", {
  data <- transform(mtcars, time = seq_len(nrow(mtcars)))
  temporal <- autoxplain(data, "mpg",
    model_set = "quick", explain = FALSE,
    validation = validation_split(time = "time", gap = 1)
  )
  unavailable <- AutoXplainR:::prepare_report_uncertainty(temporal)
  expect_identical(unavailable$.report_uncertainty$status, "unavailable")
  expect_match(unavailable$.report_uncertainty$reason, "Temporal")
  expect_null(unavailable$performance_uncertainty)
  expect_error(AutoXplainR:::prepare_report_uncertainty(temporal, TRUE), "Temporal")
  expect_error(AutoXplainR:::prepare_report_uncertainty(temporal, NA), "TRUE, FALSE")

  result <- autoxplain(mtcars, "mpg", model_set = "quick", explain = FALSE)
  computed <- AutoXplainR:::prepare_report_uncertainty(result, TRUE)
  disabled <- AutoXplainR:::prepare_report_uncertainty(computed, FALSE)
  expect_null(disabled$performance_uncertainty)
  expect_identical(disabled$.report_uncertainty$status, "not_run")
  expect_match(disabled$.report_uncertainty$reason, "disabled")
  # This is the exact provenance flag the H2O nfolds=0 validation path records.
  result <- evaluate_models(result$models, result$test_data, "mpg",
    primary = "main_model", reference = "simple_baseline", evaluation_role = "validation"
  )
  result$provenance$test_used_for_validation <- TRUE
  # Capture synthetic engine reuse provenance before exercising report policy.
  result <- AutoXplainR:::seal_evaluation_result(result)
  reused <- AutoXplainR:::prepare_report_uncertainty(result)
  expect_identical(reused$.report_uncertainty$status, "unavailable")
  expect_match(reused$.report_uncertainty$reason, "reused for model selection")
  descriptive <- AutoXplainR:::prepare_report_uncertainty(result, TRUE)
  expect_identical(descriptive$.report_uncertainty$status, "computed")
  expect_true(any(grepl("descriptive interval", descriptive$performance_uncertainty$notes, fixed = TRUE)))
  html <- AutoXplainR:::explorer_baseline_comparison(descriptive, AutoXplainR:::explorer_models(descriptive))
  expect_true(grepl("reused for model selection", html, fixed = TRUE))
})

test_that("supplied evaluation rows use observation units after omission", {
  set.seed(229)
  train <- data.frame(x = rnorm(45), y = rnorm(45))
  evaluation <- data.frame(x = rnorm(15), y = rnorm(15))
  evaluation$x[c(1, 7, 8)] <- NA_real_
  result <- autoxplain(train, "y",
    test_data = evaluation, model_set = "quick", explain = FALSE,
    preprocessing_config = list(missing_value_strategy = "drop_rows")
  )
  prepared <- AutoXplainR:::prepare_report_uncertainty(result)
  expect_identical(prepared$provenance$evaluation_role, "evaluation")
  expect_identical(prepared$performance_uncertainty$units, 12L)
  expect_identical(prepared$performance_uncertainty$unit, "observation")
  expect_true(any(grepl("independent sampling units", prepared$performance_uncertainty$notes, fixed = TRUE)))
})
