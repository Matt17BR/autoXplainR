test_that("documented narrative merges retained evaluation and fitted explanations without recomputation", {
  result <- autoxplain(iris, "Species", seed = 2026)
  original <- result$explanations
  local_mocked_bindings(
    prepare_model_report_data = function(...) stop("must not recompute explanations"),
    .package = "AutoXplainR"
  )
  memo <- generate_natural_language_report(result)
  context <- AutoXplainR:::prepare_narrative_context(result)
  expect_identical(result$explanations, original)
  expect_identical(context$best_performance, result$evaluation$metrics$main_model[["log_loss"]])
  expected <- subset(original$audit$importance, model == "main_model")
  expected <- expected[order(expected$importance, decreasing = TRUE), ]
  expect_identical(context$importance_summary$rows$importance, head(expected$importance, 3L))
  expect_equal(length(context$effect_summary), length(original$effects))
  expect_match(memo, "Fitted feature evidence for multinomial logistic regression", fixed = TRUE)
  expect_match(memo, context$importance_summary$top_features[[1L]], fixed = TRUE)
  expect_match(memo, "ALE for", fixed = TRUE)
  expect_false(grepl("can represent none", memo, fixed = TRUE))
})

test_that("audit and importance overrides preserve evaluation and other retained components", {
  result <- autoxplain(mtcars, "mpg")
  audit <- result$explanations$audit
  audit$findings <- data.frame(severity = "note", code = "custom_check", message = "Review the instrument.",
                               recommendation = "Check its calibration.")
  importance <- data.frame(feature = "supplied_feature", importance = 12.345)
  context <- AutoXplainR:::prepare_narrative_context(result, audit, importance)
  memo <- generate_natural_language_report(result, audit = audit, importance_data = importance)
  expect_identical(context$best_performance, result$evaluation$metrics$main_model[["rmse"]])
  expect_identical(context$findings, audit$findings)
  expect_identical(context$importance_summary$rows, importance)
  expect_equal(length(context$effect_summary), length(result$explanations$effects))
  expect_match(memo, "simple baseline", fixed = TRUE)
  expect_match(memo, "Review the instrument.", fixed = TRUE)
  expect_match(memo, "supplied_feature: 12.345", fixed = TRUE)
  expect_match(memo, "identity is not verified", fixed = TRUE)
  omitted <- AutoXplainR:::prepare_narrative_context(result, effects = list())
  expect_length(omitted$effect_summary, 0L)
  expect_false(is.null(omitted$importance_summary))
})

test_that("narrative attachments reject foreign identities and retain valid serialized overrides", {
  result <- autoxplain(mtcars, "mpg", seed = 13)
  other <- autoxplain(mtcars, "mpg", seed = 14)
  expect_error(generate_natural_language_report(result, audit = other$explanations$audit),
               "same selected model explainers")
  expect_error(generate_natural_language_report(result, pdp_data = other$explanations$effects),
               "same primary model")
  expect_error(generate_natural_language_report(result, importance_data = other$explanations$screening),
               "same primary model")
  copy <- unserialize(serialize(result, NULL))
  expect_silent(generate_natural_language_report(copy, audit = result$explanations$audit,
                                                 pdp_data = result$explanations$effects,
                                                 importance_data = result$explanations$screening))
})

test_that("unavailable evidence retains a visible reason", {
  fitted_only <- autoxplain(mtcars, "mpg", explain = FALSE)
  memo <- generate_natural_language_report(fitted_only)
  expect_match(memo, "Feature importance was not supplied", fixed = TRUE)
  expect_match(memo, "Fitted effect curves were not supplied", fixed = TRUE)

  result <- autoxplain(mtcars, "mpg")
  result$explanations$failures <- data.frame(feature = "sensor", reason = "No usable observations.")
  memo <- generate_natural_language_report(result)
  expect_match(memo, "Effect unavailable for sensor: No usable observations.", fixed = TRUE)
  replacement <- AutoXplainR:::prepare_narrative_context(result, effects = list())
  expect_null(replacement$effect_failures)
  expect_error(generate_natural_language_report(result, importance_data = list(bad = 1)),
               "must contain feature and importance")
})

test_that("hosted validation is disclosed as format-only rather than numerical verification", {
  result <- autoxplain(mtcars, "mpg")
  memo <- generate_natural_language_report(
    result, provider = "custom", model = "mock-model", base_url = "https://example.com/v1/chat/completions",
    structured = FALSE, transport = function(request) "RMSE was 999999 on these rows."
  )
  expect_match(memo, "RMSE was 999999", fixed = TRUE)
  expect_match(memo, "Verify every statement against the computed report", fixed = TRUE)
  expect_match(attr(memo, "narrative_provenance")$content_validation,
               "numerical grounding not checked", fixed = TRUE)
})

test_that("narratives preserve optional diagnostic status without running checks", {
  result <- autoxplain(mtcars, "mpg", explain = FALSE)
  original <- result
  context <- AutoXplainR:::prepare_narrative_context(result)
  expect_identical(context$diagnostic_status$prediction_disagreement$status, "not_run")
  expect_match(generate_natural_language_report(result), "prediction disagreement: not run", fixed = TRUE)
  expect_identical(result, original)
  result$diagnostic_status <- list(prediction_disagreement = AutoXplainR:::report_diagnostic_record(
    "prediction_disagreement", "failed", "evaluation rows", reason = "Injected prediction failure."
  ))
  local_mocked_bindings(
    prepare_report_diagnostics = function(...) stop("must not recompute optional checks"),
    .package = "AutoXplainR"
  )
  context <- AutoXplainR:::prepare_narrative_context(result)
  expect_identical(context$diagnostic_status$prediction_disagreement$status, "failed")
  memo <- generate_natural_language_report(result)
  expect_match(memo, "prediction disagreement: failed. Injected prediction failure.", fixed = TRUE)
})

test_that("dashboard aliases emit one lifecycle warning and preserve their result", {
  result <- autoxplain(mtcars, "mpg", explain = FALSE)
  for (fun in list(generate_dashboard, create_simple_dashboard)) {
    warnings <- list()
    path <- tempfile(fileext = ".html")
    output <- withCallingHandlers(fun(result, path, n_repeats = 2L), warning = function(warning) {
      warnings[[length(warnings) + 1L]] <<- warning
      invokeRestart("muffleWarning")
    })
    expect_length(warnings, 1L)
    expect_s3_class(warnings[[1L]], "autoxplain_deprecated")
    expect_match(conditionMessage(warnings[[1L]]), "render_model_report", fixed = TRUE)
    expect_match(conditionMessage(warnings[[1L]]), "no earlier than 0.6.0", fixed = TRUE)
    expect_identical(as.character(output), normalizePath(path))
    expect_type(attr(output, "diagnostic_status"), "list")
    expect_true(file.exists(output))
    unlink(path)
  }
})
