report_preparation_fixture <- function() {
  d <- data.frame(x = seq(-2, 2, length.out = 80))
  d$y <- d$x^2 + sin(3 * d$x) / 10
  result <- autoxplain(d, "y", model_set = "comparison", seed = 27, explain = FALSE)
  result$explanations <- AutoXplainR:::prepare_model_report_data(result, top_features = 1, n_repeats = 2)
  result
}

test_that("secondary effect failures propagate to every aggregate status surface", {
  original <- AutoXplainR::explain_effect
  local_mocked_bindings(explain_effect = function(model, ...) {
    if (model$label != "main_model") stop("secondary curve unavailable")
    original(model, ...)
  }, .package = "AutoXplainR")
  result <- report_preparation_fixture()
  status <- result$explanations$effect_status
  failed <- status[status$status == "failed", ]
  expect_setequal(failed$model_id, c("simple_baseline", "small_tree", "flexible_tree"))
  expect_identical(failed$feature, rep("x", 3))
  expect_identical(failed$reason, rep("secondary curve unavailable", 3))
  expect_identical(nrow(result$explanations$failures), 0L)
  expect_equal(
    result$explanations$effects$x$accumulated_effect,
    original(as_explainers(result, models = "main_model")[[1]], "x", n_points = 16)$accumulated_effect
  )
  view <- AutoXplainR:::report_view_model(result)
  records <- Filter(function(record) identical(record$status, "failed"), view$diagnostics)
  expect_length(records, 3)
  expect_setequal(vapply(records, function(record) record$entities$model_id, character(1)), failed$model_id)
  expect_identical(evidence_summary(result)$explanations$effect_status, status)
})

test_that("one failed class of one model does not erase successful siblings", {
  original <- AutoXplainR::explain_effect
  local_mocked_bindings(explain_effect = function(model, ..., class = NULL) {
    if (model$label == "simple_baseline" && identical(class, "versicolor")) stop("one class unavailable")
    original(model, ..., class = class)
  }, .package = "AutoXplainR")
  result <- autoxplain(iris[c("Sepal.Length", "Species")], "Species", model_set = "quick", explain = FALSE)
  result$explanations <- AutoXplainR:::prepare_model_report_data(result, top_features = 1, n_repeats = 2)
  status <- result$explanations$effect_status
  expect_equal(nrow(status), 6)
  failed <- status[status$status == "failed", ]
  expect_identical(failed$model_id, "simple_baseline")
  expect_identical(failed$feature, "Sepal.Length")
  expect_identical(failed$prediction_class, "versicolor")
  expect_identical(sum(status$status == "available"), 5L)
  expect_length(Filter(
    function(record) identical(record$status, "failed"),
    AutoXplainR:::report_view_model(result)$diagnostics
  ), 1)
  expect_identical(evidence_summary(result)$explanations$effect_status, status)
})

test_that("explicit primary effects do not hide stale secondary evidence", {
  result <- report_preparation_fixture()
  result$.report_context <- AutoXplainR:::prepare_report_context(result)
  primary <- AutoXplainR:::report_explainers(result, "main_model")[[1]]
  replacement <- list(x = explain_effect(primary, "x", method = "pdp", n_points = 7))
  changed <- AutoXplainR:::prepare_report_effects(result, result$explanations$audit,
    replacement,
    explicit_effects = TRUE
  )
  expect_identical(changed$explanations$effects_by_model$main_model$x, replacement$x)
  expect_identical(
    changed$explanations$effects_by_model$small_tree,
    result$explanations$effects_by_model$small_tree
  )
  expect_identical(changed$explanations$effect_status$method[
    changed$explanations$effect_status$model_id == "main_model"
  ], "pdp")
  result$explanations$effects_by_model$small_tree$x <- replacement$x
  expect_error(
    AutoXplainR:::prepare_report_effects(result, result$explanations$audit, replacement, explicit_effects = TRUE),
    "stale or foreign"
  )
})

test_that("explicit empty and class-specific overrides have exact scope", {
  result <- autoxplain(iris[c("Sepal.Length", "Species")], "Species", model_set = "quick", explain = FALSE)
  result$explanations <- AutoXplainR:::prepare_model_report_data(result, top_features = 1, n_repeats = 2)
  result$.report_context <- AutoXplainR:::prepare_report_context(result)
  primary <- AutoXplainR:::report_explainers(result, "main_model")[[1]]
  replacement <- list(Sepal.Length = explain_effect(primary, "Sepal.Length", class = "virginica"))
  changed <- AutoXplainR:::prepare_report_effects(result, result$explanations$audit,
    replacement,
    explicit_effects = TRUE
  )
  primary_status <- changed$explanations$effect_status[
    changed$explanations$effect_status$model_id == "main_model",
  ]
  expect_equal(nrow(primary_status), 1)
  expect_identical(primary_status$prediction_class, "virginica")
  expect_length(changed$explanations$effects_by_class$setosa$main_model, 0)
  expect_identical(changed$explanations$effects_by_class$virginica$main_model, replacement)
  empty <- AutoXplainR:::prepare_report_effects(result, result$explanations$audit,
    list(),
    explicit_effects = TRUE
  )
  expect_false(any(empty$explanations$effect_status$model_id == "main_model"))
  expect_equal(sum(empty$explanations$effect_status$model_id == "simple_baseline"), 3)
  # Corrupt a redundant retained view: validation must not silently select its
  # otherwise-valid duplicate from effects_by_model.
  result$explanations$effects_by_class$setosa$simple_baseline$Sepal.Length <- replacement$Sepal.Length
  expect_error(
    AutoXplainR:::prepare_report_effects(result, result$explanations$audit, list(), explicit_effects = TRUE),
    "stale or foreign|wrong prediction class"
  )
})

test_that("a render snapshot performs one full prediction batch per model", {
  result <- report_preparation_fixture()
  original <- AutoXplainR:::make_prediction_adapter
  calls <- integer()
  local_mocked_bindings(make_prediction_adapter = function(...) {
    adapter <- original(...)
    function(newdata) {
      calls <<- c(calls, nrow(newdata))
      adapter(newdata)
    }
  }, .package = "AutoXplainR")
  context <- AutoXplainR:::prepare_report_context(result)
  expect_equal(sum(calls == nrow(result$test_data)), 4)
  expect_equal(length(calls), 8)
  result$.report_context <- context
  before <- calls
  expect_identical(AutoXplainR:::report_predictions(result), context$predictions)
  compare_model_behavior(result)
  prediction_ambiguity(result)
  performance_uncertainty(result, n_boot = 20)
  subgroup_performance(result, "x", min_rows = 1L, metric = "mae")
  expect_identical(calls, before)
  # The cached numerical predictions agree with an independent direct model call.
  expect_equal(
    unname(context$predictions$main_model),
    unname(predict(result$models$main_model, result$test_data))
  )
})

test_that("fresh snapshots detect changed coefficients, rows, and outcomes", {
  result <- report_preparation_fixture()
  initial <- AutoXplainR:::prepare_report_context(result)
  result$.report_context <- initial
  changed <- result
  changed$models$main_model$coefficients[[1]] <- changed$models$main_model$coefficients[[1]] + 10
  expect_error(AutoXplainR:::prepare_report_context(changed), "Stored evaluation evidence")
  reevaluated <- evaluate_models(changed$models, changed$test_data, changed$target_column,
    primary = "main_model", reference = "simple_baseline"
  )
  fresh <- AutoXplainR:::prepare_report_context(reevaluated)
  expect_equal(fresh$predictions$main_model - initial$predictions$main_model, rep(10, nrow(result$test_data)))
  changed <- result
  changed$test_data <- changed$test_data[rev(seq_len(nrow(changed$test_data))), ]
  expect_error(AutoXplainR:::prepare_report_context(changed), "Stored evaluation evidence")
  changed <- result
  changed$test_data$y <- changed$test_data$y + 1
  expect_error(AutoXplainR:::prepare_report_context(changed), "Stored evaluation evidence")
  expect_identical(result$.report_context, initial)
})
