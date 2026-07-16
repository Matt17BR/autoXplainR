test_that("cross-model ALE curves share rows, target, and grid", {
  result <- autoxplain(
    mtcars,
    "mpg",
    model_set = "comparison",
    test_fraction = 0.4,
    seed = 2026
  )
  comparison <- compare_model_effects(
    result,
    feature = "wt",
    n_points = 6L,
    min_support = 0.2
  )

  expect_s3_class(comparison, "autoxplain_model_effects")
  expect_identical(comparison$method, "ale")
  expect_identical(comparison$prediction_target, "predicted value")
  expect_gte(comparison$n_models, 2L)
  expect_identical(comparison$n_effect_rows, comparison$n_evaluation_rows)
  expect_equal(
    nrow(comparison$comparisons),
    choose(comparison$n_models, 2L)
  )
  grids <- split(comparison$curves$feature_value, comparison$curves$model_id)
  expect_true(all(vapply(grids[-1L], identical, logical(1), grids[[1L]])))
  expect_true(all(comparison$curves$support >= 0 & comparison$curves$support <= 1))
  expect_true(all(comparison$comparisons$n_overlap > 0L))
  expect_true(all(is.finite(comparison$comparisons$centered_shape_rmse)))
  expect_true(all(comparison$models$effect_status == "included"))
  expect_true(any(comparison$models$best_performance))
  expect_match(comparison$scope$inference, "not causal effects", fixed = TRUE)
  expect_output(print(comparison), "Findings")

  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(comparison, path)
  restored <- readRDS(path)
  expect_equal(restored$curves, comparison$curves)
  expect_equal(restored$comparisons, comparison$comparisons)
})

test_that("PDP reports sampled effect rows separately from evaluation rows", {
  result <- autoxplain(
    mtcars,
    "mpg",
    model_set = "comparison",
    test_fraction = 0.45,
    seed = 2027
  )
  comparison <- compare_model_effects(
    result,
    feature = "wt",
    method = "pdp",
    n_points = 5L,
    sample_size = 4L,
    seed = 18L,
    min_support = 0
  )

  expect_identical(comparison$n_effect_rows, 4L)
  expect_gt(comparison$n_evaluation_rows, comparison$n_effect_rows)
  expect_match(comparison$scope$alignment, "same reproducible sample")
  sampled_rows <- lapply(comparison$effects, attr, "reference_rows")
  expect_true(all(vapply(sampled_rows[-1L], identical, logical(1), sampled_rows[[1L]])))
  expect_match(comparison$scope$intervals, "fixed-model")

  misaligned <- comparison$effects
  attr(misaligned[[2L]], "reference_rows") <- rev(attr(misaligned[[2L]], "reference_rows"))
  expect_error(
    AutoXplainR:::comparison_assert_aligned_effects(misaligned, "wt"),
    "reference rows"
  )
})

test_that("shape summaries require at least two supported grid points", {
  curves <- data.frame(
    model_id = rep(c("first", "second"), each = 3L),
    family = rep(c("linear", "tree"), each = 3L),
    backend = rep(c("stats", "rpart"), each = 3L),
    grid_index = rep(1:3, 2L),
    feature_value = rep(1:3, 2L),
    effect_value = c(1, 2, 3, 2, 5, 8),
    supported = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE),
    dependence_warning = FALSE,
    stringsAsFactors = FALSE
  )
  pairs <- AutoXplainR:::comparison_effect_pairs(curves, "x", "pdp", 0.8)
  findings <- AutoXplainR:::comparison_effect_findings(
    pairs,
    data.frame(model_id = character(), error = character()),
    dependence_warning = FALSE,
    method = "pdp",
    min_support = 0.8
  )

  expect_identical(pairs$n_overlap, 1L)
  expect_true(is.na(pairs$centered_shape_rmse))
  expect_true("insufficient_overlap" %in% findings$code)
})

test_that("categorical PDP comparison aligns levels and support", {
  set.seed(606)
  data <- data.frame(
    group = factor(rep(c("a", "b", "c"), each = 40L)),
    x = rnorm(120)
  )
  data$y <- as.numeric(data$group) + data$x^2 + rnorm(120, sd = 0.15)
  result <- autoxplain(
    data,
    "y",
    model_set = "comparison",
    test_fraction = 0.3,
    seed = 606
  )
  comparison <- compare_model_effects(
    result,
    feature = "group",
    method = "pdp",
    n_points = 3L,
    min_support = 0.5
  )

  expect_setequal(as.character(comparison$curves$feature_value), c("a", "b", "c"))
  expect_true(all(is.na(comparison$comparisons$direction_agreement)))
  expect_true(all(comparison$comparisons$n_overlap == 3L))
  expect_error(
    compare_model_effects(result, "group", method = "ale"),
    "ALE supports numeric features"
  )
})

test_that("multiclass comparison retains a non-first probability target", {
  result <- autoxplain(
    iris,
    "Species",
    model_set = "comparison",
    test_fraction = 0.35,
    seed = 707
  )
  comparison <- compare_model_effects(
    result,
    feature = "Petal.Length",
    method = "pdp",
    class = "virginica",
    n_points = 5L,
    seed = 707
  )

  expect_identical(comparison$prediction_class, "virginica")
  expect_identical(
    comparison$prediction_target,
    "probability for class `virginica`"
  )
  expect_true(all(vapply(comparison$effects, function(effect) {
    identical(attr(effect, "prediction_class"), "virginica")
  }, logical(1))))
  expect_error(
    compare_model_effects(result, "Petal.Length", class = "not-a-class"),
    "must name one multiclass outcome level"
  )
})

test_that("effect comparison validates model and feature scope", {
  quick <- autoxplain(mtcars, "mpg", seed = 12)
  expect_error(compare_model_effects(quick, "wt"), "at least two")
  comparison <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 12)
  expect_error(compare_model_effects(comparison, "missing"), "unavailable")
  expect_error(compare_model_effects(comparison, "wt", class = "x"), "only used")
  expect_error(compare_model_effects(comparison, "wt", min_support = 2), "from 0 and 1")
})

test_that("one failed effect retains requested-model status and a successful best", {
  result <- autoxplain(
    mtcars,
    "mpg",
    model_set = "comparison",
    test_fraction = 0.4,
    seed = 909
  )
  candidates <- result$leaderboard[result$leaderboard$role != "baseline", , drop = FALSE]
  metric <- result$evaluation$primary_metric
  best_index <- if (metric %in% AutoXplainR:::higher_is_better_metrics()) {
    which.max(candidates[[metric]])
  } else {
    which.min(candidates[[metric]])
  }
  failed_id <- candidates$model_id[[best_index]]
  original_explain_effect <- AutoXplainR::explain_effect
  testthat::local_mocked_bindings(
    explain_effect = function(model, ...) {
      if (identical(model$label, failed_id)) {
        stop("deliberate grid prediction failure", call. = FALSE)
      }
      original_explain_effect(model, ...)
    },
    .package = "AutoXplainR"
  )

  comparison <- compare_model_effects(
    result,
    "wt",
    method = "pdp",
    n_points = 5L,
    min_support = 0
  )
  failed_row <- comparison$models[comparison$models$model_id == failed_id, , drop = FALSE]

  expect_identical(nrow(comparison$models), length(comparison$requested_model_ids))
  expect_identical(failed_row$effect_status, "failed")
  expect_match(failed_row$effect_error, "deliberate grid prediction failure")
  expect_true(failed_row$best_performance_before_effects)
  expect_false(failed_row$best_performance)
  expect_true(any(
    comparison$models$best_performance &
      comparison$models$effect_status == "included"
  ))
  expect_true("effect_failures" %in% comparison$findings$code)
})

test_that("effect plots accept style overrides on a graphics device", {
  result <- autoxplain(
    mtcars,
    "mpg",
    model_set = "comparison",
    test_fraction = 0.4,
    seed = 1001
  )
  comparison <- compare_model_effects(
    result,
    "wt",
    n_points = 5L,
    min_support = 0.5
  )
  comparison$curves$supported[[1L]] <- FALSE
  path <- tempfile(fileext = ".pdf")
  on.exit(unlink(path), add = TRUE)
  grDevices::pdf(path)
  returned <- plot(
    comparison,
    type = "l",
    col = c("#0072B2", "#D55E00", "#009E73"),
    lty = 3,
    pch = 4,
    main = "Effect comparison"
  )
  grDevices::dev.off()

  expect_identical(returned, comparison)
  expect_gt(file.info(path)$size, 0)
})
