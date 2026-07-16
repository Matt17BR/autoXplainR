test_that("model behavior comparison separates prior cards from computed evidence", {
  result <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 2026)
  comparison <- compare_model_behavior(result)

  expect_s3_class(comparison, "autoxplain_model_behavior")
  expect_equal(comparison$task, "regression")
  expect_setequal(
    comparison$model_ids,
    c("main_model", "small_tree", "flexible_tree")
  )
  expect_equal(nrow(comparison$prediction_pairs), choose(3L, 2L))
  expect_equal(comparison$performance_metric, "rmse")
  expect_equal(comparison$tradeoff_metric, "model_size_kb")
  expect_equal(comparison$tradeoff_kind, "resource proxy")
  expect_equal(
    comparison$distance_definition,
    "absolute difference in predicted target units"
  )
  expect_true(all(c(
    "family", "backend", "performance_score", "tradeoff_proxy",
    "pareto_optimal", "nonlinearity", "interactions", "strengths", "cautions"
  ) %in% names(comparison$models)))
  expect_true(all(comparison$models$behavior_source ==
                    "AutoXplainR learner registry"))
  expect_true(all(comparison$prediction_pairs$mean_prediction_distance >= 0))
  expect_true(all(is.na(comparison$prediction_pairs$class_disagreement_rate)))
  expect_null(comparison$feature_evidence)
  expect_null(comparison$shape_evidence)
  expect_match(comparison$scope$shape_evidence, "No automatic effect-shape")
  expect_equal(
    comparison$evidence_sources$evidence_kind,
    c("prior knowledge", "computed", "computed", "computed")
  )
  expect_false(tail(comparison$evidence_sources$available, 1L))
  expect_named(summary(comparison), c(
    "task", "n_models", "n_evaluation_rows", "evaluation_role",
    "performance_metric", "best_performance_models", "tradeoff_metric",
    "tradeoff_kind",
    "pareto_models", "largest_disagreement_pair",
    "largest_mean_prediction_distance", "distance_definition",
    "feature_rank_agreement", "scope_note"
  ))
  expect_output(print(comparison), "behavior cards = prior knowledge")
  expect_output(print(comparison), "computed evidence")
})

test_that("paired regression distances equal direct unified predictions", {
  result <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 17)
  model_ids <- c("main_model", "small_tree")
  comparison <- compare_model_behavior(result, models = model_ids)
  explainers <- as_explainers(result, models = model_ids)
  expected <- mean(abs(
    predict(explainers[[1L]], explainers[[1L]]$data) -
      predict(explainers[[2L]], explainers[[2L]]$data)
  ))

  expect_equal(
    comparison$prediction_pairs$mean_prediction_distance,
    expected
  )
  expect_equal(comparison$ambiguity$model_ids, model_ids)
})

test_that("classification comparisons use probability-aware distances", {
  set.seed(73)
  data <- data.frame(x = rnorm(220), z = rnorm(220))
  data$event <- factor(ifelse(
    data$x^2 + 0.4 * data$z + rnorm(220) > 0.8,
    "yes",
    "no"
  ))
  binary <- autoxplain(data, "event", model_set = "comparison", seed = 91)
  binary_comparison <- compare_model_behavior(binary)

  expect_equal(
    binary_comparison$distance_definition,
    "absolute difference in positive-class probability"
  )
  expect_true(all(
    binary_comparison$prediction_pairs$mean_prediction_distance >= 0 &
      binary_comparison$prediction_pairs$mean_prediction_distance <= 1
  ))
  expect_true(all(
    binary_comparison$prediction_pairs$class_disagreement_rate >= 0 &
      binary_comparison$prediction_pairs$class_disagreement_rate <= 1
  ))

  multiclass <- autoxplain(
    iris,
    "Species",
    model_set = "comparison",
    seed = 91
  )
  multiclass_comparison <- compare_model_behavior(multiclass)
  expect_equal(
    multiclass_comparison$distance_definition,
    "total-variation distance between class-probability vectors"
  )
  expect_true(all(
    multiclass_comparison$prediction_pairs$max_prediction_distance <= 1 + 1e-12
  ))
  expect_true(all(is.na(
    multiclass_comparison$prediction_pairs$prediction_rank_correlation
  )))
  expect_true(all(vapply(
    multiclass_comparison$prediction_pairs$per_class_rank_correlations,
    function(value) identical(names(value), levels(iris$Species)),
    logical(1L)
  )))
  expected_aggregate <- vapply(
    multiclass_comparison$prediction_pairs$per_class_rank_correlations,
    function(value) mean(value[is.finite(value)]),
    numeric(1L)
  )
  expect_equal(
    multiclass_comparison$prediction_pairs$mean_class_rank_correlation,
    expected_aggregate
  )
})

test_that("tuned family winners share the model behavior contract", {
  set.seed(118)
  data <- data.frame(x = rnorm(90), z = rnorm(90))
  data$y <- sin(data$x) + data$z^2 + rnorm(90, sd = 0.2)
  result <- autoxplain(
    data,
    "y",
    model_set = "tuned",
    portfolio = "core",
    max_models = 6L,
    nfolds = 2L,
    seed = 14
  )
  comparison <- compare_model_behavior(result)

  expect_gte(comparison$n_models, 2L)
  expect_equal(
    nrow(comparison$prediction_pairs),
    choose(comparison$n_models, 2L)
  )
  expect_true(all(comparison$models$family %in%
                    learner_catalog("regression")$family))
  expect_true(all(comparison$models$behavior_source ==
                    "AutoXplainR learner registry"))
})

test_that("optional explanation audit adds compact rank evidence", {
  result <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 36)
  model_ids <- c("main_model", "small_tree")
  explainers <- as_explainers(result, models = model_ids)
  audit <- audit_explanations(
    explainers,
    features = c("wt", "hp", "disp"),
    n_repeats = 3L,
    seed = 81
  )
  comparison <- compare_model_behavior(
    result,
    models = model_ids,
    explanation_audit = audit
  )

  evidence <- comparison$feature_evidence
  expect_type(evidence, "list")
  expect_equal(evidence$n_repeats, 3L)
  expect_setequal(unique(evidence$top_features$model), model_ids)
  expect_true(all(evidence$top_features$rank_within_model <= 5L))
  expect_true(all(c(
    "feature", "importance", "sign_stability", "evidence_grade", "claim"
  ) %in% names(evidence$top_features)))
  expect_equal(nrow(evidence$rank_agreement), 1L)
  expect_named(evidence$rank_agreement, c(
    "model_a", "model_b", "spearman_rank_agreement"
  ))
  expect_true("importance_range" %in% names(evidence$feature_ranges))
  expect_match(evidence$scope_note, "not population uncertainty")
  expect_true(tail(comparison$evidence_sources$available, 1L))
  expect_output(print(comparison), "3 repeats")
})

test_that("model behavior comparison rejects mismatched inputs", {
  quick <- autoxplain(mtcars, "mpg", seed = 2)
  expect_error(compare_model_behavior(quick), "at least two")
  expect_error(compare_model_behavior(list()), "returned by")

  result <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 2)
  expect_error(
    compare_model_behavior(result, performance_tolerance = -0.1),
    "performance_tolerance"
  )
  expect_error(
    compare_model_behavior(result, explanation_audit = list()),
    "audit_explanations"
  )

  incomplete_audit <- audit_explanations(
    as_explainers(result, models = c("main_model", "small_tree")),
    features = c("wt", "hp"),
    n_repeats = 2L
  )
  expect_error(
    compare_model_behavior(
      result,
      models = c("main_model", "flexible_tree"),
      explanation_audit = incomplete_audit
    ),
    "does not cover selected model IDs"
  )
})
