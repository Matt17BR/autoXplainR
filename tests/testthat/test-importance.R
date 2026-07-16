test_that("permutation importance recovers known signal", {
  fixture <- make_regression_fixture()
  explainer <- explain_model(
    fixture$model, fixture$test, "y",
    metadata = list(evaluation_role = "test")
  )
  importance <- calculate_permutation_importance(
    explainer, n_repeats = 25, seed = 11
  )

  expect_s3_class(importance, "autoxplain_importance")
  expect_equal(importance$feature[[1]], "x1")
  expect_gt(importance$importance[importance$feature == "x2"], 0)
  expect_equal(dim(attr(importance, "repeat_scores")), c(3, 25))
  expect_match(attr(importance, "interval_type"), "Monte Carlo")
  expect_output(print(importance), "Monte Carlo")
})

test_that("feature groups are permuted jointly", {
  fixture <- make_regression_fixture()
  explainer <- explain_model(fixture$model, fixture$test, "y")
  importance <- calculate_permutation_importance(
    explainer,
    feature_groups = list(signal = c("x1", "x2"), null = "noise"),
    n_repeats = 5,
    seed = 12
  )
  expect_equal(sort(importance$feature), c("null", "signal"))
  expect_gt(importance$importance[importance$feature == "signal"],
            importance$importance[importance$feature == "null"])
})

test_that("blocked permutations and random state are reproducible", {
  fixture <- make_regression_fixture()
  explainer <- explain_model(fixture$model, fixture$test, "y")
  set.seed(999)
  before <- .Random.seed
  first <- calculate_permutation_importance(
    explainer, features = "x1", within = "x2", n_repeats = 4, seed = 10
  )
  expect_equal(.Random.seed, before)
  second <- calculate_permutation_importance(
    explainer, features = "x1", within = "x2", n_repeats = 4, seed = 10
  )
  expect_equal(first, second)
})

test_that("binary metrics are finite and correctly directed", {
  fixture <- make_binary_fixture()
  explainer <- explain_model(fixture$model, fixture$test, "y")
  for (metric in c("logloss", "brier", "accuracy", "auc")) {
    importance <- calculate_permutation_importance(
      explainer, metric = metric, n_repeats = 4, seed = 15
    )
    expect_true(all(is.finite(importance$importance)))
    expect_equal(attr(importance, "metric"), metric)
  }
  expect_identical(
    attr(calculate_permutation_importance(
      explainer, metric = "brier_score", n_repeats = 2, seed = 16
    ), "metric"),
    "brier"
  )
  expect_identical(
    attr(calculate_permutation_importance(
      explainer, metric = "log_loss", n_repeats = 2, seed = 16
    ), "metric"),
    "logloss"
  )
})

test_that("binary Brier importance uses positive-class squared probability error", {
  data <- data.frame(
    probability = c(0.9, 0.7, 0.4, 0.1, 0.2, 0.8),
    noise = seq_len(6),
    outcome = factor(
      c("yes", "yes", "no", "no", "no", "yes"),
      levels = c("no", "yes")
    )
  )
  explainer <- explain_model(
    model = list(),
    data = data,
    y = "outcome",
    task = "binary",
    positive = "yes",
    predict_function = function(model, newdata) newdata$probability,
    metadata = list(primary_metric = "brier_score")
  )
  expected <- mean((data$probability - as.numeric(data$outcome == "yes"))^2)
  importance <- calculate_permutation_importance(
    explainer,
    features = "probability",
    n_repeats = 3,
    seed = 17
  )
  audit <- audit_explanations(
    explainer,
    features = "probability",
    n_repeats = 3,
    seed = 17
  )

  expect_identical(attr(importance, "metric"), "brier")
  expect_equal(attr(importance, "baseline_score"), expected)
  expect_identical(audit$config$metric, "brier")
  expect_equal(audit$performance$score, expected)

  log_loss_explainer <- explainer
  log_loss_explainer$label <- "log-loss model"
  log_loss_explainer$metadata$primary_metric <- "log_loss"
  expect_error(
    audit_explanations(
      list(explainer, log_loss_explainer),
      features = "probability",
      n_repeats = 2,
      seed = 17
    ),
    "same performance metric"
  )
})

test_that("replacement importance data inherits the explainer outcome contract", {
  data <- data.frame(
    probability = c(0.9, 0.7, 0.2, 0.1, 0.3, 0.8),
    outcome = factor(
      c("yes", "yes", "no", "no", "no", "yes"),
      levels = c("no", "yes")
    )
  )
  explainer <- explain_model(
    model = list(),
    data = data,
    y = "outcome",
    task = "binary",
    positive = "yes",
    predict_function = function(model, newdata) newdata$probability
  )
  reversed <- data
  reversed$outcome <- factor(
    as.character(reversed$outcome),
    levels = c("yes", "no")
  )
  importance <- calculate_permutation_importance(
    explainer,
    data = reversed,
    metric = "brier",
    n_repeats = 2,
    seed = 19
  )
  expected <- mean((data$probability - as.numeric(data$outcome == "yes"))^2)
  expect_equal(attr(importance, "baseline_score"), expected)

  unseen <- data
  unseen$outcome <- as.character(unseen$outcome)
  unseen$outcome[[1L]] <- "maybe"
  expect_error(
    calculate_permutation_importance(explainer, data = unseen, n_repeats = 2),
    "classes absent from the explainer contract"
  )

  missing <- data
  missing$outcome[[1L]] <- NA
  expect_error(
    calculate_permutation_importance(explainer, data = missing, n_repeats = 2),
    "missing values"
  )

  regression <- make_regression_fixture()
  regression_explainer <- explain_model(
    regression$model,
    regression$test,
    "y"
  )
  incompatible <- regression$test
  incompatible$y <- factor(incompatible$y)
  expect_error(
    calculate_permutation_importance(
      regression_explainer,
      data = incompatible,
      n_repeats = 2
    ),
    "Regression requires a numeric target"
  )
})

test_that("audits retain model dimensions when comparing one feature", {
  fixture <- make_regression_fixture()
  first <- explain_model(
    fixture$model,
    fixture$test,
    "y",
    label = "first model"
  )
  second <- explain_model(
    fixture$model,
    fixture$test,
    "y",
    label = "second model"
  )

  audit <- audit_explanations(
    list(first, second),
    features = "x1",
    n_repeats = 2,
    seed = 171
  )

  expect_s3_class(audit, "autoxplain_audit")
  expect_equal(dim(audit$explanation_agreement$rank_correlation), c(2L, 2L))
  expect_identical(
    colnames(audit$explanation_agreement$rank_correlation),
    c("first model", "second model")
  )
  expect_identical(
    audit$explanation_agreement$importance_ranges$feature,
    "x1"
  )
})

test_that("multiclass Brier importance uses the unscaled classwise squared sum", {
  data <- data.frame(
    p_a = c(0.8, 0.1, 0.2, 0.6, 0.2, 0.1),
    p_b = c(0.1, 0.7, 0.2, 0.3, 0.6, 0.2),
    p_c = c(0.1, 0.2, 0.6, 0.1, 0.2, 0.7),
    outcome = factor(c("a", "b", "c", "a", "b", "c"), levels = c("a", "b", "c"))
  )
  probability_predictor <- function(model, newdata) {
    probability <- as.matrix(newdata[c("p_a", "p_b", "p_c")])
    colnames(probability) <- c("a", "b", "c")
    probability
  }
  explainer <- explain_model(
    model = list(),
    data = data,
    y = "outcome",
    task = "multiclass",
    predict_function = probability_predictor
  )
  probability <- probability_predictor(NULL, data)
  one_hot <- diag(3)[as.integer(data$outcome), , drop = FALSE]
  expected <- mean(rowSums((probability - one_hot)^2))
  importance <- calculate_permutation_importance(
    explainer,
    metric = "brier",
    feature_groups = list(probabilities = c("p_a", "p_b", "p_c")),
    n_repeats = 3,
    seed = 18
  )

  expect_identical(attr(importance, "metric"), "brier")
  expect_equal(attr(importance, "baseline_score"), expected)
  expect_gte(attr(importance, "baseline_score"), 0)
  expect_lte(attr(importance, "baseline_score"), 2)
})

test_that("importance validates groups, metrics, and counts", {
  fixture <- make_regression_fixture()
  explainer <- explain_model(fixture$model, fixture$test, "y")
  expect_error(calculate_permutation_importance(explainer, n_repeats = 0), "whole number")
  expect_error(calculate_permutation_importance(explainer, metric = "auc"), "Regression")
  expect_error(calculate_permutation_importance(explainer, metric = "brier"), "Regression")
  expect_error(calculate_permutation_importance(explainer, features = "missing"), "Unknown")
  expect_error(
    calculate_permutation_importance(explainer, feature_groups = list(group = "missing")),
    "known feature"
  )

  binary <- make_binary_fixture()
  binary_explainer <- explain_model(binary$model, binary$test, "y")
  expect_error(
    calculate_permutation_importance(binary_explainer, metric = "rmse"),
    "Classification supports"
  )
})
