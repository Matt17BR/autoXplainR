test_that("blocked permutations preserve singleton strata and donor bijection", {
  strata <- factor(c("a", "b", "b", "c", "d", "d", "d", "e"))
  for (seed in c(1L, 4L, 19L)) {
    permutation <- withr::with_seed(seed, AutoXplainR:::stratified_permutation(8, strata))
    expect_identical(sort(permutation), seq_len(8))
    expect_identical(strata[permutation], strata)
    expect_identical(permutation[c(1, 4, 8)], c(1L, 4L, 8L))
  }
  data <- data.frame(x = 1:20, group = factor(1:20), y = 1:20)
  explainer <- explain_model(list(), data, "y", task = "regression",
                             predict_function = function(newdata) newdata$x)
  expect_warning(importance <- calculate_permutation_importance(
    explainer, features = "x", within = "group", n_repeats = 20, seed = 4
  ), NA)
  expect_equal(importance$importance, 0)
  expect_true(all(attr(importance, "repeat_scores") == 0))
})

test_that("ALE recovers analytic linear effects on irregular and tied inputs", {
  inputs <- list(
    c(seq(0, 1, length.out = 51), seq(10, 100, length.out = 50)),
    rep(0:4, each = 10),
    rep(c(0, 1), c(91, 9)),
    seq(0, 10, length.out = 120)^3
  )
  for (x in inputs) {
    data <- data.frame(x = x, z = sin(seq_along(x)))
    data$y <- 3 * data$x - 2 * data$z
    explainer <- explain_model(list(), data, "y", task = "regression",
                               predict_function = function(newdata) 3 * newdata$x - 2 * newdata$z)
    effect <- explain_effect(explainer, feature = "x", n_points = 20)
    expect_equal(effect$accumulated_effect, 3 * (effect$x - mean(x)), tolerance = 1e-10)
    expect_equal(diff(effect$accumulated_effect) / diff(effect$x),
                 rep(3, nrow(effect) - 1L), tolerance = 1e-10)
    expect_equal(range(effect$x), range(x))
    expect_true(all(attr(effect, "bin_counts") > 0L))
    expect_equal(sum(attr(effect, "bin_counts")), length(x))
    expect_true(is.na(effect$n[[1L]]))
    expect_match(attr(effect, "estimator_note"), "type 1", fixed = TRUE)
  }
})

test_that("ALE boundary effects match a nonlinear additive oracle and empirical centering", {
  data <- data.frame(x = rep(c(-3, -1, 0, 2, 10), c(5, 11, 7, 17, 9)))
  data$z <- cos(seq_len(nrow(data)))
  data$y <- data$x^2 + data$z
  explainer <- explain_model(list(), data, "y", task = "regression",
                             predict_function = function(newdata) newdata$x^2 + newdata$z)
  effect <- explain_effect(explainer, feature = "x", n_points = 20)
  # This is the closed-form additive effect, not a reconstruction of ALE bins.
  exact_boundary_values <- effect$x^2
  empirical_center <- mean(stats::approx(effect$x, exact_boundary_values,
                                         xout = data$x)$y)
  expect_equal(effect$accumulated_effect, exact_boundary_values - empirical_center,
               tolerance = 1e-12)
  expect_equal(mean(stats::approx(effect$x, effect$accumulated_effect,
                                  xout = data$x)$y), 0, tolerance = 1e-12)
  expect_equal(effect$std_error, rep(0, nrow(effect)), tolerance = 1e-12)
})

test_that("ALE uncertainty uses the same boundary centering as the estimate", {
  data <- data.frame(x = c(0, 0.5, 1, 2, 3, 4), z = c(1, 2, 3, 2, 4, 6))
  data$y <- data$x * data$z
  explainer <- explain_model(list(), data, "y", task = "regression",
                             predict_function = function(newdata) newdata$x * newdata$z)
  effect <- explain_effect(explainer, feature = "x", n_points = 2)
  # Boundaries are 0,1,4. Local changes are z and 3*z, each on three rows.
  # Reference interpolation yields centering coefficients 3/4 and 1/3.
  local_se <- c(stats::sd(c(1, 2, 3)), stats::sd(c(6, 12, 18))) / sqrt(3)
  expected_coefficients <- rbind(c(-3 / 4, -1 / 3), c(1 / 4, -1 / 3), c(1 / 4, 2 / 3))
  expected_se <- sqrt(rowSums(sweep(expected_coefficients, 2, local_se, "*")^2))
  expect_equal(effect$x, c(0, 1, 4))
  expect_equal(effect$std_error, expected_se, tolerance = 1e-12)
})

test_that("audits reject incompatible evaluation rows outcomes and events", {
  data <- data.frame(x = 1:50, y = 2 * (1:50))
  predict_x <- function(newdata) 2 * newdata$x
  first <- explain_model(list(), data, "y", task = "regression", label = "first",
                         predict_function = predict_x)
  same <- explain_model(list(), data, "y", task = "regression", label = "same",
                        predict_function = predict_x)
  audit <- audit_explanations(list(first, same), n_repeats = 4)
  expect_equal(audit$prediction_agreement$score, 1)
  expect_equal(audit$prediction_agreement$ambiguity, 0)
  reversed <- explain_model(list(), data[50:1, ], "y", task = "regression",
                            label = "reversed", predict_function = predict_x)
  expect_error(audit_explanations(list(first, reversed)), "same ordered evaluation")
  altered <- same
  altered$y[[1L]] <- 100
  expect_error(audit_explanations(list(first, altered)), "same ordered evaluation")
  binary <- data.frame(x = seq(-2, 2, length.out = 50))
  binary$y <- factor(rep(c("no", "yes"), 25))
  model <- stats::glm(y ~ x, data = binary, family = stats::binomial())
  yes <- explain_model(model, binary, "y", positive = "yes", label = "yes")
  no <- explain_model(model, binary, "y", positive = "no", label = "no")
  expect_error(audit_explanations(list(yes, no)), "positive event")
})

test_that("audit association context is invariant to the reported feature subset", {
  data <- data.frame(x = 1:100, proxy = 1:100, y = 1:100)
  explainer <- explain_model(list(), data, "y", task = "regression",
                             predict_function = function(newdata) newdata$x)
  full <- audit_explanations(explainer, n_repeats = 4)
  selected <- audit_explanations(explainer, features = "x", n_repeats = 4)
  expect_equal(selected$dependence, full$dependence[full$dependence$feature == "x", ])
  expect_equal(selected$importance$max_association, 1)
  expect_identical(selected$importance$dependence_status, "association_flagged")
  expect_identical(selected$dependence$associated_feature, "proxy")
  expect_identical(selected$findings$feature[selected$findings$code == "feature_dependence"], "x")
})

test_that("scoped shuffle diagnostics do not turn baseline null effects into warnings", {
  data <- data.frame(x = 1:100, y = 1:100)
  primary <- explain_model(list(), data, "y", task = "regression", label = "primary",
                           predict_function = function(newdata) newdata$x)
  baseline <- explain_model(list(), data, "y", task = "regression", label = "baseline",
                            predict_function = function(newdata) rep(50.5, nrow(newdata)))
  alone <- audit_explanations(primary, n_repeats = 20)
  combined <- audit_explanations(list(primary, baseline), n_repeats = 20)
  selected <- combined$importance[combined$importance$model == "primary", , drop = FALSE]
  rownames(selected) <- NULL
  expect_equal(selected, alone$importance)
  baseline_row <- combined$importance[combined$importance$model == "baseline", ]
  expect_identical(baseline_row$shuffle_status, "no_observed_change")
  expect_false(any(combined$findings$model == "baseline" &
                     combined$findings$severity %in% c("warning", "critical"), na.rm = TRUE))
  expect_false(any(c("grade", "grade_note", "stable_claim_rate") %in% names(combined$summary)))
  expect_false("evidence_grade" %in% names(combined$importance))
  expect_identical(alone$diagnostic_status$comparison$status, "insufficient_evidence")
  expect_match(alone$diagnostic_status$comparison$reason, "Fewer than two")
})

test_that("a small Spearman result explicitly leaves other dependence unassessed", {
  data <- data.frame(x = rep(-50:50, each = 2))
  data$proxy <- data$x^2
  data$y <- data$x^2
  explainer <- explain_model(list(), data, "y", task = "regression",
                             predict_function = function(newdata) newdata$x^2)
  audit <- audit_explanations(explainer, features = "x", n_repeats = 4)
  expect_lt(audit$dependence$max_association, 0.01)
  expect_identical(audit$importance$dependence_status, "limited_screen")
  expect_match(audit$summary$association_scope, "do not establish independence", fixed = TRUE)
  expect_true("association_screen_scope" %in% audit$findings$code)
  pdp <- explain_effect(explainer, feature = "x", method = "pdp", n_points = 3)
  expect_match(attr(pdp, "association_scope"), "nonlinear or joint dependence", fixed = TRUE)
})

test_that("threshold edge metrics match independent confusion-count definitions", {
  patterns <- as.matrix(expand.grid(rep(list(c(FALSE, TRUE)), 4)))
  for (i in seq_len(nrow(patterns))) {
    for (j in seq_len(nrow(patterns))) {
      truth <- as.logical(patterns[i, ])
      predicted <- as.logical(patterns[j, ])
      table <- table(factor(truth, levels = c(FALSE, TRUE)),
                     factor(predicted, levels = c(FALSE, TRUE)))
      tp <- unname(table[2, 2])
      fp <- unname(table[1, 2])
      fn <- unname(table[2, 1])
      result <- AutoXplainR:::threshold_performance(truth, as.numeric(predicted), .5, 1, 1)
      denominator <- 2 * tp + fp + fn
      expect_equal(result$f1, if (denominator > 0) 2 * tp / denominator else NA_real_)
      expected <- if (length(unique(truth)) == 2L) {
        mean(diag(prop.table(table, 1)))
      } else {
        NA_real_
      }
      expect_equal(result$balanced_accuracy, expected)
    }
  }
})

test_that("label-only explainers support accuracy without fabricated probabilities", {
  data <- data.frame(x = rep(c(0, 1), each = 10))
  data$y <- factor(ifelse(data$x == 1, "yes", "no"))
  labels <- function(newdata) {
    factor(ifelse(newdata$x == 1, "yes", "no"), levels = c("no", "yes"))
  }
  first <- explain_model(list(), data, "y", task = "binary", label = "first",
                         predict_function = labels)
  second <- explain_model(list(), data, "y", task = "binary", label = "second",
                          predict_function = labels)
  importance <- calculate_permutation_importance(first, metric = "accuracy", n_repeats = 4)
  expect_equal(attr(importance, "baseline_score"), 1)
  expect_error(calculate_permutation_importance(first, metric = "logloss"), "requires probabilities")
  expect_error(calculate_permutation_importance(first, metric = "brier"), "requires probabilities")
  audit <- audit_explanations(list(first, second), metric = "accuracy", n_repeats = 4)
  expect_equal(audit$prediction_agreement$score, 1)
  expect_equal(audit$prediction_agreement$ambiguity, 0)
})

test_that("effect and audit provenance bind to current evaluation contents", {
  data <- data.frame(x = 1:20, y = 2 * (1:20))
  explainer <- explain_model(list(), data, "y", task = "regression",
                             predict_function = function(newdata) 2 * newdata$x)
  original_id <- AutoXplainR:::current_explainer_fingerprint(explainer)
  effect <- explain_effect(explainer, feature = "x", n_points = 4)
  expect_identical(attr(effect, "explainer_fingerprint"), original_id)
  changed <- explainer$data
  changed$x[[1L]] <- -100
  replacement <- explain_effect(explainer, data = changed, feature = "x", n_points = 4)
  expect_false(identical(attr(replacement, "explainer_fingerprint"), original_id))
  explainer$data <- changed
  audit <- audit_explanations(explainer, n_repeats = 4)
  current_id <- AutoXplainR:::current_explainer_fingerprint(explainer)
  expect_identical(unname(audit$provenance$explainer_fingerprints), current_id)
  expect_false(identical(current_id, original_id))
  expect_identical(attr(audit$importance_objects[[1L]], "explainer_fingerprint"), current_id)
})
