test_that("bounded shuffling matches the recorded sample and preserves full scoring", {
  set.seed(441)
  data <- data.frame(x = rnorm(150), z = rnorm(150))
  data$y <- 3 * data$x + data$z + rnorm(150)
  model <- lm(y ~ x + z, data)
  explainer <- explain_model(model, data, "y")
  rng <- .Random.seed
  sampled <- calculate_permutation_importance(explainer,
    max_rows = 37,
    n_repeats = 4, seed = 19, sample_seed = 71
  )
  expect_identical(.Random.seed, rng)
  sampling <- attr(sampled, "sampling")
  expect_length(sampling$row_indices, 37)
  expect_identical(anyDuplicated(sampling$row_indices), 0L)
  manual <- calculate_permutation_importance(model,
    data = data[sampling$row_indices, ], target_column = "y", n_repeats = 4, seed = 19
  )
  expect_equal(attr(sampled, "repeat_scores"), attr(manual, "repeat_scores"), tolerance = 0)
  expect_equal(attr(sampled, "baseline_score"), sqrt(mean(residuals(model)[sampling$row_indices]^2)))
  expect_equal(attr(sampled, "full_baseline_score"), sqrt(mean(residuals(model)^2)))
  expect_identical(attr(sampled, "explainer_fingerprint"), explainer$provenance$fingerprint)
  complete <- calculate_permutation_importance(explainer, n_repeats = 2, seed = 8)
  uncapped <- calculate_permutation_importance(explainer, max_rows = 1000, n_repeats = 2, seed = 8)
  expect_identical(complete, uncapped)
})

test_that("sampled audits compare models using all evaluation outcomes", {
  set.seed(16)
  data <- data.frame(x = rnorm(90), z = rnorm(90))
  data$y <- 2 * data$x + data$z + rnorm(90)
  models <- list(lm(y ~ x + z, data), lm(y ~ x, data))
  explainers <- lapply(models, explain_model, data = data, y = "y")
  names(explainers) <- c("both", "one")
  audit <- audit_explanations(explainers, max_rows = 21, n_repeats = 3, seed = 72)
  expected <- vapply(models, function(model) sqrt(mean(residuals(model)^2)), numeric(1))
  expect_equal(audit$performance$score, unname(expected))
  samples <- lapply(audit$importance_objects, function(x) attr(x, "sampling")$row_indices)
  expect_identical(samples[[1]], samples[[2]])
  expect_equal(audit$diagnostic_status$permutation$evidence$rows, 21)
  expect_equal(audit$diagnostic_status$evaluation$evidence$rows, 90)
  expect_match(audit$summary$scope_note, "21 sampled evaluation rows out of 90")
})

test_that("bounded effects retain original row identities and a full reference identity", {
  set.seed(331)
  data <- data.frame(x = rnorm(100), z = rnorm(100))
  data$y <- data$x^2 + data$z + rnorm(100)
  model <- lm(y ~ I(x^2) + z, data)
  explainer <- explain_model(model, data, "y")
  for (method in c("ale", "pdp")) {
    effect <- explain_effect(explainer,
      feature = "x", method = method,
      max_rows = 29, sample_size = NULL, n_points = 5, seed = 18
    )
    indices <- attr(effect, "sampling")$row_indices
    manual <- explain_effect(model,
      data = data[indices, c("x", "z")], feature = "x",
      method = method, n_points = 5, sample_size = NULL, seed = 18
    )
    expect_equal(as.matrix(effect), as.matrix(manual))
    expect_setequal(attr(effect, "reference_rows"), indices)
    expect_identical(attr(effect, "explainer_fingerprint"), explainer$provenance$fingerprint)
    expect_equal(attr(effect, "n_reference"), 29)
  }
})

test_that("a rare class absent from explanation rows is disclosed", {
  data <- data.frame(x = seq_len(100), event = factor(c(rep("no", 99), "yes"), c("no", "yes")))
  model <- glm(event ~ 1, data, family = binomial())
  explainer <- explain_model(model, data, "event")
  importance <- calculate_permutation_importance(explainer, max_rows = 2, n_repeats = 2, seed = 2)
  sampling <- attr(importance, "sampling")
  expect_identical(sampling$missing_classes, "yes")
  expect_match(AutoXplainR:::explanation_sampling_note(sampling), "No sampled observations belong to: yes")
  expect_equal(attr(importance, "full_baseline_score"), -mean(log(c(rep(.99, 99), .01))))
  auc <- audit_explanations(explainer, metric = "auc", max_rows = 2, n_repeats = 2, seed = 2)
  expect_equal(auc$performance$score, .5)
  expect_identical(auc$diagnostic_status$permutation$status, "unavailable")
  expect_match(auc$importance$unavailable_reason, "both outcome classes")
  expect_true("importance_unavailable" %in% auc$findings$code)
})

test_that("PDP row caps make one final uniform draw and count support separately", {
  data <- data.frame(x = 1:8, z = (1:8)^2, y = 1:8 + (1:8)^2)
  model <- lm(y ~ x + z, data)
  for (seed in 1:30) {
    effect <- explain_effect(model,
      data = data[c("x", "z")], feature = "x",
      method = "pdp", max_rows = 4, sample_size = 2, n_points = 3, seed = seed
    )
    expected <- withr::with_seed(seed, sample.int(8, 2))
    expect_identical(attr(effect, "reference_rows"), expected)
    expect_equal(attr(effect, "n_reference"), 2)
    expect_equal(attr(effect, "n_support"), 4)
  }
})

test_that("missing full-evaluation classes do not get misleading cap advice", {
  sampling <- list(
    sampled = TRUE, rows_used = 2L, rows_available = 100L,
    missed_classes = character(), missing_classes = "yes", absent_evaluation_classes = "yes"
  )
  note <- AutoXplainR:::explanation_sampling_note(sampling)
  expect_match(note, "requires additional evaluation observations")
  expect_false(grepl("Increase the explanation row limit", note, fixed = TRUE))
})

test_that("a supplied audit does not bypass the budget for newly rendered curves", {
  set.seed(85)
  data <- data.frame(x = rnorm(50), z = rnorm(50))
  data$y <- data$x + data$z + rnorm(50)
  result <- evaluate_models(list(linear = lm(y ~ x + z, data)), data, "y")
  audit <- audit_explanations(as_explainers(result), max_rows = 5, n_repeats = 2)
  original <- AutoXplainR::explain_effect
  observed <- list()
  local_mocked_bindings(explain_effect = function(...) {
    effect <- original(...)
    observed[[length(observed) + 1L]] <<- attr(effect, "sampling")
    effect
  }, .package = "AutoXplainR")
  path <- tempfile(fileext = ".html")
  on.exit(unlink(path))
  render_model_report(result, path,
    audit = audit, explanation_rows = 3L,
    uncertainty = FALSE, report_data = "none"
  )
  expect_true(length(observed) > 0L)
  expect_true(all(vapply(observed, function(x) x$rows_used == 3L && x$rows_available == 50L, logical(1))))
  observed <- list()
  render_model_report(result, path,
    audit = audit, explanation_rows = NULL,
    uncertainty = FALSE, report_data = "none"
  )
  expect_true(length(observed) > 0L)
  expect_true(all(vapply(observed, function(x) x$rows_used == 50L && !x$sampled, logical(1))))
  result$explanations <- AutoXplainR:::prepare_model_report_data(result,
    top_features = 2, n_repeats = 2, explanation_rows = NULL
  )
  retained <- result$explanations$effects
  observed <- list()
  render_model_report(result, path,
    audit = audit, explanation_rows = 3L,
    uncertainty = FALSE, report_data = "none"
  )
  expect_true(length(observed) > 0L)
  expect_true(all(vapply(observed, function(x) x$rows_used == 3L, logical(1))))
  expect_identical(result$explanations$effects, retained)
  observed <- list()
  render_model_report(result, path,
    audit = audit, effects = retained, explanation_rows = 3L,
    uncertainty = FALSE, report_data = "none"
  )
  expect_length(observed, 0L)
})
