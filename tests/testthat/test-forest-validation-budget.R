forest_budget_fixture <- function(task = "regression") {
  with_preserved_seed(281L, {
    data <- data.frame(x = runif(120), z = rnorm(120))
    data$y <- switch(task,
      regression = sin(5 * data$x) + data$z,
      binary = factor(rep(c("zebra", "ant"), 60L), levels = c("zebra", "ant")),
      multiclass = factor(rep(c("zebra", "ant", "mole"), 40L), levels = c("zebra", "ant", "mole"))
    )
    data
  })
}

forest_budget_plan <- function(task = "regression", search = "adaptive", automatic = TRUE, predictors = 20L) {
  classes <- if (task == "regression") 1L else if (task == "multiclass") 3L else 2L
  grids <- if (identical(search, "adaptive") && isTRUE(automatic)) {
    adaptive_parameter_grids("forest", 50000L, predictors, task, classes, 913L, 1L)
  } else {
    NULL
  }
  plan <- local_tuning_plan(
    1L, 50000L, predictors, task, classes,
    learners = "forest", seed = 913L, custom_grids = grids
  )
  apply_forest_validation_budget(plan, forest_validation_budget_policy(50000L, predictors, search, automatic))
}

test_that("only the heavy automatic tier uses 128 CV trees and 256 final trees", {
  cases <- list(
    c(49999L, 20L, 500L, 500L), c(50000L, 20L, 256L, 500L),
    c(49999L, 80L, 256L, 500L), c(50000L, 80L, 128L, 256L),
    c(463715L, 90L, 128L, 256L), c(464810L, 54L, 128L, 256L)
  )
  for (case in cases) {
    policy <- forest_validation_budget_policy(case[[1L]], case[[2L]], "adaptive")
    expect_identical(policy$validation_num_trees, case[[3L]])
    expect_identical(policy$validation_budget_policy_id, "forest-tree-budget-v3")
    expect_identical(policy$validation_budget_work, as.double(case[[1L]]) * case[[2L]])
    expect_identical(policy$final_num_trees, case[[4L]])
    expect_identical(policy$final_budget_active, case[[4L]] == 256L)
    expect_identical(policy$screening_num_trees, 128L)
  }
  for (search in c("grid", "adaptive")) {
    policy <- forest_validation_budget_policy(50000L, 80L, search, automatic = FALSE)
    expect_identical(policy$validation_num_trees, 500L)
    expect_identical(policy$final_num_trees, 500L)
    expect_false(policy$validation_budget_active)
    expect_false(policy$final_budget_active)
  }
  policy <- forest_validation_budget_policy(50000L, 80L, "adaptive")
  expect_match(policy$validation_budget_reason, "at least four million")
  expect_match(policy$validation_budget_scope, "CV scores describe 128-tree fits", fixed = TRUE)
  expect_match(policy$validation_budget_scope, "final 256-tree model", fixed = TRUE)
  plan <- apply_forest_validation_budget(data.frame(family = c("forest", "boosting")), policy)
  expect_identical(plan$validation_num_trees, c(128L, NA_integer_))
  expect_identical(plan$forest_final_num_trees, c(256L, NA_integer_))
  expect_identical(plan$forest_validation_budget_policy_id, c("forest-tree-budget-v3", NA_character_))
})

test_that("full-pool metadata plans every adaptive anchor and proposal at 256 trees", {
  skip_if_package_unavailable("ranger")
  # These are metadata-only plans, not full-data fits or predictive evidence.
  for (case in list(c(463715L, 90L), c(464810L, 54L))) {
    task <- if (case[[2L]] == 90L) "regression" else "multiclass"
    classes <- if (task == "regression") 1L else 7L
    budget <- forest_validation_budget_policy(case[[1L]], case[[2L]], "adaptive")
    grids <- adaptive_parameter_grids(
      "forest", case[[1L]], case[[2L]], task, classes, 817L, 12L,
      forest_budget = budget
    )
    plan <- local_tuning_plan(
      12L, case[[1L]], case[[2L]], task, classes,
      learners = "forest", seed = 817L, custom_grids = grids
    )
    expect_identical(vapply(plan$parameters, `[[`, integer(1), "num.trees"), rep(256L, 12L))
    expect_true(all(grepl("trees = 256,", plan$hyperparameters, fixed = TRUE)))
    expect_identical(plan$seed, vapply(plan$parameters, function(parameters) {
      stable_configuration_seed(817L, "forest", parameters)
    }, integer(1)))
    expect_identical(plan$complexity_proxy, vapply(plan$parameters, function(parameters) {
      max(1, forest_learner_complexity(parameters, case[[1L]], case[[2L]], task, classes))
    }, numeric(1)))
  }
})

test_that("heavy CV and final fits match native forests at their effective seeds", {
  skip_if_package_unavailable("ranger")
  # Large-work planning uses no observations. Native fits remain 120-row fixtures.
  for (task in c("regression", "binary", "multiclass")) {
    data <- forest_budget_fixture(task)
    plan <- forest_budget_plan(task, predictors = 80L)
    requested <- plan$parameters[[1L]]
    expect_identical(requested$num.trees, 256L)
    expect_identical(plan$seed, stable_configuration_seed(913L, "forest", requested))
    effective <- requested
    effective$num.trees <- 128L
    effective$mtry <- 2L
    expected_seed <- stable_configuration_seed(913L, "forest", effective)
    fitted <- fit_tuning_configuration(plan, data, "y", task, fit_scope = "resampling_fold")
    reference <- ranger::ranger(
      x = data[c("x", "z")], y = data$y, num.trees = 128L, mtry = 2L,
      min.node.size = requested$min.node.size, sample.fraction = requested$sample.fraction,
      probability = task != "regression", respect.unordered.factors = "order",
      num.threads = 1L, seed = expected_seed, oob.error = FALSE, verbose = FALSE
    )
    expected <- predict(reference, data = data, num.threads = 1L)$predictions
    if (task == "binary") expected <- expected[, "ant"]
    expect_identical(fitted$fit$forest, reference$forest)
    expect_equal(predict(fitted, data), expected, tolerance = 0, ignore_attr = TRUE)
    record <- attr(fitted, "autoxplain_tuning_fit")
    expect_identical(record$requested_parameters$num.trees, 256L)
    expect_identical(record$requested_parameter_key, canonical_tuning_parameter_key(requested))
    expect_identical(record$requested_configuration_seed, plan$seed)
    expect_identical(record$effective_parameters, effective)
    expect_identical(record$effective_parameter_key, canonical_tuning_parameter_key(effective))
    expect_identical(record$fit_seed, expected_seed)
    expect_identical(record$learned$forest_budget$effective_num_trees, 128L)
    expect_identical(record$learned$forest_budget$policy_id, "forest-tree-budget-v3")
    expect_match(record$learned$forest_budget$scope_note, "complete CV uses 128 trees", fixed = TRUE)
    final <- fit_tuning_configuration(plan, data, "y", task)
    effective$num.trees <- 256L
    final_seed <- stable_configuration_seed(913L, "forest", effective)
    final_reference <- ranger::ranger(
      x = data[c("x", "z")], y = data$y, num.trees = 256L, mtry = 2L,
      min.node.size = requested$min.node.size, sample.fraction = requested$sample.fraction,
      probability = task != "regression", respect.unordered.factors = "order",
      num.threads = 1L, seed = final_seed, oob.error = TRUE, verbose = FALSE
    )
    final_prediction <- predict(final_reference, data = data, num.threads = 1L)$predictions
    if (task == "binary") final_prediction <- final_prediction[, "ant"]
    expect_identical(final$fit$forest, final_reference$forest)
    expect_equal(predict(final, data), final_prediction, tolerance = 0, ignore_attr = TRUE)
    expect_equal(final$fit$num.trees, 256L)
    expect_true(final$fit_details$oob_computed)
    expect_identical(final$fit$prediction.error, final_reference$prediction.error)
    final_record <- attr(final, "autoxplain_tuning_fit")
    expect_identical(final_record$scope, "full_training_refit")
    expect_identical(final_record$requested_parameters, requested)
    expect_identical(final_record$effective_parameters, effective)
    expect_identical(final_record$requested_parameter_key, canonical_tuning_parameter_key(requested))
    expect_identical(final_record$effective_parameter_key, canonical_tuning_parameter_key(effective))
    expect_identical(final_record$requested_configuration_seed, plan$seed)
    expect_identical(final_record$fit_seed, final_seed)
    expect_identical(final$fit_details$forest_budget$effective_num_trees, 256L)
    expect_identical(final$fit_details$forest_budget$final_num_trees, 256L)
    expect_identical(final$fit_details$forest_budget$validation_num_trees, 128L)
  }
})

test_that("failed heavy-tier native attempts retain 128-tree evidence", {
  skip_if_package_unavailable("ranger")
  data <- forest_budget_fixture()
  plan <- forest_budget_plan(predictors = 80L)
  fold <- prepare_tuning_fold(data, "y", "regression", rep(1:2, 60L), 1L, FALSE, list())
  observed <- new.env(parent = emptyenv())
  local_mocked_bindings(fit_forest_learner = function(data, target, task, parameters, seed) {
    observed$trees <- parameters$num.trees
    observed$seed <- seed
    stop("injected heavy-tier backend failure")
  })
  recorded <- score_tuning_configuration(plan, fold, "y", "regression", 1L)$score
  expect_match(recorded$error, "injected heavy-tier")
  expect_identical(observed$trees, 128L)
  expect_identical(recorded$requested_parameters[[1L]]$num.trees, 256L)
  expect_identical(recorded$effective_parameters[[1L]]$num.trees, 128L)
  expect_identical(recorded$fit_seed, observed$seed)
  expect_identical(recorded$learned[[1L]]$forest_budget$effective_num_trees, 128L)
  expect_identical(recorded$learned[[1L]]$forest_budget$policy_id, "forest-tree-budget-v3")
  expect_identical(recorded$learned[[1L]]$forest_budget$final_num_trees, 256L)
  expect_identical(recorded$model_fit_attempts, 1L)
  expect_identical(recorded$calibration_fit_attempts, 0L)
})

test_that("failed heavy final fits retain their attempted 256-tree identity", {
  skip_if_package_unavailable("ranger")
  data <- forest_budget_fixture()
  plan <- forest_budget_plan(predictors = 80L)
  observed <- new.env(parent = emptyenv())
  local_mocked_bindings(fit_forest_learner = function(data, target, task, parameters, seed) {
    observed$parameters <- parameters
    observed$seed <- seed
    observed$rows <- nrow(data)
    stop("injected heavy final backend failure")
  })
  failed <- safely_timed_model_fit(function() fit_tuning_configuration(plan, data, "y", "regression"))
  requested <- plan$parameters[[1L]]
  effective <- requested
  effective$mtry <- 2L
  expect_false(failed$ok)
  expect_match(failed$error, "injected heavy final")
  expect_identical(observed$rows, nrow(data))
  expect_identical(observed$parameters$num.trees, 256L)
  expect_identical(failed$fit_spec$requested_parameters, requested)
  expect_identical(failed$fit_spec$effective_parameters, effective)
  expect_identical(failed$fit_spec$requested_parameter_key, canonical_tuning_parameter_key(requested))
  expect_identical(failed$fit_spec$effective_parameter_key, canonical_tuning_parameter_key(effective))
  expect_identical(failed$fit_spec$requested_configuration_seed, plan$seed)
  expect_identical(failed$fit_spec$fit_seed, observed$seed)
  expect_identical(observed$seed, stable_configuration_seed(913L, "forest", effective))
  expect_identical(failed$learned$forest_budget$scope, "full_training_refit")
  expect_identical(failed$learned$forest_budget$effective_num_trees, 256L)
  expect_identical(failed$learned$forest_budget$final_num_trees, 256L)
  expect_identical(failed$learned$forest_budget$validation_num_trees, 128L)
  expect_identical(failed$learned$forest_budget$policy_id, "forest-tree-budget-v3")
  expect_identical(failed$fit_work$model_fit_attempts, 1L)
  expect_identical(failed$fit_work$calibration_fit_attempts, 0L)
})

test_that("forest validation budgets apply only to large automatic adaptive searches", {
  policy <- forest_validation_budget_policy(50000L, 20L, "adaptive")
  expect_true(policy$validation_budget_active)
  expect_identical(policy$screening_num_trees, 128L)
  expect_identical(policy$validation_num_trees, 256L)
  expect_identical(policy$final_num_trees, 500L)
  expect_false(policy$final_budget_active)
  expect_match(policy$validation_budget_scope, "Every requested CV fold")
  expect_match(policy$validation_budget_scope, "do not measure the final 500-tree model exactly")
  for (other in list(
    forest_validation_budget_policy(49999L, 20L, "adaptive"),
    forest_validation_budget_policy(50000L, 20L, "grid"),
    forest_validation_budget_policy(50000L, 80L, "grid"),
    forest_validation_budget_policy(50000L, 20L, "adaptive", automatic = FALSE)
  )) {
    expect_false(other$validation_budget_active)
    expect_false(other$final_budget_active)
    expect_identical(other$final_num_trees, 500L)
    expect_identical(other$validation_num_trees, 500L)
    plan <- data.frame(family = c("regularized", "forest", "boosting"))
    expect_identical(apply_forest_validation_budget(plan, other)$validation_num_trees, rep(NA_integer_, 3L))
  }
  plan <- data.frame(family = c("regularized", "forest", "boosting"))
  expect_identical(apply_forest_validation_budget(plan, policy)$validation_num_trees, c(NA_integer_, 256L, NA_integer_))
})

test_that("bounded CV fits match native 256-tree forests with the effective seed", {
  skip_if_package_unavailable("ranger")
  # The policy is planned for a large input; these native fits use only 120 rows.
  for (task in c("regression", "binary", "multiclass")) {
    data <- forest_budget_fixture(task)
    plan <- forest_budget_plan(task)
    requested <- plan$parameters[[1L]]
    expected <- requested
    expected$num.trees <- 256L
    expected$mtry <- 2L
    expected_seed <- stable_configuration_seed(913L, "forest", expected)
    fitted <- fit_tuning_configuration(plan, data, "y", task, fit_scope = "resampling_fold")
    reference <- ranger::ranger(
      x = data[c("x", "z")], y = data$y, num.trees = 256L, mtry = 2L,
      min.node.size = requested$min.node.size, sample.fraction = requested$sample.fraction,
      probability = task != "regression", respect.unordered.factors = "order",
      num.threads = 1L, seed = expected_seed, oob.error = FALSE, write.forest = TRUE,
      verbose = FALSE
    )
    native_prediction <- predict(reference, data = data)$predictions
    if (task == "binary") native_prediction <- native_prediction[, "ant"]
    expect_equal(predict(fitted, data), native_prediction, tolerance = 0, ignore_attr = TRUE)
    expect_identical(fitted$fit$forest, reference$forest)
    record <- attr(fitted, "autoxplain_tuning_fit")
    expect_equal(fitted$fit$num.trees, 256L)
    expect_identical(record$requested_parameters$num.trees, 500L)
    expect_identical(record$effective_parameters, expected)
    expect_identical(record$effective_parameter_key, canonical_tuning_parameter_key(expected))
    expect_identical(record$fit_seed, expected_seed)
    expect_identical(record$learned$forest_budget$effective_num_trees, 256L)
    expect_identical(record$learned$forest_budget$final_num_trees, 500L)
    expect_identical(record$fit_work$model_fit_attempts, 1L)
    expect_identical(record$fit_work$calibration_fit_attempts, 0L)

    final <- fit_tuning_configuration(plan, data, "y", task)
    full_expected <- expected
    full_expected$num.trees <- 500L
    expect_equal(final$fit$num.trees, 500L)
    expect_identical(attr(final, "autoxplain_tuning_fit")$effective_parameters, full_expected)
    expect_identical(attr(final, "autoxplain_tuning_fit")$fit_seed,
      stable_configuration_seed(913L, "forest", full_expected)
    )
    expect_identical(final$fit_details$forest_budget$effective_num_trees, 500L)
    expect_true(final$fit_details$oob_computed)
  }
})

test_that("screening keeps 128 trees and exact forest searches retain the requested count", {
  skip_if_package_unavailable("ranger")
  data <- forest_budget_fixture()
  plan <- forest_budget_plan(predictors = 80L)
  plan$parameters[[1L]] <- adaptive_screen_parameters(plan$parameters[[1L]], "forest")
  screened <- fit_tuning_configuration(plan, data, "y", "regression", fit_scope = "screening")
  expect_equal(screened$fit$num.trees, 128L)
  expect_identical(attr(screened, "autoxplain_tuning_fit")$effective_parameters$num.trees, 128L)
  expect_identical(screened$fit_details$forest_budget$effective_num_trees, 128L)
  expect_identical(screened$fit_details$forest_budget$final_num_trees, 256L)
  custom <- local_tuning_plan(
    1L, 50000L, 80L, "regression", 1L, learners = "forest", seed = 913L,
    custom_grids = list(forest = list(list(
      num.trees = 73L, mtry = 2L, min.node.size = 3L, sample.fraction = .9, splitrule = "extratrees"
    )))
  )
  custom <- apply_forest_validation_budget(custom)
  budgeted <- local_tuning_plan(
    1L, 50000L, 80L, "regression", 1L, learners = "forest", seed = 913L,
    family_budgets = c(forest = 1L)
  )
  budgeted <- apply_forest_validation_budget(budgeted)
  exact_plans <- list(
    forest_budget_plan(search = "grid", predictors = 80L),
    forest_budget_plan(automatic = FALSE, predictors = 80L), budgeted, custom
  )
  expect_identical(vapply(exact_plans, function(plan) plan$parameters[[1L]]$num.trees, integer(1)),
    c(500L, 500L, 500L, 73L)
  )
  for (plan in exact_plans) {
    for (scope in c("resampling_fold", "full_training_refit")) {
      exact <- fit_tuning_configuration(plan, data, "y", "regression", fit_scope = scope)
      expect_equal(exact$fit$num.trees, plan$parameters[[1L]]$num.trees)
      expect_identical(
        attr(exact, "autoxplain_tuning_fit")$effective_parameters$num.trees, plan$parameters[[1L]]$num.trees
      )
      expect_null(exact$fit_details$forest_budget)
    }
  }
})

test_that("failed bounded forest CV fits retain their actual attempted count and seed", {
  skip_if_package_unavailable("ranger")
  data <- forest_budget_fixture()
  plan <- forest_budget_plan()
  fold <- prepare_tuning_fold(data, "y", "regression", rep(1:2, 60L), 1L, FALSE, list())
  actual <- new.env(parent = emptyenv())
  local_mocked_bindings(fit_forest_learner = function(data, target, task, parameters, seed) {
    actual$parameters <- parameters
    actual$seed <- seed
    stop("injected bounded forest failure")
  })
  recorded <- score_tuning_configuration(plan, fold, "y", "regression", 1L)$score
  expect_match(recorded$error, "injected bounded forest")
  expect_identical(actual$parameters$num.trees, 256L)
  expect_identical(recorded$requested_parameters[[1L]]$num.trees, 500L)
  expect_identical(recorded$effective_parameters[[1L]]$num.trees, actual$parameters$num.trees)
  expect_identical(recorded$fit_seed, actual$seed)
  expect_identical(recorded$learned[[1L]]$forest_budget$effective_num_trees, 256L)
  expect_identical(recorded$model_fit_attempts, 1L)
  expect_identical(recorded$calibration_fit_attempts, 0L)
})

test_that("large-input orchestration keeps every requested fold and training row", {
  skip_if_package_unavailable("ranger")
  for (predictors in c(20L, 80L)) {
    expected_trees <- if (predictors == 80L) 128L else 256L
    final_trees <- if (predictors == 80L) 256L else 500L
    # This is an orchestration test with a stub backend, not a 50,000-row model
    # benchmark. The separate tests above verify actual native ranger fits.
    data <- as.data.frame(matrix(rep(seq_len(50000L), predictors), nrow = 50000L))
    data$y <- sin(seq_len(50000L))
    calls <- new.env(parent = emptyenv())
    calls$records <- list()
    local_mocked_bindings(
      fit_forest_learner = function(data, target, task, parameters, seed) {
        calls$records[[length(calls$records) + 1L]] <- list(
          rows = nrow(data), trees = parameters$num.trees, seed = seed,
          scope = attr(parameters, "autoxplain_fit_scope") %||% "full_training_refit"
        )
        new_autoxplain_fitted_model(
          family = "forest", backend = "ranger",
          fit = structure(list(mean = mean(data[[target]])), class = "ranger"),
          task = task, features = setdiff(names(data), target), parameters = parameters,
          seed = seed, fit_details = list(threads = 1L)
        )
      },
      make_prediction_adapter = function(model, ...) {
        function(newdata) rep(model$fit$mean, nrow(newdata))
      }
    )
    control <- default_resolved_tuning_control("regression", 2L, 5L)
    control$search <- control$search_requested <- "adaptive"
    control$screening_rows <- 100L
    control$retain_oof <- FALSE
    tuning <- with_preserved_seed(48L, tune_supervised_candidates(
      data, "y", "regression", FALSE, list(), 2L, 5L, "best",
      control = control, seed = 48L, learners = "forest"
    ))
    final <- refit_tuned_candidates(tuning, data, "y", "regression")
    scope <- vapply(calls$records, `[[`, character(1), "scope")
    rows <- vapply(calls$records, `[[`, integer(1), "rows")
    trees <- vapply(calls$records, `[[`, integer(1), "trees")
    expect_equal(sum(scope == "resampling_fold"), 5L)
    expect_identical(rows[scope == "resampling_fold"], rep(40000L, 5L))
    expect_identical(trees[scope == "resampling_fold"], rep(expected_trees, 5L))
    expect_identical(rows[scope == "screening"], rep(80L, 2L))
    expect_identical(trees[scope == "screening"], rep(128L, 2L))
    expect_identical(rows[scope == "full_training_refit"], 50000L)
    expect_identical(trees[scope == "full_training_refit"], final_trees)
    expect_equal(tuning$folds_used, 5L)
    expect_identical(tuning$fold_scores$validation_rows, rep(10000L, 5L))
    expect_identical(tuning$input_policy$forest$validation_num_trees, expected_trees)
    expect_identical(tuning$input_policy$forest$final_num_trees, final_trees)
    expect_identical(tuning$input_policy$forest$final_budget_active, predictors == 80L)
    expect_true(all(vapply(tuning$plan$parameters, `[[`, integer(1), "num.trees") == final_trees))
    expect_identical(tuning$plan$seed, vapply(tuning$plan$parameters, function(parameters) {
      stable_configuration_seed(48L, "forest", parameters)
    }, integer(1)))
    attempt <- final$tuning$refit$attempts[1L, , drop = FALSE]
    final_model <- final$fits$main_model$model
    record <- attr(final_model, "autoxplain_tuning_fit")
    expect_identical(attempt$requested_parameters[[1L]]$num.trees, final_trees)
    expect_identical(attempt$effective_parameters[[1L]]$num.trees, final_trees)
    expect_identical(attempt$requested_parameters[[1L]], record$requested_parameters)
    expect_identical(attempt$effective_parameters[[1L]], record$effective_parameters)
    expect_identical(attempt$requested_parameter_key, record$requested_parameter_key)
    expect_identical(attempt$effective_parameter_key, record$effective_parameter_key)
    expect_identical(attempt$requested_configuration_seed, record$requested_configuration_seed)
    expect_identical(attempt$fit_seed, stable_configuration_seed(48L, "forest", record$effective_parameters))
    expect_identical(attempt$fit_seed, record$fit_seed)
    expect_identical(attempt$learned[[1L]]$forest_budget$final_num_trees, final_trees)
    expect_identical(final_model$fit_details$forest_budget$final_num_trees, final_trees)
  }
})
