search_test_data <- function(n = 300L) {
  with_preserved_seed(2901L, {
    data <- data.frame(x = stats::runif(n), z = stats::runif(n))
    data$y <- sin(7 * data$x) + data$z^2 + stats::rnorm(n, sd = .1)
    rownames(data) <- paste0("case-", seq_len(n))
    data
  })
}

test_that("adaptive search preserves complete CV and actual stopping evidence", {
  skip_if_not_installed("ranger", minimum_version = "0.18.0")
  skip_if_not_installed("xgboost", minimum_version = "3.2.1.1")
  calls <- new.env(parent = emptyenv())
  calls$fits <- 0L
  original_boosting <- fit_boosting_core
  original_forest <- fit_forest_learner
  local_mocked_bindings(
    fit_boosting_core = function(...) {
      calls$fits <- calls$fits + 1L
      original_boosting(...)
    },
    fit_forest_learner = function(...) {
      calls$fits <- calls$fits + 1L
      original_forest(...)
    }
  )
  data <- search_test_data()
  result <- autoxplain(
    data, "y", learners = c("forest", "boosting"),
    max_models = 6L, nfolds = 2L, tuning_rule = "best", explain = FALSE
  )
  tuning <- result$tuning
  complete <- tuning$candidates$configuration_id[tuning$candidates$status == "ok"]
  excluded <- tuning$candidates$configuration_id[tuning$candidates$status == "screened_out"]
  expect_length(complete, 2L)
  expect_length(excluded, 4L)
  expect_setequal(tuning$fold_scores$configuration_id, complete)
  expect_false(any(tuning$out_of_fold_predictions$configuration_id %in% excluded))
  expect_equal(
    as.integer(table(tuning$out_of_fold_predictions$configuration_id)),
    rep(nrow(result$training_data), length(complete))
  )
  expect_setequal(tuning$screening$scores$configuration_id, tuning$plan$configuration_id)
  expect_equal(tuning$resources$total_backend_fit_attempts, calls$fits + 1L)
  boosted_ids <- tuning$candidates$configuration_id[
    tuning$candidates$family == "boosting" & tuning$candidates$status == "ok"
  ]
  boosting <- tuning$fold_scores[tuning$fold_scores$configuration_id %in% boosted_ids, ]
  rounds <- vapply(boosting$effective_parameters, `[[`, integer(1), "nrounds")
  fit <- result$models[[which(vapply(result$models, function(model) {
    inherits(model, "autoxplain_fitted_model") && model$backend == "xgboost"
  }, logical(1)))]]
  expect_equal(fit$parameters$nrounds, ceiling(stats::median(rounds)))
  expect_equal(xgboost::xgb.get.num.boosted.rounds(fit$fit), fit$parameters$nrounds)
  expect_true(all(vapply(boosting$learned, function(x) {
    x$round_selection$training_rows + x$round_selection$validation_rows <= max(boosting$training_rows)
  }, logical(1))))
  aggregate <- tuning_evidence(result)
  expect_null(aggregate$screening$partition[["training_row", exact = TRUE]])
  expect_null(aggregate$screening$partition[["validation_sampling_weight", exact = TRUE]])
  expect_true(all(lengths(aggregate$candidates$parameters) > 0L))
  expect_equal(sum(aggregate$families$failed), 0L)
  expect_equal(sum(aggregate$families$screened_out), 4L)
})

test_that("final evaluation labels never choose screening settings or boosting rounds", {
  skip_if_not_installed("xgboost", minimum_version = "3.2.1.1")
  training <- search_test_data()
  evaluation <- transform(search_test_data(40L), y = y + 1)
  rownames(evaluation) <- paste0("eval-", seq_len(nrow(evaluation)))
  fit <- function(evaluation) {
    autoxplain(
      training, "y", test_data = evaluation, learners = "boosting", max_models = 3L, nfolds = 2L,
      tuning_rule = "best", explain = FALSE
    )
  }
  first <- fit(evaluation)
  changed <- fit(transform(evaluation, y = y * -1000))
  expect_identical(first$tuning$screening$promotion, changed$tuning$screening$promotion)
  expect_identical(first$tuning$fold_scores$score, changed$tuning$fold_scores$score)
  expect_identical(first$tuning$fold_scores$effective_parameters, changed$tuning$fold_scores$effective_parameters)
  expect_identical(predict(first, evaluation), predict(changed, evaluation))
  expect_false(identical(first$evaluation$metrics, changed$evaluation$metrics))
})

test_that("a scheduling deadline preserves one complete candidate and labels skipped families", {
  local_mocked_bindings(search_deadline_reached = function(...) TRUE)
  result <- autoxplain(
    search_test_data(90L), "y", learners = c("linear", "tree"),
    max_models = 5L, nfolds = 3L, explain = FALSE,
    tuning_control = tuning_control(time_limit = .001)
  )
  expect_identical(unique(result$tuning$fold_scores$configuration_id), "linear_01")
  expect_identical(nrow(result$tuning$fold_scores), 3L)
  expect_true(all(result$tuning$candidates$status[-1L] == "not_validated_time_limit"))
  expect_identical(result$tuning$families_resampling_failed, character())
  expect_identical(result$tuning$families_not_validated, "tree")
  expect_identical(result$tuning$refit$families_not_retained, "tree")
  expect_true(all(is.finite(predict(result, result$test_data))))
})

test_that("failed boosting calibration does not destroy a usable forest search", {
  skip_if_not_installed("ranger", minimum_version = "0.18.0")
  skip_if_not_installed("xgboost", minimum_version = "3.2.1.1")
  local_mocked_bindings(prepare_boosting_calibration = function(...) stop("inner recipe unavailable"))
  result <- autoxplain(
    search_test_data(), "y", learners = c("forest", "boosting"),
    max_models = 4L, nfolds = 2L, explain = FALSE
  )
  expect_identical(result$tuning$families_resampling_failed, "boosting")
  expect_true(all(result$tuning$candidates$status[result$tuning$candidates$family == "boosting"] == "screening_failed"))
  expect_true(any(grepl("inner recipe unavailable", result$tuning$screening$scores$error)))
  expect_equal(result$tuning$resources$calibration_fit_attempts, 0L)
  expect_true(all(is.finite(predict(result, result$test_data))))
  expect_error(
    autoxplain(
      search_test_data(), "y", learners = c("forest", "boosting"), max_models = 4L, nfolds = 2L, explain = FALSE,
      tuning_control = tuning_control(failure_policy = "stop")
    ), "inner recipe unavailable"
  )
})

test_that("native-only controls preserve exact grids and validate their bounds", {
  expect_error(tuning_control(threads = 0L), "threads")
  expect_error(tuning_control(time_limit = Inf), "time_limit")
  expect_error(tuning_control(early_stopping = NA), "early_stopping")
  expect_error(tuning_control(screening_rows = 2L), "screening_rows")
  expect_error(
    autoxplain(
      search_test_data(40L), "y", learners = "tree", max_models = 2L,
      tuning_control = tuning_control(search = "adaptive"), explain = FALSE
    ), "supports regularized"
  )
  expect_identical(default_local_tuning_budget("tabular"), 18L)
  expect_identical(portfolio_learner_families("tabular", "multiclass"), c("regularized", "forest", "boosting"))
})

test_that("a fully failed adaptive screen exposes its actual errors and attempted settings", {
  skip_if_not_installed("xgboost", minimum_version = "3.2.1.1")
  withr::local_seed(715L)
  data <- data.frame(x = runif(240L, -3, 3), z = rnorm(240L))
  data$y <- ifelse(abs(data$x) < .3, 10, 0)
  for (early_stopping in c(TRUE, FALSE)) {
    failure <- tryCatch(autoxplain(
      data, "y", task = "regression", learners = "boosting", max_models = 2L,
      nfolds = 2L, seed = 21L, verbosity = "quiet", explain = FALSE,
      tuning_control = tuning_control(
        search = "adaptive", metric = "rmsle", patience = 3L, screening_rows = 120L,
        threads = 1L, early_stopping = early_stopping
      )
    ), error = identity)
    expect_s3_class(failure, "autoxplain_screening_error")
    expect_match(conditionMessage(failure), "Screening failed for all 2 attempted settings", fixed = TRUE)
    expect_match(conditionMessage(failure), "RMSLE requires nonnegative predictions", fixed = TRUE)
    expect_identical(failure$screening$scores$configuration_id, c("boosting_01", "boosting_02"))
    expect_true(all(grepl("negative predictions are not clipped", failure$screening$scores$error, fixed = TRUE)))
    expect_true(all(!is.finite(failure$screening$scores$score)))
    expect_true(all(failure$plan$search_status == "screening_failed"))
    expect_false(any(failure$screening$promotion$promoted))
  }
})
