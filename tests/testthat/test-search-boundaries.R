boundary_search_data <- function(n = 240L, seed = 386L) {
  with_preserved_seed(seed, {
    data <- data.frame(x = stats::runif(n), z = stats::runif(n))
    data$y <- sin(9 * data$x) + 2 * data$z^2 + stats::rnorm(n, sd = .2)
    rownames(data) <- paste0("boundary-", seed, "-", seq_len(n))
    data
  })
}

boundary_boosting_control <- function(fold_ids = NULL, early_stopping = TRUE) {
  tuning_control(
    grids = list(boosting = list(
      nrounds = 18L, eta = .2, max_depth = 3L, min_child_weight = 1,
      subsample = .8, colsample_bytree = .8, reg_alpha = 0, reg_lambda = 1,
      encoding = "matrix"
    )),
    fold_ids = fold_ids, early_stopping = early_stopping, patience = 3L,
    threads = 1L, search = "grid"
  )
}

test_that("OOF retention cannot change adaptive AUC fitting or score aggregation", {
  skip_if_not_installed("ranger", minimum_version = "0.18.0")
  training <- boundary_search_data(260L)
  training$y <- factor(ifelse(training$y > stats::median(training$y), "yes", "no"))
  evaluation <- boundary_search_data(50L, seed = 891L)
  evaluation$y <- factor(ifelse(evaluation$y > stats::median(evaluation$y), "yes", "no"))
  run <- function(retain, holdout = evaluation) {
    autoxplain(
      training, "y", test_data = holdout, learners = "forest", max_models = 2L,
      nfolds = 2L, tuning_rule = "best", explain = FALSE, seed = 421L,
      tuning_control = tuning_control(
        metric = "auc", retain_oof = retain,
        screening_rows = 80L, finalists_per_family = 2L
      )
    )
  }
  retained <- run(TRUE)
  omitted <- run(FALSE)
  expect_null(omitted$tuning$out_of_fold_predictions)
  oof <- retained$tuning$out_of_fold_predictions
  expect_equal(nrow(oof), 2L * nrow(training))
  expect_identical(sort(oof$training_row), sort(rep(seq_len(nrow(training)), 2L)))
  expect_true(all(is.na(oof$case_loss)))
  expect_identical(retained$tuning$selected_configuration, omitted$tuning$selected_configuration)
  expect_identical(retained$tuning$screening$promotion, omitted$tuning$screening$promotion)
  expect_identical(retained$tuning$fold_scores$score, omitted$tuning$fold_scores$score)
  expect_identical(predict(retained, evaluation), predict(omitted, evaluation))

  # Independent positive-negative pair comparisons, then row-weighted folds.
  expected <- vapply(split(oof, oof$configuration_id), function(candidate) {
    fold_scores <- vapply(split(candidate, candidate$fold), function(rows) {
      positive <- rows$truth == "yes"
      probability <- rows$probabilities[, "yes"]
      differences <- outer(probability[positive], probability[!positive], "-")
      mean((differences > 0) + .5 * (differences == 0))
    }, numeric(1))
    sum(fold_scores * as.integer(table(candidate$fold))) / nrow(candidate)
  }, numeric(1))
  candidates <- retained$tuning$candidates
  expect_equal(candidates$cv_score, unname(expected[candidates$configuration_id]))
  expect_gt(diff(range(expected)), .001)
  expect_identical(retained$tuning$selected_configuration, names(which.max(expected)))
  expect_identical(retained$tuning$direction, "maximize")
  expect_identical(retained$evaluation$primary_metric, "roc_auc")

  changed_data <- evaluation
  changed_data$x <- 1 - changed_data$x
  changed_data$y <- factor(ifelse(changed_data$y == "yes", "no", "yes"))
  changed <- run(FALSE, changed_data)
  # Score a fixed input through both independently fitted results. A separate
  # changed-input assertion makes this fixture sensitive to prediction changes.
  original_prediction <- predict(retained, evaluation)
  expect_length(original_prediction, nrow(evaluation))
  expect_identical(original_prediction, predict(changed, evaluation))
  expect_gt(max(abs(original_prediction - predict(changed, changed_data))), .1)
  expect_false(identical(retained$evaluation$metrics, changed$evaluation$metrics))
})

test_that("screening scores restore rare-class prevalence after sampling", {
  data <- data.frame(x = seq_len(1000L), y = factor(rep("common", 1000L), c("common", "rare")))
  data$y[c(1L, 251L, 501L, 751L)] <- "rare"
  rownames(data) <- paste0("rare-source-", seq_len(nrow(data)))
  assignment <- list(id = rep(1:4, each = 250L), labels = as.character(1:4), folds = 4L)
  control <- list(
    screening_rows = 100L, early_stopping = FALSE, metric = "log_loss",
    finalists_per_family = 1L, time_limit = NULL, failure_policy = "continue"
  )
  plan <- local_tuning_plan(1L, 1000L, 1L, "binary", 2L, seed = 43L, learners = "forest")
  # A real intercept fit makes the oracle analytic. The native engine itself
  # is irrelevant to whether orchestration applies the sampling weights.
  local_mocked_bindings(fit_tuning_configuration = function(configuration, data, target, task, ...) {
    stats::glm(y ~ 1, data = data, family = stats::binomial())
  })
  screened <- run_adaptive_screening(
    plan, data, "y", "binary", assignment, control,
    TRUE, list(missing_value_strategy = "impute"), 83L, proc.time()[["elapsed"]]
  )
  partition <- screened$evidence$partition
  probability <- mean(data$y[partition$training_row] == "rare")
  original_prevalence <- mean(data$y[assignment$id == partition$fold] == "rare")
  sampled_prevalence <- mean(data$y[partition$validation_row] == "rare")
  expected <- -original_prevalence * log(probability) - (1 - original_prevalence) * log1p(-probability)
  unweighted <- -sampled_prevalence * log(probability) - (1 - sampled_prevalence) * log1p(-probability)
  expect_equal(partition$validation_rows, 20L)
  expect_equal(original_prevalence, 1 / 250)
  expect_equal(sampled_prevalence, 1 / 20)
  expect_gt(abs(expected - unweighted), .1)
  expect_equal(screened$evidence$scores$score, expected, tolerance = 1e-9)
  expect_identical(screened$plan$search_status, "scheduled")
})

test_that("grouped boosting fits use disjoint actual rows at every boundary", {
  skip_if_not_installed("xgboost", minimum_version = "3.2.1.1")
  data <- boundary_search_data(300L)
  data$site <- rep(seq_len(30L), each = 10L)
  calls <- new.env(parent = emptyenv())
  calls$fits <- list()
  original <- fit_boosting_core
  local_mocked_bindings(fit_boosting_core = function(data, target, task, parameters, seed,
                                                     threads = 1L, validation = NULL, ...) {
    calls$fits[[length(calls$fits) + 1L]] <- list(
      training = rownames(data), validation = rownames(validation), seed = seed,
      nrounds = parameters$nrounds
    )
    original(data, target, task, parameters, seed, threads, validation, ...)
  })
  result <- autoxplain(
    data, "y", learners = "boosting", max_models = 1L, nfolds = 3L,
    validation = validation_split(group = "site"), explain = FALSE, seed = 582L,
    tuning_control = boundary_boosting_control()
  )
  trace <- calls$fits
  calibration <- trace[vapply(trace, function(fit) length(fit$validation) > 0L, logical(1))]
  expect_length(calibration, 3L)
  expect_length(trace, 7L)
  assignment <- result$tuning$fold_assignment
  evaluation_rows <- rownames(result$test_data)
  expect_gt(length(evaluation_rows), 0L)
  expect_gt(nrow(result$training_data), 100L)
  group_for <- function(rows) data$site[match(rows, rownames(data))]
  for (fit in calibration) {
    expect_gt(length(fit$training), 20L)
    expect_gt(length(fit$validation), 0L)
    expect_length(intersect(fit$training, fit$validation), 0L)
    expect_length(intersect(group_for(fit$training), group_for(fit$validation)), 0L)
    fit_rows <- c(fit$training, fit$validation)
    expect_length(intersect(fit_rows, evaluation_rows), 0L)
    folds <- unique(assignment$fold[match(fit_rows, assignment$source_row)])
    expect_length(folds, 2L)
    expect_setequal(fit_rows, assignment$source_row[assignment$fold %in% folds])
  }
  final <- trace[[length(trace)]]
  expect_setequal(final$training, rownames(result$training_data))
  expect_length(final$validation, 0L)
  scores <- result$tuning$fold_scores
  expect_equal(nrow(scores), 3L)
  rounds <- vapply(scores$effective_parameters, `[[`, integer(1), "nrounds")
  expect_equal(final$nrounds, ceiling(stats::median(rounds)))
  expect_identical(final$seed, result$tuning$refit$attempts$fit_seed[[1L]])
  expect_equal(result$tuning$resources$calibration_fit_attempts, 3L)
  expect_equal(result$tuning$resources$model_fit_attempts, 3L)
  expect_equal(result$tuning$resources$total_backend_fit_attempts, length(trace) + 1L)

  replay_environment <- new.env(parent = environment())
  eval(parse(text = selection_grid_code(result)), replay_environment)
  replay <- autoxplain(
    data, "y", learners = "boosting", max_models = NULL, nfolds = 3L,
    validation = validation_split(group = "site"), explain = FALSE, seed = 582L,
    tuning_control = replay_environment$control
  )
  expect_identical(replay$tuning$fold_assignment, result$tuning$fold_assignment)
  expect_identical(replay$tuning$fold_scores$score, scores$score)
  expect_identical(replay$tuning$fold_scores$effective_parameters, scores$effective_parameters)
  expect_identical(predict(replay, result$test_data), predict(result, result$test_data))
})

test_that("two supplied folds keep stopping disabled inside indivisible training units", {
  skip_if_not_installed("xgboost", minimum_version = "3.2.1.1")
  training <- boundary_search_data(100L)
  evaluation <- boundary_search_data(30L, seed = 823L)
  ids <- rep(c("first unit", "second unit"), each = 50L)
  run <- function(control) {
    autoxplain(
      training, "y", test_data = evaluation,
      learners = "boosting", max_models = 1L, nfolds = 2L, explain = FALSE,
      tuning_rule = "best", seed = 71L, tuning_control = control
    )
  }
  enabled <- run(boundary_boosting_control(ids))
  fixed <- run(boundary_boosting_control(ids, FALSE))
  scores <- enabled$tuning$fold_scores
  expect_equal(nrow(scores), 2L)
  expect_identical(enabled$tuning$resources$calibration_fit_attempts, 0L)
  expect_identical(
    vapply(scores$learned, function(record) record$round_selection$status, character(1)),
    rep("skipped", 2L)
  )
  expect_identical(vapply(scores$effective_parameters, `[[`, integer(1), "nrounds"), rep(18L, 2L))
  expect_identical(scores$fit_seed, fixed$tuning$fold_scores$fit_seed)
  expect_identical(scores$score, fixed$tuning$fold_scores$score)
  expect_identical(predict(enabled, evaluation), predict(fixed, evaluation))

  missing_original <- new.env(parent = environment())
  expect_error(eval(parse(text = selection_grid_code(enabled)), missing_original), "Set original_result")
  replay_environment <- new.env(parent = environment())
  replay_environment$original_result <- enabled
  eval(parse(text = selection_grid_code(enabled)), replay_environment)
  replay <- autoxplain(
    training, "y", test_data = evaluation, learners = "boosting",
    max_models = NULL, nfolds = 2L, explain = FALSE, tuning_rule = "best", seed = 71L,
    tuning_control = replay_environment$control
  )
  expect_identical(replay$tuning$fold_assignment, enabled$tuning$fold_assignment)
  expect_identical(replay$tuning$resources$calibration_fit_attempts, 0L)
  expect_identical(predict(replay, evaluation), predict(enabled, evaluation))
})

test_that("a failed calibration fit is counted without inventing a complete model fit", {
  skip_if_not_installed("ranger", minimum_version = "0.18.0")
  skip_if_not_installed("xgboost", minimum_version = "3.2.1.1")
  calls <- new.env(parent = emptyenv())
  calls$calibration <- 0L
  original <- fit_boosting_core
  local_mocked_bindings(fit_boosting_core = function(..., validation = NULL) {
    if (!is.null(validation)) {
      calls$calibration <- calls$calibration + 1L
      stop("Injected failure after a native calibration fit was requested.")
    }
    original(..., validation = validation)
  })
  result <- autoxplain(
    boundary_search_data(), "y", learners = c("forest", "boosting"),
    max_models = 4L, nfolds = 2L, explain = FALSE,
    tuning_control = tuning_control(search = "adaptive", screening_rows = 100L)
  )
  scores <- result$tuning$screening$scores
  boosting_ids <- result$tuning$plan$configuration_id[result$tuning$plan$family == "boosting"]
  failed <- scores[scores$configuration_id %in% boosting_ids, ]
  expect_equal(nrow(failed), 2L)
  expect_equal(calls$calibration, 2L)
  expect_equal(failed$calibration_fit_attempts, c(1L, 1L))
  expect_equal(failed$model_fit_attempts, c(0L, 0L))
  expect_true(all(is.na(failed$score)))
  expect_equal(result$tuning$resources$calibration_fit_attempts, calls$calibration)
  expect_equal(result$tuning$resources$model_fit_attempts, 4L)
  expect_identical(result$tuning$families_resampling_failed, "boosting")
})

test_that("deadline execution continues past failures until one entire candidate succeeds", {
  original <- fit_tuning_configuration
  local_mocked_bindings(
    search_deadline_reached = function(...) TRUE,
    fit_tuning_configuration = function(configuration, ...) {
      if (configuration$family[[1L]] == "linear") stop("Injected first-family failure")
      original(configuration, ...)
    }
  )
  result <- autoxplain(
    boundary_search_data(90L), "y", learners = c("linear", "tree"),
    max_models = 5L, nfolds = 3L, explain = FALSE,
    tuning_control = tuning_control(time_limit = .001)
  )
  candidates <- result$tuning$candidates
  scores <- result$tuning$fold_scores
  expect_equal(sum(candidates$status == "ok"), 1L)
  expect_equal(sum(candidates$status == "failed"), 1L)
  expect_equal(sum(candidates$status == "not_validated_time_limit"), 3L)
  expect_equal(nrow(scores), 6L)
  expect_identical(as.integer(table(scores$configuration_id)), c(3L, 3L))
  winner <- candidates$configuration_id[candidates$selected]
  expect_length(winner, 1L)
  expect_true(startsWith(winner, "tree_"))
  oof <- result$tuning$out_of_fold_predictions
  expect_identical(unique(oof$configuration_id), winner)
  expect_equal(nrow(oof), nrow(result$training_data))
  expect_identical(sort(oof$training_row), seq_len(nrow(result$training_data)))
  expect_true(all(is.finite(predict(result, result$test_data))))
})

test_that("discarding OOF predictions preserves actual omitted source rows and selected fits", {
  training <- boundary_search_data(80L)
  omitted_rows <- seq(3L, 80L, by = 9L)
  training$x[omitted_rows] <- NA_real_
  evaluation <- boundary_search_data(20L, seed = 581L)
  ids <- rep(c("north", "south"), each = 40L)
  run <- function(retain) {
    autoxplain(
      training, "y", test_data = evaluation,
      learners = "linear", max_models = 1L, explain = FALSE,
      preprocessing_config = list(missing_value_strategy = "drop_rows"),
      tuning_control = tuning_control(fold_ids = ids, retain_oof = retain)
    )
  }
  retained <- run(TRUE)
  omitted <- run(FALSE)
  expect_equal(nrow(retained$training_data), nrow(training) - length(omitted_rows))
  expect_equal(retained$tuning$rows_requested, nrow(training))
  expect_equal(retained$tuning$rows_omitted, length(omitted_rows))
  expect_identical(retained$tuning$omitted_rows, omitted$tuning$omitted_rows)
  oof <- retained$tuning$out_of_fold_predictions
  expect_equal(nrow(oof), nrow(training) - length(omitted_rows))
  expect_identical(sort(oof$training_row), setdiff(seq_len(nrow(training)), omitted_rows))
  expect_identical(oof$source_row, rownames(training)[oof$training_row])
  expect_identical(retained$tuning$fold_scores$score, omitted$tuning$fold_scores$score)
  expect_identical(predict(retained, evaluation), predict(omitted, evaluation))
  # Explicit base-R fits check that omission did not renumber row identities
  # and accidentally pair predictions with another row's outcome.
  for (label in unique(ids)) {
    fit <- stats::lm(y ~ x + z, training[ids != label, ], na.action = stats::na.omit)
    rows <- which(ids == label & !seq_len(nrow(training)) %in% omitted_rows)
    actual <- oof$estimate[match(rows, oof$training_row)]
    expect_length(actual, length(rows))
    expect_equal(actual, unname(stats::predict(fit, training[rows, ])), tolerance = 1e-12)
  }
})

test_that("final rounds include skipped folds and round an even median upward", {
  plan <- local_tuning_plan(1L, 90L, 2L, "regression", 1L, seed = 81L, learners = "boosting")
  scores <- data.frame(
    configuration_id = rep(plan$configuration_id, 3L), fold = 1:3, score = c(.9, 1.1, 1.2)
  )
  scores$effective_parameters <- I(list(list(nrounds = 4L), list(nrounds = 5L), list(nrounds = 18L)))
  scores$learned <- I(lapply(c("calibrated", "calibrated", "skipped"), function(status) {
    list(round_selection = list(status = status))
  }))
  result <- record_boosting_refit_rounds(plan, scores)
  expect_identical(result$round_selection[[1L]]$selected_rounds, 5L)
  expect_identical(result$round_selection[[1L]]$fold_rounds, c("1" = 4L, "2" = 5L, "3" = 18L))
  expect_equal(result$round_selection[[1L]]$calibrated_folds, 2L)
  even <- record_boosting_refit_rounds(plan, scores[1:2, ])
  expect_identical(even$round_selection[[1L]]$selected_rounds, 5L)
  # A corrupt failed fold must not contaminate the count used to fit all rows.
  scores$score[[2L]] <- NA_real_
  scores$effective_parameters[[2L]]$nrounds <- 100000L
  failed <- record_boosting_refit_rounds(plan, scores)
  expect_identical(failed$round_selection[[1L]]$selected_rounds, 11L)
  expect_identical(failed$round_selection[[1L]]$fold_rounds, c("1" = 4L, "3" = 18L))
})

test_that("negative held-out RMSLE predictions remain inspectable in a complete report", {
  training <- data.frame(x = seq(1, 10, length.out = 80L))
  training$y <- 2 + training$x + sin(training$x) / 10
  evaluation <- data.frame(x = c(-20, -15, -10), y = c(3, 4, 5))
  result <- autoxplain(
    training, "y", test_data = evaluation, learners = "linear", max_models = 1L,
    nfolds = 2L, explain = FALSE, tuning_control = tuning_control(metric = "rmsle")
  )
  prediction <- predict(result, evaluation)
  expect_length(prediction, nrow(evaluation))
  expect_true(all(prediction < 0))
  expect_true(is.na(result$evaluation$metrics$main_model[["rmsle"]]))
  expect_equal(
    result$evaluation$metrics$main_model[["rmse"]], sqrt(mean((evaluation$y - prediction)^2))
  )
  expect_false(isFALSE(result$evaluation$beats_baseline))
  availability <- result$evaluation$metric_availability
  primary <- availability[availability$model_id == "main_model", ]
  expect_equal(nrow(primary), 1L)
  expect_identical(primary$status, "unavailable")
  expect_equal(primary$invalid_predictions, nrow(evaluation))
  expect_true(primary$reason %in% result$evaluation$notes$message)
  path <- tempfile(fileext = ".html")
  on.exit(unlink(path), add = TRUE)
  expect_no_error(render_model_report(result, path, n_repeats = 2L))
  expect_gt(file.info(path)$size, 1000)
  expect_identical(predict(result, evaluation), prediction)
})

test_that("single-class final evaluation keeps AUC unavailable without breaking the report", {
  training <- data.frame(
    x = seq_len(100L), y = factor(ifelse(seq_len(100L) %% 5L == 0L, "yes", "no"))
  )
  evaluation <- data.frame(
    x = seq(1.5, 30.5, length.out = 30L), y = factor(rep("no", 30L), levels = c("no", "yes"))
  )
  result <- autoxplain(
    training, "y", test_data = evaluation, learners = "tree", max_models = 1L,
    nfolds = 2L, explain = FALSE, tuning_control = tuning_control(metric = "auc")
  )
  expect_equal(nrow(result$tuning$fold_scores), 2L)
  expect_true(all(is.finite(result$tuning$fold_scores$score)))
  expect_true(is.na(result$evaluation$metrics$main_model[["roc_auc"]]))
  expect_true(is.finite(result$evaluation$metrics$main_model[["log_loss"]]))
  expect_false(isFALSE(result$evaluation$beats_baseline))
  availability <- result$evaluation$metric_availability
  expect_identical(availability$status, rep("unavailable", 2L))
  expect_true(all(nzchar(availability$reason)))
  importance <- calculate_permutation_importance(
    as_explainers(result, models = "main_model")[[1L]], n_repeats = 2L
  )
  expect_true(all(is.na(importance$importance)))
  expect_identical(attr(importance, "metric"), "auc")
  expect_match(attr(importance, "unavailable_reason"), "original evaluation rows", fixed = TRUE)
  expect_false(grepl("Increase max_rows", attr(importance, "unavailable_reason"), fixed = TRUE))
  explainers <- as_explainers(result, models = "main_model")
  undefined <- audit_explanations(explainers, n_repeats = 2L)
  expect_gt(nrow(undefined$importance), 0L)
  expect_true(all(is.na(undefined$importance$importance)))
  expect_false("low_monte_carlo_budget" %in% undefined$findings$code)
  # The same fitted model and evaluation rows can support log loss. Keeping
  # its small-budget note proves the unavailable-AUC guard is selective.
  usable <- audit_explanations(explainers, metric = "log_loss", n_repeats = 2L)
  expect_gt(nrow(usable$importance), 0L)
  expect_true(all(is.finite(usable$importance$importance)))
  expect_true("low_monte_carlo_budget" %in% usable$findings$code)
  expect_error(
    selection_binary_auc(rep(FALSE, 3L), c(.1, .2, .3)), class = "autoxplain_auc_outcome_domain"
  )
  # A broken probability contract must still error before the class-domain
  # handling. Neither invalid probabilities nor prediction errors are hidden.
  expect_error(selection_binary_auc(rep(FALSE, 3L), c(.1, 1.2, .3)), "not clipped")
  expect_error(selection_binary_auc(rep(FALSE, 3L), c(.1, Inf, .3)), "finite numeric")
  path <- tempfile(fileext = ".html")
  on.exit(unlink(path), add = TRUE)
  expect_no_error(render_model_report(result, path, n_repeats = 2L))
  expect_gt(file.info(path)$size, 1000)
})
