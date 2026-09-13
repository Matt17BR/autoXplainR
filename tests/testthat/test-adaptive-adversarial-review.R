# Independent review probes. Expected values come from native calls, observed
# argument interception, or invariants rather than reproducing search helpers.
adversarial_review_data <- function(n = 120L) {
  with_preserved_seed(818L, {
    data.frame(x = rnorm(n), z = rnorm(n), y = rnorm(n))
  })
}

test_that("failed post-calibration fits retain the native attempt and round evidence", {
  skip_if_not_installed("xgboost", minimum_version = "3.2.1.1")
  data <- adversarial_review_data()
  parameters <- boosting_learner_grid(120L, 2L, "regression", 1L)[[1L]]
  parameters$nrounds <- 50L
  parameters$eta <- .3
  plan <- local_tuning_plan(
    1L, 120L, 2L, "regression", 1L, learners = "boosting", seed = 17L,
    custom_grids = list(boosting = list(parameters))
  )
  plan$patience <- 3L
  plan$metric <- "rmse"
  fold <- prepare_tuning_fold(
    data, "y", "regression", rep(1:2, 60L), 1L, TRUE, list(verbose = FALSE)
  )
  fold$boosting_calibration <- prepare_boosting_calibration(
    data[seq(2L, 120L, 2L), ], "y", "regression", TRUE, list(verbose = FALSE), 53L
  )
  original <- fit_boosting_core
  actual <- new.env(parent = emptyenv())
  local_mocked_bindings(fit_boosting_core = function(
    data, target, task, parameters, seed, threads = 1L, validation = NULL,
    early_stopping_rounds = NULL, metric = NULL
  ) {
    if (is.null(validation)) {
      actual$rounds <- parameters$nrounds
      actual$seed <- seed
      stop("injected fixed-round backend failure")
    }
    answer <- original(
      data, target, task, parameters, seed, threads, validation, early_stopping_rounds, metric
    )
    actual$calibration <- answer$calibration
    answer
  })
  recorded <- score_tuning_configuration(plan, fold, "y", "regression", 1L)$score
  expect_lt(actual$rounds, parameters$nrounds)
  expect_equal(recorded$model_fit_attempts, 1L)
  expect_equal(recorded$calibration_fit_attempts, 1L)
  expect_match(recorded$error, "injected fixed-round")
  expect_identical(recorded$effective_parameters[[1L]]$nrounds, actual$rounds)
  expect_identical(recorded$fit_seed, actual$seed)
  expect_equal(recorded$learned[[1L]]$round_selection$curve, actual$calibration$curve)
  expect_identical(recorded$learned[[1L]]$round_selection$selected_rounds, actual$rounds)
})

test_that("refit fallback retains the failed model's actual rounds and seed", {
  skip_if_not_installed("xgboost", minimum_version = "3.2.1.1")
  training <- adversarial_review_data()
  evaluation <- adversarial_review_data(30L)
  parameters <- boosting_learner_grid(120L, 2L, "regression", 1L)[[1L]]
  parameters$nrounds <- 30L
  parameters$eta <- .3
  second <- parameters
  second$max_depth <- 3L
  original <- fit_boosting_core
  actual <- new.env(parent = emptyenv())
  actual$attempts <- 0L
  local_mocked_bindings(fit_boosting_core = function(
    data, target, task, parameters, seed, threads = 1L, validation = NULL,
    early_stopping_rounds = NULL, metric = NULL
  ) {
    actual$attempts <- actual$attempts + 1L
    if (is.null(validation) && nrow(data) == nrow(training) && is.null(actual$rounds)) {
      actual$rounds <- parameters$nrounds
      actual$seed <- seed
      stop("injected first full-training failure")
    }
    original(data, target, task, parameters, seed, threads, validation, early_stopping_rounds, metric)
  })
  result <- autoxplain(
    training, "y", test_data = evaluation, learners = "boosting", max_models = 2L,
    nfolds = 2L, tuning_rule = "best", seed = 17L, explain = FALSE,
    tuning_control = tuning_control(
      search = "grid", early_stopping = TRUE, patience = 3L,
      grids = list(boosting = list(parameters, second))
    )
  )
  attempts <- result$tuning$refit$attempts
  failed <- attempts[attempts$status == "failed", , drop = FALSE]
  expect_true(result$tuning$refit$fallback_used)
  expect_equal(nrow(failed), 1L)
  expect_lt(actual$rounds, parameters$nrounds)
  expect_identical(failed$effective_parameters[[1L]]$nrounds, actual$rounds)
  expect_identical(failed$fit_seed, actual$seed)
  expect_identical(failed$learned[[1L]]$round_selection$selected_rounds, actual$rounds)
  expect_equal(result$tuning$resources$total_backend_fit_attempts, actual$attempts + 1L)
})

test_that("calibration probabilities follow outcome names despite reordered factor levels", {
  skip_if_not_installed("xgboost", minimum_version = "3.2.1.1")
  data <- adversarial_review_data()
  for (task in c("binary", "multiclass")) {
    labels <- if (task == "binary") c("zebra", "ant") else c("zebra", "ant", "mole")
    data$y <- factor(rep(labels, length.out = nrow(data)), levels = labels)
    training <- data[1:90, ]
    validation <- data[91:120, ]
    validation$y <- factor(as.character(validation$y), levels = rev(labels))
    parameters <- boosting_learner_grid(120L, 2L, task, length(labels))[[1L]]
    parameters$nrounds <- 8L
    fitted <- fit_boosting_core(
      training, "y", task, parameters, 97L, validation = validation,
      early_stopping_rounds = 3L, metric = "log_loss"
    )
    x <- as.matrix(validation[c("x", "z")])
    expected <- vapply(fitted$calibration$curve$round, function(round) {
      p <- predict(fitted$fit, x, iterationrange = c(1L, round))
      if (task == "binary") {
        p <- ifelse(as.character(validation$y) == labels[[2L]], p, 1 - p)
      } else {
        p <- p[cbind(seq_len(nrow(validation)), match(as.character(validation$y), labels))]
      }
      -mean(log(p))
    }, numeric(1))
    expect_equal(fitted$calibration$curve$score, expected, tolerance = 1e-14)
    expect_equal(fitted$calibration$selected_rounds, which.min(expected))
  }
})

test_that("refit aggregation includes skipped folds and ignores unsuccessful scores", {
  plan <- data.frame(configuration_id = "boosting_01", family = "boosting")
  plan$parameters <- I(list(list(nrounds = 50L)))
  scores <- data.frame(
    configuration_id = "boosting_01", fold = 1:4, score = c(.3, .4, .5, NA_real_)
  )
  scores$effective_parameters <- I(lapply(c(2L, 50L, 9L, 1L), function(rounds) list(nrounds = rounds)))
  scores$learned <- I(lapply(c("calibrated", "skipped", "calibrated", "calibrated"), function(status) {
    list(round_selection = list(status = status))
  }))
  aggregate <- record_boosting_refit_rounds(plan, scores)$round_selection[[1L]]
  expect_identical(aggregate$selected_rounds, 9L)
  expect_identical(unname(aggregate$fold_rounds), c(2L, 50L, 9L))
  expect_identical(aggregate$calibrated_folds, 2L)
})

test_that("failed calibration records its attempted cap without inventing a selected round", {
  skip_if_not_installed("xgboost", minimum_version = "3.2.1.1")
  data <- adversarial_review_data()
  parameters <- boosting_learner_grid(120L, 2L, "regression", 1L)[[1L]]
  parameters$nrounds <- 50L
  plan <- local_tuning_plan(
    1L, 120L, 2L, "regression", 1L, learners = "boosting", seed = 17L,
    custom_grids = list(boosting = list(parameters))
  )
  plan$patience <- 3L
  plan$metric <- "rmse"
  fold <- prepare_tuning_fold(
    data, "y", "regression", rep(1:2, 60L), 1L, TRUE, list(verbose = FALSE)
  )
  fold$boosting_calibration <- prepare_boosting_calibration(
    data[seq(2L, 120L, 2L), ], "y", "regression", TRUE, list(verbose = FALSE), 53L
  )
  actual <- new.env(parent = emptyenv())
  local_mocked_bindings(fit_boosting_core = function(
    data, target, task, parameters, seed, threads = 1L, validation = NULL,
    early_stopping_rounds = NULL, metric = NULL
  ) {
    expect_false(is.null(validation))
    actual$seed <- seed
    actual$rounds <- parameters$nrounds
    stop("injected native calibration failure")
  })
  recorded <- score_tuning_configuration(plan, fold, "y", "regression", 1L)$score
  expect_match(recorded$error, "injected native calibration")
  expect_identical(recorded$effective_parameters[[1L]]$nrounds, actual$rounds)
  expect_identical(recorded$fit_seed, actual$seed)
  expect_identical(recorded$calibration_fit_attempts, 1L)
  expect_identical(recorded$model_fit_attempts, 0L)
  expect_null(recorded$learned[[1L]]$round_selection)
})
