boosting_calibration_fixture <- function(task = "regression") {
  set.seed(218)
  data <- data.frame(x = rnorm(180), z = runif(180), segment = factor(rep(letters[1:3], 60)))
  data$y <- switch(task,
    regression = data$x^2 + data$z + rnorm(180, sd = 0.5),
    binary = factor(rep(c("no", "yes"), 90), levels = c("no", "yes")),
    multiclass = factor(rep(c("red", "green", "blue"), 60), levels = c("red", "green", "blue"))
  )
  data
}

prepare_calibration_test <- function(data, task = "regression", groups = NULL, config = list()) {
  AutoXplainR:::prepare_boosting_calibration(
    data, "y", task, TRUE,
    utils::modifyList(list(missing_value_strategy = "impute", novel_level_strategy = "mode"), config),
    seed = 816L, groups = groups
  )
}

test_that("inner preprocessing is fitted before seeing calibration values", {
  data <- boosting_calibration_fixture()
  initial <- prepare_calibration_test(data)
  data$x[initial$training_row[1:5]] <- NA_real_
  data$x[initial$validation_row] <- 1e9
  data$x[initial$validation_row[[1]]] <- NA_real_
  data$segment <- as.character(data$segment)
  data$segment[initial$validation_row] <- "validation only"
  prepared <- prepare_calibration_test(data)
  expected_median <- stats::median(data$x[initial$training_row], na.rm = TRUE)
  expect_identical(prepared$training_row, initial$training_row)
  expect_equal(prepared$recipe$imputations$x, expected_median)
  expect_equal(prepared$validation$x[[1]], expected_median)
  expect_false("validation only" %in% prepared$recipe$factor_levels$segment)
  expect_equal(prepared$novel_levels_mapped, length(prepared$validation_row))
  expect_length(intersect(prepared$training_row, prepared$validation_row), 0L)
  expect_identical(sort(c(prepared$training_row, prepared$validation_row)), seq_len(nrow(data)))

  changed <- data
  changed$y[prepared$validation_row] <- 1e8
  changed$x[prepared$validation_row] <- -1e9
  changed$segment[prepared$validation_row] <- "another unseen category"
  repeated <- prepare_calibration_test(changed)
  expect_identical(prepared$training, repeated$training)
  expect_identical(prepared$recipe, repeated$recipe)
  set.seed(53)
  before <- .Random.seed
  invisible(prepare_calibration_test(data))
  expect_identical(.Random.seed, before)
})

test_that("inner calibration respects groups and documents unsplittable classes", {
  data <- boosting_calibration_fixture("binary")
  groups <- rep(seq_len(30), each = 6)
  prepared <- prepare_calibration_test(data, "binary", groups)
  expect_identical(prepared$status, "ready")
  expect_true(prepared$grouped)
  expect_length(intersect(groups[prepared$training_row], groups[prepared$validation_row]), 0L)
  expect_equal(sort(unique(as.character(prepared$training$y))), c("no", "yes"))
  expect_equal(sort(unique(as.character(prepared$validation$y))), c("no", "yes"))
  unsplittable <- prepare_calibration_test(data, "binary", as.character(data$y))
  expect_identical(unsplittable$status, "skipped")
  expect_match(unsplittable$reason, "fewer than two independent training groups")
  one_group <- prepare_calibration_test(data, "binary", rep(1L, nrow(data)))
  expect_identical(one_group$status, "skipped")
  expect_error(prepare_calibration_test(data, "binary", groups[-1]), "identify every raw training row")
  parameters <- list(nrounds = 77L)
  skipped <- AutoXplainR:::calibrate_boosting_rounds(one_group, "y", "binary", parameters, 81L)
  expect_identical(skipped$rounds, 77L)
  expect_identical(skipped$evidence$status, "skipped")
  expect_identical(skipped$evidence$stop_reason, one_group$reason)
  small <- prepare_calibration_test(data[1:10, ], "binary")
  expect_identical(small$status, "skipped")
  rare <- data
  rare$y <- factor(c("yes", rep("no", nrow(data) - 1)))
  expect_match(prepare_calibration_test(rare, "binary")$reason, "at least two rows")
  constant <- data.frame(x = rep(0, 30), y = seq_len(30))
  expect_match(prepare_calibration_test(constant)$reason, "No predictor varies")
})

test_that("inner preprocessing omissions preserve raw row identities", {
  data <- boosting_calibration_fixture()
  initial <- prepare_calibration_test(data)
  omitted <- c(initial$training_row[[1]], initial$validation_row[[1]])
  data$x[omitted] <- NA_real_
  prepared <- prepare_calibration_test(data, config = list(missing_value_strategy = "drop_rows"))
  expect_equal(prepared$training_rows_omitted, 1L)
  expect_equal(prepared$validation_rows_omitted, 1L)
  expect_false(any(omitted %in% c(prepared$training_row, prepared$validation_row)))
  expect_identical(rownames(prepared$training), rownames(data)[prepared$training_row])
  expect_identical(rownames(prepared$validation), rownames(data)[prepared$validation_row])
})

test_that("fixed-round boosting core matches an independent native fit", {
  skip_if_package_unavailable("xgboost")
  for (task in c("regression", "binary", "multiclass")) {
    for (encoding in c("matrix", "native")) {
      data <- boosting_calibration_fixture(task)
      parameters <- AutoXplainR:::boosting_learner_grid(180, 3, task, 3)[[1]]
      parameters$nrounds <- 8L
      parameters$encoding <- encoding
      fitted <- AutoXplainR:::fit_boosting_core(data, "y", task, parameters, 41L)
      x <- if (encoding == "native") {
        AutoXplainR:::bake_boosting_native_blueprint(fitted$blueprint, data)
      } else {
        AutoXplainR:::bake_matrix_blueprint(fitted$blueprint, data)
      }
      label <- if (task == "regression") data$y else as.numeric(data$y) - 1L
      native_parameters <- list(
        objective = switch(task,
          regression = "reg:squarederror", binary = "binary:logistic", multiclass = "multi:softprob"
        ),
        eval_metric = switch(task, regression = "rmse", binary = "logloss", multiclass = "mlogloss"),
        eta = .1, max_depth = 2L, min_child_weight = 1, subsample = 1,
        colsample_bytree = 1, alpha = 0, lambda = 1, nthread = 1L, seed = 41L, verbosity = 0L
      )
      if (task == "multiclass") native_parameters$num_class <- 3L
      matrix <- if (encoding == "native") {
        native_parameters$tree_method <- "hist"
        native_parameters$max_bin <- 256L
        xgboost::xgb.QuantileDMatrix(x, label = label, nthread = 1L, max_bin = 256L)
      } else {
        xgboost::xgb.DMatrix(x, label = label, nthread = 1L)
      }
      reference <- xgboost::xgb.train(native_parameters, matrix, nrounds = 8L, verbose = 0L)
      expect_equal(stats::predict(fitted$fit, x), stats::predict(reference, x), tolerance = 0)
      expect_null(fitted$calibration)
      expect_equal(xgboost::xgb.get.num.boosted.rounds(fitted$fit), 8L)
    }
  }
})

test_that("early stopping rounds match native validation scores without off-by-one", {
  skip_if_package_unavailable("xgboost")
  set.seed(563)
  data <- data.frame(x = rnorm(200), z = rnorm(200), y = rnorm(200))
  prepared <- prepare_calibration_test(data)
  parameters <- AutoXplainR:::boosting_learner_grid(200, 2, "regression", 1)[[1]]
  parameters$nrounds <- 150L
  parameters$max_depth <- 5L
  parameters$eta <- 0.3
  calibrated <- AutoXplainR:::calibrate_boosting_rounds(
    prepared, "y", "regression", parameters, 73L, patience = 5L
  )
  before <- .Random.seed
  repeated <- AutoXplainR:::calibrate_boosting_rounds(
    prepared, "y", "regression", parameters, 73L, patience = 5L
  )
  expect_identical(repeated, calibrated)
  expect_identical(.Random.seed, before)
  training <- as.matrix(prepared$training[c("x", "z")])
  validation <- as.matrix(prepared$validation[c("x", "z")])
  reference <- xgboost::xgb.train(
    list(
      objective = "reg:squarederror", eval_metric = "rmse", eta = .3, max_depth = 5L,
      min_child_weight = 1, subsample = 1, colsample_bytree = 1, alpha = 0, lambda = 1,
      nthread = 1L, seed = 73L, verbosity = 0L
    ),
    xgboost::xgb.DMatrix(training, label = prepared$training$y, nthread = 1L),
    nrounds = 150L,
    evals = list(inner = xgboost::xgb.DMatrix(validation, label = prepared$validation$y, nthread = 1L)),
    early_stopping_rounds = 5L, verbose = 0L
  )
  reference_curve <- as.data.frame(attr(reference, "evaluation_log"))
  expect_equal(calibrated$rounds, as.integer(xgboost::xgb.attr(reference, "best_iteration")) + 1L)
  expect_equal(calibrated$evidence$curve$score, reference_curve$inner_rmse, tolerance = 1e-7)
  expect_identical(calibrated$evidence$stop_reason, "patience_reached")
  expect_equal(calibrated$evidence$attempted_rounds, calibrated$rounds + 5L)
  expect_lt(calibrated$evidence$attempted_rounds, parameters$nrounds)
  parameters$nrounds <- calibrated$rounds
  full <- AutoXplainR:::fit_boosting_core(data, "y", "regression", parameters, 73L)
  expect_equal(full$blueprint$training_rows, nrow(data))
  expect_equal(xgboost::xgb.get.num.boosted.rounds(full$fit), calibrated$rounds)
})

test_that("native categorical classification calibration uses the requested Brier score", {
  skip_if_package_unavailable("xgboost")
  for (task in c("binary", "multiclass")) {
    data <- boosting_calibration_fixture(task)
    prepared <- prepare_calibration_test(data, task)
    parameters <- AutoXplainR:::boosting_learner_grid(180, 3, task, 3)[[1]]
    parameters$nrounds <- 12L
    parameters$encoding <- "native"
    fit <- AutoXplainR:::fit_boosting_core(
      prepared$training, "y", task, parameters, 30L,
      validation = prepared$validation, early_stopping_rounds = 3L, metric = "brier_score"
    )
    x <- AutoXplainR:::bake_boosting_native_blueprint(fit$blueprint, prepared$validation)
    expected <- vapply(fit$calibration$curve$round, function(round) {
      probability <- stats::predict(fit$fit, x, iterationrange = c(1L, round))
      if (task == "binary") {
        mean((probability - as.numeric(prepared$validation$y == "yes"))^2)
      } else {
        truth <- matrix(0, nrow(probability), 3L)
        truth[cbind(seq_len(nrow(probability)), as.integer(prepared$validation$y))] <- 1
        mean(rowSums((probability - truth)^2))
      }
    }, numeric(1))
    expect_equal(fit$calibration$curve$score, expected, tolerance = 1e-15)
    expect_equal(fit$calibration$selected_rounds, which.min(fit$calibration$curve$score))
    expect_identical(fit$calibration$metric, "brier_score")
    expect_identical(fit$calibration$direction, "minimize")
  }
})

test_that("calibration maximizes natural AUC and scores exact native probability ties", {
  skip_if_package_unavailable("xgboost")
  data <- boosting_calibration_fixture("binary")
  prepared <- prepare_calibration_test(data, "binary")
  parameters <- AutoXplainR:::boosting_learner_grid(180, 3, "binary", 2)[[1]]
  parameters$nrounds <- 40L
  fit <- AutoXplainR:::fit_boosting_core(
    prepared$training, "y", "binary", parameters, 61L,
    validation = prepared$validation, early_stopping_rounds = 4L, metric = "roc_auc"
  )
  x <- AutoXplainR:::bake_matrix_blueprint(fit$blueprint, prepared$validation)
  truth <- prepared$validation$y == "yes"
  expected <- vapply(fit$calibration$curve$round, function(round) {
    probability <- stats::predict(fit$fit, x, iterationrange = c(1L, round))
    differences <- outer(probability[truth], probability[!truth], "-")
    mean((differences > 0) + .5 * (differences == 0))
  }, numeric(1))
  expect_equal(fit$calibration$curve$score, expected, tolerance = 0)
  expect_equal(fit$calibration$selected_rounds, which.max(expected))
  expect_identical(fit$calibration$direction, "maximize")
})

test_that("calibration RMSLE preserves its domain instead of clipping predictions", {
  skip_if_package_unavailable("xgboost")
  data <- boosting_calibration_fixture()
  data$y <- exp(data$x / 2)
  prepared <- prepare_calibration_test(data)
  parameters <- AutoXplainR:::boosting_learner_grid(180, 3, "regression", 1)[[1]]
  parameters$nrounds <- 12L
  fit <- AutoXplainR:::fit_boosting_core(
    prepared$training, "y", "regression", parameters, 69L,
    validation = prepared$validation, early_stopping_rounds = 4L, metric = "rmsle"
  )
  x <- AutoXplainR:::bake_matrix_blueprint(fit$blueprint, prepared$validation)
  expected <- vapply(fit$calibration$curve$round, function(round) {
    prediction <- stats::predict(fit$fit, x, iterationrange = c(1L, round))
    sqrt(mean((log1p(prepared$validation$y) - log1p(prediction))^2))
  }, numeric(1))
  expect_equal(fit$calibration$curve$score, expected, tolerance = 0)
  expect_equal(fit$calibration$selected_rounds, which.min(expected))
  negative_training <- prepared$training
  negative_training$y <- -negative_training$y
  expect_error(AutoXplainR:::fit_boosting_core(
    negative_training, "y", "regression", parameters, 69L,
    validation = prepared$validation, early_stopping_rounds = 4L, metric = "rmsle"
  ), class = "autoxplain_rmsle_prediction_domain")
})

test_that("calibration reports the round cap and applies explicit native thread counts", {
  skip_if_package_unavailable("xgboost")
  data <- boosting_calibration_fixture()
  prepared <- prepare_calibration_test(data)
  parameters <- AutoXplainR:::boosting_learner_grid(180, 3, "regression", 1)[[1]]
  parameters$nrounds <- 3L
  fit <- AutoXplainR:::fit_boosting_core(
    prepared$training, "y", "regression", parameters, 16L, threads = 2L,
    validation = prepared$validation, early_stopping_rounds = 5L
  )
  expect_identical(fit$calibration$stop_reason, "round_limit")
  expect_identical(fit$calibration$attempted_rounds, 3L)
  configuration <- xgboost::xgb.config(fit$fit)
  expect_identical(configuration$learner$generic_param$nthread, "2")
  expect_error(AutoXplainR:::fit_boosting_core(data, "y", "regression", parameters, 16L, threads = 0),
    "threads.*whole number"
  )
  expect_error(AutoXplainR:::fit_boosting_core(
    data, "y", "regression", parameters, 16L, early_stopping_rounds = 5L
  ), "requires an explicit inner-validation")
})
