test_that("tuning receives raw predictors before full-training screening", {
  set.seed(410)
  training <- data.frame(
    x = seq(-2, 2, length.out = 36),
    global_constant = 1,
    case_id = sprintf("case-%03d", seq_len(36)),
    mostly_missing = c(rep(NA_real_, 30), seq_len(6))
  )
  training$y <- 1 + 2 * training$x + rnorm(36, sd = 0.1)
  rownames(training) <- paste0("outer-row-", seq_len(nrow(training)))
  evaluation <- training[25:36, , drop = FALSE]

  result <- autoxplain(
    training,
    "y",
    test_data = evaluation,
    model_set = "tuned",
    portfolio = "core",
    learners = "linear",
    max_models = 1,
    nfolds = 3,
    preprocessing_config = list(
      enable_id_removal = TRUE,
      missing_value_strategy = "drop_columns",
      missing_column_threshold = 0.5
    ),
    seed = 410
  )

  expect_identical(
    result$tuning$predictors_received,
    c("x", "global_constant", "case_id", "mostly_missing")
  )
  screened <- c("global_constant", "case_id", "mostly_missing")
  expect_false(any(screened %in% result$features))
  expect_length(result$tuning$fold_preprocessing, 3L)
  expect_true(all(vapply(result$tuning$fold_preprocessing, function(fold) {
    identical(fold$predictors_received, result$tuning$predictors_received) &&
      all(screened %in% fold$predictors_removed) &&
      "x" %in% fold$predictors_retained
  }, logical(1))))
})

test_that("constant-predictor decisions are learned independently by fold", {
  raw <- data.frame(
    stable = seq_len(12),
    rare = c(1, rep(0, 11)),
    y = seq_len(12) / 3
  )
  rownames(raw) <- paste0("source-", seq_len(nrow(raw)))
  assignment <- rep(1:3, each = 4)

  first <- AutoXplainR:::prepare_tuning_fold(
    raw,
    target = "y",
    task = "regression",
    fold_assignment = assignment,
    fold = 1,
    enable_preprocessing = TRUE,
    preprocessing_config = list(missing_value_strategy = "impute")
  )
  second <- AutoXplainR:::prepare_tuning_fold(
    raw,
    target = "y",
    task = "regression",
    fold_assignment = assignment,
    fold = 2,
    enable_preprocessing = TRUE,
    preprocessing_config = list(missing_value_strategy = "impute")
  )

  expect_true("rare" %in% first$removed_features)
  expect_false("rare" %in% first$retained_features)
  expect_false("rare" %in% second$removed_features)
  expect_true("rare" %in% second$retained_features)
  expect_identical(first$validation_row, which(assignment == 1L))
  expect_identical(first$source_row, rownames(raw)[assignment == 1L])
})

test_that("drop_rows reports a class removed from a classification fold", {
  raw <- data.frame(
    x = seq_len(12),
    z = seq_len(12) / 10,
    event = factor(rep(c("no", "yes"), 6), levels = c("no", "yes"))
  )
  assignment <- c(rep(1L, 4), rep(2L, 8))
  training_rows <- which(assignment != 1L)
  affected <- training_rows[raw$event[training_rows] == "yes"]
  raw$x[affected] <- NA_real_

  expect_error(
    AutoXplainR:::prepare_tuning_fold(
      raw,
      target = "event",
      task = "binary",
      fold_assignment = assignment,
      fold = 1,
      enable_preprocessing = TRUE,
      preprocessing_config = list(missing_value_strategy = "drop_rows")
    ),
    paste0(
      "Tuning fold 1 lost outcome class `yes` from its training partition because ",
      "`missing_value_strategy = \\\"drop_rows\\\"`.*",
      "missing_value_strategy = \\\"impute\\\""
    )
  )
})
