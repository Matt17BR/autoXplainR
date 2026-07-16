h2o_preprocessing_config <- function(missing_value_strategy = "impute") {
  list(
    enable_target_handling = TRUE,
    enable_character_to_factors = TRUE,
    enable_ordered_factors = FALSE,
    enable_ordinal_factors = FALSE,
    enable_id_removal = FALSE,
    missing_value_strategy = missing_value_strategy,
    novel_level_strategy = "mode",
    verbose = FALSE
  )
}

test_that("H2O preparation creates a reproducible outer split before preprocessing", {
  data <- data.frame(
    x = seq_len(60),
    z = rep(c("a", "b"), 30),
    y = seq_len(60) + 0.1
  )
  seed <- 912L
  expected <- AutoXplainR:::make_evaluation_split(
    data,
    "y",
    "regression",
    0.2,
    seed
  )
  evaluation_rows <- as.integer(rownames(expected$evaluation))
  training_rows <- setdiff(seq_len(nrow(data)), evaluation_rows)
  data$x[evaluation_rows] <- 1e6
  data$x[evaluation_rows[[1L]]] <- NA_real_
  data$x[training_rows[[1L]]] <- NA_real_
  expected_training_median <- stats::median(
    data$x[-evaluation_rows],
    na.rm = TRUE
  )

  set.seed(71)
  random_state <- .Random.seed
  prepared <- AutoXplainR:::prepare_h2o_outer_split(
    data = data,
    test_data = NULL,
    target = "y",
    task = "regression",
    test_fraction = 0.2,
    seed = seed,
    enable_preprocessing = TRUE,
    preprocessing_config = h2o_preprocessing_config()
  )
  repeated <- AutoXplainR:::prepare_h2o_outer_split(
    data = data,
    test_data = NULL,
    target = "y",
    task = "regression",
    test_fraction = 0.2,
    seed = seed,
    enable_preprocessing = TRUE,
    preprocessing_config = h2o_preprocessing_config()
  )

  expect_identical(.Random.seed, random_state)
  expect_identical(
    rownames(prepared$training$data),
    rownames(repeated$training$data)
  )
  expect_identical(
    rownames(prepared$evaluation$data),
    rownames(repeated$evaluation$data)
  )
  expect_length(intersect(
    rownames(prepared$training$data),
    rownames(prepared$evaluation$data)
  ), 0L)
  expect_setequal(
    c(rownames(prepared$training$data), rownames(prepared$evaluation$data)),
    rownames(data)
  )
  expect_equal(nrow(prepared$training$data), 48L)
  expect_equal(nrow(prepared$evaluation$data), 12L)
  expect_equal(
    prepared$training$recipe$imputations$x,
    expected_training_median
  )
  expect_equal(
    prepared$evaluation$data$x[[which(is.na(data$x[evaluation_rows]))]],
    expected_training_median
  )
  expect_identical(prepared$split_method, "reproducible random holdout")
  expect_false(prepared$test_data_supplied)
  expect_equal(prepared$test_fraction_requested, 0.2)
})

test_that("H2O automatic classification holdouts retain every class", {
  data <- data.frame(
    x = seq_len(90),
    y = factor(rep(c("first", "second", "third"), each = 30L))
  )
  prepared <- AutoXplainR:::prepare_h2o_outer_split(
    data = data,
    test_data = NULL,
    target = "y",
    task = "multiclass",
    test_fraction = 0.2,
    seed = 19L,
    enable_preprocessing = TRUE,
    preprocessing_config = h2o_preprocessing_config("keep")
  )

  expect_identical(prepared$split_method, "reproducible stratified holdout")
  expect_equal(
    as.integer(table(prepared$training$data$y)),
    c(24L, 24L, 24L)
  )
  expect_equal(
    as.integer(table(prepared$evaluation$data$y)),
    c(6L, 6L, 6L)
  )
  expect_identical(
    levels(prepared$evaluation$data$y),
    levels(prepared$training$data$y)
  )
})

test_that("H2O preparation preserves explicit test data behavior", {
  training <- data.frame(
    x = c(1, 2, 3, 4, NA, 6, 7, 8, 9, 10),
    group = rep(c("a", "b"), 5),
    y = seq_len(10)
  )
  evaluation <- data.frame(
    x = c(NA, 1e9),
    group = c("a", "new"),
    y = c(11, 12),
    extra = c("ignored", "ignored")
  )
  prepared <- AutoXplainR:::prepare_h2o_outer_split(
    data = training,
    test_data = evaluation,
    target = "y",
    task = "regression",
    test_fraction = 0.4,
    seed = 22L,
    enable_preprocessing = TRUE,
    preprocessing_config = h2o_preprocessing_config()
  )

  expected_median <- stats::median(training$x, na.rm = TRUE)
  expect_identical(rownames(prepared$training$data), rownames(training))
  expect_identical(rownames(prepared$evaluation$data), rownames(evaluation))
  expect_equal(prepared$training$recipe$imputations$x, expected_median)
  expect_equal(prepared$evaluation$data$x[[1L]], expected_median)
  expect_equal(as.character(prepared$evaluation$data$group), c("a", "a"))
  expect_false("extra" %in% names(prepared$evaluation$data))
  expect_identical(prepared$split_method, "user-supplied evaluation data")
  expect_true(prepared$test_data_supplied)
  expect_true(is.na(prepared$test_fraction_requested))
})

test_that("H2O preparation rejects invalid evaluation outcomes", {
  training <- data.frame(
    x = seq_len(12),
    event = factor(rep(c("no", "yes"), 6L), levels = c("no", "yes"))
  )
  missing_outcome <- data.frame(
    x = 13:16,
    event = factor(c("no", "yes", NA, "no"), levels = c("no", "yes"))
  )
  unseen_outcome <- data.frame(
    x = 13:16,
    event = factor(c("no", "yes", "other", "no"))
  )

  prepare <- function(evaluation) {
    AutoXplainR:::prepare_h2o_outer_split(
      data = training,
      test_data = evaluation,
      target = "event",
      task = "binary",
      test_fraction = 0.2,
      seed = 4L,
      enable_preprocessing = TRUE,
      preprocessing_config = h2o_preprocessing_config("keep")
    )
  }
  expect_error(prepare(missing_outcome), "missing values or classes absent")
  expect_error(prepare(unseen_outcome), "unseen levels")
})

test_that("H2O evaluation roles reflect their actual selection use", {
  automatic <- AutoXplainR:::resolve_h2o_evaluation_role(
    test_data_supplied = FALSE,
    use_test_as_validation = TRUE,
    nfolds = 0L
  )
  supplied_evaluation <- AutoXplainR:::resolve_h2o_evaluation_role(
    test_data_supplied = TRUE,
    use_test_as_validation = FALSE,
    nfolds = 5L
  )
  ignored_validation_request <- AutoXplainR:::resolve_h2o_evaluation_role(
    test_data_supplied = TRUE,
    use_test_as_validation = TRUE,
    nfolds = 5L
  )
  explicit_test <- AutoXplainR:::resolve_h2o_evaluation_role(
    test_data_supplied = TRUE,
    use_test_as_validation = FALSE,
    nfolds = 5L,
    evaluation_role = "test"
  )
  selection_validation <- AutoXplainR:::resolve_h2o_evaluation_role(
    test_data_supplied = TRUE,
    use_test_as_validation = TRUE,
    nfolds = 0L
  )

  expect_false(automatic$test_used_for_validation)
  expect_identical(automatic$evaluation_role, "test")
  expect_false(supplied_evaluation$test_used_for_validation)
  expect_identical(supplied_evaluation$evaluation_role, "evaluation")
  expect_false(ignored_validation_request$test_used_for_validation)
  expect_true(ignored_validation_request$validation_requested_but_not_used)
  expect_identical(ignored_validation_request$evaluation_role, "evaluation")
  expect_false(explicit_test$test_used_for_validation)
  expect_identical(explicit_test$evaluation_role, "test")
  expect_true(selection_validation$test_used_for_validation)
  expect_identical(selection_validation$evaluation_role, "validation")
  expect_error(
    AutoXplainR:::resolve_h2o_evaluation_role(
      TRUE, TRUE, 0L, evaluation_role = "test"
    ),
    "contradicts"
  )
  expect_error(
    AutoXplainR:::resolve_h2o_evaluation_role(
      TRUE, TRUE, 0L, evaluation_role = "evaluation"
    ),
    "used for H2O model selection"
  )
})

test_that("H2O preparation applies overlap policy and rejects infinite predictors", {
  training <- data.frame(x = seq_len(12), y = seq_len(12) + 0.5)
  expect_warning(
    AutoXplainR:::prepare_h2o_outer_split(
      data = training,
      test_data = training[1:2, , drop = FALSE],
      target = "y",
      task = "regression",
      test_fraction = 0.2,
      seed = 4L,
      enable_preprocessing = TRUE,
      preprocessing_config = h2o_preprocessing_config("keep")
    ),
    "may indicate training/evaluation leakage.*do not prove"
  )
  expect_error(
    AutoXplainR:::prepare_h2o_outer_split(
      data = training,
      test_data = training[1:2, , drop = FALSE],
      target = "y",
      task = "regression",
      test_fraction = 0.2,
      seed = 4L,
      enable_preprocessing = TRUE,
      preprocessing_config = h2o_preprocessing_config("keep"),
      overlap_action = "error"
    ),
    "values exactly match.*do not prove"
  )

  evaluation <- data.frame(x = c(13, Inf), y = c(13.5, 14.5))
  expect_error(
    AutoXplainR:::prepare_h2o_outer_split(
      data = training,
      test_data = evaluation,
      target = "y",
      task = "regression",
      test_fraction = 0.2,
      seed = 4L,
      enable_preprocessing = TRUE,
      preprocessing_config = h2o_preprocessing_config("keep")
    ),
    "must not contain infinite values.*convert them to missing"
  )
})

test_that("H2O reproducibility provenance distinguishes fixed and timed searches", {
  fixed <- AutoXplainR:::h2o_reproducibility_provenance(
    0L, include_algos = NULL, exclude_algos = "DeepLearning"
  )
  timed <- AutoXplainR:::h2o_reproducibility_provenance(
    30L, include_algos = NULL, exclude_algos = NULL
  )

  expect_identical(fixed$mode, "more reproducible fixed-model-budget search")
  expect_false(fixed$time_limited)
  expect_false(fixed$deep_learning_possible)
  expect_identical(timed$mode, "best-effort seeded search")
  expect_true(timed$time_limited)
  expect_true(timed$deep_learning_possible)
  expect_match(timed$note, "does not guarantee")
})

test_that("H2O selection provenance distinguishes cross-validation and validation", {
  cross_validated <- AutoXplainR:::h2o_candidate_selection_provenance(
    nfolds = 5L,
    used_for_validation = FALSE,
    evaluation_role = "evaluation"
  )
  validation_ranked <- AutoXplainR:::h2o_candidate_selection_provenance(
    nfolds = 0L,
    used_for_validation = TRUE,
    evaluation_role = "validation"
  )

  expect_match(cross_validated, "5-fold training-only cross-validation")
  expect_match(cross_validated, "evaluation rows were not used")
  expect_false(grepl("unless", cross_validated, fixed = TRUE))
  expect_match(validation_ranked, "supplied validation rows because nfolds = 0")
  expect_match(validation_ranked, "not an independent test")
})

test_that("H2O cannot rank multiple models on training error alone", {
  expect_error(
    autoxplain(
      mtcars,
      "mpg",
      engine = "h2o",
      nfolds = 0L,
      init_h2o = FALSE
    ),
    "requires package `h2o`|rank models without cross-validation"
  )

  # Exercise the validation branch without requiring an H2O cluster by
  # temporarily bypassing the optional dependency guard. The call must advance
  # past the nfolds contract and fail only when it tries to establish H2O.
  local_mocked_bindings(
    require_optional = function(...) invisible(TRUE),
    ensure_h2o_connection = function(...) stop("connection sentinel"),
    .package = "AutoXplainR"
  )
  expect_error(
    autoxplain(
      mtcars,
      "mpg",
      engine = "h2o",
      nfolds = 0L,
      init_h2o = FALSE
    ),
    "rank models without cross-validation"
  )
  expect_error(
    autoxplain(
      mtcars[1:24, , drop = FALSE],
      "mpg",
      engine = "h2o",
      test_data = mtcars[25:32, , drop = FALSE],
      use_test_as_validation = TRUE,
      nfolds = 0L,
      init_h2o = FALSE
    ),
    "connection sentinel"
  )
})
