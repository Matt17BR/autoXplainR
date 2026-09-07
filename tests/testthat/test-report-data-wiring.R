test_that("automatic data capture uses original positions with arbitrary row names", {
  set.seed(550)
  data <- data.frame(x = rnorm(60), constant_context = "fixed", y = rnorm(60))
  rownames(data) <- paste0("original-row-", seq_len(nrow(data)))
  data$x[c(2, 11, 29, 57)] <- NA_real_
  result <- autoxplain(data, "y",
    model_set = "quick", seed = 29, explain = FALSE,
    preprocessing_config = list(missing_value_strategy = "drop_rows")
  )
  context <- result$data_context
  map <- context$row_map
  expect_setequal(map$source_row, seq_len(nrow(data)))
  expect_identical(anyDuplicated(map$row_key), 0L)
  expect_true(all(map$source == "data"))
  expect_setequal(map$source_row[!map$retained], c(2L, 11L, 29L, 57L))
  for (partition in c("training", "evaluation")) {
    rows <- map[map$partition == partition, ]
    expect_identical(context$raw[[partition]], data[rows$source_row, , drop = FALSE])
    retained <- rows[rows$retained, ]
    processed <- if (partition == "training") result$training_data else result$test_data
    expect_identical(rownames(processed), rownames(data)[retained$source_row])
    expect_identical(retained$processed_position, seq_len(nrow(processed)))
  }
  expect_false("constant_context" %in% result$features)
  expect_true("constant_context" %in% names(context$raw$training))
  expect_identical(result$evaluation_context$x, result$test_data$x)
  expect_silent(AutoXplainR:::validate_data_context(context, result$training_data, result$test_data))
})

test_that("supplied evaluations keep separate source identities and excluded rows", {
  set.seed(81)
  training <- data.frame(x = rnorm(48), y = rnorm(48))
  evaluation <- data.frame(x = rnorm(12) + 20, y = rnorm(12))
  # Deliberately overlapping row names are not source identities.
  rownames(training) <- paste0("case-", seq_len(nrow(training)))
  rownames(evaluation) <- paste0("case-", seq_len(nrow(evaluation)))
  training$x[c(2, 7)] <- NA_real_
  evaluation$x[c(1, 9)] <- NA_real_
  result <- autoxplain(training, "y",
    test_data = evaluation, model_set = "quick", explain = FALSE,
    preprocessing_config = list(missing_value_strategy = "drop_rows")
  )
  context <- result$data_context
  expect_identical(context$raw$training, training)
  expect_identical(context$raw$evaluation, evaluation)
  map <- context$row_map
  expect_identical(anyDuplicated(map$row_key), 0L)
  expect_true(all(map$source[map$partition == "training"] == "data"))
  expect_true(all(map$source[map$partition == "evaluation"] == "test_data"))
  expect_identical(result$evaluation_row_indices, setdiff(seq_len(12), c(1L, 9L)))
  expect_identical(result$evaluation_context$x, evaluation$x[result$evaluation_row_indices])
  expect_silent(AutoXplainR:::validate_data_context(context, result$training_data, result$test_data))
})

test_that("temporal context preserves gap rows and uses original time positions", {
  set.seed(93)
  data <- data.frame(time = rep(seq_len(30), each = 2), x = rnorm(60), y = rnorm(60))
  rownames(data) <- paste0("visit-", seq_len(nrow(data)))
  data$x[c(3, 58)] <- NA_real_
  result <- autoxplain(data, "y",
    model_set = "quick", explain = FALSE,
    validation = validation_split(time = "time", gap = 2),
    preprocessing_config = list(missing_value_strategy = "drop_rows")
  )
  map <- result$data_context$row_map
  expect_setequal(map$source_row, seq_len(nrow(data)))
  gap <- which(data$time %in% c(23L, 24L))
  expect_identical(result$data_context$raw$excluded, data[gap, , drop = FALSE])
  expect_setequal(map$source_row[map$partition == "excluded"], gap)
  expect_true(all(!map$retained[map$partition == "excluded"]))
  expect_false("time" %in% result$features)
  expect_identical(result$data_context$columns$role[result$data_context$columns$name == "time"], "split")
  eval_source <- map$source_row[map$partition == "evaluation" & map$retained]
  expect_identical(result$validation$evaluation_rows, eval_source)
  expect_identical(result$evaluation_context$time, data$time[eval_source])
  expect_silent(AutoXplainR:::validate_data_context(result$data_context, result$training_data, result$test_data))
})

test_that("H2O outer preparation supplies the same capture contract without a live engine", {
  set.seed(139)
  data <- data.frame(x = rnorm(60), y = rnorm(60))
  rownames(data) <- paste0("h2o-source-", seq_len(60))
  data$x[c(1, 9, 51)] <- NA_real_
  config <- list(
    enable_target_handling = TRUE, enable_character_to_factors = TRUE,
    enable_id_removal = FALSE, missing_value_strategy = "drop_rows", novel_level_strategy = "mode", verbose = FALSE
  )
  for (supplied in c(FALSE, TRUE)) {
    training <- if (supplied) data[1:45, , drop = FALSE] else data
    evaluation <- if (supplied) data[46:60, , drop = FALSE] else NULL
    prepared <- AutoXplainR:::prepare_h2o_outer_split(training, evaluation, "y", "regression", .2, 31,
      enable_preprocessing = TRUE, preprocessing_config = config
    )
    context <- AutoXplainR:::capture_data_context(
      prepared$raw_training, prepared$raw_evaluation, "y", "x", prepared$training, prepared$evaluation,
      training_source_rows = prepared$training_source_rows,
      evaluation_source_rows = prepared$evaluation_source_rows, evaluation_source = prepared$evaluation_source
    )
    expect_identical(anyDuplicated(context$row_map$row_key), 0L)
    expect_identical(context$raw$training, training[prepared$training_source_rows, , drop = FALSE])
    source <- if (supplied) evaluation else data
    expect_identical(context$raw$evaluation, source[prepared$evaluation_source_rows, , drop = FALSE])
    expect_identical(prepared$evaluation_context$x, prepared$evaluation$data$x)
    expect_silent(AutoXplainR:::validate_data_context(context, prepared$training$data, prepared$evaluation$data))
  }
})
