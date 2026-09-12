scaling_profile_fixture <- function(strategy = "impute") {
  training <- data.frame(
    x = c(NA, 2:20), kind = rep(c("a", "b"), 10),
    y = 2 * seq_len(20), private_id = paste0("TRAIN_PRIVATE_", seq_len(20))
  )
  evaluation <- data.frame(
    x = c(NA, 1, 10, 20, 21, 30), kind = c("new", "a", "b", "a", NA, "b"),
    y = 2 * c(1, 1, 10, 20, 21, 30), private_id = paste0("EVAL_PRIVATE_", 1:6)
  )
  train <- preprocess_data(training[c("x", "kind", "y")], "y", missing_value_strategy = strategy)
  test <- AutoXplainR:::apply_preprocessing_recipe(evaluation, train$recipe, "y",
    missing_value_strategy = strategy, novel_level_strategy = "mode"
  )
  context <- AutoXplainR:::capture_data_context(training, evaluation, "y", c("x", "kind"), train, test)
  structure(list(
    training_data = train$data, test_data = test$data, target_column = "y",
    features = c("x", "kind"), data_context = context, provenance = list(seed = 4)
  ), class = "autoxplain_result")
}

test_that("relationship sampling preserves exact whole-column summaries", {
  result <- scaling_profile_fixture()
  full <- AutoXplainR:::prepare_data_explorer(result, report_data_control(max_pair_rows = NULL))
  sampled <- AutoXplainR:::prepare_data_explorer(result, report_data_control(max_pair_rows = 7, seed = 12))
  for (stage in c("raw", "processed")) {
    expect_identical(sampled$profile$stages[[stage]]$columns, full$profile$stages[[stage]]$columns)
    pairs <- sampled$profile$stages[[stage]]$pairs
    expect_true(all(vapply(pairs, function(pair) pair$training$n_total == 7L, logical(1))))
    expect_true(all(vapply(pairs, function(pair) pair$training$n_population == 20L, logical(1))))
    expect_true(all(vapply(pairs, function(pair) pair$evaluation$n_sample == 6L, logical(1))))
    expect_true(all(vapply(pairs, function(pair) pair$training$sampled && !pair$evaluation$sampled, logical(1))))
  }
  expect_identical(sampled$profile$stages$raw$columns$x$training$n_missing, 1L)
  expect_equal(sampled$profile$stages$raw$columns$x$training$quantiles$median, 11)
  expect_null(sampled$rows)
  expect_identical(result$training_data, scaling_profile_fixture()$training_data)
})

test_that("the sample is shared by pairs and follows retained source rows across views", {
  result <- scaling_profile_fixture("drop_rows")
  raw <- result$data_context$raw
  processed <- list(training = result$training_data, evaluation = result$test_data)
  map <- result$data_context$row_map
  set.seed(29)
  rng <- .Random.seed
  indices <- AutoXplainR:::data_pair_samples(raw, processed, map, 3L, 17L)
  expect_identical(.Random.seed, rng)
  expect_identical(indices, AutoXplainR:::data_pair_samples(raw, processed, map, 3L, 17L))
  for (partition in c("training", "evaluation")) {
    mapped <- map$processed_position[map$partition == partition][indices$raw[[partition]]]
    expect_true(all(stats::na.omit(mapped) %in% indices$processed[[partition]]))
    expect_length(indices$processed[[partition]], 3L)
    expect_false(anyDuplicated(indices$processed[[partition]]) > 0L)
  }
  export <- AutoXplainR:::prepare_data_explorer(result, report_data_control(max_pair_rows = 3L, seed = 17L))
  # Compare the sampled summary against direct complete-pair statistics from
  # source positions, including rows excluded by preprocessing.
  x <- raw$training$x[indices$raw$training]
  y <- raw$training$y[indices$raw$training]
  ok <- is.finite(x) & is.finite(y)
  pair <- export$profile$stages$raw$pairs[["1_2"]]$training
  expect_equal(pair$n_complete, sum(ok))
  expect_equal(sum(pair$cells$n), sum(ok))
  if (sum(ok) >= 3L) expect_equal(pair$association$value, stats::cor(x[ok], y[ok], method = "spearman"))
  expect_identical(.Random.seed, rng)
})

test_that("full relationship mode includes all observations and bounded row export stays independent", {
  result <- scaling_profile_fixture()
  rows <- AutoXplainR:::prepare_data_explorer(
    result, report_data_control("rows", max_rows = 4L, max_pair_rows = NULL, seed = 9L)
  )
  expect_length(rows$rows, 4L)
  expect_identical(rows$profile$stages$raw$pairs[["1_2"]]$training$n_sample, 20L)
  expect_false(rows$profile$stages$raw$pairs[["1_2"]]$training$sampled)
  expected <- AutoXplainR:::data_pair_profile(
    result$data_context$raw$training$x,
    result$data_context$raw$training$y, rows$profile$stages$raw$columns$x$axis,
    rows$profile$stages$raw$columns$y$axis, 20L
  )
  actual <- rows$profile$stages$raw$pairs[["1_2"]]$training
  expect_identical(actual[names(expected)], expected)
  old <- unclass(report_data_control("summary"))
  old$max_pair_rows <- NULL
  class(old) <- "autoxplain_report_data_control"
  expect_identical(AutoXplainR:::normalize_report_data_control(old)$max_pair_rows, 10000L)
  expect_null(AutoXplainR:::normalize_report_data_control(report_data_control(max_pair_rows = NULL))$max_pair_rows)
  expect_error(report_data_control(max_pair_rows = 0), "max_pair_rows")
  expect_error(report_data_control(max_pair_rows = Inf), "max_pair_rows")
})

test_that("sampling cannot hide rare categories or missing values in univariate summaries", {
  raw <- data.frame(x = seq_len(10001), y = seq_len(10001),
                    category = c(rep("common", 9999), "rare", NA))
  columns <- data.frame(name = names(raw))
  partitions <- list(training = raw, evaluation = NULL)
  profile <- AutoXplainR:::build_data_profile(list(), partitions, NULL, columns, "y", seed = 42L)
  distribution <- profile$stages$processed$columns$category$training
  expect_identical(distribution$n_total, 10001L)
  expect_identical(distribution$n_missing, 1L)
  expect_identical(distribution$counts, c(9999L, 1L, 0L, 0L))
  expect_identical(profile$stages$processed$pairs[["1_2"]]$training$n_sample, 10000L)
  expect_true(profile$stages$processed$pairs[["1_2"]]$training$sampled)
})

test_that("direct column exports preserve every record, exclusion and nonfinite flag", {
  result <- scaling_profile_fixture("drop_rows")
  for (limit in c(4L, 26L)) {
    control <- report_data_control("rows", max_rows = limit, context_columns = "private_id")
    records <- AutoXplainR:::prepare_data_explorer(result, control)
    columns <- AutoXplainR:::prepare_data_explorer(result, control, row_layout = "columns")
    expect_identical(columns$manifest, records$manifest)
    expect_identical(columns$profile, records$profile)
    expect_identical(columns$rows$length, length(records$rows))
    restored <- lapply(seq_len(columns$rows$length), function(i) {
      row <- lapply(columns$rows$meta, `[`, i)
      row$nonfinite <- lapply(columns$rows$nonfinite, function(stage) {
        names(stage)[vapply(stage, function(indices) i %in% indices, logical(1))]
      })
      for (stage in c("raw", "processed")) {
        row[stage] <- list(
          if (is.null(columns$rows[[stage]])) NULL else lapply(columns$rows[[stage]], function(column) {
            value <- column[[i]]
            if (is.na(value)) NULL else unname(value)
          })
        )
      }
      row
    })
    expect_identical(restored, records$rows)
  }
  numeric <- data.frame(x = c(1, Inf, NA, -Inf), y = c(1, 2, 3, 4))
  export <- AutoXplainR:::data_export_columns(numeric, c(4L, NA_integer_, 2L, 1L), c("x", "y"))
  expect_identical(export$nonfinite$x, c(1L, 3L))
  expect_equal(export$values$x, c(NA, NA, NA, 1))
  expect_equal(export$values$y, c(4, NA, 2, 1))
})

test_that("column exports preserve mixed partition types and absent training", {
  combined <- AutoXplainR:::data_combine_export_columns(list(
    AutoXplainR:::data_export_columns(data.frame(x = c(1, Inf)), 1:2, "x"),
    AutoXplainR:::data_export_columns(data.frame(x = c("one", NA_character_)), 1:2, "x")
  ), "x", c(2L, 2L))
  expect_identical(combined$values$x, list(1, NA_real_, "one", NA_character_))
  expect_equal(combined$nonfinite$x, 2L)
  result <- scaling_profile_fixture()
  result$data_context <- NULL
  result$training_data <- NULL
  columns <- AutoXplainR:::prepare_data_explorer(result, "rows", row_layout = "columns")
  expect_identical(columns$rows$meta$row_key, paste0("evaluation:", 1:6))
  expect_true(all(lengths(columns$rows$meta) == 6L))
  expect_null(columns$rows$raw)
  expect_identical(columns$manifest$individual_records, 6L)
})

test_that("identifier categories cannot masquerade as perfect pair associations", {
  id <- paste0("person", 1:6)
  unique_numeric <- AutoXplainR:::data_pair_association(id, c(8, 1, 3, 2, 7, 4))
  unique_category <- AutoXplainR:::data_pair_association(id, c("a", "a", "b", "b", "c", "c"))
  for (association in list(unique_numeric, unique_category)) {
    expect_identical(association$status, "unavailable")
    expect_match(association$reason, "no repeated categories", fixed = TRUE)
    expect_true(is.na(association$value))
    expect_identical(association$categorical$x$n_categories, 6L)
    expect_equal(association$categorical$x$singleton_fraction, 1)
  }
  partial <- AutoXplainR:::data_pair_association(c("a", "b", "b", "c", "d", "d"), c(8, 1, 3, 2, 7, 4))
  expect_identical(partial$status, "available")
  expect_identical(partial$categorical$x, list(
    n_categories = 4L, n_singleton_rows = 2L,
    singleton_fraction = 1 / 3, n_replicated_rows = 4L, n_repeated_categories = 2L
  ))
  expect_null(partial$categorical$y)
  expect_identical(AutoXplainR:::data_pair_association(1:6, 6:1)$status, "available")
  empty <- AutoXplainR:::categorical_replication_summary(c(NA, NA))
  expect_identical(empty$n_categories, 0L)
  expect_equal(empty$singleton_fraction, 0)
})
