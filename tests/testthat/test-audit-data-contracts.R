test_that("subgroups retain raw categories and context excluded from model inputs", {
  training <- data.frame(
    x = seq_len(60), region = rep(c("a", "b"), 30),
    patient_id = seq_len(60), y = sin(seq_len(60))
  )
  evaluation <- data.frame(
    x = 61:68, region = c("a", "new", NA, "b", "new", "a", "b", NA),
    patient_id = rep(c("site-1", "site-2"), 4), y = cos(61:68),
    context_only = rep(c("east", "west"), 4)
  )
  result <- autoxplain(model_set = "quick", training, "y", test_data = evaluation, explain = FALSE,
                       preprocessing_config = list(enable_id_removal = TRUE))
  expect_false("patient_id" %in% result$features)
  expect_false("context_only" %in% names(result$test_data))
  expect_false("y" %in% names(result$evaluation_context))
  expect_identical(result$evaluation_context$region, evaluation$region)
  expect_identical(result$evaluation_row_indices, seq_len(nrow(evaluation)))
  diagnostic <- subgroup_performance(result, "region", min_rows = 1)
  expect_setequal(diagnostic$performance$group, c("a", "b", "new", "(missing)"))
  expect_equal(diagnostic$performance$rows, rep(2L, 4))
  predicted <- predict(result, evaluation)
  new_rows <- which(evaluation$region == "new")
  expect_equal(diagnostic$performance$rmse[diagnostic$performance$group == "new"],
               sqrt(mean((predicted[new_rows] - evaluation$y[new_rows])^2)))
  expect_equal(subgroup_performance(result, "patient_id")$n_groups, 2L)
  expect_equal(subgroup_performance(result, "context_only")$n_groups, 2L)
  damaged <- result
  damaged$evaluation_context <- damaged$evaluation_context[8:1, , drop = FALSE]
  expect_error(subgroup_performance(damaged, "region"), "not aligned")
})

test_that("row filtering aligns raw subgroup values without filtering on extra context", {
  training <- data.frame(x = 1:60, y = sin(1:60))
  evaluation <- data.frame(x = c(61, NA, 63, 64, 65, 66), y = cos(61:66),
                           site = c("a", "discarded", "b", NA, "a", "b"))
  result <- autoxplain(model_set = "quick", training, "y", test_data = evaluation, explain = FALSE,
                       preprocessing_config = list(missing_value_strategy = "drop_rows"))
  expect_identical(result$evaluation_row_indices, c(1L, 3L, 4L, 5L, 6L))
  expect_identical(result$evaluation_context$site, evaluation$site[c(1, 3, 4, 5, 6)])
  expect_identical(rownames(result$evaluation_context), rownames(result$test_data))
  diagnostic <- subgroup_performance(result, "site", min_rows = 1)
  expect_equal(sum(diagnostic$performance$rows), 5L)
  expect_false("discarded" %in% diagnostic$performance$group)
  expect_true("(missing)" %in% diagnostic$performance$group)
})

test_that("H2O internal CV rejects learned external transforms before engine work", {
  validate <- AutoXplainR:::validate_h2o_preprocessing_contract
  for (strategy in c("impute", "drop_columns")) {
    expect_error(validate(TRUE, list(missing_value_strategy = strategy), 5L),
                 "learned external preprocessing")
    expect_silent(validate(TRUE, list(missing_value_strategy = strategy), 0L))
  }
  expect_error(validate(TRUE, list(enable_ordinal_factors = TRUE), 2L), "ordinal")
  expect_silent(validate(FALSE, list(missing_value_strategy = "impute"), 5L))
  expect_silent(validate(TRUE, list(missing_value_strategy = "keep"), 5L))
  expect_silent(validate(TRUE, list(missing_value_strategy = "drop_rows"), 5L))
  local_mocked_bindings(ensure_h2o_connection = function(...) stop("engine must not be reached"),
                        .package = "AutoXplainR")
  expect_error(autoxplain(mtcars, "mpg", engine = "h2o", explain = FALSE,
                          preprocessing_config = list(missing_value_strategy = "impute")),
               "learned external preprocessing")
})

test_that("structured split context remains aligned after dropping model-input rows", {
  withr::local_seed(45)
  data <- data.frame(site = rep(paste0("site-", 1:8), each = 10),
                     x = rnorm(80), y = rnorm(80))
  data$x[seq(1, 80, by = 10)] <- NA_real_
  result <- autoxplain(model_set = "quick", data, "y", validation = validation_split(group = "site"),
                       test_fraction = 0.25, explain = FALSE,
                       preprocessing_config = list(missing_value_strategy = "drop_rows"))
  expect_false("site" %in% result$features)
  expect_identical(result$evaluation_context$site,
                   data$site[result$validation$evaluation_rows])
  expect_identical(result$evaluation_context$site, result$validation$evaluation_groups)
  diagnostic <- subgroup_performance(result, "site", min_rows = 1)
  expect_equal(diagnostic$performance$rows, c(9L, 9L))
  expect_equal(diagnostic$rows, nrow(result$test_data))
})

test_that("H2O outer preparation returns aligned raw context", {
  training <- data.frame(x = 1:30, y = sin(1:30))
  evaluation <- data.frame(x = c(31, NA, 33, 34), y = cos(31:34),
                           site = c("a", "drop", "b", NA))
  prepared <- AutoXplainR:::prepare_h2o_outer_split(
    training, evaluation, "y", "regression", 0.2, 3L, TRUE,
    list(missing_value_strategy = "drop_rows", novel_level_strategy = "mode")
  )
  expect_identical(prepared$evaluation_row_indices, c(1L, 3L, 4L))
  expect_identical(prepared$evaluation_context$site, c("a", "b", NA_character_))
  expect_identical(rownames(prepared$evaluation_context), rownames(prepared$evaluation$data))
})

test_that("preprocessing preserves ordered factors and exact retained row positions", {
  data <- data.frame(
    rank = ordered(c("low", "medium", "high", NA), levels = c("low", "medium", "high")),
    y = 1:4
  )
  prepared <- preprocess_data(data, "y", missing_value_strategy = "impute")
  baked <- AutoXplainR:::apply_preprocessing_recipe(data, prepared$recipe, "y")
  expect_true(is.ordered(prepared$data$rank))
  expect_true(is.ordered(baked$data$rank))
  expect_identical(prepared$data$rank, baked$data$rank)
  dropped <- preprocess_data(data, "y", missing_value_strategy = "drop_rows")
  expect_identical(dropped$row_indices, 1:3)
  logical_data <- data.frame(flag = c(TRUE, FALSE, TRUE, NA), y = 1:4)
  logical_prepared <- preprocess_data(logical_data, "y", missing_value_strategy = "impute")
  expect_identical(logical_prepared$data$flag, c(TRUE, FALSE, TRUE, TRUE))
  expect_identical(AutoXplainR:::apply_preprocessing_recipe(
    logical_data, logical_prepared$recipe, "y"
  )$data$flag, logical_prepared$data$flag)
})

test_that("neural encoding survives ordered inputs, changed contrasts, and serialization", {
  withr::local_seed(43)
  data <- data.frame(
    rank = ordered(rep(c("low", "medium", "high"), 20),
                   levels = c("low", "medium", "high")),
    category = factor(rep(c("a", "b"), 30)), x = rnorm(60), y = rnorm(60)
  )
  result <- autoxplain(data, "y", model_set = "tuned", learners = "neural",
                       max_models = 1, nfolds = 2, explain = FALSE)
  model <- result$models$main_model
  expect_s3_class(model$blueprint, "autoxplain_matrix_blueprint")
  expected <- predict(result, data[1:4, ])
  withr::local_options(contrasts = c("contr.sum", "contr.helmert"))
  restored <- unserialize(serialize(result, NULL))
  expect_equal(predict(restored, data[1:4, ]), expected)
  expect_equal(predict(restored, data[1, , drop = FALSE]), expected[1])
  expect_true(is.ordered(restored$test_data$rank))
  for (task in c("binary", "multiclass")) {
    classification <- data
    classification$y <- if (task == "binary") {
      factor(rep(c("no", "yes"), 30))
    } else {
      factor(rep(c("a", "b", "c"), 20))
    }
    fitted <- AutoXplainR:::fit_tuned_neural_network(classification, "y", task, 2L, 0.1)
    probability <- predict(fitted, classification[1, , drop = FALSE])
    expect_true(all(is.finite(probability)))
    if (task == "multiclass") expect_equal(dim(probability), c(1L, 3L))
  }
})

test_that("exported formula models retain no fitting environment or sibling models", {
  withr::local_seed(44)
  data <- as.data.frame(matrix(rnorm(300 * 10), nrow = 300))
  data$patient_id <- paste0("private-", seq_len(300))
  data$y <- data$V1 + rnorm(300)
  result <- autoxplain(model_set = "quick", data, "y", explain = FALSE,
                       preprocessing_config = list(enable_id_removal = TRUE))
  model <- result$models$simple_baseline
  expect_identical(environment(stats::formula(model)), baseenv())
  expect_identical(environment(attr(model$model, "terms")), baseenv())
  expect_false("patient_id" %in% names(model$model))
  archive <- serialize(model, NULL)
  expect_lt(length(archive), 50000L)
  restored <- unserialize(archive)
  expect_equal(predict(restored, data[1:3, ]), predict(model, data[1:3, ]))
})
