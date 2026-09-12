test_that("automatic boosting input policy is based on expansion rather than outcomes", {
  data <- data.frame(x = rep(1, 60000), group = factor(rep(seq_len(1000), 60)), y = 1)
  resolve <- AutoXplainR:::resolve_boosting_encoding
  expect_identical(resolve(list(), data, "y")$encoding, "native")
  data$y <- rev(seq_len(nrow(data)))
  expect_identical(resolve(list(), data, "y")$encoding, "native")
  expect_identical(resolve(list(encoding = "matrix"), data, "y")$encoding, "matrix")
  expect_identical(resolve(list(), data[1:1000, ], "y")$encoding, "matrix")
  parameters <- AutoXplainR:::boosting_learner_grid(100, 2, "regression", 1)[[1]]
  original_seed <- AutoXplainR:::stable_configuration_seed(71, "boosting", parameters)
  parameters$encoding <- "matrix"
  expect_identical(AutoXplainR:::stable_configuration_seed(71, "boosting", parameters), original_seed)
  parameters$encoding <- "native"
  expect_false(identical(AutoXplainR:::stable_configuration_seed(71, "boosting", parameters), original_seed))
})

test_that("native prediction rejects changed predictor types before coercion", {
  data <- data.frame(x = c(0, 1, 2), flag = c(TRUE, FALSE, TRUE), group = factor(c("a", "b", "a")))
  blueprint <- AutoXplainR:::fit_boosting_native_blueprint(data, names(data))
  for (replacement in list(
    factor(c("0", "1", "2")), c("0", "1", "2"), 1:3 + 0i,
    I(matrix(1:6, nrow = 3))
  )) {
    changed <- data
    changed$x <- replacement
    expect_error(AutoXplainR:::bake_boosting_native_blueprint(blueprint, changed), "types changed: x")
  }
  changed <- data
  changed$flag <- as.numeric(changed$flag)
  expect_error(AutoXplainR:::bake_boosting_native_blueprint(blueprint, changed), "types changed: flag")
  changed <- data
  changed$group <- as.character(changed$group)
  expect_identical(
    AutoXplainR:::bake_boosting_native_blueprint(blueprint, changed),
    AutoXplainR:::bake_boosting_native_blueprint(blueprint, data)
  )
})

test_that("automatic native representation stays fixed across smaller folds and refit", {
  skip_if_not_installed("xgboost")
  set.seed(944)
  data <- data.frame(x = rnorm(16000), group = factor(rep(seq_len(4000), 4)))
  data$y <- data$x^2 + rnorm(nrow(data), sd = .2)
  evaluation <- data[1:20, ]
  evaluation$x <- evaluation$x + .0123
  parameters <- AutoXplainR:::boosting_learner_grid(nrow(data), 2, "regression", 1)[[1]]
  parameters$nrounds <- 2L
  parameters$encoding <- "auto"
  result <- autoxplain(data, "y",
    test_data = evaluation, learners = "boosting", nfolds = 2,
    explain = FALSE, tuning_control = tuning_control(
      grids = list(boosting = parameters),
      family_budgets = c(boosting = 1L), retain_oof = FALSE
    )
  )
  expect_identical(result$tuning$input_policy$boosting$requested, "auto")
  expect_identical(result$tuning$input_policy$boosting$encoding, "native")
  expect_true(all(result$tuning$fold_scores$training_rows == 8000))
  expect_true(all(vapply(
    result$tuning$fold_scores$effective_parameters,
    function(x) identical(x$encoding, "native"), logical(1)
  )))
  expect_identical(result$models$main_model$parameters$encoding, "native")
  expect_equal(result$models$main_model$blueprint$training_rows, nrow(data))
  expect_null(attr(result$models$main_model, "autoxplain_tuning_fit")$computation_change)
  evidence <- tuning_evidence(result)
  expect_identical(evidence$input_policy, result$tuning$input_policy)
  expect_match(AutoXplainR:::selection_input_policy(evidence), "fixed across all folds and refits")
})

test_that("native category fits preserve factor identity, zeros and reload predictions", {
  skip_if_not_installed("xgboost")
  set.seed(819)
  data <- data.frame(x = c(rep(0, 30), rnorm(90)), group = factor(rep(letters[1:4], 30)))
  names(data) <- c("price[USD]", "segment ` name")
  values <- data[[1]]^2 + as.integer(data[[2]])
  for (task in c("regression", "binary", "multiclass")) {
    data$y <- switch(task,
      regression = values,
      binary = factor(rep(c("no", "yes"), 60)),
      multiclass = factor(rep(c("a", "b", "c"), 40))
    )
    parameters <- AutoXplainR:::boosting_learner_grid(nrow(data), 2, task, 3)[[1]]
    parameters$encoding <- "native"
    parameters$nrounds <- 10L
    model <- AutoXplainR:::fit_boosting_learner(data, "y", task, parameters, 15L)
    prepared <- AutoXplainR:::bake_boosting_native_blueprint(model$blueprint, data)
    expect_identical(prepared[[1]], data[[1]])
    expect_identical(ncol(prepared), 2L)
    reordered <- data
    reordered[[2]] <- factor(as.character(data[[2]]), levels = rev(levels(data[[2]])))
    expect_equal(predict(model, data), predict(model, reordered), tolerance = 0)
    expect_equal(predict(model, data), stats::predict(model$fit, prepared), ignore_attr = TRUE)
    path <- tempfile(fileext = ".rds")
    saveRDS(model, path)
    expect_equal(predict(readRDS(path), data), predict(model, data), tolerance = 0)
    unlink(path)
    expect_identical(model$fit_details$computation$matrix_type, "QuantileDMatrix")
    invalid <- data
    invalid[[2]] <- as.character(invalid[[2]])
    invalid[[2]][1] <- "unseen"
    expect_error(predict(model, invalid), "unseen levels")
  }
})

test_that("native boosting is usable through the ordinary guided search", {
  skip_if_not_installed("xgboost")
  set.seed(739)
  data <- data.frame(x = rnorm(80), category = factor(rep(letters[1:4], 20)))
  data$y <- data$x^2 + as.integer(data$category)
  parameters <- AutoXplainR:::boosting_learner_grid(80, 2, "regression", 1)[[1]]
  parameters$encoding <- "native"
  parameters$nrounds <- 10L
  result <- autoxplain(data, "y",
    learners = "boosting", nfolds = 2, explain = FALSE,
    tuning_control = tuning_control(grids = list(boosting = parameters), family_budgets = c(boosting = 1L))
  )
  expect_identical(result$tuning$candidates$status, "ok")
  expect_identical(result$models$main_model$parameters$encoding, "native")
  expect_true(all(vapply(
    result$tuning$fold_scores$effective_parameters,
    function(x) identical(x$encoding, "native"), logical(1)
  )))
  unknown <- data[1:2, ]
  unknown$category <- c("never seen", "b")
  expect_true(all(is.finite(predict(result, unknown))))
  expect_match(AutoXplainR:::model_specification(result, "main_model")$summary, "native")
})
