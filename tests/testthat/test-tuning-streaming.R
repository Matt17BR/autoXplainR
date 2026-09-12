test_that("single-loss fold scoring matches evaluation at probability boundaries", {
  loss <- AutoXplainR:::tuning_prediction_loss
  evaluate <- AutoXplainR:::evaluate_predictions
  cases <- list(
    list(
      task = "regression", observed = c(-10, 0, 1, 8),
      predicted = c(-8, 0, 2, 4), metrics = c("rmse", "mae")
    ),
    list(
      task = "binary", observed = factor(c("yes", "no", "yes", "no"), c("no", "yes")),
      predicted = c(0, 1, 1e-18, 1 - 1e-14), metrics = c("log_loss", "brier_score")
    ),
    list(
      task = "multiclass", observed = factor(c("a", "b", "c", "a"), c("a", "b", "c")),
      predicted = cbind(c = c(0, 0, 1, .25), a = c(0, .5, 0, .5), b = c(1, .5, 0, .25)),
      metrics = c("log_loss", "brier_score")
    )
  )
  for (case in cases) {
    contract <- list(task = case$task, class_levels = levels(case$observed), positive = "yes")
    expected <- evaluate(case$observed, case$predicted, contract)
    for (metric in case$metrics) {
      expect_identical(loss(case$observed, case$predicted, metric, contract), unname(expected[[metric]]))
    }
  }
  expect_error(loss(
    factor(c("no", "yes")), factor(c("yes", "no")), "log_loss",
    list(task = "binary", positive = "yes")
  ), "probabilities")
})

test_that("streamed stochastic folds preserve paired records and OOF retention is optional", {
  set.seed(873)
  data <- data.frame(x = rnorm(90), z = rnorm(90))
  data$y <- sin(data$x) + data$z + rnorm(90, sd = .2)
  evaluation <- data.frame(x = c(-.41, .73, 1.14), z = c(.82, -.63, -.55), y = c(.4, .1, .8))
  fold_ids <- rep(c("west", "north", "east"), c(15, 30, 45))
  fit <- function(retain) {
    autoxplain(data, "y",
      test_data = evaluation,
      learners = c("linear", "tree", "neural"), max_models = 4, explain = FALSE,
      tuning_control = tuning_control(fold_ids = fold_ids, retain_oof = retain), seed = 41
    )
  }
  rng <- .Random.seed
  retained <- fit(TRUE)
  expect_identical(.Random.seed, rng)
  compact <- fit(FALSE)
  expect_identical(.Random.seed, rng)
  fields <- setdiff(names(retained$tuning$fold_scores), "elapsed_ms")
  expect_equal(retained$tuning$fold_scores[fields], compact$tuning$fold_scores[fields])
  expect_identical(retained$tuning$selected_configuration, compact$tuning$selected_configuration)
  expect_equal(predict(retained, evaluation), predict(compact, evaluation), tolerance = 0)
  expect_null(compact$tuning$out_of_fold_predictions)
  records <- retained$tuning$out_of_fold_predictions
  for (id in unique(records$configuration_id)) {
    values <- records[records$configuration_id == id, ]
    expect_setequal(values$training_row, seq_len(nrow(data)))
    expect_identical(values$fold, match(fold_ids[values$training_row], unique(fold_ids)))
    expect_equal(values$case_loss, (data$y[values$training_row] - values$estimate)^2)
  }
})

test_that("a changed computation method is explicit in refit evidence", {
  folds <- data.frame(configuration_id = c("a", "a", "b"), score = c(.1, .2, .3))
  folds$effective_parameters <- I(list(list(encoding = "matrix"), list(encoding = "native"), list(encoding = "matrix")))
  message <- AutoXplainR:::tuning_computation_change(list(fold_scores = folds), "a", list(encoding = "native"))
  expect_match(message, "matrix/native in CV; native at refit", fixed = TRUE)
  expect_match(message, "smaller training folds", fixed = TRUE)
  unchanged <- AutoXplainR:::tuning_computation_change(list(fold_scores = folds), "b", list(encoding = "matrix"))
  expect_identical(unchanged, "")
})
