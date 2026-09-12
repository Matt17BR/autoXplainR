test_that("neural budgets are explicit and retain legacy initialization seeds", {
  legacy <- tuning_control(grids = list(neural = list(size = 2L, decay = .03)))
  expect_identical(legacy$grids$neural[[1L]]$maxit, 2000L)
  expect_error(tuning_control(grids = list(neural = list(size = 2L, decay = .03, maxit = 0))), "maxit")
  expect_error(tuning_control(grids = list(neural = list(size = 2L, decay = .03, maxit = 1.5))), "maxit")
  parameters <- list(size = 2L, decay = .03)
  old_seed <- AutoXplainR:::stable_configuration_seed(824L, "neural", parameters)
  expect_identical(old_seed, 906524673L)
  for (limit in c(1L, 500L, 2000L)) {
    expect_identical(AutoXplainR:::stable_configuration_seed(
      824L, "neural", c(parameters, list(maxit = limit))
    ), old_seed)
  }
})

test_that("compact neural calls reproduce native fits for every outcome shape", {
  set.seed(844)
  data <- data.frame(x = rnorm(100), z = runif(100), group = factor(rep(letters[1:4], 25)))
  outcome <- data$x + sin(data$z * 5) + rnorm(100, sd = .2)
  for (task in c("regression", "binary", "multiclass")) {
    data$y <- switch(task,
      regression = outcome,
      binary = factor(ifelse(outcome > median(outcome), "yes", "no")),
      multiclass = factor(rep(c("a", "b", "c", "d"), 25))
    )
    fit <- withr::with_seed(190L, AutoXplainR:::fit_tuned_neural_network(
      data, "y", task,
      size = 3L, decay = .03, maxit = 40L
    ))
    expect_identical(fit$model$call$x, quote(x))
    expect_identical(fit$model$call$y, quote(y))
    expect_identical(fit$model$call$maxit, 40L)
    expect_identical(fit$maxit, 40L)
    x <- AutoXplainR:::bake_matrix_blueprint(fit$blueprint, data)
    y <- switch(task,
      regression = (data$y - fit$y_center) / fit$y_scale,
      binary = as.numeric(data$y == fit$class_levels[[2L]]),
      multiclass = nnet::class.ind(data$y)
    )
    native <- withr::with_seed(190L, eval(fit$model$call))
    fields <- setdiff(names(native), "call")
    expect_identical(fit$model[fields], native[fields])
    expected <- predict(native, x, type = "raw")
    expected <- switch(task,
      regression = as.numeric(expected) * fit$y_scale + fit$y_center,
      binary = as.numeric(expected),
      multiclass = {
        colnames(expected) <- fit$class_levels
        expected
      }
    )
    expect_equal(predict(fit, data), expected, tolerance = 1e-12)
    saved <- tempfile(fileext = ".rds")
    saveRDS(fit, saved)
    expect_identical(predict(readRDS(saved), data), predict(fit, data))
    unlink(saved)
    expect_match(fit$call_reconstruction, "fit_seed", fixed = TRUE)
  }
})

test_that("the public workflow records the requested neural optimizer budget", {
  set.seed(53)
  data <- data.frame(x = rnorm(100), z = rnorm(100), y = rnorm(100))
  control <- tuning_control(
    grids = list(neural = list(size = 3L, decay = .01, maxit = 1L)),
    optimization_policy = "warn"
  )
  result <- autoxplain(data[1:80, ], "y",
    test_data = data[81:100, ],
    learners = "neural", max_models = 1L, nfolds = 2L,
    tuning_control = control, seed = 53L, explain = FALSE
  )
  expect_true(all(vapply(
    result$tuning$fold_scores$learned,
    function(settings) identical(settings$maxit, 1L), logical(1)
  )))
  expect_true(all(result$tuning$fold_scores$optimization_status == "not_converged"))
  model <- result$models[[result$provenance$primary_model_id]]
  expect_identical(model$maxit, 1L)
  expect_identical(model$model$call$maxit, 1L)
  expect_match(AutoXplainR:::render_model_selection(result), "iteration limit = 1", fixed = TRUE)
  specifications <- AutoXplainR:::model_specification(result, result$provenance$primary_model_id)
  expect_match(specifications$summary, "at most 1 iteration", fixed = TRUE)
})
