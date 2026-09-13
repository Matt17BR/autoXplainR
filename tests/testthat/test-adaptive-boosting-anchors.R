test_that("allocated adaptive search includes matched depth anchors within the existing budget", {
  local_mocked_bindings(
    learner_is_available = function(definition) TRUE, .package = "AutoXplainR"
  )
  cases <- list(
    list(
      learners = c("forest", "boosting"), n = 50000L, p = 54L,
      task = "multiclass", classes = 7L, budget = 10L
    ),
    list(
      learners = c("forest", "boosting"), n = 50000L, p = 90L,
      task = "regression", classes = 1L, budget = 10L
    ),
    list(
      learners = c("regularized", "forest", "boosting"), n = 233L, p = 4L,
      task = "binary", classes = 2L, budget = 18L
    )
  )
  reference_anchors <- NULL
  for (case in cases) {
    grids <- adaptive_parameter_grids(
      case$learners, case$n, case$p, case$task, case$classes, 71L, case$budget
    )
    plan <- local_tuning_plan(
      case$budget, case$n, case$p, case$task, case$classes,
      learners = case$learners, seed = 71L, custom_grids = grids
    )
    expect_equal(nrow(plan), case$budget)
    expect_false(anyDuplicated(plan$configuration_id) > 0L)
    parameters <- plan$parameters[plan$family == "boosting"]
    expect_length(parameters, case$budget / length(case$learners))
    anchors <- lapply(parameters[1:3], identity)
    expect_identical(vapply(anchors, `[[`, integer(1), "max_depth"), c(3L, 6L, 10L))
    # Depth is the only varying anchor control, so regularization and learning
    # rate cannot masquerade as the effect of deeper interaction capacity.
    without_depth <- lapply(anchors, function(parameters) parameters[setdiff(names(parameters), "max_depth")])
    expect_identical(without_depth[[1L]], without_depth[[2L]])
    expect_identical(without_depth[[2L]], without_depth[[3L]])
    expect_identical(anchors[[2L]]$eta, .05)
    expect_identical(anchors[[2L]]$min_child_weight, 1)
    expect_identical(anchors[[2L]]$reg_lambda, 1)
    expect_identical(anchors[[2L]]$reg_alpha, 0)
    expect_identical(anchors[[2L]]$subsample, .8)
    expect_identical(anchors[[2L]]$colsample_bytree, .8)
    expect_true(all(vapply(parameters, `[[`, integer(1), "nrounds") == 2000L))
    screened <- lapply(parameters, adaptive_screen_parameters, family = "boosting")
    expect_true(all(vapply(screened, `[[`, integer(1), "nrounds") == 600L))
    expect_true(all(vapply(parameters, `[[`, integer(1), "nrounds") == 2000L))
    if (!is.null(reference_anchors)) expect_identical(anchors, reference_anchors)
    reference_anchors <- anchors
  }
})

test_that("matched anchors do not displace seeded diversity or expand small search budgets", {
  for (budget in c(2L, 3L, 5L, 9L)) {
    first <- adaptive_parameter_grids("boosting", 1000L, 4L, "regression", 1L, 71L, budget)$boosting
    other <- adaptive_parameter_grids("boosting", 1000L, 4L, "regression", 1L, 72L, budget)$boosting
    expect_length(first, budget)
    expect_false(anyDuplicated(vapply(first, canonical_tuning_parameter_key, character(1))) > 0L)
    anchor_count <- min(budget, 3L)
    expect_identical(first[seq_len(anchor_count)], other[seq_len(anchor_count)])
    if (budget > 3L) {
      expect_false(identical(first[4:budget], other[4:budget]))
      expect_true(any(vapply(first[4:budget], `[[`, numeric(1), "eta") != .05))
      expect_true(any(vapply(first[4:budget], `[[`, numeric(1), "min_child_weight") != 1))
      expect_true(any(vapply(first[4:budget], `[[`, numeric(1), "reg_lambda") != 1))
    }
  }
})

test_that("matched medium and deep anchors reach native regression and probability models unchanged", {
  skip_if_package_unavailable("xgboost")
  grid <- adaptive_parameter_grids("boosting", 50000L, 54L, "multiclass", 7L, 71L, 5L)$boosting
  data <- with_preserved_seed(407L, {
    data.frame(x = rnorm(240), z = runif(240), u = rnorm(240))
  })
  for (task in c("regression", "binary", "multiclass")) {
    labels <- if (task == "binary") c("zebra", "ant") else c("zebra", "ant", "mole")
    data$y <- if (task == "regression") {
      sin(data$x) + data$z * data$u
    } else {
      factor(rep(labels, length.out = nrow(data)), levels = labels)
    }
    for (index in 2:3) {
      parameters <- grid[[index]]
      # Native checks use an eight-round prefix on 240 rows; search coverage
      # and its unchanged 600/2000 caps are asserted separately above.
      parameters$nrounds <- 8L
      parameters$encoding <- "matrix"
      plan <- local_tuning_plan(
        1L, nrow(data), 3L, task, if (task == "regression") 1L else length(labels),
        learners = "boosting", seed = 809L, custom_grids = list(boosting = list(parameters))
      )
      fitted <- fit_tuning_configuration(plan, data, "y", task)
      record <- attr(fitted, "autoxplain_tuning_fit")
      native_parameters <- list(
        objective = switch(task,
          regression = "reg:squarederror", binary = "binary:logistic", multiclass = "multi:softprob"
        ),
        eval_metric = switch(task, regression = "rmse", binary = "logloss", multiclass = "mlogloss"),
        eta = .05, max_depth = c(6L, 10L)[[index - 1L]], min_child_weight = 1,
        subsample = .8, colsample_bytree = .8, alpha = 0, lambda = 1,
        seed = record$fit_seed, nthread = 1L, verbosity = 0L
      )
      if (task == "multiclass") native_parameters$num_class <- length(labels)
      y <- if (task == "regression") data$y else as.integer(data$y) - 1L
      matrix <- xgboost::xgb.DMatrix(as.matrix(data[c("x", "z", "u")]), label = y, nthread = 1L)
      reference <- xgboost::xgb.train(native_parameters, matrix, nrounds = 8L, verbose = 0L)
      expected <- predict(reference, as.matrix(data[c("x", "z", "u")]))
      if (task == "multiclass") colnames(expected) <- labels
      expect_equal(predict(fitted, data), expected, tolerance = 0, ignore_attr = TRUE)
      expect_identical(record$requested_parameters, parameters)
      expect_identical(record$effective_parameters, parameters)
      if (task == "multiclass") expect_identical(colnames(predict(fitted, data)), labels)
      config <- xgboost::xgb.config(fitted$fit)$learner$gradient_booster$tree_train_param
      expect_equal(as.integer(config$max_depth), native_parameters$max_depth)
      expect_equal(as.numeric(config$min_child_weight), 1)
      expect_equal(as.numeric(config$lambda), 1)
      expect_equal(as.numeric(config$alpha), 0)
      expect_equal(as.numeric(config$eta), .05, tolerance = 1e-7)
    }
  }
})
