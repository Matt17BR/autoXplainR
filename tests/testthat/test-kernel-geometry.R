test_that("one-hot matrix geometry treats nominal levels symmetrically", {
  training <- data.frame(
    numeric_value = c(-3, -2, -1, 0, 1, 2),
    group = factor(c("a", "a", "a", "a", "b", "c"), levels = c("a", "b", "c")),
    enabled = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)
  )
  blueprint <- AutoXplainR:::fit_matrix_blueprint(
    training,
    center = TRUE,
    scale = TRUE,
    categorical_encoding = "one_hot"
  )
  baked <- AutoXplainR:::bake_matrix_blueprint(blueprint, training)
  group_columns <- names(blueprint$column_predictors)[
    blueprint$column_predictors == "group"
  ]
  logical_columns <- names(blueprint$column_predictors)[
    blueprint$column_predictors == "enabled"
  ]

  expect_identical(blueprint$categorical_encoding, "one_hot")
  expect_length(group_columns, 3L)
  expect_length(logical_columns, 2L)
  expect_equal(unname(blueprint$center[group_columns]), rep(0, 3L))
  expect_equal(unname(blueprint$scale[group_columns]), rep(1, 3L))
  expect_setequal(as.numeric(baked[, group_columns]), c(0, 1))
  expect_setequal(as.numeric(baked[, logical_columns]), c(0, 1))
  expect_equal(unname(blueprint$scale[logical_columns]), rep(1, 2L))
  expect_equal(mean(baked[, "numeric_value"]), 0, tolerance = 1e-12)
  expect_equal(stats::sd(baked[, "numeric_value"]), 1, tolerance = 1e-12)

  representatives <- baked[c(1L, 5L, 6L), group_columns, drop = FALSE]
  expect_equal(
    unname(as.vector(stats::dist(representatives))),
    rep(sqrt(2), 3L),
    tolerance = 1e-12
  )
})

test_that("treatment encoding remains the matrix blueprint default", {
  training <- data.frame(group = factor(c("a", "b", "c", "a")))
  blueprint <- AutoXplainR:::fit_matrix_blueprint(training)
  group_columns <- names(blueprint$column_predictors)[
    blueprint$column_predictors == "group"
  ]

  expect_identical(blueprint$categorical_encoding, "treatment")
  expect_length(group_columns, 2L)
})

test_that("radial SVR is equivariant to positive affine outcome rescaling", {
  skip_if_not_installed("e1071")
  training <- data.frame(
    x = seq(-2, 2, length.out = 72),
    group = factor(rep(c("a", "b", "c"), 24))
  )
  training$y <- with(
    training,
    sin(2 * x) + 0.3 * x + c(a = -0.4, b = 0.1, c = 0.7)[group]
  )
  parameters <- list(cost = 4, gamma_multiplier = 1, epsilon = 0.1)
  original <- AutoXplainR:::fit_kernel_learner(
    training, "y", "regression", parameters, seed = 101L
  )
  shifted <- training
  shifted$y <- 37 + 9 * training$y
  rescaled <- AutoXplainR:::fit_kernel_learner(
    shifted, "y", "regression", parameters, seed = 101L
  )

  original_prediction <- predict(original, training)
  rescaled_prediction <- predict(rescaled, training)
  expect_equal(
    (rescaled_prediction - 37) / 9,
    original_prediction,
    tolerance = 1e-3
  )
  expect_equal(original$fit_details$outcome_center, mean(training$y))
  expect_equal(original$fit_details$outcome_scale, stats::sd(training$y))
  expect_true(original$fit_details$outcome_standardized)
  expect_identical(original$blueprint$categorical_encoding, "one_hot")

  restored <- unserialize(serialize(original, NULL))
  expect_equal(predict(restored, training), original_prediction, tolerance = 1e-12)
})

test_that("kernel and neighbor learners both request distance-safe encoding", {
  data <- data.frame(
    x = seq(-1, 1, length.out = 18),
    group = factor(rep(c("a", "b", "c"), 6)),
    y = rep(c(0, 1, 2), 6) + seq(-1, 1, length.out = 18)
  )

  if (requireNamespace("e1071", quietly = TRUE)) {
    kernel <- AutoXplainR:::fit_kernel_learner(
      data,
      "y",
      "regression",
      list(cost = 1, gamma_multiplier = 1, epsilon = 0.1),
      seed = 5L
    )
    expect_identical(kernel$blueprint$categorical_encoding, "one_hot")
  }
  if (requireNamespace("kknn", quietly = TRUE)) {
    neighbors <- AutoXplainR:::fit_neighbors_learner(
      data,
      "y",
      "regression",
      list(k = 3L, distance = 2, kernel = "optimal"),
      seed = 5L
    )
    expect_identical(neighbors$blueprint$categorical_encoding, "one_hot")
  }
})

test_that("low-budget kernel search prefixes vary every effective SVR control", {
  first <- AutoXplainR:::kernel_learner_grid(100L, 5L, "regression", 1L)
  second <- AutoXplainR:::kernel_learner_grid(100L, 5L, "regression", 1L)
  prefix <- first[seq_len(4L)]

  expect_identical(first, second)
  expect_gt(length(unique(vapply(prefix, `[[`, numeric(1L), "cost"))), 1L)
  expect_gt(
    length(unique(vapply(prefix, `[[`, numeric(1L), "gamma_multiplier"))),
    1L
  )
  expect_gt(length(unique(vapply(prefix, `[[`, numeric(1L), "epsilon"))), 1L)
  expect_equal(
    nrow(unique(data.frame(
      cost = vapply(prefix, `[[`, numeric(1L), "cost"),
      gamma = vapply(prefix, `[[`, numeric(1L), "gamma_multiplier"),
      epsilon = vapply(prefix, `[[`, numeric(1L), "epsilon")
    ))),
    4L
  )
})

test_that("low-budget neighbor search prefixes vary every geometry control", {
  first <- AutoXplainR:::neighbors_learner_grid(100L, 5L, "regression", 1L)
  second <- AutoXplainR:::neighbors_learner_grid(100L, 5L, "regression", 1L)
  prefix <- first[seq_len(4L)]

  expect_identical(first, second)
  expect_gt(length(unique(vapply(prefix, `[[`, integer(1L), "k"))), 1L)
  expect_setequal(vapply(prefix, `[[`, numeric(1L), "distance"), c(1, 2))
  expect_setequal(
    vapply(prefix, `[[`, character(1L), "kernel"),
    c("optimal", "triangular")
  )
  expect_equal(
    nrow(unique(data.frame(
      k = vapply(prefix, `[[`, integer(1L), "k"),
      distance = vapply(prefix, `[[`, numeric(1L), "distance"),
      kernel = vapply(prefix, `[[`, character(1L), "kernel")
    ))),
    4L
  )
})
