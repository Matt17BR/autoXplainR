test_that("matrix blueprints preserve a dense training schema", {
  training <- data.frame(
    check.names = FALSE,
    "odd numeric name" = c(1, 2, 4, 8),
    "segment-type" = factor(c("small", "large", "small", "large")),
    active = c(TRUE, FALSE, TRUE, FALSE)
  )
  blueprint <- AutoXplainR:::fit_matrix_blueprint(
    training,
    center = TRUE,
    scale = TRUE
  )
  baked <- AutoXplainR:::bake_matrix_blueprint(blueprint, training)

  expect_s3_class(blueprint, "autoxplain_matrix_blueprint")
  expect_identical(blueprint$output, "dense")
  expect_true(is.matrix(baked))
  expect_type(baked, "double")
  expect_identical(colnames(baked), blueprint$columns)
  expect_false("(Intercept)" %in% colnames(baked))
  expect_equal(unname(colMeans(baked)), rep(0, ncol(baked)), tolerance = 1e-12)
  expect_equal(
    unname(apply(baked, 2L, stats::sd)),
    rep(1, ncol(baked)),
    tolerance = 1e-12
  )
  expect_identical(blueprint$training_rows, nrow(training))
  expect_false(any(vapply(blueprint, is.data.frame, logical(1L))))
})

test_that("one predictor and an optional intercept never lose dimensions", {
  training <- data.frame(check.names = FALSE, "one predictor" = c(2, 4, 6, 8))
  without_intercept <- AutoXplainR:::fit_matrix_blueprint(training)
  with_intercept <- AutoXplainR:::fit_matrix_blueprint(training, intercept = TRUE)

  baked <- AutoXplainR:::bake_matrix_blueprint(
    without_intercept,
    data.frame(check.names = FALSE, "one predictor" = c(10, 12))
  )
  baked_with_intercept <- AutoXplainR:::bake_matrix_blueprint(with_intercept, training)

  expect_equal(dim(baked), c(2L, 1L))
  expect_identical(colnames(baked), without_intercept$columns)
  expect_match(colnames(baked), "one predictor", fixed = TRUE)
  expect_equal(dim(baked_with_intercept), c(4L, 2L))
  expect_identical(colnames(baked_with_intercept), with_intercept$columns)
  expect_identical(colnames(baked_with_intercept)[[1L]], "(Intercept)")
})

test_that("baking enforces predictor types, levels, and exact columns", {
  training <- data.frame(
    amount = c(1, 2, 3, 4),
    group = factor(c("a", "b", "a", "b")),
    enabled = c(TRUE, TRUE, FALSE, FALSE)
  )
  blueprint <- AutoXplainR:::fit_matrix_blueprint(training)

  expect_error(
    AutoXplainR:::bake_matrix_blueprint(blueprint, training[c("amount", "group")]),
    "missing predictor columns: enabled"
  )
  expect_error(
    AutoXplainR:::bake_matrix_blueprint(
      blueprint,
      transform(training, group = c("a", "b", "new", "a"))
    ),
    "contains unseen levels: new"
  )
  expect_error(
    AutoXplainR:::bake_matrix_blueprint(blueprint, transform(training, amount = as.character(amount))),
    "`amount` must be numeric"
  )
  expect_error(
    AutoXplainR:::bake_matrix_blueprint(blueprint, transform(training, enabled = as.integer(enabled))),
    "`enabled` must be logical"
  )

  compatible <- transform(training, group = as.character(group), outcome = 1:4)
  baked <- AutoXplainR:::bake_matrix_blueprint(blueprint, compatible)
  expect_identical(colnames(baked), blueprint$columns)
  expect_equal(nrow(baked), nrow(compatible))

  damaged <- blueprint
  damaged$columns <- rev(damaged$columns)
  expect_error(
    AutoXplainR:::bake_matrix_blueprint(damaged, training),
    "Encoded columns do not match the training blueprint"
  )
})

test_that("all matrix statistics and levels come from training only", {
  training <- data.frame(
    x = c(1, 2, 3, 4),
    category = factor(c("train-a", "train-b", "train-a", "train-b"),
                      levels = c("train-a", "train-b", "evaluation-only"))
  )
  blueprint <- AutoXplainR:::fit_matrix_blueprint(
    training,
    center = TRUE,
    scale = TRUE
  )
  before <- unserialize(serialize(blueprint, NULL))
  evaluation <- data.frame(
    x = c(-1e9, 1e9),
    category = c("train-a", "train-b")
  )
  baked <- AutoXplainR:::bake_matrix_blueprint(blueprint, evaluation)

  expect_identical(blueprint, before)
  expect_identical(blueprint$xlevels$category, c("train-a", "train-b"))
  expect_equal(blueprint$center[["x"]], mean(training$x))
  expect_equal(blueprint$scale[["x"]], stats::sd(training$x))
  expect_gt(max(abs(baked[, "x"])), 1e8)
  expect_error(
    AutoXplainR:::bake_matrix_blueprint(
      blueprint,
      data.frame(x = 5, category = "evaluation-only")
    ),
    "unseen levels: evaluation-only"
  )
})

test_that("matrix blueprint validation is actionable", {
  expect_error(
    AutoXplainR:::fit_matrix_blueprint(data.frame(x = 1:3), predictors = character()),
    "one or more unique column names"
  )
  expect_error(
    AutoXplainR:::fit_matrix_blueprint(data.frame(x = numeric())),
    "at least one training row"
  )
  expect_error(
    AutoXplainR:::fit_matrix_blueprint(data.frame(x = 1:3), predictors = "missing"),
    "missing predictor columns: missing"
  )
  expect_error(
    AutoXplainR:::fit_matrix_blueprint(data.frame(x = c(1, NA, 3))),
    "must not contain missing values: x"
  )
  expect_error(
    AutoXplainR:::fit_matrix_blueprint(data.frame(x = c(1, Inf, 3))),
    "must be finite: x"
  )
  expect_error(
    AutoXplainR:::fit_matrix_blueprint(data.frame(group = factor(rep("a", 3)))),
    "at least two observed levels: group"
  )
  expect_error(
    AutoXplainR:::fit_matrix_blueprint(data.frame(x = I(list(1, 2, 3)))),
    "supports numeric, logical, factor, and character predictors"
  )
  expect_error(
    AutoXplainR:::fit_matrix_blueprint(data.frame(x = 1:3), center = 1),
    "`center` must be TRUE or FALSE"
  )
})

test_that("an included intercept is not centered or scaled away", {
  training <- data.frame(x = c(1, 2, 4, 8))
  blueprint <- AutoXplainR:::fit_matrix_blueprint(
    training,
    intercept = TRUE,
    center = TRUE,
    scale = TRUE
  )
  baked <- AutoXplainR:::bake_matrix_blueprint(blueprint, training)

  expect_equal(unname(baked[, "(Intercept)"]), rep(1, nrow(training)))
  expect_equal(blueprint$center[["(Intercept)"]], 0)
  expect_equal(blueprint$scale[["(Intercept)"]], 1)
})

test_that("safe formulas preserve literal backticks in column names", {
  data <- data.frame(
    check.names = FALSE,
    "input`value" = seq_len(8),
    "other value" = rep(c(0, 1), 4),
    "target`value" = 2 * seq_len(8) + rep(c(0, 1), 4)
  )
  formula <- AutoXplainR:::safe_reformulate(
    c("input`value", "other value"),
    response = "target`value"
  )
  fit <- stats::lm(formula, data = data)
  prediction <- stats::predict(fit, data)
  expect_length(prediction, nrow(data))
  expect_true(all(is.finite(prediction)))
})
