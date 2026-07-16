test_that("ALE recovers a monotone linear effect", {
  fixture <- make_regression_fixture(n = 300)
  explainer <- explain_model(fixture$model, fixture$test, "y")
  ale <- explain_effect(explainer, feature = "x1", n_points = 10)

  expect_s3_class(ale, "autoxplain_effect")
  expect_equal(attr(ale, "method"), "ale")
  expect_identical(attr(ale, "task"), "regression")
  expect_identical(attr(ale, "prediction_target"), "predicted value")
  expect_gt(cor(ale$x1, ale$accumulated_effect), 0.95)
  expect_true(all(ale$support >= 0 & ale$support <= 1))
  expect_output(print(ale), "AutoXplainR ALE")
})

test_that("multiclass effects retain the exact selected probability target", {
  model <- nnet::multinom(Species ~ ., data = iris, trace = FALSE)
  explainer <- explain_model(model, iris, "Species", task = "multiclass")
  effect <- explain_effect(
    explainer,
    feature = "Petal.Length",
    method = "pdp",
    class = "virginica",
    n_points = 5L
  )

  expect_identical(attr(effect, "task"), "multiclass")
  expect_identical(attr(effect, "prediction_class"), "virginica")
  expect_identical(
    attr(effect, "prediction_target"),
    "probability for class `virginica`"
  )
  fake_result <- list(task = "multiclass")
  expect_match(
    AutoXplainR:::effect_plain_summary(effect, fake_result),
    "class `virginica`",
    fixed = TRUE
  )
})

test_that("raw common classifiers infer their probability task", {
  multinomial <- nnet::multinom(Species ~ ., data = iris, trace = FALSE)
  multinomial_effect <- explain_effect(
    multinomial,
    data = iris[setdiff(names(iris), "Species")],
    feature = "Petal.Length",
    method = "pdp",
    class = "virginica",
    n_points = 4L
  )
  expect_identical(attr(multinomial_effect, "task"), "multiclass")
  expect_identical(attr(multinomial_effect, "prediction_class"), "virginica")

  tree <- rpart::rpart(Species ~ ., data = iris, method = "class")
  tree_effect <- explain_effect(
    tree,
    data = iris[setdiff(names(iris), "Species")],
    feature = "Petal.Length",
    method = "pdp",
    class = "versicolor",
    n_points = 4L
  )
  expect_identical(attr(tree_effect, "task"), "multiclass")
  expect_identical(attr(tree_effect, "prediction_class"), "versicolor")

  default_effect <- explain_effect(
    multinomial,
    data = iris[setdiff(names(iris), "Species")],
    feature = "Petal.Length",
    method = "pdp",
    n_points = 4L
  )
  expect_identical(attr(default_effect, "prediction_class"), "setosa")
  expect_identical(
    attr(default_effect, "prediction_target"),
    "probability for class `setosa`"
  )
  expect_error(
    explain_effect(
      multinomial,
      data = iris[setdiff(names(iris), "Species")],
      feature = "Petal.Length",
      method = "pdp",
      class = "unknown",
      n_points = 4L
    ),
    "must name one multiclass outcome level"
  )
})

test_that("raw binary classifiers retain the discovered positive class", {
  data <- transform(
    mtcars,
    outcome = factor(ifelse(am == 1, "manual", "automatic"),
                     levels = c("automatic", "manual"))
  )
  model <- stats::glm(outcome ~ wt + hp, data = data, family = stats::binomial())
  effect <- explain_effect(
    model,
    data = data[c("wt", "hp")],
    feature = "wt",
    method = "pdp",
    n_points = 4L
  )

  expect_identical(attr(effect, "task"), "binary")
  expect_identical(attr(effect, "prediction_class"), "manual")
  expect_identical(
    attr(effect, "prediction_target"),
    "probability for positive class `manual`"
  )
  expect_error(
    explain_effect(
      model,
      data = data[c("wt", "hp")],
      feature = "wt",
      positive = "unknown",
      n_points = 4L
    ),
    "must name one binary outcome level"
  )
})

test_that("ALE uncertainty stays unavailable when a bin is a singleton", {
  small <- data.frame(x = 1:6, z = c(0, 1, 0, 1, 0, 1))
  small$y <- 2 * small$x + small$z
  model <- stats::lm(y ~ x + z, data = small)
  effect <- explain_effect(
    model,
    data = small[c("x", "z")],
    feature = "x",
    method = "ale",
    n_points = 5L
  )

  expect_true(all(is.na(effect$std_error)))
  expect_true(all(is.na(effect$conf_low)))
  expect_true(all(is.na(effect$conf_high)))
  expect_match(attr(effect, "interval_note"), "within-bin variation")
  expect_false(grepl("Monte Carlo", attr(effect, "interval_note"), fixed = TRUE))
  expect_output(print(effect), "bands:")
})

test_that("ALE uncertainty propagates centered independent-bin variation", {
  standard_error <- AutoXplainR:::ale_centered_standard_error(
    bin_se = c(1, 2),
    weights = c(0.25, 0.75)
  )

  expect_equal(standard_error, c(1.5, 0.5))
  expect_true(all(is.na(AutoXplainR:::ale_centered_standard_error(
    bin_se = c(1, NA_real_),
    weights = c(0.25, 0.75)
  ))))
})

test_that("PDP includes support and dependence diagnostics", {
  set.seed(1)
  data <- data.frame(x = rnorm(160))
  data$z <- data$x + rnorm(160, sd = 0.03)
  data$y <- 2 * data$x + rnorm(160, sd = 0.1)
  model <- lm(y ~ x + z, data[1:100, ])
  explainer <- explain_model(model, data[101:160, ], "y")
  pdp <- explain_effect(explainer, feature = "x", method = "pdp", n_points = 8)

  expect_equal(attr(pdp, "method"), "pdp")
  expect_true(attr(pdp, "dependence_warning"))
  expect_gt(attr(pdp, "max_association"), 0.9)
  expect_true(all(c("support", "conf_low", "conf_high") %in% names(pdp)))
})

test_that("categorical PDP preserves values", {
  data <- data.frame(group = factor(rep(c("a", "b"), each = 40)), x = rnorm(80))
  data$y <- ifelse(data$group == "b", 2, 0) + data$x
  model <- lm(y ~ group + x, data)
  explainer <- explain_model(model, data, "y")
  pdp <- explain_effect(explainer, feature = "group", method = "pdp")
  expect_setequal(as.character(pdp$group), c("a", "b"))
  expect_error(explain_effect(explainer, feature = "group", method = "ale"), "numeric")
})

test_that("effect aliases and validation work", {
  fixture <- make_regression_fixture()
  explainer <- explain_model(fixture$model, fixture$test, "y")
  expect_warning(
    pdp <- calculate_partial_dependence(explainer, feature = "x1", grid_size = 4),
    "deprecated"
  )
  expect_equal(nrow(pdp), 4)
  expect_error(explain_effect(explainer, feature = "missing"), "name one column")
  expect_error(explain_effect(explainer, feature = "x1", quantile_range = c(1, 0)),
               "increasing")
})
