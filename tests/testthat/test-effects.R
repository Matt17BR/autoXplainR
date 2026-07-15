test_that("ALE recovers a monotone linear effect", {
  fixture <- make_regression_fixture(n = 300)
  explainer <- explain_model(fixture$model, fixture$test, "y")
  ale <- explain_effect(explainer, feature = "x1", n_points = 10)

  expect_s3_class(ale, "autoxplain_effect")
  expect_equal(attr(ale, "method"), "ale")
  expect_gt(cor(ale$x1, ale$accumulated_effect), 0.95)
  expect_true(all(ale$support >= 0 & ale$support <= 1))
  expect_output(print(ale), "AutoXplainR ALE")
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
