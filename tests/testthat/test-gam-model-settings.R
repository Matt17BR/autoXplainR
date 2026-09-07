test_that("GAM identity distinguishes actual input basis dimensions from requested limits", {
  skip_if_package_unavailable("mgcv")
  data <- data.frame(
    limited = rep(1:4, 20),
    curved = seq(-2, 2, length.out = 80),
    category = factor(rep(c("a", "b"), each = 40))
  )
  data$y <- sin(data$curved) + .2 * data$limited +
    .1 * (data$category == "b") + .01 * cos(seq_len(80))
  requested <- list(k = 8L, gamma = 1.4, select = TRUE)
  model <- AutoXplainR:::fit_additive_learner(data, "y", "regression", requested, seed = 51L)
  spec <- AutoXplainR:::model_specification(
    list(models = list(additive = model), task = "regression"), "additive"
  )

  # Inspect mgcv's retained smooth objects, independently of the wrapper metadata.
  native_dimensions <- vapply(model$fit$smooth, function(smooth) smooth$bs.dim, numeric(1))
  expect_equal(native_dimensions, c(3, 8))
  expect_equal(unname(spec$parameters$smooth_k), native_dimensions)
  expect_identical(names(spec$parameters$smooth_k), c("limited", "curved"))
  expect_equal(spec$learned$`Smooth basis dimensions`, c(limited = 3, curved = 8))
  expect_equal(spec$learned$`Total effective degrees of freedom`, sum(model$fit$edf))
  expect_match(spec$summary, "smooth basis k = 3\u20138 (varies by input)", fixed = TRUE)
  expect_match(spec$summary, "gamma = 1.4, shrinkage selection = on", fixed = TRUE)
  expect_identical(model$fit_details$requested_parameters, requested)
  expect_null(spec$parameters$k)
  expect_match(AutoXplainR:::describe_additive_parameters(requested), "smooth basis limit = 8", fixed = TRUE)

  # When all inputs can use the chosen basis, the compact identity gives that k.
  requested$k <- 3L
  equal_model <- AutoXplainR:::fit_additive_learner(data, "y", "regression", requested, seed = 51L)
  equal_spec <- AutoXplainR:::model_specification(
    list(models = list(additive = equal_model), task = "regression"), "additive"
  )
  expect_equal(vapply(equal_model$fit$smooth, function(smooth) smooth$bs.dim, numeric(1)), c(3, 3))
  expect_match(equal_spec$summary, "smooth basis k = 3, gamma = 1.4", fixed = TRUE)
  expect_false(grepl("varies by input", equal_spec$summary, fixed = TRUE))
})

test_that("missing GAM basis metadata is stated rather than formatted as a blank value", {
  description <- AutoXplainR:::describe_additive_parameters(list(gamma = 1, select = FALSE))
  expect_identical(description, "smooth basis k not recorded, gamma = 1, shrinkage selection = off")
})
