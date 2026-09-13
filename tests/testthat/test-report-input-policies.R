report_input_policy_fixture <- function() {
  data <- data.frame(x = c(-2, -1, 0, 1, 2), group = factor(c("a", "b", "a", "b", "a")), y = 1:5)
  parameters <- boosting_learner_grid(5L, 2L, "regression", 1L)[[1L]]
  parameters$encoding <- "matrix"
  model <- structure(list(
    family = "boosting", backend = "xgboost", fit = list(), task = "regression",
    parameters = parameters, package_version = "fixture",
    blueprint = fit_matrix_blueprint(data, predictors = c("x", "group")),
    fit_details = list(computation = resolve_boosting_encoding(parameters, data, "y"), threads = 1L)
  ), class = "autoxplain_fitted_model")
  attr(model, "autoxplain_tuning_fit") <- list(configuration_id = "boosting_01")
  list(
    models = list(main_model = model), task = "regression", training_data = data,
    tuning = list(
      candidates = data.frame(configuration_id = "boosting_01"),
      input_policy = list(boosting = resolve_boosting_encoding(list(), data, "y"))
    )
  )
}

test_that("model dialogs distinguish automatic search policy from resolved backend requests", {
  result <- report_input_policy_fixture()
  before <- serialize(result, NULL)
  spec <- model_specification(result, "main_model")
  expect_equal(spec$learned$`Encoded inputs`, length(result$models$main_model$blueprint$columns))
  expect_identical(spec$learned$`Native threads`, 1L)
  expect_false("Input encoding and memory policy" %in% names(spec$learned))
  html <- explorer_model_spec_details(result, "main_model")
  expect_match(html, "Automatic; planned encoding = matrix", fixed = TRUE)
  expect_match(html, "Dense matrix; numeric inputs and categorical contrasts", fixed = TRUE)
  expect_match(html, "Estimated dense matrix cells", fixed = TRUE)
  expect_match(html, "does not establish the original public request", fixed = TRUE)
  expect_match(html, "<details><summary>Raw input-policy metadata</summary>", fixed = TRUE)
  # Keep the original backend reason and all unrecognized metadata behind a disclosure.
  visible <- strsplit(html, "<details><summary>Raw input-policy metadata</summary>", fixed = TRUE)[[1L]][[1L]]
  expect_false(grepl("Explicit input encoding.", visible, fixed = TRUE))
  expect_match(html, "Explicit input encoding.", fixed = TRUE)
  columns <- result$models$main_model$blueprint$columns
  expect_match(html, paste0("<details><summary>Encoded input names (", length(columns), ")</summary>"), fixed = TRUE)
  expect_match(html, html_escape(model_spec_exact_value(columns)), fixed = TRUE)
  expect_identical(serialize(result, NULL), before)
})

test_that("mixed and older encoding records never invent a public automatic request", {
  result <- report_input_policy_fixture()
  result$models$main_model$fit_details$computation$encoding <- "native"
  result$models$main_model$fit_details$computation$requested <- "native"
  html <- model_spec_boosting_policy(result, "main_model")
  expect_match(html, "Native categorical inputs", fixed = TRUE)
  expect_match(html, "Automatic; planned encoding = matrix", fixed = TRUE)
  expect_match(html, "may also include explicit choices", fixed = TRUE)
  attr(result$models$main_model, "autoxplain_tuning_fit")$configuration_id <- "unrelated"
  html <- model_spec_boosting_policy(result, "main_model")
  expect_false(grepl("Automatic; planned encoding", html, fixed = TRUE))
  expect_match(html, "Not recorded for this fit", fixed = TRUE)
  result$tuning <- NULL
  result$models$main_model$fit_details$computation <- NULL
  html <- explorer_model_spec_details(result, "main_model")
  expect_match(html, "Dense matrix; numeric inputs and categorical contrasts", fixed = TRUE)
  expect_match(html, "Not recorded for this fit", fixed = TRUE)
  result$models$main_model$parameters$encoding <- NULL
  expect_match(model_spec_boosting_policy(result, "main_model"), "Not recorded", fixed = TRUE)
  direct <- report_input_policy_fixture()
  direct$tuning <- NULL
  direct$models$main_model$fit_details$computation$requested <- "auto"
  direct$models$main_model$fit_details$computation$reason <- "Recorded automatic backend choice."
  html <- model_spec_boosting_policy(direct, "main_model")
  expect_match(html, "Recorded automatic backend choice.", fixed = TRUE)
  expect_false(grepl("Automatic; planned encoding", html, fixed = TRUE))
})

test_that("input-policy metadata stays complete and HTML escaped", {
  result <- report_input_policy_fixture()
  result$models$main_model$fit_details$computation$future <- list(message = "<script>bad()</script>", exact = .1)
  html <- model_spec_boosting_policy(result, "main_model")
  expect_match(html, "future = list", fixed = TRUE)
  expect_match(html, "&lt;script&gt;bad()&lt;/script&gt;", fixed = TRUE)
  expect_false(grepl("<script>bad()", html, fixed = TRUE))
})

test_that("no-op preprocessing is concise without hiding enabled or nontrivial transformations", {
  identity_center <- stats::setNames(rep(0, 90), paste0("x", seq_len(90)))
  identity_scale <- identity_center + 1
  expect_identical(
    model_spec_preprocessing_vector(identity_center, FALSE, "center"),
    "<p>All 90 recorded centers are 0 (no centering adjustment).</p>"
  )
  expect_identical(
    model_spec_preprocessing_vector(identity_scale, FALSE, "scale"),
    "<p>All 90 recorded scales are 1 (no scaling adjustment).</p>"
  )
  expect_match(model_spec_preprocessing_vector(identity_center, NULL, "center"), "All 90 recorded centers")
  expect_match(model_spec_preprocessing_vector(identity_scale, NULL, "scale"), "All 90 recorded scales")
  expect_match(model_spec_preprocessing_vector(identity_center, TRUE, "center"), "Encoded-column centers")
  expect_match(model_spec_preprocessing_vector(identity_scale, TRUE, "scale"), "Encoded-column scales")
  for (flag in list(FALSE, TRUE, NULL)) {
    expect_match(
      model_spec_preprocessing_vector(c(a = 0, b = .10000000000000002), flag, "center"),
      "0.10000000000000002", fixed = TRUE
    )
    expect_match(
      model_spec_preprocessing_vector(c(a = 1, b = 1.0000000000000002), flag, "scale"),
      "1.0000000000000002", fixed = TRUE
    )
    expect_match(model_spec_preprocessing_vector(c(a = 0, b = NA_real_), flag, "center"), "Encoded-column centers")
    expect_match(model_spec_preprocessing_vector(c(a = 1, b = Inf), flag, "scale"), "Encoded-column scales")
  }
  expect_identical(model_spec_preprocessing_vector(NULL, FALSE, "center"), "")
})

test_that("generated preprocessing details preserve exact applied blueprint values and encoded inputs", {
  data <- data.frame(x = c(-2, -1, 0, 1, 2), z = c(4, 8, 7, 6, 9))
  result <- report_input_policy_fixture()
  blueprint <- fit_matrix_blueprint(data, center = TRUE, scale = TRUE)
  result$models$main_model$blueprint <- blueprint
  before <- serialize(result, NULL)
  baked <- bake_matrix_blueprint(blueprint, data)
  html <- explorer_model_spec_details(result, "main_model")
  expect_match(html, "Encoded-column centers", fixed = TRUE)
  expect_match(html, "Encoded-column scales", fixed = TRUE)
  for (value in c(blueprint$center, blueprint$scale)) {
    expect_match(html, model_spec_exact_value(unname(value)), fixed = TRUE)
  }
  expect_identical(bake_matrix_blueprint(result$models$main_model$blueprint, data), baked)
  expect_identical(serialize(result, NULL), before)
})

test_that("public boosting reports preserve fitted models and every prediction", {
  skip_if_package_unavailable("xgboost")
  data <- data.frame(x = seq_len(60L), group = factor(rep(c("a", "b", "c"), 20L)))
  data$y <- sin(data$x) + as.integer(data$group)
  parameters <- boosting_learner_grid(60L, 2L, "regression", 1L)[[1L]]
  parameters$nrounds <- 2L
  parameters$encoding <- "auto"
  result <- autoxplain(
    data, "y", learners = "boosting", max_models = 1L, nfolds = 2L,
    explain = FALSE, seed = 617L,
    tuning_control = tuning_control(grids = list(boosting = parameters))
  )
  before <- serialize(result, NULL)
  predictions <- lapply(result$models, stats::predict, newdata = data)
  path <- tempfile(fileext = ".html")
  on.exit(unlink(path))
  render_model_report(result, path)
  html <- paste(readLines(path), collapse = "\n")
  expect_match(html, "Automatic; planned encoding = matrix", fixed = TRUE)
  expect_match(html, "no centering adjustment", fixed = TRUE)
  expect_match(html, "no scaling adjustment", fixed = TRUE)
  expect_identical(serialize(result, NULL), before)
  expect_identical(lapply(result$models, stats::predict, newdata = data), predictions)
})
