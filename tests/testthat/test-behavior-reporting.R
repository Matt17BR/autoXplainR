test_that("beginner report separates capacity cards from computed evidence", {
  result <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 2026)
  path <- tempfile(fileext = ".html")
  on.exit(unlink(path), add = TRUE)
  render_model_report(result, path, top_features = 2L, n_repeats = 2L)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(html, "How are these model families different?", fixed = TRUE)
  expect_match(html, "Prior/model-capacity knowledge", fixed = TRUE)
  expect_match(html, "do not show that this fitted model actually used", fixed = TRUE)
  expect_match(html, "Computed evidence from this analysis", fixed = TRUE)
  expect_match(html, "Family", fixed = TRUE)
  expect_match(html, "Backend", fixed = TRUE)
  expect_match(html, "resource proxy", fixed = TRUE)
  expect_match(html, "not structural complexity", fixed = TRUE)
})

test_that("aggregate narrative context records retained engines and refit truth", {
  set.seed(912)
  data <- data.frame(x = rnorm(90), z = rnorm(90))
  data$y <- data$x^2 + data$z + rnorm(90, sd = 0.2)
  result <- autoxplain(
    data,
    "y",
    model_set = "tuned",
    portfolio = "core",
    max_models = 5L,
    nfolds = 2L,
    seed = 18L
  )
  context <- AutoXplainR:::prepare_analysis_context(result)
  text <- AutoXplainR:::context_to_text(context)
  memo <- AutoXplainR:::create_fallback_report(context)

  expect_setequal(context$retained_models$family, unique(
    result$leaderboard$family[result$leaderboard$role != "baseline"]
  ))
  expect_true(all(nzchar(context$retained_models$backend)))
  expect_identical(
    context$tuning_summary$final_configuration,
    result$tuning$final_configuration
  )
  expect_match(text, "Retained model identities (aggregate metadata)", fixed = TRUE)
  expect_match(text, "PRIOR/MODEL-CAPACITY KNOWLEDGE", fixed = TRUE)
  expect_match(text, "do not show that the fitted models used", fixed = TRUE)
  expect_match(text, "COMPUTED MODEL-COMPARISON EVIDENCE", fixed = TRUE)
  expect_match(text, "Actual final fitted configuration", fixed = TRUE)
  expect_match(text, result$tuning$final_configuration, fixed = TRUE)
  expect_match(memo, "What kinds of models were retained?", fixed = TRUE)
  expect_match(memo, "What did the retained models do differently?", fixed = TRUE)
})

test_that("fallback and failed-family audit fields reach beginner outputs", {
  result <- autoxplain(
    mtcars,
    "mpg",
    model_set = "tuned",
    portfolio = "core",
    max_models = 4L,
    nfolds = 2L,
    seed = 44L
  )
  selected <- result$tuning$selected_configuration
  alternative <- setdiff(result$tuning$candidates$configuration_id, selected)[[1L]]
  result$tuning$final_configuration <- alternative
  result$tuning$refit$fallback_used <- TRUE
  result$tuning$refit$status <- "partial"
  result$tuning$families_resampling_failed <- "forest"
  result$tuning$refit$families_resampling_failed <- "forest"
  result$tuning$refit$families_refit_failed <- "kernel"

  context <- AutoXplainR:::prepare_analysis_context(result)
  text <- AutoXplainR:::context_to_text(context)
  memo <- AutoXplainR:::create_fallback_report(context)
  path <- tempfile(fileext = ".html")
  on.exit(unlink(path), add = TRUE)
  render_model_report(result, path, top_features = 1L, n_repeats = 2L)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_true(context$tuning_summary$fallback_used)
  expect_match(text, "fallback used: yes", fixed = TRUE)
  expect_match(text, "Families with no complete resampling result: forest", fixed = TRUE)
  expect_match(text, "Families that failed full-training refit: kernel", fixed = TRUE)
  expect_match(memo, "recorded refit fallback was used", fixed = TRUE)
  expect_match(html, "The resampling choice could not be refitted", fixed = TRUE)
  expect_match(html, "No complete resampling result", fixed = TRUE)
  expect_match(html, "Full-training refit failed", fixed = TRUE)
})
