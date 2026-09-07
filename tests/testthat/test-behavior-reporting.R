test_that("reports identify actual fits and preserve computed model differences", {
  result <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 2026)
  path <- tempfile(fileext = ".html")
  on.exit(unlink(path), add = TRUE)
  render_model_report(result, path, top_features = 2L, n_repeats = 2L)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")

  for (id in names(result$models)) {
    spec <- AutoXplainR:::model_specification(result, id)
    expect_true(grepl(AutoXplainR:::html_escape(spec$summary), html, fixed = TRUE))
    expect_true(grepl(AutoXplainR:::html_escape(spec$engine), html, fixed = TRUE))
  }
  view <- AutoXplainR:::report_disagreement_view(result)
  ids <- view$performance$model_id
  predictions <- lapply(as_explainers(result, models = ids), function(explainer) predict(explainer, explainer$data))
  for (i in seq_len(nrow(view$pairs))) {
    row <- view$pairs[i, ]
    expected <- abs(predictions[[row$model_a]] - predictions[[row$model_b]])
    expect_equal(row$mean_prediction_distance, mean(expected))
    expect_equal(row$p90_prediction_distance, unname(quantile(expected, .9)))
  }
  expect_true(grepl('class="prediction-disagreement"', html, fixed = TRUE))
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
  expect_false(grepl("PRIOR/MODEL-CAPACITY KNOWLEDGE", text, fixed = TRUE))
  expect_match(text, "COMPUTED MODEL-COMPARISON EVIDENCE", fixed = TRUE)
  expect_match(text, "Actual final fitted configuration", fixed = TRUE)
  expect_match(text, result$tuning$final_configuration, fixed = TRUE)
  expect_false(grepl("What kinds of models were retained?", memo, fixed = TRUE))
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
  # This presentation fixture deliberately changes captured stage metadata.
  html <- AutoXplainR:::render_model_selection(result)

  expect_true(context$tuning_summary$fallback_used)
  expect_match(text, "fallback used: yes", fixed = TRUE)
  expect_match(text, "Families with no complete resampling result: forest", fixed = TRUE)
  expect_match(text, "Families that failed full-training refit: kernel", fixed = TRUE)
  expect_match(memo, "recorded refit fallback was used", fixed = TRUE)
  expect_true(grepl("The original choice failed refitting", html, fixed = TRUE))
  evidence <- tuning_evidence(result)
  expect_identical(evidence$family_failures$resampling, "forest")
  expect_identical(evidence$family_failures$refit, "kernel")
  expect_true(grepl("No complete resampling result: Random forest (forest)", html, fixed = TRUE))
  expect_true(grepl("Full-training refit failed: Radial support vector (kernel)", html, fixed = TRUE))
})
