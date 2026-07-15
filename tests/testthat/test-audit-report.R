test_that("audit detects explanation disagreement among competitive models", {
  set.seed(77)
  n <- 360
  x1 <- rnorm(n)
  x2 <- x1 + rnorm(n, sd = 0.15)
  data <- data.frame(x1 = x1, x2 = x2, noise = rnorm(n))
  data$y <- x1 + x2 + rnorm(n, sd = 0.25)
  train <- data[1:240, ]
  test <- data[241:360, ]
  e1 <- explain_model(lm(y ~ x1, train), test, "y", label = "x1 model",
                      metadata = list(evaluation_role = "test"))
  e2 <- explain_model(lm(y ~ x2, train), test, "y", label = "x2 model",
                      metadata = list(evaluation_role = "test"))
  audit <- audit_explanations(
    list(e1, e2), n_repeats = 12, performance_tolerance = 0.25,
    dependence_threshold = 0.7, seed = 2
  )

  expect_s3_class(audit, "autoxplain_audit")
  expect_equal(audit$summary$n_models, 2)
  expect_equal(audit$summary$n_near_optimal, 2)
  expect_true(all(c("evidence_grade", "claim") %in% names(audit$importance)))
  expect_true(any(audit$findings$code == "feature_dependence"))
  expect_output(print(audit), "evidence audit")
  expect_type(summary(audit), "list")
})

test_that("reports are standalone, escaped, and provenance-rich", {
  fixture <- make_regression_fixture()
  explainer <- explain_model(
    fixture$model, fixture$test, "y", label = "<unsafe & model>",
    metadata = list(evaluation_role = "test")
  )
  audit <- audit_explanations(explainer, features = c("x1", "x2"), n_repeats = 4)
  audit$optional_narrative <- "Secondary <script>alert('no')</script>"
  path <- tempfile(fileext = ".html")
  output <- render_explanation_report(audit, path, title = "Evidence <review>")
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_true(file.exists(output))
  expect_match(html, "<!doctype html>", fixed = TRUE)
  expect_match(html, "Evidence &lt;review&gt;", fixed = TRUE)
  expect_false(grepl("<unsafe & model>", html, fixed = TRUE))
  expect_false(grepl("<script>alert", html, fixed = TRUE))
  expect_match(html, "Secondary &lt;script&gt;", fixed = TRUE)
  expect_match(html, "Explainer IDs", fixed = TRUE)
  expect_error(render_explanation_report(audit, tempfile(fileext = ".txt")), "html")
})

test_that("guided reports lead with evaluation and progressively disclose evidence", {
  set.seed(120)
  data <- data.frame(x = rnorm(120), z = rnorm(120))
  data$y <- 3 * data$x + rnorm(120, sd = 0.4)
  result <- autoxplain(data, "y", seed = 31)
  narrative <- generate_natural_language_report(result)
  path <- tempfile(fileext = ".html")
  output <- render_model_report(
    result,
    path,
    title = "Understanding <y>",
    narrative = paste0(narrative, "\n<script>unsafe</script>"),
    top_features = 2,
    n_repeats = 3
  )
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")

  expect_match(html, "Guided model report", fixed = TRUE)
  expect_match(html, "The modeling question", fixed = TRUE)
  expect_match(html, "Did the model generalize?", fixed = TRUE)
  expect_match(html, "How large were individual errors?", fixed = TRUE)
  expect_match(html, "Patterns used for prediction", fixed = TRUE)
  expect_match(html, "What this analysis does not establish", fixed = TRUE)
  expect_match(html, "Understanding &lt;y&gt;", fixed = TRUE)
  expect_false(grepl("<script>unsafe</script>", html, fixed = TRUE))
  expect_match(html, "&lt;script&gt;unsafe&lt;/script&gt;", fixed = TRUE)
  expect_match(html, "<svg class=\"effect-plot\"", fixed = TRUE)
  expect_match(html, "<details class=\"advanced\">", fixed = TRUE)
  expect_false(grepl("<details class=\"advanced\" open", html, fixed = TRUE))
  expect_match(html, "Verify this prose against", fixed = TRUE)
})

test_that("dashboard compatibility entry point produces the guided report", {
  result <- autoxplain(mtcars, "mpg", seed = 18)
  path <- tempfile(fileext = ".html")
  output <- generate_dashboard(
    result,
    output_file = path,
    top_features = 2,
    n_repeats = 3,
    include_llm_report = TRUE
  )
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")

  expect_match(html, "Understanding mpg", fixed = TRUE)
  expect_match(html, "Plain-language memo", fixed = TRUE)
  expect_match(html, "Provider used: local", fixed = TRUE)
  expect_match(html, "simple baseline", ignore.case = TRUE)
  expect_match(html, "Important context for these scores", fixed = TRUE)
  expect_error(
    generate_dashboard(result, tempfile(fileext = ".html"), narrative_args = list("bad")),
    "named list"
  )
})

test_that("guided reports support classification effect targets", {
  result <- autoxplain(iris, "Species", seed = 21)
  path <- tempfile(fileext = ".html")
  render_model_report(result, path, top_features = 1, n_repeats = 2)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(html, "multiclass", fixed = TRUE)
  expect_match(html, "probability for class", fixed = TRUE)
  expect_match(html, "Which classes were confused?", fixed = TRUE)
  expect_error(render_model_report(result, tempfile(fileext = ".txt")), "html")
})

test_that("comparison reports explain Pareto trade-offs without selecting on holdout", {
  result <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 2026)
  path <- tempfile(fileext = ".html")
  render_model_report(result, path, top_features = 2, n_repeats = 2)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(html, "What trade-offs did the candidates make?", fixed = TRUE)
  expect_match(html, "<svg class=\"tradeoff-plot\"", fixed = TRUE)
  expect_match(html, "Pareto-efficient", fixed = TRUE)
  expect_match(html, "primary model remains pre-specified", ignore.case = TRUE)
  expect_match(html, "#models", fixed = TRUE)
})

test_that("legacy dashboard helpers remain functional compatibility layers", {
  set.seed(33)
  data <- data.frame(x = rnorm(100), z = rnorm(100))
  data$y <- 2 * data$x + rnorm(100, sd = 0.5)
  result <- autoxplain(data, "y", seed = 33)
  prepared <- AutoXplainR:::prepare_dashboard_data(
    result,
    top_features = 2,
    n_repeats = 2,
    max_models = 2
  )

  expect_s3_class(prepared$audit, "autoxplain_audit")
  expect_length(prepared$importance_list, 2L)
  expect_length(prepared$pdp_data, 2L)
  expect_match(prepared$correlation_insights_html, "supplied models only")

  one_model <- result
  one_model$models <- one_model$models["main_model"]
  expect_match(
    AutoXplainR:::calculate_correlation_insights(one_model),
    "One model supplied"
  )
  expect_warning(
    retired <- AutoXplainR:::create_dashboard_rmd(list()),
    "retired"
  )
  expect_match(paste(retired, collapse = "\n"), "Retired AutoXplainR")
  expect_match(
    AutoXplainR:::create_simple_html(autoxplain_result = result),
    "<!doctype html>",
    fixed = TRUE
  )
})
