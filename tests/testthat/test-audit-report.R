test_that("audit detects explanation disagreement among competitive models", {
  set.seed(77)
  n <- 360
  x1 <- rnorm(n)
  x2 <- x1 + rnorm(n, sd = 0.15)
  data <- data.frame(x1 = x1, x2 = x2, noise = rnorm(n))
  data$y <- x1 + x2 + rnorm(n, sd = 0.25)
  train <- data[1:240, ]
  test <- data[241:360, ]
  e1 <- explain_model(lm(y ~ x1, train), test, "y",
    label = "x1 model",
    metadata = list(evaluation_role = "test")
  )
  e2 <- explain_model(lm(y ~ x2, train), test, "y",
    label = "x2 model",
    metadata = list(evaluation_role = "test")
  )
  audit <- audit_explanations(
    list(e1, e2),
    n_repeats = 12, performance_tolerance = 0.25,
    dependence_threshold = 0.7, seed = 2
  )

  expect_s3_class(audit, "autoxplain_audit")
  expect_equal(audit$summary$n_models, 2)
  expect_equal(audit$summary$n_near_optimal, 2)
  expect_true(all(c("shuffle_status", "dependence_status", "claim") %in% names(audit$importance)))
  expect_true(any(audit$findings$code == "feature_dependence"))
  expect_output(print(audit), "evidence audit")
  expect_type(summary(audit), "list")
})

test_that("reports are standalone, escaped, and provenance-rich", {
  fixture <- make_regression_fixture()
  explainer <- explain_model(
    fixture$model, fixture$test, "y",
    label = "<unsafe & model>",
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
  result <- autoxplain(model_set = "quick", data, "y", seed = 31)
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

  expect_match(html, "Model comparison", fixed = TRUE)
  expect_match(html, "Compare the models", fixed = TRUE)
  expect_match(html, "test evaluation", fixed = TRUE)
  expect_match(html, "Where do predictions go wrong?", fixed = TRUE)
  expect_match(html, "Feature importance &amp; effects", fixed = TRUE)
  expect_match(html, "Reading this analysis", fixed = TRUE)
  expect_match(html, "Understanding &lt;y&gt;", fixed = TRUE)
  expect_false(grepl("<script>unsafe</script>", html, fixed = TRUE))
  expect_match(html, "&lt;script&gt;unsafe&lt;/script&gt;", fixed = TRUE)
  expect_match(html, "<details class=\"advanced\">", fixed = TRUE)
  expect_false(grepl("<details class=\"advanced\" open", html, fixed = TRUE))
  expect_match(html, "Verify this prose against", fixed = TRUE)
})

test_that("guided reports distinguish selection validation from held-out tests", {
  result <- autoxplain(model_set = "quick", mtcars, "mpg", seed = 2026, evaluation_role = "validation")
  path <- tempfile(fileext = ".html")
  on.exit(unlink(path), add = TRUE)

  render_model_report(result, path, top_features = 1, n_repeats = 2)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(html, "validation evaluation", fixed = TRUE)
  expect_match(html, "Validation scores: these rows may have influenced model selection", fixed = TRUE)
  expect_false(grepl("Did the model generalize?", html, fixed = TRUE))
})

test_that("reports keep supplied evaluation data descriptively neutral by default", {
  training <- mtcars[1:24, , drop = FALSE]
  evaluation <- mtcars[25:32, , drop = FALSE]
  result <- autoxplain(model_set = "quick", training, "mpg", test_data = evaluation, seed = 2026)
  path <- tempfile(fileext = ".html")
  on.exit(unlink(path), add = TRUE)

  render_model_report(result, path, top_features = 1, n_repeats = 2)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_identical(result$provenance$evaluation_role, "evaluation")
  expect_match(html, 'data-role="evaluation"', fixed = TRUE)
  expect_false(grepl("evaluation evaluation", html, fixed = TRUE))
  expect_match(html, "Supplied evaluation rows", fixed = TRUE)
  expect_match(html, "independence from model selection is not asserted", fixed = TRUE)
  expect_false(grepl("Did the model generalize?", html, fixed = TRUE))
})

test_that("dashboard compatibility entry point produces the guided report", {
  result <- autoxplain(model_set = "quick", mtcars, "mpg", seed = 18)
  path <- tempfile(fileext = ".html")
  expect_warning(output <- generate_dashboard(
    result,
    output_file = path,
    top_features = 2,
    n_repeats = 3,
    include_llm_report = TRUE
  ), class = "autoxplain_deprecated")
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")

  expect_match(html, "Understanding mpg", fixed = TRUE)
  expect_match(html, "Plain-language memo", fixed = TRUE)
  expect_match(html, "Provider used: local", fixed = TRUE)
  expect_match(html, 'data-model-row="simple_baseline"', fixed = TRUE)
  expect_match(html, "leading-caveat", fixed = TRUE)
  expect_error(
    generate_dashboard(result, tempfile(fileext = ".html"), narrative_args = list("bad")),
    "named list"
  )
})

test_that("report screening and audit use the result's primary metric", {
  set.seed(124)
  data <- data.frame(x = rnorm(90), z = rnorm(90))
  data$y <- data$x + rnorm(90, sd = 0.4)
  result <- autoxplain(data, "y", seed = 124, nfolds = 2L, max_models = 3L,
    tuning_control = tuning_control(metric = "mae")
  )
  prepared <- AutoXplainR:::prepare_model_report_data(
    result,
    top_features = 1,
    n_repeats = 2,
    max_models = 2
  )
  explainer <- as_explainers(result, models = 1)[[1L]]
  direct_audit <- audit_explanations(
    explainer,
    features = "x",
    n_repeats = 2,
    seed = 124
  )

  expect_identical(explainer$metadata$primary_metric, "mae")
  expect_identical(direct_audit$config$metric, "mae")
  expect_identical(attr(prepared$screening, "metric"), "mae")
  expect_identical(prepared$audit$config$metric, "mae")
  expect_true(all(prepared$audit$performance$metric == "mae"))
})

test_that("guided reports support classification effect targets", {
  result <- autoxplain(model_set = "quick", iris, "Species", seed = 21)
  path <- tempfile(fileext = ".html")
  render_model_report(result, path, top_features = 1, n_repeats = 2)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(html, "multiclass", fixed = TRUE)
  expect_match(html, "probability for class", fixed = TRUE)
  payload <- strsplit(html, '<script type="application/json" id="axr-predictions-payload">', fixed = TRUE)[[1L]][2L]
  payload <- jsonlite::fromJSON(strsplit(payload, "</script>", fixed = TRUE)[[1L]][1L], simplifyVector = FALSE)
  for (model in payload$models) {
    expect_identical(model$task, "multiclass")
    expect_setequal(unlist(model$labels), levels(iris$Species))
    expect_equal(sum(vapply(model$confusion, `[[`, numeric(1), "count")), nrow(result$test_data))
    expect_match(model$calibration$scope, "confidence", ignore.case = TRUE)
  }
  expect_error(render_model_report(result, tempfile(fileext = ".txt")), "html")
})

test_that("comparison reports explain Pareto trade-offs without selecting on holdout", {
  result <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 2026)
  path <- tempfile(fileext = ".html")
  render_model_report(result, path, top_features = 2, n_repeats = 2)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(html, "Compare the models", fixed = TRUE)
  # Keep the trade-off view discoverable by its name. Literal curve geometry and
  # its interpretation are checked against independent browser fixtures.
  expect_match(html, "Pareto frontier", fixed = TRUE)
  expect_match(html, "pre-specified default", ignore.case = TRUE)
  expect_match(html, "id=\"models\"", fixed = TRUE)
})

test_that("legacy dashboard helpers remain functional compatibility layers", {
  set.seed(33)
  data <- data.frame(x = rnorm(100), z = rnorm(100))
  data$y <- 2 * data$x + rnorm(100, sd = 0.5)
  result <- autoxplain(model_set = "quick", data, "y", seed = 33)
  prepared <- AutoXplainR:::prepare_dashboard_data(
    result,
    top_features = 2,
    n_repeats = 2,
    max_models = 2
  )

  expect_s3_class(prepared$audit, "autoxplain_audit")
  expect_length(prepared$importance_list, 2L)
  expect_length(prepared$pdp_data, 2L)
  expect_match(prepared$correlation_insights_html, "unavailable.*constant")

  one_model <- evaluate_models(result$models["main_model"], result$test_data,
    result$target_column, features = result$features
  )
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
