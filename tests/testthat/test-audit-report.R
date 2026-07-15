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
