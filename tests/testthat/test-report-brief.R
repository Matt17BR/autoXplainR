test_that("effect graphics preserve numeric geometry and expose scale and support", {
  effect <- data.frame(
    x = c(0, 1, 10), accumulated_effect = c(-2, -1, 2),
    conf_low = c(-2.2, -1.2, 1.8), conf_high = c(-1.8, -.8, 2.2),
    support = c(1, .5, .2)
  )
  attr(effect, "method") <- "ale"
  html <- AutoXplainR:::effect_svg(effect, "x")
  match <- regmatches(html, regexec('class="effect-line" points="([^"]+)"', html))[[1L]][2L]
  coordinates <- do.call(rbind, lapply(strsplit(match, " ", fixed = TRUE)[[1L]], function(p) {
    as.numeric(strsplit(p, ",", fixed = TRUE)[[1L]])
  }))
  expect_equal(diff(coordinates[, 1])[2] / diff(coordinates[, 1])[1], 9)
  expect_match(html, "zero-line", fixed = TRUE)
  expect_match(html, "Relative support 0–1", fixed = TRUE)
  expect_match(html, "effect-band", fixed = TRUE)
  expect_match(html, "min-width:600px", fixed = TRUE)
  expect_match(html, 'role="region" tabindex="0"', fixed = TRUE)
})

test_that("categorical effects use named points without a connecting trend", {
  effect <- data.frame(
    group = factor(c("east", "north & west", "south")),
    partial_dependence = c(1, 3, 2), support = c(1, .5, .8)
  )
  attr(effect, "method") <- "pdp"
  html <- AutoXplainR:::effect_svg(effect, "group")
  expect_false(grepl('class="effect-line"', html, fixed = TRUE))
  expect_match(html, "north &amp; west", fixed = TRUE)
  expect_match(html, "effect-point", fixed = TRUE)
})

test_that("report view uses retained evidence without computing missing checks", {
  result <- autoxplain(model_set = "quick", mtcars, "mpg", explain = FALSE, seed = 91)
  result$explanations <- NULL
  testthat::local_mocked_bindings(audit_explanations = function(...) stop("must not compute"), .package = "AutoXplainR")
  view <- AutoXplainR:::report_view_model(result)
  expect_s3_class(view, "autoxplain_report_view")
  expect_identical(view$identity$model_label, "linear regression")
  expect_identical(view$diagnostics$importance$status, "not_run")
  expect_null(view$audit)
  result$explanations <- list(failures = data.frame(feature = "wt", reason = "insufficient support"))
  failed <- AutoXplainR:::report_view_model(result)
  expect_identical(failed$diagnostics[["effect:wt"]]$status, "failed")
  override <- AutoXplainR:::report_view_model(result, effects = list())
  expect_null(override$effect_failures)
  expect_null(override$diagnostics[["effect:wt"]])
})

test_that("attached audits and effects cannot change the represented fitted analysis", {
  result <- autoxplain(model_set = "quick", mtcars, "mpg", seed = 13)
  other <- autoxplain(model_set = "quick", mtcars, "mpg", seed = 14)
  expect_error(
    render_model_report(result, tempfile(fileext = ".html"), audit = other$explanations$audit),
    "same selected model explainers"
  )
  expect_error(
    render_model_report(result, tempfile(fileext = ".html"), effects = other$explanations$effects),
    "same primary model"
  )
  unidentified <- result$explanations$effects
  attr(unidentified[[1L]], "explainer_fingerprint") <- NULL
  expect_error(
    render_model_report(result, tempfile(fileext = ".html"), effects = unidentified),
    "same primary model"
  )
  expect_error(
    render_model_report(result, tempfile(fileext = ".html"), target_units = character()),
    "unit label"
  )
})

test_that("brief links named findings to evidence and gives mobile and print routes", {
  result <- autoxplain(model_set = "quick", mtcars, "mpg", seed = 13)
  path <- tempfile(fileext = ".html")
  render_model_report(result, path, target_units = "miles per gallon")
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "miles per gallon", fixed = TRUE)
  expect_match(html, "linear regression", fixed = TRUE)
  expect_match(html, 'data-page-link="checks"', fixed = TRUE)
  expect_match(html, 'href="#provenance"', fixed = TRUE)
  expect_false(grepl('class="grade', html, fixed = TRUE))
  expect_match(html, "Print this view", fixed = TRUE)
  expect_match(html, "details::details-content", fixed = TRUE)
  links <- regmatches(html, gregexpr('href="#evidence-[^"]+"', html))[[1L]]
  expect_match(html, 'id="uncertainty"', fixed = TRUE)
  for (link in links) {
    id <- sub('href="#', "", sub('"$', "", link))
    expect_true(grepl(paste0('id="', id, '"'), html, fixed = TRUE))
  }
})

test_that("failed optional comparisons remain visible with their reason", {
  result <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 1)
  testthat::local_mocked_bindings(
    model_tradeoffs = function(...) stop("measurement <unavailable>"),
    .package = "AutoXplainR"
  )
  html <- AutoXplainR:::render_model_comparison(result)
  expect_match(html, "Resource comparison: failed", fixed = TRUE)
  expect_match(html, "measurement &lt;unavailable&gt;", fixed = TRUE)
  expect_match(html, 'id="models"', fixed = TRUE)
})

test_that("report-only failures have shared records without mutating the result", {
  result <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 1)
  testthat::local_mocked_bindings(
    model_tradeoffs = function(...) stop("resource measurement failed"), .package = "AutoXplainR"
  )
  prepared <- AutoXplainR:::prepare_report_diagnostics(result)
  view <- AutoXplainR:::report_view_model(prepared)
  expect_identical(view$diagnostics$resources$status, "failed")
  expect_identical(view$diagnostics$resources$reason, "resource measurement failed")
  expect_null(result$explanations$report_diagnostics)
  expect_identical(AutoXplainR:::report_view_model(result)$diagnostics$resources$status, "not_run")
  path <- tempfile(fileext = ".html")
  output <- render_model_report(result, path)
  records <- attr(output, "diagnostic_status")
  expect_identical(as.character(output), normalizePath(path))
  expect_identical(records$resources$status, "failed")
  expect_identical(names(records$resources), c("id", "status", "scope", "entities", "reason"))
  expect_null(records$prediction_disagreement$evidence)
  expect_null(result$explanations$report_diagnostics)
})

test_that("binary brief identifies the trained positive probability event", {
  data <- iris
  data$event <- factor(data$Species == "virginica", levels = c(FALSE, TRUE), labels = c("other", "virginica"))
  data$Species <- NULL
  result <- autoxplain(model_set = "quick", data, "event", seed = 12)
  view <- AutoXplainR:::report_view_model(result)
  expect_identical(view$identity$positive, "virginica")
  overview <- AutoXplainR:::render_model_overview(
    result, AutoXplainR:::model_report_evaluation(result, result$explanations$audit)
  )
  expect_match(overview, "Probability event:</strong> virginica", fixed = TRUE)
})
