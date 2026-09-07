test_that("effect chart evidence preserves small signed values and its reference support", {
  effect <- structure(data.frame(
    x = c(0, 1, 10), accumulated_effect = c(-.00000012, 0, .00000027),
    conf_low = c(-.00000015, -.00000001, .00000023),
    conf_high = c(-.0000001, .00000002, .0000003),
    n = c(NA, 2, 8), support = c(.25, .25, 1)
  ), method = "ale", prediction_target = "probability for positive class `yes`")
  html <- AutoXplainR:::effect_chart(effect, "x", model_id = "selected")
  expect_match(html, "data-zero=\"true\"", fixed = TRUE)
  expect_match(html, "data-bin-left=\"1\"", fixed = TRUE)
  expect_match(html, "data-n=\"8\"", fixed = TRUE)
  # A class mentioned only inside collapsed guidance does not identify the
  # visible curve. The figure's caption must retain its prediction target.
  caption <- sub("^.*<figcaption>([^<]+)</figcaption>.*$", "\\1", html)
  expect_match(caption, "ALE", fixed = TRUE)
  expect_match(caption, "x", fixed = TRUE)
  expect_match(caption, "probability for positive class `yes`", fixed = TRUE)
  expect_match(html, "reference rows", fixed = TRUE)
  expect_match(html, "-0.000000", fixed = TRUE)
  expect_match(html, "Chart values and support", fixed = TRUE)
  expect_match(html, "aria-label", fixed = TRUE)
})

test_that("categorical effect comparisons preserve categories rather than inventing a trend", {
  effect <- structure(data.frame(
    service = c("standard", "priority", "freight"),
    partial_dependence = c(23, 18, 26), support = c(1, .5, .2)
  ), method = "pdp")
  html <- AutoXplainR:::effect_chart(effect, "service", model_id = "primary")
  expect_match(html, "data-kind=\"category\"", fixed = TRUE)
  expect_match(html, "data-category=\"freight\"", fixed = TRUE)
  expect_match(html, "category frequency", fixed = TRUE)
  expect_false(grepl("data-n=", html, fixed = TRUE))
  expect_false(grepl("axr-line", html, fixed = TRUE))
  other <- effect
  attr(other, "prediction_class") <- "other"
  expect_error(AutoXplainR:::effect_chart(effect, "service", comparison = list(other = other)), "same prediction class")
})

test_that("chart labels and attribute payloads escape hostile names", {
  effect <- structure(data.frame(x = 1:2, accumulated_effect = c(-1, 1)), method = "ale")
  label <- '</span><script>window.injected=1</script>"'
  html <- AutoXplainR:::effect_chart(effect, "x", model_id = label)
  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
  expect_match(html, "&quot;", fixed = TRUE)
})

test_that("model colors follow identity rather than a sorted score row", {
  result <- list(models = list(primary = NULL, neural = NULL, tree = NULL, baseline = NULL))
  ids <- names(result$models)
  colors <- vapply(ids, AutoXplainR:::report_model_color, character(1), result = result)
  expect_length(unique(colors), length(ids))
  expect_identical(colors[rev(ids)], vapply(rev(ids), AutoXplainR:::report_model_color, character(1), result = result))
})

test_that("generic histogram and identity charts retain their statistical geometry inputs", {
  points <- list(list(
    x = .5, y = 12, left = 0, right = 1, count = 12,
    model = "model", label = "Model", color = "#17654e", detail = "12 rows in [0, 1]"
  ))
  histogram <- AutoXplainR:::report_chart_frame("histogram", points, "Residual", "Rows", "Errors", "", zero = TRUE)
  expect_match(histogram, "data-left=\"0\"", fixed = TRUE)
  expect_match(histogram, "data-right=\"1\"", fixed = TRUE)
  scatter <- AutoXplainR:::report_chart_frame(
    "scatter", points, "Predicted", "Observed", "Evaluation", "",
    reference = "identity",
    x_limits = c(0, 1), y_limits = c(0, 1)
  )
  expect_match(scatter, 'data-x-max="1"', fixed = TRUE)
  expect_match(scatter, 'data-y-min="0"', fixed = TRUE)
  expect_error(AutoXplainR:::report_chart_frame(
    "scatter", points, "x", "y", "title", "",
    x_limits = c(1, 0)
  ), "increasing finite")
  expect_match(scatter, "data-reference=\"identity\"", fixed = TRUE)
  expect_match(scatter, "data-count=\"12\"", fixed = TRUE)
})


test_that("an invalid primary effect cannot be silently replaced by comparison evidence", {
  primary <- structure(data.frame(x = 1:2, partial_dependence = c(NA, 1)), method = "pdp")
  other <- structure(data.frame(x = 1:2, partial_dependence = c(0, 1)), method = "pdp")
  html <- AutoXplainR:::effect_chart(primary, "x", comparison = list(other = other))
  expect_match(html, "No finite primary curve was retained.", fixed = TRUE)
  expect_match(html, "Fitted effect for x", fixed = TRUE)
  expect_false(grepl("data-chart-source", html, fixed = TRUE))
})

test_that("the frontier improves only when the next measured cost is affordable", {
  point <- function(x, y, frontier = "true") list(x = x, y = y, frontier = frontier)
  points <- list(point(3, 6), point(1, 10), point(2, 11, "false"), point(3, 6), point(5, 4))
  expect_equal(
    AutoXplainR:::report_chart_frontier_points(points),
    data.frame(x = c(1, 3, 3, 5, 5), y = c(10, 10, 6, 6, 4))
  )

  # Higher scores improve upward, with the same cost-budget convention. A zero
  # cost remains a measurement; tied scores at greater cost are dominated.
  higher <- list(point(3, .8), point(0, -.1), point(1, -.1, "false"), point(3, .8))
  expect_equal(
    AutoXplainR:::report_chart_frontier_points(higher),
    data.frame(x = c(0, 3, 3), y = c(-.1, -.1, .8))
  )
})

test_that("a frontier never invents a segment for one distinct or missing measurement", {
  point <- function(x, y, frontier = "true") list(x = x, y = y, frontier = frontier)
  expect_equal(AutoXplainR:::report_chart_frontier_points(list(
    point(2, 1), point(2, 1), point(3, 2, "false"), point(NA_real_, 0), point(0, Inf)
  )), data.frame(x = 2, y = 1))
  expect_equal(
    AutoXplainR:::report_chart_frontier_points(list(point(NA_real_, 0))),
    data.frame(x = numeric(), y = numeric())
  )
})

test_that("cost axes keep a readable scale for narrow ranges and numerical near ties", {
  ticks <- AutoXplainR:::report_chart_ticks(c(1, 6), 2L)
  expect_gte(length(ticks), 2L)
  expect_true(all(diff(ticks) > 0 & ticks[-1L] <= 6) && min(ticks) >= 1)
  expect_gte(length(AutoXplainR:::report_chart_ticks(c(1.964, 2.036), 2L)), 2L)
  values <- c(1, 1 + 1e-12, 1 + 2e-12)
  limits <- AutoXplainR:::report_chart_range(values, relative_span = .001)
  expect_gte(diff(limits), .001)
  expect_lt(diff(limits), .01)
  expect_lt(limits[1L], min(values))
  expect_gt(limits[2L], max(values))
  ticks <- AutoXplainR:::report_chart_ticks(limits, 4L)
  labels <- vapply(ticks, AutoXplainR:::report_chart_axis_number, character(1))
  expect_equal(length(unique(labels)), length(ticks))
  expect_equal(as.numeric(labels), ticks, tolerance = 1e-12)
})

test_that("inspection distinguishes measurements that coincide at ordinary display precision", {
  ordinary <- AutoXplainR:::report_chart_measurement_labels(c(2.997, 3.007, 3.952, 7.565))
  expect_identical(ordinary, c("2.997", "3.007", "3.952", "7.565"))
  labels <- AutoXplainR:::report_chart_measurement_labels(c(1, 1 + 1e-12, 1 + 2e-12, 1))
  expect_identical(labels, c("1", "1.000000000001", "1.000000000002", "1"))
})
