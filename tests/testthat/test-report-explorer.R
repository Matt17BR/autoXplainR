test_that("the one-command default fits a core model comparison without optional engines", {
  for (task in c("regression", "binary", "multiclass")) {
    data <- if (task == "regression") mtcars else iris
    target <- if (task == "regression") "mpg" else "Species"
    if (task == "binary") data$Species <- factor(data$Species == "virginica")
    result <- autoxplain(data, target, seed = 92, explain = FALSE)
    expect_identical(result$provenance$model_set, "tuned")
    expect_identical(result$provenance$portfolio, "core")
    expect_s3_class(result$tuning, "autoxplain_tuning")
    expect_true(all(c("linear", "tree", "neural", "baseline") %in% result$leaderboard$family))
    expect_true(all(is.finite(predict(result, head(data)))))
    expect_equal(nrow(result$tuning$candidates), 15L)
  }
  quick <- autoxplain(mtcars, "mpg", model_set = "quick", explain = FALSE)
  expect_length(quick$models, 2L)
})

test_that("held-out outcomes cannot change the default training selection", {
  data <- make_regression_fixture()
  a <- autoxplain(data$train, "y", test_data = data$test, explain = FALSE)
  changed <- data$test
  changed$y <- rev(changed$y) + 100
  b <- autoxplain(data$train, "y", test_data = changed, explain = FALSE)
  expect_identical(a$tuning$final_configuration, b$tuning$final_configuration)
  expect_equal(a$tuning$candidates$cv_score, b$tuning$candidates$cv_score)
  expect_false(identical(a$leaderboard$rmse, b$leaderboard$rmse))
})

test_that("model-specific effects are retained, reusable and reject stale model evidence", {
  result <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 12)
  ids <- setdiff(names(result$models), "simple_baseline")
  expect_true(all(ids %in% names(result$explanations$effects_by_model)))
  for (id in ids) {
    effects <- result$explanations$effects_by_model[[id]]
    expect_true(length(effects) > 0)
    expected <- AutoXplainR:::current_explainer_fingerprint(as_explainers(result, models = id)[[1]])
    for (effect in effects) expect_identical(attr(effect, "explainer_fingerprint"), expected)
  }
  testthat::local_mocked_bindings(explain_effect = function(...) stop("must reuse"), .package = "AutoXplainR")
  expect_no_error(render_model_report(result, tempfile(fileext = ".html")))
  changed <- result
  attr(changed$explanations$effects_by_model$small_tree[[1]], "explainer_fingerprint") <- "foreign"
  expect_error(render_model_report(changed, tempfile(fileext = ".html")), "stale")
})

test_that("relationship matrices retain signs, types and unavailable values", {
  result <- list(training_data = data.frame(
    x = 1:12, reversed = 12:1, group = factor(rep(c("a", "b"), each = 6)), constant = 1
  ), features = c("x", "reversed", "group", "constant"))
  audit <- list(config = list(features = result$features))
  pairs <- AutoXplainR:::explorer_relationship_data(result, audit)$pairs
  expect_equal(pairs$value[pairs$a == "x" & pairs$b == "reversed"], -1)
  expect_true(is.na(pairs$value[pairs$a == "x" & pairs$b == "constant"]))
  mixed <- pairs[pairs$a == "x" & pairs$b == "group", ]
  expect_match(mixed$method, "unsigned")
  expect_gt(mixed$value, 0)
  expect_equal(mixed$n, 12)
  result$training_data$x[1:2] <- NA
  pairs <- AutoXplainR:::explorer_relationship_data(result, audit)$pairs
  expect_equal(pairs$n[pairs$a == "x" & pairs$b == "group"], 10)
})

test_that("screening retains inputs used by alternative models and supplies their displayed curves", {
  set.seed(871)
  x <- seq(-3, 3, length.out = 240)
  data <- data.frame(x = x, z = rnorm(240))
  data$y <- x^2 + .7 * data$z + rnorm(240, sd = .1)
  result <- autoxplain(data, "y", model_set = "comparison", seed = 17, explain = FALSE)
  prepared <- AutoXplainR:::prepare_model_report_data(result, top_features = 1, n_repeats = 3)
  tops <- vapply(prepared$screening_by_model, function(rows) rows$feature[which.max(rows$importance)], character(1))
  expect_identical(unname(tops["main_model"]), "z")
  expect_identical(unname(tops["flexible_tree"]), "x")
  expect_true(all(c("x", "z") %in% prepared$audit$config$features))
  for (id in names(prepared$effects_by_model)) {
    rows <- prepared$audit$importance[prepared$audit$importance$model == id, ]
    feature <- rows$feature[which.max(rows$importance)]
    expect_s3_class(prepared$effects_by_model[[id]][[feature]], "data.frame")
  }
})

test_that("legacy classification comparison is invariant to class codes", {
  result <- autoxplain(iris, "Species", model_set = "comparison", seed = 22, explain = FALSE)
  text <- AutoXplainR:::calculate_correlation_insights(result)
  expect_match(text, "predicted-class agreement", fixed = TRUE)
  explainers <- as_explainers(result)
  labels <- lapply(explainers, function(x) {
    p <- predict(x, x$data)
    colnames(p)[max.col(p, ties.method = "first")]
  })
  pairs <- utils::combn(seq_along(labels), 2)
  expected <- mean(apply(pairs, 2, function(pair) mean(labels[[pair[1]]] == labels[[pair[2]]])))
  expect_match(text, format(expected, digits = 3), fixed = TRUE)
})

test_that("cost enrichment tolerates engine-specific optional metadata", {
  result <- autoxplain(mtcars, "mpg", model_set = "quick", explain = FALSE)
  result$model_characteristics[[1]]$size_bytes <- NULL
  result$model_characteristics[[1]]$training_time_s <- NULL
  expect_equal(AutoXplainR:::enrich_tradeoff_leaderboard(result), result$leaderboard)
  result$leaderboard$model_size_kb <- NULL
  board <- AutoXplainR:::enrich_tradeoff_leaderboard(result)
  expect_true(is.na(board$model_size_kb[match(result$model_characteristics[[1]]$model_id, board$model_id)]))
})

test_that("explorer reports expose linked controls and preserve malicious text as text", {
  result <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 8)
  path <- tempfile(fileext = ".html")
  render_model_report(result, path, title = "Cars </title><script>window.bad=1</script>")
  html <- paste(readLines(path), collapse = "\n")
  expect_match(html, "Cars &lt;/title&gt;&lt;script&gt;window.bad=1&lt;/script&gt;", fixed = TRUE)
  expect_false(grepl("<script>window.bad=1</script>", html, fixed = TRUE))
  for (id in names(result$models)) {
    expect_match(html, paste0('data-model-panel="', id, '"'), fixed = TRUE)
    expect_match(html, paste0('data-model-row="', id, '"'), fixed = TRUE)
  }
  expect_match(html, 'id="metric-select"', fixed = TRUE)
  expect_match(html, 'id="resource-select"', fixed = TRUE)
  expect_match(html, 'role="tooltip"', fixed = TRUE)
  expect_match(html, "Print this view", fixed = TRUE)
  expect_false(grepl('src="https?://', html))
})
