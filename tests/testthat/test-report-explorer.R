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
  # Agreements on four cases: A/B = 2/4, A/C = 3/4, B/C = 2/4.
  winners <- list(A = c(1, 1, 2, 3), B = c(1, 2, 2, 1), C = c(1, 3, 2, 3))
  make_explainers <- function(labels, order = 1:3) {
    lapply(winners, function(winner) {
      probability <- matrix(.1, nrow = 4, ncol = 3, dimnames = list(NULL, labels))
      probability[cbind(1:4, winner)] <- .8
      explain_model(
        probability[, order], data.frame(row = 1:4, y = factor(labels[c(1, 2, 3, 1)], levels = labels[order])), "y",
        task = "multiclass", predict_function = function(model, data) model[data$row, , drop = FALSE]
      )
    })
  }
  explainers <- make_explainers(c("a", "b", "c"))
  testthat::local_mocked_bindings(as_explainers = function(...) explainers, .package = "AutoXplainR")
  result <- list(task = "multiclass")
  original <- AutoXplainR:::calculate_correlation_insights(result)
  expect_match(original, "predicted-class agreement is 0.583", fixed = TRUE)
  explainers <- make_explainers(c("zeta", "alpha", "mu"), c(3, 1, 2))
  expect_identical(AutoXplainR:::calculate_correlation_insights(result), original)
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

test_that("clock subtraction noise cannot invent a Pareto cost advantage", {
  first <- AutoXplainR:::elapsed_milliseconds(1000.23, 1000.231)
  second <- AutoXplainR:::elapsed_milliseconds(1001.23, 1001.231)
  expect_identical(first, 1)
  expect_identical(second, 1)
  expect_identical(AutoXplainR:::pareto_nondominated(c(2, 7), c(first, second)), c(TRUE, FALSE))
  expect_equal(AutoXplainR:::elapsed_milliseconds(0, .000123), .123)
  expect_identical(AutoXplainR:::elapsed_milliseconds(1, 1), 0)
  expect_identical(AutoXplainR:::explorer_measurement(0, resource = TRUE), "~0")
  expect_identical(AutoXplainR:::explorer_measurement(0), "0")
  expect_identical(AutoXplainR:::explorer_measurement(.125, resource = TRUE), "0.125")
})

test_that("small probability effects keep distinct signed axis labels", {
  ticks <- seq(-.01, .015, by = .005)
  labels <- vapply(ticks, AutoXplainR:::report_axis_number, character(1))
  expect_equal(as.numeric(labels), ticks)
  expect_length(unique(labels), length(ticks))
  expect_identical(AutoXplainR:::report_axis_number(0), "0")
  expect_equal(as.numeric(AutoXplainR:::report_number(-1e-7)), -1e-7)
  effect <- structure(data.frame(x = 1:3, accumulated_effect = c(-.01, 0, .013)), method = "ale")
  svg <- AutoXplainR:::effect_svg(effect, "x")
  expect_match(svg, ">0.005</text>", fixed = TRUE)
  expect_match(svg, ">-0.005</text>", fixed = TRUE)
})

test_that("report titles are escaped and offline assets remain embedded", {
  result <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 8)
  path <- tempfile(fileext = ".html")
  render_model_report(result, path, title = "Cars </title><script>window.bad=1</script>")
  html <- paste(readLines(path), collapse = "\n")
  expect_match(html, "Cars &lt;/title&gt;&lt;script&gt;window.bad=1&lt;/script&gt;", fixed = TRUE)
  expect_false(grepl("<script>window.bad=1</script>", html, fixed = TRUE))
  expect_false(grepl('src="https?://', html))
})

test_that("model details expose effective controls and learned values from the retained fit", {
  result <- autoxplain(mtcars, "mpg", seed = 92, explain = FALSE)
  tree_id <- result$leaderboard$model_id[result$leaderboard$family == "tree"]
  neural_id <- result$leaderboard$model_id[result$leaderboard$family == "neural"]
  linear_id <- result$leaderboard$model_id[result$leaderboard$family == "linear"]
  tree <- result$models[[tree_id]]
  spec <- AutoXplainR:::model_specification(result, tree_id)
  expect_equal(spec$parameters$cp, tree$control$cp)
  expect_equal(spec$parameters$minbucket, tree$control$minbucket)
  known_tree <- rpart::rpart(y ~ x, data = data.frame(x = 1:12, y = rep(c(0, 10, 20), each = 4)),
    control = rpart::rpart.control(cp = 0, minsplit = 2, minbucket = 1, maxdepth = 2, xval = 0)
  )
  known_spec <- AutoXplainR:::model_specification(list(models = list(tree = known_tree), task = "regression"), "tree")
  expect_equal(known_spec$learned$`Terminal leaves`, 3)
  expect_equal(known_spec$learned$`Fitted depth`, 2)
  expect_match(spec$engine_version, "^[0-9]")
  result$models[[tree_id]]$control$cp <- .123
  expect_equal(AutoXplainR:::model_specification(result, tree_id)$parameters$cp, .123)
  neural <- AutoXplainR:::model_specification(result, neural_id)
  expect_equal(neural$parameters$size, result$models[[neural_id]]$model$n[2])
  expect_equal(neural$parameters$maxit, 500L)
  expect_equal(neural$learned$`Fitted weights`, length(result$models[[neural_id]]$model$wts))
  linear <- AutoXplainR:::model_specification(result, linear_id)
  expect_equal(linear$coefficients, coef(result$models[[linear_id]]))
  expect_match(linear$formula, "mpg ~")
  expect_match(AutoXplainR:::explorer_model_spec_details(result, tree_id), "Fitted tree rules")
  expect_match(AutoXplainR:::explorer_model_spec_details(result, linear_id), "Fitted coefficients")
  expect_equal(extract_model_characteristics(result)[[tree_id]]$hyperparameters$cp, .123)
})

test_that("multiclass reports retain and select every class without mixing cached curves", {
  result <- autoxplain(iris, "Species", model_set = "comparison", seed = 22)
  classes <- levels(iris$Species)
  by_class <- result$explanations$effects_by_class
  expect_identical(names(by_class), classes)
  id <- result$provenance$primary_model_id
  first <- by_class[[classes[1]]][[id]][[1]]
  last <- by_class[[classes[3]]][[id]][[1]]
  expect_identical(attr(first, "prediction_class"), classes[1])
  expect_identical(attr(last, "prediction_class"), classes[3])
  expect_false(isTRUE(all.equal(first[[2]], last[[2]])))
  testthat::local_mocked_bindings(explain_effect = function(...) stop("must reuse"), .package = "AutoXplainR")
  path <- tempfile(fileext = ".html")
  expect_no_error(render_model_report(result, path))
  html <- paste(readLines(path), collapse = "\n")
  expect_match(html, 'id="effect-class-select"', fixed = TRUE)
  for (class in classes) expect_match(html, paste0('data-class-panel="', class, '"'), fixed = TRUE)
  attr(result$explanations$effects_by_class[[classes[3]]][[id]][[1]], "prediction_class") <- classes[1]
  expect_error(render_model_report(result, tempfile(fileext = ".html")), "wrong prediction class")
})

test_that("a model outside the explanation budget offers recovery without an empty selector", {
  result <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 12, explain = FALSE)
  result$explanations <- AutoXplainR:::prepare_model_report_data(result, max_models = 1, n_repeats = 2)
  html <- AutoXplainR:::explorer_features(
    result, result$explanations$audit,
    result$explanations$effects, AutoXplainR:::explorer_models(result)
  )
  expect_match(html, "Rebuild the report with a larger model budget", fixed = TRUE)
  expect_match(html, "max_models = length(result$models)", fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr('class="feature-select"', html, fixed = TRUE))), 1L)
})

test_that("classification mistakes retain class probabilities and prioritize confident errors", {
  probability <- cbind(cat = c(.6, .03, .1), dog = c(.3, .02, .8), bird = c(.1, .95, .1))
  observed <- factor(c("cat", "dog", "cat"), levels = c("cat", "dog", "bird"))
  rows <- AutoXplainR:::explorer_classification_mistakes(observed, probability[, 3:1], levels(observed))
  expect_equal(rows$Row, c(2L, 3L))
  expect_equal(rows$Predicted, c("bird", "dog"))
  expect_equal(rows[["Probability of observed class"]], c(.02, .1))
  expect_equal(rows[["Probability of predicted class"]], c(.95, .8))
  binary <- AutoXplainR:::explorer_classification_mistakes(c("no", "yes", "no"), c(.5, .01, .1), c("no", "yes"))
  expect_equal(binary$Row, c(2L, 1L))
  expect_equal(binary$Predicted, c("no", "yes"))
  expect_equal(binary[["Probability of observed class"]], c(.01, .5))
  expect_equal(binary[["Probability of predicted class"]], c(.99, .5))
})
