effect_computation_fixture <- function(task = "multiclass") {
  data <- data.frame(
    x = rep(seq(-2, 2, length.out = 12), 3),
    z = sin(seq_len(36)), group = factor(rep(c("small", "large", "medium"), 12))
  )
  data$y <- switch(task,
    regression = data$x * data$z + .25 * data$x^2,
    binary = factor(rep(c("event", "other"), 18), levels = c("event", "other")),
    multiclass = factor(rep(c("third", "first", "middle"), 12), levels = c("third", "first", "middle"))
  )
  predictor <- function(model, newdata) {
    value <- newdata$x * newdata$z + .25 * newdata$x^2 + as.numeric(newdata$group) / 3
    if (model$task == "regression") {
      return(value)
    }
    if (model$task == "binary") {
      return(stats::plogis(value))
    }
    probability <- cbind(middle = exp(value / 3), third = exp(-value / 4), first = 1)
    probability / rowSums(probability)
  }
  evaluate_models(list(model = list(task = task)), data, "y",
    task = task,
    positive = if (task == "binary") "event" else NULL,
    predict_functions = list(model = predictor), primary = "model", seed = 61
  )
}

test_that("class bundles preserve all effect values, uncertainty and scope", {
  for (task in c("regression", "binary", "multiclass")) {
    result <- effect_computation_fixture(task)
    context <- AutoXplainR:::prepare_report_context(result)
    explainer <- context$explainers$model
    classes <- if (task == "multiclass") explainer$class_levels else NA_character_
    for (feature in c("x", "group")) {
      method <- if (feature == "x") "ale" else "pdp"
      expected <- lapply(classes, function(class) {
        explain_effect(explainer,
          feature = feature, method = method, n_points = 5L, seed = 92L, max_rows = 23L,
          class = if (task == "multiclass") class else NULL
        )
      })
      actual <- AutoXplainR:::explain_effect_bundle(
        explainer, feature,
        method, 5L, 92L, 23L, classes, context
      )
      for (index in seq_along(classes)) expect_identical(actual[[index]], expected[[index]])
      expect_silent(AutoXplainR:::validate_explanation_context(context))
    }
  }
})

test_that("a multiclass bundle predicts each modified batch once", {
  context <- AutoXplainR:::prepare_report_context(effect_computation_fixture())
  explainer <- context$explainers$model
  original <- AutoXplainR:::validate_predictions
  calls <- integer()
  local_mocked_bindings(validate_predictions = function(x, n, ...) {
    calls <<- c(calls, n)
    original(x, n, ...)
  }, .package = "AutoXplainR")
  for (method in c("ale", "pdp")) {
    feature <- if (method == "ale") "x" else "group"
    calls <- integer()
    bundle <- AutoXplainR:::explain_effect_bundle(
      explainer, feature,
      method, 5L, 92L, 23L, explainer$class_levels, context
    )
    expect_true(all(vapply(bundle, inherits, logical(1), "autoxplain_effect")))
    expect_equal(sum(calls == 23L), if (method == "ale") 2L else 3L)
    expect_false(any(calls == nrow(explainer$data)))
    AutoXplainR:::validate_explanation_context(context)
    expect_equal(sum(calls == nrow(explainer$data)), 1L)
  }
})

test_that("batch-dependent predictions keep separate ALE boundaries", {
  data <- data.frame(x = rep(0:3, each = 3), y = factor(rep(c("a", "b", "c"), 4)))
  predictor <- function(newdata) {
    centered <- newdata$x - mean(newdata$x)
    first <- .2 + .03 * centered
    second <- .3 - .02 * centered
    cbind(a = first, b = second, c = 1 - first - second)
  }
  result <- evaluate_models(list(model = list(label = "batch-dependent")), data, "y",
    predict_functions = list(model = predictor)
  )
  context <- AutoXplainR:::prepare_report_context(result)
  explainer <- context$explainers$model
  bundle <- AutoXplainR:::explain_effect_bundle(
    explainer, "x", "ale", 2L, 2L, NULL,
    explainer$class_levels, context
  )
  for (class in explainer$class_levels) {
    separate <- explain_effect(explainer, "x", n_points = 2L, seed = 2L, class = class)
    expect_identical(bundle[[class]], separate)
  }
  lower <- data.frame(x = c(0, 1))
  upper <- data.frame(x = c(1, 2))
  separate <- predictor(upper) - predictor(lower)
  joint <- predictor(rbind(lower, upper))
  expect_equal(separate, matrix(0, 2, 3, dimnames = list(NULL, c("a", "b", "c"))), tolerance = 1e-15)
  expect_gt(max(abs(joint[3:4, ] - joint[1:2, ])), .02)
})

test_that("local prediction reuse never reuses different ordered rows", {
  cache <- new.env(parent = emptyenv())
  cache$entries <- list()
  first <- AutoXplainR:::cache_effect_predictions(function(data) data$x - mean(data$x), cache)
  expect_identical(first(data.frame(x = c(0, 1))), c(-.5, .5))
  second <- AutoXplainR:::cache_effect_predictions(function(data) data$x - mean(data$x), cache)
  expect_identical(second(data.frame(x = c(1, 0))), c(.5, -.5))
})

test_that("local prediction reuse preserves empty outputs and typed errors", {
  cache <- new.env(parent = emptyenv())
  cache$entries <- list()
  empty <- AutoXplainR:::cache_effect_predictions(function(data) numeric(nrow(data)), cache)
  expect_identical(empty(data.frame(x = numeric())), numeric())
  again <- AutoXplainR:::cache_effect_predictions(function(data) numeric(nrow(data)), cache)
  expect_identical(again(data.frame(x = numeric())), numeric())
  cache$entries <- list()
  failure <- structure(list(message = "Prediction unavailable", call = NULL),
    class = c("fixture_prediction_error", "error", "condition")
  )
  failed <- AutoXplainR:::cache_effect_predictions(function(data) stop(failure), cache)
  expect_error(failed(data.frame(x = 1)), class = "fixture_prediction_error")
  reused <- AutoXplainR:::cache_effect_predictions(function(data) stop("should not repeat"), cache)
  expect_error(reused(data.frame(x = 1)), class = "fixture_prediction_error")
})

test_that("mutations during report computation cannot keep the initial identity", {
  state <- new.env(parent = emptyenv())
  state$scale <- 1
  data <- data.frame(x = seq(-2, 2, length.out = 24))
  data$z <- data$x^2
  data$y <- data$x
  predictor <- function(newdata) {
    if (any(newdata$z != newdata$x^2)) state$scale <- 2
    newdata$x + state$scale * (newdata$z - newdata$x^2)
  }
  result <- evaluate_models(list(model = list(label = "mutating")), data, "y",
    predict_functions = list(model = predictor)
  )
  expect_identical(state$scale, 1)
  expect_error(
    AutoXplainR:::prepare_model_report_data(result, top_features = 1L, n_repeats = 1L),
    "Prediction state changed during explanation computation"
  )
  expect_identical(state$scale, 2)
  expect_identical(predictor(data), data$x)
})

test_that("shared curves preserve missing-row scope and singleton uncertainty", {
  data <- data.frame(
    x = c(NA, 0:10), group = factor(rep(c("b", "a", NA), 4)),
    y = factor(rep(c("z", "x", "y"), 4))
  )
  predictor <- function(newdata) {
    x <- replace(newdata$x, is.na(newdata$x), 0)
    p <- .2 + .01 * x
    cbind(z = p, x = .3, y = .7 - p)
  }
  result <- evaluate_models(list(model = list(label = "missing")), data, "y",
    predict_functions = list(model = predictor)
  )
  context <- AutoXplainR:::prepare_report_context(result)
  explainer <- context$explainers$model
  set.seed(7)
  initial_rng <- .Random.seed
  for (feature in c("x", "group")) {
    method <- if (feature == "x") "ale" else "pdp"
    bundle <- AutoXplainR:::explain_effect_bundle(
      explainer, feature, method,
      10L, 81L, 9L, explainer$class_levels, context
    )
    for (class in explainer$class_levels) {
      independent <- explain_effect(explainer,
        feature = feature, method = method,
        n_points = 10L, seed = 81L, max_rows = 9L, class = class
      )
      expect_identical(bundle[[class]], independent)
      if (method == "ale") expect_true(all(is.na(independent$std_error)))
    }
  }
  expect_identical(.Random.seed, initial_rng)
})

test_that("shared-context completion detects lexical changes invisible at baseline", {
  state <- new.env(parent = emptyenv())
  state$scale <- 1
  data <- data.frame(x = seq(-2, 2, length.out = 24))
  data$z <- data$x^2
  data$y <- data$x
  predictor <- function(newdata) newdata$x + state$scale * (newdata$z - newdata$x^2)
  result <- evaluate_models(list(model = list(label = "lexical")), data, "y",
    predict_functions = list(model = predictor)
  )
  context <- AutoXplainR:::prepare_report_context(result)
  explainer <- context$explainers$model
  initial <- explain_effect(explainer, "x", n_points = 4)
  before <- predict(explainer, explainer$data)
  state$scale <- 2
  expect_identical(predict(explainer, explainer$data), before)
  expect_error(AutoXplainR:::validate_explanation_context(context), "Prediction state changed")
  fresh <- explain_effect(explainer, "x", n_points = 4)
  expect_false(identical(attr(initial, "explainer_fingerprint"), attr(fresh, "explainer_fingerprint")))
  expect_false(identical(initial$accumulated_effect, fresh$accumulated_effect))
  changed <- explainer
  changed$data <- changed$data[rev(seq_len(nrow(data))), ]
  expect_error(AutoXplainR:::explanation_context_values(changed, context), "does not match")
})

test_that("shared ALE matches independent local differences and bin uncertainty", {
  data <- data.frame(x = rep(c(0, 1, 2, 3), each = 4), z = seq_len(16) / 8)
  data$y <- data$x * data$z + .25 * data$x^2
  predictor <- function(newdata) newdata$x * newdata$z + .25 * newdata$x^2
  result <- evaluate_models(list(model = list(label = "analytic")), data, "y",
    predict_functions = list(model = predictor)
  )
  context <- AutoXplainR:::prepare_report_context(result)
  effect <- AutoXplainR:::explain_effect_bundle(
    context$explainers$model, "x", "ale",
    2L, 31L, NULL, NA_character_, context
  )[[1L]]
  boundaries <- c(0, 1, 3)
  bins <- ifelse(data$x <= 1, 1L, 2L)
  lower <- boundaries[bins]
  upper <- boundaries[bins + 1L]
  local <- (upper - lower) * data$z + .25 * (upper^2 - lower^2)
  bin_mean <- vapply(1:2, function(index) mean(local[bins == index]), numeric(1))
  bin_se <- vapply(1:2, function(index) sd(local[bins == index]) / sqrt(sum(bins == index)), numeric(1))
  fractions <- (data$x - lower) / (upper - lower)
  centering <- c(mean((bins > 1) + (bins == 1) * fractions), mean((bins == 2) * fractions))
  coefficients <- rbind(c(0, 0), c(1, 0), c(1, 1)) - matrix(centering, 3, 2, byrow = TRUE)
  expected <- as.numeric(coefficients %*% bin_mean)
  se <- sqrt(rowSums(sweep(coefficients, 2, bin_se, "*")^2))
  expect_identical(attr(effect, "bin_boundaries"), boundaries)
  expect_equal(effect$accumulated_effect, expected, tolerance = 1e-15)
  expect_equal(effect$std_error, se, tolerance = 1e-15)
  expect_equal(effect$conf_low, expected - qnorm(.975) * se, tolerance = 1e-15)
  expect_equal(effect$conf_high, expected + qnorm(.975) * se, tolerance = 1e-15)
})

test_that("native forest and boosting class bundles preserve independent curves", {
  skip_if_package_unavailable("ranger")
  skip_if_package_unavailable("xgboost")
  for (task in c("regression", "binary", "multiclass")) {
    data <- effect_computation_fixture(task)$test_data
    forest_parameters <- list(
      num.trees = 24L, mtry = 2L, min.node.size = 3L,
      sample.fraction = .8, splitrule = "default"
    )
    boosting_parameters <- AutoXplainR:::boosting_learner_grid(nrow(data), 3L, task, 3L)[[1L]]
    boosting_parameters$nrounds <- 6L
    models <- list(
      forest = AutoXplainR:::fit_forest_learner(data, "y", task, forest_parameters, 39L),
      boosting = AutoXplainR:::fit_boosting_learner(data, "y", task, boosting_parameters, 39L)
    )
    result <- evaluate_models(models, data, "y", task = task)
    context <- AutoXplainR:::prepare_report_context(result)
    for (explainer in context$explainers) {
      classes <- if (task == "multiclass") explainer$class_levels else NA_character_
      for (feature in c("x", "group")) {
        method <- if (feature == "x") "ale" else "pdp"
        bundled <- AutoXplainR:::explain_effect_bundle(
          explainer, feature, method,
          4L, 5L, 21L, classes, context
        )
        for (index in seq_along(classes)) {
          independent <- explain_effect(explainer,
            feature = feature, method = method,
            n_points = 4L, seed = 5L, max_rows = 21L,
            class = if (task == "multiclass") classes[[index]] else NULL
          )
          expect_identical(bundled[[index]], independent)
        }
      }
    }
    expect_silent(AutoXplainR:::validate_explanation_context(context))
  }
})
