test_that("first reports cannot mix constructor scores with changed supplied or guided models", {
  data <- data.frame(x = seq(-1, 1, length.out = 12))
  data$y <- data$x
  predictor <- function(model, newdata) model$coefficient * newdata$x
  original <- evaluate_models(list(fit = list(coefficient = 1)), data, "y",
    predict_functions = list(fit = predictor)
  )
  expect_null(original$explanations)
  expect_equal(original$leaderboard$rmse, 0)
  copy <- unserialize(serialize(original, NULL))
  expect_silent(AutoXplainR:::validate_evaluation_snapshot(copy))
  copy$models$fit$coefficient <- 3
  destination <- tempfile(fileext = ".html")
  expect_error(render_model_report(copy, destination), "Stored evaluation evidence")
  expect_false(file.exists(destination))
  expect_error(evidence_summary(copy), "Stored evaluation evidence")
  expect_error(as_explainers(copy), "Stored evaluation evidence")
  expect_error(print(copy), "Stored evaluation evidence")
  expect_error(summary(copy), "Stored evaluation evidence")
  refreshed <- evaluate_models(copy$models, data, "y", predict_functions = list(fit = predictor))
  expect_equal(refreshed$leaderboard$rmse, sqrt(mean((data$y - 3 * data$x)^2)))
  expect_silent(AutoXplainR:::validate_evaluation_snapshot(refreshed))

  guided <- autoxplain(mtcars, "mpg", model_set = "quick", explain = FALSE)
  guided$models$main_model$coefficients[[1L]] <- guided$models$main_model$coefficients[[1L]] + 3
  expect_error(render_model_report(guided, destination), "Stored evaluation evidence")
})

test_that("seals bind event, class, data and selection contracts but permit derived benchmark costs", {
  data <- data.frame(x = 1:6, y = factor(rep(c("no", "yes"), 3), levels = c("no", "yes")))
  predictor <- function(model, newdata) rep(model, nrow(newdata))
  original <- evaluate_models(list(a = .6, b = .4), data, "y", reference = "b",
    predict_functions = list(a = predictor, b = predictor)
  )
  mutations <- list(
    function(x) {
      x$prediction_schema$positive <- "no"
      x
    },
    function(x) {
      x$prediction_schema$class_levels <- rev(x$prediction_schema$class_levels)
      x
    },
    function(x) {
      x$prediction_contracts$a$positive <- "no"
      x
    },
    function(x) {
      x$provenance$primary_model_id <- "b"
      x
    },
    function(x) {
      x$provenance$reference_model_id <- NULL
      x
    },
    function(x) {
      x$test_data <- x$test_data[6:1, ]
      x
    },
    function(x) {
      x$test_data$y <- rev(x$test_data$y)
      x
    },
    function(x) {
      x$leaderboard$log_loss <- 0
      x
    },
    function(x) {
      x$leaderboard$prediction_time_ms <- 0
      x
    },
    function(x) {
      x$preprocessing_metadata$enabled <- TRUE
      x
    },
    function(x) {
      x$validation$evaluation_groups <- rep("same", 6)
      x
    },
    function(x) {
      x$provenance$test_used_for_validation <- TRUE
      x
    },
    function(x) {
      x$tuning <- list(selection = "invented")
      x
    },
    function(x) {
      x$model_diagnostics$training_time_ms <- 100
      x
    },
    function(x) {
      x$provenance$workflow <- "invented training workflow"
      x
    },
    function(x) {
      x$provenance$candidate_selection <- "The test set selected this model."
      x
    },
    function(x) {
      x$provenance$training_rows <- 1000L
      x
    },
    function(x) {
      x$engine <- "base"
      x
    },
    function(x) {
      x$leaderboard$rank <- x$leaderboard$rank + 1
      x
    },
    function(x) {
      x$evaluation$beats_baseline <- !x$evaluation$beats_baseline
      x
    }
  )
  for (change in mutations) {
    expect_error(AutoXplainR:::prepare_report_context(change(original)), "Stored evaluation evidence")
  }
  original$leaderboard$repeated_prediction_ms_per_row <- c(.2, .3)
  original$provenance$target_units <- "units"
  original$provenance$package_version <- "presentation-only version"
  original$provenance$primary_model_label <- "Readable label"
  expect_silent(AutoXplainR:::validate_evaluation_snapshot(original))
})

test_that("mutable state in model attributes is part of the prediction identity", {
  data <- data.frame(x = seq(-1, 1, length.out = 10))
  data$z <- data$x
  data$y <- data$x
  predictor <- function(model, newdata) {
    newdata$x + attr(model, "state")$coefficient * (newdata$x - newdata$z)
  }
  for (model in list(list(), 1, function() NULL, new.env(parent = emptyenv()))) {
    state <- new.env(parent = emptyenv())
    state$coefficient <- 0
    attr(model, "state") <- state
    result <- evaluate_models(list(fit = model), data, "y", predict_functions = list(fit = predictor))
    explainer <- as_explainers(result)[[1L]]
    initial <- explainer$provenance$fingerprint
    state$coefficient <- 10
    expect_equal(predict(explainer, data), data$y)
    expect_false(identical(AutoXplainR:::current_explainer_fingerprint(explainer), initial))
    expect_error(render_model_report(result, tempfile(fileext = ".html")), "Stored evaluation evidence")
  }
})

test_that("unsealed legacy scores are checked against cached predictions and outcomes", {
  result <- autoxplain(mtcars, "mpg", model_set = "quick", explain = FALSE)
  result$.evaluation_snapshot <- NULL
  expect_silent(AutoXplainR:::prepare_report_context(result))
  changed <- result
  changed$models$main_model$coefficients[[1L]] <- changed$models$main_model$coefficients[[1L]] + 2
  expect_error(AutoXplainR:::prepare_report_context(changed), "legacy evaluation evidence")
  changed <- result
  changed$test_data <- changed$test_data[rev(seq_len(nrow(changed$test_data))), ]
  expect_error(AutoXplainR:::prepare_report_context(changed), "ordered observed")
  changed <- result
  changed$test_data$mpg <- changed$test_data$mpg + 1
  expect_error(AutoXplainR:::prepare_report_context(changed), "ordered observed")
  result$evaluation$predictions <- NULL
  expect_silent(AutoXplainR:::prepare_report_context(result))
  result$leaderboard$rmse <- 0
  expect_error(AutoXplainR:::prepare_report_context(result), "legacy evaluation evidence.*rmse")
})

test_that("closure state invalidates evidence when evaluation predictions are unchanged", {
  data <- data.frame(x = seq(-2, 2, length.out = 24))
  data$z <- data$x
  data$y <- data$x
  state <- new.env(parent = emptyenv())
  state$coefficient <- 0
  helper <- function(newdata) newdata$x + state$coefficient * (newdata$x - newdata$z)
  predictor <- function(model, newdata) helper(newdata)
  result <- evaluate_models(list(fit = list()), data, "y", predict_functions = list(fit = predictor))
  explainer <- as_explainers(result)[[1L]]
  fingerprint <- AutoXplainR:::current_explainer_fingerprint(explainer)
  initial <- calculate_permutation_importance(explainer, features = c("x", "z"), n_repeats = 2, seed = 2)
  result$explanations <- AutoXplainR:::prepare_model_report_data(result, top_features = 2, n_repeats = 2)
  state$coefficient <- 10
  expect_equal(predict(explainer, data), data$y)
  expect_false(identical(AutoXplainR:::current_explainer_fingerprint(explainer), fingerprint))
  expect_error(render_model_report(result, tempfile(fileext = ".html")), "Stored evaluation evidence")
  changed <- calculate_permutation_importance(explainer, features = c("x", "z"), n_repeats = 2, seed = 2)
  expect_equal(initial$importance[initial$feature == "z"], 0)
  expect_gt(changed$importance[changed$feature == "z"], 1)
  expect_gt(changed$importance[changed$feature == "x"], initial$importance[initial$feature == "x"])
})

test_that("native formula transforms bind lexical state without capturing unrelated session objects", {
  data <- data.frame(x = seq(-2, 2, length.out = 30))
  data$z <- cos(data$x)
  data$y <- 2 * data$x + data$z
  state <- new.env(parent = emptyenv())
  state$coefficient <- 1
  fitted <- lm(y ~ x + I(state$coefficient * (x - z)), data)
  evaluation <- transform(data, z = x)
  result <- evaluate_models(list(fit = fitted), evaluation, "y", features = c("x", "z"))
  explainer <- as_explainers(result)[[1L]]
  before <- predict(explainer, evaluation)
  fingerprint <- AutoXplainR:::current_explainer_fingerprint(explainer)
  unrelated_object <- runif(10)
  expect_length(unrelated_object, 10)
  expect_silent(AutoXplainR:::validate_evaluation_snapshot(result))
  expect_identical(AutoXplainR:::current_explainer_fingerprint(explainer), fingerprint)
  roundtrip <- unserialize(serialize(result, NULL))
  expect_silent(AutoXplainR:::validate_evaluation_snapshot(roundtrip))
  state$coefficient <- 10
  expect_equal(predict(explainer, evaluation), before)
  expect_false(identical(AutoXplainR:::current_explainer_fingerprint(explainer), fingerprint))
  expect_error(render_model_report(result, tempfile(fileext = ".html")), "Stored evaluation evidence")
})

test_that("unsupported dynamic and active context is explicit", {
  dynamic <- function(newdata) get("slope") * newdata$x
  expect_error(AutoXplainR:::prediction_function_context(dynamic), "dynamic.*get")
  alias <- local({
    lookup <- get
    function(newdata) lookup("slope") * newdata$x
  })
  expect_error(AutoXplainR:::prediction_function_context(alias), "alias of a dynamic")
  reads <- 0L
  state <- new.env(parent = emptyenv())
  makeActiveBinding("coefficient", function() {
    reads <<- reads + 1L
    2
  }, state)
  active <- function(newdata) state$coefficient * newdata$x
  expect_error(AutoXplainR:::prediction_function_context(active), "active binding")
  expect_identical(reads, 0L)
  stochastic <- function(newdata) rnorm(nrow(newdata))
  expect_error(AutoXplainR:::prediction_function_context(stochastic), "dynamic or external call.*rnorm")
})

test_that("current predictions cannot replace the official assessment even with unchanged context", {
  data <- data.frame(x = 1:10, y = 2 * (1:10))
  result <- evaluate_models(list(fit = lm(y ~ x, data)), data, "y")
  explainers <- as_explainers(result)
  explainers$fit$reference_predictions <- explainers$fit$reference_predictions + 1
  expect_error(AutoXplainR:::validate_recorded_evaluation(result, explainers), "ordered primary_prediction")
})

test_that("custom S3 prediction dispatch binds its referenced state off the evaluation manifold", {
  data <- data.frame(x = seq(-1, 1, length.out = 20))
  data$z <- data$x
  data$y <- data$x
  state <- new.env(parent = emptyenv())
  state$coefficient <- 0
  method <- function(object, newdata, ...) {
    newdata$x + state$coefficient * (newdata$x - newdata$z)
  }
  registry <- get(".__S3MethodsTable__.", envir = asNamespace("stats"))
  registerS3method("predict", "axr_stateful", method, envir = asNamespace("stats"))
  withr::defer(rm("predict.axr_stateful", envir = registry))
  fitted <- structure(list(), class = "axr_stateful")
  result <- evaluate_models(list(fit = fitted), data, "y")
  explainer <- as_explainers(result)[[1L]]
  initial <- explainer$provenance$fingerprint
  roundtrip <- unserialize(serialize(result, NULL))
  expect_silent(AutoXplainR:::validate_evaluation_snapshot(roundtrip))
  expect_equal(predict(roundtrip, data), data$y)
  state$coefficient <- 10
  expect_equal(predict(explainer, data), data$y)
  expect_false(identical(AutoXplainR:::current_explainer_fingerprint(explainer), initial))
  expect_error(render_model_report(result, tempfile(fileext = ".html")), "Stored evaluation evidence")
})

test_that("native response links and separate offset expressions bind their executing state", {
  training <- data.frame(x = seq(-2, 2, length.out = 40), z = cos(seq(-2, 2, length.out = 40)))
  training$y <- factor(rep(c("no", "yes"), 20), levels = c("no", "yes"))
  state <- new.env(parent = emptyenv())
  state$slope <- 1
  fitted <- glm(y ~ x + z, training, family = binomial())
  fitted$coefficients <- c("(Intercept)" = 0, x = 1, z = -1)
  fitted$family$linkinv <- function(eta) plogis(state$slope * eta)
  evaluation <- transform(training, z = x)
  result <- evaluate_models(list(fit = fitted), evaluation, "y")
  explainer <- as_explainers(result)[[1L]]
  off_grid <- transform(evaluation, z = x + 1)
  expect_equal(predict(explainer, off_grid), rep(plogis(-1), nrow(evaluation)))
  state$slope <- 10
  expect_equal(predict(explainer, evaluation), explainer$reference_predictions)
  expect_equal(predict(explainer, off_grid), rep(plogis(-10), nrow(evaluation)))
  expect_error(render_model_report(result, tempfile(fileext = ".html")), "Stored evaluation evidence")

  training$y <- 2 * training$x + training$z
  state$slope <- 1
  # Older predict.lm() resolves separate offsets outside the formula environment.
  # Use state native prediction can resolve on every supported R version, while
  # still changing only its off-grid behavior and requiring stale evidence to fail.
  had_offset_state <- exists(".axr_offset_identity_test", envir = globalenv(), inherits = FALSE)
  previous_offset_state <- get0(".axr_offset_identity_test", envir = globalenv(), inherits = FALSE)
  withr::defer({
    if (had_offset_state) {
      assign(".axr_offset_identity_test", previous_offset_state, envir = globalenv())
    } else {
      rm(".axr_offset_identity_test", envir = globalenv())
    }
  })
  assign(".axr_offset_identity_test", state, envir = globalenv())
  fitted <- lm(y ~ x + z, training, offset = .axr_offset_identity_test$slope * (x - z))
  evaluation <- transform(training, z = x)
  expect_equal(unname(stats::predict(fitted, evaluation)), 3 * evaluation$x)
  result <- evaluate_models(list(fit = fitted), evaluation, "y")
  explainer <- as_explainers(result)[[1L]]
  before <- predict(explainer, off_grid)
  state$slope <- 10
  expect_equal(predict(explainer, evaluation), explainer$reference_predictions)
  expect_equal(predict(explainer, off_grid) - before, rep(-9, nrow(evaluation)))
  expect_error(render_model_report(result, tempfile(fileext = ".html")), "Stored evaluation evidence")
})

test_that("native prediction contrast functions retain their lexical identity", {
  state <- new.env(parent = emptyenv())
  state$slope <- 1
  contrast <- function(n, contrasts = TRUE, sparse = FALSE) {
    stats::contr.treatment(n, contrasts = contrasts, sparse = sparse) * state$slope
  }
  data <- data.frame(x = 1:12, group = factor(rep(c("a", "b", "c"), 4)))
  data$y <- data$x + c(a = 0, b = 1, c = 2)[as.character(data$group)]
  fitted <- lm(y ~ x + group, data)
  evaluation <- data
  evaluation$group <- factor("a", levels = levels(data$group))
  assign(".axr_contrast_identity_test", contrast, envir = globalenv())
  withr::defer(rm(".axr_contrast_identity_test", envir = globalenv()))
  for (choice in list(contrast, ".axr_contrast_identity_test")) {
    state$slope <- 1
    fitted$contrasts <- list(group = choice)
    result <- evaluate_models(list(fit = fitted), evaluation, "y")
    explainer <- as_explainers(result)[[1L]]
    before <- predict(explainer, data)
    state$slope <- 10
    expect_equal(predict(explainer, evaluation), explainer$reference_predictions)
    expect_gt(max(abs(predict(explainer, data) - before)), 10)
    expect_error(render_model_report(result, tempfile(fileext = ".html")), "Stored evaluation evidence")
  }
})

test_that("captured fitted models resolve predictor names in newdata rather than ambient bindings", {
  x <- 1
  data <- data.frame(x = 1:10, y = 2 * (1:10))
  fit <- lm(y ~ x, data)
  predictor <- function(newdata) predict(fit, newdata)
  result <- evaluate_models(list(captured = list()), data, "y", predict_functions = list(captured = predictor))
  initial <- as_explainers(result)[[1L]]$provenance$fingerprint
  x <- 200
  expect_equal(x, 200)
  expect_equal(predict(result, data), data$y)
  expect_silent(AutoXplainR:::validate_evaluation_snapshot(result))
  expect_identical(as_explainers(result)[[1L]]$provenance$fingerprint, initial)
})

test_that("automatic supplied task inference preserves unobserved declared classes", {
  predictor <- function(model, newdata) rep(.2, nrow(newdata))
  data <- data.frame(x = 1:5, y = factor(rep("no", 5), levels = c("no", "yes")))
  result <- evaluate_models(list(fit = list()), data, "y", predict_functions = list(fit = predictor))
  expect_identical(result$task, "binary")
  expect_true(is.na(result$leaderboard$roc_auc))
  data$y <- factor(c("a", "b", "a", "b", "a"), levels = c("a", "b", "c"))
  matrix_predictor <- function(model, newdata) {
    matrix(rep(c(.5, .3, .2), each = nrow(newdata)),
      ncol = 3, dimnames = list(NULL, c("a", "b", "c"))
    )
  }
  result <- evaluate_models(list(fit = list()), data, "y", predict_functions = list(fit = matrix_predictor))
  expect_identical(result$task, "multiclass")
  expect_identical(colnames(predict(result, data)), c("a", "b", "c"))
})
