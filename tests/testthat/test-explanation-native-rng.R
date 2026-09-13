native_rng_fixture <- function(task) {
  data <- data.frame(x = seq(-2, 2, length.out = 48L))
  data$z <- data$x^2
  data$y <- switch(task,
    regression = sin(data$x) + .1 * data$z,
    binary = factor(rep(c("no", "yes"), 24L), levels = c("no", "yes")),
    multiclass = factor(rep(c("a", "b", "c"), 16L), levels = c("a", "b", "c"))
  )
  parameters <- list(
    num.trees = 11L, mtry = 2L, min.node.size = 3L,
    sample.fraction = .8, splitrule = "default"
  )
  attr(parameters, "autoxplain_threads") <- 1L
  model <- fit_forest_learner(data, "y", task, parameters, 9181L)
  explain_model(model, data, "y",
    task = task,
    positive = if (task == "binary") "no" else NULL
  )
}

native_rng_calls <- function(explainer, private = FALSE) {
  importance <- if (private) calculate_permutation_importance_impl else calculate_permutation_importance
  audit <- if (private) audit_explanations_impl else audit_explanations
  effect <- if (private) explain_effect_impl else explain_effect
  list(
    importance = function() {
      importance(explainer,
        features = c("x", "z"), n_repeats = 3L,
        max_rows = 13L, seed = 9187L
      )
    },
    audit = function() {
      audit(explainer,
        features = c("x", "z"), n_repeats = 3L,
        max_rows = 13L, seed = 9187L
      )
    },
    ale = function() {
      effect(explainer,
        feature = "x", method = "ale", n_points = 4L,
        max_rows = 13L, seed = 9187L, class = if (explainer$task == "multiclass") "b" else NULL
      )
    },
    pdp = function() {
      effect(explainer,
        feature = "x", method = "pdp", n_points = 4L,
        max_rows = 13L, sample_size = 11L, seed = 9187L,
        class = if (explainer$task == "multiclass") "b" else NULL
      )
    }
  )
}

without_rng_audit_timestamp <- function(value) {
  if (inherits(value, "autoxplain_audit")) value$provenance$created_at <- NULL
  value
}

remove_test_rng <- function() {
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
}

test_that("public native explanations preserve caller RNG and complete scalar evidence", {
  skip_if_package_unavailable("ranger")
  withr::local_preserve_seed()
  for (task in c("regression", "binary", "multiclass")) {
    explainer <- native_rng_fixture(task)
    public <- native_rng_calls(explainer)
    scalar <- native_rng_calls(explainer, private = TRUE)
    for (name in names(public)) {
      set.seed(9199L)
      before <- .Random.seed
      expected <- without_rng_audit_timestamp(scalar[[name]]())
      # The native predictor really consumes RNG outside the seeded shuffles.
      # This ensures the public-boundary test would fail without preservation.
      expect_false(identical(before, .Random.seed), info = paste(task, name))
      set.seed(9199L)
      actual <- without_rng_audit_timestamp(public[[name]]())
      expect_identical(.Random.seed, before, info = paste(task, name))
      expect_identical(actual, expected, info = paste(task, name))
      remove_test_rng()
      invisible(public[[name]]())
      expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE), info = paste(task, name))
    }
  }
})

test_that("prediction failures restore native explanation RNG with and without a prior seed", {
  skip_if_package_unavailable("ranger")
  withr::local_preserve_seed()
  for (task in c("regression", "binary", "multiclass")) {
    explainer <- native_rng_fixture(task)
    native_adapter <- explainer$predict_function
    # Run the real native probability/regression prediction before failing on
    # modified rows, so this exercises the native seed draw on the error path.
    explainer$predict_function <- function(newdata) {
      value <- native_adapter(newdata)
      if (any(newdata$z != newdata$x^2)) stop("deliberate failure after native prediction")
      value
    }
    for (call in native_rng_calls(explainer)) {
      set.seed(9203L)
      before <- .Random.seed
      expect_error(call(), "deliberate failure after native prediction")
      expect_identical(.Random.seed, before)
      remove_test_rng()
      expect_error(call(), "deliberate failure after native prediction")
      expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    }
  }
})

test_that("effect aliases inherit the complete public RNG boundary", {
  skip_if_package_unavailable("ranger")
  withr::local_preserve_seed()
  explainer <- native_rng_fixture("multiclass")
  calls <- list(
    function() {
      calculate_partial_dependence(explainer,
        feature = "x", n_points = 4L,
        sample_size = 11L, class = "b"
      )
    },
    function() calculate_accumulated_local_effects(explainer, feature = "x", n_points = 4L, class = "b"),
    function() {
      calculate_partial_dependence_multi(explainer,
        features = c("x", "z"), n_points = 4L,
        sample_size = 11L, class = "b"
      )
    }
  )
  for (call in calls) {
    set.seed(9209L)
    before <- .Random.seed
    invisible(call())
    expect_identical(.Random.seed, before)
    remove_test_rng()
    invisible(call())
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  }
})

test_that("native report preparation and rendering preserve RNG on success and failure", {
  skip_if_package_unavailable("ranger")
  withr::local_preserve_seed()
  paths <- character()
  withr::defer(unlink(paths))
  for (task in c("regression", "binary", "multiclass")) {
    explainer <- native_rng_fixture(task)
    data <- explainer$data
    data$y <- explainer$y
    adapter <- explainer$predict_function
    failing_adapter <- function(newdata) {
      value <- adapter(newdata)
      if (any(newdata$z != newdata$x^2)) stop("deliberate failure after native prediction")
      value
    }
    for (failure in c(FALSE, TRUE)) {
      result <- evaluate_models(list(forest = explainer$model), data, "y",
        task = task,
        positive = explainer$positive,
        predict_functions = if (failure) list(forest = failing_adapter) else NULL
      )
      calls <- list(
        prepare = function() {
          prepare_model_report_data(result,
            top_features = 1L,
            n_repeats = 1L, max_models = 1L, explanation_rows = 13L
          )
        },
        render = function() {
          output <- tempfile(fileext = ".html")
          paths <<- c(paths, output)
          value <- render_model_report(result, output,
            top_features = 1L,
            n_repeats = 1L, max_models = 1L, explanation_rows = 13L,
            uncertainty = FALSE, report_data = "none"
          )
          expect_true(file.exists(output))
          value
        }
      )
      for (call in calls) {
        for (prior_seed in c(TRUE, FALSE)) {
          if (prior_seed) set.seed(9221L) else remove_test_rng()
          before <- if (prior_seed) .Random.seed else NULL
          if (failure) {
            expect_error(call(), "deliberate failure after native prediction")
          } else {
            expect_no_error(call())
          }
          if (prior_seed) {
            expect_identical(.Random.seed, before)
          } else {
            expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
          }
        }
      }
    }
  }
})

test_that("the report RNG boundary preserves omitted-argument evidence reuse", {
  skip_if_package_unavailable("ranger")
  withr::local_preserve_seed()
  explainer <- native_rng_fixture("binary")
  data <- explainer$data
  data$y <- explainer$y
  result <- evaluate_models(list(forest = explainer$model), data, "y",
    task = "binary", positive = "no"
  )
  result$explanations <- prepare_model_report_data(result,
    top_features = 1L,
    n_repeats = 1L, max_models = 1L, explanation_rows = 13L
  )
  local_mocked_bindings(calculate_permutation_importance_impl = function(...) {
    stop("Retained report evidence must not be recomputed")
  })
  path <- tempfile(fileext = ".html")
  withr::defer(unlink(path))
  set.seed(9227L)
  before <- .Random.seed
  expect_no_error(render_model_report(result, path, uncertainty = FALSE, report_data = "none"))
  expect_true(file.exists(path))
  expect_identical(.Random.seed, before)
})
