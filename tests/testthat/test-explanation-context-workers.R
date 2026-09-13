make_context_worker_fixture <- function(task = "regression") {
  withr::local_seed(819)
  data <- data.frame(x1 = rnorm(60), x2 = rnorm(60), block = rep(c("a", "b"), 30))
  if (task == "regression") {
    data$y <- 2 * data$x1 + data$x2 + rnorm(60)
    models <- list(first = lm(y ~ x1 + x2, data), second = lm(y ~ x1, data))
    functions <- NULL
  } else if (task == "binary") {
    data$y <- factor(ifelse(runif(60) < plogis(data$x1), "yes", "no"), levels = c("no", "yes"))
    models <- list(
      first = glm(y ~ x1 + x2, data, family = binomial()),
      second = glm(y ~ x1, data, family = binomial())
    )
    functions <- NULL
  } else {
    data$y <- factor(rep(c("a", "b", "c"), 20))
    models <- list(first = list(scale = .5), second = list(scale = .4))
    adapter <- function(model, newdata) {
      values <- cbind(
        a = exp(model$scale * newdata$x1),
        b = exp(model$scale * newdata$x2), c = rep(1, nrow(newdata))
      )
      values / rowSums(values)
    }
    functions <- list(first = adapter, second = adapter)
  }
  result <- evaluate_models(
    models, data, "y", task = task,
    positive = if (task == "binary") "no" else NULL, predict_functions = functions
  )
  AutoXplainR:::prepare_report_context(result)
}

test_that("private importance preserves values, groups, blocking, sampling and RNG", {
  for (task in c("regression", "binary", "multiclass")) {
    context <- make_context_worker_fixture(task)
    explainer <- context$explainers[[1L]]
    arguments <- list(
      model = explainer, feature_groups = list(joint = c("x1", "x2")),
      within = "block", n_repeats = 4L, max_rows = 19L, seed = 83L, sample_seed = 71L
    )
    withr::local_seed(231)
    before <- .Random.seed
    fresh <- do.call(calculate_permutation_importance, arguments)
    reused <- do.call(
      AutoXplainR:::calculate_permutation_importance_impl,
      c(arguments, list(prediction_context = context))
    )
    expect_identical(.Random.seed, before)
    expect_identical(reused, fresh)
    expect_identical(attr(reused, "sampling")$rows_used, 19L)
    expect_identical(attr(reused, "feature_groups"), list(joint = c("x1", "x2")))
  }
})

test_that("private audit preserves complete statistical evidence for every task", {
  for (task in c("regression", "binary", "multiclass")) {
    context <- make_context_worker_fixture(task)
    arguments <- list(
      explainers = context$explainers, features = c("x1", "x2"),
      n_repeats = 4L, max_rows = 19L, seed = 17L, performance_tolerance = 1
    )
    fresh <- do.call(audit_explanations, arguments)
    reused <- do.call(
      AutoXplainR:::audit_explanations_impl,
      c(arguments, list(prediction_context = context))
    )
    fresh$provenance$created_at <- reused$provenance$created_at <- NULL
    expect_identical(reused, fresh)
    expected <- vapply(context$explainers, function(explainer) {
      AutoXplainR:::metric_score(
        explainer$y, context$predictions[[explainer$label]], reused$config$metric, explainer
      )
    }, numeric(1))
    expect_identical(reused$performance$score, unname(expected))
  }
})

test_that("context reuse removes full batches while keeping each perturbation intact", {
  context <- make_context_worker_fixture()
  original <- AutoXplainR:::predict.autoxplain_explainer
  calls <- integer()
  local_mocked_bindings(predict.autoxplain_explainer = function(object, newdata, ...) {
    calls <<- c(calls, nrow(newdata))
    original(object, newdata, ...)
  }, .package = "AutoXplainR")
  arguments <- list(
    explainers = context$explainers, features = c("x1", "x2"),
    n_repeats = 4L, max_rows = 15L, seed = 9L, performance_tolerance = 1
  )
  fresh <- do.call(audit_explanations, arguments)
  expect_equal(sum(calls == 15L), 16L)
  expect_equal(sum(calls == 60L), 8L)
  calls <- integer()
  reused <- do.call(
    AutoXplainR:::audit_explanations_impl,
    c(arguments, list(prediction_context = context))
  )
  expect_identical(calls, rep(15L, 16L))
  AutoXplainR:::validate_explanation_context(context)
  expect_identical(tail(calls, 2L), c(60L, 60L))
  fresh$provenance$created_at <- reused$provenance$created_at <- NULL
  expect_identical(reused, fresh)
})

test_that("context workers reject replacement rows and changed model state", {
  context <- make_context_worker_fixture()
  original <- context$explainers[[1L]]
  changed <- original
  changed$data <- changed$data[60:1, , drop = FALSE]
  changed$y <- changed$y[60:1]
  expect_error(
    AutoXplainR:::calculate_permutation_importance_impl(
      changed, n_repeats = 2L, prediction_context = context
    ), "does not match"
  )
  changed <- original
  changed$model$coefficients[[1L]] <- changed$model$coefficients[[1L]] + 1
  expect_error(
    AutoXplainR:::calculate_permutation_importance_impl(
      changed, n_repeats = 2L, prediction_context = context
    ), "does not match"
  )
  replacement <- original$data
  replacement$y <- original$y + 1
  expect_error(
    AutoXplainR:::calculate_permutation_importance_impl(
      original, data = replacement, target_column = "y", n_repeats = 2L,
      prediction_context = context
    ), "does not match"
  )
})

test_that("batch-dependent prediction contracts retain their original permutation batches", {
  data <- data.frame(x = seq_len(40), z = sin(seq_len(40)), y = cos(seq_len(40)))
  model <- lm(y ~ I(x - mean(x)) + z, data)
  result <- evaluate_models(list(centered = model), data, "y")
  context <- AutoXplainR:::prepare_report_context(result)
  explainer <- context$explainers[[1L]]
  fresh <- calculate_permutation_importance(
    explainer, features = c("x", "z"), n_repeats = 4L, max_rows = 13L, seed = 11L
  )
  reused <- AutoXplainR:::calculate_permutation_importance_impl(
    explainer,
    features = c("x", "z"), n_repeats = 4L, max_rows = 13L, seed = 11L,
    prediction_context = context
  )
  expect_identical(reused, fresh)
})

test_that("public importance ignores an old context and recomputes mutable predictions", {
  state <- new.env(parent = emptyenv())
  state$coefficient <- 2
  predictor <- local({
    values <- state
    function(newdata) values$coefficient * newdata$x
  })
  data <- data.frame(x = seq_len(30), y = 2 * seq_len(30))
  result <- evaluate_models(
    list(custom = list()), data, "y", predict_functions = list(custom = predictor)
  )
  context <- AutoXplainR:::prepare_report_context(result)
  explainer <- context$explainers[[1L]]
  first <- calculate_permutation_importance(explainer, n_repeats = 3L, seed = 8L)
  state$coefficient <- 3
  expect_error(AutoXplainR:::validate_explanation_context(context), "Prediction state changed")
  attr(explainer, "prediction_context") <- context
  second <- calculate_permutation_importance(explainer, n_repeats = 3L, seed = 8L)
  expect_equal(attr(second, "full_baseline_score"), sqrt(mean(data$x^2)))
  expect_false(identical(attr(first, "explainer_fingerprint"), attr(second, "explainer_fingerprint")))
})
