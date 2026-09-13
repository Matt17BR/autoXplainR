native_progress_data <- function() {
  x <- seq(-2, 2, length.out = 220L)
  data.frame(x = x, z = sin(seq_along(x)), y = sin(x) + cos(seq_along(x)))
}

native_progress_fit <- function(verbosity, threads = 1L) {
  autoxplain(
    native_progress_data(), "y", learners = "forest", max_models = 1L,
    nfolds = 2L, seed = 73L, verbosity = verbosity, explain = FALSE,
    tuning_control = tuning_control(
      search = "adaptive", screening_rows = 120L,
      threads = threads
    )
  )
}

test_that("info reaches native screening, CV and full refit without statistical changes", {
  skip_if_package_unavailable("ranger")
  withr::local_preserve_seed()
  actual <- AutoXplainR:::fit_forest_learner
  captured <- list()
  local_mocked_bindings(fit_forest_learner = function(data, target, task, parameters, seed, progress = FALSE) {
    model <- actual(data, target, task, parameters, seed, progress = progress)
    scope <- attr(parameters, "autoxplain_fit_scope")
    if (is.null(scope)) scope <- "full_training_refit"
    captured[[length(captured) + 1L]] <<- list(progress = progress, scope = scope, model = model)
    model
  }, .package = "AutoXplainR")
  for (threads in c(1L, 4L)) {
    captured <- list()
    set.seed(171L)
    quiet <- suppressMessages(native_progress_fit("quiet", threads))
    quiet_rng <- .Random.seed
    quiet_native <- captured
    captured <- list()
    set.seed(171L)
    info <- withCallingHandlers(native_progress_fit("info", threads), message = function(condition) {
      stats::runif(1L)
      invokeRestart("muffleMessage")
    })
    expect_identical(.Random.seed, quiet_rng)
    expect_identical(info$tuning$plan, quiet$tuning$plan)
    expect_identical(info$tuning$fold_assignment, quiet$tuning$fold_assignment)
    expect_identical(info$tuning$fold_scores$fit_seed, quiet$tuning$fold_scores$fit_seed)
    expect_identical(info$tuning$fold_scores$score, quiet$tuning$fold_scores$score)
    expect_identical(info$tuning$out_of_fold_predictions, quiet$tuning$out_of_fold_predictions)
    expect_identical(predict(info, native_progress_data()), predict(quiet, native_progress_data()))
    expect_setequal(
      vapply(captured, `[[`, character(1), "scope"),
      c("screening", "resampling_fold", "full_training_refit")
    )
    expect_length(captured, length(quiet_native))
    for (i in seq_along(captured)) {
      left <- captured[[i]]$model
      right <- quiet_native[[i]]$model
      expect_true(captured[[i]]$progress)
      expect_false(quiet_native[[i]]$progress)
      expect_true(left$fit$call$verbose)
      expect_false(right$fit$call$verbose)
      expect_identical(left$fit$call$num.threads, threads)
      expect_identical(left$parameters, right$parameters)
      expect_identical(left$seed, right$seed)
      expect_identical(left$fit_details, right$fit_details)
      expect_identical(left$fit$forest, right$fit$forest)
      expect_identical(left$fit$predictions, right$fit$predictions)
      expect_identical(left$fit$prediction.error, right$fit$prediction.error)
      expect_false("progress" %in% names(left$parameters))
      expect_null(attr(left$parameters, "autoxplain_progress"))
    }
  }
})

test_that("legacy custom learner and refit signatures remain usable", {
  skip_if_package_unavailable("ranger")
  actual <- AutoXplainR:::fit_forest_learner
  called <- FALSE
  local_mocked_bindings(fit_forest_learner = function(data, target, task, parameters, seed) {
    called <<- TRUE
    actual(data, target, task, parameters, seed)
  }, .package = "AutoXplainR")
  result <- suppressMessages(native_progress_fit("info"))
  expect_true(called)
  refitted <- FALSE
  legacy <- function(configuration, data, target, task) {
    refitted <<- TRUE
    AutoXplainR:::fit_tuning_configuration(configuration, data, target, task)
  }
  fit <- suppressMessages(
    AutoXplainR:::refit_tuned_candidates(
      result$tuning, native_progress_data(), "y", "regression", fitter = legacy, progress = TRUE
    )
  )
  expect_true(refitted)
  expect_true(length(fit$fits) > 0L)
})

test_that("native-fit failure preserves caller RNG and the next quiet policy", {
  skip_if_package_unavailable("ranger")
  withr::local_preserve_seed()
  observed <- logical()
  local_mocked_bindings(fit_forest_learner = function(data, target, task, parameters, seed, progress = FALSE) {
    observed <<- c(observed, progress)
    stats::runif(1L)
    stop("deliberate native progress test failure")
  }, .package = "AutoXplainR")
  for (has_seed in c(TRUE, FALSE)) {
    set.seed(175L)
    before <- .Random.seed
    if (!has_seed) rm(".Random.seed", envir = .GlobalEnv)
    expect_error(suppressMessages(native_progress_fit("info")), "deliberate native progress test failure")
    if (has_seed) expect_identical(.Random.seed, before) else expect_false(exists(".Random.seed", .GlobalEnv))
    expect_true(tail(observed, 1L))
    expect_error(suppressMessages(native_progress_fit("quiet")), "deliberate native progress test failure")
    expect_false(tail(observed, 1L))
    if (has_seed) expect_identical(.Random.seed, before) else expect_false(exists(".Random.seed", .GlobalEnv))
  }
})
