thread_policy_control <- function(threads = NULL, search = "auto", learners = c("forest", "boosting"), ...) {
  resolve_tuning_control(
    tuning_control(threads = threads, search = search, ...),
    learners, "regression", seq_len(50000L), 2L, NULL, 2L * length(learners), TRUE
  )
}

test_that("automatic native threads respect allocated cores and R check limits", {
  expect_null(tuning_control()$threads)
  expect_output(print(tuning_control()), "automatic per native fit")
  control <- thread_policy_control()
  expect_null(control$threads)
  expect_identical(control$threads_requested, "auto")
  calls <- 0L
  allocated <- 1L
  withr::local_options(list(
    parallelly.availableCores.methods = "custom",
    parallelly.availableCores.custom = function() {
      calls <<- calls + 1L
      allocated
    }
  ))
  # Exercise parallelly's documented allocation hook, not a mocked resolver.
  for (cores in c(1L, 2L, 22L)) {
    allocated <- cores
    before <- calls
    resolved <- resolve_tuning_threads(control, 50000L, 20L, c("forest", "boosting"))
    expect_identical(calls, before + 1L)
    expect_identical(resolved$threads, min(4L, cores))
    expect_identical(resolved$thread_policy$available_cores, cores)
    expect_identical(resolved$thread_policy$requested, "auto")
    expect_true(resolved$thread_policy$eligible)
  }
  withr::local_options(list(parallelly.availableCores.methods = c("custom", "_R_CHECK_LIMIT_CORES_")))
  withr::local_envvar(c(`_R_CHECK_LIMIT_CORES_` = "true"))
  checked <- resolve_tuning_threads(control, 50000L, 20L, c("forest", "boosting"))
  expect_identical(checked$threads, 2L)
  expect_identical(checked$thread_policy$available_cores, 2L)
})

test_that("exact, small and other-family searches keep one automatic native thread", {
  withr::local_options(list(
    parallelly.availableCores.methods = "custom",
    parallelly.availableCores.custom = function() stop("CPU discovery was unnecessary")
  ))
  cases <- list(
    list(control = thread_policy_control(), n = 49999L, p = 20L, learners = c("forest", "boosting")),
    list(
      control = thread_policy_control(search = "grid"), n = 50000L, p = 20L,
      learners = c("forest", "boosting")
    ),
    list(
      control = thread_policy_control(family_budgets = c(forest = 2L, boosting = 2L)),
      n = 50000L, p = 20L, learners = c("forest", "boosting")
    ),
    list(control = thread_policy_control(learners = "linear"), n = 50000L, p = 20L, learners = "linear"),
    list(control = thread_policy_control(grids = list(forest = list(list(
      num.trees = 7L, mtry = 2L, min.node.size = 3L, sample.fraction = .8, splitrule = "default"
    )))), n = 50000L, p = 20L, learners = c("forest", "boosting"))
  )
  for (case in cases) {
    resolved <- do.call(resolve_tuning_threads, case)
    expect_identical(resolved$threads, 1L)
    expect_false(resolved$thread_policy$eligible)
    expect_identical(resolved$thread_policy$available_cores, NA_integer_)
  }
  for (threads in c(1L, 4L)) {
    explicit <- resolve_tuning_threads(thread_policy_control(threads), 50000L, 20L, c("forest", "boosting"))
    expect_identical(explicit$threads, threads)
    expect_identical(explicit$threads_requested, threads)
    expect_identical(explicit$thread_policy$available_cores, NA_integer_)
    expect_match(explicit$thread_policy$reason, "Explicit native thread count retained")
  }
  for (bad in list(0, -1, 1.5, NA_real_, Inf, c(1L, 2L), "auto")) {
    expect_error(tuning_control(threads = bad), "threads")
  }
})

test_that("sparse thread records never partially match the requested setting", {
  sparse <- tuning_control(search = "grid")
  sparse$threads <- NULL
  sparse$threads_requested <- "auto"
  expect_output(print(sparse), "automatic per native fit")
  resolved <- resolve_tuning_control(sparse, "forest", "regression", seq_len(1000L), 2L, 2L, 2L, TRUE)
  expect_null(resolved[["threads", exact = TRUE]])
  expect_identical(resolved$threads_requested, "auto")
  resolved$threads <- NULL
  provenance <- tuning_control_provenance(resolved)
  expect_identical(provenance$threads, 1L)
  expect_identical(provenance$threads_requested, "auto")
  expect_identical(resolve_tuning_threads(resolved, 1000L, 2L, "forest")$threads, 1L)
  resolved$threads_requested <- 2L
  expect_identical(resolve_tuning_threads(resolved, 1000L, 2L, "forest")$threads, 2L)
})

test_that("both native engines reproduce explicit two-thread fits under automatic allocation", {
  skip_if_package_unavailable("ranger")
  skip_if_package_unavailable("xgboost")
  withr::local_options(list(
    parallelly.availableCores.methods = "custom", parallelly.availableCores.custom = function() 2L
  ))
  automatic <- resolve_tuning_threads(thread_policy_control(), 50000L, 20L, c("forest", "boosting"))
  explicit <- resolve_tuning_threads(thread_policy_control(2L), 50000L, 20L, c("forest", "boosting"))
  # Large-input CPU policy is resolved above; native fits below use only 120
  # rows and tiny fixed models. This is adapter parity, not a large benchmark.
  data <- with_preserved_seed(613L, {
    x <- rnorm(120)
    z <- runif(120)
    data.frame(x = x, z = z, y = sin(x) + z)
  })
  grids <- list(
    forest = list(list(
      num.trees = 13L, mtry = 2L, min.node.size = 3L, sample.fraction = .8, splitrule = "default"
    )),
    boosting = list(list(
      nrounds = 7L, eta = .1, max_depth = 2L, min_child_weight = 1,
      subsample = .8, colsample_bytree = 1, reg_alpha = 0, reg_lambda = 1, encoding = "matrix"
    ))
  )
  plan <- local_tuning_plan(2L, nrow(data), 2L, "regression", 1L,
    learners = c("forest", "boosting"), seed = 733L, custom_grids = grids
  )
  for (index in seq_len(nrow(plan))) {
    auto_plan <- explicit_plan <- plan[index, , drop = FALSE]
    auto_plan$threads <- automatic$threads
    explicit_plan$threads <- explicit$threads
    fitted <- fit_tuning_configuration(auto_plan, data, "y", "regression")
    reference <- fit_tuning_configuration(explicit_plan, data, "y", "regression")
    expect_identical(fitted$fit_details$threads, 2L)
    expect_identical(reference$fit_details$threads, 2L)
    expect_equal(predict(fitted, data), predict(reference, data), tolerance = 0)
    record <- attr(fitted, "autoxplain_tuning_fit")
    expected <- attr(reference, "autoxplain_tuning_fit")
    expect_identical(record$threads, 2L)
    expect_identical(record$learned$threads, 2L)
    expect_identical(record$fit_seed, expected$fit_seed)
    expect_identical(record$effective_parameters, expected$effective_parameters)
    expect_identical(record$effective_parameter_key, expected$effective_parameter_key)
    one <- auto_plan
    one$threads <- 1L
    single <- fit_tuning_configuration(one, data, "y", "regression")
    expect_identical(record$fit_seed, attr(single, "autoxplain_tuning_fit")$fit_seed)
    expect_identical(record$effective_parameter_key, attr(single, "autoxplain_tuning_fit")$effective_parameter_key)
    if (fitted$family == "forest") {
      expect_identical(fitted$fit$call$num.threads, 2L)
      expect_identical(fitted$fit$forest, reference$fit$forest)
    } else {
      config <- xgboost::xgb.config(fitted$fit)
      expect_identical(as.integer(config$learner$generic_param$nthread), 2L)
    }
  }
})

test_that("outer-training allocation is resolved once and replay pins its native thread count", {
  skip_if_package_unavailable("ranger")
  # Real large row/column counts exercise orchestration. The stub models only
  # record native arguments, so this test makes no training-time claim.
  data <- as.data.frame(matrix(rep(seq_len(50000L), 20L), nrow = 50000L))
  data$y <- sin(seq_len(50000L))
  calls <- new.env(parent = emptyenv())
  calls$cores <- 0L
  calls$threads <- integer()
  calls$rows <- integer()
  withr::local_options(list(
    parallelly.availableCores.methods = "custom",
    parallelly.availableCores.custom = function() {
      calls$cores <- calls$cores + 1L
      2L
    }
  ))
  local_mocked_bindings(
    fit_forest_learner = function(data, target, task, parameters, seed) {
      threads <- attr(parameters, "autoxplain_threads") %||% 1L
      calls$threads <- c(calls$threads, threads)
      calls$rows <- c(calls$rows, nrow(data))
      new_autoxplain_fitted_model(
        family = "forest", backend = "ranger", task = task,
        fit = structure(list(mean = mean(data[[target]])), class = "ranger"),
        features = setdiff(names(data), target), parameters = parameters, seed = seed,
        fit_details = list(threads = threads)
      )
    },
    make_prediction_adapter = function(model, ...) function(newdata) rep(model$fit$mean, nrow(newdata))
  )
  control <- thread_policy_control(learners = "forest", screening_rows = 100L, retain_oof = FALSE)
  tuning <- tune_supervised_candidates(
    data, "y", "regression", FALSE, list(), 2L, 2L, "best", control = control, seed = 19L, learners = "forest"
  )
  final <- refit_tuned_candidates(tuning, data, "y", "regression")
  expect_identical(calls$cores, 1L)
  expect_identical(calls$threads, rep(2L, 5L))
  expect_identical(calls$rows, c(80L, 80L, 25000L, 25000L, 50000L))
  expect_identical(tuning$control$threads_requested, "auto")
  expect_identical(tuning$control$threads, 2L)
  expect_identical(tuning$resources$threads, 2L)
  expect_identical(tuning$input_policy$threads, tuning$control$thread_policy)
  expect_true(all(final$tuning$plan$threads == 2L))
  replay <- selection_grid_code(list(tuning = final$tuning, target_column = "y"))
  expect_match(replay, "threads = 2L", fixed = TRUE)
  replay_environment <- new.env(parent = environment())
  eval(parse(text = replay), replay_environment)
  expect_identical(replay_environment$control$threads, 2L)
  withr::local_options(list(parallelly.availableCores.custom = function() stop("Replay must not rediscover CPUs")))
  replay_control <- resolve_tuning_control(
    replay_environment$control, "forest", "regression", data$y, 2L, 2L, 2L, TRUE
  )
  replay_control <- resolve_tuning_threads(replay_control, 50000L, 20L, "forest")
  expect_identical(replay_control$threads, 2L)
})
