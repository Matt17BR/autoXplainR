capture_importance_messages <- function(code, consume_rng = FALSE) {
  lines <- character()
  value <- withCallingHandlers(code, message = function(condition) {
    lines <<- c(lines, conditionMessage(condition))
    if (consume_rng) stats::runif(1L)
    invokeRestart("muffleMessage")
  })
  list(value = value, messages = lines)
}

importance_progress_forest <- function(task = "regression") {
  data <- data.frame(
    x = seq(-2, 2, length.out = 48L),
    category = factor(rep(c("a", "b", "c"), 16L))
  )
  data$z <- data$x^2
  data$y <- switch(task,
    regression = sin(data$x) + .1 * data$z,
    binary = factor(rep(c("no", "yes"), 24L), levels = c("no", "yes")),
    multiclass = factor(rep(c("a", "b", "c"), 16L), levels = c("a", "b", "c"))
  )
  parameters <- list(
    num.trees = 9L, mtry = 2L, min.node.size = 3L,
    sample.fraction = .8, splitrule = "default"
  )
  attr(parameters, "autoxplain_threads") <- 1L
  model <- fit_forest_learner(data, "y", task, parameters, 9311L)
  list(
    data = data, model = model,
    explainer = explain_model(model, data, "y",
      task = task,
      positive = if (task == "binary") "no" else NULL
    )
  )
}

test_that("importance progress is throttled without catch-up messages", {
  withr::local_preserve_seed()
  clock <- new.env(parent = emptyenv())
  clock$now <- 0
  clock$calls <- 0L
  local_mocked_bindings(explanation_progress_time = function() {
    clock$calls <- clock$calls + 1L
    clock$now
  })
  expect_null(new_importance_progress(FALSE, "unused", 3L, 4L))
  expect_null(update_importance_progress(NULL, 1L, 1L))
  expect_identical(clock$calls, 0L)
  set.seed(9323L)
  before <- .Random.seed
  state <- new_importance_progress(TRUE, "Forest A", 7L, 5L)
  updates <- capture_importance_messages({
    for (time in c(29.999, 30, 30.1, 59.999, 60, 500)) {
      clock$now <- time
      update_importance_progress(state, 2L, 3L)
    }
  })
  expect_length(updates$messages, 3L)
  expect_true(all(grepl("Forest A; input group 2/7; repeat 3/5", updates$messages, fixed = TRUE)))
  expect_identical(clock$calls, 7L)
  expect_identical(state$last_update, 500)
  expect_identical(.Random.seed, before)
})

test_that("native grouped importance has identical evidence and RNG in info and quiet modes", {
  skip_if_package_unavailable("ranger")
  withr::local_preserve_seed()
  clock <- new.env(parent = emptyenv())
  clock$quiet <- TRUE
  clock$now <- 0
  local_mocked_bindings(explanation_progress_time = function() {
    if (clock$quiet) stop("Quiet importance must not read the progress clock")
    clock$now <- clock$now + 31
    clock$now
  })
  for (task in c("regression", "binary", "multiclass")) {
    fixture <- importance_progress_forest(task)
    calculate <- function(progress) {
      calculate_permutation_importance_impl(fixture$explainer,
        feature_groups = list(signal = c("x", "z"), category = "category"), within = "category",
        n_repeats = 4L, seed = 9337L, sample_seed = 9341L, max_rows = 15L, progress = progress
      )
    }
    clock$quiet <- TRUE
    set.seed(9343L)
    quiet <- capture_importance_messages(calculate(FALSE))
    after_quiet <- .Random.seed
    expect_length(quiet$messages, 0L)
    clock$quiet <- FALSE
    set.seed(9343L)
    # A user's logging handler can consume random numbers. Progress emitted
    # inside the shuffle loop must not let that change later permutations.
    info <- capture_importance_messages(calculate(TRUE), consume_rng = TRUE)
    expect_identical(info$value, quiet$value)
    expect_identical(.Random.seed, after_quiet)
    expect_length(info$messages, 8L)
    expect_match(info$messages[[1L]], "input group 1/2; repeat 1/4", fixed = TRUE)
    expect_match(info$messages[[8L]], "input group 2/2; repeat 4/4", fixed = TRUE)
    expect_identical(names(attr(info$value, "feature_groups")), c("signal", "category"))
  }
  expect_false("progress" %in% names(formals(calculate_permutation_importance)))
  expect_false("progress" %in% names(formals(audit_explanations)))
})

test_that("report screening and audit both receive private progress without changing evidence", {
  skip_if_package_unavailable("ranger")
  withr::local_preserve_seed()
  fixture <- importance_progress_forest("multiclass")
  result <- evaluate_models(list(forest = fixture$model), fixture$data, "y", task = "multiclass")
  clock <- new.env(parent = emptyenv())
  clock$quiet <- TRUE
  clock$now <- 0
  local_mocked_bindings(explanation_progress_time = function() {
    if (clock$quiet) stop("Quiet reports must not read the importance progress clock")
    clock$now <- clock$now + 31
    clock$now
  })
  prepare <- function(progress) {
    prepare_model_report_data(result,
      top_features = 1L,
      n_repeats = 3L, max_models = 1L, explanation_rows = 13L, progress = progress
    )
  }
  without_creation_time <- function(value) {
    if (is.list(value)) {
      if (!is.null(names(value)) && "created_at" %in% names(value)) value["created_at"] <- NULL
      for (index in seq_along(value)) value[index] <- list(without_creation_time(value[[index]]))
    }
    value
  }
  set.seed(9349L)
  before <- .Random.seed
  quiet <- capture_importance_messages(prepare(FALSE))
  expect_identical(.Random.seed, before)
  expect_length(quiet$messages, 0L)
  clock$quiet <- FALSE
  info <- capture_importance_messages(prepare(TRUE), consume_rng = TRUE)
  expect_identical(.Random.seed, before)
  expect_identical(without_creation_time(info$value), without_creation_time(quiet$value))
  ticks <- info$messages[grepl("Input importance: forest; input group", info$messages, fixed = TRUE)]
  expect_length(ticks, 12L)
  expect_match(ticks[[9L]], "input group 3/3; repeat 3/3", fixed = TRUE)
  expect_match(ticks[[12L]], "input group 1/1; repeat 3/3", fixed = TRUE)
  expect_true(any(grepl("Screening input importance", info$messages, fixed = TRUE)))
  expect_true(any(grepl("Checking input importance", info$messages, fixed = TRUE)))
  expect_true(any(grepl("Computing effect curves", info$messages, fixed = TRUE)))
})

test_that("a failed native prediction does not report the interrupted shuffle as completed", {
  skip_if_package_unavailable("ranger")
  withr::local_preserve_seed()
  clock <- new.env(parent = emptyenv())
  clock$calls <- 0L
  local_mocked_bindings(explanation_progress_time = function() {
    clock$calls <- clock$calls + 1L
    31 * clock$calls
  })
  for (task in c("regression", "binary", "multiclass")) {
    fixture <- importance_progress_forest(task)
    adapter <- fixture$explainer$predict_function
    predictions <- 0L
    fixture$explainer$predict_function <- function(newdata) {
      value <- adapter(newdata)
      predictions <<- predictions + 1L
      # The baseline and first shuffle succeed. The second shuffle performs
      # a real native prediction, then fails before it can supply a score.
      if (predictions == 3L) stop("deliberate failure after native prediction")
      value
    }
    clock$calls <- 0L
    run <- capture_importance_messages(tryCatch(
      calculate_permutation_importance_impl(fixture$explainer,
        features = "x", n_repeats = 4L, seed = 9371L, progress = TRUE
      ),
      error = identity
    ))
    expect_s3_class(run$value, "error")
    expect_match(conditionMessage(run$value), "deliberate failure after native prediction", fixed = TRUE)
    expect_identical(predictions, 3L)
    expect_identical(clock$calls, 2L)
    expect_length(run$messages, 1L)
    expect_match(run$messages[[1L]], "input group 1/1; repeat 1/4", fixed = TRUE)
  }
})

test_that("unavailable RMSLE shuffles do not produce completed progress ticks", {
  withr::local_preserve_seed()
  data <- data.frame(x = 1:8, z = 1:8, spare = 8:1, y = rep(1, 8))
  explainer <- explain_model(list(intercept = 1), data, "y", task = "regression",
    predict_function = function(model, newdata) model$intercept + newdata$x - newdata$z,
    metadata = list(primary_metric = "rmsle")
  )
  clock <- new.env(parent = emptyenv())
  clock$calls <- 0L
  local_mocked_bindings(explanation_progress_time = function() {
    clock$calls <- clock$calls + 1L
    31 * clock$calls
  })
  calculate <- function(progress) {
    calculate_permutation_importance_impl(explainer,
      features = c("x", "spare"), n_repeats = 5L, seed = 17L, progress = progress
    )
  }
  quiet <- capture_importance_messages(calculate(FALSE))
  expect_identical(clock$calls, 0L)
  info <- capture_importance_messages(calculate(TRUE), consume_rng = TRUE)
  expect_identical(info$value, quiet$value)
  expect_identical(attr(info$value, "permutation_failures")$feature, "x")
  expect_identical(clock$calls, 6L)
  expect_length(info$messages, 5L)
  expect_true(all(grepl("input group 2/2", info$messages, fixed = TRUE)))
  expect_match(info$messages[[5L]], "repeat 5/5", fixed = TRUE)
})
