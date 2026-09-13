capture_search_messages <- function(code, randomize_logger = FALSE) {
  messages <- character()
  result <- withCallingHandlers(code, message = function(condition) {
    messages <<- c(messages, conditionMessage(condition))
    if (randomize_logger) stats::runif(1L)
    invokeRestart("muffleMessage")
  })
  list(result = result, messages = messages)
}

test_that("automatic progress covers nontrivial native forest and boosting workflows", {
  resolve <- AutoXplainR:::resolve_progress_verbosity
  expect_identical(resolve("auto", "base", "tuned", 200L, "forest"), "info")
  expect_identical(resolve("auto", "base", "tuned", 200L, "boosting"), "info")
  expect_identical(resolve("auto", "base", "tuned", 2500L, c("linear", "forest", "boosting")), "info")
  expect_identical(resolve("auto", "base", "tuned", 199L, "forest"), "quiet")
  expect_identical(resolve("auto", "base", "tuned", 100000L, c("linear", "tree", "neural")), "quiet")
  expect_identical(resolve("auto", "base", "quick", 100000L, "forest"), "quiet")
  expect_identical(resolve("auto", "h2o", "tuned", 100000L, "forest"), "quiet")
  expect_identical(resolve("quiet", "base", "tuned", 100000L, "forest"), "quiet")
  expect_identical(resolve("info", "base", "quick", 32L), "info")
  small <- capture_search_messages(autoxplain(
    mtcars, "mpg", model_set = "quick", explain = FALSE, preprocessing_config = list(verbose = TRUE)
  ))
  expect_length(small$messages, 0L)
})

test_that("real adaptive fits report actual stages without changing scores, predictions or RNG", {
  skip_if_package_unavailable("ranger")
  set.seed(463)
  data <- data.frame(x = rnorm(220), z = rnorm(220))
  data$y <- sin(data$x) + data$z^2 + rnorm(220, sd = .3)
  fit <- function(verbosity) {
    autoxplain(
      data, "y", learners = "forest", max_models = 3L, nfolds = 2L, seed = 73L,
      verbosity = verbosity, explain = FALSE, preprocessing_config = list(verbose = TRUE),
      tuning_control = tuning_control(search = "adaptive", screening_rows = 120L, finalists_per_family = 1L)
    )
  }
  set.seed(510)
  quiet <- capture_search_messages(fit("quiet"))
  expect_length(quiet$messages, 0L)
  quiet_rng <- .Random.seed
  set.seed(510)
  info <- capture_search_messages(fit("info"), randomize_logger = TRUE)
  expect_identical(.Random.seed, quiet_rng)
  phases <- info$messages[startsWith(info$messages, "AutoXplainR:")]
  expect_true(any(grepl("Preparing 220 input rows", phases, fixed = TRUE)))
  expect_equal(sum(grepl("Screening [0-9]+/[0-9]+:", phases)), 3L)
  expect_true(any(grepl("Screening complete: 3 settings assessed; 1 advanced", phases, fixed = TRUE)))
  expect_equal(sum(grepl("CV setting [0-9]+/[0-9]+:", phases)), 2L)
  expect_true(any(grepl("fold 1/2; 88 training rows; 88 assessment rows", phases, fixed = TRUE)))
  expect_true(any(grepl("fold 2/2; 88 training rows; 88 assessment rows", phases, fixed = TRUE)))
  expect_true(any(grepl("Full refit: forest", phases, fixed = TRUE) & grepl("176 training rows", phases, fixed = TRUE)))
  expect_true(any(grepl("Evaluating 2 models on 44 evaluation rows", phases, fixed = TRUE)))
  expect_true(any(grepl("Finished; 2 fitted models", phases, fixed = TRUE)))
  expect_identical(info$result$tuning$control, quiet$result$tuning$control)
  expect_identical(info$result$tuning$plan, quiet$result$tuning$plan)
  expect_identical(info$result$tuning$fold_assignment, quiet$result$tuning$fold_assignment)
  expect_identical(info$result$tuning$fold_scores$score, quiet$result$tuning$fold_scores$score)
  expect_identical(info$result$tuning$fold_scores$fit_seed, quiet$result$tuning$fold_scores$fit_seed)
  expect_identical(info$result$tuning$selected_configuration, quiet$result$tuning$selected_configuration)
  expect_identical(info$result$tuning$out_of_fold_predictions, quiet$result$tuning$out_of_fold_predictions)
  expect_identical(predict(info$result, data), predict(quiet$result, data))
})

test_that("a public native default reports progress at 200 rows and quiet overrides it", {
  skip_if_package_unavailable("ranger")
  withr::local_preserve_seed()
  set.seed(284)
  data <- data.frame(x = rnorm(200), z = runif(200))
  data$y <- data$x + data$z + rnorm(200, sd = .5)
  # A deliberately small explicit grid keeps this boundary test inexpensive.
  control <- tuning_control(grids = list(forest = list(
    mtry = 1L, min.node.size = 10L, num.trees = 20L, sample.fraction = .632, splitrule = "default"
  )))
  arguments <- list(
    data = data, target_column = "y", learners = "forest", max_models = 1L,
    nfolds = 2L, explanation_rows = 12L, tuning_control = control
  )
  set.seed(287L)
  automatic <- capture_search_messages(do.call(autoxplain, arguments), randomize_logger = TRUE)
  automatic_rng <- .Random.seed
  set.seed(287L)
  quiet <- capture_search_messages(do.call(autoxplain, c(arguments, list(verbosity = "quiet"))))
  expect_identical(.Random.seed, automatic_rng)
  expect_true(any(grepl("Preparing 200 input rows", automatic$messages, fixed = TRUE)))
  expect_true(any(grepl("Screening input importance: main_model", automatic$messages, fixed = TRUE)))
  expect_true(any(grepl("Checking input importance: main_model", automatic$messages, fixed = TRUE)))
  expect_true(any(grepl("Computing effect curves: main_model", automatic$messages, fixed = TRUE)))
  expect_length(quiet$messages, 0L)
  expect_identical(automatic$result$tuning$fold_assignment, quiet$result$tuning$fold_assignment)
  expect_identical(automatic$result$tuning$fold_scores$score, quiet$result$tuning$fold_scores$score)
  expect_identical(automatic$result$tuning$fold_scores$fit_seed, quiet$result$tuning$fold_scores$fit_seed)
  expect_identical(automatic$result$tuning$selected_configuration, quiet$result$tuning$selected_configuration)
  expect_identical(automatic$result$tuning$out_of_fold_predictions, quiet$result$tuning$out_of_fold_predictions)
  expect_identical(predict(automatic$result, data), predict(quiet$result, data))
  automatic_explainers <- as_explainers(automatic$result)
  quiet_explainers <- as_explainers(quiet$result)
  fingerprints <- function(explainers) {
    vapply(explainers, AutoXplainR:::current_explainer_fingerprint, character(1))
  }
  automatic_ids <- fingerprints(automatic_explainers)
  quiet_ids <- fingerprints(quiet_explainers)
  primary <- automatic$result$evaluation$primary_model_id
  expect_identical(primary, quiet$result$evaluation$primary_model_id)
  expect_true(automatic$result$models[[primary]]$fit$call$verbose)
  expect_false(quiet$result$models[[primary]]$fit$call$verbose)
  expect_false(identical(automatic_ids[[primary]], quiet_ids[[primary]]))
  native_identity <- automatic$result$models[[primary]]
  native_identity$fit$call$verbose <- quiet$result$models[[primary]]$fit$call$verbose
  expect_identical(
    AutoXplainR:::model_identity_payload(native_identity),
    AutoXplainR:::model_identity_payload(quiet$result$models[[primary]])
  )
  compare_bound_explanation <- function(left, right, model_id) {
    # The truthful native call records verbose TRUE/FALSE, so model-bound
    # identities differ. Check each binding before comparing every value and
    # every remaining attribute; production fingerprint/cache guards stay strict.
    expect_identical(attr(left, "explainer_fingerprint"), automatic_ids[[model_id]])
    expect_identical(attr(right, "explainer_fingerprint"), quiet_ids[[model_id]])
    attr(left, "explainer_fingerprint") <- NULL
    attr(right, "explainer_fingerprint") <- NULL
    expect_identical(left, right)
  }
  compare_bound_explanation(
    automatic$result$explanations$screening,
    quiet$result$explanations$screening, primary
  )
  left_importance <- automatic$result$explanations$audit$importance_objects
  right_importance <- quiet$result$explanations$audit$importance_objects
  expect_identical(names(left_importance), names(right_importance))
  for (id in names(left_importance)) {
    compare_bound_explanation(left_importance[[id]], right_importance[[id]], id)
  }
  left_effects <- automatic$result$explanations$effects
  right_effects <- quiet$result$explanations$effects
  expect_identical(names(left_effects), names(right_effects))
  for (feature in names(left_effects)) {
    compare_bound_explanation(left_effects[[feature]], right_effects[[feature]], primary)
  }
  expect_true(
    AutoXplainR:::validate_attached_audit(
      automatic$result$explanations$audit, automatic_explainers, automatic_ids
    )
  )
  expect_true(
    AutoXplainR:::validate_attached_audit(quiet$result$explanations$audit, quiet_explainers, quiet_ids)
  )
  expect_error(
    AutoXplainR:::validate_attached_audit(automatic$result$explanations$audit, quiet_explainers),
    "not made from the same"
  )
  expect_error(
    AutoXplainR:::validate_report_effect_collection(left_effects, primary, NULL, quiet$result, quiet_ids),
    "stale or foreign"
  )
})

test_that("explicit progress reaches explanation and report stages with real artifacts", {
  set.seed(792)
  data <- data.frame(x = rnorm(100))
  data$y <- 2 * data$x + rnorm(100)
  path <- tempfile(fileext = ".html")
  on.exit(unlink(path), add = TRUE)
  info <- capture_search_messages(autoxplain(
    data, "y", model_set = "quick", verbosity = "info", report = path, explanation_rows = 12L
  ))
  messages <- paste(info$messages, collapse = "")
  expect_match(messages, "Computing explanations using up to 12 evaluation rows", fixed = TRUE)
  expect_match(messages, "Screening input importance: main_model; 1 input; up to 12 reference rows; 5 shuffles",
    fixed = TRUE
  )
  expect_match(messages, "Checking input importance: main_model; 1 input; up to 12 reference rows; 20 shuffles",
    fixed = TRUE
  )
  expect_match(messages, "Computing effect curves: main_model; 1 input; up to 12 reference rows", fixed = TRUE)
  expect_match(messages, paste0("Writing HTML report: ", path), fixed = TRUE)
  expect_match(messages, "Finished; 2 fitted models", fixed = TRUE)
  expect_true(file.exists(path))
  expect_match(paste(readLines(path, warn = FALSE), collapse = ""), 'id="overview"', fixed = TRUE)
  expect_false(is.null(info$result$explanations))
})

test_that("explanation progress leaves computed evidence and random draws unchanged", {
  result <- autoxplain(mtcars, "mpg", model_set = "quick", explain = FALSE)
  prepare <- function(progress) {
    AutoXplainR:::prepare_model_report_data(
      result, top_features = 2L, n_repeats = 3L, explanation_rows = 5L, progress = progress
    )
  }
  withr::local_seed(57L)
  before <- .Random.seed
  quiet <- capture_search_messages(prepare(FALSE))
  info <- capture_search_messages(prepare(TRUE), randomize_logger = TRUE)
  expect_identical(.Random.seed, before)
  expect_length(quiet$messages, 0L)
  expect_true(any(grepl("simple_baseline", info$messages, fixed = TRUE)))
  quiet$result$audit$provenance$created_at <- info$result$audit$provenance$created_at <- NULL
  expect_identical(info$result, quiet$result)
})

test_that("stochastic logging preserves an absent caller seed through the public workflow", {
  withr::local_preserve_seed()
  data <- data.frame(x = seq_len(80L), y = sin(seq_len(80L)))
  fit <- function(verbosity) {
    autoxplain(
      data, "y", learners = "tree", max_models = 2L, nfolds = 2L,
      seed = 42L, verbosity = verbosity, explain = FALSE
    )
  }
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  quiet <- capture_search_messages(fit("quiet"))
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  info <- capture_search_messages(fit("info"), randomize_logger = TRUE)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  expect_true(any(startsWith(info$messages, "Preprocessing complete:")))
  expect_identical(info$result$tuning$fold_assignment, quiet$result$tuning$fold_assignment)
  expect_identical(info$result$tuning$fold_scores$score, quiet$result$tuning$fold_scores$score)
  expect_identical(info$result$tuning$out_of_fold_predictions, quiet$result$tuning$out_of_fold_predictions)
  expect_identical(predict(info$result, data), predict(quiet$result, data))
})

test_that("logging handlers that abort restore existing and absent caller seeds", {
  withr::local_preserve_seed()
  data <- data.frame(x = seq_len(80L), y = sin(seq_len(80L)))
  for (stage in c("Preparing", "Preprocessing complete:", "Fitting the baseline on")) {
    for (has_seed in c(TRUE, FALSE)) {
      set.seed(177L)
      before <- .Random.seed
      if (!has_seed) rm(".Random.seed", envir = .GlobalEnv)
      expect_error(withCallingHandlers(
        autoxplain(data, "y", learners = "tree", max_models = 2L, nfolds = 2L,
          seed = 42L, verbosity = "info", explain = FALSE
        ),
        message = function(condition) {
          stats::runif(1L)
          if (grepl(stage, conditionMessage(condition), fixed = TRUE)) stop("logger aborted")
          invokeRestart("muffleMessage")
        }
      ), "logger aborted")
      if (has_seed) {
        expect_identical(.Random.seed, before)
      } else {
        expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      }
    }
  }
})
