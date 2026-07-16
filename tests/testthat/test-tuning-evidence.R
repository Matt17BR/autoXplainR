test_that("tuning retains paired regression out-of-fold evidence", {
  set.seed(2026)
  training <- data.frame(
    x = runif(72, -2, 2),
    group = factor(rep(c("a", "b", "c"), 24)),
    noise = rnorm(72)
  )
  training$y <- with(
    training,
    x^2 + as.numeric(group) + 0.2 * noise + rnorm(72, sd = 0.2)
  )
  rownames(training) <- paste0("training-case-", seq_len(nrow(training)))
  evaluation <- make_disjoint_evaluation(training, "y", 1:18)

  result <- autoxplain(
    training,
    "y",
    test_data = evaluation,
    model_set = "tuned",
    portfolio = "core",
    max_models = 3,
    nfolds = 3,
    tuning_rule = "best",
    seed = 19
  )
  evidence <- result$tuning$out_of_fold_predictions
  valid <- result$tuning$candidates[result$tuning$candidates$status == "ok", ]

  expect_equal(result$tuning$schema_version, 4L)
  expect_equal(nrow(evidence), nrow(training) * nrow(valid))
  expect_named(evidence, c(
    "configuration_id", "family", "backend", "fold", "training_row",
    "source_row", "truth", "estimate", "predicted_class",
    "truth_probability", "case_loss", "probabilities"
  ))
  expect_equal(evidence$case_loss, (evidence$truth - evidence$estimate)^2)
  expect_true(all(is.na(evidence$predicted_class)))
  expect_true(is.matrix(evidence$probabilities))
  expect_equal(ncol(evidence$probabilities), 0L)

  by_configuration <- split(evidence, evidence$configuration_id)
  expect_true(all(vapply(by_configuration, function(rows) {
    identical(sort(rows$training_row), seq_len(nrow(training)))
  }, logical(1))))
  expect_true(all(vapply(by_configuration, function(rows) {
    identical(
      rows$source_row[order(rows$training_row)],
      rownames(training)
    )
  }, logical(1))))
  fold_pairing <- split(evidence$fold, evidence$training_row)
  expect_true(all(lengths(lapply(fold_pairing, unique)) == 1L))

  evidence_score <- vapply(by_configuration, function(rows) {
    sqrt(mean(rows$case_loss))
  }, numeric(1))
  candidate_score <- stats::setNames(valid$cv_score, valid$configuration_id)
  expect_equal(
    unname(evidence_score[names(candidate_score)]),
    unname(candidate_score),
    tolerance = 1e-12
  )
  expect_match(result$tuning$prediction_schema$comparison_note, "paired")
  expect_match(result$tuning$prediction_schema$comparison_note, "not holdout")
})

test_that("classification tuning evidence preserves complete probability vectors", {
  set.seed(818)
  training <- data.frame(x = rnorm(84), z = rnorm(84))
  probability <- stats::plogis(0.8 * training$x - 0.5 * training$z)
  training$event <- factor(
    ifelse(runif(nrow(training)) < probability, "yes", "no"),
    levels = c("no", "yes")
  )
  evaluation <- make_disjoint_evaluation(training, "event", 1:20)
  result <- autoxplain(
    training,
    "event",
    test_data = evaluation,
    model_set = "tuned",
    portfolio = "core",
    max_models = 3,
    nfolds = 3,
    seed = 27
  )
  evidence <- result$tuning$out_of_fold_predictions

  expect_true(all(is.na(evidence$estimate)))
  expect_setequal(unique(evidence$truth), levels(training$event))
  expect_true(all(evidence$predicted_class %in% levels(training$event)))
  expect_true(is.matrix(evidence$probabilities))
  expect_identical(colnames(evidence$probabilities), levels(training$event))
  expect_true(all(is.finite(evidence$probabilities)))
  expect_equal(rowSums(evidence$probabilities), rep(1, nrow(evidence)), tolerance = 1e-10)
  recorded_truth_probability <- evidence$probabilities[cbind(
    seq_len(nrow(evidence)),
    match(evidence$truth, levels(training$event))
  )]
  expect_equal(evidence$truth_probability, recorded_truth_probability)
  expect_equal(evidence$case_loss, -log(pmax(recorded_truth_probability, 1e-15)))
  expect_identical(result$tuning$prediction_schema$class_levels, c("no", "yes"))
  expect_identical(result$tuning$prediction_schema$positive_class, "yes")
})

test_that("drop_rows preserves regression evidence identity and reports omissions", {
  set.seed(909)
  training <- data.frame(x = rnorm(60), z = rnorm(60))
  training$y <- 2 * training$x - training$z + rnorm(60, sd = 0.2)
  omitted <- seq(4L, nrow(training), by = 4L)
  training$x[omitted] <- NA_real_
  rownames(training) <- paste0("regression-source-", seq_len(nrow(training)))
  evaluation <- make_disjoint_evaluation(
    training, "y", which(stats::complete.cases(training))[1:15]
  )
  result <- autoxplain(
    training,
    "y",
    test_data = evaluation,
    model_set = "tuned",
    portfolio = "core",
    max_models = 3,
    nfolds = 3,
    preprocessing_config = list(missing_value_strategy = "drop_rows"),
    seed = 90
  )
  evidence <- result$tuning$out_of_fold_predictions
  valid <- result$tuning$candidates[result$tuning$candidates$status == "ok", ]
  kept <- setdiff(seq_len(nrow(training)), omitted)

  expect_equal(result$tuning$rows_requested, 60L)
  expect_equal(result$tuning$rows_evaluated, 45L)
  expect_equal(result$tuning$rows_omitted, 15L)
  expect_identical(result$tuning$omitted_rows$training_row, omitted)
  expect_identical(
    result$tuning$omitted_rows$source_row,
    rownames(training)[omitted]
  )
  expect_equal(nrow(evidence), length(kept) * nrow(valid))
  expect_true(all(vapply(split(evidence, evidence$configuration_id), function(rows) {
    identical(sort(rows$training_row), kept)
  }, logical(1))))
  expect_true(all(valid$evaluated_rows == length(kept)))
  one_configuration <- result$tuning$fold_scores$configuration_id[[1L]]
  fold_scores <- result$tuning$fold_scores[
    result$tuning$fold_scores$configuration_id == one_configuration,
  ]
  expect_equal(sum(fold_scores$validation_rows_requested), nrow(training))
  expect_equal(sum(fold_scores$validation_rows), length(kept))
  expect_equal(sum(fold_scores$validation_rows_omitted), length(omitted))
  expect_identical(
    result$tuning$prediction_schema$coverage,
    c(rows_requested = 60L, rows_evaluated = 45L, rows_omitted = 15L)
  )
  expect_match(result$tuning$prediction_schema$omission_note, "only predicted rows")
  expect_match(result$tuning$scope_note, "15 outer-training rows were omitted")
})

test_that("drop_rows preserves classification evidence identity and probabilities", {
  set.seed(707)
  training <- data.frame(x = rnorm(72), z = rnorm(72))
  training$event <- factor(
    rep(c("no", "yes"), length.out = nrow(training)),
    levels = c("no", "yes")
  )
  omitted <- seq(4L, nrow(training), by = 4L)
  training$z[omitted] <- NA_real_
  rownames(training) <- paste0("classification-source-", seq_len(nrow(training)))
  evaluation <- make_disjoint_evaluation(
    training, "event", which(stats::complete.cases(training))[1:18]
  )
  result <- autoxplain(
    training,
    "event",
    test_data = evaluation,
    model_set = "tuned",
    portfolio = "core",
    max_models = 3,
    nfolds = 3,
    preprocessing_config = list(missing_value_strategy = "drop_rows"),
    seed = 70
  )
  evidence <- result$tuning$out_of_fold_predictions
  kept <- setdiff(seq_len(nrow(training)), omitted)
  valid <- result$tuning$candidates[result$tuning$candidates$status == "ok", ]

  expect_equal(result$tuning$rows_evaluated, length(kept))
  expect_identical(result$tuning$omitted_rows$training_row, omitted)
  expect_equal(nrow(evidence), length(kept) * nrow(valid))
  expect_true(all(vapply(split(evidence, evidence$configuration_id), function(rows) {
    rows <- rows[order(rows$training_row), , drop = FALSE]
    identical(rows$training_row, kept) &&
      identical(rows$source_row, rownames(training)[kept]) &&
      identical(rows$truth, as.character(training$event[kept]))
  }, logical(1))))
  expect_identical(colnames(evidence$probabilities), levels(training$event))
  expect_equal(rowSums(evidence$probabilities), rep(1, nrow(evidence)), tolerance = 1e-10)
  expect_true(all(valid$evaluated_rows == length(kept)))
  expect_output(print(result$tuning), "54/72 outer-training rows predicted")
})

mock_tuning_for_refit <- function() {
  plan <- AutoXplainR:::local_tuning_plan(
    max_models = 3,
    n = 40,
    p = 2,
    task = "regression",
    n_classes = 1,
    learners = c("linear", "tree", "neural"),
    seed = 11
  )
  candidates <- data.frame(
    configuration_id = plan$configuration_id,
    family = plan$family,
    backend = plan$backend,
    model = plan$model,
    hyperparameters = plan$hyperparameters,
    cv_score = c(0.1, 0.2, 0.3),
    cv_sd = c(0.01, 0.02, 0.03),
    cv_se = c(0.01, 0.02, 0.03),
    folds_completed = 3L,
    evaluated_rows = 40L,
    simplicity_rank = plan$simplicity_rank,
    complexity_proxy = plan$complexity_proxy,
    selected = c(TRUE, FALSE, FALSE),
    status = "ok",
    stringsAsFactors = FALSE
  )
  plan$selected <- candidates$selected
  structure(
    list(
      candidates = candidates,
      plan = plan,
      selected_configuration = candidates$configuration_id[[1L]],
      final_configuration = NA_character_,
      refit = NULL
    ),
    class = "autoxplain_tuning"
  )
}

test_that("a failed alternative refit is isolated and fully diagnosed", {
  tuning <- mock_tuning_for_refit()
  failed_family <- tuning$candidates$family[[2L]]
  fitter <- function(configuration, data, target, task) {
    if (configuration$family[[1L]] == failed_family) {
      stop("intentional alternative refit failure", call. = FALSE)
    }
    list(configuration_id = configuration$configuration_id[[1L]])
  }
  refitted <- AutoXplainR:::refit_tuned_candidates(
    tuning,
    data = data.frame(x = 1:8, z = 8:1, y = 1:8),
    target = "y",
    task = "regression",
    fitter = fitter
  )

  expect_true("main_model" %in% names(refitted$fits))
  expect_false(AutoXplainR:::learner_definition(failed_family)$model_id %in% names(refitted$fits))
  expect_identical(refitted$tuning$refit$status, "partial")
  expect_false(refitted$tuning$refit$fallback_used)
  expect_true(failed_family %in% refitted$tuning$refit$families_not_retained)
  failed <- refitted$tuning$refit$attempts$family == failed_family
  expect_true(any(failed))
  expect_true(all(refitted$tuning$refit$attempts$status[failed] == "failed"))
  expect_match(refitted$tuning$refit$attempts$error[failed], "intentional")
  expect_identical(
    refitted$tuning$candidates$refit_status[
      refitted$tuning$candidates$family == failed_family
    ],
    "failed"
  )
})

test_that("selected refit failure uses a deterministic recorded fallback", {
  tuning <- mock_tuning_for_refit()
  selected_id <- tuning$selected_configuration
  expected_fallback <- tuning$candidates$configuration_id[[2L]]
  fitter <- function(configuration, data, target, task) {
    if (configuration$configuration_id[[1L]] == selected_id) {
      stop("intentional selected refit failure", call. = FALSE)
    }
    list(configuration_id = configuration$configuration_id[[1L]])
  }
  refitted <- AutoXplainR:::refit_tuned_candidates(
    tuning,
    data = data.frame(x = 1:8, z = 8:1, y = 1:8),
    target = "y",
    task = "regression",
    fitter = fitter
  )

  expect_true(refitted$tuning$refit$fallback_used)
  expect_identical(refitted$tuning$final_configuration, expected_fallback)
  expect_identical(refitted$tuning$refit$final_configuration, expected_fallback)
  expect_identical(
    refitted$tuning$candidates$retained_model_id[
      refitted$tuning$candidates$configuration_id == expected_fallback
    ],
    "main_model"
  )
  expect_identical(
    refitted$tuning$candidates$refit_status[
      refitted$tuning$candidates$configuration_id == selected_id
    ],
    "failed"
  )
  expect_match(refitted$tuning$refit$attempts$error[[1L]], "intentional selected")
  notes <- AutoXplainR:::guided_tuning_refit_notes(refitted$tuning)
  expect_true("tuning_refit_fallback" %in% notes$code)
  expect_match(notes$message[notes$code == "tuning_refit_fallback"], expected_fallback)
})

test_that("families with no complete resampling configuration remain visible", {
  tuning <- mock_tuning_for_refit()
  failed_family <- tuning$candidates$family[[3L]]
  tuning$learners <- tuning$candidates$family
  tuning$candidates$status[[3L]] <- "failed"
  tuning$candidates$cv_score[[3L]] <- NA_real_
  tuning$families_resampling_failed <- failed_family
  refitted <- AutoXplainR:::refit_tuned_candidates(
    tuning,
    data = data.frame(x = 1:8, z = 8:1, y = 1:8),
    target = "y",
    task = "regression",
    fitter = function(configuration, data, target, task) {
      list(configuration_id = configuration$configuration_id[[1L]])
    }
  )

  expect_identical(refitted$tuning$refit$status, "partial")
  expect_identical(refitted$tuning$refit$families_resampling_failed, failed_family)
  expect_identical(refitted$tuning$refit$families_refit_failed, character())
  expect_true(failed_family %in% refitted$tuning$refit$families_not_retained)
  expect_false(any(refitted$tuning$refit$attempts$family == failed_family))
  notes <- AutoXplainR:::guided_tuning_refit_notes(refitted$tuning)
  expect_true("tuning_family_resampling_failed" %in% notes$code)
  expect_match(
    notes$message[notes$code == "tuning_family_resampling_failed"],
    failed_family
  )
})

test_that("all-configuration resampling failure is surfaced end to end", {
  skip_if_not_installed("mgcv")
  set.seed(55)
  training <- data.frame(group = factor(rep(c("a", "b", "c"), 24)))
  training$y <- as.numeric(training$group) + rnorm(nrow(training), sd = 0.2)
  result <- autoxplain(
    training,
    "y",
    test_data = make_disjoint_evaluation(training, "y", 1:18),
    model_set = "tuned",
    learners = c("linear", "additive"),
    max_models = 4,
    nfolds = 3,
    seed = 71
  )

  expect_identical(result$tuning$families_resampling_failed, "additive")
  expect_identical(result$tuning$refit$families_resampling_failed, "additive")
  expect_true(all(
    result$tuning$out_of_fold_predictions$family == "linear"
  ))
  expect_true("tuning_family_resampling_failed" %in% result$evaluation$notes$code)
  expect_output(print(result$tuning), "no valid CV: additive")
})

test_that("refit fails clearly only when no valid configuration can be fitted", {
  tuning <- mock_tuning_for_refit()
  expect_error(
    AutoXplainR:::refit_tuned_candidates(
      tuning,
      data = data.frame(x = 1:8, z = 8:1, y = 1:8),
      target = "y",
      task = "regression",
      fitter = function(...) stop("every final fit failed", call. = FALSE)
    ),
    "No resampling-valid configuration.*every final fit failed"
  )
})
