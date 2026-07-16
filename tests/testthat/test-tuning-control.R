test_that("tuning_control validates adapter contracts eagerly", {
  control <- tuning_control()
  expect_s3_class(control, "autoxplain_tuning_control")
  expect_identical(control$metric, "auto")
  expect_true(control$retain_oof)
  expect_identical(control$failure_policy, "continue")

  expect_error(tuning_control(grids = list(data.frame(x = 1))), "uniquely named")
  expect_error(
    tuning_control(grids = list(tree = list(list(maxdepth = 2L, cp = 0.01)))),
    "Missing: minsplit"
  )
  expect_error(
    tuning_control(grids = list(tree = list(list(
      maxdepth = 2L, cp = -0.01, minsplit = 5L
    )))),
    "Parameter `cp`"
  )
  repeated <- list(maxdepth = 2L, cp = 0.01, minsplit = 5L)
  expect_error(
    tuning_control(grids = list(tree = list(repeated, repeated))),
    "duplicate configurations"
  )
  expect_error(tuning_control(family_budgets = c(1, 2)), "uniquely named")
  expect_error(tuning_control(family_budgets = c(linear = 0)), "positive integer")
  expect_error(tuning_control(fold_ids = c("a", "a", "a", "a")), "at least two")
  expect_error(tuning_control(retain_oof = NA), "TRUE or FALSE")
})

test_that("custom grids and exact family budgets define the complete plan", {
  set.seed(401)
  training <- data.frame(x = rnorm(60), z = rnorm(60))
  training$y <- 2 * training$x - training$z + rnorm(60, sd = 0.2)
  evaluation <- make_disjoint_evaluation(training, "y", 1:15)
  tree_grid <- data.frame(
    maxdepth = c(2L, 5L),
    cp = c(0.02, 0.002),
    minsplit = c(12L, 6L)
  )
  control <- tuning_control(
    grids = list(tree = tree_grid),
    family_budgets = c(linear = 1L, tree = 2L),
    metric = "mae"
  )
  result <- autoxplain(
    training,
    "y",
    test_data = evaluation,
    model_set = "tuned",
    learners = c("linear", "tree"),
    tuning_control = control,
    nfolds = 3,
    seed = 401
  )

  counts <- table(result$tuning$plan$family)
  expect_identical(as.integer(counts), c(1L, 2L))
  expect_identical(names(counts), c("linear", "tree"))
  expect_identical(result$tuning$control$family_budgets, c(linear = 1L, tree = 2L))
  expect_identical(result$tuning$configurations_requested, 3L)
  expect_identical(result$tuning$metric, "mae")
  tree_parameters <- result$tuning$plan$parameters[result$tuning$plan$family == "tree"]
  expect_identical(vapply(tree_parameters, `[[`, integer(1), "maxdepth"), c(2L, 5L))
  expect_identical(vapply(tree_parameters, `[[`, integer(1), "minsplit"), c(12L, 6L))
  expect_match(result$provenance$candidate_selection, "`mae`")
})

test_that("family budget conflicts and exhaustion fail before fitting", {
  data <- data.frame(x = 1:30, y = 1:30 + stats::rnorm(30))
  evaluation <- make_disjoint_evaluation(data, "y", 1:8)
  budgets <- tuning_control(family_budgets = c(linear = 1L, tree = 1L))
  expect_error(
    autoxplain(
      data, "y", test_data = evaluation, model_set = "tuned",
      learners = c("linear", "tree"), max_models = 2, tuning_control = budgets
    ),
    "either explicit `max_models`"
  )
  expect_error(
    autoxplain(
      data, "y", test_data = evaluation, model_set = "tuned",
      learners = c("linear", "tree", "neural"), tuning_control = budgets
    ),
    "exactly match"
  )
  exhausted <- tuning_control(
    grids = list(tree = list(list(maxdepth = 2L, cp = 0.01, minsplit = 5L))),
    family_budgets = c(linear = 1L, tree = 2L)
  )
  expect_error(
    autoxplain(
      data, "y", test_data = evaluation, model_set = "tuned",
      learners = c("linear", "tree"), tuning_control = exhausted
    ),
    "tree requested 2 but its grid contains 1"
  )
})

test_that("supplied fold IDs are ordinary labeled V-fold assignments", {
  set.seed(812)
  training <- data.frame(x = rnorm(48), z = rnorm(48))
  training$event <- factor(
    rep(rep(c("no", "yes"), each = 8L), 3L),
    levels = c("no", "yes")
  )
  rownames(training) <- paste0("training-", seq_len(nrow(training)))
  evaluation <- make_disjoint_evaluation(training, "event", 1:12)
  ids <- rep(c("site-west", "site-east", "site-north"), each = 16L)
  control <- tuning_control(fold_ids = ids)
  result <- autoxplain(
    training,
    "event",
    test_data = evaluation,
    model_set = "tuned",
    learners = "linear",
    max_models = 1,
    tuning_control = control,
    seed = 812
  )

  assignment <- result$tuning$fold_assignment
  expect_identical(assignment$training_row, seq_len(nrow(training)))
  expect_identical(assignment$source_row, rownames(training))
  expect_identical(assignment$fold_label, ids)
  expect_identical(result$tuning$fold_source, "supplied_vfold")
  expect_identical(result$tuning$folds_used, 3L)
  expect_match(result$tuning$method, "User-supplied V-fold")
  expect_match(result$tuning$scope_note, "not rolling-origin")
  expect_true(all(vapply(split(
    result$tuning$out_of_fold_predictions$fold,
    result$tuning$out_of_fold_predictions$training_row
  ), function(x) length(unique(x)) == 1L, logical(1))))
})

test_that("supplied fold IDs require explicit alignment and class coverage", {
  training <- data.frame(
    x = seq_len(24),
    event = factor(c(rep("no", 12), rep("yes", 12)), levels = c("no", "yes"))
  )
  ids <- rep(c("first", "second"), each = 12)
  expect_error(
    autoxplain(
      training, "event", model_set = "tuned", learners = "linear",
      max_models = 1, tuning_control = tuning_control(fold_ids = ids)
    ),
    "require explicit `test_data`"
  )
  expect_error(
    autoxplain(
      training, "event",
      test_data = make_disjoint_evaluation(training, "event", 1:6),
      model_set = "tuned",
      learners = "linear", max_models = 1,
      tuning_control = tuning_control(fold_ids = ids[-1L])
    ),
    "exactly one value for each training row"
  )
  expect_error(
    autoxplain(
      training, "event",
      test_data = make_disjoint_evaluation(training, "event", 1:6),
      model_set = "tuned",
      learners = "linear", max_models = 1,
      tuning_control = tuning_control(fold_ids = ids)
    ),
    "fold `first`.*Missing from validation: yes"
  )
})

test_that("MAE and Brier tuning retain reconstructable case losses", {
  set.seed(918)
  regression <- data.frame(x = rnorm(54), z = rnorm(54))
  regression$y <- regression$x - 0.5 * regression$z + rnorm(54, sd = 0.3)
  reg <- autoxplain(
    regression,
    "y",
    test_data = make_disjoint_evaluation(regression, "y", 1:12),
    model_set = "tuned",
    learners = "linear",
    max_models = 1,
    nfolds = 3,
    tuning_control = tuning_control(metric = "mae"),
    seed = 918
  )
  reg_evidence <- reg$tuning$out_of_fold_predictions
  expect_equal(reg_evidence$case_loss, abs(reg_evidence$truth - reg_evidence$estimate))
  expect_equal(reg$tuning$candidates$cv_score, mean(reg_evidence$case_loss))
  expect_match(reg$tuning$prediction_schema$case_loss, "absolute error")
  expect_identical(reg$evaluation$primary_metric, "mae")
  expect_equal(reg$leaderboard$rank, rank(reg$leaderboard$mae, ties.method = "min"))
  expect_identical(
    reg$evaluation$winner,
    reg$leaderboard$model_id[[which.min(reg$leaderboard$mae)]]
  )

  classification <- data.frame(x = rnorm(60), z = rnorm(60))
  classification$event <- factor(
    ifelse(classification$x + classification$z > 0, "yes", "no"),
    levels = c("no", "yes")
  )
  cls <- autoxplain(
    classification,
    "event",
    test_data = make_disjoint_evaluation(classification, "event", 1:12),
    model_set = "tuned",
    learners = "linear",
    max_models = 1,
    nfolds = 3,
    tuning_control = tuning_control(metric = "brier"),
    seed = 919
  )
  cls_evidence <- cls$tuning$out_of_fold_predictions
  positive <- cls_evidence$probabilities[, "yes"]
  expected <- (positive - as.numeric(cls_evidence$truth == "yes"))^2
  expect_identical(cls$tuning$metric, "brier_score")
  expect_equal(cls_evidence$case_loss, expected)
  expect_equal(cls$tuning$candidates$cv_score, mean(expected))
  expect_identical(cls$evaluation$primary_metric, "brier_score")
  expect_equal(
    cls$leaderboard$rank,
    rank(cls$leaderboard$brier_score, ties.method = "min")
  )
  expect_identical(
    cls$evaluation$winner,
    cls$leaderboard$model_id[[which.min(cls$leaderboard$brier_score)]]
  )
})

test_that("the tuned primary metric propagates through every model comparison", {
  set.seed(920)
  data <- data.frame(x = runif(90, -2, 2), z = rnorm(90))
  data$y <- data$x^2 + 0.25 * data$z + rnorm(90, sd = 0.2)
  result <- autoxplain(
    data,
    "y",
    model_set = "tuned",
    portfolio = "core",
    max_models = 3L,
    nfolds = 3L,
    tuning_control = tuning_control(metric = "mae"),
    seed = 920L
  )

  tradeoffs <- model_tradeoffs(result)
  ambiguity <- prediction_ambiguity(result)
  behavior <- compare_model_behavior(result)
  effects <- compare_model_effects(result, "x", n_points = 5L)

  expect_identical(attr(tradeoffs, "performance_metric"), "mae")
  expect_identical(ambiguity$performance_metric, "mae")
  expect_identical(behavior$performance_metric, "mae")
  expect_identical(effects$performance_metric, "mae")
})

test_that("one-SE uncertainty follows the row-weighted fold estimand", {
  mae <- AutoXplainR:::tuning_fold_uncertainty(
    scores = c(1, 3),
    validation_rows = c(90, 10),
    metric = "mae"
  )
  expect_equal(mae$score, 1.2)
  expect_equal(mae$sd, sqrt(2))
  expect_equal(mae$se, sqrt(2) / sqrt(1 / (0.9^2 + 0.1^2)))

  rmse <- AutoXplainR:::tuning_fold_uncertainty(
    scores = c(1, 3),
    validation_rows = c(90, 10),
    metric = "rmse"
  )
  expect_equal(rmse$score, sqrt(1.8))
  expect_equal(rmse$sd, sqrt(32) / (2 * sqrt(1.8)))
  expect_equal(
    rmse$se,
    sqrt(32) / sqrt(1 / (0.9^2 + 0.1^2)) / (2 * sqrt(1.8))
  )
})

test_that("retain_oof can disable row-level evidence without losing summaries", {
  set.seed(44)
  data <- data.frame(x = rnorm(48), y = rnorm(48))
  result <- autoxplain(
    data,
    "y",
    test_data = make_disjoint_evaluation(data, "y", 1:10),
    model_set = "tuned",
    learners = "linear",
    max_models = 1,
    nfolds = 3,
    tuning_control = tuning_control(retain_oof = FALSE),
    seed = 44
  )
  expect_null(result$tuning$out_of_fold_predictions)
  expect_false(result$tuning$prediction_schema$retained)
  expect_identical(result$tuning$prediction_schema$metric, "rmse")
  expect_match(result$tuning$prediction_schema$note, "were not retained")
  expect_identical(result$tuning$schema_version, 4L)
  expect_false(result$tuning$control$retain_oof)
  expect_true(all(is.finite(result$tuning$candidates$cv_score)))
  expect_equal(nrow(result$tuning$fold_scores), 3L)
  expect_output(print(result$tuning), "row-level OOF predictions not retained")
})

test_that("configuration seeds follow canonical parameters rather than grid order", {
  shallow <- list(maxdepth = 2L, cp = 0.02, minsplit = 12L)
  flexible <- list(maxdepth = 6L, cp = 0.001, minsplit = 5L)
  first <- AutoXplainR:::local_tuning_plan(
    max_models = 2,
    n = 80,
    p = 3,
    task = "regression",
    n_classes = 1,
    learners = "tree",
    seed = 912,
    custom_grids = list(tree = list(shallow, flexible)),
    family_budgets = c(tree = 2L)
  )
  reordered <- AutoXplainR:::local_tuning_plan(
    max_models = 2,
    n = 80,
    p = 3,
    task = "regression",
    n_classes = 1,
    learners = "tree",
    seed = 912,
    custom_grids = list(tree = list(flexible, shallow)),
    family_budgets = c(tree = 2L)
  )
  first_key <- vapply(
    first$parameters,
    AutoXplainR:::canonical_tuning_parameter_key,
    character(1)
  )
  reordered_key <- vapply(
    reordered$parameters,
    AutoXplainR:::canonical_tuning_parameter_key,
    character(1)
  )
  expect_identical(
    first$seed,
    reordered$seed[match(first_key, reordered_key)]
  )
  expect_false(identical(first$seed, reordered$seed))
})

test_that("classification kernel grids have one canonical epsilon", {
  regression <- AutoXplainR:::kernel_learner_grid(100, 5, "regression", 1)
  binary <- AutoXplainR:::kernel_learner_grid(100, 5, "binary", 2)
  multiclass <- AutoXplainR:::kernel_learner_grid(100, 5, "multiclass", 3)

  expect_gt(length(unique(vapply(regression, `[[`, numeric(1), "epsilon"))), 1L)
  expect_identical(unique(vapply(binary, `[[`, numeric(1), "epsilon")), 0.1)
  expect_identical(unique(vapply(multiclass, `[[`, numeric(1), "epsilon")), 0.1)
  effective <- vapply(binary, function(parameters) {
    AutoXplainR:::canonical_tuning_parameter_key(
      parameters[c("cost", "gamma_multiplier")]
    )
  }, character(1))
  expect_identical(anyDuplicated(effective), 0L)
  expect_error(
    AutoXplainR:::canonicalize_task_specific_grids(
      list(kernel = list(list(cost = 1, gamma_multiplier = 1, epsilon = 0.2))),
      task = "binary",
      custom_families = "kernel"
    ),
    "must use `epsilon = 0.1`"
  )
})

test_that("control metrics are task-specific and defaults remain equivalent", {
  data <- data.frame(x = seq(-2, 2, length.out = 45), z = rep(1:3, 15))
  data$y <- data$x^2 + data$z / 5
  evaluation <- make_disjoint_evaluation(data, "y", 1:10)
  expect_error(
    autoxplain(
      data, "y", test_data = evaluation, model_set = "tuned",
      learners = "linear", max_models = 1,
      tuning_control = tuning_control(metric = "log_loss")
    ),
    "not available for task `regression`"
  )
  first <- autoxplain(
    data, "y", test_data = evaluation, model_set = "tuned",
    portfolio = "core", max_models = 3, nfolds = 3, seed = 102
  )
  second <- autoxplain(
    data, "y", test_data = evaluation, model_set = "tuned",
    portfolio = "core", max_models = 3, nfolds = 3, seed = 102,
    tuning_control = tuning_control()
  )
  expect_identical(first$tuning$plan, second$tuning$plan)
  expect_equal(first$tuning$candidates$cv_score, second$tuning$candidates$cv_score)
  expect_identical(
    first$tuning$out_of_fold_predictions,
    second$tuning$out_of_fold_predictions
  )
})

test_that("failure_policy stop aborts the first configuration failure", {
  skip_if_not_installed("mgcv")
  set.seed(71)
  training <- data.frame(group = factor(rep(c("a", "b", "c"), 20)))
  training$y <- as.numeric(training$group) + rnorm(nrow(training), sd = 0.1)
  evaluation <- make_disjoint_evaluation(training, "y", 1:12)

  continued <- autoxplain(
    training,
    "y",
    test_data = evaluation,
    model_set = "tuned",
    learners = c("linear", "additive"),
    max_models = 2,
    nfolds = 3,
    tuning_control = tuning_control(failure_policy = "continue"),
    seed = 71
  )
  expect_identical(continued$tuning$families_resampling_failed, "additive")
  expect_identical(continued$tuning$control$failure_policy, "continue")

  stop_refit <- continued$tuning
  stop_refit$control$failure_policy <- "stop"
  expect_error(
    AutoXplainR:::refit_tuned_candidates(
      stop_refit,
      data = training,
      target = "y",
      task = "regression",
      fitter = function(...) stop("deliberate refit failure", call. = FALSE)
    ),
    "failed during full-training refit.*failure_policy.*deliberate refit"
  )

  expect_error(
    autoxplain(
      training,
      "y",
      test_data = evaluation,
      model_set = "tuned",
      learners = c("linear", "additive"),
      max_models = 2,
      nfolds = 3,
      tuning_control = tuning_control(failure_policy = "stop"),
      seed = 71
    ),
    "Configuration `additive_01`.*tuning fold 1.*failure_policy"
  )
})

test_that("fold fit specifications collapse only effective hyperparameters", {
  training <- data.frame(
    limited = c(1, 1, 2, 2, 2),
    smooth = c(1, 2, 3, 4, 4),
    rich = seq_len(5),
    y = c(2, 1, 4, 3, 5)
  )
  configuration <- function(id, family, parameters) {
    data.frame(
      configuration_id = id,
      family = family,
      parameters = I(list(parameters)),
      search_seed = 731L,
      seed = AutoXplainR:::stable_configuration_seed(731L, family, parameters),
      stringsAsFactors = FALSE
    )
  }
  expect_collapsed <- function(family, first, second) {
    first_spec <- AutoXplainR:::tuning_configuration_fit_spec(
      configuration("first", family, first), training, "y"
    )
    second_spec <- AutoXplainR:::tuning_configuration_fit_spec(
      configuration("second", family, second), training, "y"
    )
    expect_false(identical(
      first_spec$requested_parameter_key,
      second_spec$requested_parameter_key
    ))
    expect_identical(
      first_spec$effective_parameter_key,
      second_spec$effective_parameter_key
    )
    expect_false(identical(
      first_spec$requested_configuration_seed,
      second_spec$requested_configuration_seed
    ))
    expect_identical(first_spec$fit_seed, second_spec$fit_seed)
    list(first = first_spec, second = second_spec)
  }

  additive <- expect_collapsed(
    "additive",
    list(k = 8L, gamma = 1, select = TRUE),
    list(k = 20L, gamma = 1, select = TRUE)
  )
  expect_identical(
    additive$first$effective_parameters$smooth_k,
    c(smooth = 3L, rich = 4L)
  )
  forest <- expect_collapsed(
    "forest",
    list(
      num.trees = 100L, mtry = 8L, min.node.size = 5L,
      sample.fraction = 0.8, splitrule = "default"
    ),
    list(
      num.trees = 100L, mtry = 20L, min.node.size = 5L,
      sample.fraction = 0.8, splitrule = "default"
    )
  )
  expect_identical(forest$first$effective_parameters$mtry, 3L)
  neighbors <- expect_collapsed(
    "neighbors",
    list(k = 10L, distance = 2, kernel = "optimal"),
    list(k = 20L, distance = 2, kernel = "optimal")
  )
  expect_identical(neighbors$first$effective_parameters$k, 4L)
  mars <- expect_collapsed(
    "mars",
    list(degree = 1L, nprune = 10L),
    list(degree = 1L, nprune = 20L)
  )
  expect_identical(mars$first$effective_parameters$nprune, 3L)
})

test_that("fold scores record requested and effective fit identities before fitting", {
  configuration <- function(id, k) {
    parameters <- list(k = as.integer(k), distance = 2, kernel = "optimal")
    data.frame(
      configuration_id = id,
      family = "neighbors",
      backend = "kknn",
      model = "weighted nearest-neighbor regression",
      parameters = I(list(parameters)),
      search_seed = 902L,
      seed = AutoXplainR:::stable_configuration_seed(902L, "neighbors", parameters),
      stringsAsFactors = FALSE
    )
  }
  training <- data.frame(x = seq_len(5), y = c(1, 3, 2, 5, 4))
  validation <- data.frame(x = c(1.5, 3.5), y = c(2, 3))
  fold <- list(
    training = training,
    validation = validation,
    fold_label = "audit-fold",
    validation_row = 1:2,
    source_row = c("one", "two"),
    validation_rows_requested = 2L,
    omitted_validation_row = integer(),
    novel_levels_mapped = 0L
  )
  first <- AutoXplainR:::score_tuning_configuration(
    configuration("neighbors_01", 10L),
    fold,
    "y",
    "regression",
    1L,
    retain_oof = FALSE
  )$score
  second <- AutoXplainR:::score_tuning_configuration(
    configuration("neighbors_02", 20L),
    fold,
    "y",
    "regression",
    1L,
    retain_oof = FALSE
  )$score

  expect_identical(first$configuration_id, "neighbors_01")
  expect_identical(second$configuration_id, "neighbors_02")
  expect_false(identical(
    first$requested_parameter_key,
    second$requested_parameter_key
  ))
  expect_identical(first$effective_parameter_key, second$effective_parameter_key)
  expect_false(identical(
    first$requested_configuration_seed,
    second$requested_configuration_seed
  ))
  expect_identical(first$fit_seed, second$fit_seed)
  expect_identical(first$requested_parameters[[1L]]$k, 10L)
  expect_identical(second$requested_parameters[[1L]]$k, 20L)
  expect_identical(first$effective_parameters[[1L]]$k, 4L)
  expect_identical(second$effective_parameters[[1L]]$k, 4L)
})

test_that("effective fit seeds stay invariant to ordinary grid ordering", {
  shallow <- list(maxdepth = 2L, cp = 0.02, minsplit = 12L)
  flexible <- list(maxdepth = 6L, cp = 0.001, minsplit = 5L)
  make_plan <- function(grid) {
    AutoXplainR:::local_tuning_plan(
      max_models = 2L,
      n = 80L,
      p = 2L,
      task = "regression",
      n_classes = 1L,
      learners = "tree",
      seed = 912L,
      custom_grids = list(tree = grid),
      family_budgets = c(tree = 2L)
    )
  }
  first <- make_plan(list(shallow, flexible))
  reordered <- make_plan(list(flexible, shallow))
  training <- data.frame(x = seq_len(20), z = 20:1, y = seq_len(20))
  specs <- function(plan) {
    lapply(seq_len(nrow(plan)), function(index) {
      AutoXplainR:::tuning_configuration_fit_spec(
        plan[index, , drop = FALSE], training, "y"
      )
    })
  }
  first_specs <- specs(first)
  reordered_specs <- specs(reordered)
  first_keys <- vapply(
    first_specs, `[[`, character(1), "effective_parameter_key"
  )
  reordered_keys <- vapply(
    reordered_specs, `[[`, character(1), "effective_parameter_key"
  )
  first_seeds <- vapply(first_specs, `[[`, integer(1), "fit_seed")
  reordered_seeds <- vapply(reordered_specs, `[[`, integer(1), "fit_seed")
  expect_identical(
    first_seeds,
    reordered_seeds[match(first_keys, reordered_keys)]
  )
})

test_that("full-training fits retain requested and effective provenance", {
  plan <- AutoXplainR:::local_tuning_plan(
    max_models = 1L,
    n = 20L,
    p = 1L,
    task = "regression",
    n_classes = 1L,
    learners = "linear",
    seed = 144L
  )
  training <- data.frame(x = seq_len(20), y = 2 * seq_len(20) + 1)
  model <- AutoXplainR:::fit_tuning_configuration(
    plan,
    training,
    "y",
    "regression"
  )
  provenance <- attr(model, "autoxplain_tuning_fit", exact = TRUE)

  expect_identical(provenance$configuration_id, plan$configuration_id[[1L]])
  expect_identical(provenance$scope, "full_training_refit")
  expect_identical(provenance$search_seed, 144L)
  expect_identical(provenance$requested_parameters, plan$parameters[[1L]])
  expect_identical(provenance$effective_parameters, plan$parameters[[1L]])
  expect_identical(provenance$requested_configuration_seed, plan$seed[[1L]])
  expect_identical(
    provenance$fit_seed,
    AutoXplainR:::stable_configuration_seed(
      144L,
      "linear",
      provenance$effective_parameters
    )
  )
})

test_that("refit attempts expose the final effective fit identity", {
  set.seed(145)
  data <- data.frame(x = rnorm(45), z = rnorm(45))
  data$y <- 2 * data$x - data$z + rnorm(45, sd = 0.1)
  result <- autoxplain(
    data,
    "y",
    model_set = "tuned",
    learners = "linear",
    max_models = 1L,
    nfolds = 3L,
    seed = 145L
  )
  attempt <- result$tuning$refit$attempts[1L, , drop = FALSE]
  provenance <- attr(
    result$models[[attempt$model_id[[1L]]]],
    "autoxplain_tuning_fit",
    exact = TRUE
  )

  expect_identical(attempt$configuration_id, provenance$configuration_id)
  expect_identical(attempt$requested_parameter_key, provenance$requested_parameter_key)
  expect_identical(attempt$effective_parameter_key, provenance$effective_parameter_key)
  expect_identical(
    attempt$requested_configuration_seed,
    provenance$requested_configuration_seed
  )
  expect_identical(attempt$fit_seed, provenance$fit_seed)
  expect_identical(attempt$requested_parameters[[1L]], provenance$requested_parameters)
  expect_identical(attempt$effective_parameters[[1L]], provenance$effective_parameters)
})
