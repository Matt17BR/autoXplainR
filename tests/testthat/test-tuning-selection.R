test_that("optimizer status does not silently turn termination into convergence", {
  status <- AutoXplainR:::model_optimization_record
  failed <- structure(list(convergence = 1L), class = "nnet")
  expect_identical(status(failed)$status, "not_converged")
  expect_match(status(failed)$message, "iteration limit")
  expect_identical(status(structure(list(convergence = 0L), class = "nnet"))$status, "converged")
  expect_identical(status(structure(list(converged = FALSE), class = c("glm", "lm")))$status, "not_converged")
  gam <- structure(list(converged = TRUE, outer.info = list(conv = "iteration limit reached")),
    class = c("gam", "glm", "lm")
  )
  expect_identical(status(gam)$status, "not_converged")
  expect_identical(status(structure(list(jerr = -2L), class = "glmnet"))$status, "partial")
  expect_identical(status(structure(list(), class = "svm"))$status, "unknown")
  expect_identical(status(structure(list(), class = "rpart"))$status, "not_applicable")
})

test_that("fold presentation preserves adverse evidence while omitting empty columns", {
  candidate <- data.frame(
    configuration_id = "tree_01", family = "tree", status = "ok",
    cv_score = 1, cv_se = .1, within_threshold = TRUE
  )
  folds <- data.frame(
    configuration_id = "tree_01", fold = 1:2, score = c(.9, 1.1),
    training_rows = 20L, validation_rows = 10L, validation_rows_omitted = 0L,
    fit_seed = 17L, optimization_status = "not_applicable", warning = "", error = ""
  )
  folds$requested_parameters <- I(rep(list(list(maxdepth = 2L)), 2L))
  folds$effective_parameters <- folds$requested_parameters
  folds$learned <- I(rep(list(list(leaves = 2L)), 2L))
  html <- AutoXplainR:::selection_candidate_details(candidate, list(folds = folds))
  for (label in c("Training rows", "Validation rows", "Seed", "Optimizer", "Not applicable")) {
    expect_match(html, label, fixed = TRUE)
  }
  expect_match(html, "No validation rows were omitted", fixed = TRUE)
  for (label in c(">Warnings</th>", ">Error</th>", ">Rows omitted</th>", "validation_rows_omitted")) {
    expect_false(grepl(label, html, fixed = TRUE))
  }
  folds$validation_rows_omitted[[2L]] <- 3L
  folds$error[[2L]] <- "Prediction rejected one validation batch"
  html <- AutoXplainR:::selection_candidate_details(candidate, list(folds = folds))
  expect_match(html, ">Rows omitted</th>", fixed = TRUE)
  expect_match(html, ">Error</th>", fixed = TRUE)
  expect_match(html, "Prediction rejected one validation batch", fixed = TRUE)
  expect_false(grepl("No validation rows were omitted", html, fixed = TRUE))
  expect_identical(folds$optimization_status, rep("not_applicable", 2L))
})

test_that("real iteration-limit neural fits are excluded unless explicitly retained", {
  set.seed(739)
  data <- as.data.frame(matrix(rnorm(160 * 12), 160, 12))
  data$y <- rnorm(160)
  control <- tuning_control(
    grids = list(neural = list(size = 8L, decay = .001, maxit = 1L)),
    family_budgets = c(linear = 1L, neural = 1L)
  )
  result <- autoxplain(data, "y",
    learners = c("linear", "neural"),
    tuning_control = control, nfolds = 3, seed = 739, explain = FALSE
  )
  neural <- result$tuning$candidates[result$tuning$candidates$family == "neural", ]
  expect_identical(neural$status, "failed")
  expect_true(neural$optimization_issues > 0)
  expect_false(neural$selected)
  expect_true(is.na(neural$cv_score))
  folds <- result$tuning$fold_scores[result$tuning$fold_scores$configuration_id == "neural_01", ]
  expect_true(all(folds$optimization_status == "not_converged"))
  expect_true(all(grepl("Optimization policy: exclude", folds$error, fixed = TRUE)))
  expect_true(all(vapply(folds$learned, function(x) identical(x$convergence, 1L), logical(1))))
  html <- AutoXplainR:::render_model_selection(result)
  expect_match(html, "Not converged", fixed = TRUE)
  expect_false(grepl('cx=""', html, fixed = TRUE))

  control$family_budgets <- c(neural = 1L)
  control$optimization_policy <- "warn"
  retained <- autoxplain(data, "y",
    learners = "neural", tuning_control = control,
    nfolds = 3, seed = 739, explain = FALSE
  )
  expect_identical(retained$tuning$candidates$status, "ok")
  expect_true(is.finite(retained$tuning$candidates$cv_score))
  expect_true(all(grepl("iteration limit", retained$tuning$fold_scores$warning, fixed = TRUE)))
  expect_identical(retained$tuning$refit$attempts$optimization_status, "not_converged")
  expect_identical(retained$tuning$candidates$refit_optimization_status, "not_converged")
  expect_true(all(tuning_evidence(retained)$folds$optimization_status == "not_converged"))
})

test_that("the exact one-SE decision is hand-reconstructable and tied minima preserve order", {
  candidates <- data.frame(
    configuration_id = c("neural_01", "tree_01", "linear_01", "neural_02"),
    family = c("neural", "tree", "linear", "neural"),
    simplicity_rank = c(9, 4, 1, 9), complexity_proxy = c(5, 4, 3, 3),
    complexity_definition = c("weights", "leaves", "coefficients", "weights"),
    cv_score = c(1, 1.1, 1.21, 1), cv_se = c(.2, .1, .1, .01), status = "ok"
  )
  selected <- AutoXplainR:::select_one_se_candidate(candidates, candidates$cv_score <= 1.2)
  expect_identical(candidates$configuration_id[[selected]], "tree_01")
  record <- AutoXplainR:::tuning_selection_record(candidates, "tree_01", "one_se")
  expect_equal(record$threshold, 1.2)
  expect_identical(record$best_configuration, "neural_01")
  expect_setequal(record$eligible, c("neural_01", "tree_01", "neural_02"))
  expect_identical(record$family_priority$family, c("linear", "tree", "neural"))
  expect_match(record$variability_scope, "not a confidence interval")
  expect_match(record$priority_scope, "omit some regularization")
  best <- AutoXplainR:::tuning_selection_record(candidates, "neural_01", "best")
  expect_identical(best$eligible, "neural_01")
  expect_equal(best$threshold, 1)
  # Giving neural first preference changes a policy choice, not the CV scores.
  candidates$simplicity_rank <- match(candidates$family, c("neural", "tree", "linear"))
  preferred <- AutoXplainR:::select_one_se_candidate(candidates, candidates$cv_score <= 1.2)
  expect_identical(candidates$configuration_id[[preferred]], "neural_02")
})

test_that("pooled RMSE and its selection SE use loss weights rather than mean fold RMSE", {
  observed <- AutoXplainR:::tuning_fold_uncertainty(c(1, 3), c(1, 3), "rmse")
  # Squared losses 1 and 9, weights 1/4 and 3/4: mean 7, corrected variance 32.
  # Kish effective folds = 8/5; delta derivative = 1/(2*sqrt(7)).
  expect_equal(observed$score, sqrt(7))
  expect_equal(observed$se, sqrt(20) / (2 * sqrt(7)))
  expect_false(isTRUE(all.equal(observed$score, mean(c(1, 3)))))
})

test_that("recorded grids preserve coverage and caller rationale is not invented", {
  plan <- AutoXplainR:::local_tuning_plan(15,
    n = 100, p = 3, task = "regression",
    learners = c("linear", "tree", "neural"), seed = 8
  )
  space <- attr(plan, "search_space")
  expect_equal(space$families$available, c(1, 12, 12))
  expect_equal(space$families$scheduled, c(1, 7, 7))
  expect_equal(space$families$untested, c(0, 5, 5))
  expect_match(space$limitation, "engineering choices")
  custom <- AutoXplainR:::local_tuning_plan(1,
    n = 100, p = 3, task = "regression",
    learners = "tree", seed = 8, custom_grids = list(tree = list(maxdepth = 2L, cp = .03, minsplit = 8L))
  )
  expect_identical(attr(custom, "search_space")$families$origin, "user_grid")
  expect_match(attr(custom, "search_space")$families$rationale, "does not infer")
})

test_that("tuning report retains controls, statuses, numeric evidence and executable grid code", {
  result <- autoxplain(mtcars, "mpg", max_models = 4, nfolds = 3, seed = 2026, explain = FALSE)
  evidence <- tuning_evidence(result)
  expect_identical(result$tuning$schema_version, 5L)
  expect_identical(evidence$status, "computed")
  expected_fields <- c("requested_parameters", "effective_parameters", "learned", "optimization_status")
  expect_true(all(expected_fields %in% names(evidence$folds)))
  expect_false(any(c("source_row", "training_row", "truth", "estimate") %in% names(evidence$folds)))
  expect_true(any(evidence$boundaries$position == "fixed"))
  expect_match(evidence$boundaries$interpretation[[1]], "Joint tuples|not varied")
  expect_equal(evidence$selection$threshold, evidence$selection$best_score + evidence$selection$best_se)
  eligible <- evidence$candidates$status == "ok" & evidence$candidates$cv_score <= evidence$selection$threshold
  expect_identical(evidence$candidates$within_threshold, eligible)
  html <- AutoXplainR:::render_model_selection(result)
  expect_match(html, 'id="selection"', fixed = TRUE)
  expect_match(html, 'class="workspace-page selection-section"', fixed = TRUE)
  expect_match(html, 'data-page="selection"', fixed = TRUE)
  expect_match(html, "Fold scores show variability, not confidence intervals", fixed = TRUE)
  expect_match(html, "Full-training refit attempts", fixed = TRUE)
  expect_match(html, "outer-training|Outer training")
  expect_false(grepl('cx=""', html, fixed = TRUE))
  expect_false(grepl("least-flexible model wins", html, fixed = TRUE))
  env <- new.env()
  code <- AutoXplainR:::selection_grid_code(result)
  eval(parse(text = code), env)
  expect_s3_class(env$control, "autoxplain_tuning_control")
  expect_match(code, "seed = 2026", fixed = TRUE)
  replay <- AutoXplainR:::local_tuning_plan(4,
    n = nrow(result$training_data), p = length(result$features),
    task = result$task, learners = result$tuning$learners, seed = 2026,
    custom_grids = env$control$grids, family_budgets = env$control$family_budgets
  )
  expect_identical(replay$parameters, result$tuning$plan$parameters)
  expect_identical(replay$seed, result$tuning$plan$seed)
  preferred <- autoxplain(
    mtcars, "mpg",
    max_models = 4, nfolds = 3, seed = 2026,
    tuning_control = tuning_control(family_priority = c("neural", "tree", "linear")), explain = FALSE
  )
  expect_identical(preferred$tuning$selection$family_priority$family, c("neural", "tree", "linear"))
  expect_identical(preferred$tuning$selected_configuration, "neural_01")
  expect_equal(sort(preferred$tuning$candidates$cv_score), sort(result$tuning$candidates$cv_score))
  expect_error(tuning_control(family_priority = c("tree", "tree")), "unique")
  expect_error(
    autoxplain(
      mtcars, "mpg",
      learners = "tree", tuning_control = tuning_control(family_priority = "linear")
    ),
    "family_priority"
  )
})

test_that("selection reports are inert and handle absent or hostile diagnostic records", {
  result <- autoxplain(mtcars, "mpg", learners = "linear", nfolds = 3, explain = FALSE)
  result$tuning$fold_scores$error[[1]] <- '</script><img src=x onerror="alert(1)">'
  before <- .Random.seed
  html <- AutoXplainR:::render_model_selection(result)
  expect_identical(.Random.seed, before)
  expect_false(grepl("<img src=x", html, fixed = TRUE))
  expect_match(html, "&lt;/script&gt;&lt;img", fixed = TRUE)
  legacy <- result
  legacy$tuning$search_space <- NULL
  expect_match(AutoXplainR:::render_model_selection(legacy), "predates recorded search rationale")
  result$tuning <- NULL
  expect_identical(tuning_evidence(result)$status, "not_run")
  expect_match(AutoXplainR:::render_model_selection(result), "No local training-only tuning")
})

test_that("model provenance explains predefined and externally fitted workflows", {
  quick <- autoxplain(mtcars, "mpg", model_set = "quick", explain = FALSE)
  html <- AutoXplainR:::render_model_selection(quick)
  expect_true(grepl("Model provenance", html, fixed = TRUE))
  expect_true(grepl("predefined model set", html, fixed = TRUE))
  expect_true(grepl('href="#spec-', html, fixed = TRUE))
  expect_false(grepl("No local training-only tuning record", html, fixed = TRUE))
  quick$provenance$workflow <- "supplied-model evaluation"
  quick$training_available <- FALSE
  quick$training_data <- NULL
  html <- AutoXplainR:::render_model_selection(quick)
  expect_true(grepl("caller chose the primary model", html, fixed = TRUE))
  expect_true(grepl("did not observe their fitting", html, fixed = TRUE))
  expect_true(grepl("Training data were not provided", html, fixed = TRUE))
  expect_false(grepl("0 processed training", html, fixed = TRUE))
})

test_that("multinomial optimizer budgets record backend defaults and explicit controls accurately", {
  set.seed(78)
  data <- data.frame(x = rnorm(90), y = factor(sample(c("a", "b", "c"), 90, replace = TRUE)))
  default <- AutoXplainR:::fit_linear_learner(data, "y", "multiclass", list(), 1L)
  learned <- AutoXplainR:::tuning_learned_settings(default)
  expect_identical(learned$maxit, as.integer(formals(nnet::nnet.default)$maxit))
  expect_identical(learned$maxit_source, "nnet backend default at fit time")
  explicit <- nnet::multinom(y ~ x, data, maxit = 2L, trace = FALSE)
  learned <- AutoXplainR:::tuning_learned_settings(explicit)
  expect_identical(learned$maxit, 2L)
  expect_identical(learned$maxit_source, "explicit fitted call")
})

test_that("an interior scheduled winner does not imply usable fits on either side", {
  plan <- data.frame(configuration_id = paste0("tree_0", 1:3), family = "tree")
  plan$parameters <- I(lapply(1:3, function(depth) list(maxdepth = depth)))
  candidates <- transform(plan[c("configuration_id", "family")],
    status = c("failed", "ok", "failed"), cv_score = c(NA, 1, NA)
  )
  boundaries <- AutoXplainR:::tuning_boundary_evidence(list(plan = plan, candidates = candidates))
  expect_identical(boundaries$position, "interior")
  expect_match(boundaries$interpretation, "including failed attempts", fixed = TRUE)
  evidence <- list(
    task = "regression", candidates = candidates, boundaries = boundaries,
    folds = data.frame(configuration_id = plan$configuration_id),
    search_space = list(families = data.frame(
      family = "tree", origin = "user_grid",
      scheduled = 3L, available = 3L, untested = 0L, rationale = "Caller supplied these settings."
    ))
  )
  evidence$folds$requested_parameters <- plan$parameters
  html <- AutoXplainR:::selection_family_rationale("tree", evidence)
  expect_true(grepl("Scheduled ranges include failed attempts; only 1 of 3", html, fixed = TRUE))
  expect_true(grepl("in the scheduled range", html, fixed = TRUE))
})

test_that("schema4 saved tuning omits exact selection evidence without partial-matching the rule", {
  result <- autoxplain(mtcars, "mpg", max_models = 3L, nfolds = 2L, explain = FALSE)
  result$tuning$schema_version <- 4L
  result$tuning[["selection"]] <- NULL
  result$tuning[["search_space"]] <- NULL
  original <- result$tuning$candidates
  withr::local_options(warnPartialMatchDollar = TRUE)
  evidence <- expect_warning(tuning_evidence(result), NA)
  expect_identical(evidence$status, "unavailable")
  expect_identical(evidence$selection$status, "not_recorded")
  expect_null(evidence$selection$threshold)
  expect_identical(evidence$candidates, original)
  html <- expect_warning(AutoXplainR:::render_model_selection(result), NA)
  expect_true(grepl("exact training selection record is unavailable", html, fixed = TRUE))
  expect_true(grepl("Retained candidate results", html, fixed = TRUE))
  expect_false(grepl("Eligible loss", html, fixed = TRUE))
})
