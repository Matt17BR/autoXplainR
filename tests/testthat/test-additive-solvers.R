solver_fixture <- function(n = 160L) {
  set.seed(7001L)
  data <- data.frame(
    x = runif(n, -2, 2), z = rnorm(n),
    group = factor(rep(c("ordinary", "a:b", "rare"), length.out = n))
  )
  names(data)[[2L]] <- "rain + sun"
  data$y <- sin(data$x) + .3 * data[[2L]] + .2 * (data$group == "a:b") + rnorm(n, sd = .1)
  data
}

test_that("older additive grids remain valid and explicit solver controls are checked", {
  control <- tuning_control(grids = list(additive = list(k = 5, gamma = 1, select = TRUE)))
  expect_identical(control$grids$additive[[1L]]$solver, "auto")
  expect_identical(control$grids$additive[[1L]]$discrete_bins, 10000L)
  expect_error(tuning_control(grids = list(additive = list(list(
    k = 5, gamma = 1, select = TRUE, solver = "magic"
  )))), "solver")
  expect_error(tuning_control(grids = list(additive = list(list(
    k = 5, gamma = 1, select = TRUE, discrete_bins = 3
  )))), "discrete_bins")
})

test_that("basis dimension counting preserves rare values beyond its prefix", {
  x <- c(rep(1, 4997L), 2, 3, 4)
  expect_identical(AutoXplainR:::additive_distinct_count(x, 9L), 4L)
  expect_identical(AutoXplainR:::additive_distinct_count(c(NA, Inf, x), 9L), 4L)
  expect_identical(AutoXplainR:::additive_distinct_count(seq_len(5000L), 9L), 9L)
  data <- data.frame(rare = x, continuous = seq_along(x), y = seq_along(x))
  effective <- AutoXplainR:::effective_learner_parameters(
    "additive",
    list(k = 8L, gamma = 1, select = TRUE), data, "y"
  )
  expect_identical(effective$smooth_k, c(rare = 3L, continuous = 8L))
})

test_that("unused discretization controls cannot change an effective continuous fit", {
  data <- solver_fixture()
  parameters <- list(k = 5L, gamma = 1, select = TRUE, solver = "gam", discrete_bins = 1000L)
  first <- AutoXplainR:::effective_learner_parameters("additive", parameters, data, "y")
  parameters$discrete_bins <- 10000L
  second <- AutoXplainR:::effective_learner_parameters("additive", parameters, data, "y")
  expect_identical(first, second)
  changed_target <- data
  changed_target$y <- rev(changed_target$y)
  expect_identical(
    second,
    AutoXplainR:::effective_learner_parameters("additive", parameters, changed_target, "y")
  )
})

test_that("automatic solver routing uses fitting size and honors explicit choices", {
  parameters <- list(k = 5L, gamma = 1, select = TRUE, solver = "auto")
  small <- solver_fixture()
  expect_identical(AutoXplainR:::effective_learner_parameters(
    "additive", parameters, small, "y"
  )$solver, "gam")
  tall <- data.frame(x = seq_len(10000L), y = seq_len(10000L))
  expect_identical(AutoXplainR:::effective_learner_parameters(
    "additive", parameters, tall, "y"
  )$solver, "bam")
  wide <- as.data.frame(matrix(seq_len(1000L * 31L), 1000L, 31L))
  names(wide)[31L] <- "y"
  effective <- AutoXplainR:::effective_learner_parameters("additive", parameters, wide, "y")
  expect_identical(effective$solver, "bam")
  expect_null(effective$discrete_bins)
  permuted <- wide
  permuted$y <- rev(permuted$y)
  expect_identical(effective, AutoXplainR:::effective_learner_parameters(
    "additive", parameters, permuted, "y"
  ))
  parameters$solver <- "gam"
  expect_identical(AutoXplainR:::effective_learner_parameters(
    "additive", parameters, wide, "y"
  )$solver, "gam")
})

test_that("search planning freezes auto solvers while fitting learns local bases", {
  skip_if_package_unavailable("mgcv")
  data <- data.frame(x = seq_len(10000L), y = sin(seq_len(10000L)))
  automatic <- list(k = 5L, gamma = 1, select = TRUE, solver = "auto")
  explicit <- automatic
  explicit$solver <- "gam"
  plan <- AutoXplainR:::local_tuning_plan(2L, nrow(data), 1L, "regression", 1L,
    learners = "additive", custom_grids = list(additive = list(automatic, explicit)),
    additive_planning_data = data, additive_target = "y"
  )
  expect_identical(plan$parameters[[1L]]$solver, "bam")
  expect_identical(plan$parameters[[2L]]$solver, "gam")
  expect_identical(attr(plan, "additive_policy")$configurations$additive_01$fitting_rows, 10000L)
  fold <- data[1:80, ]
  fold$x <- rep(1:4, 20L)
  model <- AutoXplainR:::fit_additive_learner(fold, "y", "regression", plan$parameters[[1L]], 10L)
  expect_true(inherits(model$fit, "bam"))
  expect_identical(model$fit_details$computation$requested_solver, "auto")
  expect_identical(model$fit_details$computation$planning_rows, 10000L)
  expect_identical(model$fit_details$computation$fitting_rows, 80L)
  expect_identical(model$fit_details$effective_parameters$smooth_k, c(x = 3L))
  expect_equal(model$fit$smooth[[1L]]$bs.dim, 3)
})

test_that("BAM predictions and metadata agree with the actual fitted engine", {
  skip_if_package_unavailable("mgcv")
  data <- solver_fixture()
  for (solver in c("gam", "bam", "bam_discrete")) {
    parameters <- list(k = 5L, gamma = 1, select = TRUE, solver = solver, discrete_bins = 10000L)
    model <- AutoXplainR:::fit_additive_learner(data, "y", "regression", parameters, 17L)
    raw <- data[1:20, setdiff(names(data), "y")]
    encoded <- raw
    names(encoded) <- unname(model$fit_details$feature_map)
    native <- stats::predict(model$fit, encoded, type = "response")
    expect_equal(predict(model, raw), as.numeric(native), tolerance = 1e-12)
    expect_identical(inherits(model$fit, "bam"), solver != "gam")
    expect_identical(model$fit_details$computation$solver, solver)
    expect_identical(model$fit_details$computation$method, if (solver == "gam") "REML" else "fREML")
    expect_identical(model$fit_details$computation$discrete, solver == "bam_discrete")
    expect_identical(model$fit_details$requested_parameters, parameters)
    expect_identical(AutoXplainR:::model_optimization_record(model)$status, "converged")
    if (solver == "bam_discrete") {
      expect_identical(model$fit_details$effective_parameters$discrete_bins, 10000L)
      expect_true(is.logical(model$fit$mgcv.conv))
    } else {
      expect_null(model$fit_details$effective_parameters$discrete_bins)
    }
    path <- tempfile(fileext = ".rds")
    saveRDS(model, path)
    expect_equal(predict(readRDS(path), raw), as.numeric(native), tolerance = 1e-12)
    unlink(path)
  }
})

test_that("failed and absent BAM convergence diagnostics cannot become success", {
  skip_if_package_unavailable("mgcv")
  model <- AutoXplainR:::fit_additive_learner(
    solver_fixture(), "y", "regression",
    list(k = 5L, gamma = 1, select = TRUE, solver = "bam_discrete"), 10L
  )
  model$fit$mgcv.conv <- FALSE
  model$fit$converged <- TRUE
  expect_identical(AutoXplainR:::model_optimization_record(model)$status, "not_converged")
  model$fit$mgcv.conv <- NA
  model$fit$converged <- TRUE
  model$fit$outer.info <- NULL
  expect_identical(AutoXplainR:::model_optimization_record(model)$status, "unknown")
  model$fit$mgcv.conv <- list(fully.converged = FALSE)
  expect_identical(AutoXplainR:::model_optimization_record(model)$status, "not_converged")
})

test_that("binary BAM resampling retains the actual solver and all validation rows", {
  skip_if_package_unavailable("mgcv")
  data <- solver_fixture(240L)
  data$y <- factor(ifelse(seq_len(nrow(data)) %% 13L == 0L, "event", "ordinary"),
    levels = c("ordinary", "event")
  )
  result <- autoxplain(data, "y",
    learners = "additive", max_models = 1L, nfolds = 2L,
    tuning_control = tuning_control(grids = list(additive = list(list(
      k = 5L, gamma = 1, select = TRUE, solver = "bam_discrete"
    )))), explain = FALSE, seed = 44L
  )
  expect_true(all(is.finite(predict(result, data))))
  folds <- result$tuning$fold_scores
  expect_true(all(folds$error == ""))
  expect_equal(sum(folds$validation_rows), result$tuning$rows_requested)
  expect_true(all(vapply(folds$learned, function(x) {
    identical(x$computation$solver, "bam_discrete") && identical(x$method, "fREML")
  }, logical(1))))
  spec <- AutoXplainR:::model_specification(result, "main_model")
  expect_identical(spec$learned$`Fitting procedure`$discrete_bins, 10000L)
  expect_match(spec$summary, "bam_discrete", fixed = TRUE)
})

test_that("native GAM and BAM evaluation reports retain their smoothing identity", {
  skip_if_package_unavailable("mgcv")
  data <- solver_fixture()
  for (discrete in c(FALSE, TRUE)) {
    native <- mgcv::bam(y ~ s(x, k = 5),
      data = data[1:140, ],
      method = "fREML", discrete = discrete
    )
    assessed <- evaluate_models(list(native = native), data[141:160, ], "y",
      training_data = data[1:140, ], evaluation_role = "test"
    )
    expect_equal(predict(assessed, data[141:160, ]),
      as.numeric(predict(native, data[141:160, ], type = "response")),
      tolerance = 1e-12
    )
    spec <- AutoXplainR:::model_specification(assessed, "native")
    expect_identical(spec$engine, "mgcv")
    expect_identical(AutoXplainR:::model_family_name(native), "additive")
    expect_identical(AutoXplainR:::model_backend_name(native), "mgcv")
    expect_identical(AutoXplainR:::friendly_model_type(native), "generalized additive model")
    expect_identical(spec$parameters$method, "fREML")
    expect_identical(spec$parameters$solver, if (discrete) "bam_discrete" else "bam")
    expect_equal(spec$learned$`Total effective degrees of freedom`, sum(native$edf))
    expect_identical(spec$learned$`Checked optimization status`, "converged")
    expect_match(spec$summary, "smooth term", fixed = TRUE)
  }
  # Tensor-product smooths do not expose a single bs.dim field. Keep that
  # absence explicit rather than failing while describing a valid custom GAM.
  names(data)[2L] <- "z"
  native <- mgcv::gam(y ~ te(x, z, k = c(4, 4)),
    data = data,
    method = "REML"
  )
  spec <- AutoXplainR:::model_specification(list(models = list(native = native)), "native")
  expect_identical(spec$engine, "mgcv")
  expect_identical(spec$parameters$method, "REML")
  expect_equal(spec$learned$`Total effective degrees of freedom`, sum(native$edf))
})

test_that("native binary BAM keeps its probability event when arrays are unwrapped", {
  skip_if_package_unavailable("mgcv")
  data <- solver_fixture()
  data$y <- factor(ifelse(seq_len(nrow(data)) %% 7L == 0L, "event", "ordinary"),
    levels = c("ordinary", "event")
  )
  native <- mgcv::bam(y ~ s(x, k = 5),
    data = data[1:140, ], family = binomial(),
    discrete = TRUE, method = "fREML"
  )
  assessed <- evaluate_models(list(native = native), data[141:160, ], "y",
    training_data = data[1:140, ], positive = "event", evaluation_role = "test"
  )
  p <- as.numeric(predict(native, data[141:160, ], type = "response"))
  expect_equal(predict(assessed, data[141:160, ]), p, tolerance = 1e-12)
  reversed <- evaluate_models(list(native = native), data[141:160, ], "y",
    training_data = data[1:140, ], positive = "ordinary", evaluation_role = "test"
  )
  expect_equal(predict(reversed, data[141:160, ]), 1 - p, tolerance = 1e-12)
  expect_false(AutoXplainR:::standard_prediction_model(
    structure(native, class = c("caller_bam", class(native)))
  ))
  state <- new.env(parent = emptyenv())
  state$scale <- 1
  native$family$linkinv <- function(eta) plogis(state$scale * eta)
  changed_link <- explain_model(native, data[141:160, ], "y", positive = "event")
  initial <- AutoXplainR:::current_explainer_fingerprint(changed_link)
  state$scale <- 2
  expect_false(identical(AutoXplainR:::current_explainer_fingerprint(changed_link), initial))
})

test_that("native GAM evidence binds custom smooth prediction state off the evaluation grid", {
  skip_if_package_unavailable("mgcv")
  data <- solver_fixture()
  native <- mgcv::gam(y ~ s(x, k = 5), data = data, method = "REML")
  class(native$smooth[[1L]]) <- c("axr_context_smooth", class(native$smooth[[1L]]))
  state <- new.env(parent = emptyenv())
  state$scale <- 1
  method <- function(object, data) {
    class(object) <- class(object)[-1L]
    mgcv::Predict.matrix(object, data) * ifelse(data$x == 0, 1, state$scale)
  }
  registry <- get(".__S3MethodsTable__.", envir = asNamespace("mgcv"))
  registerS3method("Predict.matrix", "axr_context_smooth", method, envir = asNamespace("mgcv"))
  withr::defer(rm("Predict.matrix.axr_context_smooth", envir = registry))
  evaluation <- data[1:10, ]
  evaluation$x <- 0
  explainer <- explain_model(native, evaluation, "y")
  initial <- AutoXplainR:::current_explainer_fingerprint(explainer)
  off_grid <- evaluation
  off_grid$x <- 1
  before <- predict(explainer, off_grid)
  state$scale <- 2
  expect_equal(predict(explainer, evaluation), explainer$reference_predictions)
  expect_false(isTRUE(all.equal(predict(explainer, off_grid), before)))
  expect_false(identical(AutoXplainR:::current_explainer_fingerprint(explainer), initial))
})

test_that("final BAM PIRLS failure cannot be hidden by smoothing convergence", {
  skip_if_package_unavailable("mgcv")
  data <- solver_fixture()
  data$y <- as.integer(seq_len(nrow(data)) %% 3L == 0L)
  expect_warning(captured <- AutoXplainR:::capture_additive_fit(mgcv::bam(
    y ~ s(x, k = 5), data = data, family = stats::binomial(), method = "fREML",
    nthreads = 1L, control = mgcv::gam.control(maxit = 1L)
  )), "algorithm did not converge", fixed = TRUE)
  records <- captured$warnings
  final <- Filter(function(warning) identical(warning$message, "algorithm did not converge"), records)
  expect_length(final, 1L)
  expect_identical(final[[1L]]$stage, "bgam.fit")
  expect_match(final[[1L]]$call, "bgam.fit(", fixed = TRUE)
  wrapped <- AutoXplainR:::new_autoxplain_fitted_model(
    "additive", "mgcv", captured$fit, "binary", "x", list(k = 5L, gamma = 1, select = TRUE),
    fit_details = list(optimizer_warnings = records, optimizer_warnings_captured = TRUE)
  )
  expect_identical(AutoXplainR:::model_optimization_record(wrapped)$status, "not_converged")
  expect_true(AutoXplainR:::model_optimization_record(captured$fit)$status %in% c("unknown", "not_converged"))
  # A transient warning from a different stage cannot be promoted to final
  # PIRLS failure. The successful fit's numerical diagnostics still govern it.
  ordinary <- AutoXplainR:::fit_additive_learner(
    solver_fixture(), "y", "regression", list(k = 5L, gamma = 1, select = TRUE, solver = "bam"), 18L
  )
  ordinary$fit_details$optimizer_warnings <- list(
    list(message = "Possible divergence detected in fast.REML.fit", stage = "fast.REML.fit"),
    list(message = "algorithm did not converge", stage = "initialization")
  )
  expect_identical(AutoXplainR:::model_optimization_record(ordinary)$status, "converged")
})
