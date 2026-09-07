test_that("existing linear models enter the report workflow without fitting or invented training", {
  fitted <- lm(mpg ~ wt + hp, data = mtcars[1:20, ])
  direct <- unname(predict(fitted, mtcars[21:32, ]))
  local_mocked_bindings(lm.fit = function(...) stop("must not fit"), .package = "stats")
  set.seed(824)
  initial_rng <- .Random.seed
  result <- evaluate_models(list(existing = fitted), mtcars[21:32, ], "mpg", features = c("wt", "hp"))
  expect_identical(.Random.seed, initial_rng)
  expect_identical(result$models$existing, fitted)
  expect_null(result$training_data)
  expect_false(result$training_available)
  expect_true(is.na(AutoXplainR:::result_training_rows(result)))
  expect_null(AutoXplainR:::result_reference_id(result))
  expect_null(result$tuning)
  expect_identical(result$provenance$evaluation_role, "evaluation")
  expect_equal(unname(predict(result, mtcars[21:32, ])), direct)
  residual <- mtcars$mpg[21:32] - direct
  expect_equal(result$evaluation$metrics$existing[["rmse"]], sqrt(mean(residual^2)))
  expect_true(is.na(result$evaluation$improvement_over_baseline))
  expect_false(any(grepl("^baseline_", names(result$evaluation$predictions))))
  expect_true(all(is.na(result$leaderboard$training_time_ms)))
  expect_true(all(is.na(result$leaderboard$prediction_time_ms)))
  expect_identical(result$data_context$row_map$source_row, seq_len(12L))
  expect_true(all(result$data_context$row_map$partition == "evaluation"))
  expect_true(all(result$data_context$row_map$source == "data"))
  copy <- unserialize(serialize(result, NULL))
  expect_equal(predict(copy, mtcars[1:2, ]), predict(result, mtcars[1:2, ]))
  path <- tempfile(fileext = ".html")
  expect_no_error(render_model_report(copy, path, top_features = 2, n_repeats = 2, uncertainty = FALSE))
  diagnostics <- AutoXplainR:::prepare_report_diagnostics(copy)$explanations$report_diagnostics
  for (name in c("resources", "model_behavior", "prediction_disagreement")) {
    expect_identical(diagnostics[[name]]$status, "not_applicable")
  }
  html <- paste(readLines(path), collapse = "\n")
  expect_match(html, "training", fixed = TRUE)
  expect_false(grepl("0 training rows", html, fixed = TRUE))
  expect_match(html, "fitting and selection were not observed", fixed = TRUE)
  expect_match(html, 'id="evaluation"', fixed = TRUE)
  expect_match(html, 'id="data"', fixed = TRUE)
  expect_match(html, 'id="patterns"', fixed = TRUE)
})

test_that("custom binary adapters preserve an explicit first-level event and named reference", {
  d <- data.frame(x = c(-2, -1, 0, 1, 2, 3), outcome = factor(c("yes", "no", "yes", "no", "yes", "no"),
    levels = c("yes", "no")
  ))
  predictor <- function(model, newdata) plogis(model$coefficient * newdata$x)
  models <- list(score = list(coefficient = 1), user_reference = list(coefficient = 0))
  result <- evaluate_models(models, d, "outcome",
    task = "binary", positive = "yes",
    predict_functions = list(score = predictor, user_reference = predictor), reference = "user_reference"
  )
  expected <- plogis(d$x)
  expect_equal(predict(result, d), expected)
  expect_identical(result$prediction_schema$positive, "yes")
  expect_identical(AutoXplainR:::result_reference_id(result), "user_reference")
  expect_identical(names(result$models), names(models))
  expect_equal(as.character(predict(result, d, type = "class")), ifelse(d$x >= 0, "yes", "no"))
  expect_equal(result$evaluation$metrics$score[["brier_score"]], mean((expected - as.numeric(d$outcome == "yes"))^2))
  expect_equal(result$evaluation$predictions$baseline_probability, rep(.5, nrow(d)))
  expect_equal(predict(result, d, model = "user_reference"), rep(.5, nrow(d)))
  expect_equal(length(predict(result, d[FALSE, ])), 0)
  expect_identical(levels(predict(result, d[FALSE, ], type = "class")), c("yes", "no"))
  result$explanations <- AutoXplainR:::prepare_model_report_data(result, top_features = 1, n_repeats = 2)
  copy <- unserialize(serialize(result, NULL))
  expect_equal(predict(copy, d), expected)
  expect_silent(AutoXplainR:::validate_attached_audit(result$explanations$audit, as_explainers(copy)))
  path <- tempfile(fileext = ".html")
  expect_no_error(render_model_report(copy, path))
  html <- paste(readLines(path), collapse = "\n")
  expect_match(html, "Decision cutoff for yes", fixed = TRUE)
  expect_identical(tuning_evidence(copy)$status, "not_run")
  copy$models$score$coefficient <- 2
  expect_equal(predict(copy, d), plogis(2 * d$x))
  expect_error(render_model_report(copy, tempfile(fileext = ".html")), "Stored evaluation evidence")
})

test_that("custom multiclass probabilities preserve labels, row shape and report evidence after RDS", {
  probability <- rbind(c(.7, .2, .1), c(.1, .8, .1), c(.1, .2, .7), c(.3, .3, .4), c(.6, .2, .2), c(.1, .8, .1))
  colnames(probability) <- c("cat", "dog", "bird")
  data <- data.frame(row = seq_len(6), outcome = factor(c("cat", "dog", "bird", "cat", "dog", "bird"),
    levels = c("cat", "dog", "bird")
  ))
  predictor <- function(model, newdata) model[newdata$row, , drop = FALSE]
  result <- evaluate_models(list(probabilities = probability[, c(3, 1, 2)]), data, "outcome",
    task = "multiclass",
    predict_functions = list(probabilities = predictor)
  )
  expect_equal(predict(result, data), probability)
  expect_equal(dim(predict(result, data[1, ])), c(1L, 3L))
  expect_equal(dim(predict(result, data[FALSE, ])), c(0L, 3L))
  expect_equal(as.character(predict(result, data, type = "class")), c("cat", "dog", "bird", "bird", "cat", "dog"))
  expected_loss <- -mean(log(c(.7, .8, .7, .3, .2, .1)))
  expect_equal(result$evaluation$metrics$probabilities[["log_loss"]], expected_loss)
  result$explanations <- AutoXplainR:::prepare_model_report_data(result, top_features = 1, n_repeats = 2)
  copy <- unserialize(serialize(result, NULL))
  expect_identical(predict(copy, data), predict(result, data))
  expect_identical(names(copy$explanations$effects_by_class), c("cat", "dog", "bird"))
  expect_no_error(render_model_report(copy, tempfile(fileext = ".html"), report_data = "rows", uncertainty = FALSE))
})

test_that("binary disagreement labels follow the explicit event including cutoff ties", {
  data <- data.frame(row = 1:3, outcome = factor(c("yes", "yes", "no"), levels = c("yes", "no")))
  adapter <- function(model, newdata) model[newdata$row]
  for (class_levels in list(c("yes", "no"), c("no", "yes"))) {
    data$outcome <- factor(data$outcome, levels = class_levels)
    result <- evaluate_models(list(first = c(.5, .8, .2), second = c(.5, .7, .3)), data, "outcome",
      positive = "yes", predict_functions = list(first = adapter, second = adapter)
    )
    expected <- as.character(predict(result, data, type = "class"))
    expect_identical(expected, c("yes", "yes", "no"))
    expect_identical(prediction_ambiguity(result)$rows$predicted_classes, expected)
  }
})

test_that("references and training context are explicit rather than inferred from model IDs", {
  d <- data.frame(x = 1:6, y = c(2, 3, 5, 7, 9, 10))
  model <- lm(y ~ x, d)
  result <- evaluate_models(list(simple_baseline = model), d, "y")
  expect_null(AutoXplainR:::result_reference_id(result))
  expect_identical(result$leaderboard$role, "primary")
  supplied <- evaluate_models(list(fit = model), d, "y", training_data = d, evaluation_role = "training")
  expect_identical(supplied$training_data, d)
  expect_true(supplied$training_available)
  expect_match(supplied$provenance$training_status, "not verified", fixed = TRUE)
  expect_identical(supplied$provenance$evaluation_role, "training")
  rows <- supplied$data_context$row_map
  expect_true(all(rows$source[rows$partition == "training"] == "training_data"))
  expect_true(all(rows$source[rows$partition == "evaluation"] == "data"))
  expect_false(anyDuplicated(rows$row_key) > 0L)
  expect_error(evaluate_models(list(a = model), d, "y", reference = "a"), "differ")
  expect_error(evaluate_models(list(a = model), d, "y", primary = "missing"), "one supplied model")
  expect_error(evaluate_models(list(model), d, "y"), "unique IDs")
  expect_error(evaluate_models(list(a = model), d, "y", predict_functions = list(unknown = identity)), "keyed")
  expect_error(evaluate_models(list(a = model), d, "y", features = "y"), "excluding")
  expect_error(evaluate_models(list(a = model), d, "y", labels = "unnamed"), "every model")
})

test_that("supplied probability contract cannot silently become hard labels", {
  d <- data.frame(x = 1:6, y = factor(rep(c("a", "b"), 3)))
  hard <- function(model, newdata) rep("a", nrow(newdata))
  expect_error(evaluate_models(list(model = list()), d, "y", predict_functions = list(model = hard)), "probabilities")
  result <- evaluate_models(list(model = list()), d, "y",
    predict_functions = list(model = function(model, newdata) rep(.3, nrow(newdata)))
  )
  result$prediction_contracts$model$predict_function <- hard
  expect_error(predict(result, d), "no longer returns")
  expect_error(as_explainers(result), "Stored evaluation evidence")
  expect_error(predict(result, d["y"]), "recorded model features")
})

test_that("two supplied candidates and a supplied reference have distinct comparison scope", {
  d <- data.frame(x = 1:12, y = (1:12)^2)
  linear <- lm(y ~ x, d)
  quadratic <- lm(y ~ x + I(x^2), d)
  result <- evaluate_models(list(first = linear, second = quadratic), d, "y")
  prepared <- AutoXplainR:::prepare_report_diagnostics(result)
  for (name in c("resources", "model_behavior", "prediction_disagreement")) {
    expect_identical(prepared$explanations$report_diagnostics[[name]]$status, "computed")
  }
  result <- evaluate_models(list(first = linear, second = quadratic), d, "y", reference = "second")
  prepared <- AutoXplainR:::prepare_report_diagnostics(result)
  expect_identical(prepared$explanations$report_diagnostics$resources$status, "computed")
  expect_identical(prepared$explanations$report_diagnostics$model_behavior$status, "not_applicable")
  expect_identical(prepared$explanations$report_diagnostics$prediction_disagreement$status, "not_applicable")
  result <- evaluate_models(list(simple_baseline = linear, second = quadratic, reference = linear),
    d, "y", reference = "reference"
  )
  expect_setequal(prediction_ambiguity(result)$model_ids, c("simple_baseline", "second"))
})
