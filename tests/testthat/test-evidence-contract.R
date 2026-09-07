test_that("binary event reversal complements probabilities without changing loss", {
  set.seed(83)
  d <- data.frame(x = rnorm(100))
  d$y <- factor(ifelse(runif(100) < plogis(d$x), "alpha", "zeta"),
                levels = c("zeta", "alpha"))
  fit <- glm(y ~ x, data = d, family = binomial())
  a <- explain_model(fit, d, "y", positive = "alpha")
  b <- explain_model(fit, d, "y", positive = "zeta")
  expect_equal(predict(a, d) + predict(b, d), rep(1, nrow(d)))
  expect_equal(AutoXplainR:::metric_score(d$y, predict(a, d), "logloss", a),
               AutoXplainR:::metric_score(d$y, predict(b, d), "logloss", b))
  ea <- explain_effect(a, feature = "x")
  eb <- explain_effect(b, feature = "x")
  expect_equal(ea$accumulated_effect, -eb$accumulated_effect)
  custom <- explain_model(fit, d, "y", positive = "zeta", probability_class = "alpha",
                          predict_function = function(model, newdata, ...) {
                            predict(model, newdata, type = "response")
                          })
  expect_equal(predict(custom, d), predict(b, d))
})

test_that("raw prediction types and hard-label capabilities are respected", {
  d <- data.frame(x = 1:4, y = factor(c("no", "yes", "no", "yes")))
  expect_error(explain_model(NULL, d["x"], 1:4, task = "regression",
                             predict_function = function(newdata) {
                               factor(rep("100", nrow(newdata)), levels = c("10", "100"))
                             }), "must be numeric")
  expect_error(explain_model(NULL, d, "y", predict_function = function(newdata) {
    rep("typo", nrow(newdata))
  }), "declared outcome classes")
  hard <- explain_model(NULL, d, "y", predict_function = function(newdata) {
    rep(c("no", "yes"), length.out = nrow(newdata))
  })
  expect_identical(hard$prediction_type, "class")
  expect_s3_class(predict(hard, d), "factor")
  expect_equal(AutoXplainR:::metric_score(d$y, predict(hard, d), "accuracy", hard), 1)
  expect_error(calculate_permutation_importance(hard, metric = "logloss"), "probabilit")
  expect_error(explain_effect(hard, feature = "x"), "numeric|probabilit")
})

test_that("ellipsis does not change custom adapter arity or mask its errors", {
  d <- data.frame(x = 1:4, y = 2 * (1:4))
  one <- explain_model(NULL, d, "y", predict_function = function(newdata, ...) 2 * newdata$x)
  two <- explain_model(list(beta = 2), d, "y", predict_function = function(model, newdata, ...) {
    model$beta * newdata$x
  })
  expect_equal(predict(one, d), d$y)
  expect_equal(predict(two, d), d$y)
  expect_error(explain_model(NULL, d, "y", predict_function = function(a, b, c) a), "must take")
  expect_error(explain_model(NULL, d, "y", predict_function = function(newdata, ...) {
    stop("adapter's own error")
  }), "adapter's own error", fixed = TRUE)
})

test_that("evidence identities include model state and ordered data and survive RDS", {
  r1 <- autoxplain(mtcars, "mpg", seed = 10, model_set = "comparison", explain = FALSE)
  r2 <- autoxplain(mtcars, "mpg", seed = 20, model_set = "comparison", explain = FALSE)
  ids <- c("main_model", "small_tree")
  e1 <- as_explainers(r1, models = ids)
  e2 <- as_explainers(r2, models = ids)
  expect_false(identical(e1[[1]]$provenance$fingerprint, e2[[1]]$provenance$fingerprint))
  audit <- audit_explanations(e1, n_repeats = 2)
  expect_error(compare_model_behavior(r2, models = ids, explanation_audit = audit),
               "same selected model explainers")
  copy <- unserialize(serialize(r1, NULL))
  expect_silent(AutoXplainR:::validate_attached_audit(audit, as_explainers(copy, models = ids)))
  changed <- copy
  changed$models$main_model$coefficients[[1]] <- changed$models$main_model$coefficients[[1]] + 1
  expect_error(AutoXplainR:::validate_attached_audit(audit, as_explainers(changed, models = ids)),
               "same selected model explainers")
  changed <- copy
  changed$test_data$mpg <- changed$test_data$mpg + 1
  expect_error(AutoXplainR:::validate_attached_audit(audit, as_explainers(changed, models = ids)),
               "same selected model explainers")
})

test_that("paired audits reject reordered observations and opposite events", {
  d <- data.frame(x = 1:30, y = 2 * (1:30))
  fit <- lm(y ~ x, d)
  a <- explain_model(fit, d, "y")
  b <- explain_model(fit, d[30:1, ], "y")
  expect_error(audit_explanations(list(a, b), n_repeats = 2), "same ordered evaluation")
  d$y <- factor(rep(c("no", "yes"), 15))
  fit <- glm(y ~ x, d, family = binomial())
  a <- explain_model(fit, d, "y", positive = "yes")
  b <- explain_model(fit, d, "y", positive = "no")
  expect_error(audit_explanations(list(a, b), n_repeats = 2), "same ordered evaluation")
})


test_that("changing adapter code invalidates evidence even at identical observed predictions", {
  d <- data.frame(x = seq_len(20), y = 2 * seq_len(20))
  explainer <- explain_model(NULL, d, "y", predict_function = function(newdata) 2 * newdata$x)
  audit <- audit_explanations(list(custom = explainer), n_repeats = 2)
  changed <- explainer
  changed$predict_function <- function(newdata) {
    if (all(newdata$x == seq_len(nrow(newdata)))) 2 * newdata$x else 0 * newdata$x
  }
  expect_equal(predict(changed, d), predict(explainer, d))
  expect_error(AutoXplainR:::validate_attached_audit(audit, list(custom = changed)),
               "same selected model explainers")
})

test_that("source metadata and compilation do not invalidate unchanged prediction evidence", {
  predictor <- eval(parse(text = "function(newdata) { 2 * newdata$x }", keep.source = TRUE))
  source_file <- attr(body(predictor), "srcfile")
  d <- data.frame(x = seq_len(20), y = 2 * seq_len(20))
  explainer <- explain_model(NULL, d, "y", label = "source", predict_function = predictor)
  audit <- audit_explanations(explainer, n_repeats = 2)
  fingerprint <- AutoXplainR:::current_explainer_fingerprint(explainer)
  predictions <- predict(explainer, d)

  # R's source retrieval can populate or modify this environment after fitting.
  # Neither its timestamp nor its cached source text changes the parsed function.
  source_file$timestamp <- source_file$timestamp + 1
  source_file$lines <- c("# relocated source file", source_file$lines)
  expect_identical(predict(explainer, d), predictions)
  expect_identical(AutoXplainR:::current_explainer_fingerprint(explainer), fingerprint)
  expect_silent(AutoXplainR:::validate_attached_audit(audit, list(source = explainer)))

  explainer$predict_function <- compiler::cmpfun(explainer$predict_function)
  expect_identical(predict(explainer, d), predictions)
  expect_silent(AutoXplainR:::validate_attached_audit(audit, list(source = explainer)))
  copy <- unserialize(serialize(explainer, NULL))
  expect_silent(AutoXplainR:::validate_attached_audit(audit, list(source = copy)))
})

test_that("formula source references are not statistical model state", {
  source_formula <- eval(parse(text = "mpg ~ wt", keep.source = TRUE))
  fit <- lm(source_formula, data = mtcars)
  source_ref <- attr(parse(text = "mpg ~ wt", keep.source = TRUE), "srcref")[[1L]]
  attr(fit$terms, "srcref") <- source_ref
  explainer <- explain_model(fit, mtcars, "mpg", label = "formula")
  audit <- audit_explanations(explainer, n_repeats = 2)
  source_file <- attr(source_ref, "srcfile")
  source_file$timestamp <- source_file$timestamp + 1
  expect_silent(AutoXplainR:::validate_attached_audit(audit, list(formula = explainer)))
})


test_that("console identifies scoped evidence findings and retained failures", {
  d <- data.frame(x = seq_len(100), proxy = seq_len(100) * 2 + sin(seq_len(100)),
                  y = cos(seq_len(100)))
  result <- autoxplain(model_set = "quick", d, "y")
  output <- paste(capture.output(print(result)), collapse = "\n")
  expect_match(output, "reproducible random holdout", fixed = TRUE)
  expect_match(output, "did not improve on the intercept-only baseline", fixed = TRUE)
  expect_match(output, "finding:", fixed = TRUE)
  expect_match(output, "x|proxy")
  result$explanations$failures <- data.frame(feature = "x", reason = "test adapter unavailable")
  expect_output(print(result), "effect:x", fixed = TRUE)
  fitting_only <- autoxplain(model_set = "quick", d, "y", explain = FALSE)
  expect_false(any(grepl("incomplete:", capture.output(print(fitting_only)), fixed = TRUE)))
})


test_that("native numeric and logical GLM events do not depend on evaluation level order", {
  d <- data.frame(x = seq(-2, 2, length.out = 40), y = rep(c(0, 0, 1, 0, 1), 8))
  for (logical_response in c(FALSE, TRUE)) {
    training <- d
    if (logical_response) training$y <- as.logical(training$y)
    event <- if (logical_response) "TRUE" else "1"
    negative <- if (logical_response) "FALSE" else "0"
    evaluation <- training
    evaluation$y <- factor(evaluation$y, levels = c(event, negative))
    for (keep_frame in c(FALSE, TRUE)) {
      fit <- glm(y ~ x, training, family = binomial(), model = keep_frame)
      explainer <- explain_model(fit, evaluation, "y", positive = event)
      expect_equal(predict(explainer, evaluation), as.numeric(predict(fit, evaluation, type = "response")))
    }
  }
})

test_that("factor GLMs without response levels require an explicit native event", {
  d <- data.frame(x = seq(-2, 2, length.out = 40),
                  y = factor(rep(c("yes", "no", "no", "yes", "no"), 8), levels = c("no", "yes")))
  fit <- glm(y ~ x, d, family = binomial(), model = FALSE)
  evaluation <- d
  evaluation$y <- factor(evaluation$y, levels = c("yes", "no"))
  expect_error(explain_model(fit, evaluation, "y", positive = "yes"), "probability event is unavailable")
  explainer <- explain_model(fit, evaluation, "y", positive = "yes", probability_class = "yes")
  expect_equal(predict(explainer, evaluation), as.numeric(predict(fit, evaluation, type = "response")))
  other <- explain_model(fit, evaluation, "y", positive = "no", probability_class = "yes")
  expect_equal(predict(explainer, evaluation) + predict(other, evaluation), rep(1, nrow(d)))
})
