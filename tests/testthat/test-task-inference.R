test_that("automatic binary evaluation preserves declared classes missing from observations", {
  training <- data.frame(x = seq_len(20), y = factor(rep(c("no", "yes"), 10)))
  fit <- glm(y ~ 1, training, family = binomial())
  evaluation <- training[training$y == "no", ]
  explainer <- explain_model(fit, evaluation, "y")
  expect_identical(explainer$task, "binary")
  expect_identical(explainer$class_levels, c("no", "yes"))
  expect_identical(explainer$positive, "yes")
  expect_equal(predict(explainer, explainer$data), rep(.5, nrow(evaluation)))
  importance <- calculate_permutation_importance(explainer, max_rows = 2, n_repeats = 2)
  expect_equal(attr(importance, "full_baseline_score"), log(2))
  expect_identical(attr(importance, "sampling")$absent_evaluation_classes, "yes")
  expect_error(audit_explanations(explainer, metric = "auc", n_repeats = 2), "both.*classes")
})

test_that("automatic multiclass evaluation retains absent declared probability columns", {
  for (observed in list(rep("a", 8), rep(c("a", "b"), 4))) {
    evaluation <- data.frame(x = seq_len(8), y = factor(observed, levels = c("a", "b", "c")))
    explainer <- explain_model(list(), evaluation, "y", predict_function = function(newdata) {
      matrix(rep(c(.6, .3, .1), each = nrow(newdata)), ncol = 3, dimnames = list(NULL, c("a", "b", "c")))
    })
    expect_identical(explainer$task, "multiclass")
    expect_identical(explainer$class_levels, c("a", "b", "c"))
    expect_identical(colnames(predict(explainer, explainer$data)), c("a", "b", "c"))
    importance <- calculate_permutation_importance(explainer, n_repeats = 2)
    expected <- -mean(log(c(a = .6, b = .3, c = .1)[observed]))
    expect_equal(attr(importance, "full_baseline_score"), expected)
  }
})

test_that("training inference and explicit task choices keep their previous meaning", {
  training <- data.frame(x = seq_len(20), y = factor(rep(c("no", "yes"), 10), c("no", "unused", "yes")))
  expect_identical(AutoXplainR:::detect_task(training$y), "binary")
  result <- autoxplain(training, "y", model_set = "quick", explain = FALSE)
  expect_identical(result$task, "binary")
  expect_identical(levels(result$training_data$y), c("no", "yes"))
  training$y <- factor(rep("no", 20), levels = c("no", "yes"))
  expect_error(autoxplain(training, "y", model_set = "quick", explain = FALSE), "at least two observed values")
  numeric <- data.frame(x = seq_len(6), y = rep(c(0, 1), 3))
  expect_identical(AutoXplainR:::detect_task(numeric$y), "binary")
  explicit <- explain_model(list(), numeric, "y", task = "regression", predict_function = function(newdata) {
    rep(.5, nrow(newdata))
  })
  expect_identical(explicit$task, "regression")
  expect_error(explain_model(list(), training, "y", task = "multiclass", predict_function = function(newdata) {
    rep(.5, nrow(newdata))
  }), "at least three declared outcome levels")
})
