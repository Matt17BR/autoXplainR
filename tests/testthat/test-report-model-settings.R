test_that("expanded model settings preserve seeds and nested numeric values exactly", {
  withr::local_options(OutDec = ",")
  values <- list(seed = 2147483647L, effective = list(alpha = .10000000000000002))
  html <- model_spec_table(values, "Recorded settings")
  expect_match(html, "2147483647", fixed = TRUE)
  expect_match(html, "alpha = 0.10000000000000002", fixed = TRUE)
  expect_identical(model_spec_exact_value(.8), "0.8")
  expect_identical(model_spec_exact_value(c(first = 218L, second = 395L)), "first = 218, second = 395")
  expect_identical(getOption("OutDec"), ",")
})

test_that("forest summaries distinguish requested defaults from the actual native split rule", {
  skip_if_package_unavailable("ranger")
  for (task in c("regression", "binary", "multiclass")) {
    data <- data.frame(x = seq_len(120L), z = sin(seq_len(120L)))
    data$y <- switch(task,
      regression = data$z,
      binary = factor(rep(c("no", "yes"), 60L)),
      multiclass = factor(rep(c("c", "a", "b"), 40L), levels = c("c", "a", "b"))
    )
    model <- fit_forest_learner(data, "y", task, list(
      num.trees = 16L, mtry = 2L, min.node.size = 5L, sample.fraction = .8, splitrule = "default"
    ), seed = 817L)
    result <- list(models = list(forest = model), task = task)
    before <- serialize(model, NULL)
    spec <- model_specification(result, "forest")
    expected <- if (task == "regression") "variance" else "gini"
    expect_identical(spec$parameters$splitrule, "default")
    expect_identical(spec$learned$`Split rule`, expected)
    expect_match(spec$summary, paste0("split rule = ", expected, " (default)"), fixed = TRUE)
    expect_identical(serialize(result$models$forest, NULL), before)
  }
})

test_that("retained AUC alternatives describe the maximizing selection direction", {
  data <- with_preserved_seed(816L, {
    x <- rnorm(160L)
    data.frame(x = x, y = factor(ifelse(runif(160L) < plogis(x), "yes", "no")))
  })
  result <- autoxplain(
    data, "y", learners = c("linear", "tree"), max_models = 2L, nfolds = 2L, explain = FALSE,
    tuning_control = tuning_control(metric = "auc"), seed = 817L
  )
  alternatives <- result$tuning$candidates
  alternatives <- alternatives[alternatives$refit_role == "alternative", , drop = FALSE]
  expect_gt(nrow(alternatives), 0L)
  for (id in alternatives$retained_model_id) {
    html <- explorer_model_spec_details(result, id)
    expect_match(html, "Highest training-CV score within this family", fixed = TRUE)
    expect_false(grepl("Lowest training-CV loss within this family", html, fixed = TRUE))
    seed <- attr(result$models[[id]], "autoxplain_tuning_fit")$fit_seed
    expect_match(html, as.character(seed), fixed = TRUE)
  }
})
