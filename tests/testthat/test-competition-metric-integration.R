competition_pair_auc <- function(truth, probability) {
  if (!any(truth) || all(truth)) return(NA_real_)
  pairs <- outer(probability[truth], probability[!truth], `-`)
  (sum(pairs > 0) + sum(pairs == 0) / 2) / length(pairs)
}

competition_auc_result <- function() {
  training <- data.frame(x = rep(1:12, 4), y = factor(rep(c("no", "yes"), 24)))
  evaluation <- data.frame(
    x = c(1.1, 2.1, 4.1, 6.1, 8.1, 11.1),
    y = factor(c("no", "yes", "no", "yes", "no", "yes"))
  )
  autoxplain(training, "y", test_data = evaluation, learners = "tree", nfolds = 2,
    max_models = 1, explain = FALSE, tuning_control = tuning_control(metric = "auc", search = "grid")
  )
}

test_that("competition metric resolution retains task and domain contracts", {
  expect_identical(AutoXplainR:::resolve_metric("auto", "binary", "roc_auc"), "auc")
  expect_identical(AutoXplainR:::resolve_metric("roc_auc", "binary"), "auc")
  expect_identical(AutoXplainR:::resolve_metric("auto", "regression", "rmsle"), "rmsle")
  expect_error(AutoXplainR:::resolve_metric("rmsle", "binary"), "Classification supports")
  expect_error(AutoXplainR:::resolve_metric("roc_auc", "multiclass"), "only available for binary")
  expect_equal(AutoXplainR:::metric_score(c(0, 1, 3), c(1, 3, 7), "rmsle", list()), log(2))
  expect_error(AutoXplainR:::metric_score(c(0, 1), c(0, -.1), "rmsle", list()),
    class = "autoxplain_rmsle_prediction_domain"
  )
})

test_that("AUC bootstrap tie groups agree with repeated positive-negative pairs", {
  truth <- c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE)
  probability <- c(.4, .4, .1, .8, .1, .9)
  plan <- AutoXplainR:::auc_bootstrap_plan(truth, probability)
  counts <- c(3L, 2L, 0L, 1L, 2L, 3L)
  rows <- rep(seq_along(truth), counts)
  expect_equal(AutoXplainR:::auc_bootstrap_score(plan, counts),
    competition_pair_auc(truth[rows], probability[rows])
  )
  expect_equal(AutoXplainR:::auc_bootstrap_score(plan, rep(1L, 6)), plan$estimate)
  expect_true(is.na(AutoXplainR:::auc_bootstrap_score(plan, as.integer(truth))))
  # More than 2^31 positive-negative pairs must use floating-point arithmetic.
  large_plan <- AutoXplainR:::auc_bootstrap_plan(c(FALSE, TRUE), c(.5, .5))
  expect_equal(AutoXplainR:::auc_bootstrap_score(large_plan, c(50000L, 50000L)), .5)
  large_truth <- rep(c(FALSE, TRUE), each = 50000)
  expect_equal(AutoXplainR:::selection_binary_auc(large_truth, as.numeric(large_truth)), 1)
  expect_equal(AutoXplainR:::binary_auc(large_truth, rep(.5, 100000)), .5)
})

test_that("paired AUC intervals match a brute-force observation bootstrap", {
  result <- competition_auc_result()
  predictions <- result$evaluation$predictions
  truth <- as.character(result$test_data$y) == "yes"
  n <- nrow(predictions)
  oracle <- withr::with_seed(12, t(replicate(100, {
    rows <- sample.int(n, n, replace = TRUE)
    first <- competition_pair_auc(truth[rows], predictions$primary_probability[rows])
    second <- competition_pair_auc(truth[rows], predictions$baseline_probability[rows])
    c(primary = first, baseline = second, difference = first - second)
  })))
  valid <- which(is.finite(oracle[, "primary"]))
  set.seed(821)
  before <- .Random.seed
  output <- performance_uncertainty(result, n_boot = 100, seed = 12)
  expect_identical(.Random.seed, before)
  expect_equal(as.matrix(output$draws), oracle[valid, , drop = FALSE])
  expect_identical(output$bootstrap$retained_draw_ids, valid)
  expect_identical(output$bootstrap$discarded_draw_ids, setdiff(seq_len(100), valid))
  expect_gt(output$bootstrap$discarded, 0)
  expect_equal(output$estimates$lower[3], unname(quantile(oracle[valid, "difference"], .025)))
  expect_equal(output$estimates$upper[3], unname(quantile(oracle[valid, "difference"], .975)))
  expect_true(any(grepl("conditional on sampling both classes", output$notes, fixed = TRUE)))
  html <- AutoXplainR:::render_performance_uncertainty(output, result = result)
  expect_match(html, "Positive differences favor", fixed = TRUE)
  expect_match(html, paste("from", length(valid), "bootstrap draws"), fixed = TRUE)
  expect_match(html, "Discarded", fixed = TRUE)
})

test_that("AUC bootstrap keeps unequal evaluation groups together", {
  sizes <- rep(c(4L, 6L, 8L), 10)
  data <- data.frame(site = rep(seq_len(30), sizes), x = sin(seq_len(sum(sizes))))
  data$y <- factor(rep(c("no", "yes"), nrow(data) / 2))
  result <- autoxplain(data, "y", validation = validation_split(group = "site"),
    learners = "tree", max_models = 1, nfolds = 2, explain = FALSE,
    tuning_control = tuning_control(metric = "auc", search = "grid")
  )
  predictions <- result$evaluation$predictions
  truth <- as.character(result$test_data$y) == "yes"
  groups <- result$validation$evaluation_groups[
    match(rownames(result$test_data), result$validation$evaluation_row_names)
  ]
  members <- lapply(unique(groups), function(group) which(groups == group))
  oracle <- withr::with_seed(9, t(replicate(30, {
    selected <- sample.int(length(members), length(members), replace = TRUE)
    rows <- unlist(members[selected], use.names = FALSE)
    first <- competition_pair_auc(truth[rows], predictions$primary_probability[rows])
    second <- competition_pair_auc(truth[rows], predictions$baseline_probability[rows])
    c(primary = first, baseline = second, difference = first - second)
  })))
  output <- performance_uncertainty(result, n_boot = 30, seed = 9)
  expect_identical(output$unit, "group")
  expect_identical(output$units, length(members))
  expect_equal(as.matrix(output$draws), oracle)
  expect_identical(output$bootstrap$discarded, 0L)
})

test_that("undefined AUC resamples are recorded and insufficient support is rejected", {
  draws <- cbind(primary = rep(.7, 25), baseline = rep(.5, 25), difference = rep(.2, 25))
  draws[5, ] <- NA_real_
  kept <- AutoXplainR:::auc_bootstrap_complete_draws(draws, 25L)
  expect_identical(kept$record$discarded_draw_ids, 5L)
  expect_equal(nrow(kept$draws), 24L)
  draws[1:6, ] <- NA_real_
  expect_error(AutoXplainR:::auc_bootstrap_complete_draws(draws, 25L), "at least 20 are required")
  many <- draws[rep(seq_len(25), 4), ]
  expect_error(AutoXplainR:::auc_bootstrap_complete_draws(many, 100L), "at least 80 are required")
})

test_that("RMSLE evaluation intervals match independent log-error resampling", {
  training <- data.frame(x = seq_len(60), y = 1 + seq_len(60)^2)
  evaluation <- data.frame(x = seq(1.5, 58.5, length.out = 12))
  evaluation$y <- 1 + evaluation$x^2
  result <- autoxplain(training, "y", test_data = evaluation, learners = "tree", nfolds = 2,
    max_models = 1, explain = FALSE, tuning_control = tuning_control(metric = "rmsle", search = "grid")
  )
  predictions <- result$evaluation$predictions
  oracle <- withr::with_seed(13, replicate(30, {
    rows <- sample.int(nrow(predictions), nrow(predictions), replace = TRUE)
    observed <- log1p(predictions$observed[rows])
    first <- sqrt(mean((observed - log1p(predictions$primary_prediction[rows]))^2))
    second <- sqrt(mean((observed - log1p(predictions$baseline_prediction[rows]))^2))
    first - second
  }))
  output <- performance_uncertainty(result, n_boot = 30, seed = 13)
  expect_equal(output$draws$difference, oracle)
  expect_identical(output$metric, "rmsle")
  expect_equal(output$estimates$estimate[1], result$evaluation$metrics$main_model[["rmsle"]])
  expect_match(AutoXplainR:::render_performance_uncertainty(output, result = result),
    "Negative differences favor", fixed = TRUE
  )
})

test_that("negative shuffled predictions have explicit unavailable RMSLE importance", {
  data <- data.frame(x = 1:8, z = 1:8, spare = 8:1, y = rep(1, 8))
  predictor <- function(model, newdata) model$intercept + newdata$x - newdata$z
  explainer <- explain_model(list(intercept = 1), data, "y", task = "regression",
    predict_function = predictor, metadata = list(primary_metric = "rmsle")
  )
  importance <- calculate_permutation_importance(explainer, features = c("x", "spare"), n_repeats = 5, seed = 17)
  expect_true(is.na(importance$importance[importance$feature == "x"]))
  expect_equal(importance$importance[importance$feature == "spare"], 0)
  expect_match(attr(importance, "unavailable_by_feature")[["x"]], "not clipped", fixed = TRUE)
  expect_identical(attr(importance, "permutation_failures")$feature, "x")
  expect_equal(attr(importance, "permutations_completed")[["spare"]], 5)
  audit <- audit_explanations(explainer, features = c("x", "spare"), n_repeats = 5, seed = 17)
  failed <- audit$importance[audit$importance$feature == "x", ]
  expect_identical(failed$shuffle_status, "importance_unavailable")
  expect_match(failed$unavailable_reason, "negative predictions", fixed = TRUE)
  expect_identical(audit$config$metric, "rmsle")
})

test_that("unavailable original RMSLE predictions do not invalidate other explanation evidence", {
  data <- data.frame(x = seq_len(8), y = seq_len(8))
  predictor <- function(model, newdata) model$offset + newdata$x
  bad <- explain_model(list(offset = -100), data, "y", task = "regression", label = "negative",
    predict_function = predictor, metadata = list(primary_metric = "rmsle")
  )
  good <- explain_model(list(offset = 0), data, "y", task = "regression", label = "positive",
    predict_function = predictor, metadata = list(primary_metric = "rmsle")
  )
  audit <- audit_explanations(list(bad, good), features = "x", n_repeats = 3)
  expect_identical(audit$performance$near_optimal, c(FALSE, TRUE))
  expect_true(is.na(audit$performance$relative_gap[1]))
  expect_equal(audit$performance$score[2], 0)
  only_bad <- audit_explanations(bad, features = "x", n_repeats = 3)
  expect_identical(only_bad$performance$near_optimal, FALSE)
  expect_identical(dim(only_bad$explanation_agreement$rank_correlation), c(0L, 0L))
  expect_error(AutoXplainR:::metric_score(data$y, rep(Inf, 8), "rmsle", list()), "finite numeric")
})

test_that("AUC reports compute automatic intervals and explain maximized scores", {
  result <- competition_auc_result()
  path <- tempfile(fileext = ".html")
  on.exit(unlink(path), add = TRUE)
  expect_no_error(render_model_report(result, path, top_features = 1L, n_repeats = 2L))
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "ROC AUC", fixed = TRUE)
  expect_match(html, "Positive differences favor", fixed = TRUE)
  expect_match(html, "conditional on sampling both classes", fixed = TRUE)
})

test_that("RMSLE reports retain invalid-shuffle explanations without clipping", {
  data <- withr::with_seed(371, {
    x <- seq_len(100)
    z <- x + runif(100, -.5, .5)
    data.frame(x = x, z = z, y = 5 + x - z)
  })
  result <- autoxplain(data[1:80, ], "y", test_data = data[81:100, ], learners = "linear",
    max_models = 1, nfolds = 2, explain = FALSE,
    tuning_control = tuning_control(metric = "rmsle", search = "grid")
  )
  path <- tempfile(fileext = ".html")
  on.exit(unlink(path), add = TRUE)
  expect_no_error(render_model_report(result, path, top_features = 2L, n_repeats = 3L))
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "RMSLE", fixed = TRUE)
  expect_match(html, "negative predictions are not clipped", fixed = TRUE)
  expect_match(html, "Some permutation importance was unavailable", fixed = TRUE)
})
