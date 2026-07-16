test_that("subgroup performance exposes held-out regression gaps", {
  set.seed(771)
  n <- 240
  data <- data.frame(
    x = rnorm(n),
    region = factor(rep(c("north", "south", "west"), each = n / 3))
  )
  noise <- c(north = 0.2, south = 0.7, west = 1.5)
  data$y <- 2 * data$x + rnorm(n, sd = noise[as.character(data$region)])
  result <- autoxplain(data, "y", test_fraction = 0.5, seed = 42)
  diagnostic <- subgroup_performance(result, by = "region", min_rows = 20)

  expect_s3_class(diagnostic, "autoxplain_subgroups")
  expect_equal(diagnostic$model_id, "main_model")
  expect_equal(diagnostic$primary_metric, "rmse")
  expect_equal(sum(diagnostic$performance$rows), nrow(result$test_data))
  expect_setequal(diagnostic$performance$group, levels(data$region))
  expect_true(all(c("rmse", "mae", "r_squared", "gap_from_overall") %in%
                    names(diagnostic$performance)))
  expect_equal(
    diagnostic$largest_observed_gap,
    diff(range(diagnostic$performance$rmse))
  )
  expect_output(print(diagnostic), "not fairness certification")
})

test_that("subgroup performance supports classification probability metrics", {
  set.seed(91)
  n <- 300
  data <- data.frame(
    x = rnorm(n),
    channel = factor(sample(c("app", "phone"), n, replace = TRUE))
  )
  probability <- stats::plogis(data$x + ifelse(data$channel == "app", 0.5, -0.3))
  data$converted <- factor(ifelse(runif(n) < probability, "yes", "no"))
  result <- autoxplain(data, "converted", test_fraction = 0.4, seed = 12)
  result$evaluation$primary_metric <- "brier_score"
  diagnostic <- subgroup_performance(result, "channel")

  expect_equal(diagnostic$task, "binary")
  expect_identical(diagnostic$primary_metric, "brier_score")
  expect_identical(diagnostic$secondary_metric, "accuracy")
  expect_false(identical(diagnostic$primary_metric, diagnostic$secondary_metric))
  expect_true(all(c("log_loss", "brier_score", "accuracy", "roc_auc") %in%
                    names(diagnostic$performance)))
  expect_true(all(is.finite(diagnostic$performance$log_loss)))
  expect_equal(
    diagnostic$performance$gap_from_overall,
    diagnostic$performance$brier_score - diagnostic$overall_metrics[["brier_score"]]
  )
  expect_match(diagnostic$scope_note, "does not certify fairness")
})

test_that("subgroup performance honors MAE without duplicating its supporting metric", {
  set.seed(92)
  n <- 180
  data <- data.frame(
    x = rnorm(n),
    segment = factor(rep(c("one", "two", "three"), each = n / 3))
  )
  data$y <- 1.5 * data$x + rnorm(n, sd = rep(c(0.2, 0.5, 1), each = n / 3))
  result <- autoxplain(data, "y", test_fraction = 0.5, seed = 92)
  result$evaluation$primary_metric <- "mae"
  diagnostic <- subgroup_performance(result, "segment", min_rows = 10)

  expect_identical(diagnostic$primary_metric, "mae")
  expect_identical(diagnostic$secondary_metric, "rmse")
  expect_false(identical(diagnostic$primary_metric, diagnostic$secondary_metric))
  expect_equal(
    diagnostic$performance$gap_from_overall,
    diagnostic$performance$mae - diagnostic$overall_metrics[["mae"]]
  )
  expect_identical(
    names(diagnostic$performance)[4:5],
    c("mae", "rmse")
  )
})

test_that("Brier metric definitions state task-specific formulas and ranges", {
  binary <- AutoXplainR:::metric_definitions("binary")[["brier_score"]]
  multiclass <- AutoXplainR:::metric_definitions("multiclass")[["brier_score"]]

  expect_match(binary, "positive-class probability")
  expect_match(binary, "0 \\(best\\).*1 \\(worst\\)")
  expect_match(multiclass, "sum of squared errors across all class probabilities")
  expect_match(multiclass, "0 \\(best\\).*2 \\(worst\\)")
  expect_match(multiclass, "do not compare it directly with binary Brier scores")
})

test_that("subgroup performance validates the requested comparison", {
  training <- data.frame(
    x = seq_len(50),
    y = seq_len(50) + rnorm(50)
  )
  evaluation <- transform(training, x = x + 50, y = y + 50)
  result <- autoxplain(training, "y", test_data = evaluation, seed = 8)
  result$test_data$one_group <- "same"
  result$test_data$two_groups <- rep(c("a", "b"), 25)
  result$test_data$many_groups <- rep(sprintf("group-%02d", seq_len(25)), each = 2)

  expect_error(subgroup_performance(result, "missing"), "not available")
  expect_error(subgroup_performance(result, "y"), "not the outcome")
  expect_error(subgroup_performance(result, "one_group"), "at least two")
  expect_error(subgroup_performance(result, "many_groups"), "more than 20")
  expect_error(subgroup_performance(result, "two_groups", model = c(1, 2)), "exactly one")
  expect_error(subgroup_performance(result, "x", min_rows = 0), "min_rows")
})

test_that("guided report includes subgroup context only when requested", {
  set.seed(7)
  data <- data.frame(
    x = rnorm(180),
    cohort = factor(rep(c("first", "second"), each = 90))
  )
  data$y <- data$x + rnorm(180, sd = ifelse(data$cohort == "first", 0.3, 1))
  result <- autoxplain(data, "y", test_fraction = 0.5, seed = 14)
  plain_path <- tempfile(fileext = ".html")
  group_path <- tempfile(fileext = ".html")
  render_model_report(result, plain_path, top_features = 1, n_repeats = 2)
  render_model_report(
    result, group_path, subgroup = "cohort", top_features = 1, n_repeats = 2
  )
  plain_html <- paste(readLines(plain_path, warn = FALSE), collapse = "\n")
  group_html <- paste(readLines(group_path, warn = FALSE), collapse = "\n")

  expect_false(grepl("Did performance vary across groups?", plain_html, fixed = TRUE))
  expect_match(group_html, "Did performance vary across groups?", fixed = TRUE)
  expect_match(group_html, "This is not fairness certification", fixed = TRUE)
  expect_match(group_html, "#subgroups", fixed = TRUE)
})
