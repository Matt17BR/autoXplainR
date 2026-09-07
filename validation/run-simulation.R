# A small, reproducible coverage diagnostic, not a universal coverage claim.
if (dir.exists("R")) pkgload::load_all(quiet = TRUE) else library(AutoXplainR)
dir.create("validation/results", recursive = TRUE, showWarnings = FALSE)
repetitions <- 100L
results <- lapply(c(20L, 100L), function(n_test) {
  rows <- lapply(seq_len(repetitions), function(index) {
    set.seed(1000L * n_test + index)
    train <- data.frame(x = rnorm(200))
    train$y <- 2 * train$x + rnorm(200)
    test <- data.frame(x = rnorm(n_test))
    test$y <- 2 * test$x + rnorm(n_test)
    result <- autoxplain(train, "y", test_data = test, evaluation_role = "test", explain = FALSE)
    # For new independent standard-normal x and noise, the conditional MSE of
    # this particular fitted line is 1 + intercept^2 + (slope - 2)^2.
    coefficients <- stats::coef(result$models$main_model)
    truth <- sqrt(1 + coefficients[[1L]]^2 + (coefficients[[2L]] - 2)^2)
    interval <- performance_uncertainty(result, n_boot = 300L, seed = index)$estimates[1L, ]
    data.frame(n_test = n_test, replicate = index, true_conditional_rmse = truth,
               lower = interval$lower, upper = interval$upper,
               covered = interval$lower <= truth && truth <= interval$upper)
  })
  do.call(rbind, rows)
})
results <- do.call(rbind, results)
utils::write.csv(results, "validation/results/bootstrap-coverage-draws.csv", row.names = FALSE)
summary <- do.call(rbind, lapply(split(results, results$n_test), function(x) {
  coverage <- mean(x$covered)
  data.frame(n_test = x$n_test[1], repetitions = nrow(x), nominal = 0.95,
             empirical_coverage = coverage, monte_carlo_se = sqrt(coverage * (1 - coverage) / nrow(x)),
             mean_width = mean(x$upper - x$lower))
}))
utils::write.csv(summary, "validation/results/bootstrap-coverage-summary.csv", row.names = FALSE)
print(summary)
