test_that("paired bootstrap agrees with independently resampled stored errors", {
  result <- autoxplain(mtcars, "mpg", explain = FALSE)
  set.seed(91)
  rng <- .Random.seed
  uncertainty <- performance_uncertainty(result, n_boot = 100, seed = 12)
  expect_identical(.Random.seed, rng)
  predictions <- result$evaluation$predictions
  draws <- withr::with_seed(12, replicate(100, {
    rows <- sample.int(nrow(predictions), nrow(predictions), replace = TRUE)
    x <- predictions[rows, ]
    sqrt(mean((x$observed - x$primary_prediction)^2)) -
      sqrt(mean((x$observed - x$baseline_prediction)^2))
  }))
  expect_equal(uncertainty$draws$difference, draws)
  expect_equal(uncertainty$estimates$lower[3], unname(quantile(draws, 0.025)))
  expect_equal(uncertainty$estimates$upper[3], unname(quantile(draws, 0.975)))
  expect_equal(uncertainty$estimates$estimate[1], result$evaluation$metrics$main_model[["rmse"]])
  expect_identical(performance_uncertainty(result, 100, seed = 12), uncertainty)
})

test_that("bootstrap preserves whole groups and rejects temporal IID inference", {
  data <- withr::with_seed(3, data.frame(site = rep(1:30, each = 4), x = rnorm(120), y = rnorm(120)))
  result <- autoxplain(data, "y", validation = validation_split(group = "site"), explain = FALSE)
  output <- performance_uncertainty(result, n_boot = 30, seed = 4)
  expect_identical(output$unit, "group")
  expect_equal(output$units, 6L)
  predictions <- result$evaluation$predictions
  groups <- split(seq_len(nrow(predictions)), match(result$validation$evaluation_groups,
                                                    unique(result$validation$evaluation_groups)))
  oracle <- withr::with_seed(4, replicate(30, {
    x <- predictions[unlist(groups[sample.int(6, 6, replace = TRUE)]), ]
    sqrt(mean((x$observed - x$primary_prediction)^2))
  }))
  expect_equal(output$draws$primary, oracle)
  time <- autoxplain(data, "y", validation = validation_split(time = "site"), explain = FALSE)
  expect_error(performance_uncertainty(time, 20), "dependence-aware")
  expect_error(performance_uncertainty(result, 2), "n_boot")
  expect_error(performance_uncertainty(result, confidence = 1), "confidence")
})

test_that("classification loss matches independently calculated log loss", {
  result <- autoxplain(iris, "Species", explain = FALSE)
  uncertainty <- performance_uncertainty(result, 30)
  probability <- predict(result, result$test_data)
  truth <- match(result$test_data$Species, colnames(probability))
  expected <- -mean(log(pmax(probability[cbind(seq_along(truth), truth)], 1e-15)))
  expect_equal(uncertainty$estimates$estimate[1], expected)
})
