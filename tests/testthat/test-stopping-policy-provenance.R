stopping_policy_data <- function(n = 160L) {
  x <- seq(-2, 2, length.out = n)
  data.frame(x = x, z = sin(seq_len(n)), y = sin(3 * x) + cos(seq_len(n)) * .2)
}

test_that("grid and adaptive stopping policy agrees with actual native fits", {
  skip_if_package_unavailable("xgboost")
  data <- stopping_policy_data()
  for (search in c("grid", "adaptive")) {
    for (stopping in c(FALSE, TRUE)) {
      result <- autoxplain(
        data[1:128, ], "y", test_data = data[129:160, ], learners = "boosting",
        max_models = 2L, nfolds = 2L, tuning_rule = "best", explain = FALSE, seed = 812L,
        tuning_control = tuning_control(
          search = search, early_stopping = stopping, patience = 3L,
          screening_rows = 80L, threads = 1L
        )
      )
      evidence <- tuning_evidence(result)
      rationale <- evidence$search_space$families$rationale
      expect_match(rationale, paste("calibration is", if (stopping) "enabled" else "disabled"), fixed = TRUE)
      expect_identical(result$tuning$control$early_stopping, stopping)
      records <- result$tuning$fold_scores
      if (search == "adaptive") records <- rbind(records, result$tuning$screening$scores[names(records)])
      requested <- vapply(records$requested_parameters, `[[`, integer(1), "nrounds")
      effective <- vapply(records$effective_parameters, `[[`, integer(1), "nrounds")
      rounds <- lapply(records$learned, `[[`, "round_selection")
      if (stopping) {
        expect_true(all(vapply(rounds, function(record) identical(record$status, "calibrated"), logical(1))))
        expect_identical(effective, vapply(rounds, `[[`, integer(1), "selected_rounds"))
        expect_true(all(effective >= 1L & effective <= requested))
        expect_gt(result$tuning$resources$calibration_fit_attempts, 0L)
        expect_match(rationale, "unusable splits keep their requested cap", fixed = TRUE)
      } else {
        expect_true(all(vapply(rounds, is.null, logical(1))))
        expect_identical(effective, requested)
        expect_identical(result$tuning$resources$calibration_fit_attempts, 0L)
        expect_false(grepl("Round counts are chosen inside training splits", rationale, fixed = TRUE))
      }
      model <- result$models[[which(vapply(result$models, function(model) {
        inherits(model, "autoxplain_fitted_model") && model$backend == "xgboost"
      }, logical(1)))]]
      expect_equal(xgboost::xgb.get.num.boosted.rounds(model$fit), model$parameters$nrounds)
      if (stopping) {
        final <- evidence$refit$attempts
        id <- final$configuration_id[final$status == "ok"][[1L]]
        folds <- result$tuning$fold_scores[result$tuning$fold_scores$configuration_id == id, ]
        fold_rounds <- vapply(folds$effective_parameters, `[[`, integer(1), "nrounds")
        expect_equal(model$parameters$nrounds, ceiling(stats::median(fold_rounds)))
      }
      html <- AutoXplainR:::render_model_selection(result)
      expect_true(grepl(AutoXplainR:::html_escape(rationale), html, fixed = TRUE))
    }
  }
})

test_that("enabled calibration can skip unusable groups without claiming a round search", {
  skip_if_package_unavailable("xgboost")
  data <- stopping_policy_data()
  result <- autoxplain(
    data[1:128, ], "y", test_data = data[129:160, ], learners = "boosting",
    max_models = 1L, nfolds = 2L, explain = FALSE, seed = 913L,
    tuning_control = tuning_control(
      search = "grid", early_stopping = TRUE, fold_ids = rep(1:2, each = 64L), threads = 1L
    )
  )
  rationale <- tuning_evidence(result)$search_space$families$rationale
  expect_match(rationale, "calibration is enabled", fixed = TRUE)
  expect_match(rationale, "record why calibration was skipped", fixed = TRUE)
  rounds <- lapply(result$tuning$fold_scores$learned, `[[`, "round_selection")
  expect_true(all(vapply(rounds, function(record) identical(record$status, "skipped"), logical(1))))
  expect_true(all(vapply(rounds, function(record) {
    grepl("Fewer than two independent training groups", record$stop_reason, fixed = TRUE)
  }, logical(1))))
  expect_identical(result$tuning$resources$calibration_fit_attempts, 0L)
  for (index in seq_len(nrow(result$tuning$fold_scores))) {
    expect_identical(
      result$tuning$fold_scores$effective_parameters[[index]]$nrounds,
      result$tuning$fold_scores$requested_parameters[[index]]$nrounds
    )
  }
  expect_match(AutoXplainR:::render_model_selection(result), "Calibration skipped:", fixed = TRUE)
})

test_that("mixed skipped folds retain their caps in the recorded refit median", {
  plan <- data.frame(configuration_id = "boosting_01", family = "boosting")
  plan$parameters <- I(list(list(nrounds = 100L)))
  scores <- data.frame(configuration_id = "boosting_01", fold = 1:3, score = c(.3, .2, .4))
  scores$learned <- I(list(
    list(round_selection = list(status = "calibrated")),
    list(round_selection = list(status = "skipped")),
    list(round_selection = list(status = "calibrated"))
  ))
  scores$effective_parameters <- I(lapply(c(10L, 100L, 15L), function(rounds) list(nrounds = rounds)))
  actual <- AutoXplainR:::record_boosting_refit_rounds(plan, scores)$round_selection[[1L]]
  expect_identical(actual$selected_rounds, 15L)
  expect_identical(unname(actual$fold_rounds), c(10L, 100L, 15L))
  expect_identical(actual$calibrated_folds, 2L)
  expect_match(actual$scope, "2 of 3 folds calibrated", fixed = TRUE)
  expect_match(actual$scope, "any skipped fold kept its requested round cap", fixed = TRUE)
})
