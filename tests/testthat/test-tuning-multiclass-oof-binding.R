multiclass_oof_fixture <- function() {
  with_preserved_seed(317L, {
    labels <- c("zebra", "ant", "mole", "ibis", "cat", "wolf", "bear")
    outcome <- factor(rep(labels, c(196L, 183L, 41L, 5L, 32L, 25L, 25L)), levels = labels)
    data <- data.frame(x = rnorm(length(outcome)), z = runif(length(outcome)), y = outcome)
    rownames(data) <- paste0("source-", seq_len(nrow(data)))
    folds <- integer(nrow(data))
    for (label in labels) {
      rows <- which(outcome == label)
      folds[rows] <- rep(seq_len(5L), length.out = length(rows))
    }
    list(data = data, folds = folds, labels = labels)
  })
}

test_that("mixed native multiclass OOF matrices preserve every case and probability", {
  skip_if_package_unavailable("ranger")
  skip_if_package_unavailable("xgboost")
  fixture <- multiclass_oof_fixture()
  data <- fixture$data
  folds <- fixture$folds
  labels <- fixture$labels
  forest <- list(
    num.trees = 7L, mtry = 2L, min.node.size = 3L,
    sample.fraction = .8, splitrule = "default"
  )
  boosting <- list(
    nrounds = 3L, eta = .1, max_depth = 2L, min_child_weight = 1,
    subsample = 1, colsample_bytree = 1, reg_alpha = 0, reg_lambda = 1, encoding = "matrix"
  )
  fit <- function(retain_oof) {
    autoxplain(
      data, "y", test_data = data[seq(1L, nrow(data), length.out = 35L), ],
      enable_preprocessing = FALSE, learners = c("forest", "boosting"), max_models = 2L,
      nfolds = 5L, seed = 47L, tuning_rule = "best", explain = FALSE, overlap_action = "ignore",
      tuning_control = tuning_control(
        search = "grid", fold_ids = folds, retain_oof = retain_oof,
        grids = list(forest = list(forest), boosting = list(boosting))
      )
    )
  }
  result <- fit(TRUE)
  oof <- result$tuning$out_of_fold_predictions
  expect_identical(as.integer(table(folds)), c(104L, 102L, 101L, 100L, 100L))
  expect_equal(nrow(oof), 2L * nrow(data))
  expect_false(anyDuplicated(oof[c("configuration_id", "training_row")]) > 0L)
  expect_identical(colnames(oof$probabilities), labels)
  expect_null(rownames(oof$probabilities))
  expect_identical(oof$source_row, rownames(data)[oof$training_row])
  expect_identical(oof$truth, as.character(data$y[oof$training_row]))

  # Refit every small fold directly through each native API. These numerical
  # references do not use the package prediction adapter or OOF formatter.
  for (family in c("forest", "boosting")) {
    id <- paste0(family, "_01")
    for (fold in seq_len(5L)) {
      fit_rows <- which(folds != fold)
      assessment_rows <- which(folds == fold)
      score <- result$tuning$fold_scores[
        result$tuning$fold_scores$configuration_id == id & result$tuning$fold_scores$fold == fold,
      ]
      if (family == "forest") {
        model <- ranger::ranger(
          x = data[fit_rows, c("x", "z")], y = data$y[fit_rows], num.trees = 7L,
          mtry = 2L, min.node.size = 3L, sample.fraction = .8, probability = TRUE,
          respect.unordered.factors = "order", num.threads = 1L,
          seed = score$fit_seed, oob.error = FALSE, verbose = FALSE
        )
        expected <- predict(model, data = data[assessment_rows, c("x", "z")], num.threads = 1L)$predictions
        expected <- expected[, labels, drop = FALSE]
      } else {
        model <- xgboost::xgb.train(
          params = list(
            objective = "multi:softprob", num_class = 7L, eval_metric = "mlogloss",
            eta = .1, max_depth = 2L, min_child_weight = 1, subsample = 1,
            colsample_bytree = 1, alpha = 0, lambda = 1, nthread = 1L,
            seed = score$fit_seed, verbosity = 0L
          ),
          data = xgboost::xgb.DMatrix(
            as.matrix(data[fit_rows, c("x", "z")]),
            label = as.integer(data$y[fit_rows]) - 1L, nthread = 1L
          ),
          nrounds = 3L, verbose = 0L
        )
        expected <- predict(model, as.matrix(data[assessment_rows, c("x", "z")]))
        colnames(expected) <- labels
      }
      actual <- oof[oof$configuration_id == id & oof$fold == fold, , drop = FALSE]
      actual <- actual[order(actual$training_row), , drop = FALSE]
      expect_identical(actual$training_row, assessment_rows)
      expect_identical(actual$source_row, rownames(data)[assessment_rows])
      expect_equal(unname(actual$probabilities), unname(expected), tolerance = 0, ignore_attr = TRUE)
      truth_probability <- expected[cbind(seq_along(assessment_rows), as.integer(data$y[assessment_rows]))]
      expect_equal(actual$truth_probability, unname(truth_probability), tolerance = 0)
      expect_equal(actual$case_loss, -log(pmax(truth_probability, 1e-15)), tolerance = 0, ignore_attr = TRUE)
      expect_equal(score$score, -mean(log(pmax(truth_probability, 1e-15))), tolerance = 0)
    }
  }
  without <- fit(FALSE)
  expect_null(without$tuning$out_of_fold_predictions)
  expect_identical(without$tuning$fold_scores$score, result$tuning$fold_scores$score)
  expect_identical(without$tuning$fold_scores$effective_parameters, result$tuning$fold_scores$effective_parameters)
  expect_identical(without$tuning$final_configuration, result$tuning$final_configuration)
  for (id in names(result$models)) {
    expect_equal(predict(without, data, model = id), predict(result, data, model = id), tolerance = 0)
  }
})

test_that("OOF binding validates probability schemas and retains zero-column regression matrices", {
  row <- data.frame(configuration_id = "model_01", training_row = 1:2, source_row = c("a", "b"))
  row$probabilities <- I(matrix(numeric(), nrow = 2L, ncol = 0L))
  result <- combine_tuning_predictions(list(row, row), "regression")
  expect_identical(dim(result$probabilities), c(4L, 0L))
  expect_identical(result$source_row, c("a", "b", "a", "b"))
  row$probabilities <- I(matrix(
    c(.2, .8, .8, .2), nrow = 2L,
    dimnames = list(c("a", "b"), c("yes", "no"))
  ))
  incompatible <- row
  colnames(incompatible$probabilities) <- c("no", "yes")
  expect_error(combine_tuning_predictions(list(row, incompatible), "multiclass"), "same class order")
})
