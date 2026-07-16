test_that("H2O AutoML integrates through the model-agnostic contract", {
  skip_if_not(identical(tolower(Sys.getenv("AUTOXPLAIN_RUN_H2O")), "true"))
  skip_if_not_installed("h2o")

  set.seed(101)
  data <- data.frame(x1 = rnorm(100), x2 = rnorm(100))
  data$y <- as.integer(data$x1 + data$x2 > 0)
  result <- suppressWarnings(autoxplain(
    data[1:75, ], "y", test_data = data[76:100, ], max_models = 1,
    max_runtime_secs = 0, nfolds = 2, seed = 101, verbosity = "quiet",
    engine = "h2o", evaluation_role = "test", include_algos = "GLM"
  ))
  on.exit(try(h2o::h2o.shutdown(prompt = FALSE), silent = TRUE), add = TRUE)

  expect_s3_class(result, "autoxplain_result")
  expect_equal(result$task, "binary")
  expect_equal(result$provenance$evaluation_role, "test")
  expect_s3_class(result$engine_leaderboard, "data.frame")
  expect_identical(
    result$provenance$primary_model_id,
    as.character(result$engine_leaderboard$model_id[[1L]])
  )
  expect_identical(
    result$evaluation$primary_model_id,
    result$provenance$primary_model_id
  )
  expect_match(result$provenance$primary_model_label, "^H2O ")
  expect_match(result$provenance$candidate_selection, "training-only cross-validation")
  expect_true(all(c("log_loss", "brier_score", "role") %in% names(result$leaderboard)))
  expect_true("simple_baseline" %in% result$leaderboard$model_id)
  expect_true(is.finite(
    result$evaluation$metrics[[result$evaluation$primary_model_id]][["log_loss"]]
  ))
  explainers <- as_explainers(result)
  expect_s3_class(explainers[[1]], "autoxplain_explainer")
  audit <- audit_explanations(explainers, n_repeats = 2)
  expect_s3_class(audit, "autoxplain_audit")
  report <- tempfile(fileext = ".html")
  on.exit(unlink(report), add = TRUE)
  render_model_report(result, report, audit = audit, effects = list())
  html <- paste(readLines(report, warn = FALSE), collapse = "\n")
  expect_match(html, "Did the model generalize", fixed = TRUE)
  expect_match(html, "intercept-only baseline", fixed = TRUE)

  set.seed(202)
  regression_data <- data.frame(x1 = rnorm(90), x2 = runif(90, -1, 1))
  regression_data$y <- 2 * regression_data$x1 - regression_data$x2 +
    rnorm(90, sd = 0.15)
  regression <- suppressWarnings(autoxplain(
    regression_data[1:70, ],
    "y",
    test_data = regression_data[71:90, ],
    max_models = 1,
    max_runtime_secs = 0,
    nfolds = 2,
    seed = 202,
    verbosity = "quiet",
    engine = "h2o",
    evaluation_role = "test",
    include_algos = "GLM"
  ))

  expect_identical(regression$task, "regression")
  expect_identical(regression$evaluation$primary_metric, "rmse")
  expect_identical(
    regression$evaluation$primary_model_id,
    regression$provenance$primary_model_id
  )
  expect_true(all(c("rmse", "mae", "r_squared", "role") %in%
                    names(regression$leaderboard)))
  expect_true("simple_baseline" %in% regression$leaderboard$model_id)
  expect_true(is.finite(
    regression$evaluation$metrics[[regression$evaluation$primary_model_id]][["rmse"]]
  ))
  expect_true(all(is.finite(regression$evaluation$predictions$primary_prediction)))
  regression_explainer <- as_explainers(
    regression,
    models = regression$evaluation$primary_model_id
  )[[1L]]
  expect_s3_class(regression_explainer, "autoxplain_explainer")
  regression_importance <- calculate_permutation_importance(
    regression_explainer,
    features = "x1",
    n_repeats = 1,
    seed = 202
  )
  expect_s3_class(regression_importance, "autoxplain_importance")
  expect_identical(regression_importance$feature, "x1")

  class_rows <- split(seq_len(nrow(iris)), iris$Species)
  training_rows <- unlist(lapply(class_rows, utils::head, 40L), use.names = FALSE)
  evaluation_rows <- setdiff(seq_len(nrow(iris)), training_rows)
  multiclass <- suppressWarnings(autoxplain(
    iris[training_rows, ],
    "Species",
    test_data = iris[evaluation_rows, ],
    max_models = 1,
    max_runtime_secs = 0,
    nfolds = 2,
    seed = 303,
    verbosity = "quiet",
    engine = "h2o",
    evaluation_role = "test",
    include_algos = "GLM"
  ))

  expect_identical(multiclass$task, "multiclass")
  expect_identical(multiclass$evaluation$primary_metric, "log_loss")
  expect_identical(
    multiclass$evaluation$primary_model_id,
    multiclass$provenance$primary_model_id
  )
  expect_true(all(c("log_loss", "brier_score", "accuracy", "role") %in%
                    names(multiclass$leaderboard)))
  expect_true("simple_baseline" %in% multiclass$leaderboard$model_id)
  expect_true(is.finite(
    multiclass$evaluation$metrics[[multiclass$evaluation$primary_model_id]][["log_loss"]]
  ))
  multiclass_explainer <- as_explainers(
    multiclass,
    models = multiclass$evaluation$primary_model_id
  )[[1L]]
  expect_s3_class(multiclass_explainer, "autoxplain_explainer")
  probabilities <- predict(
    multiclass_explainer,
    multiclass_explainer$data[1:3, , drop = FALSE]
  )
  expect_identical(dim(probabilities), c(3L, 3L))
  expect_identical(colnames(probabilities), levels(iris$Species))
  expect_equal(unname(rowSums(probabilities)), rep(1, 3), tolerance = 1e-7)
})
