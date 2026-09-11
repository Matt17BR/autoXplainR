test_that("rank-deficient linear fits retain native predictions and expose the warning", {
  data <- withr::with_seed(91, data.frame(x = rnorm(80), z = rnorm(80), y = rnorm(80)))
  data$copy <- 2 * data$x
  for (model_set in c("quick", "tuned")) {
    result <- autoxplain(data[1:60, ], "y", test_data = data[61:80, ],
      model_set = model_set, learners = "linear", max_models = 1,
      nfolds = 3, explain = FALSE
    )
    id <- result$provenance$primary_model_id
    fit <- result$models[[id]]
    expect_lt(fit$rank, length(coef(fit)))
    warning <- result$model_diagnostics$fit_warning[result$model_diagnostics$model_id == id]
    expect_match(warning, "rank deficient: 3.*4 design columns")
    expect_true("model_fit_warning" %in% result$evaluation$notes$code)
    expect_match(explorer_model_spec_details(result, id), "rank deficient: 3")
    spec <- model_specification(result, id)
    expect_equal(spec$learned$`Matrix rank`, 3L)
    expect_equal(spec$learned$`Design columns (including intercept)`, 4L)
    expect_match(spec$summary, "rank deficient (3/4)", fixed = TRUE)
    native <- lm(y ~ x + z + copy, data = result$training_data)
    expect_equal(unname(suppressWarnings(predict(result, data[61:80, ]))),
                 unname(suppressWarnings(predict(native, result$test_data))))
    if (model_set == "tuned") {
      expect_true(all(grepl("rank deficient", result$tuning$fold_scores$warning)))
    }
  }
})

test_that("full-rank linear fits do not receive a rank warning", {
  data <- withr::with_seed(93, data.frame(x = rnorm(60), y = rnorm(60)))
  result <- autoxplain(data, "y", model_set = "quick", explain = FALSE)
  expect_false(any(grepl("rank deficient", result$model_diagnostics$fit_warning)))
})

test_that("supplied multiresponse models distinguish design width from coefficient count", {
  data <- withr::with_seed(951, data.frame(
    x = rnorm(60), z = rnorm(60), y = rnorm(60), second_outcome = rnorm(60)
  ))
  data$copy <- 2 * data$x
  training <- data[1:40, ]
  evaluation <- data[41:60, ]
  for (aliased in c(FALSE, TRUE)) {
    formula <- if (aliased) cbind(y, second_outcome) ~ x + z + copy else cbind(y, second_outcome) ~ x + z
    model <- lm(formula, data = training)
    features <- if (aliased) c("x", "z", "copy") else c("x", "z")
    predict_first <- function(model, newdata) stats::predict(model, newdata)[, "y"]
    result <- suppressWarnings(evaluate_models(
      list(multiresponse = model), evaluation, "y", features = features,
      predict_functions = list(multiresponse = predict_first), training_data = training
    ))
    spec <- model_specification(result, "multiresponse")
    expect_equal(spec$learned$`Design columns (including intercept)`, if (aliased) 4L else 3L)
    expect_equal(spec$learned$`Matrix rank`, 3L)
    expect_equal(spec$learned$`Fitted coefficients`, 6L)
    expect_equal(spec$learned$`Model responses`, 2L)
    expect_match(spec$summary, "6 fitted coefficients across 2 responses", fixed = TRUE)
    if (aliased) {
      expect_match(spec$summary, "rank deficient (3/4)", fixed = TRUE)
    } else {
      expect_false(grepl("rank deficient", spec$summary, fixed = TRUE))
    }
    expect_identical(spec$coefficients, coef(model))
    expect_equal(unname(suppressWarnings(predict(result, evaluation))),
                 unname(suppressWarnings(predict_first(model, evaluation))))
  }
})
