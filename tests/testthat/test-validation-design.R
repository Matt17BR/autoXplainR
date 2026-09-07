validation_fixture <- function() {
  withr::with_seed(44, data.frame(
    site = rep(seq_len(20), each = 6), time = rep(seq_len(30), each = 4),
    x = rnorm(120), y = rnorm(120)
  ))
}

test_that("group holdouts and tuning keep entire units separate", {
  data <- validation_fixture()
  data$time <- NULL
  result <- autoxplain(data, "y",
    validation = validation_split(group = "site"),
    model_set = "tuned", learners = c("linear", "tree"), max_models = 3,
    nfolds = 3, explain = FALSE
  )
  plan <- result$validation
  expect_length(intersect(plan$training_groups, plan$evaluation_groups), 0)
  expect_false("site" %in% result$features)
  expect_equal(sort(c(plan$training_rows, plan$evaluation_rows)), seq_len(nrow(data)))
  expect_true(all(vapply(
    split(plan$fold_ids, plan$training_groups), function(x) length(unique(x)) == 1L,
    logical(1)
  )))
  expect_identical(result$tuning$fold_assignment$fold, plan$fold_ids)
  expect_equal(predict(result, data[plan$evaluation_rows, ]), result$evaluation$predictions$primary_prediction)
  expect_identical(result$provenance$evaluation_role, "test")
})

test_that("temporal boundaries preserve ties, order and gap", {
  data <- validation_fixture()
  data$site <- NULL
  data$time <- as.Date("2020-01-01") + data$time
  data <- data[rev(seq_len(nrow(data))), ]
  result <- autoxplain(data, "y",
    model_set = "quick",
    validation = validation_split(time = "time", gap = 2), explain = FALSE
  )
  plan <- result$validation
  expect_lt(max(data$time[plan$training_rows]), min(data$time[plan$evaluation_rows]))
  expect_equal(length(unique(data$time[plan$excluded_rows])), 2L)
  expect_length(intersect(data$time[plan$training_rows], data$time[plan$evaluation_rows]), 0)
  expect_false("time" %in% result$features)
  expect_equal(sort(c(plan$training_rows, plan$evaluation_rows, plan$excluded_rows)), seq_len(nrow(data)))
  expect_error(
    autoxplain(data, "y", validation = validation_split(time = "time"), model_set = "tuned"),
    "rolling-origin"
  )
})

test_that("validation specifications fail before training when infeasible", {
  data <- validation_fixture()
  spec <- validation_split(group = "site")
  expect_error(validation_split(), "exactly one")
  expect_error(validation_split(group = "site", time = "time"), "exactly one")
  expect_error(validation_split(group = "site", gap = 1), "only with")
  expect_error(validation_split(time = "time", gap = -1), "gap")
  expect_error(validation_split(time = NA_character_), "column name")
  expect_error(autoxplain(model_set = "quick", data, "y", validation = list()), "validation_split")
  expect_error(autoxplain(data, "y", validation = spec, engine = "h2o"), "base engine")
  expect_error(autoxplain(model_set = "quick", data, "y", validation = spec, test_data = data), "not both")
  expect_error(autoxplain(model_set = "quick", data, "y", validation = validation_split(group = "y")), "differ")
  expect_error(
    autoxplain(data, "y",
      validation = spec, model_set = "tuned", portfolio = "core",
      tuning_control = tuning_control(fold_ids = rep(1:3, 40))
    ),
    "own fold IDs"
  )
  data$site[1] <- NA
  expect_error(autoxplain(model_set = "quick", data, "y", validation = spec), "without missing")
  data$site <- 1
  expect_error(autoxplain(model_set = "quick", data, "y", validation = spec), "at least three")
  expect_error(
    autoxplain(data, "y", model_set = "quick", validation = validation_split(time = "time", gap = 100)),
    "Too few"
  )
})

test_that("outer test labels cannot change grouped tuning or preprocessing", {
  data <- validation_fixture()
  data$time <- NULL
  fit <- function(x) {
    autoxplain(x, "y",
      validation = validation_split(group = "site"),
      model_set = "tuned", learners = "tree", max_models = 2,
      nfolds = 3, explain = FALSE
    )
  }
  first <- fit(data)
  data$y[first$validation$evaluation_rows] <- 1e8
  second <- fit(data)
  expect_identical(first$tuning$selected_configuration, second$tuning$selected_configuration)
  expect_identical(first$tuning$fold_scores$score, second$tuning$fold_scores$score)
  expect_identical(
    first$preprocessing_metadata$training_data$recipe,
    second$preprocessing_metadata$training_data$recipe
  )
})
