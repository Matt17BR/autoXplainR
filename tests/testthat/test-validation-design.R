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

grouped_class_fixture <- function() {
  withr::with_seed(20260912, data.frame(
    site = rep(sprintf("site_%02d", 1:32), each = 20),
    feature = rnorm(640),
    outcome = factor(rep(c(rep("yes", 10), rep("no", 22)), each = 20),
                     levels = c("no", "yes"))
  ))
}

test_that("grouped tuning covers classes concentrated in separate groups", {
  data <- grouped_class_fixture()
  for (seed in 1:30) {
    design <- prepare_validation_design(
      data, "outcome", NULL, validation_split(group = "site"),
      0.2, seed, "base", "tuned", "binary", 5L, NULL
    )
    folds <- design$provenance$fold_ids
    expect_true(all(table(folds, design$training$outcome) > 0), info = paste("seed", seed))
    expect_true(all(vapply(split(folds, design$provenance$training_groups),
                           function(x) length(unique(x)) == 1L, logical(1))))
    expect_length(intersect(design$provenance$training_groups,
                            design$provenance$evaluation_groups), 0)
  }
  # The published allocator failed this public call despite six positive
  # training groups being available for five folds.
  fit <- autoxplain(data, "outcome", validation = validation_split(group = "site"),
                    learners = "tree", max_models = 1, nfolds = 5, seed = 1, explain = FALSE)
  expect_true(all(is.finite(fit$tuning$fold_scores$score)))
  probabilities <- predict(fit, data[fit$validation$evaluation_rows, ])
  expect_true(all(is.finite(probabilities)))
  expect_true(all(probabilities >= 0 & probabilities <= 1))
})

test_that("mixed-class groups are indivisible and seeded allocation preserves RNG", {
  groups <- rep(letters[1:6], each = 2)
  outcome <- c("A", "B", "A", "B", "A", "C", "A", "C", "B", "C", "B", "C")
  set.seed(18)
  before <- .Random.seed
  first <- grouped_fold_ids(groups, 3, 7, outcome)
  expect_identical(.Random.seed, before)
  expect_identical(grouped_fold_ids(groups, 3, 7, outcome), first)
  expect_true(all(table(first, outcome) > 0))
  expect_true(all(vapply(split(first, groups), function(x) length(unique(x)) == 1L, logical(1))))
  renamed <- c(A = "third", B = "first", C = "second")[outcome]
  expect_identical(grouped_fold_ids(groups, 3, 7, renamed), first)
})

test_that("grouped class failures distinguish insufficient groups from bounded search", {
  expect_error(
    grouped_fold_ids(rep(letters[1:6], each = 2), 3, 1,
                     c(rep("rare", 4), rep("common", 8))),
    "rare.*2 groups.*Reduce.*nfolds"
  )
  # AB, AC, BC cannot be split into two partitions both containing ABC,
  # although each class occurs in two groups.
  expect_error(
    grouped_fold_ids(rep(letters[1:3], each = 2), 2, 1, c("A", "B", "A", "C", "B", "C")),
    "search is bounded; this does not prove no allocation exists"
  )
})

test_that("outer test classes cannot influence grouped fold allocation or fitting", {
  data <- grouped_class_fixture()
  fit <- function(x) {
    autoxplain(
      x, "outcome", validation = validation_split(group = "site"), learners = "tree",
      max_models = 2, nfolds = 5, seed = 1, explain = FALSE
    )
  }
  first <- fit(data)
  test_rows <- first$validation$evaluation_rows
  data$outcome[test_rows] <- ifelse(data$outcome[test_rows] == "yes", "no", "yes")
  second <- fit(data)
  expect_identical(first$validation$fold_ids, second$validation$fold_ids)
  expect_identical(first$tuning$selected_configuration, second$tuning$selected_configuration)
  expect_identical(first$tuning$fold_scores$score, second$tuning$fold_scores$score)
  expect_identical(first$preprocessing_metadata$training_data,
                   second$preprocessing_metadata$training_data)
  expect_identical(first$evaluation$predictions$primary_prediction,
                   second$evaluation$predictions$primary_prediction)
})

test_that("grouped class retries explore feasible arrangements beyond priority ties", {
  # This literal coverage matrix has a known valid partition, but repeating the
  # same priority order with random tie breaks rejected it in all 16 attempts.
  counts <- rbind(c(1L, 1L, 0L), c(0L, 0L, 1L), c(1L, 1L, 0L),
                  c(1L, 0L, 2L), c(0L, 1L, 2L), c(0L, 1L, 2L))
  example <- do.call(rbind, lapply(seq_len(nrow(counts)), function(group) {
    data.frame(group = group, outcome = rep(c("A", "B", "C"), counts[group, ]))
  }))
  witness <- c(1L, 1L, 2L, 3L, 2L, 3L)[example$group]
  expect_true(all(table(witness, example$outcome) > 0))
  actual <- grouped_fold_ids(example$group, 3L, 7L, example$outcome)
  expect_true(all(table(actual, example$outcome) > 0))
  expect_true(all(vapply(split(actual, example$group),
                         function(values) length(unique(values)) == 1L, logical(1))))
})
