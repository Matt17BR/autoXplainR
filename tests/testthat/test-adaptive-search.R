test_that("adaptive grids cover native model capacity with reproducible independent family streams", {
  grid <- AutoXplainR:::adaptive_parameter_grids
  set.seed(817)
  rng <- .Random.seed
  first <- grid(c("regularized", "forest", "boosting"),
    n = 50000L, p = 40L, task = "binary", n_classes = 2L, seed = 71L, max_models = 15L
  )
  expect_identical(.Random.seed, rng)
  reversed <- grid(c("boosting", "regularized", "forest"),
    n = 50000L, p = 40L, task = "binary", n_classes = 2L, seed = 71L, max_models = 15L
  )
  expect_identical(first, reversed[names(first)])
  changed <- grid(c("regularized", "forest", "boosting"),
    n = 50000L, p = 40L, task = "binary", n_classes = 2L, seed = 72L, max_models = 15L
  )
  expect_false(identical(first$boosting, changed$boosting))
  expect_identical(first$boosting[1:3], changed$boosting[1:3])
  expect_equal(vapply(first$boosting[1:3], `[[`, integer(1), "max_depth"), c(3L, 6L, 10L))
  expect_true(all(vapply(first$boosting, `[[`, numeric(1), "eta") >= .03))
  expect_true(all(vapply(first$boosting, `[[`, integer(1), "nrounds") == 2000L))
  expect_true(length(unique(vapply(first$boosting, `[[`, numeric(1), "reg_lambda"))) > 3L)
  expect_true(any(vapply(first$boosting, `[[`, numeric(1), "reg_alpha") > 0))
  expect_true(all(vapply(first$forest, `[[`, integer(1), "mtry") %in% 1:40))
  expect_setequal(vapply(first$forest[1:4], `[[`, character(1), "splitrule"), c("default", "extratrees"))
  narrow <- grid("forest", n = 25L, p = 1L, task = "regression",
    n_classes = 1L, seed = 71L, max_models = 20L
  )
  expect_length(narrow$forest, 20L)
  expect_true(all(vapply(narrow$forest, `[[`, integer(1), "mtry") == 1L))
  expect_error(grid(c("forest", "forest"), 25, 1, "regression", 1, 71, 2), "unique")
})

test_that("screening keeps original fold boundaries, rare classes and source identities", {
  partition <- AutoXplainR:::adaptive_screen_partition
  data <- data.frame(x = seq_len(1000L), y = factor(rep("common", 1000L), c("common", "rare")))
  data$y[c(1L, 251L, 501L, 751L)] <- "rare"
  rownames(data) <- paste0("source-", seq_len(nrow(data)))
  ids <- rep(seq_len(4L), each = 250L)
  groups <- rep(paste0("group-", seq_len(40L)), each = 25L)
  set.seed(431)
  rng <- .Random.seed
  actual <- partition(data, "y", "binary", list(id = ids), 91L, 100L, groups)
  expect_identical(.Random.seed, rng)
  expect_identical(actual, partition(data, "y", "binary", ids, 91L, 100L, groups))
  expect_equal(length(actual$training_row) + length(actual$validation_row), 100L)
  expect_equal(length(intersect(actual$training_row, actual$validation_row)), 0L)
  expect_true(all(ids[actual$training_row] != actual$fold))
  expect_true(all(ids[actual$validation_row] == actual$fold))
  expect_equal(length(intersect(groups[actual$training_row], groups[actual$validation_row])), 0L)
  expect_setequal(data$y[actual$training_row], levels(data$y))
  expect_setequal(data$y[actual$validation_row], levels(data$y))
  expect_identical(actual$training_source_row, rownames(data)[actual$training_row])
  expect_identical(actual$validation_source_row, rownames(data)[actual$validation_row])
  expect_equal(sum(actual$training_sampling_weight), sum(ids != actual$fold))
  expect_equal(sum(actual$validation_sampling_weight), sum(ids == actual$fold))
  weights <- actual$validation_sampling_weight
  rare_sample <- as.character(data$y[actual$validation_row]) == "rare"
  expect_equal(sum(weights * rare_sample) / sum(weights), 1 / 250)
  expect_equal(actual$sampling$validation$available, c(249L, 1L))
  expect_equal(actual$sampling$validation$sampled, c(19L, 1L))
  expect_match(actual$scope, "rows may be sampled within", fixed = TRUE)
  crossed <- groups
  crossed[[251L]] <- crossed[[1L]]
  expect_error(partition(data, "y", "binary", ids, 91L, 100L, crossed), "cross")
  impossible <- data
  impossible$y[] <- "common"
  impossible$y[[1L]] <- "rare"
  expect_error(partition(impossible, "y", "binary", ids, 91L, 100L), "every outcome class")
})

test_that("screening caps do not silently lose classes or discard an available row budget", {
  partition <- AutoXplainR:::adaptive_screen_partition
  data <- data.frame(x = 1:100, y = sin(1:100))
  ids <- rep(1:2, c(10L, 90L))
  for (seed in 1:4) {
    actual <- partition(data, "y", "regression", ids, seed, 100L)
    expect_equal(length(actual$training_row) + length(actual$validation_row), 100L)
    expect_setequal(c(actual$training_row, actual$validation_row), 1:100)
  }
  data$y <- factor(rep(c("a", "b", "c"), length.out = 100))
  expect_error(partition(data, "y", "multiclass", rep(1:2, each = 50), 4L, 12L), "row cap")
  sample_rows <- AutoXplainR:::adaptive_sample_rows
  rare <- factor(c(rep("a", 95), rep("b", 4), "c"))
  selected <- AutoXplainR:::with_preserved_seed(81, sample_rows(1:100, rare, "multiclass", 6L))
  expect_length(selected, 6L)
  expect_setequal(rare[selected], levels(rare))
  expect_false(anyDuplicated(selected) > 0L)
  expect_error(sample_rows(1:100, rare, "multiclass", 2L), "every outcome class")
})

test_that("screening work limits are separate from the requested final configuration", {
  reduce <- AutoXplainR:::adaptive_screen_parameters
  forest <- list(num.trees = 800L, mtry = 7L, min.node.size = 3L, sample.fraction = .8, splitrule = "default")
  boosting <- list(nrounds = 2000L, eta = .05, max_depth = 6L)
  expect_equal(reduce(forest, "forest")$num.trees, 128L)
  expect_equal(reduce(boosting, "boosting")$nrounds, 600L)
  expect_identical(forest$num.trees, 800L)
  expect_identical(boosting$nrounds, 2000L)
  expect_identical(reduce(list(nrounds = 40L), "boosting"), list(nrounds = 40L))
  expect_identical(reduce(list(alpha = .7), "regularized"), list(alpha = .7))
})

test_that("promotion uses the requested metric and never erases a successful family", {
  promote <- AutoXplainR:::adaptive_promote
  plan <- data.frame(configuration_id = c("f1", "f2", "b1", "b2", "r1", "r2"),
    family = c("forest", "forest", "boosting", "boosting", "regularized", "regularized")
  )
  screening <- data.frame(configuration_id = plan$configuration_id,
    score = c(.91, .76, .85, .85, NA, .60), error = "", status = "ok"
  )
  auc <- promote(plan, screening, 1L, "auc")
  expect_identical(auc$configuration_id[auc$promoted], c("f1", "b1", "r2"))
  loss <- promote(plan, screening, 1L, "log_loss")
  expect_identical(loss$configuration_id[loss$promoted], c("f2", "b1", "r2"))
  expect_identical(auc, promote(plan, screening[c(6, 4, 2, 1, 5, 3), ], 1L, "auc"))
  screening$error[[1L]] <- "Fit reached its limit and was excluded."
  screening$status[[3L]] <- "failed"
  screening$score[[5L]] <- Inf
  failed <- promote(plan, screening, 2L, "auc")
  expect_identical(failed$configuration_id[failed$promoted], c("f2", "b2", "r2"))
  expect_false(any(failed$promoted[c(1, 3, 5)]))
  missing <- promote(plan, screening[-6L, ], 1L, "auc")
  expect_false(any(missing$promoted[plan$family == "regularized"]))
  expect_error(promote(plan, rbind(screening, screening[1L, ]), 1L, "auc"), "unique")
})
