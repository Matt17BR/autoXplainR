test_that("individual identifiers cannot masquerade as perfect association", {
  id <- paste0("record-", seq_len(40))
  set.seed(78)
  value <- rnorm(40)
  association <- AutoXplainR:::feature_association
  expect_true(is.na(association(id, value)))
  expect_true(is.na(association(id, rep(c("A", "B"), 20))))
  checked <- AutoXplainR:::dependence_table(data.frame(id = id, value = value), .7)
  expect_true(all(is.na(checked$max_association)))
  expect_identical(checked$screen_status, rep("association_unavailable", 2))
  expect_false(any(checked$high_dependence))
  expect_identical(checked$predictors_unavailable, c(1L, 1L))
  model <- lm(value ~ 1)
  explainer <- explain_model(model, data.frame(id = id, value = value), "value")
  audit <- audit_explanations(explainer, n_repeats = 2)
  expect_identical(audit$diagnostic_status$association$status, "inapplicable")
  expect_true("association_screen_scope" %in% audit$findings$code)
})

test_that("observed-cell association matches an independent dense contingency reference", {
  set.seed(945)
  x <- rep(paste0("x", seq_len(1200)), each = 3)
  y <- sample(rep(paste0("y", seq_len(1100)), length.out = length(x)))
  counts <- table(x, y)
  expect_true(all(rowSums(counts) == 3))
  column_n <- colSums(counts)
  expect_setequal(unique(column_n), c(3, 4))
  # E = column_n / 1200, so each Pearson residual term is
  # (1200 * O - column_n)^2 / (1200 * column_n). Multiplying by
  # 14400 makes every term an integer because column_n is 3 or 4.
  # Their positive sum stays below 2^53 and is exact even when R has
  # no extended-precision accumulator. Summing 1.32 million unscaled
  # terms lost precision in the dense oracle on macOS ARM.
  residual_numerator <- sweep(counts * 1200, 2L, column_n, "-")
  scaled_terms <- sweep(residual_numerator^2, 2L, 12 / column_n, "*")
  expect_true(all(scaled_terms == floor(scaled_terms)))
  expect_lt(sum(scaled_terms), 2^53)
  statistic <- sum(scaled_terms) / 14400
  reference <- sqrt(statistic / (sum(counts) * (min(dim(counts)) - 1)))
  value <- AutoXplainR:::feature_association(x, y)
  expect_equal(value, reference, tolerance = 1e-13)
  expect_equal(AutoXplainR:::feature_association(y, x), reference, tolerance = 1e-13)
})
