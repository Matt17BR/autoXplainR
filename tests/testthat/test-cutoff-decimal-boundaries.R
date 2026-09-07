test_that("every displayed decimal cutoff includes probability ties", {
  # Parse independent decimal literals instead of repeating the implementation's grid arithmetic.
  labels <- c("0.00", sprintf("0.%02d", 1:99), "1.00")
  probabilities <- as.numeric(rep(labels, each = 2L))
  truth <- factor(rep(c("yes", "no"), 101L), levels = c("yes", "no"))
  records <- AutoXplainR:::prediction_cutoff_records(truth, probabilities, "yes")
  for (i in seq_along(labels)) {
    record <- records[[i]]
    expect_identical(record$threshold, as.numeric(labels[i]))
    # One positive and one negative case at every decimal value, including endpoints.
    expect_equal(
      unlist(record[c("tp", "fp", "tn", "fn")]),
      c(tp = 102L - i, fp = 102L - i, tn = i - 1L, fn = i - 1L)
    )
  }
  expect_equal(unlist(records[[58L]][c("tp", "fp", "tn", "fn")]), c(tp = 44, fp = 44, tn = 57, fn = 57))
})

test_that("public default cutoffs use the displayed decimal while explicit numbers keep their meaning", {
  defaults <- eval(formals(threshold_diagnostics)$thresholds)
  literals <- as.numeric(c(
    "0.10", "0.15", "0.20", "0.25", "0.30", "0.35", "0.40", "0.45", "0.50",
    "0.55", "0.60", "0.65", "0.70", "0.75", "0.80", "0.85", "0.90"
  ))
  expect_identical(defaults, literals)
  explicit <- c(.57, .57 + .Machine$double.eps)
  expect_identical(AutoXplainR:::validate_thresholds(explicit), explicit)
})
