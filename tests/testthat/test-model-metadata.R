test_that("weighted efficiency validates and warns about subjectivity", {
  expect_warning(
    score <- calculate_weighted_efficiency(c(0.7, 0.8, 0.9), c(1, 3, 8)),
    "candidate-set-relative"
  )
  expect_true(all(score >= 0 & score <= 1))
  expect_error(calculate_weighted_efficiency(1, 2), "at least two")
  expect_error(calculate_weighted_efficiency(c(1, NA), c(1, 2)), "finite")
})
