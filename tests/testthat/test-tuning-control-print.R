tuning_print_line <- function(lines, label) {
  lines[startsWith(lines, paste0("  ", label, ":"))]
}

test_that("automatic control printing describes conditional settings without resolving them", {
  control <- tuning_control()
  before <- serialize(control, NULL)
  printed <- NULL
  lines <- capture.output(printed <- withVisible(print(control)))
  expect_identical(serialize(control, NULL), before)
  expect_identical(printed$value, control)
  expect_false(printed$visible)
  expect_match(tuning_print_line(lines, "threads"), "automatic per native fit", fixed = TRUE)
  expect_match(tuning_print_line(lines, "screening"), "20,000 rows; 1 finalist per family (if adaptive)", fixed = TRUE)
  expect_match(tuning_print_line(lines, "early stop"), "automatic for adaptive boosting only", fixed = TRUE)
  expect_match(tuning_print_line(lines, "patience"), "30 rounds without improvement", fixed = TRUE)
  expect_match(tuning_print_line(lines, "time limit"), "none", fixed = TRUE)
  expect_null(control$threads)
  expect_null(control$early_stopping)
})

test_that("explicit adaptive controls show actual counts and the scheduling boundary", {
  control <- tuning_control(
    search = "adaptive", threads = 3L,
    screening_rows = 12340L, finalists_per_family = 2L,
    early_stopping = TRUE, patience = 17L, time_limit = 45.5,
    metric = "mae", retain_oof = FALSE, failure_policy = "stop",
    optimization_policy = "warn", fold_ids = rep(c("a", "b", "c"), 2L)
  )
  lines <- capture.output(print(control))
  expect_match(tuning_print_line(lines, "screening"), "12,340 rows; 2 finalists per family", fixed = TRUE)
  expect_false(grepl("if adaptive", tuning_print_line(lines, "screening"), fixed = TRUE))
  expect_match(tuning_print_line(lines, "early stop"), "enabled for boosting", fixed = TRUE)
  expect_match(tuning_print_line(lines, "patience"), "17 rounds", fixed = TRUE)
  expect_match(tuning_print_line(lines, "time limit"), "45.5 s (search scheduling)", fixed = TRUE)
  expect_true(any(grepl("Active fits finish", lines, fixed = TRUE)))
  expect_true(any(grepl("at least one complete CV candidate is attempted", lines, fixed = TRUE)))
  expect_true(any(grepl("Refits, explanations and reporting are outside the limit", lines, fixed = TRUE)))
  expect_true(any(grepl("total runtime can be longer", lines, fixed = TRUE)))
  expect_match(tuning_print_line(lines, "threads"), "3 per native fit", fixed = TRUE)
  expect_match(tuning_print_line(lines, "metric"), "mae", fixed = TRUE)
  expect_match(tuning_print_line(lines, "OOF rows"), "not retained", fixed = TRUE)
  expect_match(tuning_print_line(lines, "failures"), "stop", fixed = TRUE)
  expect_match(tuning_print_line(lines, "optimizer"), "warn", fixed = TRUE)
  expect_match(tuning_print_line(lines, "fold IDs"), "3 supplied folds", fixed = TRUE)
})

test_that("grid printing distinguishes configured screening and optional boosting calibration", {
  for (stopping in list(NULL, FALSE, TRUE)) {
    control <- tuning_control(
      search = "grid", screening_rows = 250L,
      finalists_per_family = 4L, early_stopping = stopping, patience = 9L
    )
    lines <- capture.output(print(control))
    expect_match(tuning_print_line(lines, "screening"), "unused in grid search", fixed = TRUE)
    expect_match(tuning_print_line(lines, "screening"), "250 rows; 4 finalists per family", fixed = TRUE)
    early <- tuning_print_line(lines, "early stop")
    if (isTRUE(stopping)) {
      expect_match(early, "enabled for boosting", fixed = TRUE)
      expect_false(grepl("unused", tuning_print_line(lines, "patience"), fixed = TRUE))
    } else {
      expect_match(early, if (is.null(stopping)) "automatic for adaptive boosting only" else "disabled", fixed = TRUE)
      expect_match(tuning_print_line(lines, "patience"), "9 rounds without improvement (unused)", fixed = TRUE)
    }
  }
  disabled <- capture.output(print(tuning_control(search = "adaptive", early_stopping = FALSE, patience = 11L)))
  expect_match(tuning_print_line(disabled, "early stop"), "disabled", fixed = TRUE)
  expect_match(tuning_print_line(disabled, "patience"), "11 rounds without improvement (unused)", fixed = TRUE)
})
