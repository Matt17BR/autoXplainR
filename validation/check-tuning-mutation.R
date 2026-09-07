# Prove that the imputation test rejects learning from held-out fold rows.
# The mutation is confined to this R process; package source is never modified.
pkgload::load_all(quiet = TRUE)
local({
  title <- "out-of-fold predictions use medians learned from each training partition"
  expressions <- parse("tests/testthat/test-tuning.R")
  selected <- Filter(function(x) {
    is.call(x) && identical(x[[1L]], quote(test_that)) && identical(x[[2L]], title)
  }, as.list(expressions))
  stopifnot(length(selected) == 1L)
  path <- tempfile(fileext = ".R")
  writeLines(deparse(selected[[1L]], width.cutoff = 100L), path)
  run_check <- function() {
    result <- testthat::test_file(path, reporter = "silent")
    frame <- as.data.frame(result)
    colSums(frame[c("failed", "error", "warning", "passed")])
  }
  positive <- run_check()
  stopifnot(positive[["failed"]] == 0, positive[["error"]] == 0, positive[["warning"]] == 0)
  original <- AutoXplainR:::prepare_tuning_fold
  testthat::local_mocked_bindings(prepare_tuning_fold = function(raw_data, target, ...) {
    for (column in setdiff(names(raw_data), target)) {
      if (is.numeric(raw_data[[column]])) {
        raw_data[[column]][is.na(raw_data[[column]])] <- stats::median(raw_data[[column]], na.rm = TRUE)
      }
    }
    original(raw_data = raw_data, target = target, ...)
  }, .package = "AutoXplainR")
  negative <- run_check()
  stopifnot(negative[["failed"]] >= 1, negative[["error"]] == 0, negative[["warning"]] == 0)
  print(rbind(correct = positive, leaked_imputation = negative))
  unlink(path)
})
