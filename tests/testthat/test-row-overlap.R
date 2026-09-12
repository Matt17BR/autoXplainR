overlap_oracle <- function(training, evaluation) {
  which(AutoXplainR:::split_row_keys(evaluation) %in% AutoXplainR:::split_row_keys(training))
}

test_that("row overlap preserves numeric bits and primitive type normalization", {
  nan_payload <- readBin(
    as.raw(c(1L, 0L, 0L, 0L, 0L, 0L, 248L, 127L)),
    numeric(), n = 1L, size = 8L, endian = "little"
  )
  values <- c(0, -0, NA_real_, NaN, nan_payload, -NaN, 1, 1 + .Machine$double.eps, Inf, -Inf)
  for (value in values) {
    training <- data.frame(value = value)
    evaluation <- data.frame(value = values)
    expect_identical(AutoXplainR:::split_row_overlap(training, evaluation), overlap_oracle(training, evaluation))
  }
  expect_identical(AutoXplainR:::split_row_overlap(data.frame(x = 0), data.frame(x = -0)), integer())
  expect_identical(AutoXplainR:::split_row_overlap(data.frame(x = NA_real_), data.frame(x = NaN)), integer())
  expect_identical(AutoXplainR:::split_row_overlap(data.frame(x = 1L), data.frame(x = 1)), 1L)
  expect_identical(AutoXplainR:::split_row_overlap(data.frame(x = TRUE), data.frame(x = 1)), integer())
  expect_identical(AutoXplainR:::split_row_overlap(data.frame(x = factor("one")), data.frame(x = "one")), 1L)
})

test_that("row overlap preserves string encodings and cannot confuse separators", {
  utf8 <- enc2utf8("caf\u00e9")
  latin1 <- iconv(utf8, "UTF-8", "latin1")
  native <- utf8
  Encoding(native) <- "unknown"
  bytes <- latin1
  Encoding(bytes) <- "bytes"
  values <- c(utf8, latin1, native, bytes, NA_character_, "NA", "a:b", "a|b", "")
  for (value in values) {
    training <- data.frame(text = value)
    evaluation <- data.frame(text = values)
    expect_identical(AutoXplainR:::split_row_overlap(training, evaluation), overlap_oracle(training, evaluation))
  }
  training <- data.frame(a = c("a|b", "a:b"), b = c("c", "c"))
  evaluation <- data.frame(a = "a", b = "b|c")
  expect_identical(AutoXplainR:::split_row_overlap(training, evaluation), integer())
})

test_that("mixed repeated rows agree with serialized equality in original evaluation order", {
  set.seed(173)
  palette <- c(0, -0, NA, NaN, Inf, -Inf, seq_len(5))
  for (repeat_id in seq_len(20L)) {
    training <- data.frame(
      number = sample(palette, 120, replace = TRUE),
      category = factor(sample(c("a", "b", NA), 120, replace = TRUE)),
      flag = sample(c(TRUE, FALSE, NA), 120, replace = TRUE), integer = sample(1:4, 120, replace = TRUE)
    )
    evaluation <- training[sample(1:120, 40, replace = TRUE), c(4, 2, 1, 3)]
    evaluation$number[seq_len(10)] <- 900 + seq_len(10)
    evaluation$category <- as.character(evaluation$category)
    evaluation$integer <- as.numeric(evaluation$integer)
    expected <- overlap_oracle(training, evaluation[names(training)])
    expect_identical(AutoXplainR:::split_row_overlap(training, evaluation[names(training)]), expected)
    message <- tryCatch({
      AutoXplainR:::check_evaluation_row_overlap(training, evaluation, "error")
      NULL
    }, error = conditionMessage)
    expect_match(message, paste0("Found ", length(expected), " rows"), fixed = TRUE)
    expect_match(message, paste(head(expected, 5L), collapse = ", "), fixed = TRUE)
  }
})

test_that("nonstandard columns retain the legacy full-row equality fallback", {
  shared <- new.env(parent = emptyenv())
  shared$value <- 3
  separate <- new.env(parent = emptyenv())
  separate$value <- 3
  training <- data.frame(x = 1, when = as.Date("2025-01-01"))
  training$first <- I(list(shared))
  training$second <- I(list(shared))
  evaluation <- training
  evaluation$second <- I(list(separate))
  expect_identical(AutoXplainR:::split_row_overlap(training, evaluation), overlap_oracle(training, evaluation))
  expect_identical(AutoXplainR:::split_row_overlap(training, training), 1L)
  expect_identical(AutoXplainR:::split_row_overlap(training[FALSE, ], evaluation), integer())
  expect_identical(AutoXplainR:::split_row_overlap(training, evaluation[FALSE, ]), integer())
  zero_columns <- data.frame(row.names = 1:3)
  expect_identical(AutoXplainR:::split_row_overlap(zero_columns, zero_columns), 1:3)
  expect_silent(AutoXplainR:::check_evaluation_row_overlap(training, training, "ignore"))
})
