report_json_script_body <- function(value) {
  sub("^<script[^>]*>", "", sub("</script>$", "", report_json_script(value, "fixture")))
}

read_test_block <- function(block) {
  if (identical(block$encoding, "json")) {
    return(block$value)
  }
  expect_identical(block$encoding, "zlib-json-v1")
  bytes <- memDecompress(jsonlite::base64_dec(block$data), "gzip")
  expect_equal(length(bytes), block$bytes)
  jsonlite::fromJSON(rawToChar(bytes), simplifyVector = FALSE)
}

test_that("compressed blocks preserve hostile Unicode and exact numeric values", {
  text <- rep('</script> & \u2028 \u2029 " \u4f60\u597d', 4000L)
  block <- report_payload_block(text, vector = TRUE)
  expect_identical(block$encoding, "zlib-json-v1")
  expect_identical(unlist(read_test_block(block), use.names = FALSE), text)
  numbers <- c(pi, 1 + .Machine$double.eps, 1e-200, 2^53 - 1)
  expect_identical(unclass(read_test_block(report_payload_block(numbers, vector = TRUE))), numbers)
  for (values in list(1L, character(), list(NULL), list(1L, "1", NULL))) {
    block <- report_payload_block(values, vector = TRUE)
    json <- jsonlite::fromJSON(report_json_script_body(block), simplifyVector = FALSE)
    expect_identical(json$value, jsonlite::fromJSON(
      jsonlite::toJSON(I(values), auto_unbox = TRUE, null = "null"),
      simplifyVector = FALSE
    ))
  }
  mixed <- rep(list(1L, "1", NULL), 4000L)
  compressed <- report_payload_block(mixed, vector = TRUE)
  expect_identical(compressed$encoding, "zlib-json-v1")
  expect_identical(read_test_block(compressed), mixed)
})

test_that("compact row data aliases only identical columns and retains source identities", {
  rows <- lapply(1:3, function(i) {
    list(
      row_key = paste0("original:", c(7L, 2L, 19L)[i]), partition = c("training", "evaluation", "evaluation")[i],
      source = "original", source_row = c(7L, 2L, 19L)[i],
      processed_position = if (i == 2L) NA_integer_ else i, retained = i != 2L,
      raw = list(same = i, changed = if (i == 2L) NULL else i, mixed = if (i == 3L) "3" else i),
      processed = list(same = i, changed = i, mixed = if (i == 3L) "3" else i),
      nonfinite = list(raw = if (i == 2L) "changed" else character(), processed = character())
    )
  })
  original <- serialize(rows, NULL)
  compact <- report_data_payload(list(
    mode = "rows", rows = rows,
    profile = list(target = "same"), manifest = list(individual_records = 3L)
  ), compact = TRUE)
  expect_identical(compact$schema_version, 2L)
  expect_identical(compact$rows$length, 3L)
  expect_identical(
    compact$rows$processed$same,
    list(encoding = "reference", stage = "raw", column = "same")
  )
  expect_identical(
    compact$rows$processed$mixed,
    list(encoding = "reference", stage = "raw", column = "mixed")
  )
  expect_false(identical(compact$rows$processed$changed$encoding, "reference"))
  expect_identical(unclass(compact$rows$nonfinite$raw$changed), 2L)
  decoded <- jsonlite::fromJSON(report_json_script_body(compact), simplifyVector = FALSE)
  expect_identical(decoded$rows$meta$row_key$value, list("original:7", "original:2", "original:19"))
  expect_identical(decoded$rows$raw$mixed$value, list(1L, 2L, "3"))
  expect_identical(decoded$rows$raw$changed$value, list(1L, NULL, 3L))
  expect_identical(decoded$rows$meta$processed_position$value, list(1L, NULL, 3L))
  expect_identical(serialize(rows, NULL), original)
})

test_that("one-row, absent-stage and no-row compact exports keep their shape", {
  row <- list(
    row_key = "evaluation:1", partition = "evaluation", source = "external", source_row = 1L,
    processed_position = 1L, retained = TRUE, raw = NULL, processed = list(x = 3),
    nonfinite = list(raw = character(), processed = character())
  )
  for (rows in list(list(row), list())) {
    value <- report_data_payload(list(mode = "rows", rows = rows, profile = list(), manifest = list()), compact = TRUE)
    decoded <- jsonlite::fromJSON(report_json_script_body(value), simplifyVector = FALSE)
    expect_identical(decoded$rows$length, length(rows))
    expect_null(decoded$rows$raw)
    expect_length(decoded$rows$meta$row_key$value, length(rows))
  }
  summary <- report_data_payload(
    list(mode = "summary", rows = NULL, profile = list(), manifest = list()), compact = TRUE
  )
  expect_null(summary$rows)
})

test_that("AsIs numeric columns keep scalar cells in both compact preparation paths", {
  values <- I(c(1, 2.5, NA_real_))
  records <- lapply(seq_along(values), function(i) {
    list(
      row_key = paste0("evaluation:", i), partition = "evaluation", source = "evaluation",
      source_row = i, processed_position = i, retained = TRUE, raw = list(x = values[i]),
      processed = list(x = values[i]), nonfinite = list(raw = character(), processed = character())
    )
  })
  direct <- report_columns_from_records(records)
  direct$raw$x <- direct$processed$x <- values
  decoded <- lapply(list(records, direct), function(rows) {
    payload <- report_data_payload(list(mode = "rows", rows = rows, profile = list()), compact = TRUE)
    jsonlite::fromJSON(report_json_script_body(payload), simplifyVector = FALSE)$rows$raw$x$value
  })
  expect_identical(decoded[[1L]], list(1L, 2.5, NULL))
  expect_identical(decoded[[2L]], decoded[[1L]])
  long <- report_payload_block(I(rep(c(1, 2.5, NA_real_), 4000)), vector = TRUE)
  expect_identical(long$encoding, "zlib-json-v1")
  expect_identical(read_test_block(long), rep(list(1L, 2.5, NULL), 4000))
})

test_that("wire-only known levels retain exported existing categories without full ID dictionaries", {
  known <- sprintf("person_%05d", 1:20000)
  axis <- list(kind = "categorical", levels = known[1:2], known_levels = known)
  columns <- list(id = list(axis = axis, training = list(n_total = 20000L), evaluation = list(n_total = 3L)))
  profile <- list(stages = list(raw = list(columns = columns), processed = list(columns = columns)))
  rows <- lapply(1:3, function(i) {
    list(
      row_key = paste0("evaluation:", i), partition = "evaluation", source = "evaluation", source_row = i,
      processed_position = i, retained = TRUE, raw = list(id = c(known[100], "new_person", NA)[i]),
      processed = list(id = c(known[100], "new_person", NA)[i]),
      nonfinite = list(raw = character(), processed = character())
    )
  })
  original <- serialize(profile, NULL)
  payload <- report_data_payload(list(mode = "rows", rows = rows, profile = profile), compact = TRUE)
  retained <- read_test_block(payload$profile)
  expect_identical(retained$stages$raw$columns$id$axis$known_levels, known[100])
  expect_identical(retained$stages$processed$columns$id$axis$known_levels, known[100])
  expect_identical(retained$stages$raw$columns$id$axis$levels, known[1:2])
  expect_identical(retained$stages$raw$columns$id$training, columns$id$training)
  summary <- report_data_payload(list(mode = "summary", rows = NULL, profile = profile), compact = TRUE)
  expect_length(read_test_block(summary$profile)$stages$raw$columns$id$axis$known_levels, 0L)
  expect_identical(serialize(profile, NULL), original)
})

test_that("columnar prediction cases preserve regression and classification evidence", {
  records <- lapply(1:4, function(i) {
    list(
      row_key = paste0("test:", c(8, 2, 15, 4)[i]), source = "test", source_row = c(8L, 2L, 15L, 4L)[i],
      partition = if (i == 1L) "training" else "evaluation", retained = i != 3L,
      processed_position = if (i == 3L) NA_integer_ else i, raw = NULL, processed = list(x = i),
      nonfinite = list(raw = character(), processed = character())
    )
  })
  original <- list(.report_export = list(mode = "rows", rows = records))
  compact <- original
  compact$.report_export$rows <- report_columns_from_records(records)
  fixtures <- list(
    list(y = c(1, 5, 8, 3), prediction = c(2, 3, 4, 6), labels = NULL, positive = NULL, task = "regression"),
    list(
      y = factor(c("no", "yes", "no", "no"), levels = c("no", "yes")), prediction = c(.2, .8, .7, .9),
      labels = c("no", "yes"), positive = "yes", task = "binary"
    ),
    list(
      y = factor(c("A", "B", "C", "A")),
      prediction = matrix(c(.8, .1, .1, .2, .7, .1, .1, .2, .7, .1, .8, .1),
        nrow = 4, byrow = TRUE,
        dimnames = list(NULL, c("A", "B", "C"))
      ), labels = c("A", "B", "C"), positive = NULL, task = "multiclass"
    )
  )
  for (fixture in fixtures) {
    before <- prediction_exported_cases(original, fixture$y, fixture$prediction, fixture$labels, fixture$positive)
    after <- prediction_exported_cases(compact, fixture$y, fixture$prediction, fixture$labels, fixture$positive)
    expect_s3_class(after, "data.frame")
    expect_identical(report_json_script_body(after), report_json_script_body(before))
    expect_identical(prediction_case_table(after, fixture$task), prediction_case_table(before, fixture$task))
    expect_identical(after$row_key, c("test:2", "test:4"))
    expect_equal(after$processed_position, c(2L, 4L))
  }
})
