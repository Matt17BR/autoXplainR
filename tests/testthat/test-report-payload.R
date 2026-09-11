test_that("embedded JSON preserves user text without ending its script element", {
  value <- list(
    name = '</script><img src=x onerror="alert(1)">',
    values = c("<", ">", "&", "\u2028", "\u2029"), missing = NA_real_
  )
  html <- report_json_script(value, 'data"payload')
  body <- sub("^<script[^>]*>", "", sub("</script>$", "", html))
  expect_false(grepl("<", body, fixed = TRUE))
  expect_equal(jsonlite::fromJSON(body)$name, value$name)
  expect_equal(jsonlite::fromJSON(body)$values, value$values)
  expect_match(html, 'id="data&quot;payload"', fixed = TRUE)
})

test_that("row serialization preserves values, nulls, arrays and mixed raw types", {
  values <- c(pi, 1 + .Machine$double.eps, 1e-200, 2^53 - 1)
  records <- lapply(seq_along(values), function(i) {
    list(
      row_key = paste0("data:", i), source_row = i,
      processed_position = if (i == 2L) NA_integer_ else i, retained = i != 2L,
      nonfinite = list(raw = if (i == 1L) c("x", "y") else character(), processed = "x"),
      raw = list(x = values[i], absent = NULL, mixed = if (i < 3L) i else as.character(i)),
      processed = list(x = if (i == 2L) NULL else values[i], absent = NULL)
    )
  })
  export <- list(mode = "rows", rows = records, manifest = list(individual_records = 4L))
  original <- serialize(export, NULL)
  encoded <- report_json_script(report_data_payload(export), "fixture")
  body <- sub("^<script[^>]*>", "", sub("</script>$", "", encoded))
  decoded <- jsonlite::fromJSON(body, simplifyVector = FALSE)
  expected_json <- jsonlite::toJSON(export,
    auto_unbox = TRUE, null = "null", na = "null", digits = 16
  )
  expected <- jsonlite::fromJSON(expected_json, simplifyVector = FALSE)
  expect_identical(decoded, expected)
  expect_identical(vapply(decoded$rows, function(row) row$raw$x, numeric(1)), values)
  expect_identical(decoded$rows[[1L]]$raw$mixed, 1L)
  expect_identical(decoded$rows[[3L]]$raw$mixed, "3")
  expect_null(decoded$rows[[2L]]$processed_position)
  expect_null(decoded$rows[[2L]]$processed$x)
  expect_identical(decoded$rows[[1L]]$nonfinite$raw, list("x", "y"))
  expect_identical(decoded$rows[[2L]]$nonfinite$raw, list())
  expect_identical(serialize(export, NULL), original)
  for (rows in list(records[1L], list(), lapply(records, function(row) {
    row["raw"] <- list(NULL)
    row
  }))) {
    export$rows <- rows
    before <- report_json_script(export, "fixture")
    after <- report_json_script(report_data_payload(export), "fixture")
    expect_identical(after, before)
  }
  export$rows <- list(
    list(raw = list(label = factor("a"), wrapped = I(1))),
    list(raw = list(label = factor("b"), wrapped = I(2)))
  )
  expect_identical(report_json_script(report_data_payload(export), "fixture"),
                   report_json_script(export, "fixture"))
})

test_that("column serialization does not weaken HTML escaping or rename data fields", {
  text <- '</script><img src=x onerror="alert(1)"> & \u2028 \u2029 \\ "\n'
  fields <- c(text, "two words", "a.b")
  record <- list(raw = setNames(list(text, "<", ">"), fields), processed = NULL)
  export <- list(mode = "rows", rows = list(record, record))
  html <- report_json_script(report_data_payload(export), "fixture")
  body <- sub("^<script[^>]*>", "", sub("</script>$", "", html))
  expect_false(grepl("<|>|&|\u2028|\u2029", body))
  decoded <- jsonlite::fromJSON(body, simplifyVector = FALSE)
  expect_identical(names(decoded$rows[[1L]]$raw), fields)
  expect_identical(decoded$rows[[1L]]$raw[[1L]], text)
  expect_null(decoded$rows[[1L]]$processed)
})
