test_that("small JSON scripts keep their exact representation", {
  expect_identical(
    report_json_chunks('{"x":1}', 'quoted"id'),
    '<script type="application/json" id="quoted&quot;id">{"x":1}</script>'
  )
  expect_identical(
    report_json_chunks("1234", "edge", chunk_size = 4L),
    '<script type="application/json" id="edge">1234</script>'
  )
})

test_that("large escaped JSON fragments reconstruct the original text exactly", {
  value <- list(
    text = 'café你好🧪\\"</script><script>window.pwned=true</script>&\u2028\u2029',
    number = 1.2345678901234567
  )
  original <- report_json_script(value, "fixture")
  json <- sub("^<script[^>]*>", "", sub("</script>$", "", original))
  for (size in c(1L, 7L, 13L, 32L)) {
    html <- report_json_chunks(json, 'quoted"id', size)
    fragments <- regmatches(html, gregexpr(
      '<script type="application/octet-stream"[^>]*>[^<]*</script>', html,
      perl = TRUE
    ))[[1L]]
    texts <- sub("^<script[^>]*>", "", sub("</script>$", "", fragments))
    expect_identical(paste0(texts, collapse = ""), json)
    expect_true(all(nchar(texts, type = "chars") <= size))
    expect_identical(jsonlite::fromJSON(paste0(texts, collapse = "")), value)
    expect_false(grepl("<script>window.pwned", html, fixed = TRUE))
    expect_true(grepl('data-json-owner="quoted&quot;id"', html, fixed = TRUE))
  }
})
