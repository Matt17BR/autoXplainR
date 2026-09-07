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
