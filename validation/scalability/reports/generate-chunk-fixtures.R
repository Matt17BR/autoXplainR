# Text-transport boundaries, independent of compression and model fitting.
pkgload::load_all(".", quiet = TRUE)
out <- file.path(Sys.getenv("AXR_SCALE_OUTPUT", path.expand("~/.cache/autoxplain-scale-0.7.0/reports")), "chunks")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
value <- list(text = 'café你好🧪\\"</script><script>window.pwned=true</script>&\u2028\u2029',
  number = 1.2345678901234567)
plain <- AutoXplainR:::report_json_script(value, "original")
json <- sub("^<script[^>]*>", "", sub("</script>$", "", plain))
html <- '<!doctype html><meta charset="utf-8"><h1>Static evidence remains</h1>'
for (size in c(1L, 7L, 13L, 32L)) {
  html <- paste0(html, AutoXplainR:::report_json_chunks(json, paste0("edge-", size), size))
}
for (kind in c("owner", "order", "missing")) {
  altered <- AutoXplainR:::report_json_chunks(json, kind, 13L)
  altered <- switch(kind,
    owner = sub('data-json-owner="owner"', 'data-json-owner="different"', altered, fixed = TRUE),
    order = sub('data-json-chunk="1"', 'data-json-chunk="2"', altered, fixed = TRUE),
    missing = sub('<script type="application/octet-stream"[^>]*>[^<]*</script>', "", altered, perl = TRUE)
  )
  html <- paste0(html, altered)
}
writeLines(html, file.path(out, "boundary.html"), useBytes = TRUE)
writeLines(json, file.path(out, "source.json"), useBytes = TRUE)
