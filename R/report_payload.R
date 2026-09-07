# HTML script parsing happens before JSON parsing. Escape script delimiters even
# though application/json is not executable, including user-supplied column names.
report_json_script <- function(payload, id) {
  json <- as.character(jsonlite::toJSON(
    payload, auto_unbox = TRUE, null = "null", na = "null", digits = 16,
    dataframe = "rows", POSIXt = "ISO8601", force = TRUE
  ))
  replacements <- c("<" = "\\u003c", ">" = "\\u003e", "&" = "\\u0026")
  for (character in names(replacements)) {
    json <- gsub(character, replacements[[character]], json, fixed = TRUE)
  }
  json <- gsub("\u2028", "\\u2028", json, fixed = TRUE)
  json <- gsub("\u2029", "\\u2029", json, fixed = TRUE)
  paste0('<script type="application/json" id="', html_escape(id), '">', json, "</script>")
}

render_report_data_manifest <- function(export) {
  manifest <- export$manifest
  mode <- export$mode %||% "summary"
  description <- switch(mode,
    rows = paste0(manifest$individual_records, " individual records are embedded, including outcomes and predictions."),
    none = "Data exploration and individual prediction records are omitted.",
    "This report includes aggregate data summaries and no individual prediction records."
  )
  paste0(
    '<section class="export-manifest" aria-labelledby="export-manifest-title">',
    '<h3 id="export-manifest-title">What this HTML contains</h3><p>', html_escape(description), "</p>",
    if (length(manifest$columns)) paste0(
      "<p>Exported data columns: ", html_escape(paste(manifest$columns, collapse = ", ")), ".</p>"
    ),
    '<p class="muted">Model details and aggregate summaries can still contain sensitive information. ',
    "Filtering or hiding a panel does not remove embedded content.</p>",
    '<pre tabindex="0" role="region" aria-label="Report export command"><code>', html_escape(paste0(
      'render_model_report(result, "report.html", report_data = ', deparse(mode), ")"
    )), "</code></pre></section>"
  )
}
