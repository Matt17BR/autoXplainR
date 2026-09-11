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

# jsonlite encodes data-frame columns together instead of dispatching once per
# scalar. Nested frames retain the same row objects in JSON. Only the temporary
# serialization copy changes; callers keep the original list of row records.
report_data_payload <- function(export) {
  if (identical(export$mode, "rows") && length(export$rows)) {
    export$rows <- report_row_records(export$rows)
  }
  export
}

report_row_records <- function(records) {
  fields <- names(records[[1L]])
  same_fields <- vapply(records, function(record) identical(names(record), fields), logical(1))
  if (is.null(fields) || anyNA(fields) || any(!nzchar(fields)) || anyDuplicated(fields) || !all(same_fields)) {
    return(records)
  }
  columns <- lapply(fields, function(field) {
    values <- lapply(records, `[[`, field)
    present <- !vapply(values, is.null, logical(1))
    if (!any(present)) return(rep(NA, length(values)))
    observed <- values[present]
    if (all(present) && all(vapply(observed, is.list, logical(1)))) {
      return(report_row_records(values))
    }
    if (!all(lengths(observed) == 1L) || any(vapply(observed, is.object, logical(1)))) return(values)
    kinds <- unique(vapply(observed, typeof, character(1)))
    missing <- if (all(kinds %in% c("integer", "double"))) {
      NA_real_
    } else if (identical(kinds, "character")) {
      NA_character_
    } else if (identical(kinds, "logical")) {
      NA
    } else {
      # Raw training and evaluation values can have different types. Keep such
      # columns as lists so numbers are not silently converted to strings.
      return(values)
    }
    vapply(values, function(value) if (is.null(value)) missing else value, missing)
  })
  structure(setNames(columns, fields), class = "data.frame", row.names = seq_along(records))
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
