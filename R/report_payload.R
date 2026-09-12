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
report_data_payload <- function(export, compact = FALSE) {
  if (isTRUE(compact)) return(report_compact_data_payload(export))
  if (identical(export$mode, "rows") && length(export$rows)) {
    export$rows <- report_row_records(export$rows)
  }
  export
}

# Keep large vectors separate so the browser can open one column without
# allocating the rest of the dataset. Small blocks remain readable JSON.
report_payload_block <- function(value, vector = FALSE) {
  if (isTRUE(vector)) {
    # AsIs protects an R column, not an additional array dimension per cell.
    # Keeping scalar AsIs wrappers produced [value] cells that could not be
    # plotted or numerically filtered by the report.
    if (inherits(value, "AsIs")) value <- unclass(value)
    if (is.list(value) && !is.data.frame(value)) {
      value <- lapply(value, function(cell) if (inherits(cell, "AsIs") && length(cell) == 1L) unclass(cell) else cell)
    }
  }
  json <- as.character(jsonlite::toJSON(
    if (vector) I(value) else value, auto_unbox = TRUE, null = "null", na = "null", digits = 16,
    dataframe = "rows", POSIXt = "ISO8601", force = TRUE
  ))
  bytes <- nchar(json, type = "bytes")
  if (bytes >= 16384L) {
    compressed <- jsonlite::base64_enc(memCompress(charToRaw(enc2utf8(json)), "gzip"))
    if (nchar(compressed, type = "bytes") < bytes * 0.9) {
      # R calls this gzip, but memCompress emits a zlib-wrapped DEFLATE stream.
      return(list(encoding = "zlib-json-v1", bytes = bytes, data = compressed))
    }
  }
  list(encoding = "json", value = if (vector) I(value) else value)
}

report_columns_from_records <- function(records) {
  if (identical(records$layout, "columns-v1")) return(records)
  n <- length(records)
  metadata <- c("row_key", "partition", "source", "source_row", "processed_position", "retained")
  get_column <- function(field, stage = NULL) {
    values <- lapply(records, function(row) if (is.null(stage)) row[[field]] else row[[stage]][[field]])
    # Reuse the type-preserving transpose for mixed raw/processed data.
    if (!length(values)) return(list())
    report_row_records(lapply(values, function(value) list(value = value)))$value
  }
  values <- lapply(c("raw", "processed"), function(stage) {
    fields <- unique(unlist(lapply(records, function(row) names(row[[stage]])), use.names = FALSE))
    if (!length(fields)) return(NULL)
    setNames(lapply(fields, get_column, stage = stage), fields)
  })
  names(values) <- c("raw", "processed")
  nonfinite <- lapply(c("raw", "processed"), function(stage) {
    fields <- names(values[[stage]])
    setNames(lapply(fields, function(field) {
      which(vapply(records, function(row) field %in% row$nonfinite[[stage]], logical(1)))
    }), fields)
  })
  names(nonfinite) <- c("raw", "processed")
  list(
    layout = "columns-v1", length = n,
    meta = setNames(lapply(metadata, get_column), metadata),
    raw = values$raw, processed = values$processed, nonfinite = nonfinite
  )
}

report_compact_data_payload <- function(export) {
  rows <- NULL
  columns <- NULL
  if (identical(export$mode, "rows")) {
    columns <- report_columns_from_records(export$rows)
    values <- lapply(c("raw", "processed"), function(stage) {
      if (is.null(columns[[stage]])) return(NULL)
      setNames(lapply(names(columns[[stage]]), function(name) {
        value <- columns[[stage]][[name]]
        if (stage == "processed" && name %in% names(columns$raw) && identical(value, columns$raw[[name]])) {
          return(list(encoding = "reference", stage = "raw", column = name))
        }
        report_payload_block(value, vector = TRUE)
      }), names(columns[[stage]]))
    })
    names(values) <- c("raw", "processed")
    rows <- list(
      layout = "columns-v1", length = columns$length,
      meta = lapply(columns$meta, report_payload_block, vector = TRUE),
      raw = values$raw, processed = values$processed,
      nonfinite = lapply(columns$nonfinite, function(stage) lapply(stage, function(indices) I(indices)))
    )
  }
  profile <- export$profile
  for (stage in names(profile$stages)) {
    for (name in names(profile$stages[[stage]]$columns)) {
      axis <- profile$stages[[stage]]$columns[[name]]$axis
      if (is.null(axis$known_levels)) next
      # Known levels are used only to classify exported row values as existing
      # or novel. Counts and displayed bins already contain full-data results.
      used <- if (!is.null(columns[[stage]][[name]])) {
        unique(as.character(unlist(columns[[stage]][[name]], use.names = FALSE)))
      } else {
        character()
      }
      axis$known_levels <- intersect(axis$known_levels, used)
      profile$stages[[stage]]$columns[[name]]$axis <- axis
    }
  }
  list(
    schema_version = 2L, mode = export$mode,
    profile = report_payload_block(profile), rows = rows, manifest = export$manifest
  )
}

report_predictions_payload <- function(view) {
  view$schema_version <- 2L
  view$models <- lapply(view$models, function(model) {
    if (is.null(model$cases)) return(model)
    columns <- if (is.data.frame(model$cases)) model$cases else report_row_records(model$cases)
    model$cases <- list(
      layout = "case-columns-v1", length = nrow(columns) %||% 0L,
      columns = lapply(columns, function(column) report_payload_block(column, vector = !is.data.frame(column)))
    )
    model
  })
  view
}

report_row_records <- function(records) {
  if (!length(records)) return(structure(list(), class = "data.frame", row.names = integer()))
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
