data_profile_table <- function(profile, stage, name) {
  column <- profile$stages[[stage]]$columns[[name]]
  if (!identical(column$axis$status, "available")) {
    return(paste0("<p>", html_escape(column$axis$reason %||% "No usable values."), "</p>"))
  }
  counts <- function(partition) {
    value <- column[[partition]]$counts
    if (is.null(value)) rep(NA_real_, length(column$axis$labels)) else value
  }
  rows <- data.frame(
    Value = column$axis$labels, Training = counts("training"),
    Evaluation = counts("evaluation"), check.names = FALSE
  )
  html_table(rows, 0L, caption = paste(name, "\u2014", profile$stages[[stage]]$population))
}

data_profile_inventory <- function(profile) {
  stage <- if ("raw" %in% names(profile$stages)) "raw" else "processed"
  rows <- lapply(profile$columns$name, function(name) {
    column <- profile$stages[[stage]]$columns[[name]]
    data.frame(
      Column = name, Role = profile$columns$role[match(name, profile$columns$name)],
      Type = column$axis$kind, `Training missing` = column$training$n_missing %||% NA_integer_,
      `Evaluation missing` = column$evaluation$n_missing %||% NA_integer_,
      `Training distinct` = column$training$n_unique %||% NA_integer_,
      check.names = FALSE
    )
  })
  html_table(do.call(rbind, rows), 0L,
    caption = if (stage == "raw") "All supplied rows, before preprocessing" else "Processed retained rows only"
  )
}

explorer_data <- function(result, export) {
  if (is.null(export) || identical(export$mode, "none")) {
    return("")
  }
  profile <- export$profile
  manifest <- export$manifest
  target <- profile$target
  variables <- profile$columns$name
  initial_stage <- if ("raw" %in% names(profile$stages)) "raw" else "processed"
  initial_y <- if (length(variables) > 1L) variables[variables != target][1L] else target
  column_buttons <- paste(vapply(seq_along(variables), function(index) {
    name <- variables[index]
    info <- profile$stages[[initial_stage]]$columns[[name]]
    missing <- (info$training$n_missing %||% 0L) + (info$evaluation$n_missing %||% 0L)
    paste0(
      '<button type="button" class="data-column" data-column-name="', html_escape(name),
      '" aria-pressed="', if (name == target) "true" else "false", '"><strong>', html_escape(name),
      "</strong><span>", html_escape(profile$columns$role[index]), " \u00b7 ", html_escape(info$axis$kind),
      if (missing > 0) paste0(" \u00b7 ", missing, " missing") else "", "</span></button>"
    )
  }, character(1)), collapse = "")
  modes <- names(profile$stages)
  mode_labels <- c(raw = "Raw supplied values", processed = "Values used by models")
  training_available <- isTRUE(profile$stages[[initial_stage]]$training_available)
  disclosure <- if (export$mode == "rows") {
    paste0(
      "This HTML includes ", manifest$individual_records, " individual records across ",
      length(manifest$columns), " columns, including rows hidden by filters."
    )
  } else {
    paste0(
      "This HTML includes aggregate data summaries for ", length(manifest$columns),
      " columns. No individual records are exported by the data explorer."
    )
  }
  notice <- if (export$mode == "rows") {
    paste(manifest$individual_records, "individual records embedded")
  } else {
    paste("Aggregate summaries for", length(manifest$columns), "columns")
  }
  fallback <- paste(vapply(modes, function(stage) {
    paste0(
      "<h3>", html_escape(mode_labels[stage]), "</h3>",
      paste(vapply(variables, function(name) {
        paste0(
          "<details><summary>", html_escape(name), "</summary>",
          data_profile_table(profile, stage, name), "</details>"
        )
      }, character(1)), collapse = "")
    )
  }, character(1)), collapse = "")
  paste0(
    '<section id="data" class="workspace-page data-explorer" data-page="data" aria-labelledby="data-title">',
    '<p class="section-number">Data</p><h2 id="data-title">Explore the data</h2>',
    '<p class="data-export-note">', html_escape(notice),
    ' \u00b7 <a href="#data-export-details">Export details</a></p>',
    '<div class="data-controls"><label>Values <select id="data-stage">',
    explorer_options(modes, unname(mode_labels[modes]), initial_stage), "</select></label>",
    '<label>Compare <select id="data-split">',
    if (training_available) {
      '<option value="both">Training and evaluation</option><option value="training">Training only</option>'
    },
    '<option value="evaluation">Evaluation only</option></select></label>',
    '<label id="data-scale-control">Bars <select id="data-scale">',
    '<option value="percent">Percent within each split</option>',
    '<option value="count">Number of rows</option></select></label></div>',
    if (!training_available) {
      '<p class="data-chart-note">Training data was not supplied; no training comparison is available.</p>'
    },
    '<p id="data-population" class="data-population" role="status">', html_escape(manifest$scope), "</p>",
    "<noscript><style>#data .data-controls,#data .data-workspace{display:none!important}</style>",
    '<div class="data-static"><h3>Distribution tables</h3>',
    "<p>Open a column below to inspect its counts. Interactive pair plots, filters and ",
    "individual-record inspection require JavaScript.</p>", fallback, "</div></noscript>",
    '<div class="data-workspace"><aside class="data-column-list" aria-label="Dataset columns">',
    '<label for="data-search">Find a column</label><input id="data-search" type="search" placeholder="Column name">',
    '<div id="data-columns">', column_buttons, '</div></aside><div class="data-main">',
    '<div class="data-section-heading"><h3 id="data-variable-title">', html_escape(target), "</h3>",
    '<span id="data-variable-role">Outcome</span></div>',
    '<div id="data-summary" class="data-summary"></div>',
    '<div class="data-view-controls" role="group" aria-label="Data workspace">',
    '<button type="button" data-data-view="distribution" aria-pressed="true" ',
    'aria-controls="data-distribution-panel">Distribution</button>',
    '<button type="button" data-data-view="relationships" aria-pressed="false" ',
    'aria-controls="relationships">Relationships</button>',
    '<button type="button" data-data-view="records" aria-pressed="false" ',
    'aria-controls="data-records-panel">Records</button></div>',
    '<div id="data-pair-control" class="data-pair-control" hidden><label>Compare with <select id="data-y">',
    explorer_options(variables, variables, initial_y), "</select></label></div>",
    '<div id="data-distribution-panel" data-data-panel="distribution">',
    '<div id="data-distribution" class="data-chart" tabindex="0" role="region" aria-label="Column distribution"></div>',
    '<p id="data-distribution-note" class="data-chart-note"></p>',
    '<details class="data-values"><summary>Distribution counts and missing values</summary>',
    '<div id="data-distribution-table">', data_profile_table(profile, initial_stage, target), "</div></details></div>",
    '<div id="relationships" class="data-relationships" data-data-panel="relationships" hidden>',
    '<p class="data-chart-note">Joint counts show where observations lie. ',
    "Binned outcome averages describe these rows, not fitted or causal effects.</p>",
    '<div id="data-association" class="data-association"></div>',
    '<div id="data-conditional" class="data-conditional"></div>',
    '<div id="data-pair" class="data-chart" tabindex="0" role="region" aria-label="Joint data distribution"></div>',
    '<p id="data-pair-note" class="data-chart-note" role="status"></p>',
    '<details><summary>Pair counts and sample sizes</summary><div id="data-pair-table"></div></details></div>',
    '<div id="data-records-panel" data-data-panel="records" hidden>',
    if (export$mode == "rows") {
      data_row_controls(variables)
    } else {
      paste0(
        '<div class="data-row-explanation"><h3>Inspect individual records</h3>',
        "<p>This report contains aggregate summaries. ",
        "For linked scatter points, row inspection and arbitrary filters, ",
        'create a report with <code>report_data = "rows"</code>. This explicitly embeds individual records.</p></div>'
      )
    },
    '</div></div></div><details class="data-ledger"><summary>Missing values, column types and preprocessing</summary>',
    data_profile_inventory(profile),
    if (!is.null(profile$row_ledger)) {
      html_table(profile$row_ledger, 0L,
        caption = "Supplied and retained rows; exclusions include preprocessing and validation design"
      )
    },
    "<p>Raw and processed views can have different row counts. Category mapping and imputation change values; ",
    "the processed view contains only retained model rows.</p></details>",
    '<details id="data-export-details"><summary>What this file contains</summary><p>',
    html_escape(disclosure), " ", html_escape(manifest$privacy), "</p>",
    "<p>Columns: ", html_escape(paste(manifest$columns, collapse = ", ")), "</p><p>",
    html_escape(manifest$scope), "</p><p>", html_escape(manifest$sampling),
    if (!is.null(manifest$seed)) paste0("; seed ", manifest$seed), "</p>",
    "<p>Aggregate profiles use all available rows. Pair summaries: ", profile$pair_coverage$included,
    " of ", profile$pair_coverage$total, ". ", html_escape(profile$pair_coverage$policy), "</p></details>",
    report_json_script(export, "axr-data-payload"), "</section>"
  )
}

data_row_controls <- function(variables) {
  paste0(
    '<div class="data-row-workspace"><h3>Inspect and filter exported rows</h3>',
    '<p id="data-sample-note" class="data-chart-note"></p>',
    '<form id="data-filter-form" class="data-filter-form"><label>Column <select id="data-filter-column">',
    explorer_options(variables), '</select></label><label>Condition <select id="data-filter-op">',
    '<option value="eq">Equals</option><option value="contains">Contains text</option>',
    '<option value="in">One of these categories</option>',
    '<option value="ge">At least</option><option value="le">At most</option>',
    '<option value="missing">Is missing</option><option value="nonfinite">Is non-finite</option>',
    '<option value="present">Has a usable value</option>',
    '</select></label><label>Value <input id="data-filter-value" type="text"></label>',
    '<label id="data-filter-level-label" hidden>Category values ',
    '<select id="data-filter-levels" multiple size="4"></select></label>',
    '<button type="submit">Add filter</button>',
    '<button type="button" id="data-filter-reset">Reset filters</button></form>',
    '<div id="data-filter-chips" class="data-filter-chips" role="group" aria-label="Active filters"></div>',
    '<p id="data-filter-status" role="status"></p>',
    '<details id="data-scatter-details"><summary>Linked scatter of exported records</summary>',
    '<div id="data-row-scatter" class="data-chart" tabindex="0" role="region" ',
    'aria-label="Exported row scatter"></div></details>',
    '<div class="data-row-toolbar"><label>Sort <select id="data-row-sort">',
    '<option value="source_row">Source position</option>', explorer_options(variables),
    '</select></label><button id="data-row-direction" type="button">Ascending</button>',
    '<button id="data-row-prev" type="button">Previous rows</button>',
    '<span id="data-row-page"></span><button id="data-row-next" type="button">Next rows</button></div>',
    '<div id="data-row-table" class="table-wrap" tabindex="0" role="region" aria-label="Exported records"></div>',
    '<div id="data-selected-row" class="data-selected-row" role="status" tabindex="0">',
    "Select a table row or plotted point to inspect it.</div>",
    '<p class="data-chart-note">Filters describe the exported sample. ',
    "They do not refit models or change official scores. ",
    "Subgroups chosen after looking at outcomes are exploratory.</p></div>"
  )
}
