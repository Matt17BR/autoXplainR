# Responsive charts carry retained values, never browser-side statistical estimates.
# The fallback SVG and exact-value table work without JavaScript. charts.js only
# changes geometry and visible model series as the report controls change.

report_model_color <- function(model_id, result = NULL) {
  palette <- c("#17654e", "#285da8", "#9b4d24", "#76518d", "#85631e", "#a13e61", "#227482", "#555b65")
  ids <- names(result$models %||% list())
  index <- match(model_id, ids)
  if (is.na(index)) {
    bytes <- utf8ToInt(as.character(model_id %||% "model"))
    index <- if (length(bytes)) sum(bytes * seq_along(bytes)) %% length(palette) + 1L else 1L
  }
  if (index > length(palette)) {
    return(grDevices::hcl(h = index * 137.508 %% 360, c = 48, l = 40))
  }
  palette[index]
}

report_chart_attrs <- function(values) {
  paste(vapply(names(values), function(name) {
    value <- values[[name]]
    if (is.null(value) || !length(value) || is.na(value[1L])) {
      return("")
    }
    if (is.numeric(value)) value <- format(value, digits = 17L, scientific = FALSE, trim = TRUE)
    paste0(" data-", name, '="', html_escape(as.character(value)), '"')
  }, character(1)), collapse = "")
}

report_chart_label <- function(result, model_id, fallback = model_id) {
  if (!is.null(result) && model_id %in% names(result$models)) {
    return(explorer_label(result, model_id))
  }
  as.character(fallback)
}

report_chart_range <- function(values, zero = FALSE) {
  finite <- values[is.finite(values)]
  if (zero) finite <- c(0, finite)
  if (!length(finite)) {
    return(c(0, 1))
  }
  limits <- range(finite)
  span <- diff(limits)
  if (span == 0) span <- max(abs(limits[1]) * .2, .1)
  limits + c(-1, 1) * span * .09
}

report_chart_source <- function(point) {
  paste0("<span hidden data-chart-source", report_chart_attrs(point), "></span>")
}

report_chart_table <- function(rows, caption, method_note = NULL) {
  paste0(
    '<details class="axr-chart-values"><summary>Chart values and support</summary>',
    html_table(rows, digits = 5L, caption = paste0(caption, " (rounded for display)")),
    if (!is.null(method_note)) paste0("<p>", html_escape(method_note), "</p>"), "</details>"
  )
}

report_chart_frame <- function(kind, points, x_label, y_label, caption, table,
                               note = "", primary_model = NULL, zero = FALSE,
                               interval_note = NULL, reference = NULL,
                               x_limits = NULL, y_limits = NULL, short_note = NULL) {
  for (limits in list(x_limits, y_limits)) {
    if (is.null(limits)) next
    valid <- is.numeric(limits) && length(limits) == 2L &&
      all(is.finite(limits)) && limits[1] < limits[2]
    if (!valid) {
      stop("Chart limits must be two increasing finite values.", call. = FALSE)
    }
  }
  attrs <- list(
    kind = kind, `x-label` = x_label, `y-label` = y_label,
    `primary-model` = primary_model, zero = if (zero) "true" else "false", reference = reference,
    `x-min` = x_limits[1], `x-max` = x_limits[2], `y-min` = y_limits[1], `y-max` = y_limits[2]
  )
  fallback_points <- if (!is.null(primary_model)) {
    Filter(function(point) identical(point$model, primary_model), points)
  } else {
    points
  }
  paste0(
    '<figure class="axr-chart"', report_chart_attrs(attrs), "><figcaption>", html_escape(caption),
    '</figcaption><div class="axr-chart-viewport">',
    report_chart_fallback(kind, fallback_points, x_label, y_label, zero, reference, x_limits, y_limits), "</div>",
    paste(vapply(points, report_chart_source, character(1)), collapse = ""),
    '<p class="axr-chart-detail" aria-live="polite">',
    "Values and support are available in the following table.</p>",
    if (!is.null(short_note) && nzchar(short_note)) {
      paste0('<p class="axr-chart-note">', html_escape(short_note), "</p>")
    },
    if (nzchar(note)) {
      paste0(
        '<details class="axr-chart-guidance"><summary>Reading the chart</summary><p>',
        html_escape(note), "</p></details>"
      )
    },
    if (!is.null(interval_note) && nzchar(interval_note)) {
      paste0('<p class="axr-chart-note">', html_escape(interval_note), "</p>")
    },
    table, "</figure>"
  )
}

tradeoff_chart <- function(tradeoffs, result = NULL) {
  metric <- attr(tradeoffs, "performance_metric")
  resource <- attr(tradeoffs, "complexity_metric")
  if (!length(metric) || !length(resource) || !nrow(tradeoffs)) {
    return(render_diagnostic_state("Resource comparison", "not_run", "No finite comparison was retained."))
  }
  # Stable IDs determine order and colors; frontier sorting never renumbers a model.
  ids <- names(result$models %||% list())
  if (length(ids)) tradeoffs <- tradeoffs[order(match(tradeoffs$model_id, ids)), , drop = FALSE]
  labels <- vapply(seq_len(nrow(tradeoffs)), function(i) {
    report_chart_label(result, tradeoffs$model_id[i], tradeoffs$model[i])
  }, character(1))
  x_label <- switch(resource,
    training_time_ms = "Fit time (ms)",
    prediction_time_ms = "Batch prediction time (ms)",
    model_size_kb = "R object size (KiB)",
    pretty_complexity(resource)
  )
  unit <- if (metric %in% c("rmse", "mae")) result$provenance$target_units else NULL
  y_label <- paste0(pretty_metric(metric), if (!is.null(unit)) paste0(" (", unit, ")"))
  points <- lapply(seq_len(nrow(tradeoffs)), function(i) {
    list(
      x = tradeoffs[[resource]][i], y = tradeoffs[[metric]][i], model = tradeoffs$model_id[i],
      label = labels[i], color = report_model_color(tradeoffs$model_id[i], result),
      frontier = if (isTRUE(tradeoffs$pareto_optimal[i])) "true" else "false",
      detail = paste0(
        labels[i], "; ", y_label, ": ", report_axis_number(tradeoffs[[metric]][i]),
        "; ", x_label, ": ", report_axis_number(tradeoffs[[resource]][i]),
        if (isTRUE(tradeoffs$pareto_optimal[i])) "; not dominated on these two measurements" else ""
      )
    )
  })
  rows <- data.frame(
    Model = labels, Score = tradeoffs[[metric]], Resource = tradeoffs[[resource]],
    `Not dominated` = tradeoffs$pareto_optimal, check.names = FALSE
  )
  names(rows)[2:3] <- c(y_label, x_label)
  direction <- if (isTRUE(attr(tradeoffs, "higher_is_better"))) "upper left" else "lower left"
  note <- paste0(
    "The ", direction, " combines a better score with less resource use. ",
    "Outlines mark nondominated measurements, not a recommended model."
  )
  if (grepl("time", resource)) {
    note <- paste(note, "Times describe this machine and batch; small differences may reflect timer resolution.")
  }
  report_chart_frame(
    "cost", points, x_label, y_label, "Predictive score and measured resource use",
    report_chart_table(rows, "Retained model scores and resource measurements"), note,
    short_note = if (isTRUE(attr(tradeoffs, "higher_is_better"))) {
      "The upper left combines a higher score with less resource use."
    } else {
      "The lower left combines a lower loss with less resource use."
    }
  )
}

effect_chart <- function(effect, feature, result = NULL, model_id = NULL, comparison = NULL) {
  if (!is.data.frame(effect) || !nrow(effect)) {
    return(render_diagnostic_state("Fitted effect", "not_run", "No effect values were retained."))
  }
  model_id <- model_id %||% result$provenance$primary_model_id %||% "model"
  effects <- c(stats::setNames(list(effect), model_id), comparison %||% list())
  effects <- effects[!duplicated(names(effects))]
  effects <- Filter(function(item) is.data.frame(item) && nrow(item) > 0L, effects)
  method <- attr(effect, "method") %||% "pdp"
  primary_y <- effect[[if (method == "ale") "accumulated_effect" else "partial_dependence"]]
  if (!length(primary_y) || any(!is.finite(primary_y))) {
    return(render_diagnostic_state("Fitted effect", "failed", "No finite primary curve was retained."))
  }
  numeric_x <- is.numeric(effect[[1]])
  target <- attr(effect, "prediction_target") %||% result$target_column %||% "fitted prediction"
  units <- result$provenance$target_units %||%
    if (identical(result$task, "regression")) "target units" else if (!is.null(result$task)) "probability" else NULL
  y_label <- paste0(
    if (method == "ale") "Centered effect" else "Average prediction",
    if (!is.null(units)) paste0(" (", units, ")")
  )
  series <- lapply(names(effects), function(id) {
    item <- effects[[id]]
    if (!identical(is.numeric(item[[1]]), numeric_x) || !identical(attr(item, "method") %||% "pdp", method)) {
      stop("Compared effects must use the same input type and method.", call. = FALSE)
    }
    if (!identical(attr(item, "prediction_class"), attr(effect, "prediction_class"))) {
      stop("Compared effects must describe the same prediction class.", call. = FALSE)
    }
    y <- item[[if (method == "ale") "accumulated_effect" else "partial_dependence"]]
    if (!length(y) || any(!is.finite(y))) {
      return(list())
    }
    label <- report_chart_label(result, id)
    lapply(seq_along(y), function(i) {
      n <- if ("n" %in% names(item)) item$n[i] else NA_real_
      support <- if ("support" %in% names(item)) item$support[i] else NA_real_
      low <- if ("conf_low" %in% names(item)) item$conf_low[i] else NA_real_
      high <- if ("conf_high" %in% names(item)) item$conf_high[i] else NA_real_
      input <- as.character(item[[1]][i])
      list(
        x = if (numeric_x) item[[1]][i] else i, y = y[i], low = low, high = high,
        category = if (!numeric_x) input else NULL, model = id, label = label,
        color = report_model_color(id, result), n = n, support = support,
        `bin-left` = if (method == "ale" && i > 1L) item[[1]][i - 1L] else NA_real_,
        detail = paste0(
          label, "; ", feature, " = ", input, "; ", y_label, ": ", report_axis_number(y[i]),
          if (is.finite(low) && is.finite(high)) {
            paste0("; descriptive band [", report_axis_number(low), ", ", report_axis_number(high), "]")
          },
          if (is.finite(n)) {
            paste0("; bin rows: ", n)
          } else if (is.finite(support)) {
            paste0("; relative support: ", report_axis_number(support))
          }
        )
      )
    })
  })
  points <- unlist(series, recursive = FALSE)
  if (!length(points)) {
    return(render_diagnostic_state("Fitted effect", "failed", "No finite curve was retained."))
  }
  rows <- do.call(rbind, lapply(names(effects), function(id) {
    item <- as.data.frame(effects[[id]])
    data.frame(Model = report_chart_label(result, id), item, check.names = FALSE)
  }))
  note <- paste0(toupper(method), " describes ", target, ". Fitted associations do not establish intervention effects.")
  population <- if (is.null(result)) "reference" else "evaluation reference"
  if (method == "ale" && any(vapply(points, function(point) is.finite(point$n), logical(1)))) {
    note <- paste(note, paste0(
      "Support bars count ", population, " rows in each observed interval; the first point is a bin boundary."
    ))
  } else {
    note <- paste(note, if (numeric_x) {
      paste0("Support is a relative local-window measure on the ", population, " rows, not disjoint bin counts.")
    } else {
      paste0(
        "Support is category frequency among the ", population,
        " rows relative to the most frequent displayed category, not an absolute row count."
      )
    })
  }
  report_chart_frame(if (numeric_x) "effect" else "category", points, feature, y_label,
    paste(toupper(method), "for", feature),
    report_chart_table(rows, paste("Fitted", feature, "values by model"), attr(effect, "interval_note")),
    note,
    primary_model = model_id, zero = method == "ale",
    short_note = paste0(
      toupper(method), " for ", target, ". Support: ",
      if (method == "ale") {
        paste(population, "rows per interval.")
      } else if (numeric_x) {
        paste("relative neighborhoods on", population, "rows.")
      } else {
        paste("relative category frequency on", population, "rows.")
      }
    ),
    interval_note = if (!is.null(attr(effect, "interval_note"))) {
      "Intervals describe this fixed model; they exclude model-fitting and population uncertainty."
    } else {
      NULL
    }
  )
}

report_chart_svg_text <- function(value, x, y, width = 300, anchor = "start", class = "") {
  columns <- max(5L, floor(width / 7.2))
  words <- strsplit(value, "[[:space:]]+")[[1L]]
  lines <- character()
  current <- ""
  for (word in words) {
    if (nzchar(current) && nchar(current) + nchar(word) + 1L > columns) {
      lines <- c(lines, current)
      current <- ""
    }
    while (nchar(word) > columns) {
      lines <- c(lines, substr(word, 1L, columns))
      word <- substring(word, columns + 1L)
    }
    current <- paste0(current, if (nzchar(current)) " ", word)
  }
  if (nzchar(current)) lines <- c(lines, current)
  if (!length(lines)) lines <- ""
  paste0(
    '<text x="', x, '" y="', y, '" text-anchor="', anchor, '" class="', class, '">',
    paste(vapply(seq_along(lines), function(i) {
      paste0('<tspan x="', x, '" dy="', if (i == 1L) 0 else 15, '">', html_escape(lines[i]), "</tspan>")
    }, character(1)), collapse = ""), "</text>"
  )
}

report_chart_fallback <- function(kind, points, x_label, y_label, zero = FALSE, reference = NULL,
                                  x_limits = NULL, y_limits = NULL) {
  width <- 300
  categorical <- identical(kind, "category")
  histogram <- identical(kind, "histogram")
  categories <- unique(vapply(points, function(point) point$category %||% "", character(1)))
  left <- if (categorical) 112 else 48
  right <- width - 18
  top <- 42
  row_height <- if (categorical) max(50, max(nchar(categories)) / 12 * 17 + 20) else 0
  bottom <- if (categorical) top + length(categories) * row_height else 242
  support <- kind == "effect" && any(vapply(points, function(point) {
    is.finite(point$n %||% NA_real_) || is.finite(point$support %||% NA_real_)
  }, logical(1)))
  height <- bottom + if (support) 126 else 70
  xs <- vapply(points, function(point) point$x, numeric(1))
  ys <- vapply(points, function(point) point$y, numeric(1))
  lower <- vapply(points, function(point) point$low %||% NA_real_, numeric(1))
  upper <- vapply(points, function(point) point$high %||% NA_real_, numeric(1))
  boundaries <- unlist(lapply(points, function(point) c(point$left, point$right)), use.names = FALSE)
  limits <- report_chart_range(if (categorical) c(ys, lower, upper) else c(xs, boundaries), zero = categorical && zero)
  ylim <- report_chart_range(c(ys, lower, upper), zero = zero)
  if (identical(reference, "identity")) limits <- ylim <- report_chart_range(c(xs, ys))
  if (histogram && all(ys >= 0)) ylim[1] <- 0
  if (kind == "cost" && all(xs >= 0)) limits[1] <- max(0, limits[1])
  if (kind == "cost" && all(ys >= 0)) ylim[1] <- max(0, ylim[1])
  if (!is.null(x_limits)) limits <- x_limits
  if (!is.null(y_limits)) ylim <- y_limits
  if (!categorical) {
    left <- max(left, max(nchar(vapply(pretty(ylim, 4), report_axis_number, character(1)))) * 7.2 + 10)
  }
  px <- function(x) left + (x - limits[1]) / diff(limits) * (right - left)
  py <- function(y) bottom - (y - ylim[1]) / diff(ylim) * (bottom - top)
  horizontal <- paste(vapply(pretty(limits, 3), function(value) {
    if (value < limits[1] || value > limits[2]) {
      return("")
    }
    paste0(
      if (categorical) {
        paste0(
          '<line class="axr-grid" x1="', px(value), '" x2="', px(value),
          '" y1="', top, '" y2="', bottom, '"/>'
        )
      },
      report_chart_svg_text(report_axis_number(value), px(value), bottom + 20, anchor = "middle")
    )
  }, character(1)), collapse = "")
  vertical <- if (!categorical) {
    paste(vapply(pretty(ylim, 4), function(value) {
      if (value < ylim[1] || value > ylim[2]) {
        return("")
      }
      paste0(
        '<line class="axr-grid" x1="', left, '" x2="', right, '" y1="', py(value), '" y2="', py(value), '"/>',
        report_chart_svg_text(report_axis_number(value), left - 7, py(value) + 4, anchor = "end")
      )
    }, character(1)), collapse = "")
  } else {
    ""
  }
  reference_line <- if (identical(reference, "identity")) {
    paste0(
      '<line class="axr-zero" x1="', px(limits[1]), '" x2="', px(limits[2]),
      '" y1="', py(limits[1]), '" y2="', py(limits[2]), '"/>'
    )
  } else if (zero && !categorical) {
    paste0('<line class="axr-zero" x1="', left, '" x2="', right, '" y1="', py(0), '" y2="', py(0), '"/>')
  } else if (zero) {
    paste0('<line class="axr-zero" x1="', px(0), '" x2="', px(0), '" y1="', top, '" y2="', bottom, '"/>')
  } else {
    ""
  }
  glyphs <- paste(vapply(seq_along(points), function(i) {
    point <- points[[i]]
    cx <- px(if (categorical) point$y else point$x)
    cy <- if (categorical) top + (match(point$category, categories) - .5) * row_height else py(point$y)
    count <- point$count %||% NA_real_
    maximum <- max(1, vapply(points, function(item) item$count %||% 0, numeric(1)), na.rm = TRUE)
    radius <- if (is.finite(count)) 10 * sqrt(count / maximum) else 4
    band <- if (categorical && is.finite(point$low %||% NA_real_) && is.finite(point$high %||% NA_real_)) {
      paste0(
        '<line x1="', px(point$low), '" x2="', px(point$high), '" y1="', cy, '" y2="', cy,
        '" stroke="', point$color, '" stroke-width="2"/>'
      )
    } else {
      ""
    }
    bar <- if (histogram && is.finite(point$left %||% NA_real_) && is.finite(point$right %||% NA_real_)) {
      paste0(
        '<rect class="axr-band" x="', px(point$left), '" y="', py(point$y),
        '" width="', max(1, px(point$right) - px(point$left) - 1), '" height="', abs(py(0) - py(point$y)),
        '" fill="', point$color, '"/>'
      )
    } else {
      ""
    }
    paste0(
      bar, band, '<g tabindex="0" role="img" aria-label="', html_escape(point$detail), '">',
      '<circle class="axr-point" cx="', cx, '" cy="', cy, '" r="', radius, '" fill="', point$color,
      '"><title>', html_escape(point$detail), "</title></circle>",
      if (kind == "cost") {
        report_chart_svg_text(point$label,
          if (cx > width / 2) right else left + 4,
          max(top + 12, cy - 10 - (i %% 2) * 24),
          width = 160,
          anchor = if (cx > width / 2) "end" else "start", class = "axr-model-label"
        )
      },
      "</g>"
    )
  }, character(1)), collapse = "")
  category_labels <- if (categorical) {
    paste(vapply(seq_along(categories), function(i) {
      label_y <- top + (i - .5) * row_height - (ceiling(nchar(categories[i]) / 12) - 1) * 7.5
      report_chart_svg_text(categories[i], left - 8, label_y,
        width = left - 16, anchor = "end"
      )
    }, character(1)), collapse = "")
  } else {
    ""
  }
  lines <- if (kind == "effect") {
    paste(vapply(unique(vapply(points, `[[`, character(1), "model")), function(id) {
      group <- Filter(function(point) identical(point$model, id), points)
      band <- if (all(is.finite(c(lower, upper)))) {
        polygon <- c(
          vapply(group, function(point) paste(px(point$x), py(point$low), sep = ","), character(1)),
          rev(vapply(group, function(point) paste(px(point$x), py(point$high), sep = ","), character(1)))
        )
        paste0(
          '<polygon class="axr-band" fill="', group[[1]]$color, '" points="', paste(polygon, collapse = " "), '"/>'
        )
      } else {
        ""
      }
      paste0(
        band, '<polyline class="axr-line" stroke="', group[[1]]$color, '" points="',
        paste(vapply(group, function(point) {
          paste(px(point$x), py(point$y), sep = ",")
        }, character(1)), collapse = " "), '"/>'
      )
    }, character(1)), collapse = "")
  } else {
    ""
  }
  support_bars <- if (support) {
    counts <- vapply(points, function(point) point$n %||% NA_real_, numeric(1))
    has_counts <- any(is.finite(counts))
    amounts <- if (has_counts) counts else vapply(points, function(point) point$support %||% NA_real_, numeric(1))
    maximum <- max(amounts, na.rm = TRUE)
    if (!is.finite(maximum) || maximum <= 0) maximum <- 1
    floor <- bottom + 100
    bars <- paste(vapply(seq_along(points), function(i) {
      point <- points[[i]]
      if (!is.finite(amounts[i])) {
        return("")
      }
      start <- if (is.finite(point$`bin-left` %||% NA_real_)) px(point$`bin-left`) else px(point$x) - 2
      width <- if (has_counts) max(0, px(point$x) - start) else 4
      paste0(
        '<rect class="axr-support" x="', start, '" y="', floor - 20 * amounts[i] / maximum,
        '" width="', width, '" height="', 20 * amounts[i] / maximum, '"/>'
      )
    }, character(1)), collapse = "")
    paste0(
      report_chart_svg_text(if (has_counts) "Rows in each interval" else "Relative support (maximum = 1)",
        left, bottom + 70,
        width = right - left
      ), bars,
      report_chart_svg_text(report_axis_number(maximum), left - 7, floor - 16, anchor = "end"),
      report_chart_svg_text("0", left - 7, floor + 3, anchor = "end")
    )
  } else {
    ""
  }
  paste0(
    '<svg viewBox="0 0 ', width, " ", height, '" role="group" aria-label="',
    html_escape(paste(y_label, "by", x_label)), '">', horizontal, vertical, reference_line,
    lines, glyphs, category_labels, support_bars,
    report_chart_svg_text(if (categorical) x_label else y_label, 2, 15, width = width - 4),
    report_chart_svg_text(if (categorical) y_label else x_label, (left + right) / 2, bottom + 42,
      width = right - left, anchor = "middle"
    ), "</svg>"
  )
}
