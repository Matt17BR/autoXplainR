# Presentation records contain aggregate counts unless the report explicitly
# exports individual rows. Statistical summaries always use all evaluation rows.
prediction_bin_breaks <- function(values, bins = 10L) {
  limits <- range(values[is.finite(values)])
  if (!all(is.finite(limits))) {
    return(c(0, 1))
  }
  if (diff(limits) == 0) limits <- limits + c(-1, 1) * max(abs(limits[1]) * .1, .5)
  pretty(limits, n = bins)
}

prediction_bin_index <- function(values, breaks) {
  pmin(length(breaks) - 1L, pmax(1L, findInterval(values, breaks, rightmost.closed = TRUE)))
}

prediction_regression_records <- function(observed, predicted) {
  shared <- prediction_bin_breaks(c(observed, predicted), 10L)
  x <- prediction_bin_index(predicted, shared)
  y <- prediction_bin_index(observed, shared)
  count <- table(
    factor(x, levels = seq_len(length(shared) - 1L)),
    factor(y, levels = seq_len(length(shared) - 1L))
  )
  cells <- which(count > 0, arr.ind = TRUE)
  density <- data.frame(
    predicted_low = shared[cells[, 1]], predicted_high = shared[cells[, 1] + 1L],
    observed_low = shared[cells[, 2]], observed_high = shared[cells[, 2] + 1L],
    n = as.integer(count[cells])
  )
  residual <- observed - predicted
  error_breaks <- prediction_bin_breaks(residual, 12L)
  error_bin <- prediction_bin_index(residual, error_breaks)
  histogram <- data.frame(
    low = head(error_breaks, -1), high = utils::tail(error_breaks, -1),
    n = tabulate(error_bin, nbins = length(error_breaks) - 1L)
  )
  bins <- sort(unique(x))
  bias <- do.call(rbind, lapply(bins, function(bin) {
    members <- x == bin
    n <- sum(members)
    data.frame(
      low = shared[bin], high = shared[bin + 1L], n = n,
      mean_residual = if (n > 1L) mean(residual[members]) else NA_real_,
      mean_absolute_error = if (n > 1L) mean(abs(residual[members])) else NA_real_
    )
  }))
  list(
    density = density, residual_histogram = histogram, bias = bias,
    metrics = c(rmse = sqrt(mean(residual^2)), mae = mean(abs(residual)), bias = mean(residual))
  )
}

prediction_confusion <- function(observed, predicted, labels) {
  counts <- table(factor(observed, levels = labels), factor(predicted, levels = labels))
  rows <- lapply(seq_along(labels), function(i) {
    lapply(seq_along(labels), function(j) {
      list(
        observed = labels[i], predicted = labels[j], count = as.integer(counts[i, j]),
        rate = if (sum(counts[i, ]) > 0) unname(counts[i, j] / sum(counts[i, ])) else NULL
      )
    })
  })
  unlist(rows, recursive = FALSE)
}

prediction_cutoff_records <- function(observed, probability, positive) {
  truth <- as.character(observed) == positive
  # Integer division represents each displayed decimal cutoff directly;
  # repeated 0.01 multiplication can put 0.57 just above a literal probability 0.57.
  thresholds <- (0:100) / 100
  # Count strict lower bounds once per class. Ties at the cutoff are positive.
  fn <- findInterval(thresholds, sort(probability[truth]), left.open = TRUE)
  tn <- findInterval(thresholds, sort(probability[!truth]), left.open = TRUE)
  tp <- sum(truth) - fn
  fp <- sum(!truth) - tn
  lapply(seq_along(thresholds), function(i) {
    record <- list(
      threshold = thresholds[i], tp = tp[i], fp = fp[i], tn = tn[i], fn = fn[i],
      accuracy = (tp[i] + tn[i]) / length(truth),
      sensitivity = safe_threshold_rate(tp[i], tp[i] + fn[i]),
      specificity = safe_threshold_rate(tn[i], tn[i] + fp[i]),
      precision = safe_threshold_rate(tp[i], tp[i] + fp[i])
    )
    fields <- c("accuracy", "sensitivity", "specificity", "precision", "fp", "fn")
    record$display <- stats::setNames(lapply(fields, function(field) {
      value <- record[[field]]
      if (!is.finite(value)) {
        return("Not defined")
      }
      if (field %in% c("fp", "fn")) as.character(value) else sprintf("%.1f%%", 100 * value)
    }), fields)
    record
  })
}

prediction_calibration_records <- function(observed, probability, labels, positive = NULL) {
  if (is.matrix(probability)) {
    probability <- probability[, labels, drop = FALSE]
    winning <- max.col(probability, ties.method = "first")
    confidence <- probability[cbind(seq_along(winning), winning)]
    event <- as.character(observed) == labels[winning]
    scope <- "Confidence calibration: fraction correct among predictions in each confidence bin."
  } else {
    confidence <- probability
    event <- as.character(observed) == positive
    scope <- paste0("Event calibration: observed frequency of ", positive, " within each probability bin.")
  }
  grouped <- calibration_groups(confidence, event, 5L)
  values <- lapply(seq_len(nrow(grouped)), function(bin) {
    list(
      low = grouped$probability_min[bin], high = grouped$probability_max[bin], n = grouped$rows[bin],
      mean_probability = grouped$mean_probability[bin], observed_rate = grouped$observed_rate[bin],
      correct_or_events = as.integer(round(grouped$rows[bin] * grouped$observed_rate[bin]))
    )
  })
  list(
    scope = scope, bins = values,
    calibration_error = sum(grouped$rows * grouped$calibration_gap) / length(confidence)
  )
}

prediction_exported_cases <- function(result, observed, probability, labels = NULL, positive = NULL) {
  export <- result$.report_export
  if (!identical(export$mode, "rows")) {
    return(NULL)
  }
  if (is.matrix(probability)) probability <- probability[, labels, drop = FALSE]
  rows <- Filter(function(row) {
    identical(row$partition, "evaluation") && isTRUE(row$retained) &&
      length(row$processed_position) == 1L && !is.na(row$processed_position)
  }, export$rows %||% list())
  lapply(rows, function(row) {
    i <- row$processed_position
    if (i < 1 || i > length(observed)) stop("Exported evaluation row is not aligned with predictions.", call. = FALSE)
    prediction <- if (is.null(labels)) {
      probability[i]
    } else if (is.matrix(probability)) {
      labels[max.col(probability[i, , drop = FALSE], ties.method = "first")]
    } else {
      if (probability[i] >= .5) positive else setdiff(labels, positive)[[1L]]
    }
    class_probability <- function(label) {
      if (is.null(labels)) {
        return(NULL)
      }
      if (is.matrix(probability)) {
        return(unname(probability[i, as.character(label)]))
      }
      if (as.character(label) == positive) probability[i] else 1 - probability[i]
    }
    list(
      row_key = row$row_key, source = row$source, source_row = row$source_row,
      processed_position = i, observed = if (is.null(labels)) observed[i] else as.character(observed[i]),
      predicted = prediction,
      observed_probability = class_probability(observed[i]), predicted_probability = class_probability(prediction),
      residual = if (is.null(labels)) observed[i] - probability[i] else NULL,
      probability = if (is.null(labels)) {
        NULL
      } else if (is.matrix(probability)) {
        stats::setNames(as.list(as.numeric(probability[i, ])), labels)
      } else {
        probability[i]
      }
    )
  })
}

prepare_prediction_view <- function(result, model_ids) {
  explainers <- report_explainers(result, models = model_ids)
  predictions <- report_predictions(result, models = model_ids, explainers = explainers)
  models <- lapply(model_ids, function(id) {
    explainer <- explainers[[id]]
    observed <- explainer$y
    probability <- predictions[[id]]
    regression <- identical(result$task, "regression")
    labels <- if (regression) NULL else explainer$class_levels
    predicted <- if (regression) {
      probability
    } else if (is.matrix(probability)) {
      labels[max.col(probability, ties.method = "first")]
    } else {
      ifelse(probability >= .5, explainer$positive, setdiff(labels, explainer$positive)[[1L]])
    }
    list(
      model_id = id, label = explorer_label(result, id), task = result$task, n = length(observed),
      positive = explainer$positive, labels = labels,
      r_code = prediction_r_code(id, result$task, labels, explainer$positive),
      regression = if (regression) prediction_regression_records(observed, probability) else NULL,
      confusion = if (!regression) prediction_confusion(observed, predicted, labels) else NULL,
      accuracy = if (!regression) mean(as.character(observed) == predicted) else NULL,
      calibration = if (!regression) prediction_calibration_records(observed, probability, labels, explainer$positive),
      cutoffs = if (identical(result$task, "binary")) {
        prediction_cutoff_records(observed, probability, explainer$positive)
      },
      cases = prediction_exported_cases(result, observed, probability, labels, explainer$positive)
    )
  })
  list(
    mode = result$.report_export$mode %||% "summary", models = models,
    evaluation_role = result$provenance$evaluation_role %||% "evaluation"
  )
}

prediction_r_code <- function(id, task, labels = NULL, positive = NULL) {
  literal <- function(value) paste(deparse(value), collapse = " ")
  call <- paste0("predict(result, new_data, model = ", literal(id), ")")
  if (task != "binary") return(list(prediction = call))
  list(
    prediction = paste0("probability <- ", call),
    cutoff_prefix = "predicted_class <- factor(ifelse(probability >= ",
    cutoff_suffix = paste0(
      ", ", literal(positive), ", ", literal(setdiff(labels, positive)[[1L]]),
      "), levels = ", literal(labels), ")"
    )
  )
}

prediction_r_example <- function(model) {
  code <- model$r_code %||% prediction_r_code(model$model_id, model$task, model$labels, model$positive)
  if (model$task != "binary") return(code$prediction)
  paste0(code$prediction, "\n", code$cutoff_prefix, "0.50", code$cutoff_suffix)
}

prediction_points <- function(rows, x, y, model, result, detail, count = NULL) {
  lapply(seq_len(nrow(rows)), function(i) {
    list(
      x = x[i], y = y[i], model = model$model_id, label = model$label,
      color = report_model_color(model$model_id, result), detail = paste(model$label, detail(i), sep = "; "),
      count = if (!is.null(count)) count[i] else NULL
    )
  })
}

prediction_regression_html <- function(model, result) {
  regression <- model$regression
  density <- regression$density
  units <- result$provenance$target_units
  unit <- if (!is.null(units)) paste0(" (", units, ")") else ""
  interval <- function(a, b) paste0("[", report_axis_number(a), ", ", report_axis_number(b), "]")
  density_points <- prediction_points(
    density, (density$predicted_low + density$predicted_high) / 2,
    (density$observed_low + density$observed_high) / 2, model, result,
    function(i) {
      paste0(
        "predicted bin ", interval(density$predicted_low[i], density$predicted_high[i]),
        "; observed bin ", interval(density$observed_low[i], density$observed_high[i]), "; rows ", density$n[i]
      )
    }, density$n
  )
  histogram <- regression$residual_histogram
  histogram_points <- prediction_points(
    histogram, (histogram$low + histogram$high) / 2, histogram$n,
    model, result, function(i) {
      paste0(
        "residual bin ", interval(histogram$low[i], histogram$high[i]),
        "; rows ", histogram$n[i]
      )
    }, histogram$n
  )
  histogram_points <- lapply(seq_along(histogram_points), function(i) {
    c(histogram_points[[i]], list(left = histogram$low[i], right = histogram$high[i]))
  })
  bias <- regression$bias
  usable <- bias[is.finite(bias$mean_residual), , drop = FALSE]
  bias_points <- prediction_points(
    usable, (usable$low + usable$high) / 2, usable$mean_residual,
    model, result, function(i) {
      paste0(
        "prediction bin ", interval(usable$low[i], usable$high[i]),
        "; mean observed minus predicted ", report_axis_number(usable$mean_residual[i]), "; rows ", usable$n[i]
      )
    }
  )
  paste0(
    '<div class="prediction-metrics">',
    prediction_metric("RMSE", regression$metrics[["rmse"]], scope = model$model_id),
    prediction_metric("MAE", regression$metrics[["mae"]], scope = model$model_id),
    prediction_metric("Mean residual", regression$metrics[["bias"]], scope = model$model_id), "</div>",
    '<div class="prediction-chart-grid">',
    report_chart_frame("scatter", density_points, paste0("Predicted", unit), paste0("Observed", unit),
      "Observed and predicted", report_chart_table(density, "Occupied evaluation bins and row counts"),
      "Each point is an occupied two-dimensional bin, not an individual record. Point area reflects the count.",
      reference = "identity"
    ),
    report_chart_frame("histogram", histogram_points, paste0("Observed minus predicted", unit), "Rows",
      "Distribution of errors", report_chart_table(histogram, "Residual bin counts"),
      "Positive residuals mean this model predicted too low. The bins include every evaluation row.",
      zero = TRUE
    ),
    "</div>",
    report_chart_frame("scatter", bias_points, paste0("Predicted bin midpoint", unit), paste0("Mean residual", unit),
      "Does error change with the prediction?", report_chart_table(bias, "Mean residual and MAE by prediction bin"),
      "A mean residual above zero indicates underprediction in that bin. Bins with fewer than two rows omit means.",
      zero = TRUE
    )
  )
}

prediction_metric <- function(label, value, attribute = NULL, percent = FALSE, scope = "prediction") {
  definition <- switch(label,
    Accuracy = "The percentage of evaluation rows assigned their observed class.",
    `False positives` = "Predicted the event when it did not occur. The event label is shown above the cutoff.",
    `False negatives` = "Missed an event: predicted the other class when the event occurred.",
    Sensitivity = "Of the observed events, the percentage identified correctly. Also called recall.",
    Specificity = "Of the observed non-events, the percentage identified correctly. Also called true-negative rate.",
    Precision = paste(
      "Of the rows predicted to be events, the percentage that actually were events.",
      "Undefined when no events are predicted."
    ),
    RMSE = "Root mean squared error, in outcome units. Larger errors receive more weight than they do in MAE.",
    MAE = "Mean absolute error, in outcome units: the average distance between a prediction and the observed outcome.",
    `Mean residual` = paste(
      "Average observed outcome minus prediction.",
      "Positive means underprediction on average; negative means overprediction."
    ),
    NULL
  )
  paste0(
    "<div><span>", html_escape(label),
    if (!is.null(definition)) explorer_help(paste(label, "for", scope), definition),
    "</span><strong",
    if (!is.null(attribute)) paste0(' data-cutoff-metric="', attribute, '"'), ">",
    html_escape(if (!is.finite(value)) {
      "Not defined"
    } else {
      if (percent) sprintf("%.1f%%", 100 * value) else report_axis_number(value)
    }), "</strong></div>"
  )
}

prediction_confusion_html <- function(cells, labels, model_id) {
  paste0(
    if (length(labels) > 4L) '<p class="prediction-scope">Scroll to inspect all classes.</p>',
    '<div class="table-wrap" role="region" tabindex="0" aria-label="Confusion counts and within-class rates">',
    '<table class="prediction-confusion" data-confusion-table data-class-count="', length(labels), '"',
    if (length(labels) > 4L) " data-many-classes", "><caption>",
    "Observed classes in rows; predicted classes in columns. Percentages are within each observed class.</caption>",
    '<thead><tr><th scope="col">Observed / predicted</th>',
    paste0('<th scope="col">', html_escape(labels), "</th>", collapse = ""), "</tr></thead><tbody>",
    paste(vapply(labels, function(label) {
      selected <- Filter(function(cell) identical(cell$observed, label), cells)
      paste0('<tr><th scope="row">', html_escape(label), "</th>", paste(vapply(selected, function(cell) {
        paste0(
          '<td data-observed="', html_escape(cell$observed), '" data-predicted="', html_escape(cell$predicted),
          '"><strong data-cell-count>', cell$count, "</strong><span data-cell-rate>",
          if (is.null(cell$rate)) "Not defined" else sprintf("%.1f%%", 100 * cell$rate),
          "</span></td>"
        )
      }, character(1)), collapse = ""), "</tr>")
    }, character(1)), collapse = ""), "</tbody></table></div>"
  )
}

prediction_classification_html <- function(model, result) {
  binary <- identical(model$task, "binary")
  initial <- if (binary) model$cutoffs[[51L]] else NULL
  calibration <- model$calibration
  bins <- do.call(rbind, lapply(calibration$bins, as.data.frame))
  points <- prediction_points(
    bins, bins$mean_probability, bins$observed_rate, model, result,
    function(i) {
      paste0(
        "probability group [", report_axis_number(bins$low[i]), ", ",
        report_axis_number(bins$high[i]), "]; mean probability ", report_axis_number(bins$mean_probability[i]),
        "; observed rate ", report_axis_number(bins$observed_rate[i]),
        "; rows ", bins$n[i]
      )
    }, bins$n
  )
  slider_id <- report_anchor(paste0("prediction-cutoff-", model$model_id))
  paste0(
    '<div class="classification-grid"><div class="classification-outcomes">',
    if (binary) {
      paste0(
        '<div class="prediction-cutoff"><div class="prediction-cutoff-title"><label for="',
        slider_id, '">Decision cutoff for ',
        html_escape(model$positive), ": <output data-cutoff-value>0.50</output></label>",
        explorer_help(paste("Cutoff scope for", model$model_id), paste(
          "These are descriptive scores on the recorded evaluation rows.",
          "Exploring a cutoff does not change the official leaderboard or establish independent validation."
        )), "</div>",
        '<input id="', slider_id,
        '" data-prediction-cutoff type="range" min="0" max="100" step="1" value="50" disabled>',
        "<p data-cutoff-rule>Probability at least 0.50 predicts ", html_escape(model$positive), ".</p>",
        "<noscript><p>The static table uses cutoff 0.50. Enable JavaScript to explore ",
        "the precomputed cutoff grid.</p></noscript></div>"
      )
    },
    '<div class="prediction-metrics" aria-live="polite">',
    prediction_metric(
      "Accuracy", model$accuracy, if (binary) "accuracy" else NULL,
      percent = TRUE, scope = model$model_id
    ),
    if (binary) {
      paste0(
        prediction_metric("False positives", initial$fp, "fp", scope = model$model_id),
        prediction_metric("False negatives", initial$fn, "fn", scope = model$model_id),
        prediction_metric("Sensitivity", initial$sensitivity, "sensitivity", percent = TRUE, scope = model$model_id),
        prediction_metric("Specificity", initial$specificity, "specificity", percent = TRUE, scope = model$model_id),
        prediction_metric("Precision", initial$precision, "precision", percent = TRUE, scope = model$model_id)
      )
    },
    "</div>", prediction_confusion_html(model$confusion, model$labels, model$model_id),
    '</div><div class="classification-calibration">',
    report_chart_frame(
      "scatter", points, "Mean predicted probability", "Observed rate", "Calibration",
      report_chart_table(bins, calibration$scope), paste(
        calibration$scope,
        "Groups use the same rank grouping as the leaderboard's binned calibration gap:",
        "at most five groups, reduced for small evaluation sets; tied probabilities stay together.",
        "Each point compares the group's mean probability with its observed rate.",
        "The diagonal marks agreement. Point area represents the number of rows in the group.",
        "The cutoff does not change calibration."
      ),
      reference = "identity", x_limits = c(0, 1), y_limits = c(0, 1),
      short_note = paste(
        if (binary) paste0("Event: ", model$positive, ".") else "Confidence versus fraction correct.",
        "Point area shows row count; the diagonal marks agreement."
      )
    ), "</div></div>"
  )
}

prediction_ordered_cases <- function(cases, task) {
  if (!length(cases)) {
    return(cases)
  }
  index <- if (task == "regression") {
    order(-vapply(cases, function(row) abs(row$residual), numeric(1)))
  } else {
    order(
      vapply(cases, function(row) identical(as.character(row$observed), as.character(row$predicted)), logical(1)),
      vapply(cases, function(row) row$observed_probability, numeric(1))
    )
  }
  cases[index]
}

prediction_case_table <- function(cases, task) {
  if (!length(cases)) {
    return("<p>No retained evaluation records are present in the exported row sample.</p>")
  }
  rows <- head(prediction_ordered_cases(cases, task), 10L)
  case_value <- function(value) {
    if (task != "regression") return(html_escape(as.character(value)))
    paste0(
      '<span title="', html_escape(as.character(value)), '">',
      report_axis_number(as.numeric(value)), "</span>"
    )
  }
  paste0(
    '<div class="table-wrap"><table class="prediction-cases"><caption>',
    "Up to ten exported evaluation records, ",
    if (task == "regression") {
      "largest absolute errors first."
    } else {
      "mistakes first, then lowest probability assigned to the observed class."
    },
    if (task == "binary") " Case predictions use the official 0.50 cutoff.", "</caption><thead><tr>",
    '<th scope="col">Source record</th><th scope="col">Observed</th><th scope="col">Predicted</th>',
    if (task == "regression") {
      '<th scope="col">Observed minus predicted</th>'
    } else {
      paste0(
        '<th scope="col"><abbr title="Probability assigned to the observed class">P(observed)</abbr></th>',
        '<th scope="col"><abbr title="Probability assigned to the predicted class">P(predicted)</abbr></th>'
      )
    }, "</tr></thead><tbody>",
    paste(vapply(rows, function(row) {
      paste0(
        '<tr data-case-row="', html_escape(row$row_key), '"><td><a href="#data" data-navigate data-select-row="',
        html_escape(row$row_key), '">', html_escape(paste(row$source, row$source_row)), "</a></td><td>",
        case_value(row$observed), "</td><td>", case_value(row$predicted), "</td>",
        if (task == "regression") {
          paste0("<td>", case_value(row$residual), "</td>")
        } else {
          paste0(
            "<td>", report_axis_number(row$observed_probability), "</td><td>",
            report_axis_number(row$predicted_probability), "</td>"
          )
        }, "</tr>"
      )
    }, character(1)), collapse = ""), "</tbody></table></div>"
  )
}

explorer_predictions <- function(result, models) {
  view <- prepare_prediction_view(result, models$table$model_id)
  panels <- vapply(view$models, function(model) {
    paste0(
      '<div class="model-panel prediction-panel" data-model-panel="', html_escape(model$model_id),
      '" data-prediction-model="', html_escape(model$model_id), '"><h3 class="model-heading">',
      html_escape(model$label),
      "</h3>", explorer_model_identity(result, model$model_id),
      '<p class="prediction-scope">Diagnostics use all ', model$n, " ", html_escape(view$evaluation_role),
      " rows for ", html_escape(model$label), ".</p>",
      if (model$task == "regression") {
        prediction_regression_html(model, result)
      } else {
        prediction_classification_html(model, result)
      },
      if (identical(view$mode, "rows")) {
        paste0(
          '<details class="prediction-records"><summary>Inspect exported evaluation records (',
          length(model$cases), " of ", model$n, ")</summary><p>These records are the explicitly exported sample. ",
          "The diagnostics above use every evaluation row. Select a source record to inspect it in Explore data.</p>",
          prediction_case_table(model$cases, model$task), "</details>"
        )
      } else {
        paste0(
          '<p class="prediction-scope">Individual errors are not embedded. ',
          'Use report_data = "rows" to export linked evaluation records.</p>'
        )
      },
      '<details><summary>Use this fitted model in R</summary><pre tabindex="0"><code data-prediction-code>',
      html_escape(prediction_r_example(model)), "</code></pre></details></div>"
    )
  }, character(1))
  paste0(
    '<section id="evaluation" class="workspace-page prediction-workspace" data-page="evaluation" ',
    'aria-labelledby="evaluation-title"><p class="section-number">Predictions</p>',
    '<h2 id="evaluation-title">Where do predictions go wrong?</h2>',
    '<div class="task-controls">',
    explorer_model_control(models, result$provenance$primary_model_id, "prediction-model-select"),
    "</div>", paste(panels, collapse = ""), explorer_disagreement(result),
    report_json_script(view, "axr-predictions-payload"), "</section>"
  )
}
