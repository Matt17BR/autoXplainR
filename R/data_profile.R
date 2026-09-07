# Descriptive data summaries. These functions require no fitted model and never
# turn a difference between supplied samples into a distribution-shift test.
data_column_kind <- function(value) {
  if (inherits(value, "Date")) {
    return("date")
  }
  if (inherits(value, "POSIXt")) {
    return("datetime")
  }
  if (is.numeric(value)) {
    return("numeric")
  }
  if (is.factor(value) || is.character(value) || is.logical(value)) {
    return("categorical")
  }
  "unsupported"
}

data_numeric <- function(value) {
  if (data_column_kind(value) %in% c("numeric", "date", "datetime")) as.numeric(value) else NULL
}

data_axis_label <- function(value, kind) {
  if (kind == "date") {
    return(as.character(as.Date(value, origin = "1970-01-01")))
  }
  if (kind == "datetime") {
    return(format(as.POSIXct(value, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M", tz = "UTC"))
  }
  vapply(value, function(number) {
    format(signif(number, 4L), trim = TRUE,
           scientific = abs(number) > 0 && (abs(number) < .001 || abs(number) >= 1e6))
  }, character(1), USE.NAMES = FALSE)
}

data_axis <- function(training, evaluation, bins = 24L, max_levels = 20L) {
  reference <- if (is.null(training)) evaluation else training
  kind <- data_column_kind(reference)
  if (kind == "unsupported") {
    return(list(kind = kind, status = "unsupported", reason = "This column type cannot be plotted."))
  }
  if (kind == "categorical") {
    train <- as.character(training)
    counts <- sort(table(train, useNA = "no"), decreasing = TRUE)
    levels <- if (is.ordered(reference)) {
      intersect(base::levels(reference), names(counts))
    } else {
      names(counts)
    }
    if (is.null(training)) levels <- names(sort(table(as.character(evaluation)), decreasing = TRUE))
    kept <- head(levels, max_levels)
    other_text <- if (is.null(training)) "Other evaluation levels" else "Other training levels"
    novel_text <- if (is.null(training)) "Unmapped values" else "New in evaluation"
    other_label <- utils::tail(make.unique(c(kept, other_text)), 1L)
    novel_label <- utils::tail(make.unique(c(kept, other_label, novel_text)), 1L)
    return(list(
      kind = kind, status = "available", levels = unname(kept),
      known_levels = unname(levels), labels = c(unname(kept), other_label, novel_label),
      ordered = is.ordered(reference), basis = if (is.null(training)) {
        "Evaluation categories; this column is absent from training"
      } else {
        "Categories ordered by training counts; missing values remain separate"
      },
      other_code = length(kept) + 1L, novel_code = length(kept) + 2L
    ))
  }
  finite <- data_numeric(training)
  finite <- finite[is.finite(finite)]
  basis <- "Training range; evaluation overflow is shown separately"
  if (!length(finite)) {
    finite <- data_numeric(evaluation)
    finite <- finite[is.finite(finite)]
    basis <- "Evaluation range because no finite training values are available"
  }
  if (!length(finite)) {
    return(list(kind = kind, status = "unavailable", reason = "No finite values in either partition."))
  }
  limits <- range(finite)
  if (limits[1L] == limits[2L]) {
    delta <- if (kind == "date") .5 else max(abs(limits[1L]) * .01, .5)
    limits <- limits + c(-delta, delta)
  }
  breaks <- seq(limits[1L], limits[2L], length.out = bins + 1L)
  labels <- paste0(
    data_axis_label(head(breaks, -1L), kind), " to ",
    data_axis_label(utils::tail(breaks, -1L), kind)
  )
  list(
    kind = kind, status = "available", breaks = breaks,
    labels = c(
      paste0("< ", data_axis_label(breaks[1L], kind)), labels,
      paste0("> ", data_axis_label(utils::tail(breaks, 1L), kind))
    ),
    basis = basis, timezone = if (kind == "datetime") "UTC" else NULL
  )
}

data_bin_codes <- function(value, axis) {
  if (!identical(axis$status, "available")) {
    return(rep(NA_integer_, length(value)))
  }
  if (axis$kind == "categorical") {
    text <- as.character(value)
    code <- match(text, axis$levels)
    other <- is.na(code) & !is.na(text)
    code[other] <- ifelse(text[other] %in% axis$known_levels, axis$other_code, axis$novel_code)
    return(code)
  }
  values <- data_numeric(value)
  code <- findInterval(values, axis$breaks, rightmost.closed = TRUE) + 1L
  code[!is.finite(values)] <- NA_integer_
  as.integer(code)
}

data_distribution <- function(value, axis, rows) {
  if (is.null(value)) {
    return(list(status = "unavailable", reason = "Column absent from this partition.", n_total = rows))
  }
  if (!identical(axis$status, "available")) {
    return(list(
      status = axis$status, reason = axis$reason,
      n_total = length(value), n_missing = sum(is.na(value))
    ))
  }
  code <- data_bin_codes(value, axis)
  number <- data_numeric(value)
  finite <- if (is.null(number)) numeric() else number[is.finite(number)]
  list(
    status = "available", n_total = length(value), n_missing = sum(is.na(value)),
    n_nonfinite = if (is.null(number)) 0L else sum(!is.finite(number) & !is.na(number)),
    n_used = sum(!is.na(code)), n_unique = length(unique(value[!is.na(value)])),
    counts = tabulate(code, nbins = length(axis$labels)),
    mean = if (length(finite)) mean(finite) else NULL,
    quantiles = if (length(finite)) {
      values <- as.numeric(stats::quantile(finite, probs = c(0, .25, .5, .75, 1), names = FALSE))
      as.list(setNames(values, c("min", "q1", "median", "q3", "max")))
    } else {
      NULL
    }
  )
}

data_pair_association <- function(x, y) {
  kinds <- vapply(list(x, y), data_column_kind, character(1))
  unavailable <- function(reason, n = 0L) {
    list(status = "unavailable", method = NULL, value = NA_real_, n = n, reason = reason)
  }
  if (is.null(x) || is.null(y) || any(kinds == "unsupported")) {
    return(unavailable("A column is unavailable or has an unsupported type."))
  }
  x_numeric <- kinds[1L] %in% c("numeric", "date", "datetime")
  y_numeric <- kinds[2L] %in% c("numeric", "date", "datetime")
  if (x_numeric) x <- as.numeric(x)
  if (y_numeric) y <- as.numeric(y)
  complete <- !is.na(x) & !is.na(y)
  if (x_numeric) complete <- complete & is.finite(x)
  if (y_numeric) complete <- complete & is.finite(y)
  x <- x[complete]
  y <- y[complete]
  n <- length(x)
  if (n < 3L || length(unique(x)) < 2L || length(unique(y)) < 2L) {
    return(unavailable("At least three complete pairs and variation in both columns are needed.", n))
  }
  method <- if (x_numeric && y_numeric) {
    "Spearman correlation (signed)"
  } else if (x_numeric != y_numeric) {
    "Correlation ratio (unsigned)"
  } else {
    "Cramer's V (unsigned)"
  }
  value <- if (x_numeric && y_numeric) {
    stats::cor(x, y, method = "spearman")
  } else if (x_numeric != y_numeric) {
    feature_association(x, y)
  } else {
    # Sum occupied contingency cells without allocating an all-levels square matrix.
    a <- match(x, unique(x))
    b <- match(y, unique(y))
    key <- paste(a, b, sep = ":")
    counts <- table(key)
    first <- match(names(counts), key)
    expected <- tabulate(a)[a[first]] * tabulate(b)[b[first]] / n
    chi_squared <- max(0, sum(as.numeric(counts)^2 / expected) - n)
    sqrt(chi_squared / (n * min(max(a) - 1L, max(b) - 1L)))
  }
  list(
    status = "available", method = method, value = unname(value), n = n,
    scope = paste("Unbinned complete pairs. Small association does not establish independence",
                  "or exclude nonlinear or joint dependence.")
  )
}

data_pair_profile <- function(x, y, x_axis, y_axis, rows, positive = NULL) {
  association <- data_pair_association(x, y)
  if (is.null(x) || is.null(y)) {
    return(list(status = "unavailable", reason = "A column is absent from this partition.",
                n_total = rows, association = association))
  }
  if (!identical(x_axis$status, "available") || !identical(y_axis$status, "available")) {
    return(list(status = "unavailable", reason = "A column has no usable display values.",
                n_total = rows, association = association))
  }
  a <- data_bin_codes(x, x_axis)
  b <- data_bin_codes(y, y_axis)
  ok <- !is.na(a) & !is.na(b)
  width <- length(x_axis$labels)
  joint <- tabulate(a[ok] + (b[ok] - 1L) * width,
    nbins = width * length(y_axis$labels)
  )
  occupied <- which(joint > 0L)
  cells <- data.frame(
    x = (occupied - 1L) %% width + 1L,
    y = (occupied - 1L) %/% width + 1L, n = joint[occupied]
  )
  conditional <- NULL
  conditional_event <- NULL
  response <- data_numeric(y)
  if (!is.null(response) && any(ok)) {
    groups <- split(response[ok], a[ok])
    conditional <- do.call(rbind, lapply(names(groups), function(bin) {
      values <- groups[[bin]]
      data.frame(
        x = as.integer(bin), n = length(values), mean = mean(values),
        median = stats::median(values), min = min(values), max = max(values)
      )
    }))
    rownames(conditional) <- NULL
  }
  if (!is.null(positive) && any(ok)) {
    groups <- split(as.character(y[ok]) == positive, a[ok])
    conditional_event <- do.call(rbind, lapply(names(groups), function(bin) {
      event <- groups[[bin]]
      data.frame(x = as.integer(bin), n = length(event), events = sum(event), rate = mean(event))
    }))
    rownames(conditional_event) <- NULL
  }
  list(
    status = "available", n_total = rows, n_complete = sum(ok),
    n_excluded = rows - sum(ok), cells = cells, conditional = conditional,
    conditional_event = conditional_event,
    association = association,
    interpretation = "Observed joint counts; not fitted effects, causal effects or a significance test."
  )
}

data_pair_indices <- function(n_columns, target_position, max_pairs = 512L) {
  n_columns <- assert_count(n_columns, "n_columns", minimum = 0L)
  max_pairs <- assert_count(max_pairs, "max_pairs", minimum = 0L)
  if (n_columns < 2L) return(data.frame(x = integer(), y = integer()))
  has_target <- length(target_position) == 1L && !is.na(target_position) &&
    target_position >= 1L && target_position <= n_columns
  required <- if (has_target) n_columns - 1L else 0L
  size <- as.integer(min(choose(n_columns, 2L), max(max_pairs, required)))
  x <- integer(size)
  y <- integer(size)
  others <- seq_len(n_columns)
  used <- 0L
  if (has_target) {
    target_position <- as.integer(target_position)
    others <- others[-target_position]
    positions <- seq_len(required)
    # This is the original combination order restricted to target pairs.
    x[positions] <- pmin(others, target_position)
    y[positions] <- pmax(others, target_position)
    used <- required
  }
  i <- 1L
  while (used < size) {
    take <- min(size - used, length(others) - i)
    positions <- seq.int(used + 1L, used + take)
    x[positions] <- others[i]
    y[positions] <- others[seq.int(i + 1L, i + take)]
    used <- used + take
    i <- i + 1L
  }
  data.frame(x = x, y = y)
}

build_data_profile <- function(raw, processed, row_map, columns, target, bins = 24L,
                               max_pairs = 512L, positive = NULL) {
  bins <- assert_count(bins, "bins", minimum = 2L)
  variables <- columns$name
  stages <- list()
  inputs <- list(raw = raw, processed = processed)
  pair_indices <- data_pair_indices(length(variables), match(target, variables), max_pairs)
  for (stage in names(inputs)) {
    data <- inputs[[stage]]
    if (is.null(data$training) && is.null(data$evaluation)) next
    profiles <- setNames(lapply(variables, function(name) {
      axis <- data_axis(data$training[[name]], data$evaluation[[name]], bins)
      if (stage == "processed" && is.null(data$training[[name]]) && is.null(data$evaluation[[name]])) {
        axis <- list(
          kind = data_column_kind(raw$training[[name]] %||% raw$evaluation[[name]]),
          status = "unavailable", reason = "This column was not used by the models; choose Raw supplied values."
        )
      }
      list(
        axis = axis, training = data_distribution(data$training[[name]], axis, nrow(data$training) %||% 0L),
        evaluation = data_distribution(data$evaluation[[name]], axis, nrow(data$evaluation) %||% 0L)
      )
    }), variables)
    pairs <- list()
    if (nrow(pair_indices)) {
      for (i in seq_len(nrow(pair_indices))) {
        a <- pair_indices$x[i]
        b <- pair_indices$y[i]
        # The target belongs on the response axis for observed conditional summaries.
        if (variables[a] == target) {
          temporary <- a
          a <- b
          b <- temporary
        }
        x <- variables[a]
        y <- variables[b]
        pairs[[paste(sort(c(a, b)), collapse = "_")]] <- list(
          x = x, y = y,
          training = data_pair_profile(
            data$training[[x]], data$training[[y]],
            profiles[[x]]$axis, profiles[[y]]$axis, nrow(data$training) %||% 0L,
            positive = if (y == target) positive else NULL
          ),
          evaluation = data_pair_profile(
            data$evaluation[[x]], data$evaluation[[y]],
            profiles[[x]]$axis, profiles[[y]]$axis, nrow(data$evaluation) %||% 0L,
            positive = if (y == target) positive else NULL
          )
        )
      }
    }
    stages[[stage]] <- list(
      columns = profiles, pairs = pairs,
      population = if (stage == "raw") "All supplied rows before preprocessing" else "Retained model rows",
      training_rows = nrow(data$training), evaluation_rows = nrow(data$evaluation),
      training_available = !is.null(data$training), evaluation_available = !is.null(data$evaluation)
    )
  }
  ledger <- if (is.null(row_map)) {
    NULL
  } else {
    do.call(rbind, lapply(unique(row_map$partition), function(partition) {
      rows <- row_map[row_map$partition == partition, , drop = FALSE]
      data.frame(
        partition = partition, supplied = nrow(rows), retained = sum(rows$retained),
        excluded = sum(!rows$retained)
      )
    }))
  }
  list(
    schema_version = "1.0", columns = columns, target = target, stages = stages, row_ledger = ledger,
    pair_coverage = list(
      included = nrow(pair_indices), total = choose(length(variables), 2L),
      policy = "Target pairs first, then original column order; all individual profiles are retained"
    ),
    scope = "Descriptive summaries of the supplied partitions. Aggregate output is not anonymization."
  )
}

data_sample_indices <- function(sizes, maximum, seed) {
  total <- sum(sizes)
  if (total <= maximum) {
    return(lapply(sizes, seq_len))
  }
  allocation <- floor(maximum * sizes / total)
  nonempty <- which(sizes > 0L)
  allocation[nonempty] <- pmax(allocation[nonempty], 1L)
  while (sum(allocation) > maximum) {
    index <- which.max(allocation)
    allocation[index] <- allocation[index] - 1L
  }
  while (sum(allocation) < maximum) {
    index <- which.max(maximum * sizes / total - allocation)
    allocation[index] <- allocation[index] + 1L
  }
  with_preserved_seed(seed, lapply(seq_along(sizes), function(i) {
    if (!allocation[i]) integer() else sort(sample.int(sizes[i], allocation[i]))
  }))
}

data_export_value <- function(value, index) {
  if (is.null(value) || is.na(index)) {
    return(NULL)
  }
  kind <- data_column_kind(value)
  if (kind == "unsupported" || is.na(value[index])) {
    return(NULL)
  }
  if (kind %in% c("date", "datetime")) {
    return(as.numeric(value[index]))
  }
  if (kind == "categorical") {
    return(as.character(value[index]))
  }
  if (!is.finite(value[index])) {
    return(NULL)
  }
  unname(value[index])
}

data_nonfinite_columns <- function(data, index, variables) {
  if (is.null(data) || is.na(index)) return(character())
  variables[vapply(variables, function(name) {
    value <- data_numeric(data[[name]])
    !is.null(value) && !is.na(value[index]) && !is.finite(value[index])
  }, logical(1))]
}

prepare_data_explorer <- function(result, report_data = "summary") {
  control <- normalize_report_data_control(report_data)
  if (control$mode == "none") {
    return(list(
      mode = "none", profile = NULL, rows = NULL,
      manifest = list(mode = "none", individual_records = 0L, columns = character())
    ))
  }
  context <- result$data_context
  processed <- list(training = result$training_data, evaluation = result$test_data)
  available <- !is.null(context)
  if (available) validate_data_context(context, processed$training, processed$evaluation)
  variables <- unique(c(result$target_column, control$columns %||% result$features, control$context_columns))
  bad <- setdiff(control$columns %||% result$features, result$features)
  if (length(bad)) stop("Use context_columns to include non-model columns: ", paste(bad, collapse = ", "))
  raw <- if (available) context$raw else list()
  known <- if (available) context$columns$name else unique(c(names(processed$training), names(processed$evaluation)))
  absent <- setdiff(variables, known)
  if (length(absent)) stop("Requested report columns are unavailable: ", paste(absent, collapse = ", "))
  columns <- if (available) {
    context$columns[match(variables, context$columns$name), , drop = FALSE]
  } else {
    data.frame(
      name = variables, raw_type = "unavailable",
      model_type = vapply(variables, function(name) {
        paste(class((processed$training %||% processed$evaluation)[[name]]), collapse = "/")
      }, character(1)),
      role = ifelse(variables == result$target_column, "target", "predictor"), stringsAsFactors = FALSE
    )
  }
  rownames(columns) <- NULL
  positive <- if (identical(result$task, "binary")) {
    result$prediction_schema$positive %||%
      levels((processed$training %||% processed$evaluation)[[result$target_column]])[2L]
  } else {
    NULL
  }
  profile <- build_data_profile(
    raw, processed, if (available) context$row_map else NULL,
    columns, result$target_column, positive = positive
  )
  profile$task <- result$task %||% "unknown"
  profile$positive <- positive
  rows <- NULL
  seed <- control$seed %||% result$provenance$seed %||% 2026L
  if (control$mode == "rows") {
    reference <- if (available) raw else processed
    sizes <- vapply(reference[c("training", "evaluation")], function(data) nrow(data) %||% 0L, integer(1))
    selected <- data_sample_indices(sizes, control$max_rows, seed)
    rows <- list()
    for (i in seq_along(selected)) {
      partition <- c("training", "evaluation")[i]
      mapping <- if (available) context$row_map[context$row_map$partition == partition, , drop = FALSE] else NULL
      for (index in selected[[i]]) {
        processed_index <- if (available) mapping$processed_position[index] else index
        rows[[length(rows) + 1L]] <- list(
          row_key = if (available) mapping$row_key[index] else paste(partition, index, sep = ":"),
          partition = partition, source = if (available) mapping$source[index] else "processed position",
          source_row = if (available) mapping$source_row[index] else index,
          processed_position = processed_index,
          retained = !is.na(processed_index),
          nonfinite = list(
            raw = if (available) data_nonfinite_columns(raw[[partition]], index, variables) else character(),
            processed = data_nonfinite_columns(processed[[partition]], processed_index, variables)
          ),
          raw = if (available) {
            setNames(lapply(variables, function(name) {
              data_export_value(raw[[partition]][[name]], index)
            }), variables)
          } else {
            NULL
          },
          processed = setNames(lapply(variables, function(name) {
            data_export_value(processed[[partition]][[name]], processed_index)
          }), variables)
        )
      }
    }
  }
  n_supplied <- if (available) {
    (nrow(raw$training) %||% 0L) + (nrow(raw$evaluation) %||% 0L)
  } else {
    (nrow(processed$training) %||% 0L) + (nrow(processed$evaluation) %||% 0L)
  }
  list(
    mode = control$mode, profile = profile, rows = rows,
    manifest = list(
      mode = control$mode, columns = variables, individual_records = length(rows),
      full_rows = n_supplied, sampled = length(rows) > 0 && length(rows) < n_supplied,
      sampling = if (control$mode == "rows") {
        "Proportional split allocation; uniform sample without replacement within each split"
      } else {
        "No individual records exported"
      },
      seed = if (control$mode == "rows") seed else NULL,
      raw_status = if (available) "available" else "unavailable",
      training_available = !is.null(processed$training),
      scope = if (available) {
        "Raw supplied rows and processed retained rows are distinct populations."
      } else {
        "This older result has processed values only; raw training and excluded rows were not retained."
      },
      privacy = "Aggregate output is not anonymization. Anyone receiving row-mode HTML receives every embedded record."
    )
  )
}
