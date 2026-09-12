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

categorical_replication_summary <- function(x) {
  x <- x[!is.na(x)]
  observed <- unique(x)
  counts <- tabulate(match(x, observed), nbins = length(observed))
  singletons <- sum(counts == 1L)
  list(n_categories = length(counts), n_singleton_rows = singletons,
       singleton_fraction = if (length(x)) singletons / length(x) else 0,
       n_replicated_rows = length(x) - singletons,
       n_repeated_categories = sum(counts > 1L))
}

data_pair_association <- function(x, y) {
  kinds <- vapply(list(x, y), data_column_kind, character(1))
  categorical <- list(x = NULL, y = NULL)
  unavailable <- function(reason, n = 0L) {
    list(status = "unavailable", method = NULL, value = NA_real_, n = n, reason = reason,
         categorical = categorical)
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
  if (!x_numeric) categorical$x <- categorical_replication_summary(x)
  if (!y_numeric) categorical$y <- categorical_replication_summary(y)
  if (n < 3L || length(unique(x)) < 2L || length(unique(y)) < 2L) {
    return(unavailable("At least three complete pairs and variation in both columns are needed.", n))
  }
  if (any(vapply(categorical, function(value) {
    !is.null(value) && value$n_repeated_categories == 0L
  }, logical(1)))) {
    return(unavailable(paste("A categorical column has no repeated categories among complete pairs;",
                             "association cannot separate group structure from individual identifiers."), n))
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
    categorical = categorical,
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
    # Build columns once. One data frame per occupied bin is expensive when a
    # report contains hundreds of pairs in both raw and processed partitions.
    conditional <- data.frame(
      x = as.integer(names(groups)), n = lengths(groups),
      mean = vapply(groups, mean, numeric(1)),
      median = vapply(groups, stats::median, numeric(1)),
      min = vapply(groups, min, numeric(1)), max = vapply(groups, max, numeric(1))
    )
    rownames(conditional) <- NULL
  }
  if (!is.null(positive) && any(ok)) {
    groups <- split(as.character(y[ok]) == positive, a[ok])
    conditional_event <- data.frame(
      x = as.integer(names(groups)), n = lengths(groups),
      events = vapply(groups, sum, integer(1)), rate = vapply(groups, mean, numeric(1))
    )
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

data_pair_samples <- function(raw, processed, row_map, maximum, seed) {
  # Keep selected source rows in both views. Filling from the remaining retained
  # rows gives a uniform sample of the processed population even after exclusions.
  draw <- function(n, size) {
    if (size >= n) return(seq_len(n))
    sort(sample.int(n, size, useHash = size <= n / 2))
  }
  with_preserved_seed(seed, {
    selected <- list(raw = list(), processed = list())
    for (partition in c("training", "evaluation")) {
      raw_n <- nrow(raw[[partition]]) %||% 0L
      processed_n <- nrow(processed[[partition]]) %||% 0L
      raw_size <- min(raw_n, maximum %||% raw_n)
      processed_size <- min(processed_n, maximum %||% processed_n)
      chosen <- draw(raw_n, raw_size)
      selected$raw[[partition]] <- chosen
      mapping <- if (is.null(row_map)) NULL else row_map$processed_position[row_map$partition == partition]
      if (!is.null(mapping) && length(mapping) == raw_n && raw_n > 0L) {
        retained <- mapping[chosen]
        retained <- retained[!is.na(retained)]
        if (length(retained) < processed_size) {
          remaining <- setdiff(seq_len(processed_n), retained)
          retained <- c(retained, remaining[draw(length(remaining), processed_size - length(retained))])
        }
        selected$processed[[partition]] <- sort(retained)
      } else {
        selected$processed[[partition]] <- draw(processed_n, processed_size)
      }
    }
    selected
  })
}

build_data_profile <- function(raw, processed, row_map, columns, target, bins = 24L,
                               max_pairs = 512L, positive = NULL, max_pair_rows = 10000L,
                               seed = 2026L) {
  bins <- assert_count(bins, "bins", minimum = 2L)
  variables <- columns$name
  stages <- list()
  inputs <- list(raw = raw, processed = processed)
  selected <- data_pair_samples(raw, processed, row_map, max_pair_rows, seed)
  pair_indices <- data_pair_indices(length(variables), match(target, variables), max_pairs)
  for (stage in names(inputs)) {
    data <- inputs[[stage]]
    if (is.null(data$training) && is.null(data$evaluation)) next
    profiles <- setNames(lapply(variables, function(name) {
      if (stage == "processed" && !is.null(stages$raw) &&
            identical(data$training[[name]], raw$training[[name]]) &&
            identical(data$evaluation[[name]], raw$evaluation[[name]]) &&
            !(is.null(data$training[[name]]) && is.null(data$evaluation[[name]]))) {
        return(stages$raw$columns[[name]])
      }
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
    sampling <- lapply(c("training", "evaluation"), function(partition) {
      n <- nrow(data[[partition]]) %||% 0L
      size <- length(selected[[stage]][[partition]])
      list(n_population = n, n_sample = size, sampled = size < n)
    })
    names(sampling) <- c("training", "evaluation")
    pair_data <- lapply(names(sampling), function(partition) {
      indices <- selected[[stage]][[partition]]
      setNames(lapply(variables, function(name) {
        value <- data[[partition]][[name]]
        if (length(indices) == length(value)) value else value[indices]
      }), variables)
    })
    names(pair_data) <- names(sampling)
    pair_profile <- function(x, y, partition) {
      value <- data_pair_profile(
        pair_data[[partition]][[x]], pair_data[[partition]][[y]],
        profiles[[x]]$axis, profiles[[y]]$axis, sampling[[partition]]$n_sample,
        positive = if (y == target) positive else NULL
      )
      c(value, sampling[[partition]])
    }
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
          training = pair_profile(x, y, "training"),
          evaluation = pair_profile(x, y, "evaluation")
        )
      }
    }
    stages[[stage]] <- list(
      columns = profiles, pairs = pairs,
      population = if (stage == "raw") "All supplied rows before preprocessing" else "Retained model rows",
      training_rows = nrow(data$training), evaluation_rows = nrow(data$evaluation),
      pair_sampling = sampling,
      training_available = !is.null(data$training), evaluation_available = !is.null(data$evaluation)
    )
  }
  ledger <- if (is.null(row_map)) {
    NULL
  } else {
    do.call(rbind, lapply(unique(row_map$partition), function(partition) {
      retained <- row_map$retained[row_map$partition == partition]
      data.frame(
        partition = partition, supplied = length(retained), retained = sum(retained),
        excluded = sum(!retained)
      )
    }))
  }
  list(
    schema_version = "1.0", columns = columns, target = target, stages = stages, row_ledger = ledger,
    pair_coverage = list(
      included = nrow(pair_indices), total = choose(length(variables), 2L),
      policy = "Target pairs first, then original column order; all individual profiles are retained"
    ),
    pair_sampling = list(
      max_rows = max_pair_rows, seed = seed,
      method = paste("Uniform sample without replacement per partition;",
                     "shared source rows across raw and processed views where retained"),
      scope = paste("Relationship counts, conditional summaries and associations describe the analyzed sample.",
                    "Individual-column summaries use all rows. Rare groups may be missed by sampling.")
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

data_export_columns <- function(data, indices, variables) {
  nonfinite <- setNames(vector("list", length(variables)), variables)
  columns <- vector("list", length(variables))
  for (i in seq_along(variables)) {
    value <- data[[variables[i]]]
    kind <- data_column_kind(value)
    if (is.null(value) || kind == "unsupported") {
      columns[[i]] <- rep(NA, length(indices))
      nonfinite[[i]] <- integer()
      next
    }
    value <- value[indices]
    number <- data_numeric(value)
    if (!is.null(number)) {
      nonfinite[[i]] <- which(!is.na(number) & !is.finite(number))
      if (kind == "numeric") value[!is.finite(value)] <- NA
    }
    columns[[i]] <- if (kind %in% c("date", "datetime")) {
      as.numeric(value)
    } else if (kind == "categorical") {
      as.character(value)
    } else {
      value
    }
  }
  names(columns) <- variables
  list(values = columns, nonfinite = nonfinite)
}

data_export_partition <- function(data, indices, variables) {
  if (!length(indices)) return(list(values = list(), nonfinite = list()))
  exported <- data_export_columns(data, indices, variables)
  nonfinite <- rep(list(character()), length(indices))
  for (name in variables) {
    for (row in exported$nonfinite[[name]]) nonfinite[[row]] <- c(nonfinite[[row]], name)
  }
  list(
    values = lapply(seq_along(indices), function(row) {
      lapply(exported$values, function(column) {
        value <- column[row]
        if (is.na(value)) NULL else unname(value)
      })
    }),
    nonfinite = nonfinite
  )
}

data_combine_export_columns <- function(partitions, variables, sizes) {
  values <- setNames(lapply(variables, function(name) {
    columns <- lapply(partitions, function(partition) partition$values[[name]])
    columns <- columns[lengths(columns) > 0L]
    if (!length(columns)) return(logical())
    kinds <- unique(vapply(columns, typeof, character(1)))
    # Raw training and evaluation may have different types. A list preserves
    # individual values instead of coercing numeric observations to text.
    if (length(kinds) > 1L) {
      columns <- lapply(columns, as.list)
    }
    do.call(c, unname(columns))
  }), variables)
  offsets <- c(0L, head(cumsum(sizes), -1L))
  nonfinite <- setNames(lapply(variables, function(name) {
    unlist(lapply(seq_along(partitions), function(i) {
      (partitions[[i]]$nonfinite[[name]] %||% integer()) + offsets[i]
    }), use.names = FALSE)
  }), variables)
  list(values = values, nonfinite = nonfinite)
}

prepare_data_explorer <- function(result, report_data = "summary", row_layout = c("records", "columns")) {
  row_layout <- match.arg(row_layout)
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
  seed <- control$seed %||% result$provenance$seed %||% 2026L
  profile <- build_data_profile(
    raw, processed, if (available) context$row_map else NULL,
    columns, result$target_column, positive = positive,
    max_pair_rows = control$max_pair_rows, seed = seed
  )
  profile$task <- result$task %||% "unknown"
  profile$positive <- positive
  rows <- NULL
  if (control$mode == "rows") {
    reference <- if (available) raw else processed
    sizes <- vapply(reference[c("training", "evaluation")], function(data) nrow(data) %||% 0L, integer(1))
    selected <- data_sample_indices(sizes, control$max_rows, seed)
    rows <- list()
    column_partitions <- list(raw = list(), processed = list(), meta = list())
    for (i in seq_along(selected)) {
      partition <- c("training", "evaluation")[i]
      mapping <- if (available) {
        context$row_map[which(context$row_map$partition == partition)[selected[[i]]], , drop = FALSE]
      } else {
        NULL
      }
      processed_indices <- if (available) mapping$processed_position else selected[[i]]
      if (row_layout == "columns") {
        if (available) column_partitions$raw[[i]] <- data_export_columns(raw[[partition]], selected[[i]], variables)
        column_partitions$processed[[i]] <- data_export_columns(processed[[partition]], processed_indices, variables)
        count <- length(selected[[i]])
        column_partitions$meta[[i]] <- list(
          row_key = if (available) mapping$row_key else if (count) {
            paste(partition, selected[[i]], sep = ":")
          } else {
            character()
          },
          partition = rep(partition, count),
          source = if (available) mapping$source else rep("processed position", count),
          source_row = if (available) mapping$source_row else selected[[i]],
          processed_position = processed_indices, retained = !is.na(processed_indices)
        )
        next
      }
      # Classify and convert each selected column once, rather than once for
      # every row. Excluded rows keep NA processed positions and null values.
      raw_export <- if (available) data_export_partition(raw[[partition]], selected[[i]], variables) else NULL
      processed_export <- data_export_partition(processed[[partition]], processed_indices, variables)
      for (row in seq_along(selected[[i]])) {
        index <- selected[[i]][row]
        processed_index <- processed_indices[row]
        rows[[length(rows) + 1L]] <- list(
          row_key = if (available) mapping$row_key[row] else paste(partition, index, sep = ":"),
          partition = partition, source = if (available) mapping$source[row] else "processed position",
          source_row = if (available) mapping$source_row[row] else index,
          processed_position = processed_index,
          retained = !is.na(processed_index),
          nonfinite = list(
            raw = if (available) raw_export$nonfinite[[row]] else character(),
            processed = processed_export$nonfinite[[row]]
          ),
          raw = if (available) raw_export$values[[row]] else NULL,
          processed = processed_export$values[[row]]
        )
      }
    }
    if (row_layout == "columns") {
      sizes <- lengths(selected)
      raw_columns <- if (available) data_combine_export_columns(column_partitions$raw, variables, sizes) else NULL
      processed_columns <- data_combine_export_columns(column_partitions$processed, variables, sizes)
      meta <- lapply(names(column_partitions$meta[[1L]]), function(name) {
        unlist(lapply(column_partitions$meta, `[[`, name), use.names = FALSE)
      })
      names(meta) <- names(column_partitions$meta[[1L]])
      rows <- list(
        layout = "columns-v1", length = sum(sizes), meta = meta,
        raw = raw_columns$values, processed = processed_columns$values,
        nonfinite = list(raw = raw_columns$nonfinite, processed = processed_columns$nonfinite)
      )
    }
  }
  n_supplied <- if (available) {
    (nrow(raw$training) %||% 0L) + (nrow(raw$evaluation) %||% 0L)
  } else {
    (nrow(processed$training) %||% 0L) + (nrow(processed$evaluation) %||% 0L)
  }
  n_exported <- if (identical(rows$layout, "columns-v1")) rows$length else length(rows)
  list(
    mode = control$mode, profile = profile, rows = rows,
    manifest = list(
      mode = control$mode, columns = variables, individual_records = n_exported,
      full_rows = n_supplied, sampled = n_exported > 0 && n_exported < n_supplied,
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
