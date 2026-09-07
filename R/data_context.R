# Raw observations are retained locally, independently of the HTML export policy.
# Row keys identify source positions; input row names are never browser IDs.
capture_data_context <- function(training, evaluation, target, features,
                                 training_processed, evaluation_processed,
                                 training_source_rows = seq_len(nrow(training) %||% 0L),
                                 evaluation_source_rows = seq_len(nrow(evaluation)),
                                 evaluation_source = "test_data", split_method = NULL,
                                 split_column = NULL, excluded = NULL,
                                 excluded_source_rows = integer(), training_source = "data") {
  for (value in list(training, evaluation)) {
    if (is.null(value)) next
    assert_data_frame(value, "raw partition")
    if (anyDuplicated(names(value))) stop("Raw data columns must have unique names.", call. = FALSE)
    if (!target %in% names(value)) stop("Raw partitions must contain the target.", call. = FALSE)
  }
  if (!evaluation_source %in% c("data", "test_data")) {
    stop("`evaluation_source` must be data or test_data.", call. = FALSE)
  }
  if (!training_source %in% c("data", "training_data")) {
    stop("`training_source` must be data or training_data.", call. = FALSE)
  }
  make_map <- function(raw, processed, source_rows, partition, source) {
    if (is.null(raw)) {
      if (!is.null(processed$data) || length(source_rows) || length(processed$row_indices)) {
        stop("An unavailable raw partition cannot have processed rows or source positions.", call. = FALSE)
      }
      return(data.frame(
        row_key = character(), source = character(), source_row = integer(),
        partition = character(), raw_position = integer(), processed_position = integer(),
        retained = logical(), reason = character(), stringsAsFactors = FALSE
      ))
    }
    if (length(source_rows) != nrow(raw) || anyNA(source_rows) ||
          any(source_rows < 1 | source_rows != as.integer(source_rows)) || anyDuplicated(source_rows)) {
      stop("Source positions must identify each raw partition row exactly once.", call. = FALSE)
    }
    indices <- processed$row_indices
    if (!is.data.frame(processed$data) || is.null(indices) ||
          length(indices) != nrow(processed$data) || anyNA(indices) || anyDuplicated(indices) ||
          any(indices < 1 | indices > nrow(raw) | indices != as.integer(indices))) {
      stop("Processed row indices must map exactly to the raw partition.", call. = FALSE)
    }
    position <- match(seq_len(nrow(raw)), indices)
    data.frame(
      row_key = paste(source, source_rows, sep = ":"), source = source,
      source_row = as.integer(source_rows), partition = partition,
      raw_position = seq_len(nrow(raw)), processed_position = position,
      retained = !is.na(position),
      reason = ifelse(is.na(position), "Removed by preprocessing", "Retained"),
      stringsAsFactors = FALSE
    )
  }
  mapping <- rbind(
    make_map(training, training_processed, training_source_rows, "training", training_source),
    make_map(evaluation, evaluation_processed, evaluation_source_rows, "evaluation", evaluation_source)
  )
  if (!is.null(excluded) && nrow(excluded)) {
    excluded_map <- make_map(
      excluded,
      list(data = excluded[FALSE, , drop = FALSE], row_indices = integer()),
      excluded_source_rows, "excluded", "data"
    )
    excluded_map$reason <- "Excluded by validation design"
    mapping <- rbind(mapping, excluded_map)
  }
  if (anyDuplicated(mapping$row_key)) {
    stop("Source row keys overlap across partitions; supply original source positions.", call. = FALSE)
  }
  variables <- unique(c(names(training), names(evaluation)))
  columns <- data.frame(
    name = variables,
    raw_type = vapply(variables, function(name) {
      paste(class((training[[name]] %||% evaluation[[name]])), collapse = "/")
    }, character(1)),
    model_type = vapply(variables, function(name) {
      value <- (training_processed$data %||% evaluation_processed$data)[[name]]
      if (is.null(value)) "not used" else paste(class(value), collapse = "/")
    }, character(1)),
    role = ifelse(variables == target, "target", ifelse(variables %in% features, "predictor",
      ifelse(variables %in% split_column, "split", "context or removed")
    )), stringsAsFactors = FALSE
  )
  raw <- list(training = training, evaluation = evaluation, excluded = excluded)
  structure(list(
    schema_version = "1.0", status = "available", target = target,
    raw = raw, columns = columns, row_map = mapping,
    provenance = list(
      split_method = split_method, split_column = split_column,
      raw_fingerprint = content_fingerprint(list(raw, mapping)),
      processed_fingerprints = list(
        training = content_fingerprint(training_processed$data),
        evaluation = content_fingerprint(evaluation_processed$data)
      )
    )
  ), class = "autoxplain_data_context")
}

validate_data_context <- function(context, training, evaluation) {
  if (!identical(context$schema_version, "1.0") || !is.data.frame(context$row_map)) {
    stop("Unsupported retained data context; refit the analysis.", call. = FALSE)
  }
  if (!identical(
    context$provenance$raw_fingerprint,
    content_fingerprint(list(context$raw, context$row_map))
  )) {
    stop("Retained raw data or row mapping changed; recapture the data context.", call. = FALSE)
  }
  expected <- context$provenance$processed_fingerprints
  if (!identical(expected$training, content_fingerprint(training)) ||
        !identical(expected$evaluation, content_fingerprint(evaluation))) {
    stop("Retained data context does not match the processed result rows.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Control the data included in a standalone report
#'
#' The default includes aggregate distributions and relationships. Row export
#' is explicit: anyone receiving the HTML receives every embedded record, even
#' when browser filters hide it. Aggregate output is not anonymization; category
#' labels, small groups and fitted model details can still disclose information.
#'
#' @param mode `"summary"`, `"rows"`, or `"none"`. Strings can also be passed
#'   directly as `report_data` when fitting or rendering a report.
#' @param columns Predictor names to include. `NULL` includes all model inputs.
#'   The target is included automatically. These columns must be model inputs;
#'   use `context_columns` to explicitly include other retained columns.
#'   This selects data-explorer values; it does not redact feature names,
#'   fitted-model details or explanations elsewhere in the report.
#' @param context_columns Additional retained raw columns to export explicitly,
#'   such as a site or time column excluded from fitting. Empty by default.
#' @param max_rows Maximum exported records across training and evaluation.
#'   Sampling allocates rows proportionally between splits and samples uniformly
#'   within each split. Unfiltered aggregate profiles use the full available
#'   data for each stage; browser row filters describe only the exported sample
#'   and leave model scores unchanged.
#' @param seed Optional sampling seed. `NULL` uses the analysis seed, or 2026.
#' @return An `autoxplain_report_data_control` object.
#' @export
report_data_control <- function(mode = c("summary", "rows", "none"), columns = NULL,
                                context_columns = character(), max_rows = 5000L, seed = NULL) {
  mode <- match.arg(mode)
  check_columns <- function(value, name) {
    if (!is.null(value) && (!is.character(value) || anyNA(value) ||
                              any(!nzchar(value)) || anyDuplicated(value))) {
      stop("`", name, "` must contain unique non-empty column names.", call. = FALSE)
    }
  }
  check_columns(columns, "columns")
  check_columns(context_columns, "context_columns")
  max_rows <- assert_count(max_rows, "max_rows", minimum = 2L)
  if (!is.null(seed)) seed <- assert_count(seed, "seed", minimum = 0L)
  structure(list(
    mode = mode, columns = columns, context_columns = context_columns,
    max_rows = max_rows, seed = seed
  ), class = "autoxplain_report_data_control")
}

normalize_report_data_control <- function(report_data) {
  if (inherits(report_data, "autoxplain_report_data_control")) {
    return(do.call(report_data_control, unclass(report_data)))
  }
  if (!is.character(report_data) || length(report_data) != 1L || is.na(report_data)) {
    stop("`report_data` must be summary, rows, none, or report_data_control().", call. = FALSE)
  }
  report_data_control(report_data)
}
