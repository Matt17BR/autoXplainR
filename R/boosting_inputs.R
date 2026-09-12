resolve_boosting_encoding <- function(parameters, data, target) {
  requested <- parameters$encoding %||% "auto"
  features <- setdiff(names(data), target)
  width <- sum(vapply(data[features], function(column) {
    if (is.factor(column) || is.character(column)) max(1L, length(unique(column)) - 1L) else 1L
  }, integer(1)))
  cells <- as.double(nrow(data)) * width
  expanded <- width > 2 * length(features)
  native <- requested == "native" || (requested == "auto" && expanded && cells > 5e7)
  list(
    requested = requested, encoding = if (native) "native" else "matrix",
    matrix_columns_estimate = width, matrix_cells_estimate = cells,
    rows = nrow(data), input_columns = length(features),
    reason = if (requested != "auto") {
      "Explicit input encoding."
    } else if (native) {
      paste(
        "Categorical expansion exceeds twice the input width and 50 million matrix cells;",
        "native categorical splits avoid constructing that dense expansion."
      )
    } else {
      "The estimated categorical expansion fits below the automatic native-encoding threshold."
    },
    matrix_type = if (native) "QuantileDMatrix" else "DMatrix",
    max_bin = 256L,
    scope = paste(
      "Native encoding uses category partitions instead of numeric contrasts and can change predictions.",
      "This is a computation policy, not a guarantee of memory use or predictive quality."
    )
  )
}

fit_boosting_native_blueprint <- function(data, features) {
  kinds <- vapply(data[features], matrix_blueprint_predictor_kind, character(1))
  if (anyNA(kinds)) stop("Unsupported native boosting predictor type.", call. = FALSE)
  levels <- lapply(data[features][kinds == "categorical"], function(x) {
    if (is.factor(x)) levels(droplevels(x)) else levels(factor(x))
  })
  structure(list(
    predictors = features, predictor_kinds = kinds, factor_levels = levels,
    training_rows = nrow(data),
    feature_map = stats::setNames(sprintf("input_%06d", seq_along(features)), features),
    categorical_encoding = "native", output = "data.frame"
  ), class = "autoxplain_boosting_blueprint")
}

bake_boosting_native_blueprint <- function(blueprint, data) {
  output <- data[blueprint$predictors]
  kinds <- vapply(output, matrix_blueprint_predictor_kind, character(1))
  changed <- is.na(kinds) | kinds != blueprint$predictor_kinds
  if (any(changed)) {
    stop("Native boosting predictor types changed: ", paste(names(output)[changed], collapse = ", "), ".",
      call. = FALSE
    )
  }
  for (name in names(output)) {
    if (name %in% names(blueprint$factor_levels)) {
      value <- as.character(output[[name]])
      unknown <- !is.na(value) & !value %in% blueprint$factor_levels[[name]]
      if (any(unknown)) stop("Native boosting predictor `", name, "` has unseen levels.", call. = FALSE)
      output[[name]] <- factor(value, levels = blueprint$factor_levels[[name]])
    } else {
      if (!is.numeric(output[[name]]) && !is.logical(output[[name]])) {
        stop("Native boosting predictor `", name, "` must be numeric or logical.", call. = FALSE)
      }
      output[[name]] <- as.numeric(output[[name]])
      if (any(!is.finite(output[[name]]))) {
        stop("Native boosting predictor `", name, "` must contain finite values.", call. = FALSE)
      }
    }
    if (anyNA(output[[name]])) stop("Native boosting predictors must not contain missing values.", call. = FALSE)
  }
  names(output) <- unname(blueprint$feature_map)
  output
}
