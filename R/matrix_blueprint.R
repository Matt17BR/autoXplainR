fit_matrix_blueprint <- function(data,
                                 predictors = names(data),
                                 center = FALSE,
                                 scale = FALSE,
                                 intercept = FALSE,
                                 categorical_encoding = c("treatment", "one_hot"),
                                 output = c("dense", "sparse")) {
  assert_data_frame(data, "data")
  if (anyDuplicated(names(data))) {
    stop("`data` must have unique column names.", call. = FALSE)
  }
  if (!nrow(data)) {
    stop("`data` must contain at least one training row.", call. = FALSE)
  }
  if (!is.character(predictors) || !length(predictors) ||
        anyNA(predictors) || any(!nzchar(predictors)) || anyDuplicated(predictors)) {
    stop("`predictors` must contain one or more unique column names.", call. = FALSE)
  }
  missing_predictors <- setdiff(predictors, names(data))
  if (length(missing_predictors)) {
    stop(
      "Training data is missing predictor columns: ",
      paste(missing_predictors, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  assert_matrix_blueprint_flag(center, "center")
  assert_matrix_blueprint_flag(scale, "scale")
  assert_matrix_blueprint_flag(intercept, "intercept")
  categorical_encoding <- match.arg(categorical_encoding)
  output <- match.arg(output)
  if (identical(output, "sparse")) {
    require_optional("Matrix", "encoding sparse model inputs")
    if (isTRUE(center) || isTRUE(scale)) {
      stop("Sparse encoding delegates centering and scaling to the learner.", call. = FALSE)
    }
  }

  training <- data[predictors]
  kinds <- vapply(training, matrix_blueprint_predictor_kind, character(1L))
  unsupported <- names(kinds)[is.na(kinds)]
  if (length(unsupported)) {
    stop(
      "Matrix encoding supports numeric, logical, factor, and character predictors. ",
      "Convert or remove: ", paste(unsupported, collapse = ", "), ".",
      call. = FALSE
    )
  }
  missing_values <- names(training)[vapply(training, anyNA, logical(1L))]
  if (length(missing_values)) {
    stop(
      "Training predictors must not contain missing values: ",
      paste(missing_values, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  invalid_numeric <- names(training)[vapply(training, function(column) {
    is.numeric(column) && any(!is.finite(column))
  }, logical(1L))]
  if (length(invalid_numeric)) {
    stop(
      "Training numeric predictors must be finite: ",
      paste(invalid_numeric, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  ordered_predictors <- vapply(training, is.ordered, logical(1L))
  categorical <- kinds == "categorical"
  training[categorical] <- lapply(training[categorical], function(column) {
    if (is.factor(column)) {
      droplevels(column)
    } else {
      factor(column)
    }
  })
  insufficient_levels <- names(training)[categorical & vapply(
    training,
    function(column) !is.factor(column) || nlevels(column) < 2L,
    logical(1L)
  )]
  if (length(insufficient_levels)) {
    stop(
      "Categorical training predictors need at least two observed levels: ",
      paste(insufficient_levels, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  formula <- stats::as.formula("~ .")
  environment(formula) <- baseenv()
  frame <- stats::model.frame(
    formula,
    data = training,
    na.action = stats::na.fail,
    drop.unused.levels = TRUE
  )
  terms <- stats::terms(frame)
  encoding_contrasts <- if (identical(categorical_encoding, "one_hot")) {
    nominal <- categorical | kinds == "logical"
    lapply(training[nominal], stats::contrasts, contrasts = FALSE)
  } else {
    NULL
  }
  encoded <- if (identical(output, "sparse")) {
    sparse_blueprint_matrix(terms, frame, encoding_contrasts)
  } else if (length(encoding_contrasts)) {
    stats::model.matrix(terms, frame, contrasts.arg = encoding_contrasts)
  } else {
    stats::model.matrix(terms, frame)
  }
  contrasts <- attr(encoded, "contrasts")
  if (is.null(contrasts)) contrasts <- list()
  column_assign <- attr(encoded, "assign")
  if (!isTRUE(intercept) && "(Intercept)" %in% colnames(encoded)) {
    keep <- colnames(encoded) != "(Intercept)"
    encoded <- encoded[, keep, drop = FALSE]
    column_assign <- column_assign[keep]
  }
  if (!ncol(encoded)) {
    stop("Training predictors produced no encoded columns.", call. = FALSE)
  }
  if (!matrix_blueprint_values_finite(encoded)) {
    stop("Training predictors produced non-finite encoded values.", call. = FALSE)
  }

  # A generated name such as segmentB can also be a literal numeric column.
  # Preserve both inputs and their source mapping without sending duplicate
  # column names to engines that identify inputs by name.
  colnames(encoded) <- make.unique(colnames(encoded))
  columns <- colnames(encoded)
  term_predictors <- all.vars(terms)
  column_predictors <- rep(NA_character_, length(columns))
  assigned <- column_assign > 0L
  column_predictors[assigned] <- term_predictors[column_assign[assigned]]
  names(column_predictors) <- columns

  # Standardizing dummy columns by their prevalence changes the distance between
  # otherwise symmetric categories. In distance-oriented encodings, only genuine
  # numeric predictors are standardized; one-hot coordinates remain exactly 0/1.
  standardizable <- rep(TRUE, length(columns))
  if (identical(categorical_encoding, "one_hot")) {
    standardizable <- !is.na(column_predictors) &
      kinds[column_predictors] == "numeric"
  }
  standardizable[is.na(standardizable)] <- FALSE
  names(standardizable) <- columns

  center_values <- stats::setNames(rep(0, length(columns)), columns)
  if (isTRUE(center) && any(standardizable)) {
    center_values[standardizable] <- colMeans(encoded[, standardizable, drop = FALSE])
    # On platforms without extended precision, a finite column's sum can
    # overflow before colMeans divides by its length. Keep ordinary results,
    # and recompute only those centers using bounded, unit-sized coordinates.
    for (column in which(!is.finite(center_values))) {
      values <- encoded[, column]
      magnitude <- max(abs(values))
      center_values[[column]] <- mean(values / magnitude) * magnitude
    }
  }
  scale_values <- stats::setNames(rep(1, length(columns)), columns)
  zero_variance_columns <- character()
  if (isTRUE(scale) && any(standardizable)) {
    learned_scale <- apply(
      encoded[, standardizable, drop = FALSE], 2L, matrix_blueprint_sd
    )
    if (any(!is.finite(learned_scale))) {
      stop(
        "Predictor scales exceed the finite numeric range: ",
        paste(names(learned_scale)[!is.finite(learned_scale)], collapse = ", "),
        ". Rescale these input units before fitting.", call. = FALSE
      )
    }
    zero_variance_columns <- names(learned_scale)[learned_scale == 0]
    learned_scale[learned_scale == 0] <- 1
    scale_values[names(learned_scale)] <- learned_scale
  }
  if ("(Intercept)" %in% columns) {
    center_values[["(Intercept)"]] <- 0
    scale_values[["(Intercept)"]] <- 1
    zero_variance_columns <- setdiff(zero_variance_columns, "(Intercept)")
  }

  structure(
    list(
      predictors = predictors,
      predictor_kinds = kinds,
      ordered_predictors = ordered_predictors,
      terms = terms,
      xlevels = lapply(training[categorical], levels),
      contrasts = contrasts,
      columns = columns,
      unique_column_names = TRUE,
      column_predictors = column_predictors,
      intercept = isTRUE(intercept),
      categorical_encoding = categorical_encoding,
      centered = isTRUE(center),
      scaled = isTRUE(scale),
      center = center_values,
      scale = scale_values,
      standardized_columns = columns[standardizable],
      zero_variance_columns = zero_variance_columns,
      training_rows = nrow(training),
      output = output
    ),
    class = "autoxplain_matrix_blueprint"
  )
}

bake_matrix_blueprint <- function(blueprint, newdata) {
  if (!inherits(blueprint, "autoxplain_matrix_blueprint")) {
    stop("`blueprint` must be created by `fit_matrix_blueprint()`.", call. = FALSE)
  }
  assert_data_frame(newdata, "newdata")
  if (anyDuplicated(names(newdata))) {
    stop("`newdata` must have unique column names.", call. = FALSE)
  }
  missing_predictors <- setdiff(blueprint$predictors, names(newdata))
  if (length(missing_predictors)) {
    stop(
      "New data is missing predictor columns: ",
      paste(missing_predictors, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  data <- newdata[blueprint$predictors]
  missing_values <- names(data)[vapply(data, anyNA, logical(1L))]
  if (length(missing_values)) {
    stop(
      "New predictor values must not be missing: ",
      paste(missing_values, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  for (predictor in blueprint$predictors) {
    expected <- blueprint$predictor_kinds[[predictor]]
    column <- data[[predictor]]
    if (identical(expected, "numeric")) {
      if (!is.numeric(column) || is.complex(column)) {
        stop("New predictor `", predictor, "` must be numeric.", call. = FALSE)
      }
      if (any(!is.finite(column))) {
        stop("New numeric predictor `", predictor, "` must be finite.", call. = FALSE)
      }
    } else if (identical(expected, "logical")) {
      if (!is.logical(column)) {
        stop("New predictor `", predictor, "` must be logical.", call. = FALSE)
      }
    } else {
      if (!is.factor(column) && !is.character(column)) {
        stop(
          "New predictor `", predictor, "` must be categorical (factor or character).",
          call. = FALSE
        )
      }
      values <- as.character(column)
      unseen <- setdiff(unique(values), blueprint$xlevels[[predictor]])
      if (length(unseen)) {
        stop(
          "New predictor `", predictor, "` contains unseen levels: ",
          paste(unseen, collapse = ", "),
          ".",
          call. = FALSE
        )
      }
      data[[predictor]] <- factor(
        values,
        levels = blueprint$xlevels[[predictor]],
        ordered = isTRUE(blueprint$ordered_predictors[[predictor]])
      )
    }
  }

  frame <- stats::model.frame(
    blueprint$terms,
    data = data,
    xlev = blueprint$xlevels,
    na.action = stats::na.fail
  )
  encoded <- if (identical(blueprint$output, "sparse")) {
    require_optional("Matrix", "encoding sparse model inputs")
    sparse_blueprint_matrix(blueprint$terms, frame, blueprint$contrasts)
  } else if (length(blueprint$contrasts)) {
    stats::model.matrix(
      blueprint$terms,
      frame,
      contrasts.arg = blueprint$contrasts
    )
  } else {
    stats::model.matrix(blueprint$terms, frame)
  }
  if (!isTRUE(blueprint$intercept) && "(Intercept)" %in% colnames(encoded)) {
    encoded <- encoded[, colnames(encoded) != "(Intercept)", drop = FALSE]
  }
  if (isTRUE(blueprint$unique_column_names)) {
    colnames(encoded) <- make.unique(colnames(encoded))
  }
  if (!identical(colnames(encoded), blueprint$columns)) {
    stop(
      "Encoded columns do not match the training blueprint. Expected: ",
      paste(blueprint$columns, collapse = ", "),
      "; produced: ", paste(colnames(encoded), collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (!matrix_blueprint_values_finite(encoded)) {
    stop("New predictors produced non-finite encoded values.", call. = FALSE)
  }

  if (identical(blueprint$output, "sparse")) return(encoded)
  raw_encoded <- encoded
  encoded <- sweep(encoded, 2L, blueprint$center, "-")
  encoded <- sweep(encoded, 2L, blueprint$scale, "/")
  # The subtraction can overflow even when the standardized result is finite.
  # Dividing first is a fallback for those columns, preserving ordinary results.
  overflow <- which(colSums(!is.finite(encoded)) > 0L)
  for (column in overflow) {
    encoded[, column] <- raw_encoded[, column] / blueprint$scale[[column]] -
      blueprint$center[[column]] / blueprint$scale[[column]]
  }
  if (!matrix_blueprint_values_finite(encoded)) {
    stop(
      "Standardized predictor values exceed the finite numeric range. ",
      "Check the input units against the training data.", call. = FALSE
    )
  }
  storage.mode(encoded) <- "double"
  encoded
}

matrix_blueprint_values_finite <- function(x) {
  if (inherits(x, "sparseMatrix")) return(all(is.finite(x@x)))
  all(is.finite(x))
}

sparse_blueprint_matrix <- function(terms, frame, contrasts) {
  # Matrix's formula encoder interprets punctuation in literal predictor names.
  # Encode the same additive columns using safe internal names, then restore the
  # names stats::model.matrix would produce, including contrast suffixes.
  original_names <- names(frame)
  safe_names <- paste0(".ax_sparse", seq_along(frame))
  names(frame) <- safe_names
  safe_terms <- stats::terms(stats::as.formula("~ ."), data = frame)
  attr(frame, "terms") <- safe_terms
  if (length(contrasts)) {
    names(contrasts) <- safe_names[match(names(contrasts), original_names)]
  }
  encoded <- Matrix::sparse.model.matrix(safe_terms, frame, contrasts.arg = contrasts)
  assignment <- attr(encoded, "assign")
  assigned <- which(assignment > 0L)
  labels <- attr(terms, "term.labels")
  colnames(encoded)[assigned] <- paste0(
    labels[assignment[assigned]],
    substring(colnames(encoded)[assigned], nchar(safe_names[assignment[assigned]]) + 1L)
  )
  restored_contrasts <- attr(encoded, "contrasts")
  if (length(restored_contrasts)) {
    names(restored_contrasts) <- original_names[match(names(restored_contrasts), safe_names)]
    attr(encoded, "contrasts") <- restored_contrasts
  }
  encoded
}

matrix_blueprint_sd <- function(x) {
  value <- stats::sd(x)
  magnitude <- max(abs(x))
  ordinary_units <- magnitude >= sqrt(.Machine$double.xmin) &&
    magnitude <= sqrt(.Machine$double.xmax)
  if (is.finite(value) && value > 0 && ordinary_units) return(value)
  if (!is.finite(magnitude) || magnitude == 0 || length(x) < 2L) return(0)
  # Squaring very large or small finite values can overflow or underflow in sd().
  # Subtract a reference first to preserve small differences near a large offset.
  # If that subtraction overflows, the inputs span a wide range, so normalize
  # the original values instead. Both paths restore the original units.
  differences <- x - x[[1L]]
  if (all(is.finite(differences))) {
    spread <- max(abs(differences))
    if (spread == 0) return(0)
    return(stats::sd(differences / spread) * spread)
  }
  stats::sd(x / magnitude) * magnitude
}

matrix_blueprint_predictor_kind <- function(column) {
  if (is.null(dim(column)) && is.numeric(column) && !is.complex(column)) {
    return("numeric")
  }
  if (is.null(dim(column)) && is.logical(column)) return("logical")
  if (is.null(dim(column)) && (is.factor(column) || is.character(column))) {
    return("categorical")
  }
  NA_character_
}

assert_matrix_blueprint_flag <- function(value, argument) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop("`", argument, "` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(TRUE)
}

safe_reformulate <- function(termlabels, response = NULL) {
  symbols <- lapply(termlabels, as.name)
  right_hand_side <- if (length(symbols)) {
    Reduce(function(left, right) call("+", left, right), symbols)
  } else {
    1
  }
  expression <- if (is.null(response)) {
    call("~", right_hand_side)
  } else {
    call("~", as.name(response), right_hand_side)
  }
  # These formulas contain only column names, addition, and an intercept. A
  # caller environment would retain fitting data and sibling models on export.
  structure(expression, class = "formula", .Environment = baseenv())
}
