fit_matrix_blueprint <- function(data,
                                 predictors = names(data),
                                 center = FALSE,
                                 scale = FALSE,
                                 intercept = FALSE,
                                 categorical_encoding = c("treatment", "one_hot")) {
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
  encoded <- if (length(encoding_contrasts)) {
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
  if (any(!is.finite(encoded))) {
    stop("Training predictors produced non-finite encoded values.", call. = FALSE)
  }

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
  }
  scale_values <- stats::setNames(rep(1, length(columns)), columns)
  zero_variance_columns <- character()
  if (isTRUE(scale) && any(standardizable)) {
    learned_scale <- apply(encoded[, standardizable, drop = FALSE], 2L, stats::sd)
    invalid_scale <- !is.finite(learned_scale) | learned_scale == 0
    zero_variance_columns <- names(learned_scale)[invalid_scale]
    learned_scale[invalid_scale] <- 1
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
      output = "dense"
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
  encoded <- if (length(blueprint$contrasts)) {
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
  if (!identical(colnames(encoded), blueprint$columns)) {
    stop(
      "Encoded columns do not match the training blueprint. Expected: ",
      paste(blueprint$columns, collapse = ", "),
      "; produced: ", paste(colnames(encoded), collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (any(!is.finite(encoded))) {
    stop("New predictors produced non-finite encoded values.", call. = FALSE)
  }

  encoded <- sweep(encoded, 2L, blueprint$center, "-")
  encoded <- sweep(encoded, 2L, blueprint$scale, "/")
  storage.mode(encoded) <- "double"
  encoded
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
  structure(expression, class = "formula", .Environment = parent.frame())
}
