# Captured lexical state is evidence, too: equal predictions on observed rows
# do not imply equal behavior on shuffled rows or feature-effect grids.
standard_prediction_model <- function(model) {
  inherits(model, "H2OModel") || class(model)[[1L]] %in% c(
    "lm", "glm", "multinom", "rpart", "autoxplain_tuned_nnet", "autoxplain_fitted_model"
  )
}

resolved_custom_prediction_method <- function(model) {
  if (!is.object(model) || isS4(model) || standard_prediction_model(model)) return(NULL)
  for (class in class(model)) {
    method <- utils::getS3method("predict", class, optional = TRUE, envir = globalenv())
    if (!is.null(method)) return(method)
  }
  NULL
}

prediction_function_context <- function(fun, data_variables = character()) {
  if (is.null(fun)) return(NULL)
  seen_environments <- list()
  seen_functions <- list()
  unsupported <- function(reason) {
    stop("Custom prediction context cannot be bound to reusable evidence: ", reason,
      ". Use a deterministic predictor with explicit model state and lexical inputs; ",
      "dynamic lookup, active bindings, and external mutable dependencies are unsupported.", call. = FALSE
    )
  }
  trusted_environment <- function(environment) {
    identical(environment, baseenv()) || identical(environment, emptyenv()) ||
      isNamespace(environment) || startsWith(environmentName(environment), "package:")
  }
  dynamic <- c(
    "get", "get0", "mget", "dynGet", "eval", "evalq", "eval.parent", "parse", "source", "sys.source",
    "globalenv", "parent.frame", "sys.frame", "sys.frames", "getFromNamespace", "getAnywhere",
    "load", "readRDS", "readLines", "readBin", "scan", "read.table", "read.csv", "read.csv2",
    "Sys.getenv", "Sys.time", "Sys.Date", "system", "system2", "url", "socketConnection",
    "assign", "delayedAssign", "makeActiveBinding", ".Call", ".External", ".C", ".Fortran", "do.call",
    "runif", "rnorm", "rbinom", "rpois", "rgamma", "rexp", "rbeta", "rchisq", "rt", "rf", "rgeom",
    "rhyper", "rlogis", "rlnorm", "rmultinom", "rnbinom", "rweibull", "rcauchy", "sample", "sample.int"
  )
  dynamic_functions <- Filter(is.function, lapply(dynamic, function(name) {
    get0(name, envir = baseenv(), mode = "function", inherits = TRUE) %||%
      get0(name, envir = asNamespace("stats"), mode = "function", inherits = FALSE)
  }))
  called_names <- function(code) {
    if (missing(code)) return(character())
    if (!is.call(code)) return(character())
    head <- code[[1L]]
    name <- if (is.symbol(head)) {
      as.character(head)
    } else if (is.call(head) && as.character(head[[1L]]) %in% c("::", ":::")) {
      as.character(head[[3L]])
    } else {
      ""
    }
    c(name, unlist(lapply(as.list(code)[-1L], called_names), use.names = FALSE))
  }
  binding <- function(name, environment) {
    current <- environment
    while (!identical(current, emptyenv())) {
      if (exists(name, current, inherits = FALSE)) {
        if (bindingIsActive(name, current)) unsupported(paste0("active binding `", name, "`"))
        return(walk(get(name, current, inherits = FALSE)))
      }
      current <- parent.env(current)
    }
    list(unresolved_lexical_name = name)
  }
  walk <- function(value) {
    if (inherits(value, c("formula", "terms"))) return(model_identity_payload(value, data_variables))
    if (standard_prediction_model(value)) {
      # Native fitted objects include namespace-created implementation closures
      # and external engine representations; their statistical state and formula
      # transform dependencies are handled by the native model payload.
      return(list(native_model = model_identity_payload(value, data_variables),
        attributes = lapply(attributes(value), walk)
      ))
    }
    if (is.environment(value)) {
      if (trusted_environment(value) || identical(value, globalenv())) {
        unsupported("a prediction explicitly captures a shared execution environment")
      }
      existing <- which(vapply(seen_environments, identical, logical(1), value))
      if (length(existing)) return(list(environment_reference = existing[[1L]]))
      seen_environments[[length(seen_environments) + 1L]] <<- value
      fields <- sort(ls(value, all.names = TRUE))
      contents <- lapply(fields, function(name) {
        if (bindingIsActive(name, value)) unsupported(paste0("active binding `", name, "`"))
        walk(get(name, value, inherits = FALSE))
      })
      return(list(environment_contents = stats::setNames(contents, fields),
        attributes = lapply(attributes(value), walk),
        prediction_method = if (!is.null(resolved_custom_prediction_method(value))) {
          walk(resolved_custom_prediction_method(value))
        }
      ))
    }
    if (typeof(value) %in% c("externalptr", "weakref") || inherits(value, "connection")) {
      unsupported("an external pointer, weak reference, or connection is captured")
    }
    if (is.function(value)) {
      if (any(vapply(dynamic_functions, identical, logical(1), value))) {
        unsupported("an alias of a dynamic or external function")
      }
      clean <- utils::removeSource(value)
      code <- model_identity_payload(clean)
      if (is.primitive(value) || trusted_environment(environment(value))) {
        return(list(code = code, attributes = lapply(attributes(clean), walk)))
      }
      existing <- which(vapply(seen_functions, identical, logical(1), clean))
      if (length(existing)) return(list(function_reference = existing[[1L]]))
      seen_functions[[length(seen_functions) + 1L]] <<- clean
      calls <- unique(c(called_names(body(clean)), unlist(lapply(formals(clean), called_names), use.names = FALSE)))
      if (any(calls %in% dynamic)) {
        unsupported(paste0("dynamic or external call `", intersect(calls, dynamic)[[1L]], "()`"))
      }
      globals <- codetools::findGlobals(clean, merge = TRUE)
      dependencies <- lapply(sort(globals), binding, environment = environment(value))
      return(list(code = code, lexical_bindings = stats::setNames(dependencies, sort(globals)),
        attributes = lapply(attributes(clean), walk),
        prediction_method = if (!is.null(resolved_custom_prediction_method(value))) {
          walk(resolved_custom_prediction_method(value))
        }
      ))
    }
    if (isS4(value)) {
      fields <- methods::slotNames(value)
      return(list(class = class(value), slots = stats::setNames(lapply(fields, function(name) {
        walk(methods::slot(value, name))
      }), fields), attributes = lapply(attributes(value), walk)))
    }
    if (is.list(value)) {
      contents <- lapply(value, walk)
      method <- resolved_custom_prediction_method(value)
      return(list(list_values = contents, attributes = lapply(attributes(value), walk),
        prediction_method = if (!is.null(method)) walk(method)
      ))
    }
    value_attributes <- attributes(value)
    method <- resolved_custom_prediction_method(value)
    attributes(value) <- NULL
    list(value = model_identity_payload(value, data_variables),
      attributes = lapply(value_attributes, walk),
      prediction_method = if (!is.null(method)) walk(method)
    )
  }
  walk(fun)
}

evaluation_snapshot_fingerprint <- function(result) {
  # Display labels and a separately validated benchmark may be attached locally.
  # All original score, ranking, cost and model-family claims remain immutable.
  board_fields <- setdiff(names(result$leaderboard), c("model", "repeated_prediction_ms_per_row"))
  cosmetic_provenance <- c(
    "created_at", "package_version", "r_version", "title", "subtitle", "target_units",
    "target_label", "primary_model_label"
  )
  substantive_provenance <- result$provenance[setdiff(names(result$provenance), cosmetic_provenance)]
  contracts <- lapply(result$prediction_contracts, function(contract) {
    list(task = contract$task, class_levels = contract$class_levels, positive = contract$positive,
      custom_instance = contract$custom_instance,
      prediction_context = prediction_function_context(
        contract$predict_function, c(result$features, result$target_column)
      )
    )
  })
  content_fingerprint(list(
    version = 1L, models = stats::setNames(lapply(names(result$models), function(id) {
      prediction_function_context(result$models[[id]], c(result$features, result$target_column))
    }), names(result$models)),
    task = result$task, engine = result$engine, schema_version = result$schema_version,
    features = result$features, target = result$target_column,
    schema = result$prediction_schema, contracts = contracts,
    preprocessing = model_identity_payload(result$preprocessing_metadata),
    validation = result$validation, tuning = model_identity_payload(result$tuning),
    evaluation_data = result$evaluation_data, test_data = result$test_data,
    evaluation_context = result$evaluation_context, evaluation_row_indices = result$evaluation_row_indices,
    data_context = result$data_context, training_data = result$training_data,
    training_available = result$training_available,
    provenance = substantive_provenance, reference = result_reference_id(result),
    evaluation = result$evaluation, metric_table = result$leaderboard[board_fields],
    engine_leaderboard = result$engine_leaderboard,
    model_diagnostics = result$model_diagnostics,
    model_characteristics = result$model_characteristics
  ))
}

seal_evaluation_result <- function(result) {
  result$.evaluation_snapshot <- list(version = 1L, fingerprint = evaluation_snapshot_fingerprint(result))
  result
}

validate_evaluation_snapshot <- function(result) {
  snapshot <- result$.evaluation_snapshot
  if (is.null(snapshot)) return(invisible(TRUE))
  if (!identical(snapshot$fingerprint, evaluation_snapshot_fingerprint(result))) {
    stop("Stored evaluation evidence no longer matches the models, prediction contracts, schema, or data. ",
      "Re-evaluate the models with `evaluate_models()` before preparing another report; ",
      "do not combine saved scores with changed predictions.", call. = FALSE
    )
  }
  invisible(TRUE)
}

# Every render verifies current predictions against the saved assessment. This
# also protects legacy results, whose former off-grid state cannot be recovered.
validate_recorded_evaluation <- function(result, explainers) {
  legacy <- is.null(result$.evaluation_snapshot)
  incompatible <- function(reason) {
    stop("Stored ", if (legacy) "legacy ", "evaluation evidence does not match current predictions: ", reason,
      ". Re-evaluate the models with `evaluate_models()` before reporting.", call. = FALSE
    )
  }
  equal_values <- function(a, b) {
    if (is.numeric(a) && is.numeric(b)) {
      return(isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = 1e-10)))
    }
    identical(as.character(a), as.character(b))
  }
  evaluated <- lapply(explainers, function(explainer) {
    predicted <- explainer$reference_predictions
    metrics <- evaluate_predictions(explainer$y, predicted, explainer)
    if (explainer$task != "regression") {
      metrics <- c(metrics, calibration_error = calibration_from_explainer(
        explainer, predicted = predicted
      )$calibration_error)
    }
    list(explainer = explainer, predictions = predicted, metrics = metrics)
  })
  primary <- result$provenance$primary_model_id %||% names(result$models)[[1L]]
  recorded <- result$evaluation$predictions
  if (!is.null(recorded) && primary %in% names(evaluated)) {
    current <- guided_prediction_table(evaluated, result$task, primary)
    reference <- result_reference_id(result)
    if (!is.null(reference) && reference %in% names(evaluated)) {
      predicted <- evaluated[[reference]]$predictions
      if (result$task == "regression") current$baseline_prediction <- as.numeric(predicted)
      if (result$task == "binary") current$baseline_probability <- as.numeric(predicted)
      if (result$task == "multiclass") {
        levels <- evaluated[[reference]]$explainer$class_levels
        current$baseline_prediction <- levels[max.col(predicted[, levels, drop = FALSE], ties.method = "first")]
      }
    } else {
      current <- current[!grepl("^baseline_", names(current))]
    }
    common <- intersect(names(recorded), names(current))
    for (field in common) {
      if (!equal_values(recorded[[field]], current[[field]])) incompatible(paste("ordered", field))
    }
  }
  for (id in names(evaluated)) {
    metrics <- evaluated[[id]]$metrics
    saved <- result$evaluation$metrics[[id]]
    board_row <- match(id, result$leaderboard$model_id)
    board <- if (!is.na(board_row)) result$leaderboard[board_row, , drop = FALSE] else list()
    checked <- FALSE
    for (source in list(saved, board)) {
      common <- intersect(names(metrics), names(source))
      for (metric in common) {
        checked <- TRUE
        if (!equal_values(metrics[[metric]], source[[metric]])) incompatible(paste(id, metric))
      }
    }
    if (!checked) incompatible(paste("no comparable official metrics for", id))
  }
  invisible(TRUE)
}
