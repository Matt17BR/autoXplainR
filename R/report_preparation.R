# A render owns one short-lived snapshot. Rebuilding it is mandatory at every
# public render boundary: a saved snapshot never authorizes reuse after mutation.
prepare_report_context <- function(result, models = NULL) {
  validate_evaluation_snapshot(result)
  result$.report_context <- NULL
  explainers <- result_explainers(result, models = models)
  validate_evaluation_snapshot(result)
  validate_recorded_evaluation(result, explainers)
  structure(list(
    explainers = explainers,
    predictions = lapply(explainers, function(explainer) explainer$reference_predictions),
    fingerprints = vapply(explainers, function(explainer) {
      explainer$provenance$fingerprint
    }, character(1))
  ), class = "autoxplain_report_context")
}

report_explainers <- function(result, models = NULL) {
  context <- result$.report_context
  if (is.null(context)) {
    return(as_explainers(result, models = models))
  }
  select_models(context$explainers, models)
}

report_predictions <- function(result, models = NULL, explainers = NULL) {
  context <- result$.report_context
  if (!is.null(context)) {
    return(select_models(context$predictions, models))
  }
  explainers <- explainers %||% as_explainers(result, models = models)
  lapply(explainers, function(explainer) {
    explainer$reference_predictions %||% predict(explainer, explainer$data)
  })
}

report_fingerprints <- function(result, models = NULL, explainers = NULL) {
  context <- result$.report_context
  if (!is.null(context)) {
    ids <- names(select_models(context$explainers, models))
    return(context$fingerprints[ids])
  }
  explainers <- explainers %||% as_explainers(result, models = models)
  vapply(explainers, function(explainer) explainer$provenance$fingerprint, character(1))
}

empty_effect_status <- function() {
  data.frame(
    model_id = character(), feature = character(),
    prediction_class = character(), method = character(), status = character(),
    reason = character(), stringsAsFactors = FALSE
  )
}

# Derive statuses from the actual retained evidence, including legacy objects.
# The canonical key is model / prediction class / feature, not feature alone.
report_effect_status <- function(result) {
  evidence <- result$explanations
  if (is.null(evidence)) {
    return(empty_effect_status())
  }
  primary <- result$provenance$primary_model_id %||% names(result$models)[[1L]]
  first_class <- if (identical(result$task, "multiclass")) {
    result_class_levels(result)[[1L]]
  } else if (identical(result$task, "binary")) {
    result_positive_class(result)
  } else {
    NA_character_
  }
  collected <- new.env(parent = emptyenv())
  collected$rows <- list()
  add <- function(values, id, prediction_class = first_class) {
    for (feature in names(values)) {
      effect <- values[[feature]]
      failed <- inherits(effect, "effect_failure")
      collected$rows[[length(collected$rows) + 1L]] <- data.frame(
        model_id = id, feature = feature,
        prediction_class = attr(effect, "prediction_class") %||% prediction_class,
        method = attr(effect, "method") %||% NA_character_,
        status = if (failed) "failed" else "available",
        reason = if (failed) as.character(effect) else NA_character_,
        stringsAsFactors = FALSE
      )
    }
  }
  for (id in names(evidence$effects_by_model)) add(evidence$effects_by_model[[id]], id)
  for (class in names(evidence$effects_by_class)) {
    for (id in names(evidence$effects_by_class[[class]])) {
      add(evidence$effects_by_class[[class]][[id]], id, class)
    }
  }
  add(evidence$effects, primary)
  if (!is.null(evidence$failures) && nrow(evidence$failures)) {
    for (i in seq_len(nrow(evidence$failures))) {
      already_failed <- vapply(collected$rows, function(row) {
        identical(row$model_id, primary) &&
          identical(row$feature, evidence$failures$feature[[i]]) &&
          identical(row$prediction_class, first_class) && identical(row$status, "failed")
      }, logical(1))
      if (any(already_failed)) next
      failure <- structure(evidence$failures$reason[[i]], class = "effect_failure")
      add(stats::setNames(list(failure), evidence$failures$feature[[i]]), primary)
    }
  }
  if (!length(collected$rows)) {
    return(empty_effect_status())
  }
  output <- do.call(rbind, collected$rows)
  # The legacy primary failure view remains authoritative for old saved objects.
  key <- output[c("model_id", "feature", "prediction_class")]
  output <- output[!duplicated(key, fromLast = TRUE), , drop = FALSE]
  rownames(output) <- NULL
  output
}

validate_report_effect_collection <- function(values, id, prediction_class,
                                              result, fingerprints) {
  bad_names <- is.null(names(values)) || anyNA(names(values)) ||
    any(!nzchar(names(values))) || anyDuplicated(names(values))
  invalid_names <- length(values) && bad_names
  if (!is.list(values) || invalid_names) {
    stop("Effect collections must be lists with unique non-empty feature names.", call. = FALSE)
  }
  for (feature in names(values)) {
    effect <- values[[feature]]
    if (inherits(effect, "effect_failure")) next
    if (!is.data.frame(effect) || !identical(attr(effect, "feature"), feature)) {
      stop("Attached effect names must match their recorded feature.", call. = FALSE)
    }
    if (!id %in% names(fingerprints) ||
          !identical(attr(effect, "explainer_fingerprint"), fingerprints[[id]])) {
      stop("A model-specific effect is stale or foreign; recompute explanations before reporting.", call. = FALSE)
    }
    if (identical(result$task, "multiclass") &&
          !identical(attr(effect, "prediction_class"), prediction_class)) {
      stop("A retained effect has the wrong prediction class; recompute explanations.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

# Explicit effects replace the primary model's displayed curves across classes.
# Other models retain their independently validated evidence. Empty means none;
# it never silently requests recomputation of the old primary curves.
prepare_report_effects <- function(result, audit, effects = NULL, explicit_effects = FALSE) {
  primary <- result$provenance$primary_model_id %||% names(result$models)[[1L]]
  multiclass <- identical(result$task, "multiclass")
  classes <- if (multiclass) result_class_levels(result) else NA_character_
  ids <- names(audit$importance_objects)
  explainers <- report_explainers(result, models = ids)
  fingerprints <- report_fingerprints(result, models = ids, explainers = explainers)
  result$explanations$audit <- audit
  effects <- effects %||% result$explanations$effects %||% list()
  if (explicit_effects) {
    result$explanations$failures <- NULL
    result$explanations$effects_by_model[[primary]] <- list()
    if (multiclass) {
      for (class in classes) result$explanations$effects_by_class[[class]][[primary]] <- list()
    }
  }
  result$explanations$effects <- effects
  # Validate redundant saved views before choosing one, so an explicit override
  # cannot conceal a foreign secondary curve in another retained collection.
  if (multiclass && length(setdiff(names(result$explanations$effects_by_class), classes))) {
    stop("A retained effect has the wrong prediction class; recompute explanations.", call. = FALSE)
  }
  for (id in intersect(ids, names(result$explanations$effects_by_model))) {
    validate_report_effect_collection(
      result$explanations$effects_by_model[[id]],
      id, classes[[1L]], result, fingerprints
    )
  }
  for (class in names(result$explanations$effects_by_class)) {
    for (id in intersect(ids, names(result$explanations$effects_by_class[[class]]))) {
      validate_report_effect_collection(
        result$explanations$effects_by_class[[class]][[id]],
        id, class, result, fingerprints
      )
    }
  }
  for (feature in names(effects)) {
    class <- if (multiclass) attr(effects[[feature]], "prediction_class") else NA_character_
    if (multiclass && (is.null(class) || !class %in% classes)) {
      stop("An attached effect has an unknown prediction class.", call. = FALSE)
    }
    validate_report_effect_collection(effects[feature], primary, class, result, fingerprints)
  }
  output <- lapply(classes, function(class) {
    saved <- if (multiclass) result$explanations$effects_by_class[[class]] else NULL
    if (!multiclass || identical(class, classes[[1L]])) {
      by_model <- result$explanations$effects_by_model %||% list()
      by_model[names(saved)] <- saved
      saved <- by_model
    }
    primary_effects <- if (multiclass) {
      Filter(function(effect) {
        identical(attr(effect, "prediction_class"), class)
      }, effects)
    } else {
      effects
    }
    previous_primary <- saved[[primary]]
    if (length(primary_effects) || explicit_effects) {
      saved[[primary]] <- primary_effects
    }
    if ((!multiclass || identical(class, classes[[1L]])) && !explicit_effects &&
          !is.null(result$explanations$failures)) {
      failures <- result$explanations$failures
      for (i in seq_len(nrow(failures))) {
        feature <- failures$feature[[i]]
        previous <- previous_primary[[feature]]
        saved[[primary]][[feature]] <- if (inherits(previous, "effect_failure")) {
          previous
        } else {
          structure(failures$reason[[i]], class = "effect_failure")
        }
      }
    }
    values <- lapply(ids, function(id) {
      existing <- saved[[id]]
      if (!is.null(existing)) {
        validate_report_effect_collection(existing, id, class, result, fingerprints)
        return(existing)
      }
      rows <- audit$importance[audit$importance$model == id, , drop = FALSE]
      features <- head(rows$feature[order(-rows$importance)], result$explanations$config$top_features %||% 8L)
      stats::setNames(lapply(features, function(feature) {
        method <- if (is.numeric(explainers[[id]]$data[[feature]])) "ale" else "pdp"
        tryCatch(
          explain_effect(explainers[[id]], feature,
            method = method,
            n_points = 16L, seed = result$provenance$seed,
            max_rows = result$explanations$config$explanation_rows,
            class = if (multiclass) class else NULL
          ),
          error = function(error) structure(conditionMessage(error), class = "effect_failure", method = method)
        )
      }), features)
    })
    stats::setNames(values, ids)
  })
  result$explanations$effects_by_model <- output[[1L]]
  result$explanations$effects_by_class <- if (multiclass) stats::setNames(output, classes) else NULL
  if (!explicit_effects && !length(effects)) {
    result$explanations$effects <- Filter(function(effect) !inherits(effect, "effect_failure"), output[[1L]][[primary]])
  }
  result$explanations$effect_status <- report_effect_status(result)
  result
}
