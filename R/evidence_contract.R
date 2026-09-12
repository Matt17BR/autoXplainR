# Identities bind evidence to serializable model state and ordered evaluation
# content. They are compatibility checks, not signatures or security credentials.
content_fingerprint <- function(value) {
  paste0("axr-", digest::digest(value, algo = "sha256", serializeVersion = 2L))
}

model_identity_payload <- function(value, data_variables = character()) {
  # Do not retain a whole calling session. Formulas below bind only referenced
  # transform inputs, since those can affect predictions on new or perturbed rows.
  if (is.environment(value)) return(list(environment = environmentName(value)))
  if (is.function(value)) {
    # Source references include mutable srcfile environments (line caches,
    # timestamps and paths). Loading source during a traceback or vignette
    # build must not change the identity of otherwise identical executable code.
    value <- utils::removeSource(value)
    return(list(formals = formals(value), body = body(value),
                environment = environmentName(environment(value))))
  }
  if (isS4(value)) {
    slots <- methods::slotNames(value)
    return(list(class = class(value), slots = setNames(lapply(slots, function(name) {
      model_identity_payload(methods::slot(value, name), data_variables)
    }), slots)))
  }
  if (is.list(value)) {
    output <- lapply(value, model_identity_payload, data_variables = data_variables)
    attributes(output) <- lapply(attributes(value), model_identity_payload, data_variables = data_variables)
    if (inherits(value, "gam")) {
      output$prediction_smooth_context <- gam_smooth_prediction_context(value, data_variables)
    }
    # predict.glm(type = "response") executes this fitted object's inverse link.
    # Standard stats links retain their established identity; caller-defined
    # links additionally bind the lexical state they execute on perturbed rows.
    if (inherits(value, "glm") && is.function(value$family$linkinv) &&
          !isNamespace(environment(value$family$linkinv))) {
      output$family$linkinv <- prediction_function_context(value$family$linkinv, data_variables)
    }
    if (inherits(value, "lm") && !is.null(value$call$offset)) {
      environment <- attr(value$terms, ".Environment")
      arguments <- as.pairlist(stats::setNames(rep(list(NULL), length(data_variables)), data_variables))
      transform <- eval(call("function", arguments, value$call$offset), envir = environment)
      output$prediction_offset_context <- prediction_function_context(transform)
    }
    if (!is.null(value$contrasts)) {
      contexts <- lapply(as.list(value$contrasts), function(contrast) {
        fun <- if (is.function(contrast)) contrast else if (is.character(contrast) && length(contrast) == 1L) {
          get0(contrast, envir = asNamespace("stats"), mode = "function", inherits = TRUE)
        }
        if (is.function(fun) && !is.primitive(fun) && !isNamespace(environment(fun))) {
          prediction_function_context(fun, data_variables)
        }
      })
      contexts <- Filter(Negate(is.null), contexts)
      if (length(contexts)) output$prediction_contrast_context <- contexts
    }
    return(output)
  }
  if (inherits(value, c("formula", "terms")) && is.environment(attr(value, ".Environment"))) {
    environment <- attr(value, ".Environment")
    arguments <- as.pairlist(stats::setNames(rep(list(NULL), length(data_variables)), data_variables))
    transform <- eval(call("function", arguments, value[[length(value)]]), envir = environment)
    code <- utils::removeSource(value)
    attr(code, ".Environment") <- NULL
    return(list(formula = code, transform_context = prediction_function_context(transform)))
  }
  if (is.language(value)) value <- utils::removeSource(value)
  if (is.language(value) || is.pairlist(value)) {
    attr(value, ".Environment") <- NULL
  }
  value
}

new_explainer_instance <- local({
  counter <- 0L
  function() {
    counter <<- counter + 1L
    content_fingerprint(list(Sys.getpid(), as.numeric(Sys.time()), counter))
  }
})

prediction_adapter_identity <- function(adapter, data_variables = character()) {
  adapter_environment <- environment(adapter)
  setting <- function(name) {
    if (!is.null(adapter_environment) && exists(name, adapter_environment, inherits = FALSE)) {
      model_identity_payload(get(name, adapter_environment, inherits = FALSE))
    } else {
      NULL
    }
  }
  list(code = model_identity_payload(adapter),
       custom_code = setting("predict_function"),
       custom_context = if (!is.null(adapter_environment) &&
                              exists("predict_function", adapter_environment, inherits = FALSE)) {
         prediction_function_context(get("predict_function", adapter_environment, inherits = FALSE), data_variables)
       },
       native_event = setting("native_event"), task = setting("task"),
       positive = setting("positive"), class_levels = setting("class_levels"))
}

explainer_content_fingerprint <- function(model, data, y, task, positive,
                                          class_levels, predictions, instance = NULL, adapter = NULL) {
  content_fingerprint(list(
    contract = "3", model = prediction_function_context(model, names(data)), data = data,
    outcome = y, task = task, positive = positive, class_levels = class_levels,
    predictions = predictions, custom_instance = instance,
    adapter = if (!is.null(adapter)) prediction_adapter_identity(adapter, names(data)) else NULL
  ))
}

current_explainer_fingerprint <- function(explainer) {
  # Recheck predictions as custom closures or external model handles can mutate
  # independently of the visible R model object.
  predictions <- predict(explainer, explainer$data)
  explainer_content_fingerprint(
    explainer$model, explainer$data, explainer$y, explainer$task,
    explainer$positive, explainer$class_levels, predictions,
    explainer$provenance$custom_instance, explainer$predict_function
  )
}

assert_common_evaluation <- function(explainers) {
  reference <- explainers[[1L]]
  for (explainer in explainers) {
    same <- identical(reference$task, explainer$task) &&
      identical(reference$data, explainer$data) && identical(reference$y, explainer$y) &&
      identical(reference$class_levels, explainer$class_levels) &&
      identical(reference$positive, explainer$positive)
    if (!same) {
      stop("Paired comparisons require the same ordered evaluation data, outcomes, ",
           "class levels and positive event. Align the explainers before auditing.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

validate_attached_audit <- function(audit, explainers, expected_fingerprints = NULL) {
  if (!inherits(audit, "autoxplain_audit")) {
    stop("`audit` must be returned by `audit_explanations()`.", call. = FALSE)
  }
  expected <- expected_fingerprints %||%
    vapply(explainers, current_explainer_fingerprint, character(1))
  observed <- audit$provenance$explainer_fingerprints[names(explainers)]
  if (length(observed) != length(expected) || anyNA(observed) ||
        !identical(unname(observed), unname(expected))) {
    stop("The audit was not made from the same selected model explainers and ordered ",
         "evaluation evidence. Recompute it for this result.", call. = FALSE)
  }
  invisible(TRUE)
}
