# Identities bind evidence to serializable model state and ordered evaluation
# content. They are compatibility checks, not signatures or security credentials.
content_fingerprint <- function(value) {
  paste0("axr-", digest::digest(value, algo = "sha256", serializeVersion = 2L))
}

model_identity_payload <- function(value) {
  # Formula environments can retain an entire calling session. Statistical
  # state is in the fitted object; formula code is retained without that session.
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
      model_identity_payload(methods::slot(value, name))
    }), slots)))
  }
  if (is.list(value)) {
    output <- lapply(value, model_identity_payload)
    attributes(output) <- lapply(attributes(value), model_identity_payload)
    return(output)
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

prediction_adapter_identity <- function(adapter) {
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
       native_event = setting("native_event"), task = setting("task"),
       positive = setting("positive"), class_levels = setting("class_levels"))
}

explainer_content_fingerprint <- function(model, data, y, task, positive,
                                          class_levels, predictions, instance = NULL, adapter = NULL) {
  content_fingerprint(list(
    contract = "2", model = model_identity_payload(model), data = data,
    outcome = y, task = task, positive = positive, class_levels = class_levels,
    predictions = predictions, custom_instance = instance,
    adapter = if (!is.null(adapter)) prediction_adapter_identity(adapter) else NULL
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

validate_attached_audit <- function(audit, explainers) {
  if (!inherits(audit, "autoxplain_audit")) {
    stop("`audit` must be returned by `audit_explanations()`.", call. = FALSE)
  }
  expected <- vapply(explainers, current_explainer_fingerprint, character(1))
  observed <- audit$provenance$explainer_fingerprints[names(explainers)]
  if (length(observed) != length(expected) || anyNA(observed) ||
        !identical(unname(observed), unname(expected))) {
    stop("The audit was not made from the same selected model explainers and ordered ",
         "evaluation evidence. Recompute it for this result.", call. = FALSE)
  }
  invisible(TRUE)
}
