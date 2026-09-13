# Reuse complete class probabilities only for identical ordered prediction
# batches. Never concatenate upper/lower or different PDP grid rows: supported
# custom predictors can depend on the composition of each batch.
cache_effect_predictions <- function(predict_function, cache) {
  force(predict_function)
  force(cache)
  cursor <- new.env(parent = emptyenv())
  cursor$position <- 0L
  function(newdata) {
    cursor$position <- cursor$position + 1L
    position <- cursor$position
    entry <- if (length(cache$entries) >= position) cache$entries[[position]] else NULL
    if (is.null(entry) || !identical(entry$data, newdata)) {
      computed <- tryCatch(
        list(prediction = predict_function(newdata)),
        error = function(error) list(error = error)
      )
      entry <- c(list(data = newdata), computed)
      # A mismatched batch is predicted independently. Replacing the first
      # recorded request could prevent later classes from reusing their rows.
      if (length(cache$entries) < position) cache$entries[[position]] <- entry
    }
    if (!is.null(entry$error)) stop(entry$error)
    entry$prediction
  }
}

explain_effect_bundle <- function(explainer, feature, method, n_points, seed,
                                  max_rows, classes, prediction_context) {
  cache <- new.env(parent = emptyenv())
  cache$entries <- list()
  multiclass <- identical(explainer$task, "multiclass")
  output <- lapply(classes, function(class) {
    tryCatch(
      explain_effect_impl(
        explainer,
        feature = feature, method = method,
        n_points = n_points, seed = seed, max_rows = max_rows,
        class = if (multiclass) class else NULL,
        prediction_context = prediction_context, prediction_cache = cache
      ),
      error = function(error) structure(conditionMessage(error), class = "effect_failure", method = method)
    )
  })
  if (multiclass) names(output) <- classes
  output
}
