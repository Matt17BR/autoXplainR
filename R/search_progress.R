# Progress is carried separately from statistical controls and stored evidence.
# Printing never changes the fit plan, candidate order or random-number state.
resolve_progress_verbosity <- function(verbosity, engine, model_set, rows, learners = NULL) {
  if (!identical(verbosity, "auto")) return(verbosity)
  # Match the input-size boundary for automatic native adaptive search. Forest
  # and boosting workflows can spend substantial time fitting and explaining
  # even modest tables; tiny teaching examples remain quiet by default.
  nontrivial_native_search <- identical(engine, "base") && identical(model_set, "tuned") &&
    rows >= 200L && any(learners %in% c("forest", "boosting"))
  if (nontrivial_native_search) "info" else "quiet"
}

search_progress <- function(enabled, ...) {
  # User-supplied logging handlers can draw random numbers. Progress must not
  # change either the active fitting stream or the caller's random state.
  if (isTRUE(enabled)) withr::with_preserve_seed(message("AutoXplainR: ", ..., appendLF = TRUE))
  invisible(NULL)
}

search_progress_count <- function(rows) {
  format(rows, big.mark = ",", scientific = FALSE, trim = TRUE)
}

explanation_progress <- function(enabled, stage, explainer, features, max_rows, repeats = NULL) {
  if (!isTRUE(enabled)) return(invisible(NULL))
  search_progress(
    enabled, stage, ": ", explainer$label, "; ", length(features),
    if (length(features) == 1L) " input; up to " else " inputs; up to ",
    search_progress_count(min(nrow(explainer$data), max_rows %||% Inf)), " reference rows",
    if (!is.null(repeats)) paste0("; ", repeats, " shuffles per input"), "."
  )
}

explanation_progress_time <- function() {
  unname(proc.time()[["elapsed"]])
}

new_importance_progress <- function(enabled, model, groups, repeats) {
  # This state belongs to one importance computation. Quiet runs never read
  # the clock; active runs report only after a shuffle has completed.
  if (!isTRUE(enabled)) return(NULL)
  state <- new.env(parent = emptyenv())
  state$model <- model
  state$groups <- groups
  state$repeats <- repeats
  state$last_update <- explanation_progress_time()
  state
}

update_importance_progress <- function(state, group, repeat_index) {
  if (is.null(state)) return(invisible(NULL))
  now <- explanation_progress_time()
  if (now - state$last_update < 30) return(invisible(NULL))
  search_progress(
    TRUE, "Input importance: ", state$model, "; input group ", group, "/", state$groups,
    "; repeat ", repeat_index, "/", state$repeats, "."
  )
  state$last_update <- now
  invisible(NULL)
}

search_progress_fit <- function(enabled, stage, configuration, training_rows, assessment_rows = NULL,
                                position = NULL, total = NULL, fold = NULL, folds = NULL, role = NULL) {
  if (!isTRUE(enabled)) return(invisible(NULL))
  search_progress(
    enabled, stage,
    if (!is.null(position)) paste0(" ", position, "/", total), ": ",
    configuration$family[[1L]], " (", configuration$configuration_id[[1L]], ")",
    if (!is.null(fold)) paste0(", fold ", fold, "/", folds),
    if (!is.null(role)) paste0(" [", role, "]"),
    "; ", search_progress_count(training_rows), " training rows",
    if (!is.null(assessment_rows)) paste0("; ", search_progress_count(assessment_rows), " assessment rows"),
    "."
  )
}
