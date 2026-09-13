# Resolve CPU allocation only after search mode and outer-training inputs are
# known. Native thread counts are execution settings, never seed/key inputs.
resolve_tuning_threads <- function(control, n, p, learners) {
  requested <- control[["threads_requested", exact = TRUE]] %||% control[["threads", exact = TRUE]] %||% "auto"
  automatic <- identical(requested, "auto")
  eligible <- identical(control$search, "adaptive") &&
    !length(control$grids) && is.null(control$family_budgets) &&
    all(learners %in% c("regularized", "forest", "boosting")) &&
    any(learners %in% c("forest", "boosting")) && as.double(n) * p >= 1e6
  available <- NA_integer_
  if (!automatic) {
    effective <- assert_count(requested, "threads")
    reason <- "Explicit native thread count retained unchanged; the caller controls CPU allocation."
  } else if (eligible) {
    available <- as.integer(unname(parallelly::availableCores()))
    effective <- min(4L, available)
    reason <- paste(
      "Large automatic adaptive forest or boosting search uses up to four native threads,",
      "bounded by the CPU allocation reported by parallelly::availableCores()."
    )
  } else {
    effective <- 1L
    reason <- paste(
      "Automatic native threading remains at one for small inputs, grid search,",
      "custom grids, exact family budgets, or searches outside forest and boosting."
    )
  }
  control$threads <- effective
  control$threads_requested <- requested
  control$thread_policy <- list(
    policy_id = "native-threads-v1", requested = requested, effective = effective,
    available_cores = available, eligible = eligible, reason = reason,
    scope = paste(
      "Counts apply to native forest and boosting fits and predictions; other backends keep their existing settings.",
      "Folds and configurations run sequentially. Explicit counts are not clamped.",
      "Replay uses the resolved count explicitly; hardware does not change configuration seeds."
    )
  )
  control
}
