#' Extract a compact, versioned evidence summary
#'
#' Returns aggregate results suitable for review or an analysis registry. It
#' excludes raw rows, row names, per-case predictions, group identifiers, fitted
#' objects, and narrative provider credentials. Target and feature names, aggregate
#' statistics, and diagnostic messages remain; review them before sharing.
#'
#' @param result An [autoxplain()] result.
#' @return A plain list with `schema_version`, package version, task, model
#'   selection, evaluation, explanation summaries, and interpretation limits.
#'   Schema 1.0 allows additional fields within a minor package release. Removing
#'   or changing the meaning of a field requires a new schema major version.
#' @export
#' @examples
#' result <- autoxplain(mtcars, "mpg")
#' evidence <- evidence_summary(result)
#' evidence$evaluation
#' # Optional JSON export:
#' # jsonlite::write_json(evidence, "evidence.json", auto_unbox = TRUE, pretty = TRUE)
evidence_summary <- function(result) {
  if (!inherits(result, "autoxplain_result")) {
    stop("`result` must be returned by `autoxplain()`.", call. = FALSE)
  }
  audit <- result$explanations$audit
  list(
    schema_version = "1.0",
    package_version = result$provenance$package_version %||% package_version_or_development(),
    task = result$task,
    target = result$target_column,
    features = result$features,
    selection = list(
      primary_model_id = result$provenance$primary_model_id,
      method = result$provenance$candidate_selection,
      seed = result$provenance$seed
    ),
    evaluation = list(
      role = result$provenance$evaluation_role,
      split_method = result$provenance$split_method,
      rows = nrow(result$test_data),
      primary_metric = result$evaluation$primary_metric,
      metrics = result$evaluation$metrics,
      metric_definitions = result$evaluation$metric_definitions,
      improvement_over_baseline = result$evaluation$improvement_over_baseline,
      notes = result$evaluation$notes
    ),
    explanations = if (is.null(audit)) NULL else list(
      config = audit$config,
      importance = as.data.frame(audit$importance),
      findings = audit$findings,
      effect_failures = result$explanations$failures
    ),
    limits = c(
      "Evaluation describes the supplied rows and study design.",
      "Permutation intervals describe Monte Carlo error, not population uncertainty.",
      "Effects describe fitted associations, not causal interventions.",
      "Supplied model comparisons do not cover every competitive model.",
      "No fairness, safety, or deployment certification is provided."
    )
  )
}
