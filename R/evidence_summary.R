#' Extract a compact, versioned evidence summary
#'
#' Returns aggregate results suitable for review or an analysis registry. It
#' excludes raw rows, row names, per-case predictions, group identifiers, fitted
#' objects, and narrative provider credentials. Target and feature names, aggregate
#' statistics, and diagnostic messages remain; review them before sharing.
#'
#' @param result An [autoxplain()] or [evaluate_models()] result.
#' @return A plain list with `schema_version`, package version, task, model
#'   selection, evaluation, explanation summaries, and interpretation limits.
#'   Schema 2.0 allows additional fields within a minor package release. Removing
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
    stop("`result` must be returned by `autoxplain()` or `evaluate_models()`.", call. = FALSE)
  }
  validate_evaluation_snapshot(result)
  audit <- result$explanations$audit
  view <- report_view_model(result)
  # Preserve R's count tables in the audit; export labelled scalar values.
  if (!is.null(audit$config$sampling)) {
    audit$config$sampling <- evidence_sampling_summary(audit$config$sampling)
  }
  if (!is.null(audit$diagnostic_status$permutation$evidence$sampling)) {
    audit$diagnostic_status$permutation$evidence$sampling <- evidence_sampling_summary(
      audit$diagnostic_status$permutation$evidence$sampling
    )
  }
  list(
    schema_version = "2.0",
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
      scope = audit$summary$scope_note,
      association_scope = audit$summary$association_scope,
      model_diagnostics = audit$model_diagnostics,
      diagnostic_status = audit$diagnostic_status,
      importance = as.data.frame(audit$importance),
      findings = audit$findings,
      effect_failures = result$explanations$failures,
      effect_status = view$effect_status
    ),
    diagnostic_status = lapply(view$diagnostics, function(record) {
      record[intersect(c("id", "status", "scope", "entities", "reason", "interpretation"),
                       names(record))]
    }),
    limits = c(
      "Evaluation describes the supplied rows and study design.",
      "Permutation intervals describe Monte Carlo error, not population uncertainty.",
      "Effects describe fitted associations, not causal interventions.",
      "Supplied model comparisons do not cover every competitive model.",
      "No fairness, safety, or deployment certification is provided."
    )
  )
}

evidence_sampling_summary <- function(sampling) {
  for (name in c("full_class_counts", "class_counts")) {
    counts <- sampling[[name]]
    if (!is.null(counts)) {
      sampling[[name]] <- stats::setNames(as.list(as.integer(counts)), names(counts))
    }
  }
  sampling
}
