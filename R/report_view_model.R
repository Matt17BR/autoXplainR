# A presentation-neutral snapshot. This helper never computes diagnostics or
# substitutes a successful result for a missing or failed check.
report_diagnostic_record <- function(id, status, scope, entities = NULL,
                                     evidence = NULL, interpretation = "", reason = NULL) {
  if (is.null(entities)) {
    entities <- data.frame(
      model_id = character(), model_label = character(), feature = character(),
      stringsAsFactors = FALSE
    )
  }
  list(
    id = id, status = status, scope = scope, entities = entities,
    evidence = evidence, interpretation = interpretation, reason = reason
  )
}

report_model_label <- function(result, id) {
  labels <- result$model_labels %||% result$provenance$model_labels
  if (!is.null(labels) && id %in% names(labels)) {
    return(as.character(labels[[id]]))
  }
  leaderboard <- result$leaderboard
  if (is.data.frame(leaderboard) && "model_id" %in% names(leaderboard)) {
    hit <- match(id, leaderboard$model_id)
    label_column <- intersect(c("model", "model_label", "label"), names(leaderboard))
    if (!is.na(hit) && length(label_column)) {
      return(as.character(leaderboard[[label_column[[1L]]]][[hit]]))
    }
  }
  if (identical(id, result$provenance$primary_model_id %||% "main_model")) {
    return(result$provenance$primary_model_label %||% id)
  }
  known <- c(
    simple_baseline = "Intercept-only baseline", small_tree = "Small decision tree",
    flexible_tree = "Flexible decision tree"
  )
  if (id %in% names(known)) unname(known[[id]]) else id
}

report_view_model <- function(result, audit = NULL, effects = NULL) {
  stopifnot(inherits(result, "autoxplain_result"))
  effects_override <- !is.null(effects)
  audit <- audit %||% result$explanations$audit
  effects <- effects %||% result$explanations$effects %||% list()
  primary <- result$evaluation$primary_model_id %||%
    result$provenance$primary_model_id %||% names(result$models)[[1L]]
  selection <- if (inherits(result$tuning, "autoxplain_tuning")) {
    paste0(
      "Selected using ", result$tuning$folds_used,
      " training folds; evaluation rows did not select this model."
    )
  } else if (identical(result$engine, "h2o")) {
    if (isTRUE(result$provenance$test_used_for_validation)) {
      "Selected by H2O using the supplied validation rows."
    } else {
      "Selected by H2O using training-only cross-validation."
    }
  } else {
    "Pre-specified model; candidate evaluation ranks did not select it."
  }
  identity <- list(
    target = result$target_column, task = result$task, model_id = primary,
    positive = if (identical(result$task, "binary")) {
      levels(result$training_data[[result$target_column]])[[2L]]
    } else {
      NULL
    },
    model_label = report_model_label(result, primary), engine = result$engine %||% "h2o",
    evaluation_role = result$provenance$evaluation_role %||% "evaluation",
    split_method = result$provenance$split_method %||% "user configured",
    training_rows = nrow(result$training_data),
    evaluation_rows = nrow(result$test_data %||% result$training_data),
    target_units = result$provenance$target_units %||% NULL,
    analysis_label = result$provenance$analysis_label %||% NULL,
    selection_note = selection
  )
  entity <- function(model = primary, feature = NA_character_) {
    data.frame(
      model_id = model,
      model_label = vapply(model, function(id) report_model_label(result, id), character(1)),
      feature = feature, stringsAsFactors = FALSE
    )
  }
  diagnostics <- list(
    evaluation = report_diagnostic_record(
      "evaluation",
      if (is.null(result$evaluation)) "not_run" else "available",
      identity$evaluation_role, entity(), result$evaluation,
      "Scores describe the evaluation rows and configured validation design."
    ),
    importance = report_diagnostic_record(
      "importance",
      if (is.null(audit)) "not_run" else "available", "fitted-model shuffle variation",
      entity(), audit$importance,
      "Shuffle intervals describe the implemented randomization, not population uncertainty."
    ),
    performance_uncertainty = report_diagnostic_record(
      "performance_uncertainty",
      if (is.null(result$performance_uncertainty)) "not_run" else "available",
      "evaluation sample conditional on fitted models", entity(), result$performance_uncertainty
    )
  )
  optional <- c("resources", "model_behavior", "prediction_disagreement", "decision_cutoffs")
  for (id in optional) {
    diagnostics[[id]] <- report_diagnostic_record(
      id, if (id == "decision_cutoffs" && result$task != "binary") "not_applicable" else "not_run",
      "supplied fitted models and evaluation rows",
      reason = if (id == "decision_cutoffs" && result$task != "binary") {
        "Decision-cutoff comparisons apply to binary classification."
      } else {
        "This optional check has not been computed."
      }
    )
  }
  retained_status <- result$diagnostic_status %||% list()
  retained_status <- c(retained_status, result$explanations$report_diagnostics %||%
                         attr(result$report_file, "diagnostic_status") %||% list())
  for (id in names(retained_status)) diagnostics[[id]] <- retained_status[[id]]
  if (!is.null(audit$diagnostic_status)) {
    status <- audit$diagnostic_status
    if (is.list(status) && !is.data.frame(status)) diagnostics[names(status)] <- status
    if (is.data.frame(status)) {
      for (i in seq_len(nrow(status))) {
        id <- as.character((status$diagnostic %||% status$id %||% status$component)[[i]])
        diagnostics[[id]] <- report_diagnostic_record(id, as.character(status$status[[i]]),
          "supplied models and evaluation rows",
          evidence = status[i, , drop = FALSE],
          reason = as.character((status$reason %||% rep("", nrow(status)))[[i]])
        )
      }
    }
  }
  for (feature in names(effects)) {
    diagnostics[[paste0("effect:", feature)]] <-
      report_diagnostic_record(
        paste0("effect:", feature), "available", "fixed fitted model",
        entity(feature = feature), effects[[feature]]
      )
  }
  failures <- if (effects_override) NULL else result$explanations$failures
  if (!is.null(failures) && nrow(failures)) {
    for (i in seq_len(nrow(failures))) {
      feature <- failures$feature[[i]]
      diagnostics[[paste0("effect:", feature)]] <- report_diagnostic_record(
        paste0("effect:", feature), "failed", "fixed fitted model", entity(feature = feature),
        reason = failures$reason[[i]]
      )
    }
  }
  findings <- list()
  raw <- audit$findings
  if (!is.null(raw) && nrow(raw)) {
    for (i in seq_len(nrow(raw))) {
      item <- report_diagnostic_record(as.character(raw$code[[i]]), "available",
        if ("scope" %in% names(raw)) raw$scope[[i]] else "supplied fitted models",
        evidence = raw$evidence[[i]],
        interpretation = raw$message[[i]]
      )
      item$severity <- raw$severity[[i]]
      item$title <- raw$message[[i]]
      item$action <- raw$recommendation[[i]]
      model <- if ("model" %in% names(raw)) raw$model[[i]] else NA_character_
      feature <- if ("feature" %in% names(raw)) raw$feature[[i]] else NA_character_
      if (!is.na(model) && nzchar(model)) {
        item$entities <- entity(model, feature)
      } else if ("entities" %in% names(raw)) {
        item$entities <- raw$entities[[i]]
      }
      findings[[length(findings) + 1L]] <- item
    }
  }
  structure(list(
    identity = identity, evaluation = result$evaluation, audit = audit,
    effects = effects, effect_failures = failures, findings = findings,
    diagnostics = diagnostics
  ), class = "autoxplain_report_view")
}

# Optional report checks have the same state representation as retained audit
# checks. Preparation returns a new local result; it does not mutate its caller.
report_optional_diagnostic <- function(id, result, compute, applicable = TRUE, reason = NULL) {
  scope <- "supplied fitted models and evaluation rows"
  if (!applicable) {
    return(report_diagnostic_record(id, "not_applicable", scope, reason = reason))
  }
  tryCatch(
    report_diagnostic_record(id, "computed", scope, evidence = compute()),
    error = function(error) {
      report_diagnostic_record(
        id, "failed", scope,
        reason = conditionMessage(error)
      )
    }
  )
}

prepare_report_diagnostics <- function(result) {
  multiple <- length(result$models) > 2L
  binary <- identical(result$task, "binary")
  records <- list(
    resources = report_optional_diagnostic(
      "resources", result,
      function() model_tradeoffs(result), multiple, "No additional candidate models were supplied."
    ),
    model_behavior = report_optional_diagnostic(
      "model_behavior", result,
      function() compare_model_behavior(result), multiple, "No additional candidate models were supplied."
    ),
    prediction_disagreement = report_optional_diagnostic(
      "prediction_disagreement", result,
      function() prediction_ambiguity(result), multiple, "No additional candidate models were supplied."
    ),
    decision_cutoffs = report_optional_diagnostic(
      "decision_cutoffs", result,
      function() threshold_diagnostics(result, thresholds = c(0.3, 0.5, 0.7)),
      binary, "Decision-cutoff comparisons apply to binary classification."
    )
  )
  result$explanations$report_diagnostics <- records
  result
}

report_get_diagnostic <- function(result, id, compute) {
  result$explanations$report_diagnostics[[id]] %||%
    report_optional_diagnostic(id, result, compute)
}
