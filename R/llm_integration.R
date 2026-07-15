#' Generate an evidence-constrained narrative
#'
#' Produces a deterministic local narrative by default, or sends only an
#' aggregated analysis context to the Gemini API when an API key is supplied.
#' Raw rows, model objects, and predictions are never included in the prompt.
#' The generated text is explicitly secondary to the numeric audit.
#'
#' @param autoxplain_result An `autoxplain_result` or `autoxplain_audit`.
#' @param importance_data Optional permutation importance data.
#' @param pdp_data Optional feature-effect list.
#' @param model_characteristics Optional model metadata.
#' @param audit Optional `autoxplain_audit`.
#' @param api_key Gemini API key. When `NULL`, `GEMINI_API_KEY` is consulted.
#'   If no key is available, a local deterministic narrative is returned.
#' @param model Gemini model identifier.
#' @param max_tokens Maximum response tokens.
#' @param temperature Sampling temperature.
#' @param use_remote Whether remote generation is allowed. Set `FALSE` to
#'   guarantee local-only operation.
#' @param fallback Return the deterministic narrative when the remote call
#'   fails.
#'
#' @return A single character string.
#' @export
generate_natural_language_report <- function(autoxplain_result,
                                             importance_data = NULL,
                                             pdp_data = NULL,
                                             model_characteristics = NULL,
                                             audit = NULL,
                                             api_key = NULL,
                                             model = "gemini-3.5-flash",
                                             max_tokens = 1000L,
                                             temperature = 0.2,
                                             use_remote = TRUE,
                                             fallback = TRUE) {
  max_tokens <- assert_count(max_tokens, "max_tokens")
  if (length(temperature) != 1L || !is.numeric(temperature) || !is.finite(temperature) ||
        temperature < 0 || temperature > 2) {
    stop("`temperature` must be between 0 and 2.", call. = FALSE)
  }
  if (inherits(autoxplain_result, "autoxplain_audit")) {
    audit <- autoxplain_result
    autoxplain_result <- NULL
  }
  if (!is.null(autoxplain_result) && !inherits(autoxplain_result, "autoxplain_result")) {
    stop("The first argument must be an AutoXplainR result or audit.", call. = FALSE)
  }
  if (!is.null(audit) && !inherits(audit, "autoxplain_audit")) {
    stop("`audit` must be returned by `audit_explanations()`.", call. = FALSE)
  }
  context <- if (!is.null(audit)) {
    prepare_audit_context(audit)
  } else {
    prepare_analysis_context(
      autoxplain_result, importance_data, pdp_data, model_characteristics
    )
  }
  local_report <- create_fallback_report(context)
  if (!isTRUE(use_remote)) return(local_report)
  api_key <- api_key %||% Sys.getenv("GEMINI_API_KEY", unset = "")
  if (!nzchar(api_key)) return(local_report)
  prompt <- create_report_prompt(context)
  tryCatch(
    call_gemini_api(prompt, api_key, model, max_tokens, temperature),
    error = function(error) {
      if (!isTRUE(fallback)) stop(error)
      warning("Remote narrative failed; returning the local evidence summary: ",
              conditionMessage(error), call. = FALSE)
      local_report
    }
  )
}

prepare_audit_context <- function(audit) {
  top <- audit$importance[order(audit$importance$importance, decreasing = TRUE), , drop = FALSE]
  top <- top[!duplicated(top$feature), , drop = FALSE]
  top <- head(top, 8L)
  list(
    context_type = "audit",
    task_type = audit$provenance$automl_task %||% "unspecified",
    target_column = audit$provenance$automl_target %||% "unspecified",
    n_features = length(audit$config$features),
    n_models = audit$summary$n_models,
    best_model_type = audit$performance$model[[which.min(audit$performance$relative_gap)]],
    best_performance = audit$performance$score[[which.min(audit$performance$relative_gap)]],
    best_metric = audit$config$metric,
    grade = audit$summary$grade,
    grade_note = audit$summary$grade_note,
    stable_claim_rate = audit$summary$stable_claim_rate,
    importance_summary = list(
      top_features = top$feature,
      evidence_grades = as.character(top$evidence_grade),
      claims = top$claim,
      metric = audit$config$metric
    ),
    findings = audit$findings[c("severity", "code", "message", "recommendation")],
    pdp_summary = NULL,
    disclosure = paste(
      "Aggregated diagnostics only. No raw rows, model objects, case-level",
      "predictions, or secrets are included."
    )
  )
}

prepare_analysis_context <- function(autoxplain_result,
                                     importance_data = NULL,
                                     pdp_data = NULL,
                                     model_characteristics = NULL) {
  if (!inherits(autoxplain_result, "autoxplain_result")) {
    stop("`autoxplain_result` must be returned by `autoxplain()`.", call. = FALSE)
  }
  leaderboard <- as.data.frame(autoxplain_result$leaderboard)
  numeric_columns <- names(leaderboard)[vapply(leaderboard, is.numeric, logical(1))]
  metric <- if (length(numeric_columns)) numeric_columns[[1L]] else "unavailable"
  performance <- if (length(numeric_columns)) leaderboard[[metric]][[1L]] else NA_real_
  importance_summary <- if (!is.null(importance_data) && is.data.frame(importance_data) &&
                              all(c("feature", "importance") %in% names(importance_data))) {
    ordered <- importance_data[order(importance_data$importance, decreasing = TRUE), , drop = FALSE]
    list(top_features = head(ordered$feature, 3L), metric = attr(importance_data, "metric") %||%
           "permutation importance")
  } else {
    NULL
  }
  list(
    context_type = "automl",
    task_type = autoxplain_result$task %||%
      detect_task(autoxplain_result$training_data[[autoxplain_result$target_column]]),
    target_column = autoxplain_result$target_column,
    n_features = length(autoxplain_result$features),
    n_models = length(autoxplain_result$models),
    best_model_type = extract_model_type(names(autoxplain_result$models)[[1L]]),
    best_performance = performance,
    best_metric = metric,
    importance_summary = importance_summary,
    pdp_summary = if (!is.null(pdp_data)) {
      list(features_analyzed = names(pdp_data), n_features = length(pdp_data))
    } else {
      NULL
    },
    disclosure = "Aggregated AutoML metadata only; no raw rows are included."
  )
}

create_report_prompt <- function(context) {
  serialized <- context_to_text(context)
  paste0(
    "You are writing a short model-explanation evidence memo. Use only the facts below.\n",
    "Rules:\n",
    "- Never imply causality, fairness, safety, or regulatory compliance.\n",
    "- Distinguish descriptive permutation variability from population inference.\n",
    "- Treat grade labels as heuristic diagnostics, never certification.\n",
    "- Lead with critical limitations and disagreement before feature rankings.\n",
    "- Do not invent metrics, model behavior, domain meaning, or recommendations.\n",
    "- Use plain language and at most 500 words.\n\n",
    serialized
  )
}

context_to_text <- function(context) {
  lines <- c(
    paste("Task:", context$task_type %||% "unspecified"),
    paste("Target:", context$target_column %||% "unspecified"),
    paste("Models:", context$n_models %||% "unspecified"),
    paste("Features:", context$n_features %||% "unspecified"),
    paste("Reference model:", context$best_model_type %||% "unspecified"),
    paste("Reference score:", context$best_metric %||% "metric", "=",
          context$best_performance %||% "unavailable")
  )
  if (!is.null(context$grade)) {
    lines <- c(lines, paste("Diagnostic evidence grade:", context$grade),
               paste("Stable claim rate:", format_percent(context$stable_claim_rate)))
  }
  if (!is.null(context$importance_summary)) {
    lines <- c(lines, paste("Top features:",
                            paste(context$importance_summary$top_features, collapse = ", ")))
  }
  if (!is.null(context$findings) && nrow(context$findings)) {
    finding_lines <- apply(context$findings, 1L, function(row) {
      paste0("- [", row[["severity"]], "] ", row[["message"]],
             " Action: ", row[["recommendation"]])
    })
    lines <- c(lines, "Audit findings:", finding_lines)
  }
  lines <- c(lines, paste("Disclosure:", context$disclosure %||% "Aggregated context only."))
  paste(lines, collapse = "\n")
}

call_gemini_api <- function(prompt, api_key, model, max_tokens, temperature) {
  require_optional("httr", "the optional Gemini narrative")
  require_optional("jsonlite", "the optional Gemini narrative")
  if (!is.character(model) || length(model) != 1L || !nzchar(model)) {
    stop("`model` must be a non-empty Gemini model identifier.", call. = FALSE)
  }
  endpoint <- paste0(
    "https://generativelanguage.googleapis.com/v1beta/models/",
    utils::URLencode(model, reserved = TRUE),
    ":generateContent"
  )
  body <- list(
    contents = list(list(role = "user", parts = list(list(text = prompt)))),
    generationConfig = list(
      temperature = temperature,
      maxOutputTokens = max_tokens,
      candidateCount = 1L
    )
  )
  response <- httr::POST(
    endpoint,
    httr::add_headers(
      `x-goog-api-key` = api_key,
      `Content-Type` = "application/json"
    ),
    body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")),
    encode = "raw",
    httr::timeout(30)
  )
  raw_text <- httr::content(response, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(jsonlite::fromJSON(raw_text, simplifyVector = FALSE),
                     error = function(error) NULL)
  if (httr::http_error(response)) {
    message <- parsed$error$message %||% paste("HTTP", httr::status_code(response))
    stop("Gemini request failed: ", message, call. = FALSE)
  }
  parts <- parsed$candidates[[1L]]$content$parts %||% list()
  text <- paste(vapply(parts, function(part) part$text %||% "", character(1)), collapse = "\n")
  if (!nzchar(text)) stop("Gemini returned no narrative text.", call. = FALSE)
  text
}

create_fallback_report <- function(context) {
  heading <- if (identical(context$context_type, "audit")) {
    "# Explanation Evidence Report"
  } else {
    "# AutoML Analysis Report"
  }
  lines <- c(
    heading,
    "",
    "## Scope",
    paste0(
      "This is a descriptive summary of a ", context$task_type %||% "modeling",
      " task for `", context$target_column %||% "unspecified", "`, covering ",
      context$n_models %||% "an unspecified number of", " model(s) and ",
      context$n_features %||% "an unspecified number of", " feature(s)."
    )
  )
  if (!is.null(context$grade)) {
    lines <- c(
      lines, "", "## Reliability first",
      paste0("The heuristic explanation-evidence grade is **", context$grade,
             "**; this is a diagnostic, not a certification."),
      paste0("The stable-claim rate is ", format_percent(context$stable_claim_rate), ".")
    )
  }
  if (!is.null(context$findings) && nrow(context$findings)) {
    lines <- c(lines, "", "## Findings")
    for (index in seq_len(nrow(context$findings))) {
      lines <- c(lines, paste0(
        "- **", context$findings$severity[[index]], " - ",
        context$findings$code[[index]], ":** ", context$findings$message[[index]],
        " ", context$findings$recommendation[[index]]
      ))
    }
  }
  if (!is.null(context$importance_summary)) {
    lines <- c(
      lines, "", "## Feature evidence",
      paste0(
        "The leading descriptive permutation-importance features are ",
        paste(context$importance_summary$top_features, collapse = ", "),
        ". They describe model reliance on the evaluation data and should not be read as causal effects."
      )
    )
  }
  if (!is.null(context$pdp_summary)) {
    lines <- c(
      lines, "", "## Partial Dependence",
      paste0("Partial dependence was evaluated for ", context$pdp_summary$n_features,
             " feature(s). Inspect support and dependence warnings before interpreting the curves.")
    )
  }
  lines <- c(
    lines, "", "## Required limitations",
    "Permutation repeats quantify computation-level variability, not population uncertainty. ",
    "Feature explanations do not establish causality, fairness, safety, or deployment readiness. ",
    "Use held-out data, domain review, and external validation.",
    "", paste0("Data disclosure: ", context$disclosure %||% "Aggregated context only.")
  )
  paste(lines, collapse = "\n")
}

extract_model_type_simple <- function(model_id) extract_model_type(model_id)
