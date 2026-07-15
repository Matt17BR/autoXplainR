#' List supported narrative providers
#'
#' Provider defaults reflect public offerings checked in July 2026 and can
#' change independently of AutoXplainR. The local provider is deterministic;
#' every remote provider is opt-in.
#'
#' @return A data frame describing providers, default models, and credentials.
#' @export
narrative_providers <- function() {
  data.frame(
    provider = c("local", "gemini", "groq", "ollama", "openrouter", "custom"),
    remote = c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE),
    default_model = c(
      NA_character_, "gemini-3.5-flash", "qwen/qwen3.6-27b", "gemma3:4b",
      "openrouter/free", NA_character_
    ),
    key_environment_variable = c(
      NA_character_, "GEMINI_API_KEY", "GROQ_API_KEY", NA_character_,
      "OPENROUTER_API_KEY", NA_character_
    ),
    reproducibility = c(
      "deterministic", "model pinned", "model pinned", "local model pinned",
      "router may select different free models", "depends on endpoint"
    ),
    stringsAsFactors = FALSE
  )
}

#' Generate an evidence-constrained narrative
#'
#' Produces a deterministic local narrative by default. Remote or locally
#' hosted generative models are used only when `provider` is set explicitly.
#' Raw rows, fitted objects, case-level predictions, and secrets are never
#' included in the prompt. Generated prose remains secondary to the computed
#' evaluation and explanation evidence.
#'
#' @param autoxplain_result An `autoxplain_result` or `autoxplain_audit`.
#' @param importance_data Optional permutation importance data.
#' @param pdp_data Optional feature-effect list.
#' @param model_characteristics Optional model metadata.
#' @param audit Optional `autoxplain_audit`.
#' @param provider One of `"local"`, `"gemini"`, `"groq"`, `"ollama"`,
#'   `"openrouter"`, or `"custom"`. The default is always `"local"`.
#' @param api_key Provider API key. For an explicitly selected hosted provider,
#'   the corresponding environment variable shown by [narrative_providers()]
#'   is consulted when this is `NULL`.
#' @param model Model identifier. `NULL` uses the provider default shown by
#'   [narrative_providers()]. A custom provider requires an explicit model.
#' @param base_url Optional endpoint override. A custom provider requires it.
#' @param max_tokens Maximum response tokens.
#' @param temperature Sampling temperature.
#' @param timeout Request timeout in seconds.
#' @param fallback Return the deterministic narrative when the remote call
#'   fails.
#' @param use_remote Deprecated compatibility switch. `FALSE` forces local
#'   generation; `TRUE` with no explicit provider selects Gemini. Prefer
#'   `provider`.
#' @param transport Optional advanced request function for testing or custom
#'   networking. It receives one request list and must return narrative text.
#'
#' @return A single character string with a `narrative_provenance` attribute.
#' @export
generate_natural_language_report <- function(autoxplain_result,
                                             importance_data = NULL,
                                             pdp_data = NULL,
                                             model_characteristics = NULL,
                                             audit = NULL,
                                             provider = c(
                                               "local", "gemini", "groq", "ollama",
                                               "openrouter", "custom"
                                             ),
                                             api_key = NULL,
                                             model = NULL,
                                             base_url = NULL,
                                             max_tokens = 1000L,
                                             temperature = 0.2,
                                             timeout = 30,
                                             fallback = TRUE,
                                             use_remote = NULL,
                                             transport = NULL) {
  provider_was_missing <- missing(provider)
  provider <- match.arg(provider)
  max_tokens <- assert_count(max_tokens, "max_tokens")
  if (length(temperature) != 1L || !is.numeric(temperature) || !is.finite(temperature) ||
        temperature < 0 || temperature > 2) {
    stop("`temperature` must be between 0 and 2.", call. = FALSE)
  }
  if (length(timeout) != 1L || !is.numeric(timeout) || !is.finite(timeout) || timeout <= 0) {
    stop("`timeout` must be one positive number of seconds.", call. = FALSE)
  }
  if (!is.logical(fallback) || length(fallback) != 1L || is.na(fallback)) {
    stop("`fallback` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(use_remote)) {
    if (!is.logical(use_remote) || length(use_remote) != 1L || is.na(use_remote)) {
      stop("`use_remote` must be TRUE, FALSE, or NULL.", call. = FALSE)
    }
    if (!use_remote) provider <- "local"
    if (use_remote && provider_was_missing) provider <- "gemini"
  }
  if (!is.null(transport) && !is.function(transport)) {
    stop("`transport` must be a function or NULL.", call. = FALSE)
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
  if (provider == "local") {
    return(annotate_narrative(
      local_report, provider, "local", NULL, remote = FALSE,
      fallback = FALSE, context$disclosure
    ))
  }
  prompt <- create_report_prompt(context)
  tryCatch(
    {
      provider_config <- resolve_narrative_provider(provider, model, base_url, api_key)
      request <- build_narrative_request(
        prompt = prompt,
        config = provider_config,
        max_tokens = max_tokens,
        temperature = temperature,
        timeout = timeout
      )
      text <- if (is.null(transport)) perform_narrative_request(request) else transport(request)
      if (!is.character(text) || length(text) != 1L || is.na(text) || !nzchar(trimws(text))) {
        stop("The narrative provider returned no usable text.", call. = FALSE)
      }
      annotate_narrative(
        text, provider, provider, provider_config$model,
        remote = provider_config$remote, fallback = FALSE, context$disclosure
      )
    },
    error = function(error) {
      if (!isTRUE(fallback)) stop(error)
      warning("Narrative provider failed; returning the local evidence summary: ",
              conditionMessage(error), call. = FALSE)
      annotate_narrative(
        local_report, provider, "local", NULL, remote = FALSE,
        fallback = TRUE, context$disclosure, conditionMessage(error)
      )
    }
  )
}

resolve_narrative_provider <- function(provider, model, base_url, api_key) {
  registry <- narrative_providers()
  row <- registry[registry$provider == provider, , drop = FALSE]
  resolved_model <- model %||% row$default_model[[1L]]
  if (!is.character(resolved_model) || length(resolved_model) != 1L ||
        is.na(resolved_model) || !nzchar(resolved_model)) {
    stop("`model` must be supplied for provider `", provider, "`.", call. = FALSE)
  }
  endpoints <- c(
    gemini = "https://generativelanguage.googleapis.com/v1beta/models",
    groq = "https://api.groq.com/openai/v1/chat/completions",
    ollama = "http://localhost:11434/v1/chat/completions",
    openrouter = "https://openrouter.ai/api/v1/chat/completions"
  )
  default_endpoint <- if (provider %in% names(endpoints)) endpoints[[provider]] else NULL
  endpoint <- base_url %||% default_endpoint
  if (!is.character(endpoint) || length(endpoint) != 1L || is.na(endpoint) ||
        !grepl("^https?://", endpoint)) {
    stop("`base_url` must be a complete HTTP(S) endpoint for provider `", provider, "`.",
         call. = FALSE)
  }
  environment_name <- row$key_environment_variable[[1L]]
  resolved_key <- api_key
  if (is.null(resolved_key) && !is.na(environment_name)) {
    resolved_key <- Sys.getenv(environment_name, unset = "")
  }
  key_required <- provider %in% c("gemini", "groq", "openrouter")
  if (key_required && (!is.character(resolved_key) || length(resolved_key) != 1L ||
                         !nzchar(resolved_key))) {
    stop(
      "Provider `", provider, "` requires `api_key` or ", environment_name, ".",
      call. = FALSE
    )
  }
  if (!is.null(resolved_key) &&
        (!is.character(resolved_key) || length(resolved_key) != 1L || is.na(resolved_key))) {
    stop("`api_key` must be a single string or NULL.", call. = FALSE)
  }
  list(
    provider = provider,
    model = resolved_model,
    endpoint = endpoint,
    api_key = resolved_key %||% "",
    remote = !grepl(
      "^https?://(localhost|127[.]0[.]0[.]1|\\[::1\\])(?=[:/])",
      endpoint,
      perl = TRUE
    )
  )
}

build_narrative_request <- function(prompt, config, max_tokens, temperature, timeout) {
  if (config$provider == "gemini") {
    endpoint <- paste0(
      sub("/$", "", config$endpoint), "/",
      utils::URLencode(config$model, reserved = TRUE),
      ":generateContent"
    )
    return(list(
      provider = config$provider,
      model = config$model,
      url = endpoint,
      headers = c(
        `x-goog-api-key` = config$api_key,
        `Content-Type` = "application/json"
      ),
      body = list(
        contents = list(list(role = "user", parts = list(list(text = prompt)))),
        generationConfig = list(
          temperature = temperature,
          maxOutputTokens = max_tokens,
          candidateCount = 1L
        )
      ),
      response_format = "gemini",
      timeout = timeout
    ))
  }
  headers <- c(`Content-Type` = "application/json")
  if (nzchar(config$api_key)) {
    headers <- c(headers, Authorization = paste("Bearer", config$api_key))
  }
  if (config$provider == "openrouter") {
    headers <- c(headers, `X-Title` = "AutoXplainR")
  }
  list(
    provider = config$provider,
    model = config$model,
    url = config$endpoint,
    headers = headers,
    body = list(
      model = config$model,
      messages = list(list(role = "user", content = prompt)),
      temperature = temperature,
      max_tokens = max_tokens,
      stream = FALSE
    ),
    response_format = "openai",
    timeout = timeout
  )
}

perform_narrative_request <- function(request) {
  require_optional("httr", "optional generated narratives")
  require_optional("jsonlite", "optional generated narratives")
  header_call <- c(list(.headers = request$headers), list())
  response <- httr::POST(
    request$url,
    do.call(httr::add_headers, header_call),
    body = charToRaw(jsonlite::toJSON(request$body, auto_unbox = TRUE, null = "null")),
    encode = "raw",
    httr::timeout(request$timeout)
  )
  raw_text <- httr::content(response, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(
    jsonlite::fromJSON(raw_text, simplifyVector = FALSE),
    error = function(error) NULL
  )
  if (httr::http_error(response)) {
    message <- parsed$error$message %||% paste("HTTP", httr::status_code(response))
    stop(request$provider, " request failed: ", message, call. = FALSE)
  }
  text <- if (request$response_format == "gemini") {
    parts <- parsed$candidates[[1L]]$content$parts %||% list()
    paste(vapply(parts, function(part) part$text %||% "", character(1)), collapse = "\n")
  } else {
    parsed$choices[[1L]]$message$content %||% ""
  }
  if (!is.character(text) || length(text) != 1L || !nzchar(trimws(text))) {
    stop(request$provider, " returned no narrative text.", call. = FALSE)
  }
  text
}

annotate_narrative <- function(text,
                               provider_requested,
                               provider_used,
                               model,
                               remote,
                               fallback,
                               disclosure,
                               error = NULL) {
  attr(text, "narrative_provenance") <- list(
    provider_requested = provider_requested,
    provider_used = provider_used,
    model = model,
    remote = remote,
    fallback = fallback,
    disclosure = disclosure,
    error = error
  )
  text
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
  evaluation <- autoxplain_result$evaluation
  if (!is.null(evaluation$primary_metric)) {
    metric <- evaluation$primary_metric
    reference_model <- if ("main_model" %in% names(evaluation$metrics)) {
      "main_model"
    } else {
      evaluation$winner
    }
    performance <- evaluation$metrics[[reference_model]][[metric]]
    baseline_performance <- evaluation$metrics[["simple_baseline"]][[metric]] %||% NA_real_
    metric_definition <- evaluation$metric_definitions[[metric]] %||% "See the metric documentation."
  } else {
    numeric_columns <- setdiff(
      names(leaderboard)[vapply(leaderboard, is.numeric, logical(1))],
      "rank"
    )
    metric <- if (length(numeric_columns)) numeric_columns[[1L]] else "unavailable"
    performance <- if (length(numeric_columns)) leaderboard[[metric]][[1L]] else NA_real_
    baseline_performance <- NA_real_
    metric_definition <- "See the model-engine documentation for this metric."
    reference_model <- names(autoxplain_result$models)[[1L]]
  }
  winner_row <- leaderboard[leaderboard$model_id == reference_model, , drop = FALSE]
  winner_label <- if (nrow(winner_row) && "model" %in% names(winner_row)) {
    winner_row$model[[1L]]
  } else {
    extract_model_type(reference_model)
  }
  importance_summary <- if (!is.null(importance_data) && is.data.frame(importance_data) &&
                              all(c("feature", "importance") %in% names(importance_data))) {
    ordered <- importance_data[order(importance_data$importance, decreasing = TRUE), , drop = FALSE]
    list(top_features = head(ordered$feature, 3L), metric = attr(importance_data, "metric") %||%
           "permutation importance")
  } else {
    NULL
  }
  list(
    context_type = "model_result",
    task_type = autoxplain_result$task %||%
      detect_task(autoxplain_result$training_data[[autoxplain_result$target_column]]),
    target_column = autoxplain_result$target_column,
    engine = autoxplain_result$engine %||% "h2o",
    n_features = length(autoxplain_result$features),
    n_models = length(autoxplain_result$models),
    best_model_type = winner_label,
    best_performance = performance,
    best_metric = metric,
    baseline_performance = baseline_performance,
    improvement_over_baseline = evaluation$improvement_over_baseline %||% NA_real_,
    beats_baseline = evaluation$beats_baseline %||% NA,
    metric_definition = metric_definition,
    evaluation_rows = nrow(autoxplain_result$test_data %||% autoxplain_result$training_data),
    evaluation_role = autoxplain_result$provenance$evaluation_role %||% "unspecified",
    importance_summary = importance_summary,
    pdp_summary = if (!is.null(pdp_data)) {
      list(features_analyzed = names(pdp_data), n_features = length(pdp_data))
    } else {
      NULL
    },
    disclosure = paste(
      "Aggregated diagnostics only. No raw rows, fitted model objects, case-level",
      "predictions, or secrets are included."
    )
  )
}

create_report_prompt <- function(context) {
  serialized <- context_to_text(context)
  paste0(
    "You are writing a short model fit, evaluation, and explanation memo for a first-time modeler. ",
    "Use only the facts below.\n",
    "Rules:\n",
    "- Start with held-out performance and comparison with the simple baseline.\n",
    "- Define technical terms next to the result and use plain language.\n",
    "- Never imply causality, fairness, safety, or regulatory compliance.\n",
    "- Distinguish descriptive permutation variability from population inference.\n",
    "- Treat grade labels as heuristic diagnostics, never certification.\n",
    "- For an explanation audit, lead with critical limitations and disagreement before rankings.\n",
    "- Do not invent metrics, model behavior, domain meaning, or recommendations.\n",
    "- Use plain language and at most 500 words.\n\n",
    serialized
  )
}

context_to_text <- function(context) {
  lines <- c(
    paste("Task:", context$task_type %||% "unspecified"),
    paste("Target:", context$target_column %||% "unspecified"),
    paste("Engine:", context$engine %||% "unspecified"),
    paste("Models:", context$n_models %||% "unspecified"),
    paste("Features:", context$n_features %||% "unspecified"),
    paste("Reference model:", context$best_model_type %||% "unspecified"),
    paste("Reference score:", context$best_metric %||% "metric", "=",
          context$best_performance %||% "unavailable")
  )
  if (!is.null(context$evaluation_role)) {
    lines <- c(
      lines,
      paste("Evaluation role:", context$evaluation_role),
      paste("Evaluation rows:", context$evaluation_rows %||% "unspecified"),
      paste("Metric meaning:", context$metric_definition %||% "unspecified")
    )
  }
  if (is.finite(context$baseline_performance %||% NA_real_)) {
    lines <- c(
      lines,
      paste("Simple baseline score:", context$baseline_performance),
      paste("Relative improvement over baseline:",
            format_percent(context$improvement_over_baseline))
    )
  }
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

create_fallback_report <- function(context) {
  heading <- if (identical(context$context_type, "audit")) {
    "# Explanation Evidence Report"
  } else {
    "# Model Fit and Evaluation Report"
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
  if (!is.null(context$evaluation_role)) {
    lines <- c(
      lines, "", "## Did the model improve on a simple baseline?",
      paste0(
        "The ", context$best_model_type, " was evaluated on ",
        context$evaluation_rows, " ", context$evaluation_role, " rows. Its **",
        context$best_metric, "** was ", report_number(context$best_performance, 4L), "."
      )
    )
    if (is.finite(context$baseline_performance %||% NA_real_)) {
      comparison <- if (isTRUE(context$beats_baseline)) {
        paste0(
          "That is a ", format_percent(context$improvement_over_baseline),
          " improvement over the intercept-only baseline (",
          report_number(context$baseline_performance, 4L), ")."
        )
      } else {
        paste0(
          "It did not improve on the intercept-only baseline (",
          report_number(context$baseline_performance, 4L),
          "). Treat the fitted relationships as exploratory."
        )
      }
      lines <- c(lines, comparison)
    }
    lines <- c(
      lines, "", "## What the main metric means",
      paste0("**", context$best_metric, ":** ", context$metric_definition)
    )
  }
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
    lines, "", "## What to do next",
    paste(
      "Inspect the held-out errors and explanation evidence, check whether the data",
      "represent the intended use, and validate the result on new data before relying on it."
    ),
    "", "## Required limitations",
    "Permutation repeats quantify computation-level variability, not population uncertainty. ",
    "Feature explanations do not establish causality, fairness, safety, or deployment readiness. ",
    "Use held-out data, domain review, and external validation.",
    "", paste0("Data disclosure: ", context$disclosure %||% "Aggregated context only.")
  )
  paste(lines, collapse = "\n")
}

extract_model_type_simple <- function(model_id) extract_model_type(model_id)
