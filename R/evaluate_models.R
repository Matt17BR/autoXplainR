result_class_levels <- function(result) {
  result$prediction_schema$class_levels %||%
    if (identical(result$task, "regression")) NULL else levels(result$training_data[[result$target_column]])
}

result_positive_class <- function(result) {
  if (!identical(result$task, "binary")) {
    return(NULL)
  }
  result$prediction_schema$positive %||% result_class_levels(result)[[2L]]
}

result_training_rows <- function(result) {
  if (isFALSE(result$training_available) || is.null(result$training_data)) {
    return(NA_integer_)
  }
  nrow(result$training_data)
}

result_reference_id <- function(result) {
  if (identical(result$provenance$workflow, "supplied-model evaluation")) {
    return(result$provenance$reference_model_id)
  }
  result$provenance$reference_model_id %||%
    if ("simple_baseline" %in% names(result$models)) "simple_baseline" else NULL
}

result_prediction_contract <- function(result, id) {
  result$prediction_contracts[[id]] %||% list(
    predict_function = NULL,
    positive = result_positive_class(result), class_levels = result_class_levels(result)
  )
}

#' Evaluate already fitted models on explicitly supplied observations
#'
#' Connects an existing modeling workflow to the same prediction, explanation,
#' data and report tools as [autoxplain()]. No model is fitted, selected, or
#' replaced. The default evaluation role makes no claim that these rows were
#' excluded from training or model selection. Classification requires numeric
#' probabilities, with factor outcome levels declaring the complete class set.
#'
#' Reusable evidence requires deterministic predictors with explicit model and
#' lexical state. Referenced closure inputs, custom S3 prediction methods and
#' supported native formula, offset, contrast and inverse-link extensions are
#' checked for changes. Arbitrary dynamic lookup, external mutable dependencies
#' and stochastic prediction callbacks are unsupported; this is not a general
#' proof of purity for R code. If a model or its assessment inputs change, call
#' `evaluate_models()` again to create a new assessment before reporting.
#'
#' @param models A list of fitted objects with unique non-empty model IDs.
#' @param data Evaluation observations, including the outcome.
#' @param outcome Name of the outcome column.
#' @param task Prediction task, or `"auto"` to infer it from the outcome.
#' @param predict_functions Optional named list of custom prediction functions,
#'   keyed by model ID. Functions take `newdata`, or `model, newdata`, as in
#'   [explain_model()]. They receive the declared `features` and own any
#'   preprocessing needed by their fitted model. No recipe is learned here.
#'   Binary numeric outputs must refer to `positive`.
#' @param labels Optional named character vector of display labels.
#' @param primary ID of the user-chosen primary model; defaults to the first ID.
#' @param reference Optional ID of an explicitly supplied comparison reference.
#'   It must differ from `primary`. No intercept-only model is fitted or inferred.
#' @param training_data Optional training observations supplied only as context.
#'   Their use in fitting cannot be verified. `NULL` records training unavailable.
#' @param evaluation_role User-declared role: `"evaluation"` (neutral default),
#'   `"test"`, `"validation"`, or `"training"`. The package cannot verify the role.
#' @param positive Binary event label. Defaults to the second declared level.
#' @param features Predictor columns; `NULL` uses every column except `outcome`.
#'   Choose explicitly to keep identifiers or future-only information out of
#'   the model-input and explanation views. Other supplied columns remain local
#'   data context and require explicit `context_columns` in [report_data_control()]
#'   to enter the report's data explorer.
#' @param seed Seed recorded for subsequent explanation randomization.
#' @return An `autoxplain_result`. Call [render_model_report()] to prepare its
#'   explanations and report. Custom prediction functions are retained for
#'   [predict()] and [as_explainers()]; required packages and registered prediction
#'   methods must remain available after serialization.
#' @export
#' @examples
#' fitted <- lm(mpg ~ wt + hp, data = mtcars[1:20, ])
#' evaluated <- evaluate_models(list(linear = fitted), mtcars[21:32, ], "mpg",
#'   features = c("wt", "hp")
#' )
#' predict(evaluated, mtcars[1:2, ])
evaluate_models <- function(models, data, outcome,
                            task = c("auto", "regression", "binary", "multiclass"),
                            predict_functions = NULL, labels = NULL,
                            primary = names(models)[[1L]], reference = NULL,
                            training_data = NULL, evaluation_role = "evaluation",
                            positive = NULL, features = NULL, seed = 123L) {
  task <- match.arg(task)
  if (!is.list(models) || !length(models) || is.null(names(models)) || anyNA(names(models)) ||
        any(!nzchar(names(models))) || anyDuplicated(names(models)) || any(vapply(models, is.null, logical(1)))) {
    stop("`models` must be a non-empty named list of non-NULL fitted objects with unique IDs.", call. = FALSE)
  }
  ids <- names(models)
  assert_data_frame(data, "data")
  if (!is.character(outcome) || length(outcome) != 1L || is.na(outcome) || !outcome %in% names(data)) {
    stop("`outcome` must name one column in `data`.", call. = FALSE)
  }
  features <- features %||% setdiff(names(data), outcome)
  if (!is.character(features) || !length(features) || anyNA(features) || anyDuplicated(features) ||
        any(!features %in% names(data)) || outcome %in% features) {
    stop("`features` must identify unique predictor columns, excluding the outcome.", call. = FALSE)
  }
  check_id <- function(id, name) {
    if (!is.character(id) || length(id) != 1L || is.na(id) || !id %in% ids) {
      stop("`", name, "` must identify one supplied model.", call. = FALSE)
    }
  }
  check_id(primary, "primary")
  if (!is.null(reference)) {
    check_id(reference, "reference")
    if (identical(reference, primary)) stop("`reference` must differ from `primary`.", call. = FALSE)
  }
  if (!is.character(evaluation_role) || length(evaluation_role) != 1L ||
        is.na(evaluation_role) || !evaluation_role %in% c("evaluation", "test", "validation", "training")) {
    stop("`evaluation_role` must be evaluation, test, validation, or training, as declared by the user.", call. = FALSE)
  }
  seed <- assert_count(seed, "seed", minimum = 0L)
  predict_functions <- predict_functions %||% list()
  invalid_adapters <- length(predict_functions) && (
    is.null(names(predict_functions)) || anyNA(names(predict_functions)) ||
      anyDuplicated(names(predict_functions)) || any(!names(predict_functions) %in% ids) ||
      any(!vapply(predict_functions, is.function, logical(1)))
  )
  if (!is.list(predict_functions) || invalid_adapters) {
    stop("`predict_functions` must be a named list of functions keyed by supplied model ID.", call. = FALSE)
  }
  labels <- labels %||% stats::setNames(ids, ids)
  if (!is.character(labels) || is.null(names(labels)) || anyNA(labels) || anyNA(names(labels)) ||
        anyDuplicated(names(labels)) || !setequal(names(labels), ids) || any(!nzchar(labels))) {
    stop("`labels` must be a named non-empty label for every model ID.", call. = FALSE)
  }
  evaluation <- data[c(features, outcome)]
  if (task == "auto" && is.factor(evaluation[[outcome]]) && nlevels(evaluation[[outcome]]) >= 2L) {
    task <- if (nlevels(evaluation[[outcome]]) == 2L) "binary" else "multiclass"
  }
  explainers <- stats::setNames(lapply(ids, function(id) {
    explain_model(models[[id]], evaluation, outcome,
      task = task, label = id,
      predict_function = predict_functions[[id]], positive = positive,
      metadata = list(evaluation_role = evaluation_role, source = "supplied-model evaluation")
    )
  }), ids)
  assert_common_evaluation(explainers)
  task <- explainers[[1L]]$task
  class_levels <- explainers[[1L]]$class_levels
  positive <- explainers[[1L]]$positive
  if (task != "regression") evaluation[[outcome]] <- factor(explainers[[1L]]$y, levels = class_levels)
  if (!is.null(training_data)) {
    assert_data_frame(training_data, "training_data")
    if (!all(c(features, outcome) %in% names(training_data)) || anyDuplicated(names(training_data))) {
      stop("Training context must contain uniquely named predictors and outcome.", call. = FALSE)
    }
  }
  contracts <- lapply(explainers, function(explainer) {
    list(
      predict_function = predict_functions[[explainer$label]],
      task = task, class_levels = class_levels, positive = positive,
      custom_instance = explainer$provenance$custom_instance
    )
  })
  evaluated <- lapply(explainers, function(explainer) {
    predictions <- explainer$reference_predictions
    if (identical(explainer$prediction_type, "class")) {
      stop("evaluate_models() requires classification probabilities; use explain_model() for hard-label audits.",
        call. = FALSE
      )
    }
    calibration <- if (task == "regression") NULL else calibration_from_explainer(explainer, predicted = predictions)
    metrics <- evaluate_predictions(explainer$y, predictions, explainer)
    if (!is.null(calibration)) metrics <- c(metrics, calibration_error = calibration$calibration_error)
    list(metrics = metrics, predictions = predictions, calibration = calibration, explainer = explainer)
  })
  primary_metric <- if (task == "regression") "rmse" else "log_loss"
  metrics <- lapply(evaluated, `[[`, "metrics")
  prediction_table <- guided_prediction_table(evaluated, task, primary)
  if (is.null(reference)) {
    prediction_table <- prediction_table[!grepl("^baseline_", names(prediction_table))]
  } else {
    reference_predictions <- evaluated[[reference]]$predictions
    if (task == "regression") prediction_table$baseline_prediction <- as.numeric(reference_predictions)
    if (task == "binary") prediction_table$baseline_probability <- as.numeric(reference_predictions)
    if (task == "multiclass") {
      prediction_table$baseline_prediction <- class_levels[max.col(reference_predictions, ties.method = "first")]
    }
  }
  scores <- vapply(metrics, function(value) value[[primary_metric]], numeric(1))
  roles <- stats::setNames(rep("alternative", length(ids)), ids)
  roles[[primary]] <- "primary"
  if (!is.null(reference)) roles[[reference]] <- "baseline"
  known_family <- function(model) {
    if (inherits(model, c("lm", "glm", "multinom", "rpart", "autoxplain_tuned_nnet", "autoxplain_fitted_model"))) {
      model_family_name(model)
    } else {
      "custom"
    }
  }
  family <- vapply(models, known_family, character(1))
  leaderboard <- data.frame(
    rank = rank(scores, ties.method = "min"), model_id = ids,
    model = unname(labels[ids]), role = unname(roles), family = unname(family),
    backend = vapply(models, model_backend_name, character(1)), stringsAsFactors = FALSE
  )
  for (metric in names(metrics[[1L]])) {
    leaderboard[[metric]] <- vapply(metrics, function(value) value[[metric]], numeric(1))
  }
  leaderboard$model_size_kb <- vapply(models, function(model) as.numeric(utils::object.size(model)) / 1024, numeric(1))
  leaderboard$complexity <- vapply(seq_along(models), function(i) {
    if (family[[i]] == "custom") NA_real_ else model_complexity(models[[i]])
  }, numeric(1))
  leaderboard$training_time_ms <- NA_real_
  leaderboard$prediction_time_ms <- NA_real_
  leaderboard$fit_warning <- "Training provenance was not recorded by AutoXplainR."
  diagnostics <- leaderboard[c(
    "model_id", "training_time_ms", "prediction_time_ms", "model_size_kb",
    "complexity", "fit_warning"
  )]
  leaderboard <- leaderboard[order(leaderboard$rank, leaderboard$model_id), , drop = FALSE]
  rownames(leaderboard) <- NULL
  improvement <- if (!is.null(reference) && is.finite(scores[[reference]]) && scores[[reference]] > 0) {
    (scores[[reference]] - scores[[primary]]) / scores[[reference]]
  } else {
    NA_real_
  }
  training <- if (is.null(training_data)) NULL else training_data[c(features, outcome)]
  processed_training <- list(data = training, row_indices = seq_len(nrow(training) %||% 0L))
  processed_evaluation <- list(data = evaluation, row_indices = seq_len(nrow(evaluation)))
  result <- structure(list(
    schema_version = "2.0", engine = "supplied", models = models, model_labels = labels,
    task = task, target_column = outcome, features = features, training_data = training,
    training_available = !is.null(training), test_data = evaluation, evaluation_data = evaluation,
    evaluation_context = data[setdiff(names(data), outcome)], evaluation_row_indices = seq_len(nrow(data)),
    prediction_contracts = contracts,
    prediction_schema = list(
      features = features, prototypes = evaluation[FALSE, features, drop = FALSE],
      class_levels = class_levels, positive = positive
    ),
    leaderboard = leaderboard, model_diagnostics = diagnostics, model_characteristics = NULL,
    tuning = NULL, automl_object = NULL, explanations = NULL,
    preprocessing_metadata = list(
      enabled = FALSE, training_data = processed_training,
      test_data = processed_evaluation,
      contract = "No preprocessing learned; supplied adapters own their prediction pipeline."
    ),
    data_context = capture_data_context(training_data, data, outcome, features,
      processed_training, processed_evaluation,
      evaluation_source = "data", split_method = "user supplied", training_source = "training_data"
    ),
    evaluation = list(
      primary_metric = primary_metric, primary_model_id = primary,
      reference_model_id = reference, winner = names(which.min(scores))[[1L]], metrics = metrics,
      improvement_over_baseline = improvement, beats_baseline = is.finite(improvement) && improvement > 0,
      metric_definitions = metric_definitions(task), evaluated_rows = nrow(data),
      predictions = prediction_table,
      diagnostics = guided_prediction_diagnostics(evaluated, task, primary),
      notes = data.frame(
        severity = "note", code = "user_supplied_evaluation",
        message = paste(
          "Models, primary choice, and evaluation rows were supplied by the user;",
          "fitting and selection were not observed."
        ),
        recommendation = "Use independent evaluation rows after selecting models or decision rules.",
        stringsAsFactors = FALSE
      )
    ),
    provenance = list(
      workflow = "supplied-model evaluation", created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
      package_version = package_version_or_development(),
      r_version = paste(R.version$major, R.version$minor, sep = "."),
      seed = seed, model_set = "supplied", engine_requested = "supplied", evaluation_role = evaluation_role,
      evaluation_role_source = "user declaration", split_method = "user supplied",
      training_rows = nrow(training) %||% NA_integer_,
      training_status = if (is.null(training)) "unavailable" else "supplied context; use in fitting not verified",
      evaluation_rows = nrow(data), primary_model_id = primary, primary_model_label = labels[[primary]],
      reference_model_id = reference,
      baseline = if (is.null(reference)) "No reference model supplied" else labels[[reference]],
      candidate_selection = "The primary model was explicitly chosen by the user; evaluation ranks are descriptive."
    )
  ), class = "autoxplain_result")
  seal_evaluation_result(result)
}

supplied_explainers <- function(result, data = NULL, models = NULL) {
  evaluation <- data %||% result$test_data
  assert_data_frame(evaluation, "data")
  if (!all(c(result$features, result$target_column) %in% names(evaluation))) {
    stop("Evaluation data must contain the recorded predictors and outcome.", call. = FALSE)
  }
  evaluation <- evaluation[c(result$features, result$target_column)]
  if (result$task != "regression") {
    values <- as.character(evaluation[[result$target_column]])
    if (anyNA(values) || any(!values %in% result_class_levels(result))) {
      stop("Evaluation outcomes contain missing or undeclared classes.", call. = FALSE)
    }
    evaluation[[result$target_column]] <- factor(values, levels = result_class_levels(result))
  }
  selected <- select_models(result$models, models)
  stats::setNames(lapply(names(selected), function(id) {
    contract <- result_prediction_contract(result, id)
    explainer <- explain_model(selected[[id]], evaluation, result$target_column,
      task = result$task,
      label = id, positive = contract$positive, predict_function = contract$predict_function,
      metadata = list(
        evaluation_role = if (is.null(data)) result$provenance$evaluation_role else "evaluation",
        primary_metric = result$evaluation$primary_metric, source = "supplied-model evaluation"
      )
    )
    if (identical(explainer$prediction_type, "class")) {
      stop("The supplied model no longer returns the recorded classification probabilities.", call. = FALSE)
    }
    if (!is.null(contract$custom_instance)) {
      explainer$provenance$custom_instance <- contract$custom_instance
      explainer$provenance$fingerprint <- explainer_content_fingerprint(
        explainer$model, explainer$data, explainer$y, explainer$task, explainer$positive,
        explainer$class_levels, explainer$reference_predictions, contract$custom_instance, explainer$predict_function
      )
    }
    explainer
  }), names(selected))
}

predict_supplied_result <- function(result, newdata, model = NULL, type = c("response", "class")) {
  type <- match.arg(type)
  assert_data_frame(newdata, "newdata")
  if (anyDuplicated(names(newdata)) || !all(result$features %in% names(newdata))) {
    stop("`newdata` must contain uniquely named recorded model features.", call. = FALSE)
  }
  if (type == "class" && result$task == "regression") stop("Class predictions require classification.", call. = FALSE)
  selected <- select_models(result$models, model %||% result$provenance$primary_model_id)
  if (length(selected) != 1L) stop("Select exactly one model for prediction.", call. = FALSE)
  contract <- result_prediction_contract(result, names(selected)[[1L]])
  n <- nrow(newdata)
  if (!n) {
    if (type == "class") {
      return(factor(character(), levels = contract$class_levels))
    }
    if (result$task == "multiclass") {
      return(matrix(numeric(), 0L, length(contract$class_levels),
        dimnames = list(NULL, contract$class_levels)
      ))
    }
    return(numeric())
  }
  adapter <- make_prediction_adapter(selected[[1L]], result$task,
    positive = contract$positive,
    class_levels = contract$class_levels, predict_function = contract$predict_function
  )
  prediction <- adapter(newdata[result$features])
  validate_predictions(prediction, n, result$task, contract$class_levels)
  if (result$task != "regression" && !is.numeric(prediction)) {
    stop("The supplied model no longer returns the recorded classification probabilities.", call. = FALSE)
  }
  if (type == "class") {
    labels <- if (result$task == "binary") {
      ifelse(prediction >= .5, contract$positive, setdiff(contract$class_levels, contract$positive)[[1L]])
    } else {
      contract$class_levels[max.col(prediction, ties.method = "first")]
    }
    return(factor(labels, levels = contract$class_levels))
  }
  prediction
}
