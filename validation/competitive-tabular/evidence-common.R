# Shared verification arithmetic and identity checks. These functions do not fit models.
tabular_scalar <- function(value, label, expected = NULL, minimum = 0) {
  if (!missing(expected)) tabular_scalar(expected, paste(label, "expected value"), minimum = minimum)
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < minimum ||
      (!is.null(expected) && value != expected)) stop("Invalid ", label, ".")
  invisible(value)
}

tabular_ids <- function(ids, expected = NULL, label = "model IDs") {
  if (!is.character(ids) || !length(ids) || anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids) ||
      (!is.null(expected) && !setequal(ids, expected))) stop("Incomplete or duplicate ", label, ".")
  invisible(ids)
}

tabular_same_hash <- function(path, expected, label = basename(path)) {
  if (!is.character(expected) || length(expected) != 1L || !grepl("^[a-f0-9]{64}$", expected) ||
      !file.exists(path) || !identical(tabular_hash(path), expected)) stop("Changed or missing ", label, ".")
}

# Validate the consumed freeze before an acceptance verifier opens any partition file.
tabular_verify_freeze <- function(destination, freeze_path, library_path = NULL, require_success = TRUE) {
  if (!nzchar(freeze_path) || !file.exists(freeze_path)) stop("Acceptance verification is locked; supply the final freeze.")
  process <- jsonlite::read_json(file.path(destination, "process.json"))
  freeze <- jsonlite::read_json(freeze_path)
  permitted_status <- if (require_success) "ok" else c("failed", "timeout")
  stopifnot(identical(process$phase, "acceptance"), process$process_status %in% permitted_status,
    isTRUE(freeze$acceptance_authorized), is.character(freeze$candidate_source_sha256),
    length(freeze$candidate_source_sha256) == 1L, grepl("^[a-f0-9]{64}$", freeze$candidate_source_sha256))
  tabular_same_hash(freeze_path, process$acceptance_freeze_sha256, "consumed acceptance freeze")
  tabular_same_hash(file.path(destination, "acceptance-freeze.json"), process$acceptance_freeze_sha256)
  cache <- tabular_cache()
  tabular_same_hash(file.path(cache, "partitions.json"), freeze$partitions_sha256)
  stopifnot(identical(process$partitions_sha256, freeze$partitions_sha256))
  partitions <- jsonlite::read_json(file.path(cache, "partitions.json"))
  stopifnot(identical(freeze$protocol_sha256, partitions$protocol_sha256),
    identical(freeze$protocol_files[["README.md"]], partitions$protocol_sha256))
  required <- c("README.md", "additional-cohorts.md", "forest-family-acceptance.md", "covertype-multicore.md",
    "native-staging.md", "staged-scoring-retries.md", "forest-validation-budget-v2.md",
    "boosting-anchor-amendment-v2.md", "public-one-call.md", "forest-tree-budget-v3.md",
    "fixed-native-forest-v1.md", "fixed-native-forest-v1.json", "native-bank-original-1t-20260913.md",
    "run.py", "run-one.R", "common.R",
    "evidence-common.R", "acceptance-resources.R", "check-acceptance.R", "replay.R", "score-native.R")
  stopifnot(all(required %in% names(freeze$protocol_files)))
  for (filename in names(freeze$protocol_files)) {
    stopifnot(identical(filename, basename(filename)))
    tabular_same_hash(file.path(destination, "scripts", filename), freeze$protocol_files[[filename]])
    stopifnot(identical(process$scripts[[filename]], freeze$protocol_files[[filename]]))
  }
  entry <- Filter(function(item) all(vapply(c("case", "variant", "request", "stage", "cohort", "threads"),
    function(key) !is.null(item[[key]]) && identical(item[[key]], process[[key]]), logical(1))), freeze$allowed_runs)
  if (length(entry) != 1L) stop("This exact acceptance process was not authorized.")
  entry <- entry[[1L]]
  if (identical(process$variant, "package")) {
    if (is.null(library_path)) stop("Supply the frozen candidate library.")
    package <- normalizePath(file.path(library_path, "AutoXplainR"), mustWork = TRUE)
    files <- list.files(package, recursive = TRUE, all.files = TRUE, no.. = TRUE)
    files <- files[!file.info(file.path(package, files))$isdir]
    stopifnot(identical(sort(files), sort(names(entry$package_files))),
      identical(sort(files), sort(names(process$installed_package_files))))
    for (filename in files) {
      tabular_same_hash(file.path(package, filename), entry$package_files[[filename]])
      stopifnot(identical(entry$package_files[[filename]], process$installed_package_files[[filename]]))
    }
  } else {
    stopifnot(identical(process$stage, "score-only"))
    for (key in c("process", "model", "summary")) {
      filename <- paste0(key, if (identical(key, "model")) ".rds" else ".json")
      tabular_same_hash(file.path(process$fit_source, filename), entry[[paste0("fit_", key, "_sha256")]])
      stopifnot(identical(entry[[paste0("fit_", key, "_sha256")]], process[[paste0("fit_source_", key, "_sha256")]]))
    }
  }
  list(process = process, freeze = freeze, entry = entry)
}

tabular_verify_partition <- function(destination, summary, include_targets = TRUE) {
  process <- jsonlite::read_json(file.path(destination, "process.json"))
  stopifnot(identical(process$process_status, "ok"), identical(summary$status, "ok"))
  for (key in c("case", "variant", "phase", "request", "stage")) stopifnot(identical(process[[key]], summary[[key]]))
  # Older development supervisors predate summary hashing. Acceptance has no such exception.
  if (identical(summary$phase, "acceptance") || !is.null(process$summary_sha256)) {
    tabular_same_hash(file.path(destination, "summary.json"), process$summary_sha256)
  }
  tabular_same_hash(file.path(destination, "model.rds"), summary$saved_model_sha256)
  tabular_same_hash(file.path(destination, "predictions.rds"), summary$predictions_sha256)
  cache <- tabular_cache()
  tabular_same_hash(file.path(cache, "partitions.json"), process$partitions_sha256)
  plan <- jsonlite::read_json(file.path(cache, "partitions.json"))
  declared <- plan$cases[[paste(summary$case, summary$phase, sep = "/")]]
  stopifnot(!is.null(declared), identical(summary$task, declared$task), identical(summary$partition_files, declared$files))
  tabular_scalar(summary$training_rows, "declared training rows", declared$n_training, 1)
  tabular_scalar(summary$evaluation_rows, "declared evaluation rows", declared$n_evaluation, 1)
  tabular_scalar(summary$predictors, "declared predictor count", declared$predictors, 1)
  partition <- file.path(cache, "cases", summary$case, summary$phase)
  files <- if (include_targets) c("training.rds", "evaluation-features.rds", "evaluation-targets.rds") else "evaluation-features.rds"
  for (filename in files) tabular_same_hash(file.path(partition, filename), declared$files[[filename]]$sha256)
  list(path = partition, declared = declared, process = process)
}

tabular_verify_prediction_ids <- function(saved, predictions, variant) {
  expected <- if (identical(variant, "package")) names(saved$models) else saved$family
  tabular_ids(expected, label = "retained model IDs")
  tabular_ids(names(predictions), expected, "saved prediction IDs")
  if (identical(variant, "package")) {
    tabular_ids(saved$leaderboard$model_id, expected, "leaderboard IDs")
    tabular_ids(names(saved$evaluation$metrics), expected, "evaluation metric IDs")
  } else stopifnot(identical(expected, variant))
  expected
}

# Independent definitions extend the benchmark's primary/secondary arithmetic to
# every metric the public result reports, without calling package metric helpers.
tabular_reported_metrics <- function(y, prediction, task) {
  raw <- tabular_metrics(y, prediction, task)
  if (identical(task, "regression")) return(unlist(raw))
  p <- if (is.null(dim(prediction))) cbind(1 - prediction, prediction) else as.matrix(prediction)[, levels(y), drop = FALSE]
  chosen <- if (identical(task, "binary")) 1L + as.integer(p[, 2L] >= .5) else max.col(p, ties.method = "first")
  recall <- vapply(seq_along(levels(y)), function(k) mean(chosen[as.integer(y) == k] == k), numeric(1))
  confidence <- as.numeric(if (identical(task, "binary")) p[, 2L] else p[cbind(seq_along(y), chosen)])
  event <- if (identical(task, "binary")) as.integer(y) == 2L else as.integer(y) == chosen
  groups <- ceiling(rank(confidence, ties.method = "average") / length(y) * min(5L, max(1L, floor(length(y) / 10L))))
  calibration <- sum(vapply(split(seq_along(y), groups), function(rows) {
    length(rows) * abs(mean(confidence[rows]) - mean(event[rows]))
  }, numeric(1))) / length(y)
  out <- c(log_loss = raw$log_loss, brier_score = raw$brier, accuracy = raw$accuracy,
    calibration_error = calibration)
  if (identical(task, "binary")) {
    # Public binary metrics clip positive probability before taking log1p of
    # its complement. Benchmark losses independently floor the true-class
    # probability; these can differ at exactly one due to floating precision.
    clipped <- pmin(1 - 1e-15, pmax(1e-15, p[, 2L]))
    event <- as.integer(y) == 2L
    out[["log_loss"]] <- -mean(ifelse(event, log(clipped), log1p(-clipped)))
    out[["brier_score"]] <- mean((clipped - as.numeric(event))^2)
    c(out, balanced_accuracy = mean(recall), roc_auc = raw$auc)
  } else c(out, macro_recall = mean(recall))
}

tabular_metric_difference <- function(actual, expected, label) {
  tabular_scalar(actual, label, minimum = -Inf)
  tabular_scalar(expected, paste(label, "reference"), minimum = -Inf)
  difference <- abs(actual - expected)
  if (difference > 1e-12) stop("Independent metric disagreement: ", label, ".")
  difference
}

tabular_compare_metrics <- function(reported, independent, label) {
  tabular_ids(names(reported), names(independent), paste(label, "metric names"))
  lapply(names(independent), function(metric) {
    observed <- reported[[metric]]
    expected <- independent[[metric]]
    if (is.list(expected)) {
      tabular_ids(names(observed), names(expected), paste(label, metric))
      return(setNames(lapply(names(expected), function(k) tabular_metric_difference(observed[[k]], expected[[k]], paste(label, metric, k))), names(expected)))
    }
    tabular_metric_difference(observed, expected, paste(label, metric))
  }) |> setNames(names(independent))
}

tabular_verify_package <- function(result, predictions, summary, training, y, require_forest = TRUE) {
  ids <- tabular_verify_prediction_ids(result, predictions, "package")
  tabular_ids(names(summary$metrics), ids, "summary metric IDs")
  stopifnot(identical(summary$primary, result$provenance$primary_model_id),
    identical(summary$primary, result$evaluation$primary_model_id), summary$primary %in% ids,
    identical(summary$final_configuration, result$tuning$final_configuration),
    identical(result$task, summary$task))
  tabular_scalar(nrow(training$data), "loaded training rows", summary$training_rows, 1)
  tabular_scalar(length(y), "loaded evaluation rows", summary$evaluation_rows, 1)
  tabular_scalar(result$evaluation$evaluated_rows, "reported evaluation rows", length(y), 1)
  rows <- scores <- differences <- list()
  forest_ids <- character()
  for (id in ids) {
    independent <- tabular_metrics(y, predictions[[id]], summary$task)
    reported <- tabular_reported_metrics(y, predictions[[id]], summary$task)
    differences[[id]] <- list(summary = tabular_compare_metrics(summary$metrics[[id]], independent, paste(id, "summary")),
      evaluation = tabular_compare_metrics(as.list(result$evaluation$metrics[[id]]), as.list(reported), paste(id, "evaluation")),
      benchmark_minus_reported_primary = tabular_primary(independent, summary$task) - reported[[if (summary$task == "regression") "rmse" else "log_loss"]])
    board <- result$leaderboard[result$leaderboard$model_id == id, , drop = FALSE]
    summary_board <- Filter(function(row) identical(row$model_id, id), summary$leaderboard)
    stopifnot(nrow(board) == 1L, length(summary_board) == 1L)
    for (metric in names(reported)) {
      tabular_metric_difference(board[[metric]], reported[[metric]], paste(id, "leaderboard", metric))
      tabular_metric_difference(summary_board[[1L]][[metric]], reported[[metric]], paste(id, "summary leaderboard", metric))
    }
    model <- result$models[[id]]
    if (inherits(model, "autoxplain_fitted_model")) {
      retained <- attr(model, "autoxplain_tuning_fit")
      stopifnot(identical(retained$scope, "full_training_refit"))
      if (identical(id, summary$primary)) stopifnot(identical(retained$configuration_id, summary$final_configuration))
      attempts <- result$tuning$refit$attempts
      attempt <- attempts[attempts$model_id == id & attempts$status == "ok", , drop = FALSE]
      stopifnot(nrow(attempt) == 1L, identical(attempt$configuration_id, retained$configuration_id))
      if (identical(model$backend, "ranger")) {
        count <- model$fit$num.samples
        tabular_scalar(model$fit$num.trees, paste(id, "native forest trees"), model$parameters$num.trees, 1)
        forest_ids <- c(forest_ids, id)
        scope <- "Native ranger num.samples."
      } else if (identical(model$backend, "xgboost")) {
        count <- model$fit_details$computation$rows
        tabular_scalar(xgboost::xgb.get.num.boosted.rounds(model$fit), paste(id, "native rounds"), model$parameters$nrounds, 1)
        scope <- "Retained input-encoding row metadata; XGBoost stores no native training-row count."
      } else if (identical(model$backend, "glmnet")) {
        count <- model$fit$nobs
        scope <- "Native glmnet nobs."
      } else stop("Unsupported retained tuned backend in tabular acceptance: ", model$backend)
    } else if (inherits(model, "multinom")) {
      count <- nrow(model$fitted.values)
      scope <- "Native multinom fitted-value rows."
    } else if (inherits(model, "lm")) {
      count <- stats::nobs(model)
      scope <- "Native linear/generalized-linear nobs."
    } else stop("Unsupported retained model row evidence: ", id)
    tabular_scalar(count, paste(id, "full training rows"), nrow(training$data), 1)
    rows[[id]] <- list(training_rows = count, scope = scope)
    scores[[id]] <- independent
  }
  if (require_forest && !length(forest_ids)) stop("A complete retained native forest is mandatory.")
  list(ids = ids, forest_ids = forest_ids, metrics = scores, differences = differences, final_training_rows = rows,
    metric_scope = paste("Benchmark metrics follow the original independent true-class probability floor.",
      "Reported binary loss clips the positive probability then uses log1p for its complement; endpoint floating-point differences are recorded separately."))
}


tabular_verify_cv <- function(result, summary, training) {
  oof <- result$tuning$out_of_fold_predictions
  candidates <- result$tuning$candidates
  complete <- candidates[candidates$status == "ok", , drop = FALSE]
  tabular_ids(complete$configuration_id, label = "complete CV configuration IDs")
  cv_differences <- list()
  assignment <- result$tuning$fold_assignment
  stopifnot(identical(assignment$training_row, seq_len(nrow(training$data))))
  # Fold IDs are labels, not ordinal identities. The package can legitimately
  # renumber first-seen labels; verify the actual partition through a bijection.
  if (!identical(summary$request, "public-tabular")) {
    stopifnot(identical(as.character(assignment$fold_label), as.character(training$folds)))
    partition_table <- table(assignment$fold, training$folds)
    stopifnot(all(rowSums(partition_table > 0) == 1L),
      all(colSums(partition_table > 0) == 1L))
  } else {
    stopifnot(length(unique(assignment$fold)) == 5L, all(!is.na(assignment$fold)),
      isTRUE(summary$public_request$default_explanations_and_report),
      identical(summary$resolved_public_defaults$seed, 123L))
    tabular_ids(unlist(summary$public_request$explicit_arguments, use.names = FALSE),
      c("data", "target_column", "test_data", "portfolio", "evaluation_role", "report"), "public call argument names")
    tabular_scalar(summary$resolved_public_defaults$max_models, "public configuration budget", 18, 1)
    tabular_scalar(result$tuning$control$max_models, "retained public configuration budget", 18, 1)
    tabular_scalar(summary$resolved_public_defaults$nfolds, "reported public folds", 5, 1)
    tabular_scalar(summary$resolved_public_defaults$threads, "reported public native threads", minimum = 1)
    stopifnot(summary$resolved_public_defaults$threads <= 4L)
    tabular_scalar(summary$report$bytes, "reported public report bytes", minimum = 1)
  }
  for (configuration in complete$configuration_id) {
    rows <- oof[oof$configuration_id == configuration, , drop = FALSE]
    stopifnot(nrow(rows) == nrow(training$data),
      identical(sort(rows$training_row), seq_len(nrow(training$data))),
      identical(as.integer(rows$fold), as.integer(assignment$fold[rows$training_row])),
      identical(as.character(rows$truth), as.character(training$data$y[rows$training_row])))
    truth <- training$data$y[rows$training_row]
    prediction <- if (summary$task == "regression") rows$estimate else rows$probabilities
    independent <- tabular_metrics(truth, prediction, summary$task)
    score <- complete$cv_score[match(configuration, complete$configuration_id)]
    expected_score <- if (identical(summary$task, "binary")) {
      tabular_reported_metrics(truth, prediction, "binary")[["log_loss"]]
    } else tabular_primary(independent, summary$task)
    difference <- abs(expected_score - score)
    stopifnot(length(difference) == 1L, is.finite(difference), difference < 1e-12)
    cv_differences[[configuration]] <- difference
  }
  selected <- complete[complete$configuration_id == result$tuning$selected_configuration, , drop = FALSE]
  stopifnot(nrow(selected) == 1L, abs(selected$cv_score - min(complete$cv_score)) < 1e-12)
  list(actual_cv_folds = length(unique(assignment$fold)), fold_source = result$tuning$control$fold_source,
    complete_cv_configurations = nrow(complete), cv_score_differences = cv_differences)
}

# A replay record is useful only for this exact model, predictions, process and complete ID set.
tabular_verify_replay <- function(destination, ids, evaluation_rows) {
  replay <- jsonlite::read_json(file.path(destination, "cold-replay.json"))
  stopifnot(identical(replay$status, "ok"), isTRUE(replay$fresh_session))
  tabular_ids(unlist(replay$model_ids, use.names = FALSE), ids, "replayed model IDs")
  tabular_ids(names(replay$max_absolute_prediction_difference), ids, "replay differences")
  tabular_scalar(replay$evaluation_rows, "replayed evaluation rows", evaluation_rows, 1)
  tabular_scalar(replay$elapsed_seconds, "replay elapsed seconds")
  for (id in ids) {
    tabular_scalar(replay$max_absolute_prediction_difference[[id]], paste(id, "replay difference"))
    stopifnot(replay$max_absolute_prediction_difference[[id]] <= 1e-12)
  }
  for (item in c(model = "model.rds", predictions = "predictions.rds", process = "process.json")) {
    field <- switch(item, model.rds = "saved_model_sha256", predictions.rds = "predictions_sha256", process.json = "process_sha256")
    tabular_same_hash(file.path(destination, item), replay[[field]])
  }
  process <- jsonlite::read_json(file.path(destination, "process.json"))
  if (identical(process$phase, "acceptance")) {
    stopifnot(identical(replay$verification_source_sha256$replay, process$scripts[["replay.R"]]),
      identical(replay$verification_source_sha256$evidence_common, process$scripts[["evidence-common.R"]]),
      identical(replay$verification_source_sha256$common, process$scripts[["common.R"]]))
  }
  list(elapsed_seconds = replay$elapsed_seconds, record_sha256 = tabular_hash(file.path(destination, "cold-replay.json")),
    max_absolute_prediction_difference = replay$max_absolute_prediction_difference)
}

tabular_verify_native <- function(saved, predictions, summary, declared, y) {
  ids <- tabular_verify_prediction_ids(saved, predictions, summary$variant)
  tabular_ids(names(summary$metrics), ids, "native summary metric IDs")
  stopifnot(identical(saved$task, summary$task), identical(summary$primary, summary$variant))
  tabular_scalar(saved$threads, "saved native threads", summary$native_threads, 1)
  tabular_scalar(summary$native_training_rows, "native training-row metadata", declared$n_training, 1)
  independent <- lapply(predictions[ids], function(p) tabular_metrics(y, p, summary$task))
  differences <- setNames(lapply(ids, function(id) tabular_compare_metrics(summary$metrics[[id]], independent[[id]], paste(id, "native summary"))), ids)
  if (identical(summary$variant, "ranger")) {
    tabular_scalar(saved$model$num.samples, "native forest training rows", declared$n_training, 1)
    tabular_scalar(summary$native_verified_training_rows, "recorded native forest rows", declared$n_training, 1)
    tabular_scalar(saved$model$num.trees, "native forest trees", 500, 1)
    tabular_scalar(saved$model$forest$num.trees, "stored native forest trees", 500, 1)
    tabular_scalar(summary$native_verified_tree_count, "recorded native forest trees", 500, 1)
    scope <- "Native ranger num.samples and full 500-tree reference checked directly."
  } else {
    rounds <- xgboost::xgb.get.num.boosted.rounds(saved$model)
    tabular_scalar(rounds, "native boosting rounds", summary$selected_parameters$selected_rounds, 1)
    tabular_scalar(summary$native_verified_boosting_rounds, "recorded native boosting rounds", rounds, 1)
    scope <- "Native boosted rounds checked directly; training rows remain fit-construction metadata, not a native count."
  }
  list(ids = ids, metrics = independent, differences = differences, training_rows = declared$n_training, row_evidence_scope = scope)
}

tabular_quality_gates <- function(metrics, primary, forest_ids, references, task) {
  tabular_ids(names(metrics), label = "scored package IDs")
  tabular_ids(forest_ids, label = "scored retained forest IDs")
  stopifnot(length(primary) == 1L, primary %in% names(metrics), all(forest_ids %in% names(metrics)))
  forests <- Filter(function(item) identical(item$variant, "ranger"), references)
  if (!length(forests)) stop("No successful, complete, replayed native forest reference; forest readiness cannot pass.")
  primary_values <- vapply(references, function(item) tabular_primary(item$metrics, task), numeric(1))
  forest_values <- vapply(forests, function(item) tabular_primary(item$metrics, task), numeric(1))
  stopifnot(all(is.finite(primary_values)), all(is.finite(forest_values)))
  best <- min(primary_values)
  best_forest <- min(forest_values)
  primary_limit <- if (identical(task, "regression")) 1.05 * best else 1.10 * best + .002
  forest_limit <- if (identical(task, "regression")) 1.10 * best_forest else 1.15 * best_forest + .002
  primary_loss <- tabular_primary(metrics[[primary]], task)
  forest_loss <- setNames(vapply(forest_ids, function(id) tabular_primary(metrics[[id]], task), numeric(1)), forest_ids)
  stopifnot(is.finite(primary_loss), all(is.finite(forest_loss)))
  list(primary_model = primary, primary_loss = primary_loss, primary_limit = primary_limit,
    primary_pass = primary_loss <= primary_limit, forest_loss = as.list(forest_loss), forest_limit = forest_limit,
    forest_pass = all(forest_loss <= forest_limit))
}

tabular_verify_published_comparison <- function(case, freeze, freeze_path, y, candidate_metrics, native, task) {
  entries <- Filter(function(entry) identical(entry$case, case) && identical(entry$variant, "package") &&
    identical(entry$role, "published-baseline"), freeze$allowed_runs)
  if (!length(entries)) return(list(status = "no_declared_completed_full_comparison",
    scope = "Development-only published-release results and timeouts cannot supply a full acceptance comparison."))
  if (length(entries) != 1L) stop("The published comparison must have one unambiguous frozen run.")
  entry <- entries[[1L]]
  stopifnot(identical(entry$request, "paired"), identical(entry$stage, "fit-and-score"), identical(entry$threads, 1L),
    identical(entry$published_archive_sha256, "bc6ad22bee49a9a3fee2ba7a092d975372815e07629295b3a012871782ac2532"))
  path <- file.path(tabular_cache(), "runs", entry$cohort, case, "package")
  if (!file.exists(file.path(path, "process.json"))) stop("The declared full published comparison has not run.")
  process <- jsonlite::read_json(file.path(path, "process.json"))
  if (!is.character(entry$library) || length(entry$library) != 1L) stop("The published comparison must declare its verified private library.")
  if (process$process_status %in% c("failed", "timeout")) {
    verified_baseline <- tabular_verify_freeze(path, freeze_path, entry$library, require_success = FALSE)
    stopifnot(identical(verified_baseline$entry, entry))
    failed_resources <- tabular_verify_resources(path, require_success = FALSE)
    return(list(status = "full_comparison_failed", resources = failed_resources,
      process_status = process$process_status, process_sha256 = tabular_hash(file.path(path, "process.json")),
      scope = "An unsuccessful full comparison supplies no loss; preserve its operational failure."))
  }
  verified_baseline <- tabular_verify_freeze(path, freeze_path, entry$library)
  stopifnot(identical(verified_baseline$entry, entry))
  summary <- jsonlite::read_json(file.path(path, "summary.json"))
  stopifnot(identical(summary$package_version, "0.7.0"), identical(summary$request, "paired"))
  partition <- tabular_verify_partition(path, summary)
  resources <- tabular_verify_resources(path)
  training <- readRDS(file.path(partition$path, "training.rds"))
  model <- readRDS(file.path(path, "model.rds"))
  predictions <- readRDS(file.path(path, "predictions.rds"))
  checked <- tabular_verify_package(model, predictions, summary, training, y)
  cv <- tabular_verify_cv(model, summary, training)
  stopifnot(is.null(model$explanations), model$tuning$control$max_models == 10L, model$provenance$seed == 80711L)
  replay <- tabular_verify_replay(path, checked$ids, length(y))
  old <- tabular_primary(checked$metrics[[summary$primary]], task)
  current <- tabular_primary(candidate_metrics, task)
  best <- min(vapply(native, function(item) tabular_primary(item$metrics, task), numeric(1)))
  list(status = "completed_same_acceptance_partition", old_primary_model = summary$primary,
    old_primary_loss = old, candidate_primary_loss = current, non_regression_limit = 1.02 * old,
    non_regression_pass = current <= 1.02 * old,
    excess_gap_applicable = old > 1.10 * best, half_excess_limit = best + .5 * (old - best),
    half_excess_closed = current <= best + .5 * (old - best),
    process_sha256 = tabular_hash(file.path(path, "process.json")), resources = resources, replay = replay,
    scope = paste("Same full training/evaluation partitions; unequal work:",
      "published0.7 uses ten configurations, frozen training folds, one native thread and no explanations/report;",
      "the public candidate uses eighteen configurations, default folds/seed and default explanations/report.",
      "At least one applicable challenging case must close half the old excess across the suite; this per-case record does not assert that suite gate."))
}
