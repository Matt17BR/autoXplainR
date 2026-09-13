# Keep screening, complete validation and stopping calibration as separate
# records. Only complete validation scores can select a final configuration.
prepare_search_fold <- function(raw_data, target, task, assignment, fold, control,
                                enable_preprocessing, preprocessing_config, seed) {
  prepared <- prepare_tuning_fold(
    raw_data = raw_data, target = target, task = task, fold_assignment = assignment$id, fold = fold,
    enable_preprocessing = enable_preprocessing, preprocessing_config = preprocessing_config,
    fold_label = assignment$labels[[fold]]
  )
  if (isTRUE(control$early_stopping)) {
    rows <- which(assignment$id != fold)
    groups <- control$groups %||% if (identical(control$fold_source, "supplied_vfold")) assignment$id else NULL
    prepared$boosting_calibration <- tryCatch(
      prepare_boosting_calibration(
        raw_data[rows, , drop = FALSE], target, task,
        enable_preprocessing, preprocessing_config,
        stable_configuration_seed(seed, "boosting", list(stage = "calibration", fold = as.integer(fold))),
        groups = if (is.null(groups)) NULL else groups[rows]
      ),
      error = function(condition) list(status = "failed", reason = conditionMessage(condition))
    )
  }
  prepared
}

run_adaptive_screening <- function(plan, raw_data, target, task, assignment, control,
                                   enable_preprocessing, preprocessing_config, seed, started, progress = FALSE) {
  partition <- adaptive_screen_partition(
    raw_data, target, task, assignment, seed, control$screening_rows, groups = control$groups
  )
  search_progress(
    progress, "Preparing the common screen: ", search_progress_count(length(partition$training_row)),
    " training rows; ", search_progress_count(length(partition$validation_row)), " assessment rows."
  )
  rows <- c(partition$training_row, partition$validation_row)
  screen_data <- raw_data[rows, , drop = FALSE]
  screen_assignment <- list(
    id = c(rep(2L, length(partition$training_row)), rep(1L, length(partition$validation_row))),
    labels = c("screening assessment", "screening training")
  )
  screen_control <- control
  screen_control$early_stopping <- isTRUE(control$early_stopping) && "boosting" %in% plan$family
  # Supplied V-fold labels, if group identities are unavailable, remain units
  # inside the screening-training side as well as the complete CV stage.
  groups <- control$groups %||% if (identical(control$fold_source, "supplied_vfold")) assignment$id else NULL
  screen_control$groups <- if (is.null(groups)) NULL else groups[rows]
  prepared <- prepare_search_fold(
    screen_data, target, task, screen_assignment, 1L, screen_control,
    enable_preprocessing, preprocessing_config, seed
  )
  weights <- partition$validation_sampling_weight %||% rep(1, length(partition$validation_row))
  prepared$score_weights <- weights[match(rownames(prepared$validation), partition$validation_source_row)]
  prepared$fit_scope <- "screening"
  screen_plan <- plan
  screen_plan$parameters <- I(lapply(seq_len(nrow(plan)), function(index) {
    adaptive_screen_parameters(
      plan$parameters[[index]], plan$family[[index]],
      training_rows = nrow(prepared$training), planned_rows = nrow(raw_data),
      planned_predictors = ncol(raw_data) - 1L
    )
  }))
  records <- vector("list", nrow(plan))
  for (index in seq_len(nrow(plan))) {
    # Finish one common screening round across families before considering a
    # scheduling stop. Unseen candidates remain explicit, not failed fits.
    if (index > length(unique(plan$family)) && search_deadline_reached(started, control$time_limit)) break
    search_progress_fit(
      progress, "Screening", screen_plan[index, , drop = FALSE],
      nrow(prepared$training), nrow(prepared$validation), position = index, total = nrow(plan)
    )
    scored <- score_tuning_configuration(
      screen_plan[index, , drop = FALSE], prepared, target, task, 1L,
      metric = control$metric, retain_oof = FALSE, failure_policy = control$failure_policy, progress = progress
    )
    records[[index]] <- scored$score
  }
  scores <- do.call(rbind, records)
  scores$full_parameters <- I(plan$parameters[match(scores$configuration_id, plan$configuration_id)])
  promotion <- adaptive_promote(plan, scores, control$finalists_per_family, control$metric)
  search_progress(
    progress, "Screening complete: ", nrow(scores), " settings assessed; ",
    sum(promotion$promoted), " advanced to cross-validation."
  )
  plan$search_status <- ifelse(promotion$promoted, "scheduled", "screened_out")
  attempted <- match(plan$configuration_id, scores$configuration_id)
  plan$screening_score <- scores$score[attempted]
  plan$search_status[is.na(attempted)] <- "not_screened_time_limit"
  failed <- !is.na(attempted) & !is.finite(scores$score[attempted])
  plan$search_status[failed] <- "screening_failed"
  result <- list(
    plan = plan,
    evidence = list(
      method = "Common-sample screening followed by complete cross-validation",
      metric = control$metric, direction = selection_metric_direction(control$metric),
      partition = partition, scores = scores, promotion = promotion,
      fit_scope = paste(
        "Screening scores use smaller training samples and reduced tree/round budgets.",
        "They are not comparable with full CV scores or final evaluation scores.",
        "Screening selected these settings using outer-training outcomes, including rows later assessed by CV.",
        "The resulting CV scores are conditional selection evidence, not an independent accuracy estimate."
      )
    )
  )
  if (!any(promotion$promoted)) {
    causes <- unique(scores$error[!is.na(scores$error) & nzchar(scores$error)])
    detail <- if (length(causes)) paste(causes, collapse = "\n") else paste0(
      "None produced a finite ", control$metric, " score."
    )
    stop(errorCondition(
      paste0("Screening failed for all ", nrow(scores), " attempted settings.\n", detail),
      class = "autoxplain_screening_error", screening = result$evidence, plan = plan
    ))
  }
  result
}

search_deadline_reached <- function(started, limit) {
  !is.null(limit) && proc.time()[["elapsed"]] - started >= limit
}

execute_complete_validation <- function(plan, raw_data, target, task, assignment, control,
                                        enable_preprocessing, preprocessing_config, seed, started, progress = FALSE) {
  fold_count <- assignment$folds
  records <- new.env(parent = emptyenv())
  records$fold_preprocessing <- records$fold_omissions <- vector("list", fold_count)
  records$fold_rows <- vector("list", nrow(plan) * fold_count)
  records$prediction_rows <- if (control$retain_oof) vector("list", length(records$fold_rows)) else NULL
  records$fold_counts <- integer(fold_count)
  scheduled <- which(plan$search_status == "scheduled")
  if (!is.null(control$time_limit) && "screening_score" %in% names(plan)) {
    scheduled <- scheduled[order(selection_metric_loss(plan$screening_score[scheduled], control$metric), scheduled)]
  }
  control$early_stopping <- isTRUE(control$early_stopping) && "boosting" %in% plan$family[scheduled]
  if (!length(scheduled)) {
    stop("No configuration was scheduled for cross-validation.", call. = FALSE)
  }
  prepare <- function(fold) {
    prepared <- prepare_search_fold(
      raw_data, target, task, assignment, fold, control,
      enable_preprocessing, preprocessing_config, seed
    )
    records$fold_preprocessing[[fold]] <- list(
      fold = as.integer(fold), fold_label = assignment$labels[[fold]],
      predictors_received = setdiff(names(raw_data), target),
      predictors_retained = prepared$retained_features, predictors_removed = prepared$removed_features
    )
    records$fold_omissions[[fold]] <- prepared[c("omitted_validation_row", "omitted_source_row")]
    records$fold_counts[[fold]] <- nrow(prepared$validation)
    prepared
  }
  score <- function(index, fold, prepared) {
    row_index <- (index - 1L) * fold_count + fold
    search_progress_fit(
      progress, "CV setting", plan[index, , drop = FALSE], nrow(prepared$training), nrow(prepared$validation),
      position = match(index, scheduled), total = length(scheduled), fold = fold, folds = fold_count
    )
    scored <- score_tuning_configuration(
      plan[index, , drop = FALSE], prepared, target, task, fold,
      metric = control$metric, retain_oof = control$retain_oof,
      failure_policy = control$failure_policy, progress = progress
    )
    records$fold_rows[[row_index]] <- scored$score
    if (control$retain_oof) records$prediction_rows[[row_index]] <- scored$predictions
    is.finite(scored$score$score)
  }
  if (is.null(control$time_limit)) {
    # Fold-major execution prepares one fold at a time and shares its recipe
    # and calibration split across candidates, without retaining all folds.
    for (fold in seq_len(fold_count)) {
      prepared <- prepare(fold)
      for (index in scheduled) score(index, fold, prepared)
      rm(prepared)
    }
  } else {
    # A deadline requires complete candidate units. A fold-major early break
    # could spend the entire budget without a single selectable candidate.
    completed <- FALSE
    for (index in scheduled) {
      if (completed && search_deadline_reached(started, control$time_limit)) {
        plan$search_status[[index]] <- "not_validated_time_limit"
        next
      }
      valid <- TRUE
      for (fold in seq_len(fold_count)) {
        prepared <- prepare(fold)
        valid <- score(index, fold, prepared) && valid
        rm(prepared)
      }
      completed <- completed || valid
    }
  }
  scores <- do.call(rbind, records$fold_rows)
  predictions <- if (control$retain_oof) {
    populated <- records$prediction_rows[!vapply(records$prediction_rows, is.null, logical(1))]
    combine_tuning_predictions(populated, task)
  } else {
    NULL
  }
  list(
    plan = plan, fold_scores = scores, predictions = predictions,
    fold_preprocessing = records$fold_preprocessing, omissions = combine_tuning_omissions(records$fold_omissions),
    rows_evaluated = sum(records$fold_counts)
  )
}

record_boosting_refit_rounds <- function(plan, scores) {
  plan$round_selection <- I(rep(list(NULL), nrow(plan)))
  for (index in which(plan$family == "boosting")) {
    rows <- scores[scores$configuration_id == plan$configuration_id[[index]] & is.finite(scores$score), ]
    records <- lapply(rows$learned, function(x) x$round_selection)
    calibrated <- vapply(records, function(x) identical(x$status, "calibrated"), logical(1))
    # A skipped calibration kept the requested rounds. Include that effective
    # count instead of silently dropping hard-to-split training folds.
    if (!any(calibrated)) next
    rounds <- vapply(rows$effective_parameters, function(x) as.integer(x$nrounds), integer(1))
    plan$round_selection[[index]] <- list(
      status = "fold_aggregate", selected_rounds = as.integer(ceiling(stats::median(rounds))),
      maximum_rounds = plan$parameters[[index]]$nrounds,
      fold_rounds = stats::setNames(rounds, rows$fold), calibrated_folds = sum(calibrated),
      scope = paste(
        "The final fit uses every outer-training row at the rounded-up median of successful folds' round counts.",
        sum(calibrated), "of", length(calibrated), "folds calibrated rounds on separate inner training splits;",
        "any skipped fold kept its requested round cap.",
        "The final evaluation outcomes did not choose this value."
      )
    )
  }
  plan
}
