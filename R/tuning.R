#' Inspect automatic tuning evidence
#'
#' Returns the complete training-only resampling record created by
#' `autoxplain(..., model_set = "tuned")`. The outer evaluation rows are never
#' used to rank configurations.
#'
#' `out_of_fold_predictions` stores one row per outer-training case and
#' configuration. `training_row` is the stable position used for paired model
#' comparisons, `source_row` preserves the input row name, and `case_loss`
#' stores the per-row contribution to the configured tuning metric.
#' Classification probability vectors are stored compactly as the matrix-column
#' `probabilities`. `out_of_fold_predictions` is `NULL` when
#' `tuning_control(retain_oof = FALSE)` was used; `prediction_schema` remains as
#' an explicit `retained = FALSE` sentinel. If
#' fold-specific preprocessing removes rows (for example with
#' `missing_value_strategy = "drop_rows"`), `omitted_rows` records their identity
#' and fold, while `rows_evaluated` and candidate `evaluated_rows` count only
#' cases that actually received out-of-fold predictions.
#' Each row of `fold_scores` preserves the requested parameter list and also
#' records the fold-specific effective parameter list, its canonical key, and
#' the seed used for fitting. This makes data-dependent clamps such as `mtry`
#' or neighbor count visible and prevents two requests that become the same
#' fold fit from being compared only through different random seeds.
#'
#' `refit` records every full-training refit attempt. If the resampling-selected
#' configuration cannot be refitted, AutoXplainR tries the remaining valid
#' configurations in resampled-score order, records the first successful one in
#' `final_configuration`, and marks `fallback_used`. Failed alternative-family
#' refits are diagnosed but do not discard a usable primary model. Families for
#' which every configuration failed resampling remain visible in
#' `families_resampling_failed` and are excluded from paired prediction evidence.
#' Each refit attempt repeats the requested/effective parameter and seed record
#' for the complete outer-training data.
#'
#' @param result An `autoxplain_result` fitted with `model_set = "tuned"`.
#'
#' @return An object of class `autoxplain_tuning` containing the candidate table,
#'   fold-level scores, out-of-fold predictions, selected configuration,
#'   resampling boundary, and search settings.
#' @export
#'
#' @examples
#' tuned <- autoxplain(iris, "Sepal.Length", model_set = "tuned",
#'                     portfolio = "core", max_models = 4,
#'                     nfolds = 3, seed = 2026)
#' tuning_results(tuned)
tuning_results <- function(result) {
  if (!inherits(result, "autoxplain_result")) {
    stop("`result` must be returned by `autoxplain()`.", call. = FALSE)
  }
  if (!inherits(result$tuning, "autoxplain_tuning")) {
    stop(
      "No local tuning record is available. Use `model_set = \"tuned\"`.",
      call. = FALSE
    )
  }
  result$tuning
}

#' @export
print.autoxplain_tuning <- function(x, ...) {
  selected <- x$candidates[x$candidates$selected, , drop = FALSE]
  cat("<AutoXplainR training-only tuning>\n")
  cat("  search:     ", nrow(x$candidates), " configurations across ",
      length(unique(x$candidates$family)), " model families\n", sep = "")
  cat("  resampling: ", x$folds_used, " folds; ", x$metric,
      " minimized\n", sep = "")
  if (!is.null(x$rows_requested)) {
    if (isTRUE(x$control$retain_oof %||% TRUE)) {
      cat("  evidence:   ", x$rows_evaluated, "/", x$rows_requested,
          " outer-training rows predicted",
          if (x$rows_omitted) paste0("; ", x$rows_omitted, " omitted by preprocessing") else "",
          "\n", sep = "")
    } else {
      cat("  evidence:   row-level OOF predictions not retained\n")
    }
  }
  cat("  rule:       ", tuning_rule_label(x$selection_rule), "\n", sep = "")
  cat("  selected:   ", selected$model[[1L]], " (",
      selected$configuration_id[[1L]], ")\n", sep = "")
  if (length(x$families_resampling_failed)) {
    cat("  no valid CV: ", paste(x$families_resampling_failed, collapse = ", "),
        " (every configuration failed resampling)\n", sep = "")
  }
  if (!is.null(x$refit) && !is.na(x$final_configuration)) {
    cat("  final fit:  ", x$final_configuration,
        if (isTRUE(x$refit$fallback_used)) " (recorded fallback)" else "",
        "\n", sep = "")
    if (length(x$refit$families_refit_failed)) {
      cat("  omitted:    ", paste(x$refit$families_refit_failed, collapse = ", "),
          " (full-training refit failed)\n", sep = "")
    }
  }
  cat("  score:      ", format(round(selected$cv_score[[1L]], 5L), trim = TRUE),
      " +/- ", format(round(selected$cv_se[[1L]], 5L), trim = TRUE), " SE\n", sep = "")
  cat("  boundary:   ", x$scope_note, "\n", sep = "")
  cat("  proxy:      family-specific flexibility; values are not comparable across families\n")
  display <- x$candidates[c(
    "configuration_id", "family", "backend", "model", "hyperparameters", "cv_score", "cv_se",
    "complexity_proxy", "selected", "status"
  )]
  print.data.frame(display, row.names = FALSE, ...)
  invisible(x)
}

tune_supervised_candidates <- function(raw_data,
                                       target,
                                       task,
                                       enable_preprocessing,
                                       preprocessing_config,
                                       max_models,
                                       nfolds,
                                       selection_rule,
                                       control = NULL,
                                       seed = 123L,
                                       learners = c("linear", "tree", "neural")) {
  if (is.null(control)) {
    control <- default_resolved_tuning_control(task, max_models, nfolds)
  }
  if (!inherits(control, "autoxplain_resolved_tuning_control")) {
    stop("Internal tuning control was not resolved before fitting.", call. = FALSE)
  }
  plan <- local_tuning_plan(
    max_models = max_models,
    n = nrow(raw_data),
    p = ncol(raw_data) - 1L,
    task = task,
    n_classes = if (task == "regression") 1L else length(levels(raw_data[[target]])),
    learners = learners,
    seed = seed,
    custom_grids = control$grids,
    family_budgets = control$family_budgets
  )
  fold_assignment <- tuning_fold_assignment(
    raw_data[[target]],
    task = task,
    requested = nfolds,
    supplied_ids = control$fold_ids,
    supplied_labels = control$fold_labels
  )
  folds <- lapply(seq_len(fold_assignment$folds), function(fold) {
    prepare_tuning_fold(
      raw_data = raw_data,
      target = target,
      task = task,
      fold_assignment = fold_assignment$id,
      fold = fold,
      fold_label = fold_assignment$labels[[fold]],
      enable_preprocessing = enable_preprocessing,
      preprocessing_config = preprocessing_config
    )
  })
  fold_preprocessing <- lapply(seq_along(folds), function(fold) {
    list(
      fold = as.integer(fold),
      fold_label = fold_assignment$labels[[fold]],
      predictors_received = setdiff(names(raw_data), target),
      predictors_retained = folds[[fold]]$retained_features,
      predictors_removed = folds[[fold]]$removed_features
    )
  })
  omitted_rows <- combine_tuning_omissions(folds)
  rows_evaluated <- sum(vapply(folds, function(fold) nrow(fold$validation), integer(1)))

  fold_rows <- list()
  prediction_rows <- list()
  row_index <- 0L
  for (configuration in seq_len(nrow(plan))) {
    for (fold in seq_along(folds)) {
      row_index <- row_index + 1L
      scored <- score_tuning_configuration(
        plan[configuration, , drop = FALSE], folds[[fold]], target, task, fold,
        metric = control$metric,
        retain_oof = control$retain_oof,
        failure_policy = control$failure_policy
      )
      fold_rows[[row_index]] <- scored$score
      prediction_rows[[row_index]] <- scored$predictions
    }
  }
  fold_scores <- do.call(rbind, fold_rows)
  out_of_fold_predictions <- if (control$retain_oof) {
    combine_tuning_predictions(prediction_rows, task)
  } else {
    NULL
  }
  candidates <- summarize_tuning_candidates(
    plan, fold_scores, fold_assignment$folds, task, metric = control$metric
  )
  valid <- candidates$status == "ok" & is.finite(candidates$cv_score)
  if (!any(valid)) {
    detail <- unique(fold_scores$error[nzchar(fold_scores$error)])
    stop(
      "Automatic tuning could not fit a complete configuration across every fold",
      if (length(detail)) paste0(": ", detail[[1L]]) else ".",
      call. = FALSE
    )
  }
  if (control$retain_oof) {
    out_of_fold_predictions <- out_of_fold_predictions[
      out_of_fold_predictions$configuration_id %in% candidates$configuration_id[valid],
      , drop = FALSE
    ]
    rownames(out_of_fold_predictions) <- NULL
    expected_prediction_rows <- rows_evaluated * sum(valid)
    if (nrow(out_of_fold_predictions) != expected_prediction_rows ||
          anyDuplicated(out_of_fold_predictions[c("configuration_id", "training_row")])) {
      stop(
        "Internal tuning evidence is incomplete or has duplicate row identities.",
        call. = FALSE
      )
    }
  }
  families_resampling_failed <- setdiff(
    unique(plan$family),
    unique(candidates$family[valid])
  )
  best_index <- which(valid)[which.min(candidates$cv_score[valid])][[1L]]
  eligible <- if (selection_rule == "one_se") {
    valid & candidates$cv_score <= candidates$cv_score[[best_index]] +
      candidates$cv_se[[best_index]]
  } else {
    seq_len(nrow(candidates)) == best_index
  }
  selected_index <- if (selection_rule == "one_se") {
    select_one_se_candidate(candidates, eligible)
  } else {
    best_index
  }
  candidates$selected <- seq_len(nrow(candidates)) == selected_index
  selected_id <- candidates$configuration_id[[selected_index]]
  plan$selected <- plan$configuration_id == selected_id
  candidates <- candidates[order(
    !candidates$selected,
    candidates$status != "ok",
    candidates$cv_score,
    candidates$complexity_proxy,
    candidates$configuration_id
  ), , drop = FALSE]
  rownames(candidates) <- NULL

  structure(
    list(
      schema_version = 4L,
      method = if (identical(control$fold_source, "supplied_vfold")) {
        "User-supplied V-fold resampling with fold-specific preprocessing"
      } else {
        "K-fold resampling with fold-specific preprocessing"
      },
      metric = control$metric,
      direction = "minimize",
      selection_rule = selection_rule,
      folds_requested = if (identical(control$fold_source, "supplied_vfold")) {
        length(control$fold_labels)
      } else {
        nfolds
      },
      folds_used = fold_assignment$folds,
      fold_source = control$fold_source,
      fold_assignment = data.frame(
        training_row = seq_len(nrow(raw_data)),
        source_row = rownames(raw_data),
        fold = fold_assignment$id,
        fold_label = fold_assignment$labels[fold_assignment$id],
        stringsAsFactors = FALSE
      ),
      configurations_requested = max_models,
      configurations_evaluated = nrow(plan),
      rows_requested = nrow(raw_data),
      rows_evaluated = rows_evaluated,
      rows_omitted = nrow(omitted_rows),
      omitted_rows = omitted_rows,
      predictors_received = setdiff(names(raw_data), target),
      fold_preprocessing = fold_preprocessing,
      learners = unique(plan$family),
      learner_manifest = tuning_learner_manifest(unique(plan$family), task),
      families_resampling_failed = families_resampling_failed,
      candidates = candidates,
      fold_scores = fold_scores,
      out_of_fold_predictions = out_of_fold_predictions,
      prediction_schema = if (control$retain_oof) {
        tuning_prediction_schema(
          raw_data[[target]], task, nrow(raw_data), rows_evaluated, control$metric
        )
      } else {
        tuning_prediction_schema_not_retained(
          raw_data[[target]], task, nrow(raw_data), rows_evaluated, control$metric
        )
      },
      control = tuning_control_provenance(control),
      plan = plan,
      selected_configuration = selected_id,
      final_configuration = NA_character_,
      refit = NULL,
      scope_note = paste(
        "Configuration ranking used only resamples of the outer training rows;",
        "the held-out evaluation rows were untouched until final scoring.",
        if (identical(control$fold_source, "supplied_vfold")) {
          paste(
            "Supplied fold IDs defined ordinary V-fold resampling: each fold was validated",
            "after training on all other folds; this was not rolling-origin resampling."
          )
        } else {
          "Folds were assigned automatically within the outer training data."
        },
        if (nrow(omitted_rows)) {
          paste(
            nrow(omitted_rows), "outer-training rows were omitted consistently",
            "by fold-specific preprocessing and are listed in `omitted_rows`."
          )
        } else {
          "Every outer-training row received an out-of-fold prediction."
        }
      )
    ),
    class = "autoxplain_tuning"
  )
}

tuning_learner_manifest <- function(families, task) {
  registry <- autoxplain_learner_registry()
  rows <- lapply(families, function(family) {
    item <- registry[[family]]
    data.frame(
      family = family,
      backend = item$backend,
      model = learner_model_label(item, task),
      package = item$package %||% "AutoXplainR core",
      package_version = if (is.null(item$package)) {
        package_version_or_development()
      } else {
        as.character(utils::packageVersion(item$package))
      },
      nonlinearity = item$nonlinearity,
      interactions = item$interactions,
      one_se_priority = item$simplicity_rank,
      complexity_definition = item$complexity_label,
      strengths = item$strengths,
      cautions = item$cautions,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

local_tuning_plan <- function(max_models,
                              n,
                              p,
                              task,
                              n_classes,
                              learners = c("linear", "tree", "neural"),
                              seed = 123L,
                              custom_grids = NULL,
                              family_budgets = NULL) {
  max_models <- assert_count(max_models, "max_models")
  seed <- assert_count(seed, "seed", minimum = 0L)
  if (!is.character(learners) || !length(learners) || anyNA(learners) ||
        any(!nzchar(learners)) || anyDuplicated(learners)) {
    stop("`learners` must be a unique, non-empty character vector.", call. = FALSE)
  }
  registry <- autoxplain_learner_registry()
  unknown <- setdiff(learners, names(registry))
  if (length(unknown)) {
    stop(
      "Unknown learner families: ", paste(unknown, collapse = ", "),
      ". Use `learner_catalog()` to inspect supported families.",
      call. = FALSE
    )
  }
  unsupported <- learners[!vapply(learners, function(family) {
    task %in% registry[[family]]$tasks
  }, logical(1))]
  if (length(unsupported)) {
    stop(
      "Learner families do not support the `", task, "` task: ",
      paste(unsupported, collapse = ", "), ".",
      call. = FALSE
    )
  }
  unavailable <- learners[!vapply(learners, function(family) {
    learner_is_available(registry[[family]])
  }, logical(1))]
  if (length(unavailable)) {
    packages <- unique(vapply(unavailable, function(family) {
      registry[[family]]$package %||% family
    }, character(1)))
    stop(
      "Learner dependencies are not installed: ", paste(packages, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (max_models < length(learners)) {
    stop(
      "`max_models` must be at least the number of learner families (",
      length(learners), ").",
      call. = FALSE
    )
  }
  grids <- lapply(learners, function(family) {
    registry[[family]]$grid(n = n, p = p, task = task, n_classes = n_classes)
  })
  names(grids) <- learners
  if (!is.null(custom_grids)) {
    custom_grids <- normalize_tuning_grids(custom_grids)
    extra <- setdiff(names(custom_grids), learners)
    if (length(extra)) {
      stop(
        "Custom grids do not belong to the requested learners: ",
        paste(extra, collapse = ", "), ".",
        call. = FALSE
      )
    }
    grids[names(custom_grids)] <- custom_grids
  }
  # Adapter contracts also guard package-maintained defaults against drift.
  grids <- lapply(learners, function(family) {
    normalize_family_grid(grids[[family]], family, allow_duplicates = TRUE)
  })
  names(grids) <- learners
  grids <- canonicalize_task_specific_grids(
    grids,
    task = task,
    custom_families = names(custom_grids %||% list())
  )
  if (!is.null(family_budgets)) {
    family_budgets <- normalize_family_budgets(family_budgets)
    if (!setequal(names(family_budgets), learners)) {
      stop("`family_budgets` names must exactly match `learners`.", call. = FALSE)
    }
    family_budgets <- family_budgets[learners]
    if (sum(family_budgets) != max_models) {
      stop("`max_models` must equal the sum of exact `family_budgets`.", call. = FALSE)
    }
    exhausted <- learners[family_budgets > lengths(grids)]
    if (length(exhausted)) {
      details <- vapply(exhausted, function(family) {
        paste0(
          family, " requested ", family_budgets[[family]],
          " but its grid contains ", length(grids[[family]])
        )
      }, character(1))
      stop("Family budget exceeds available configurations: ", paste(details, collapse = "; "),
           ".", call. = FALSE)
    }
  }
  selected <- list()
  depth <- 1L
  while (length(selected) < max_models) {
    added <- FALSE
    for (family in learners) {
      if (length(selected) >= max_models) break
      allowed <- is.null(family_budgets) || depth <= family_budgets[[family]]
      if (allowed && length(grids[[family]]) >= depth) {
        selected[[length(selected) + 1L]] <- list(
          family = family,
          parameters = grids[[family]][[depth]]
        )
        added <- TRUE
      }
    }
    if (!added) break
    depth <- depth + 1L
  }
  family_counts <- setNames(integer(length(learners)), learners)
  rows <- lapply(seq_along(selected), function(index) {
    family <- selected[[index]]$family
    parameters <- selected[[index]]$parameters
    definition <- registry[[family]]
    family_counts[[family]] <<- family_counts[[family]] + 1L
    data.frame(
      configuration_id = paste0(family, "_", sprintf("%02d", family_counts[[family]])),
      family = family,
      backend = definition$backend,
      model = learner_model_label(definition, task),
      hyperparameters = definition$describe(parameters),
      parameters = I(list(parameters)),
      simplicity_rank = definition$simplicity_rank,
      complexity_definition = definition$complexity_label,
      complexity_proxy = max(1, definition$complexity(
        parameters, n = n, p = p, task = task, n_classes = n_classes
      )),
      search_seed = seed,
      seed = stable_configuration_seed(seed, family, parameters),
      selected = FALSE,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

canonicalize_task_specific_grids <- function(grids, task, custom_families = character()) {
  if (!"kernel" %in% names(grids) || identical(task, "regression")) return(grids)
  canonical_epsilon <- 0.1
  requested <- vapply(grids$kernel, `[[`, numeric(1), "epsilon")
  if ("kernel" %in% custom_families && any(requested != canonical_epsilon)) {
    stop(
      "Classification kernel grids must use `epsilon = 0.1`. The e1071 ",
      "classification adapter ignores epsilon, so AutoXplainR keeps it constant ",
      "to prevent duplicate effective configurations.",
      call. = FALSE
    )
  }
  grids$kernel <- lapply(grids$kernel, function(parameters) {
    parameters$epsilon <- canonical_epsilon
    parameters
  })
  effective <- vapply(grids$kernel, function(parameters) {
    canonical_tuning_parameter_key(parameters[c("cost", "gamma_multiplier")])
  }, character(1))
  if (anyDuplicated(effective)) {
    stop(
      "Classification kernel grid contains duplicate effective cost/gamma settings.",
      call. = FALSE
    )
  }
  grids
}

stable_configuration_seed <- function(seed, family, parameters) {
  signature <- canonical_tuning_parameter_key(parameters)
  key <- utf8ToInt(paste(seed, family, signature, sep = "|"))
  hash <- 0
  for (value in key) {
    hash <- (hash * 131 + value) %% .Machine$integer.max
  }
  as.integer(hash)
}

canonical_tuning_parameter_key <- function(parameters) {
  if (!length(parameters)) return("<default>")
  paste(vapply(names(parameters), function(name) {
    value <- parameters[[name]]
    type <- if (is.integer(value)) {
      "integer"
    } else if (is.numeric(value)) {
      "double"
    } else if (is.logical(value)) {
      "logical"
    } else {
      "character"
    }
    encoded <- canonical_tuning_values(value)
    if (length(value) == 1L && is.null(names(value))) {
      paste0(name, "=", type, ":", encoded)
    } else {
      value_names <- names(value) %||% rep("", length(value))
      pieces <- vapply(seq_along(encoded), function(index) {
        element_name <- canonical_tuning_escape(value_names[[index]])
        paste0(
          nchar(element_name, type = "bytes"), ":", element_name, "=",
          nchar(encoded[[index]], type = "bytes"), ":", encoded[[index]]
        )
      }, character(1))
      paste0(
        name, "=", type, "[", length(value), "]:",
        paste(pieces, collapse = ",")
      )
    }
  }, character(1)), collapse = "|")
}

canonical_tuning_values <- function(value) {
  encoded <- if (is.numeric(value)) {
    format(value, digits = 17L, scientific = TRUE, trim = TRUE)
  } else if (is.logical(value)) {
    ifelse(value, "TRUE", "FALSE")
  } else {
    enc2utf8(as.character(value))
  }
  vapply(encoded, canonical_tuning_escape, character(1), USE.NAMES = FALSE)
}

canonical_tuning_escape <- function(value) {
  value <- enc2utf8(as.character(value))
  value <- gsub("\\\\", "\\\\\\\\", value)
  value <- gsub("|", "\\\\|", value, fixed = TRUE)
  gsub("=", "\\\\=", value, fixed = TRUE)
}

select_one_se_candidate <- function(candidates, eligible) {
  indices <- which(eligible)
  priority <- min(candidates$simplicity_rank[indices])
  indices <- indices[candidates$simplicity_rank[indices] == priority]
  families <- unique(candidates$family[indices])
  representatives <- vapply(families, function(family) {
    family_indices <- indices[candidates$family[indices] == family]
    family_indices[order(
      candidates$complexity_proxy[family_indices],
      candidates$cv_score[family_indices],
      candidates$configuration_id[family_indices]
    )][[1L]]
  }, integer(1))
  representatives[order(
    candidates$cv_score[representatives],
    candidates$family[representatives],
    candidates$configuration_id[representatives]
  )][[1L]]
}

tuning_fold_assignment <- function(y,
                                   task,
                                   requested,
                                   supplied_ids = NULL,
                                   supplied_labels = NULL) {
  if (!is.null(supplied_ids)) {
    if (length(supplied_ids) != length(y) || anyNA(supplied_ids) ||
          !is.numeric(supplied_ids) || any(supplied_ids != floor(supplied_ids))) {
      stop("Resolved supplied fold IDs must assign every training row once.", call. = FALSE)
    }
    folds <- length(supplied_labels)
    if (folds < 2L || any(!supplied_ids %in% seq_len(folds))) {
      stop("Resolved supplied fold IDs contain an invalid fold index.", call. = FALSE)
    }
    return(list(
      id = as.integer(supplied_ids),
      folds = as.integer(folds),
      labels = as.character(supplied_labels),
      source = "supplied_vfold"
    ))
  }
  requested <- assert_count(requested, "nfolds")
  if (requested < 2L) stop("`nfolds` must be at least 2 for tuning.", call. = FALSE)
  n <- length(y)
  maximum <- if (task == "regression") {
    floor(n / 2L)
  } else {
    min(table(y, useNA = "no"))
  }
  folds <- min(requested, maximum)
  if (folds < 2L) {
    stop("At least two resampling folds require two usable rows per outcome class.",
         call. = FALSE)
  }
  id <- integer(n)
  if (task == "regression") {
    order <- sample.int(n)
    id[order] <- rep(seq_len(folds), length.out = n)
  } else {
    groups <- split(seq_len(n), as.character(y), drop = TRUE)
    for (group in groups) {
      order <- sample(group)
      id[order] <- rep(seq_len(folds), length.out = length(group))
    }
  }
  list(
    id = id,
    folds = folds,
    labels = as.character(seq_len(folds)),
    source = "automatic"
  )
}

prepare_tuning_fold <- function(raw_data,
                                target,
                                task,
                                fold_assignment,
                                fold,
                                enable_preprocessing,
                                preprocessing_config,
                                fold_label = as.character(fold)) {
  training_rows <- which(fold_assignment != fold)
  validation_rows <- which(fold_assignment == fold)
  training <- raw_data[training_rows, , drop = FALSE]
  validation <- raw_data[validation_rows, , drop = FALSE]
  assert_drop_rows_class_coverage(
    training = training,
    validation = validation,
    target = target,
    task = task,
    fold = fold_label,
    enable_preprocessing = enable_preprocessing,
    preprocessing_config = preprocessing_config
  )
  processed <- preprocess_guided_split(
    training,
    validation,
    target,
    task,
    enable_preprocessing,
    utils::modifyList(preprocessing_config, list(verbose = FALSE))
  )
  constant <- remove_guided_constant_predictors(processed, target)
  processed <- constant$processed
  if (nrow(processed$evaluation$data) < 2L) {
    stop("A tuning fold retained fewer than two rows after preprocessing.", call. = FALSE)
  }
  processed_row_names <- rownames(processed$evaluation$data)
  kept_positions <- match(processed_row_names, rownames(validation))
  if (anyNA(kept_positions) || anyDuplicated(kept_positions)) {
    stop(
      "Fold preprocessing could not preserve validation-row identity.",
      call. = FALSE
    )
  }
  omitted_positions <- setdiff(seq_along(validation_rows), kept_positions)
  list(
    training = processed$training$data,
    validation = processed$evaluation$data,
    fold_label = as.character(fold_label),
    validation_row = validation_rows[kept_positions],
    source_row = rownames(raw_data)[validation_rows[kept_positions]],
    validation_rows_requested = length(validation_rows),
    omitted_validation_row = validation_rows[omitted_positions],
    omitted_source_row = rownames(raw_data)[validation_rows[omitted_positions]],
    novel_levels_mapped = sum(
      processed$evaluation$preprocessing_log$novel_level_mappings %||% integer()
    ),
    retained_features = setdiff(names(processed$training$data), target),
    removed_features = setdiff(
      setdiff(names(raw_data), target),
      setdiff(names(processed$training$data), target)
    )
  )
}

assert_drop_rows_class_coverage <- function(training,
                                            validation,
                                            target,
                                            task,
                                            fold,
                                            enable_preprocessing,
                                            preprocessing_config) {
  if (identical(task, "regression") || !isTRUE(enable_preprocessing)) {
    return(invisible(TRUE))
  }
  strategy <- normalize_missing_strategy(
    preprocessing_config$missing_value_strategy %||% "impute"
  )
  if (!identical(strategy, "drop_rows")) return(invisible(TRUE))

  partitions <- list(training = training, validation = validation)
  for (partition_name in names(partitions)) {
    partition <- partitions[[partition_name]]
    before <- unique(as.character(partition[[target]][!is.na(partition[[target]])]))
    retained <- partition[stats::complete.cases(partition), , drop = FALSE]
    after <- unique(as.character(retained[[target]][!is.na(retained[[target]])]))
    removed_classes <- sort(setdiff(before, after))
    if (!length(removed_classes)) next
    stop(
      "Tuning fold ", fold, " lost outcome class ",
      paste0("`", removed_classes, "`", collapse = ", "),
      " from its ", partition_name, " partition because ",
      "`missing_value_strategy = \"drop_rows\"` removed every row in that class. ",
      "Use `missing_value_strategy = \"impute\"`, repair the missing predictors, ",
      "or provide more complete examples of the affected class.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

score_tuning_configuration <- function(configuration,
                                       fold,
                                       target,
                                       task,
                                       fold_id,
                                       metric = if (task == "regression") "rmse" else "log_loss",
                                       retain_oof = TRUE,
                                       failure_policy = "continue") {
  started <- proc.time()[["elapsed"]]
  error <- ""
  fit_warning <- character()
  fit_spec <- tuning_configuration_fit_spec(
    configuration,
    data = fold$training,
    target = target
  )
  result <- tryCatch(
    withCallingHandlers(
      {
        model <- fit_tuning_configuration(
          configuration,
          fold$training,
          target,
          task,
          fit_spec = fit_spec,
          fit_scope = "resampling_fold"
        )
        explainer <- explain_model(
          model,
          fold$validation,
          target,
          task = task,
          label = configuration$model[[1L]]
        )
        predictions <- predict(explainer, explainer$data)
        list(
          score = evaluate_predictions(explainer$y, predictions, explainer)[[metric]],
          predictions = if (retain_oof) {
            format_tuning_predictions(
              configuration = configuration,
              fold = fold,
              explainer = explainer,
              predictions = predictions,
              task = task,
              fold_id = fold_id,
              metric = metric
            )
          } else {
            NULL
          }
        )
      },
      warning = function(condition) {
        fit_warning <<- c(fit_warning, conditionMessage(condition))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(condition) {
      error <<- conditionMessage(condition)
      list(score = NA_real_, predictions = empty_tuning_predictions(task))
    }
  )
  if (nzchar(error) && identical(failure_policy, "stop")) {
    stop(
      "Configuration `", configuration$configuration_id[[1L]], "` (family `",
      configuration$family[[1L]], "`) failed in tuning fold ", fold$fold_label %||% fold_id,
      " under `failure_policy = \"stop\"`: ", error,
      call. = FALSE
    )
  }
  score <- data.frame(
    configuration_id = configuration$configuration_id[[1L]],
    fold = fold_id,
    requested_parameter_key = fit_spec$requested_parameter_key,
    effective_parameter_key = fit_spec$effective_parameter_key,
    requested_configuration_seed = fit_spec$requested_configuration_seed,
    fit_seed = fit_spec$fit_seed,
    score = result$score,
    validation_rows = nrow(fold$validation),
    validation_rows_requested = fold$validation_rows_requested,
    validation_rows_omitted = length(fold$omitted_validation_row),
    novel_levels_mapped = fold$novel_levels_mapped,
    elapsed_ms = max(0, 1000 * (proc.time()[["elapsed"]] - started)),
    warning = paste(unique(fit_warning), collapse = " | "),
    error = error,
    stringsAsFactors = FALSE
  )
  score$requested_parameters <- I(list(fit_spec$requested_parameters))
  score$effective_parameters <- I(list(fit_spec$effective_parameters))
  list(
    score = score,
    predictions = result$predictions
  )
}

format_tuning_predictions <- function(configuration,
                                      fold,
                                      explainer,
                                      predictions,
                                      task,
                                      fold_id,
                                      metric) {
  observed <- explainer$y
  rows <- length(observed)
  output <- data.frame(
    configuration_id = rep(configuration$configuration_id[[1L]], rows),
    family = rep(configuration$family[[1L]], rows),
    backend = rep(configuration$backend[[1L]], rows),
    fold = rep(as.integer(fold_id), rows),
    training_row = as.integer(fold$validation_row),
    source_row = as.character(fold$source_row),
    truth = if (task == "regression") as.numeric(observed) else as.character(observed),
    estimate = rep(NA_real_, rows),
    predicted_class = rep(NA_character_, rows),
    truth_probability = rep(NA_real_, rows),
    case_loss = rep(NA_real_, rows),
    stringsAsFactors = FALSE
  )
  if (task == "regression") {
    output$estimate <- as.numeric(predictions)
    output$case_loss <- if (identical(metric, "mae")) {
      abs(output$truth - output$estimate)
    } else {
      (output$truth - output$estimate)^2
    }
    output$probabilities <- I(matrix(
      numeric(), nrow = rows, ncol = 0L,
      dimnames = list(NULL, character())
    ))
    return(output)
  }

  class_levels <- explainer$class_levels
  if (task == "binary") {
    positive <- explainer$positive
    negative <- setdiff(class_levels, positive)[[1L]]
    positive_probability <- pmin(pmax(as.numeric(predictions), 0), 1)
    probability <- cbind(1 - positive_probability, positive_probability)
    colnames(probability) <- c(negative, positive)
    probability <- probability[, class_levels, drop = FALSE]
  } else {
    probability <- as.matrix(predictions)[, class_levels, drop = FALSE]
  }
  output$predicted_class <- class_levels[max.col(probability, ties.method = "first")]
  truth_column <- match(output$truth, class_levels)
  output$truth_probability <- probability[cbind(seq_len(rows), truth_column)]
  output$case_loss <- if (identical(metric, "brier_score")) {
    if (task == "binary") {
      positive <- explainer$positive
      (probability[, positive] - as.numeric(output$truth == positive))^2
    } else {
      one_hot <- matrix(0, nrow = rows, ncol = length(class_levels))
      one_hot[cbind(seq_len(rows), truth_column)] <- 1
      rowSums((probability - one_hot)^2)
    }
  } else {
    -log(pmax(output$truth_probability, 1e-15))
  }
  output$probabilities <- I(probability)
  output
}

empty_tuning_predictions <- function(task) {
  output <- data.frame(
    configuration_id = character(),
    family = character(),
    backend = character(),
    fold = integer(),
    training_row = integer(),
    source_row = character(),
    truth = if (task == "regression") numeric() else character(),
    estimate = numeric(),
    predicted_class = character(),
    truth_probability = numeric(),
    case_loss = numeric(),
    stringsAsFactors = FALSE
  )
  output$probabilities <- I(matrix(numeric(), nrow = 0L, ncol = 0L))
  output
}

combine_tuning_predictions <- function(rows, task) {
  populated <- vapply(rows, nrow, integer(1)) > 0L
  if (!any(populated)) return(empty_tuning_predictions(task))
  output <- do.call(rbind, rows[populated])
  rownames(output) <- NULL
  output
}

combine_tuning_omissions <- function(folds) {
  rows <- lapply(seq_along(folds), function(fold_id) {
    fold <- folds[[fold_id]]
    if (!length(fold$omitted_validation_row)) return(NULL)
    data.frame(
      fold = rep(as.integer(fold_id), length(fold$omitted_validation_row)),
      training_row = as.integer(fold$omitted_validation_row),
      source_row = as.character(fold$omitted_source_row),
      reason = rep("removed by fold-specific preprocessing", length(fold$omitted_validation_row)),
      stringsAsFactors = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) {
    return(data.frame(
      fold = integer(), training_row = integer(), source_row = character(),
      reason = character(), stringsAsFactors = FALSE
    ))
  }
  output <- do.call(rbind, rows)
  output <- output[order(output$training_row), , drop = FALSE]
  rownames(output) <- NULL
  output
}

tuning_prediction_schema <- function(outcome,
                                     task,
                                     rows_requested,
                                     rows_evaluated,
                                     metric) {
  class_levels <- if (task == "regression") character() else levels(outcome)
  list(
    retained = TRUE,
    unit = "one outer-training row per complete, resampling-valid configuration",
    row_identity = c(
      training_row = "position within the outer training data",
      source_row = "row name inherited from the user data"
    ),
    estimate = if (task == "regression") {
      "numeric outcome prediction"
    } else {
      "not used for classification; inspect `predicted_class` and `probabilities`"
    },
    probabilities = if (task == "regression") {
      "a zero-column matrix"
    } else {
      "a numeric matrix in `class_levels` order, row-aligned with the evidence table"
    },
    case_loss = switch(
      metric,
      rmse = "squared error (pooled and square-rooted for RMSE)",
      mae = "absolute error",
      log_loss = "negative log likelihood",
      brier_score = if (task == "binary") {
        "squared positive-class probability error"
      } else {
        "sum of squared class-probability errors"
      }
    ),
    coverage = c(
      rows_requested = as.integer(rows_requested),
      rows_evaluated = as.integer(rows_evaluated),
      rows_omitted = as.integer(rows_requested - rows_evaluated)
    ),
    omission_note = if (rows_evaluated < rows_requested) {
      paste(
        "Rows removed by fold-specific preprocessing have no prediction and are listed",
        "once in `omitted_rows`; candidate `evaluated_rows` counts only predicted rows."
      )
    } else {
      "No outer-training rows were omitted from out-of-fold evidence."
    },
    class_levels = class_levels,
    positive_class = if (task == "binary") class_levels[[2L]] else NA_character_,
    comparison_note = paste(
      "Join configurations by `training_row` (and verify `fold`) for paired",
      "out-of-fold comparisons. These rows are training evidence, not holdout estimates."
    )
  )
}

tuning_prediction_schema_not_retained <- function(outcome,
                                                  task,
                                                  rows_requested,
                                                  rows_evaluated,
                                                  metric) {
  class_levels <- if (task == "regression") character() else levels(outcome)
  list(
    retained = FALSE,
    metric = metric,
    rows_requested = as.integer(rows_requested),
    rows_evaluated = as.integer(rows_evaluated),
    class_levels = class_levels,
    positive_class = if (task == "binary") class_levels[[2L]] else NA_character_,
    note = paste(
      "Row-level out-of-fold predictions were not retained because",
      "`tuning_control(retain_oof = FALSE)` was used. Candidate and fold summaries remain."
    )
  )
}

summarize_tuning_candidates <- function(plan,
                                        fold_scores,
                                        expected_folds,
                                        task,
                                        metric = if (task == "regression") {
                                          "rmse"
                                        } else {
                                          "log_loss"
                                        }) {
  rows <- lapply(seq_len(nrow(plan)), function(index) {
    id <- plan$configuration_id[[index]]
    scores <- fold_scores[fold_scores$configuration_id == id, , drop = FALSE]
    complete <- sum(is.finite(scores$score))
    valid <- is.finite(scores$score) & is.finite(scores$validation_rows) &
      scores$validation_rows > 0
    uncertainty <- tuning_fold_uncertainty(
      scores$score[valid],
      scores$validation_rows[valid],
      metric
    )
    cv_score <- if (complete == expected_folds && sum(valid) == expected_folds) {
      uncertainty$score
    } else {
      NA_real_
    }
    data.frame(
      configuration_id = id,
      family = plan$family[[index]],
      backend = plan$backend[[index]],
      model = plan$model[[index]],
      hyperparameters = plan$hyperparameters[[index]],
      cv_score = cv_score,
      cv_sd = uncertainty$sd,
      cv_se = uncertainty$se,
      folds_completed = complete,
      evaluated_rows = sum(scores$validation_rows[is.finite(scores$score)]),
      simplicity_rank = plan$simplicity_rank[[index]],
      complexity_definition = plan$complexity_definition[[index]],
      complexity_proxy = plan$complexity_proxy[[index]],
      selected = FALSE,
      status = if (complete == expected_folds) "ok" else "failed",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

tuning_fold_uncertainty <- function(scores, validation_rows, metric) {
  if (!length(scores)) {
    return(list(score = NA_real_, sd = NA_real_, se = NA_real_))
  }
  weights <- validation_rows / sum(validation_rows)
  components <- if (identical(metric, "rmse")) scores^2 else scores
  component_mean <- sum(weights * components)
  score <- if (identical(metric, "rmse")) sqrt(component_mean) else component_mean
  if (length(scores) < 2L) {
    return(list(score = score, sd = NA_real_, se = NA_real_))
  }
  correction <- 1 - sum(weights^2)
  if (!is.finite(correction) || correction <= 0) {
    return(list(score = score, sd = NA_real_, se = NA_real_))
  }
  component_variance <- sum(weights * (components - component_mean)^2) / correction
  component_sd <- sqrt(max(0, component_variance))
  effective_folds <- 1 / sum(weights^2)
  component_se <- component_sd / sqrt(effective_folds)
  if (identical(metric, "rmse")) {
    if (score == 0) {
      metric_sd <- metric_se <- 0
    } else {
      metric_sd <- component_sd / (2 * score)
      metric_se <- component_se / (2 * score)
    }
  } else {
    metric_sd <- component_sd
    metric_se <- component_se
  }
  list(score = score, sd = metric_sd, se = metric_se)
}

fit_tuning_configuration <- function(configuration,
                                     data,
                                     target,
                                     task,
                                     fit_spec = NULL,
                                     fit_scope = "full_training_refit") {
  family <- configuration$family[[1L]]
  definition <- learner_definition(family)
  fit_spec <- fit_spec %||% tuning_configuration_fit_spec(
    configuration,
    data = data,
    target = target
  )
  model <- with_preserved_seed(
    fit_spec$fit_seed,
    definition$fit(
      data = data,
      target = target,
      task = task,
      parameters = fit_spec$requested_parameters,
      seed = fit_spec$fit_seed
    )
  )
  attr(model, "autoxplain_tuning_fit") <- list(
    configuration_id = configuration$configuration_id[[1L]],
    family = family,
    scope = fit_scope,
    search_seed = fit_spec$search_seed,
    requested_parameters = fit_spec$requested_parameters,
    effective_parameters = fit_spec$effective_parameters,
    requested_parameter_key = fit_spec$requested_parameter_key,
    effective_parameter_key = fit_spec$effective_parameter_key,
    requested_configuration_seed = fit_spec$requested_configuration_seed,
    fit_seed = fit_spec$fit_seed
  )
  model
}

tuning_configuration_fit_spec <- function(configuration, data, target) {
  family <- configuration$family[[1L]]
  requested <- configuration$parameters[[1L]]
  effective <- effective_learner_parameters(
    family,
    requested,
    data = data,
    target = target
  )
  search_seed <- if ("search_seed" %in% names(configuration)) {
    configuration$search_seed[[1L]]
  } else {
    configuration$seed[[1L]]
  }
  requested_key <- canonical_tuning_parameter_key(requested)
  effective_key <- canonical_tuning_parameter_key(effective)
  list(
    search_seed = as.integer(search_seed),
    requested_parameters = requested,
    effective_parameters = effective,
    requested_parameter_key = requested_key,
    effective_parameter_key = effective_key,
    requested_configuration_seed = as.integer(configuration$seed[[1L]]),
    fit_seed = stable_configuration_seed(search_seed, family, effective)
  )
}

fit_tuned_neural_network <- function(data, target, task, size, decay) {
  features <- setdiff(names(data), target)
  feature_formula <- safe_reformulate(features)
  frame <- stats::model.frame(feature_formula, data = data, na.action = stats::na.fail)
  terms <- stats::terms(frame)
  matrix <- stats::model.matrix(terms, frame)
  if ("(Intercept)" %in% colnames(matrix)) {
    matrix <- matrix[, colnames(matrix) != "(Intercept)", drop = FALSE]
  }
  center <- colMeans(matrix)
  scale <- apply(matrix, 2L, stats::sd)
  scale[!is.finite(scale) | scale == 0] <- 1
  x <- sweep(sweep(matrix, 2L, center, "-"), 2L, scale, "/")
  y <- data[[target]]
  levels <- if (task == "regression") NULL else base::levels(y)
  y_center <- 0
  y_scale <- 1
  arguments <- list(
    x = x,
    size = size,
    decay = decay,
    maxit = 500L,
    trace = FALSE,
    rang = 0.1
  )
  if (task == "regression") {
    y_center <- mean(y)
    y_scale <- stats::sd(y)
    if (!is.finite(y_scale) || y_scale == 0) {
      stop("A regression tuning fold has no outcome variation.", call. = FALSE)
    }
    arguments$y <- (y - y_center) / y_scale
    arguments$linout <- TRUE
  } else if (task == "binary") {
    arguments$y <- as.numeric(y == levels[[2L]])
    arguments$entropy <- TRUE
  } else {
    arguments$y <- nnet::class.ind(y)
    arguments$softmax <- TRUE
  }
  outputs <- if (task == "multiclass") length(levels) else 1L
  arguments$MaxNWts <- max(
    1000L,
    as.integer((ncol(x) + 1L) * size + (size + 1L) * outputs + 100L)
  )
  fitted <- do.call(nnet::nnet, arguments)
  structure(
    list(
      model = fitted,
      terms = terms,
      columns = colnames(matrix),
      xlevels = lapply(data[features][vapply(data[features], is.factor, logical(1))], levels),
      center = center,
      scale = scale,
      y_center = y_center,
      y_scale = y_scale,
      task = task,
      class_levels = levels,
      size = size,
      decay = decay
    ),
    class = "autoxplain_tuned_nnet"
  )
}

#' @export
predict.autoxplain_tuned_nnet <- function(object, newdata, ...) {
  assert_data_frame(newdata, "newdata")
  for (feature in intersect(names(object$xlevels), names(newdata))) {
    newdata[[feature]] <- factor(
      as.character(newdata[[feature]]),
      levels = object$xlevels[[feature]]
    )
  }
  frame <- stats::model.frame(
    object$terms,
    data = newdata,
    xlev = object$xlevels,
    na.action = stats::na.fail
  )
  matrix <- stats::model.matrix(object$terms, frame)
  if ("(Intercept)" %in% colnames(matrix)) {
    matrix <- matrix[, colnames(matrix) != "(Intercept)", drop = FALSE]
  }
  if (!identical(colnames(matrix), object$columns)) {
    stop("Neural-network prediction columns do not match the fitted schema.", call. = FALSE)
  }
  x <- sweep(sweep(matrix, 2L, object$center, "-"), 2L, object$scale, "/")
  raw <- stats::predict(object$model, x, type = "raw")
  if (object$task == "regression") {
    return(as.numeric(raw) * object$y_scale + object$y_center)
  }
  if (object$task == "binary") return(as.numeric(raw))
  output <- as.matrix(raw)
  colnames(output) <- object$class_levels
  output
}

tuning_rule_label <- function(rule) {
  if (identical(rule, "one_se")) {
    paste(
      "one-standard-error (prefer the reviewed family priority, then the",
      "least-flexible near-best setting within that family)"
    )
  } else {
    "lowest resampled error"
  }
}
