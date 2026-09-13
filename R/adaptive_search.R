# Search helpers use only the outer training data. Screening scores decide
# which configurations receive complete cross-validation, not final accuracy.
adaptive_parameter_grids <- function(learners, n, p, task, n_classes, seed, max_models,
                                     forest_budget = NULL) {
  count <- assert_count(max_models, "max_models")
  seed <- assert_count(seed, "seed", minimum = 0L)
  registry <- autoxplain_learner_registry()
  if (!is.character(learners) || !length(learners) || anyNA(learners) ||
        anyDuplicated(learners) || any(!learners %in% names(registry))) {
    stop("Adaptive search requires known, unique learner families.", call. = FALSE)
  }
  p <- assert_count(p, "p")
  n <- assert_count(n, "n", minimum = 2L)
  if ("forest" %in% learners) {
    forest_budget <- forest_budget %||% forest_validation_budget_policy(n, p, "adaptive")
  }
  grids <- lapply(learners, function(family) {
    defaults <- registry[[family]]$grid(n, p, task, n_classes)
    if (!family %in% c("regularized", "forest", "boosting")) return(defaults)
    family_seed <- stable_configuration_seed(seed, family, list(stage = "adaptive_pool"))
    grid <- with_preserved_seed(family_seed, {
      spread <- adaptive_parameter_spread(count + 8L, 8L)
      if (family == "boosting") {
        # Compare depth at a common learning rate and regularization level.
        # Otherwise the screening cap can confound deeper interaction capacity
        # with slower learning and stronger penalties. Spread proposals retain
        # rate, sampling and regularization diversity without adding slots.
        anchors <- lapply(c(3L, 6L, 10L), function(depth) {
          list(
            nrounds = 2000L, eta = .05, max_depth = depth,
            min_child_weight = 1, subsample = .8, colsample_bytree = .8,
            reg_alpha = 0, reg_lambda = 1
          )
        })
        proposals <- lapply(seq_len(nrow(spread)), function(index) {
          point <- spread[index, ]
          list(
            nrounds = 2000L,
            eta = signif(exp(log(.03) + point[[1L]] * log(.2 / .03)), 3L),
            max_depth = as.integer(2L + floor(point[[2L]] * 9L)),
            min_child_weight = signif(exp(point[[3L]] * log(32)), 3L),
            subsample = round(.6 + .4 * point[[4L]], 3L),
            colsample_bytree = round(.5 + .5 * point[[5L]], 3L),
            reg_alpha = if (point[[6L]] < .25) 0 else signif(10^(point[[6L]] * 4 - 3), 3L),
            reg_lambda = signif(10^(point[[7L]] * 3 - 1), 3L)
          )
        })
      } else if (family == "forest") {
        anchors <- utils::head(defaults, 4L)
        # The automatic final count is part of each planned configuration,
        # before normalization, parameter keys, descriptions and fitting seeds.
        # Exact grids use local_tuning_plan() directly and never enter this path.
        anchors <- lapply(anchors, function(parameters) {
          parameters$num.trees <- forest_budget$final_num_trees
          parameters
        })
        policy <- forest_search_policy(n, p, task)
        proposals <- lapply(seq_len(nrow(spread)), function(index) {
          point <- spread[index, ]
          list(
            num.trees = forest_budget$final_num_trees,
            mtry = as.integer(1L + floor(point[[1L]] * policy$mtry_upper)),
            min.node.size = as.integer(round(exp(
              log(policy$node_lower) + point[[2L]] * log(policy$node_upper / policy$node_lower)
            ))),
            sample.fraction = round(.55 + .45 * point[[3L]], 3L),
            splitrule = if (point[[4L]] < .25) "extratrees" else "default"
          )
        })
      } else {
        anchors <- defaults[c(4L, 2L, 10L)]
        proposals <- lapply(seq_len(nrow(spread)), function(index) {
          list(alpha = round(spread[index, 1L], 3L),
               path_fraction = round(.3 + .7 * spread[index, 2L], 3L))
        })
      }
      c(anchors, proposals)
    })
    keys <- vapply(grid, canonical_tuning_parameter_key, character(1))
    grid <- grid[!duplicated(keys)]
    normalize_family_grid(utils::head(grid, count), family)
  })
  stats::setNames(grids, learners)
}

# Stratified coordinates cover each parameter range without constructing a
# Cartesian product. The caller preserves and restores the user's RNG state.
adaptive_parameter_spread <- function(count, dimensions) {
  vapply(seq_len(dimensions), function(dimension) {
    (sample.int(count) - stats::runif(count)) / count
  }, numeric(count))
}

adaptive_screen_partition <- function(raw_data, target, task, fold_assignment, seed,
                                      screening_rows, groups = NULL) {
  cap <- assert_count(screening_rows, "screening_rows", minimum = 12L)
  seed <- assert_count(seed, "seed", minimum = 0L)
  ids <- if (is.list(fold_assignment)) fold_assignment$id else fold_assignment
  if (length(ids) != nrow(raw_data) || anyNA(ids) || length(unique(ids)) < 2L) {
    stop("Screening requires one valid original fold assignment per training row.", call. = FALSE)
  }
  if (!target %in% names(raw_data) || anyNA(raw_data[[target]])) {
    stop("Screening requires an observed training outcome.", call. = FALSE)
  }
  if (!is.null(groups)) {
    if (length(groups) != nrow(raw_data) || anyNA(groups)) {
      stop("Screening group labels must match the training rows without missing values.", call. = FALSE)
    }
    group_folds <- split(as.character(ids), as.character(groups))
    if (any(lengths(lapply(group_folds, unique)) != 1L)) {
      stop("Screening cannot separate groups that cross the original validation folds.", call. = FALSE)
    }
  }
  outcome <- raw_data[[target]]
  class_labels <- if (task == "regression") NULL else unique(as.character(outcome))
  folds <- unique(ids)
  eligible <- vapply(folds, function(fold) {
    training <- which(ids != fold)
    validation <- which(ids == fold)
    n_train <- length(training)
    n_valid <- length(validation)
    n_train >= 10L && n_valid >= 2L &&
      (is.null(class_labels) ||
         (setequal(as.character(outcome[training]), class_labels) &&
            setequal(as.character(outcome[validation]), class_labels)))
  }, logical(1))
  if (!any(eligible)) {
    stop("No original fold can screen models with enough rows and every outcome class on both sides.",
      call. = FALSE
    )
  }
  with_preserved_seed(seed, {
    available <- folds[eligible]
    held_out <- available[[sample.int(length(available), 1L)]]
    training_pool <- which(ids != held_out)
    validation_pool <- which(ids == held_out)
    total <- min(cap, nrow(raw_data))
    minimum_valid <- max(2L, length(class_labels))
    minimum_train <- max(10L, length(class_labels))
    if (total < minimum_train + minimum_valid) {
      stop("The screening row cap is too small to preserve the training and validation classes.",
        call. = FALSE
      )
    }
    valid_count <- min(length(validation_pool), max(minimum_valid, as.integer(round(total / 5))))
    train_count <- min(length(training_pool), total - valid_count)
    if (train_count < minimum_train) {
      train_count <- min(length(training_pool), minimum_train)
      valid_count <- min(length(validation_pool), total - train_count)
    }
    if (train_count + valid_count < total) {
      valid_count <- min(length(validation_pool), total - train_count)
    }
    training <- adaptive_sample_rows(training_pool, outcome, task, train_count)
    validation <- adaptive_sample_rows(validation_pool, outcome, task, valid_count)
    training_sampling <- adaptive_sampling_record(training_pool, training, outcome, task)
    validation_sampling <- adaptive_sampling_record(validation_pool, validation, outcome, task)
    group_count <- function(rows) if (is.null(groups)) NA_integer_ else length(unique(groups[rows]))
    list(
      training_row = training, validation_row = validation,
      training_source_row = rownames(raw_data)[training],
      validation_source_row = rownames(raw_data)[validation],
      fold = held_out, seed = seed, rows_requested = cap,
      training_rows = length(training), validation_rows = length(validation),
      training_groups = group_count(training), validation_groups = group_count(validation),
      training_sampling_weight = training_sampling$weights,
      validation_sampling_weight = validation_sampling$weights,
      sampling = list(training = training_sampling$counts, validation = validation_sampling$counts),
      scope = paste(
        "Screening uses a common sample of outer-training rows.",
        "One original validation fold supplies the screening assessment; the other folds supply training.",
        if (task != "regression") {
          "Sampling retains every observed class on both sides."
        } else {
          "Rows are sampled at random."
        },
        if (!is.null(groups)) {
          paste(
            "Whole groups stay on one side; rows may be sampled within those groups.",
            "Scores weight sampled rows, not groups equally."
          )
        } else {
          "Original fold boundaries are preserved."
        },
        "Screening and later CV scores are selection evidence; final evaluation rows are not used."
      )
    )
  })
}

adaptive_sampling_record <- function(pool, sampled, outcome, task) {
  labels <- if (task == "regression") rep("all rows", length(outcome)) else as.character(outcome)
  strata <- sort(unique(labels[pool]))
  counts <- data.frame(
    stratum = strata,
    available = tabulate(match(labels[pool], strata), nbins = length(strata)),
    sampled = tabulate(match(labels[sampled], strata), nbins = length(strata)),
    stringsAsFactors = FALSE
  )
  weights <- counts$available / counts$sampled
  list(counts = counts, weights = unname(weights[match(labels[sampled], strata)]))
}

adaptive_sample_rows <- function(rows, outcome, task, count) {
  if (count >= length(rows)) return(sort(as.integer(rows)))
  if (task == "regression") return(sort(as.integer(rows[sample.int(length(rows), count)])))
  strata <- split(rows, as.character(outcome[rows]), drop = TRUE)
  if (count < length(strata)) {
    stop("The screening sample cannot retain every outcome class.", call. = FALSE)
  }
  sizes <- lengths(strata)
  # Promote before multiplication: ordinary integer row counts can overflow
  # even when the requested sample itself is small.
  ideal <- as.double(count) * sizes / sum(sizes)
  allocated <- pmin(sizes, pmax(1L, as.integer(floor(ideal))))
  while (sum(allocated) > count) {
    removable <- which(allocated > 1L)
    index <- removable[[which.max((allocated - ideal)[removable])]]
    allocated[[index]] <- allocated[[index]] - 1L
  }
  while (sum(allocated) < count) {
    available <- which(allocated < sizes)
    index <- available[[which.max((ideal - allocated)[available])]]
    allocated[[index]] <- allocated[[index]] + 1L
  }
  chosen <- lapply(seq_along(strata), function(index) {
    values <- strata[[index]]
    values[sample.int(length(values), allocated[[index]])]
  })
  sort(as.integer(unlist(chosen, use.names = FALSE)))
}

adaptive_screen_parameters <- function(parameters, family, training_rows = NULL,
                                       planned_rows = NULL, planned_predictors = NULL) {
  if (family == "forest") {
    parameters$num.trees <- min(parameters$num.trees, 128L)
    if (!is.null(training_rows) && !is.null(planned_rows) && !is.null(planned_predictors)) {
      training_rows <- assert_count(training_rows, "training_rows", minimum = 2L)
      planned_rows <- assert_count(planned_rows, "planned_rows", minimum = 2L)
      planned_predictors <- assert_count(planned_predictors, "planned_predictors")
      if (training_rows > planned_rows) {
        stop("Screening cannot use more training rows than the complete search population.", call. = FALSE)
      }
      if (as.double(planned_rows) * planned_predictors >= 1e6 && planned_rows > 50000L) {
        # Undo only the full-row growth rule, then apply it at screening size.
        # Otherwise a large-table node size can erase useful structure in the
        # smaller sample before a configuration gets complete validation.
        scale <- sqrt(max(1, training_rows / 50000)) / sqrt(planned_rows / 50000)
        parameters$min.node.size <- max(1L, as.integer(round(parameters$min.node.size * scale)))
      }
    }
  }
  if (family == "boosting") parameters$nrounds <- min(parameters$nrounds, 600L)
  parameters
}

adaptive_promote <- function(plan, screening, finalists_per_family, metric) {
  finalists <- assert_count(finalists_per_family, "finalists_per_family")
  if (!all(c("configuration_id", "family") %in% names(plan)) ||
        anyNA(plan$configuration_id) || anyDuplicated(plan$configuration_id) ||
        !all(c("configuration_id", "score") %in% names(screening)) ||
        anyNA(screening$configuration_id) || anyDuplicated(screening$configuration_id) ||
        any(!screening$configuration_id %in% plan$configuration_id)) {
    stop("Screening promotion requires unique configuration IDs from the search plan.", call. = FALSE)
  }
  matches <- match(plan$configuration_id, screening$configuration_id)
  scores <- screening$score[matches]
  successful <- !is.na(matches) & is.finite(scores)
  if ("error" %in% names(screening)) {
    errors <- screening$error[matches]
    successful <- successful & !is.na(errors) & !nzchar(errors)
  }
  if ("status" %in% names(screening)) {
    statuses <- screening$status[matches]
    successful <- successful & !is.na(statuses) & statuses %in% c("ok", "success", "completed")
  }
  output <- data.frame(
    configuration_id = plan$configuration_id,
    promoted = FALSE,
    reason = ifelse(successful,
      "Not among the promoted settings in its family.", "Screening did not produce a valid score."
    ),
    stringsAsFactors = FALSE
  )
  for (family in unique(plan$family)) {
    eligible <- which(plan$family == family & successful)
    if (!length(eligible)) next
    ranked <- eligible[order(selection_metric_loss(scores[eligible], metric), eligible)]
    promoted <- utils::head(ranked, finalists)
    output$promoted[promoted] <- TRUE
    output$reason[promoted] <- paste(
      "Among the best successful screening scores in its family; proceed to complete cross-validation."
    )
  }
  output
}
