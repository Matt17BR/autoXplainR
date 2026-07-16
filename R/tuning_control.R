#' Advanced controls for local model tuning
#'
#' `tuning_control()` is an optional escape hatch for users who need more
#' control than the beginner defaults in [autoxplain()]. Omitting it preserves
#' AutoXplainR's existing portfolio-aware search, stratified/random V-folds,
#' default loss, retained out-of-fold predictions, and failure isolation.
#'
#' Custom grids must be a named list keyed by learner family. Each family value
#' may be a data frame (one configuration per row), a list of named parameter
#' lists, or one named parameter list. Parameter names are exact adapter
#' contracts: `linear` has no parameters; `regularized` uses `alpha` and
#' `path_fraction`; `additive` uses `k`, `gamma`, and `select`; `tree` uses
#' `maxdepth`, `cp`, and `minsplit`; `forest` uses `num.trees`, `mtry`,
#' `min.node.size`, `sample.fraction`, and `splitrule`; `boosting` uses
#' `nrounds`, `eta`, `max_depth`, `min_child_weight`, `subsample`,
#' `colsample_bytree`, `reg_alpha`, and `reg_lambda`; `neural` uses `size` and
#' `decay`; `kernel` uses `cost`, `gamma_multiplier`, and `epsilon` (fixed at
#' `0.1` for classification because that backend ignores it);
#' `neighbors` uses `k`, `distance`, and `kernel`; and `mars` uses `degree` and
#' `nprune`.
#'
#' Family budgets are exact positive configuration counts. Their names must
#' exactly match the learner families requested from [autoxplain()]. When they
#' are supplied, leave `max_models = NULL`; the sum of the budgets replaces the
#' automatic portfolio budget.
#'
#' Supplied fold IDs define ordinary V-fold resampling: each row is validated
#' once after fitting on all rows assigned to every other fold. They are not a
#' rolling-origin or forward-chaining specification. To avoid ambiguity after
#' AutoXplainR's automatic holdout split, fold IDs are accepted only when
#' `test_data` is supplied explicitly to [autoxplain()]. Candidate losses and
#' their one-standard-error uncertainty are weighted by the number of validation
#' rows in each fold; RMSE uses pooled squared loss with a delta-method standard
#' error on the RMSE scale.
#'
#' @param grids Optional named per-family custom grids.
#' @param family_budgets Optional named positive integer counts, one for every
#'   requested learner family.
#' @param fold_ids Optional atomic vector assigning every training row to one
#'   supplied V-fold.
#' @param metric Selection loss. `"auto"` resolves to RMSE for regression and
#'   log loss for classification. Regression also supports `"mae"`;
#'   classification also supports `"brier"`.
#' @param retain_oof Retain row-level out-of-fold predictions and case losses.
#' @param failure_policy `"continue"` records a failed configuration and keeps
#'   searching; `"stop"` aborts on the first resampling or refit failure.
#'
#' @return An `autoxplain_tuning_control` object for the `tuning_control`
#'   argument of [autoxplain()].
#' @export
#'
#' @examples
#' control <- tuning_control(
#'   grids = list(tree = data.frame(
#'     maxdepth = c(2L, 5L), cp = c(0.02, 0.002), minsplit = c(12L, 6L)
#'   )),
#'   family_budgets = c(linear = 1L, tree = 2L),
#'   metric = "mae"
#' )
#' control
tuning_control <- function(grids = NULL,
                           family_budgets = NULL,
                           fold_ids = NULL,
                           metric = c("auto", "rmse", "mae", "log_loss", "brier"),
                           retain_oof = TRUE,
                           failure_policy = c("continue", "stop")) {
  metric <- match.arg(metric)
  failure_policy <- match.arg(failure_policy)
  assert_tuning_flag(retain_oof, "retain_oof")
  normalized_grids <- normalize_tuning_grids(grids)
  normalized_budgets <- normalize_family_budgets(family_budgets)
  normalized_fold_ids <- normalize_fold_id_vector(fold_ids)
  structure(
    list(
      grids = normalized_grids,
      family_budgets = normalized_budgets,
      fold_ids = normalized_fold_ids,
      metric = metric,
      retain_oof = retain_oof,
      failure_policy = failure_policy
    ),
    class = "autoxplain_tuning_control"
  )
}

#' @export
print.autoxplain_tuning_control <- function(x, ...) {
  cat("<AutoXplainR tuning control>\n")
  cat("  metric:     ", x$metric, "\n", sep = "")
  cat("  OOF rows:   ", if (x$retain_oof) "retained" else "not retained", "\n", sep = "")
  cat("  failures:   ", x$failure_policy, "\n", sep = "")
  cat("  grids:      ", tuning_control_names(x$grids), "\n", sep = "")
  cat("  budgets:    ", tuning_control_names(x$family_budgets), "\n", sep = "")
  fold_text <- if (is.null(x$fold_ids)) {
    "automatic"
  } else {
    paste(length(unique(as.character(x$fold_ids))), "supplied folds")
  }
  cat("  fold IDs:   ", fold_text, "\n", sep = "")
  invisible(x)
}

tuning_control_names <- function(x) {
  if (is.null(x) || !length(x)) "automatic" else paste(names(x), collapse = ", ")
}

assert_tuning_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", name, "` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(TRUE)
}

normalize_family_budgets <- function(budgets) {
  if (is.null(budgets)) return(NULL)
  if (!is.atomic(budgets) || is.factor(budgets) || is.null(names(budgets)) ||
        !length(budgets) || anyNA(names(budgets)) || any(!nzchar(names(budgets))) ||
        anyDuplicated(names(budgets))) {
    stop(
      "`family_budgets` must be a non-empty, uniquely named numeric vector.",
      call. = FALSE
    )
  }
  unknown <- setdiff(names(budgets), names(autoxplain_learner_registry()))
  if (length(unknown)) {
    stop(
      "Unknown learner families in `family_budgets`: ", paste(unknown, collapse = ", "),
      ". Use `learner_catalog()` to inspect supported families.",
      call. = FALSE
    )
  }
  valid <- is.numeric(budgets) && all(is.finite(budgets)) && all(budgets >= 1) &&
    all(budgets == floor(budgets)) && all(budgets <= .Machine$integer.max)
  if (!valid || sum(as.double(budgets)) > .Machine$integer.max) {
    stop("Every `family_budgets` value must be a positive integer.", call. = FALSE)
  }
  stats::setNames(as.integer(budgets), names(budgets))
}

normalize_fold_id_vector <- function(fold_ids) {
  if (is.null(fold_ids)) return(NULL)
  valid_type <- is.factor(fold_ids) || is.character(fold_ids) ||
    (is.numeric(fold_ids) && !is.complex(fold_ids))
  if (!valid_type || !is.null(dim(fold_ids)) || length(fold_ids) < 4L || anyNA(fold_ids)) {
    stop(
      "`fold_ids` must be an atomic character, factor, or numeric vector without missing values.",
      call. = FALSE
    )
  }
  if (is.numeric(fold_ids) && any(!is.finite(fold_ids))) {
    stop("Numeric `fold_ids` must be finite.", call. = FALSE)
  }
  labels <- as.character(fold_ids)
  if (any(!nzchar(labels)) || length(unique(labels)) < 2L) {
    stop("`fold_ids` must contain at least two non-empty fold labels.", call. = FALSE)
  }
  labels
}

normalize_tuning_grids <- function(grids) {
  if (is.null(grids)) return(NULL)
  if (!is.list(grids) || !length(grids) || is.null(names(grids)) ||
        anyNA(names(grids)) || any(!nzchar(names(grids))) || anyDuplicated(names(grids))) {
    stop("`grids` must be a non-empty, uniquely named list of family grids.", call. = FALSE)
  }
  unknown <- setdiff(names(grids), names(tuning_parameter_contracts()))
  if (length(unknown)) {
    stop(
      "Unknown learner families in `grids`: ", paste(unknown, collapse = ", "),
      ". Use `learner_catalog()` to inspect supported families.",
      call. = FALSE
    )
  }
  output <- lapply(names(grids), function(family) {
    normalize_family_grid(grids[[family]], family)
  })
  names(output) <- names(grids)
  output
}

normalize_family_grid <- function(grid, family, allow_duplicates = FALSE) {
  contract <- tuning_parameter_contracts()[[family]]
  configurations <- if (is.data.frame(grid)) {
    if (!nrow(grid)) {
      stop("Custom grid `", family, "` must contain at least one row.", call. = FALSE)
    }
    lapply(seq_len(nrow(grid)), function(index) as.list(grid[index, , drop = FALSE]))
  } else if (is.list(grid) && identical(sort(names(grid)), sort(names(contract))) &&
               (!length(grid) || !all(vapply(grid, is.list, logical(1))))) {
    list(grid)
  } else if (is.list(grid) && length(grid) && all(vapply(grid, is.list, logical(1)))) {
    unname(grid)
  } else {
    stop(
      "Custom grid `", family,
      "` must be a data frame or a list of named parameter lists.",
      call. = FALSE
    )
  }
  normalized <- lapply(seq_along(configurations), function(index) {
    validate_tuning_parameters(configurations[[index]], family, index, contract)
  })
  duplicated <- vapply(seq_along(normalized), function(index) {
    index > 1L && any(vapply(
      normalized[seq_len(index - 1L)], identical, logical(1), normalized[[index]]
    ))
  }, logical(1))
  if (!allow_duplicates && any(duplicated)) {
    stop("Custom grid `", family, "` contains duplicate configurations.", call. = FALSE)
  }
  normalized
}

validate_tuning_parameters <- function(parameters, family, index, contract) {
  if (!is.list(parameters) || is.null(names(parameters)) && length(parameters) ||
        anyNA(names(parameters)) || any(!nzchar(names(parameters))) ||
        anyDuplicated(names(parameters))) {
    stop(
      "Configuration ", index, " in grid `", family,
      "` must be a uniquely named parameter list.",
      call. = FALSE
    )
  }
  missing <- setdiff(names(contract), names(parameters))
  extra <- setdiff(names(parameters), names(contract))
  if (length(missing) || length(extra)) {
    stop(
      "Configuration ", index, " in grid `", family, "` must use exactly: ",
      if (length(contract)) paste(names(contract), collapse = ", ") else "no parameters",
      ".",
      if (length(missing)) paste0(" Missing: ", paste(missing, collapse = ", "), ".") else "",
      if (length(extra)) paste0(" Unknown: ", paste(extra, collapse = ", "), ".") else "",
      call. = FALSE
    )
  }
  parameters <- parameters[names(contract)]
  for (parameter in names(contract)) {
    parameters[[parameter]] <- contract[[parameter]](
      parameters[[parameter]], family, index, parameter
    )
  }
  parameters
}

tuning_parameter_contracts <- function() {
  list(
    linear = list(),
    regularized = list(
      alpha = tuning_numeric(0, 1),
      path_fraction = tuning_numeric(0, 1)
    ),
    additive = list(
      k = tuning_integer(3L),
      gamma = tuning_numeric(0, Inf, lower_open = TRUE),
      select = tuning_logical()
    ),
    tree = list(
      maxdepth = tuning_integer(1L, 30L),
      cp = tuning_numeric(0, 1),
      minsplit = tuning_integer(2L)
    ),
    forest = list(
      num.trees = tuning_integer(1L),
      mtry = tuning_integer(1L),
      min.node.size = tuning_integer(1L),
      sample.fraction = tuning_numeric(0, 1, lower_open = TRUE),
      splitrule = tuning_choice(c("default", "extratrees"))
    ),
    boosting = list(
      nrounds = tuning_integer(1L),
      eta = tuning_numeric(0, 1, lower_open = TRUE),
      max_depth = tuning_integer(1L, 30L),
      min_child_weight = tuning_numeric(0, Inf),
      subsample = tuning_numeric(0, 1, lower_open = TRUE),
      colsample_bytree = tuning_numeric(0, 1, lower_open = TRUE),
      reg_alpha = tuning_numeric(0, Inf),
      reg_lambda = tuning_numeric(0, Inf)
    ),
    neural = list(
      size = tuning_integer(1L),
      decay = tuning_numeric(0, Inf)
    ),
    kernel = list(
      cost = tuning_numeric(0, Inf, lower_open = TRUE),
      gamma_multiplier = tuning_numeric(0, Inf, lower_open = TRUE),
      epsilon = tuning_numeric(0, Inf)
    ),
    neighbors = list(
      k = tuning_integer(1L),
      distance = tuning_numeric(0, Inf, lower_open = TRUE),
      kernel = tuning_choice(c(
        "rectangular", "triangular", "epanechnikov", "biweight", "triweight",
        "cos", "inv", "gaussian", "rank", "optimal"
      ))
    ),
    mars = list(
      degree = tuning_integer(1L, 2L),
      nprune = tuning_integer(3L)
    )
  )
}

tuning_numeric <- function(lower, upper, lower_open = FALSE, upper_open = FALSE) {
  force(lower)
  force(upper)
  function(value, family, index, parameter) {
    valid <- is.numeric(value) && length(value) == 1L && !is.na(value) && is.finite(value)
    if (valid) {
      valid <- if (lower_open) value > lower else value >= lower
      valid <- valid && if (upper_open) value < upper else value <= upper
    }
    if (!valid) {
      interval <- paste0(if (lower_open) "(" else "[", lower, ", ", upper,
                         if (upper_open) ")" else "]")
      stop(
        "Parameter `", parameter, "` in configuration ", index, " of grid `", family,
        "` must be one finite number in ", interval, ".",
        call. = FALSE
      )
    }
    as.numeric(value)
  }
}

tuning_integer <- function(lower, upper = .Machine$integer.max) {
  force(lower)
  force(upper)
  function(value, family, index, parameter) {
    valid <- is.numeric(value) && length(value) == 1L && !is.na(value) && is.finite(value) &&
      value == floor(value) && value >= lower && value <= upper
    if (!valid) {
      stop(
        "Parameter `", parameter, "` in configuration ", index, " of grid `", family,
        "` must be an integer from ", lower, " to ", upper, ".",
        call. = FALSE
      )
    }
    as.integer(value)
  }
}

tuning_logical <- function() {
  function(value, family, index, parameter) {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop(
        "Parameter `", parameter, "` in configuration ", index, " of grid `", family,
        "` must be TRUE or FALSE.",
        call. = FALSE
      )
    }
    value
  }
}

tuning_choice <- function(choices) {
  force(choices)
  function(value, family, index, parameter) {
    value <- if (is.factor(value) && length(value) == 1L) as.character(value) else value
    if (!is.character(value) || length(value) != 1L || is.na(value) ||
          !value %in% choices) {
      stop(
        "Parameter `", parameter, "` in configuration ", index, " of grid `", family,
        "` must be one of: ", paste(choices, collapse = ", "), ".",
        call. = FALSE
      )
    }
    value
  }
}

resolve_tuning_control <- function(control,
                                   learners,
                                   task,
                                   outcome,
                                   nfolds,
                                   max_models,
                                   automatic_max_models,
                                   test_data_supplied) {
  if (is.null(control)) control <- tuning_control()
  if (!inherits(control, "autoxplain_tuning_control")) {
    stop("`tuning_control` must be NULL or returned by `tuning_control()`.", call. = FALSE)
  }
  extra_grids <- setdiff(names(control$grids %||% list()), learners)
  if (length(extra_grids)) {
    stop(
      "Custom grids were supplied for families not requested by `learners` or `portfolio`: ",
      paste(extra_grids, collapse = ", "), ".",
      call. = FALSE
    )
  }
  budgets <- control$family_budgets
  if (!is.null(budgets)) {
    missing <- setdiff(learners, names(budgets))
    extra <- setdiff(names(budgets), learners)
    if (length(missing) || length(extra)) {
      stop(
        "`family_budgets` names must exactly match the requested learner families.",
        if (length(missing)) paste0(" Missing: ", paste(missing, collapse = ", "), ".") else "",
        if (length(extra)) paste0(" Extra: ", paste(extra, collapse = ", "), ".") else "",
        call. = FALSE
      )
    }
    if (!is.null(max_models)) {
      stop(
        "Supply either explicit `max_models` or `tuning_control(family_budgets = ...)`, not both. ",
        "Family budgets replace the automatic model budget.",
        call. = FALSE
      )
    }
    budgets <- budgets[learners]
    max_models <- sum(budgets)
  } else if (is.null(max_models)) {
    max_models <- automatic_max_models
  }
  max_models <- assert_count(max_models, "max_models")
  if (is.null(budgets) && max_models < length(learners)) {
    stop(
      "`max_models` must be at least the number of requested learner families (",
      length(learners), ").",
      call. = FALSE
    )
  }

  metric <- resolve_tuning_metric(control$metric, task)
  supplied <- NULL
  if (!is.null(control$fold_ids)) {
    if (!isTRUE(test_data_supplied)) {
      stop(
        "Supplied `fold_ids` require explicit `test_data`. This keeps fold IDs aligned ",
        "with the unchanged outer-training rows.",
        call. = FALSE
      )
    }
    supplied <- validate_supplied_fold_ids(control$fold_ids, outcome, task)
  } else {
    nfolds <- assert_count(nfolds, "nfolds")
    if (nfolds < 2L) stop("`nfolds` must be at least 2 for local tuning.", call. = FALSE)
  }
  structure(
    list(
      grids = control$grids,
      family_budgets = budgets,
      fold_ids = supplied$id %||% NULL,
      fold_labels = supplied$labels %||% NULL,
      fold_source = if (is.null(supplied)) "automatic" else "supplied_vfold",
      nfolds_argument = nfolds,
      metric_requested = control$metric,
      metric = metric,
      retain_oof = control$retain_oof,
      failure_policy = control$failure_policy,
      max_models = max_models
    ),
    class = "autoxplain_resolved_tuning_control"
  )
}

resolve_tuning_metric <- function(metric, task) {
  allowed <- if (task == "regression") {
    c("auto", "rmse", "mae")
  } else {
    c("auto", "log_loss", "brier")
  }
  if (!metric %in% allowed) {
    stop(
      "Tuning metric `", metric, "` is not available for task `", task, "`. Use one of: ",
      paste(allowed, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (metric == "auto") {
    if (task == "regression") "rmse" else "log_loss"
  } else if (metric == "brier") {
    "brier_score"
  } else {
    metric
  }
}

default_resolved_tuning_control <- function(task, max_models, nfolds) {
  structure(
    list(
      grids = NULL,
      family_budgets = NULL,
      fold_ids = NULL,
      fold_labels = NULL,
      fold_source = "automatic",
      nfolds_argument = nfolds,
      metric_requested = "auto",
      metric = resolve_tuning_metric("auto", task),
      retain_oof = TRUE,
      failure_policy = "continue",
      max_models = max_models
    ),
    class = "autoxplain_resolved_tuning_control"
  )
}

tuning_control_provenance <- function(control) {
  list(
    custom_grids = control$grids,
    custom_grid_families = names(control$grids %||% list()),
    family_budgets = control$family_budgets,
    fold_source = control$fold_source,
    fold_labels = control$fold_labels,
    nfolds_argument = control$nfolds_argument,
    metric_requested = control$metric_requested,
    metric = control$metric,
    retain_oof = control$retain_oof,
    failure_policy = control$failure_policy,
    max_models = control$max_models
  )
}

validate_supplied_fold_ids <- function(fold_ids, outcome, task) {
  if (length(fold_ids) != length(outcome)) {
    stop(
      "`fold_ids` must contain exactly one value for each training row (expected ",
      length(outcome), ", received ", length(fold_ids), ").",
      call. = FALSE
    )
  }
  labels <- unique(as.character(fold_ids))
  id <- match(as.character(fold_ids), labels)
  sizes <- tabulate(id, nbins = length(labels))
  too_small <- labels[sizes < 2L]
  if (length(too_small)) {
    stop(
      "Every supplied validation fold must contain at least two rows. Too small: ",
      paste(too_small, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (any(length(outcome) - sizes < 2L)) {
    bad <- labels[length(outcome) - sizes < 2L]
    stop(
      "Training on all other folds must leave at least two rows. Invalid folds: ",
      paste(bad, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (task != "regression") {
    classes <- unique(as.character(outcome[!is.na(outcome)]))
    for (index in seq_along(labels)) {
      validation_classes <- unique(as.character(outcome[id == index]))
      training_classes <- unique(as.character(outcome[id != index]))
      missing_validation <- setdiff(classes, validation_classes)
      missing_training <- setdiff(classes, training_classes)
      if (length(missing_validation) || length(missing_training)) {
        validation_detail <- if (length(missing_validation)) {
          paste0(" Missing from validation: ", paste(missing_validation, collapse = ", "), ".")
        } else {
          ""
        }
        training_detail <- if (length(missing_training)) {
          paste0(" Missing from training: ", paste(missing_training, collapse = ", "), ".")
        } else {
          ""
        }
        stop(
          "Supplied fold `", labels[[index]], "` does not preserve outcome-class coverage.",
          validation_detail,
          training_detail,
          call. = FALSE
        )
      }
    }
  }
  list(id = as.integer(id), labels = labels)
}
