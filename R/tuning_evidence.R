# Selection evidence distinguishes engine observations from package policy.
optimization_record <- function(status = "unknown", code = NA_character_,
                                message = "This backend did not expose a checked convergence diagnostic.",
                                iterations = NA_integer_, source = "unavailable") {
  list(
    status = status, code = as.character(code), message = message,
    iterations = as.integer(iterations), source = source
  )
}

model_optimization_record <- function(model) {
  neural <- inherits(model, "autoxplain_tuned_nnet")
  wrapped <- inherits(model, "autoxplain_fitted_model")
  fit <- if (neural) model$model else if (wrapped) model$fit else model
  if (inherits(fit, "nnet")) {
    code <- fit$convergence
    if (length(code) != 1L || is.na(code)) {
      return(optimization_record())
    }
    return(optimization_record(
      if (code == 0L) "converged" else "not_converged", code,
      if (code == 0L) {
        "The nnet optimizer reported convergence."
      } else {
        "The nnet optimizer reached its iteration limit without reporting convergence."
      },
      source = "nnet$convergence"
    ))
  }
  if (inherits(fit, "gam")) {
    outer <- fit$outer.info$conv %||% NULL
    outer_failed <- is.character(outer) && length(outer) == 1L &&
      !identical(outer, "full convergence")
    failed <- identical(fit$converged, FALSE) || outer_failed ||
      identical(fit$mgcv.conv$fully.converged, FALSE)
    known <- failed || identical(fit$converged, TRUE)
    return(optimization_record(
      if (failed) "not_converged" else if (known) "converged" else "unknown",
      outer %||% as.character(fit$converged %||% NA),
      if (failed) {
        paste("GAM fitting did not report complete convergence:", outer %||% "iteration failure")
      } else {
        "GAM convergence fields were checked; convergence is not a model adequacy test."
      }, fit$iter %||% NA_integer_, "mgcv convergence fields"
    ))
  }
  if (inherits(fit, "glm")) {
    status <- if (identical(fit$converged, TRUE)) {
      "converged"
    } else if (identical(fit$converged, FALSE)) {
      "not_converged"
    } else {
      "unknown"
    }
    return(optimization_record(
      status, fit$converged %||% NA,
      paste("GLM iterative fitting convergence:", fit$converged %||% "unavailable"),
      fit$iter %||% NA_integer_, "glm$converged"
    ))
  }
  if (inherits(fit, "glmnet")) {
    code <- fit$jerr
    if (length(code) != 1L || is.na(code)) {
      return(optimization_record())
    }
    return(optimization_record(
      if (code == 0L) "converged" else "partial", code,
      if (code == 0L) {
        "The glmnet path completed without a reported solver error."
      } else {
        "glmnet reported a solver issue or partial path; this policy does not certify the selected path point."
      },
      source = "glmnet$jerr"
    ))
  }
  if (inherits(fit, c("lm", "rpart", "ranger", "earth", "train.kknn"))) {
    return(optimization_record("not_applicable", message = paste(
      "No iterative optimizer convergence claim is made for this fitting procedure."
    ), source = paste(class(fit), collapse = "/")))
  }
  optimization_record()
}

tuning_learned_settings <- function(model) {
  neural <- inherits(model, "autoxplain_tuned_nnet")
  wrapped <- inherits(model, "autoxplain_fitted_model")
  fit <- if (neural) model$model else if (wrapped) model$fit else model
  if (neural) {
    return(list(
      encoded_inputs = fit$n[[1L]], fitted_weights = length(fit$wts),
      maxit = 500L, convergence = fit$convergence
    ))
  }
  if (inherits(fit, "glmnet")) {
    return(list(
      lambda = model$fit_details$lambda, lambda_index = model$fit_details$lambda_index,
      path_length = length(fit$lambda), requested_path_length = 80L,
      lambda_min_ratio = 0.001, standardized_inputs = TRUE
    ))
  }
  if (inherits(fit, "gam")) {
    return(list(
      effective_degrees_of_freedom = sum(fit$edf),
      smoothing_parameters = fit$sp, method = fit$method
    ))
  }
  if (inherits(fit, "svm")) {
    return(list(
      gamma = fit$gamma,
      encoded_inputs = length(model$blueprint$columns),
      support_vectors = fit$tot.nSV
    ))
  }
  if (inherits(fit, "rpart")) {
    return(list(
      terminal_leaves = sum(fit$frame$var == "<leaf>"),
      fitted_depth = max(floor(log2(as.numeric(rownames(fit$frame)))))
    ))
  }
  if (inherits(fit, "multinom")) {
    requested_maxit <- fit$call$maxit
    explicit <- is.numeric(requested_maxit) && length(requested_maxit) == 1L
    return(list(
      coefficients = sum(is.finite(stats::coef(fit))), rank = fit$rank %||% NULL,
      maxit = if (explicit) as.integer(requested_maxit) else as.integer(formals(nnet::nnet.default)$maxit),
      maxit_source = if (explicit) "explicit fitted call" else "nnet backend default at fit time",
      weight_decay = fit$decay, convergence = fit$convergence
    ))
  }
  if (inherits(fit, "lm")) {
    return(list(coefficients = sum(is.finite(stats::coef(fit))), rank = fit$rank %||% NULL))
  }
  list()
}

tuning_policy_sources <- function() {
  data.frame(
    topic = c(
      "Selection bias and holdout separation", "Fold variability", "Search design baseline",
      "Regularization and lambda paths", "Neural optimization", "Tree controls"
    ),
    url = c(
      "https://jmlr.org/papers/v11/cawley10a.html",
      "https://www.jmlr.org/papers/v5/grandvalet04a.html",
      "https://www.jmlr.org/papers/v13/bergstra12a.html",
      "https://glmnet.stanford.edu/articles/glmnet.html",
      "https://stat.ethz.ch/R-manual/R-devel/library/nnet/html/nnet.html",
      "https://stat.ethz.ch/R-manual/R-devel/library/rpart/html/rpart.control.html"
    ),
    stringsAsFactors = FALSE
  )
}

tuning_family_rationale <- function(family) {
  reasons <- c(
    linear = "One unpenalized reference fit; there is no parameter grid for this family.",
    regularized = paste(
      "Presets compare lasso, ridge and mixtures at several relative path positions.",
      "The actual lambda is learned inside each training fold."
    ),
    additive = paste(
      "Small basis limits bound coefficients per smooth; gamma and term selection",
      "offer stronger smoothing. REML learns smoothing penalties inside training folds."
    ),
    tree = paste(
      "Preset tuples move from shallow trees and larger minimum splits toward deeper trees.",
      "Minimum split sizes are fractions of outer-training row count, bounded below by four."
    ),
    forest = paste(
      "Early tuples vary inputs per split, node size, sampling fraction and split rule.",
      "The fixed 500-tree budget controls this search's computation; it is not an optimality claim."
    ),
    boosting = paste(
      "Presets jointly vary rounds, step size and tree capacity, then sampling controls.",
      "These are fixed-round fits, without an early-stopping search."
    ),
    neural = paste(
      "Small single-layer networks and several weight penalties bound the search cost.",
      "Numeric inputs and regression outcomes are standardized using each training fold."
    ),
    kernel = paste(
      "Early presets vary cost and radial width; regression epsilon uses standardized outcome units.",
      "Actual gamma divides the multiplier by the encoded input count."
    ),
    neighbors = paste(
      "Early presets compare local and smoother neighborhoods, distance powers and weights.",
      "Neighbor counts are clamped to the available training rows."
    ),
    mars = "Presets vary hinge-term count and allow either additive terms or pairwise interactions."
  )
  unname(reasons[[family]])
}

tuning_parameter_meaning <- function(family, parameter) {
  definitions <- list(
    linear = character(),
    regularized = c(
      alpha = "Penalty mixture: 0 ridge, 1 lasso; not a monotone flexibility scale.",
      path_fraction = "Position along the fitted lambda path; later usually means less shrinkage."
    ),
    additive = c(
      k = "Maximum smooth basis size, not fitted degrees of freedom.",
      gamma = "Larger values favor stronger smoothing.",
      select = "Adds penalties that can shrink whole smooth terms."
    ),
    tree = c(
      maxdepth = "Maximum permitted tree depth, not observed depth.",
      cp = "Larger thresholds discourage small-improvement splits.",
      minsplit = "Minimum training rows in a node before attempting a split."
    ),
    forest = c(
      num.trees = "Number of trees in the averaging ensemble.",
      mtry = "Predictors considered at each split.", min.node.size = "Minimum node-size control.",
      sample.fraction = "Fraction sampled for each tree.", splitrule = "How candidate splits are generated."
    ),
    boosting = c(
      nrounds = "Number of sequential boosting rounds.", eta = "Contribution of each new tree.",
      max_depth = "Maximum depth of each tree.", min_child_weight = "Minimum child Hessian weight.",
      subsample = "Fraction of rows sampled per round.", colsample_bytree = "Fraction of inputs per tree.",
      reg_alpha = "L1 leaf-weight penalty.", reg_lambda = "L2 leaf-weight penalty."
    ),
    neural = c(
      size = "Hidden units in one layer; more units permit more patterns.",
      decay = "Weight penalty; larger values discourage large weights."
    ),
    kernel = c(
      cost = "Penalty for errors or margin violations.",
      gamma_multiplier = "Radial-width multiplier divided by encoded input count.",
      epsilon = "Regression error-insensitivity width in standardized outcome units; fixed for classification."
    ),
    neighbors = c(
      k = "Number of neighbors; larger neighborhoods tend to smooth predictions.",
      distance = "Minkowski distance power.", kernel = "How neighboring observations are weighted."
    ),
    mars = c(degree = "Maximum interaction degree of hinge terms.", nprune = "Maximum number of retained terms.")
  )
  unname(definitions[[family]][[parameter]])
}

tuning_search_space <- function(grids, plan, n, p, task, custom_families) {
  family_rows <- lapply(names(grids), function(family) {
    scheduled <- sum(plan$family == family)
    data.frame(
      family = family, origin = if (family %in% custom_families) "user_grid" else "package_presets",
      available = length(grids[[family]]), scheduled = scheduled,
      untested = length(grids[[family]]) - scheduled,
      rationale = if (family %in% custom_families) {
        "These tuples were supplied by the caller; the package does not infer their scientific rationale."
      } else {
        tuning_family_rationale(family)
      }, stringsAsFactors = FALSE
    )
  })
  list(
    policy_id = "bounded-presets-v1", outer_training_rows = n, raw_predictors = p, task = task,
    allocation = "Round-robin ordered grid prefixes; explicit family budgets override the total allocation.",
    families = do.call(rbind, family_rows), grids = grids,
    limitation = paste(
      "Exact preset numbers are package engineering choices, not values justified as optimal by literature.",
      "This finite joint-configuration search does not identify causal effects of individual settings."
    ),
    sources = tuning_policy_sources()
  )
}

tuning_selection_record <- function(candidates, selected_id, rule, metric = NULL) {
  valid <- candidates$status == "ok" & is.finite(candidates$cv_score)
  best <- which(valid)[which.min(candidates$cv_score[valid])]
  threshold <- candidates$cv_score[[best]] + if (rule == "one_se") candidates$cv_se[[best]] else 0
  priority <- unique(candidates[c("family", "simplicity_rank", "complexity_definition")])
  priority <- priority[order(priority$simplicity_rank, priority$family), , drop = FALSE]
  list(
    rule = rule, best_configuration = candidates$configuration_id[[best]],
    best_score = candidates$cv_score[[best]], best_se = candidates$cv_se[[best]],
    threshold = threshold, selected_configuration = selected_id,
    eligible = if (rule == "one_se") {
      candidates$configuration_id[valid & candidates$cv_score <= threshold]
    } else {
      selected_id
    },
    family_priority = priority,
    score_method = if (identical(metric, "rmse")) {
      "RMSE is the square root of squared losses pooled across validation rows."
    } else {
      "CV loss is averaged across validation rows, weighting folds by their evaluated row counts."
    },
    variability_scope = paste(
      "Fold-score SE is a selection heuristic. Training folds overlap;",
      "it is not a confidence interval, equivalence test or population uncertainty."
    ),
    priority_scope = paste(
      "Family order is package or user preference. Capacity proxies omit some regularization controls;",
      "they do not establish a universal least-flexible model."
    )
  )
}

#' Inspect the evidence behind a tuning decision
#'
#' Returns aggregate search, candidate, fold and selection records for reports.
#' No models are fitted or rescored. Source row identities and per-case predictions
#' are omitted; feature names and diagnostic messages may remain.
#' @param result An [autoxplain()] result.
#' @return A versioned list of tuning evidence, or an explicit not-run record.
#' @export
tuning_evidence <- function(result) {
  if (!inherits(result, "autoxplain_result")) stop("`result` must be returned by `autoxplain()`.", call. = FALSE)
  tuning <- result$tuning
  if (!inherits(tuning, "autoxplain_tuning")) {
    return(list(schema_version = 1L, status = "not_run", reason = "No local training-only tuning record is retained."))
  }
  candidates <- tuning$candidates
  # Dollar access partially matches selection_rule in older saved objects.
  selection <- tuning[["selection", exact = TRUE]]
  if (!is.list(selection) || !all(c("eligible", "best_configuration", "threshold") %in% names(selection))) {
    return(list(
      schema_version = 1L, status = "unavailable", task = result$task, metric = tuning$metric,
      reason = paste(
        "The exact training selection record is unavailable for this saved run.",
        "Its original threshold decision is not reconstructed from current defaults or sorted candidate scores.",
        "Retained candidate and fold evidence remain available in tuning_results(result)."
      ),
      candidates = candidates,
      selection = list(status = "not_recorded"),
      final_configuration = tuning[["final_configuration", exact = TRUE]]
    ))
  }
  candidates$within_threshold <- candidates$configuration_id %in% selection$eligible
  candidates$lowest_cv <- candidates$configuration_id == selection$best_configuration
  candidates$final_fit <- candidates$configuration_id == tuning$final_configuration
  families <- lapply(unique(candidates$family), function(family) {
    rows <- candidates[candidates$family == family, , drop = FALSE]
    valid <- rows$status == "ok" & is.finite(rows$cv_score)
    best <- if (any(valid)) rows$configuration_id[which(valid)[which.min(rows$cv_score[valid])]] else NA_character_
    retained <- rows$configuration_id[!is.na(rows$retained_model_id %||% rep(NA_character_, nrow(rows)))]
    data.frame(
      family = family, best_cv = best, retained = paste(retained, collapse = ", "),
      selection_role = if (any(rows$selected)) "global policy selection" else "lowest family CV, with refit fallback",
      tested = nrow(rows), valid = sum(valid), failed = sum(!valid), stringsAsFactors = FALSE
    )
  })
  folds <- tuning$fold_scores
  columns <- intersect(c(
    "configuration_id", "fold", "score", "training_rows", "validation_rows",
    "validation_rows_requested", "validation_rows_omitted", "fit_seed",
    "optimization_status", "optimization_message", "warning", "error",
    "requested_parameters", "effective_parameters", "learned", "elapsed_ms"
  ), names(folds))
  list(
    schema_version = 1L, status = "computed", task = result$task, metric = tuning$metric,
    folds_used = tuning$folds_used, scope = tuning$scope_note,
    search_space = tuning$search_space %||% list(
      status = "not_recorded",
      limitation = paste(
        "This saved run predates recorded search rationale;",
        "do not reconstruct its policy from current defaults."
      )
    ),
    selection = selection, final_configuration = tuning$final_configuration,
    candidates = candidates, folds = folds[columns], families = do.call(rbind, families),
    boundaries = tuning_boundary_evidence(tuning), refit = tuning$refit,
    family_failures = list(
      resampling = unique(tuning$families_resampling_failed %||% tuning$refit$families_resampling_failed),
      refit = unique(tuning$refit$families_refit_failed)
    ),
    control = tuning$control, sources = tuning$search_space$sources %||% tuning_policy_sources()
  )
}

tuning_boundary_evidence <- function(tuning) {
  candidates <- tuning$candidates
  plan <- tuning$plan
  rows <- list()
  for (family in unique(candidates$family)) {
    valid <- candidates$family == family & candidates$status == "ok" & is.finite(candidates$cv_score)
    if (!any(valid)) next
    winner <- candidates$configuration_id[which(valid)[which.min(candidates$cv_score[valid])]]
    configurations <- plan$parameters[plan$family == family]
    selected <- plan$parameters[[match(winner, plan$configuration_id)]]
    for (parameter in names(selected)) {
      values <- lapply(configurations, `[[`, parameter)
      if (!all(vapply(values, function(x) is.numeric(x) && length(x) == 1L, logical(1)))) next
      values <- sort(unique(unlist(values)))
      value <- selected[[parameter]]
      position <- if (length(values) == 1L) {
        "fixed"
      } else if (value == min(values)) {
        "lower edge"
      } else {
        if (value == max(values)) "upper edge" else "interior"
      }
      rows[[length(rows) + 1L]] <- data.frame(
        family = family, configuration_id = winner, parameter = parameter,
        selected_value = value, tested_min = min(values), tested_max = max(values),
        distinct_values = length(values), position = position,
        interpretation = if (position == "fixed") {
          "This control was not varied."
        } else {
          paste(
            "Positions use scheduled values, including failed attempts.",
            "Joint tuples do not establish that extending one control would improve performance."
          )
        }, stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) {
    return(data.frame())
  }
  do.call(rbind, rows)
}
