# A smaller forest approximates configuration ranking while retaining every
# requested validation fold and all rows in each fold's training partition.
# Final tree counts are planned before configuration keys and fitting seeds.
# The independent final evaluation scores the actual planned final forest.
forest_validation_budget_policy <- function(n, p, search, automatic = TRUE) {
  n <- assert_count(n, "n", minimum = 2L)
  p <- assert_count(p, "p")
  adaptive <- identical(search, "adaptive")
  work <- as.double(n) * p
  active <- isTRUE(automatic) && adaptive && work >= 1e6
  final_active <- active && work >= 4e6
  validation_trees <- if (!active) 500L else if (work >= 4e6) 128L else 256L
  final_trees <- if (final_active) 256L else 500L
  list(
    validation_budget_policy_id = "forest-tree-budget-v3",
    validation_budget_active = active,
    final_budget_active = final_active,
    validation_budget_work = work,
    screening_num_trees = if (adaptive) 128L else NA_integer_,
    validation_num_trees = validation_trees,
    final_num_trees = final_trees,
    validation_budget_reason = if (active) {
      paste(
        "For automatic adaptive searches with",
        if (work >= 4e6) "at least four million" else "one million to fewer than four million",
        "training rows times predictors,",
        paste0(validation_trees, "-tree"),
        "forests approximate configuration ranking before the planned",
        paste0(final_trees, "-tree"), "final refit.",
        if (final_active) paste(
          "The automatic final tree count is fixed from outer-training input size before fitting.",
          "Fewer trees reduce ensemble averaging and can change predictions and explanations;",
          "this computation policy is not a convergence test."
        ) else ""
      )
    } else {
      "Complete validation retains the requested forest tree count."
    },
    validation_budget_scope = if (active) {
      paste(
        "Every requested CV fold is retained; this tree budget does not sample or remove training rows.",
        "CV scores describe", paste0(validation_trees, "-tree"),
        "fits and do not measure the final", paste0(final_trees, "-tree"), "model exactly.",
        "Final refits retain native OOB diagnostics.",
        "The held-out evaluation measures the final model; it does not select this budget.",
        "Use grid search or an explicit grid to validate the full requested tree count."
      )
    } else {
      "Validation and final refits use the requested tree count."
    }
  )
}

apply_forest_validation_budget <- function(plan, policy = NULL) {
  plan$validation_num_trees <- rep(NA_integer_, nrow(plan))
  plan$forest_final_num_trees <- rep(NA_integer_, nrow(plan))
  plan$forest_validation_budget_policy_id <- rep(NA_character_, nrow(plan))
  if (isTRUE(policy$validation_budget_active)) {
    plan$validation_num_trees[plan$family == "forest"] <- policy$validation_num_trees
    plan$forest_final_num_trees[plan$family == "forest"] <- policy$final_num_trees
    plan$forest_validation_budget_policy_id[plan$family == "forest"] <- policy$validation_budget_policy_id
  }
  plan
}

forest_fit_budget_record <- function(configuration, fit_scope, requested, effective) {
  budget <- configuration$validation_num_trees
  if (is.null(budget) || length(budget) != 1L || is.na(budget)) return(NULL)
  final_trees <- configuration$forest_final_num_trees %||% 500L
  list(
    policy_id = configuration$forest_validation_budget_policy_id %||% NA_character_,
    scope = fit_scope,
    requested_num_trees = requested$num.trees,
    effective_num_trees = effective$num.trees,
    validation_num_trees = as.integer(budget),
    screening_num_trees = 128L,
    final_num_trees = as.integer(final_trees),
    scope_note = paste(
      "Screening uses 128 trees; complete CV uses", budget,
      "trees with all requested folds and fold-training rows.",
      "CV approximates configuration ranking. The full-training refit uses", final_trees, "trees,",
      "and held-out evaluation measures that final model."
    )
  )
}
