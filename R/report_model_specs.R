# Keep fitted values separate from training controls: a maximum depth is not
# the depth of the retained tree, and a search-grid value is not a coefficient.
model_spec_value <- function(value) {
  if (is.null(value)) {
    return("Not recorded")
  }
  if (!length(value)) {
    return("None")
  }
  if (is.list(value)) {
    return(paste(paste(names(value), vapply(value, model_spec_value, character(1)), sep = " = "), collapse = "; "))
  }
  text <- if (is.numeric(value)) format(signif(value, 8), trim = TRUE, scientific = FALSE) else as.character(value)
  if (!is.null(names(value))) text <- paste(names(value), text, sep = " = ")
  paste(text, collapse = ", ")
}

model_specification <- function(result, id) {
  model <- result$models[[id]]
  if (is.null(model)) stop("No retained model with ID: ", id, call. = FALSE)
  intercept_baseline <- identical(id, "simple_baseline") &&
    !identical(result$provenance$workflow, "supplied-model evaluation")
  tuning <- attr(model, "autoxplain_tuning_fit")
  wrapped <- inherits(model, "autoxplain_fitted_model")
  neural <- inherits(model, "autoxplain_tuned_nnet")
  h2o <- inherits(model, "H2OModel")
  fit <- if (wrapped) model$fit else if (neural) model$model else model
  parameters <- if (h2o) model@allparameters else base_model_hyperparameters(model)
  parameters <- parameters[setdiff(names(parameters), c(
    "formula", "backend", "backend_version", "hidden_units", "weight_decay", "numeric_scaling"
  ))]
  leading <- intersect(c("maxdepth", "cp", "minsplit", "minbucket", "size", "decay", "maxit"), names(parameters))
  parameters <- parameters[c(leading, setdiff(names(parameters), leading))]
  engine <- if (h2o) {
    "h2o"
  } else if (wrapped) {
    model$backend
  } else if (inherits(fit, "rpart")) {
    "rpart"
  } else if (inherits(fit, "nnet")) {
    "nnet"
  } else if (inherits(fit, "lm")) {
    "stats"
  } else {
    "Not recorded"
  }
  if (inherits(fit, "lm") && !inherits(fit, "glm")) parameters$method <- "qr"
  if (intercept_baseline) parameters$inputs <- "none (intercept only)"
  formula <- if (neural) model$terms else tryCatch(stats::formula(fit), error = function(e) NULL)
  formula <- if (is.null(formula)) NULL else paste(trimws(deparse(formula)), collapse = " ")
  learned <- list()
  coefficients <- NULL
  if (inherits(fit, "lm") || inherits(fit, "multinom")) {
    coefficients <- stats::coef(fit)
    learned <- list(`Fitted coefficients` = sum(!is.na(coefficients)))
    if (inherits(fit, "lm")) learned$`Residual degrees of freedom` <- fit$df.residual
    if (inherits(fit, "glm")) learned$Converged <- fit$converged
    if (inherits(fit, "multinom")) learned$`Convergence code (0 = converged)` <- fit$convergence
  }
  if (inherits(fit, "rpart")) {
    learned <- list(
      `Terminal leaves` = sum(fit$frame$var == "<leaf>"),
      `Fitted depth` = max(floor(log2(as.numeric(rownames(fit$frame))))),
      `Splitting inputs` = unique(fit$frame$var[fit$frame$var != "<leaf>"])
    )
  }
  if (neural) {
    learned <- list(
      `Encoded inputs` = fit$n[1], `Hidden units` = fit$n[2], Outputs = fit$n[3],
      `Fitted weights` = length(fit$wts), `Convergence code (0 = converged)` = fit$convergence
    )
  }
  if (wrapped) {
    learned <- switch(model$backend,
      ranger = list(Trees = fit$num.trees, `Inputs per split` = fit$mtry),
      glmnet = list(
        `Selected lambda` = model$fit_details$lambda,
        `Nonzero coefficients` = fit$df[model$fit_details$lambda_index]
      ),
      mgcv = c(learned, list(
        `Smooth basis dimensions` = stats::setNames(
          vapply(fit$smooth, function(smooth) smooth$bs.dim, numeric(1)),
          vapply(fit$smooth, function(smooth) {
            feature <- names(model$fit_details$feature_map)[
              match(smooth$term, model$fit_details$feature_map)
            ]
            if (length(feature) == 1L && !is.na(feature)) feature else smooth$label
          }, character(1))
        ),
        `Total effective degrees of freedom` = sum(fit$edf)
      )),
      e1071 = list(`Support vectors` = fit$tot.nSV),
      earth = list(`Retained terms` = length(fit$selected.terms)),
      kknn = list(`Training rows retained` = model$fit_details$training_rows),
      list()
    )
  }
  summary <- if (inherits(fit, "rpart")) {
    paste0(
      "Depth \u2264", parameters$maxdepth, " \u00b7 split \u2265", parameters$minsplit,
      " rows \u00b7 cp ", model_spec_value(parameters$cp)
    )
  } else if (neural) {
    paste0(
      parameters$size, if (parameters$size == 1) " hidden unit" else " hidden units",
      " \u00b7 decay ", model_spec_value(parameters$decay),
      " \u00b7 ", length(fit$wts), " weights"
    )
  } else if (wrapped) {
    paste0(
      learner_definition(model$family)$describe(parameters),
      if (model$backend == "glmnet") paste0("; lambda = ", model_spec_value(model$fit_details$lambda))
    )
  } else if (!is.null(coefficients)) {
    if (intercept_baseline) {
      if (result$task == "regression") {
        paste0("Constant prediction = ", model_spec_value(unname(coefficients[1])))
      } else {
        "Intercept only \u00b7 training class proportions"
      }
    } else {
      paste0(
        sum(!is.na(coefficients)),
        if (sum(!is.na(coefficients)) == 1L) " fitted coefficient \u00b7 " else " fitted coefficients \u00b7 ",
        if (inherits(fit, "glm")) {
          paste(fit$family$link, "link")
        } else if (inherits(fit, "multinom")) {
          "multinomial logit"
        } else {
          "ordinary least squares"
        }
      )
    }
  } else {
    compact <- compact_hyperparameters(parameters, if (h2o) model@algorithm else engine)
    entries <- paste(
      names(utils::head(compact, 3)),
      vapply(utils::head(compact, 3), model_spec_value, character(1)), sep = " = "
    )
    paste(entries, collapse = " \u00b7 ")
  }
  if (!nzchar(summary)) summary <- "Settings not recorded"
  list(
    id = id, label = explorer_label(result, id), summary = summary, engine = engine,
    engine_version = if (wrapped) model$package_version else tuning$backend_version,
    parameters = parameters, formula = formula, learned = learned,
    coefficients = coefficients, tuning = tuning,
    blueprint = if (neural || wrapped) model$blueprint else NULL
  )
}

explorer_spec_link <- function(spec) {
  paste0(
    '<a class="spec-link" data-open-spec="', html_escape(spec$id), '" href="#spec-',
    report_anchor(spec$id), '" aria-label="Model details for ', html_escape(spec$label), '">Model details</a>'
  )
}

explorer_model_identity <- function(result, id) {
  spec <- model_specification(result, id)
  paste0(
    '<div class="model-identity"><span class="model-settings">', html_escape(spec$summary),
    "</span>", explorer_spec_link(spec), "</div>"
  )
}

model_spec_table <- function(values, caption) {
  if (!length(values)) {
    return("<p>Not recorded for this fit.</p>")
  }
  html_table(data.frame(
    Setting = names(values), Value = vapply(values, model_spec_value, character(1)), row.names = NULL
  ), caption = caption)
}

model_spec_settings_help <- function(spec) {
  detail <- switch(spec$engine,
    rpart = paste(
      "maxdepth caps the number of splits along a path; minsplit is the minimum rows considered for a split;",
      "minbucket is the minimum rows in a leaf. cp sets a minimum relative improvement for splitting.",
      "The fitted depth and leaf count above describe the resulting tree."
    ),
    nnet = if (!is.null(spec$parameters$size)) {
      "size is the number of hidden units; decay penalizes large weights; maxit caps optimization iterations."
    } else {
      "decay penalizes large coefficients. Coefficients describe log odds relative to the reference class."
    },
    mgcv = paste(
      "smooth_k records the basis dimension used for each numeric input; requested k can be reduced",
      "when an input has few distinct values. These dimensions limit flexibility; effective degrees of freedom",
      "describe the fitted smooths after penalization. gamma and select control smoothing and shrinkage."
    ),
    stats = if (identical(spec$parameters$family, "binomial")) {
      "family defines the outcome distribution; link connects the linear predictor to probability."
    } else {
      "Ordinary least squares estimates coefficients by minimizing the sum of squared training errors."
    },
    "The parameter names match the fitted R engine. Preprocessing and requested search settings appear below."
  )
  paste("Training controls and learned parameter values are different.", detail)
}

explorer_model_spec_details <- function(result, id) {
  spec <- model_specification(result, id)
  model <- result$models[[id]]
  coefs <- spec$coefficients
  coefficient_html <- if (is.null(coefs)) {
    ""
  } else {
    table <- if (is.matrix(coefs)) {
      data.frame(
        Class = rep(rownames(coefs), each = ncol(coefs)),
        Term = rep(colnames(coefs), times = nrow(coefs)), Estimate = as.vector(t(coefs))
      )
    } else {
      data.frame(Term = names(coefs), Estimate = unname(coefs))
    }
    table_html <- html_table(table, 6, caption = "Estimates on the model scale; these are fitted associations")
    paste0("<details><summary>Fitted coefficients</summary>", table_html, "</details>")
  }
  tree_html <- if (inherits(model, "rpart")) {
    paste0(
      "<details><summary>Fitted tree rules</summary><pre><code>",
      html_escape(paste(sub("[[:space:]]+$", "", utils::capture.output(print(model))), collapse = "\n")),
      "</code></pre></details>"
    )
  } else {
    ""
  }
  recipe <- result$preprocessing_metadata$training_data$recipe
  preprocessing <- c(
    recipe[c("missing_value_strategy", "novel_level_strategy", "removed_columns")],
    spec$blueprint[c("categorical_encoding", "centered", "scaled", "columns")]
  )
  if (is.null(spec$blueprint)) {
    native <- if (inherits(model, "autoxplain_fitted_model")) model$fit else model
    levels <- if (inherits(model, "H2OModel")) {
      recipe$factor_levels
    } else {
      (if (is.list(native)) native[["xlevels"]] else NULL) %||%
        attr(native, "xlevels") %||% recipe$factor_levels
    }
    preprocessing <- c(preprocessing, list(factor_levels = levels))
    if (is.list(native) && !is.null(native$contrasts)) {
      preprocessing$contrasts <- native$contrasts
    }
    if (inherits(model, "autoxplain_fitted_model")) {
      preprocessing$feature_map <- model$fit_details$feature_map
    }
  }
  tuning <- spec$tuning
  selection <- list(
    `Model ID` = id, Engine = spec$engine, `Recorded engine version` = spec$engine_version,
    `Training rows supplied` = if (is.null(result$training_data)) "Not supplied" else nrow(result$training_data)
  )
  if (!is.null(tuning)) {
    candidates <- result$tuning$candidates
    row <- candidates[candidates$configuration_id == tuning$configuration_id, , drop = FALSE]
    role <- row$refit_role
    if (!length(role) || is.na(role[[1L]])) {
      role <- if (identical(id, result$provenance$primary_model_id)) "selected" else "alternative"
    }
    selection_rule <- if (identical(as.character(role[[1L]]), "alternative")) {
      "Lowest training-CV loss within this family; refit fallback recorded if needed"
    } else if (identical(as.character(role[[1L]]), "fallback")) {
      paste0("Refit fallback after the ", result$tuning$selection_rule, " selection could not be fitted")
    } else {
      result$tuning$selection_rule
    }
    selection <- c(selection, list(
      Configuration = tuning$configuration_id, `Fit seed` = tuning$fit_seed,
      `Training CV folds` = result$tuning$folds_used, `CV metric` = result$tuning$metric,
      `Training CV score` = row$cv_score, `Selection rule` = selection_rule,
      `Optimization status` = row$refit_optimization_status %||% "Not recorded",
      `Optimization note` = row$refit_optimization_message %||% ""
    ))
  }
  warning <- result$model_diagnostics$fit_warning[match(id, result$model_diagnostics$model_id)]
  paste0(
    '<details class="model-spec" id="spec-', report_anchor(id), '"><summary>',
    html_escape(paste(spec$label, spec$summary, sep = " \u00b7 ")), "</summary>",
    '<div class="model-spec-content" data-spec-label="', html_escape(spec$label), '">',
    '<p class="model-settings">', html_escape(spec$summary), "</p>",
    if (length(warning) && !is.na(warning) && nzchar(warning)) {
      paste0('<p class="evaluation-note">Fit warning: ', html_escape(warning), "</p>")
    },
    if (!is.null(spec$formula)) {
      paste0(
        "<h3>Model formula</h3><pre><code>",
        html_escape(spec$formula), "</code></pre>"
      )
    },
    "<h3>What was fitted</h3>", model_spec_table(spec$learned, "Measured structure of the retained fit"),
    coefficient_html, tree_html,
    if (inherits(model, "H2OModel") && !is.null(model@model$model_summary)) {
      html_table(as.data.frame(model@model$model_summary), caption = "Fitted H2O model summary")
    },
    "<h3>Training settings</h3><details><summary>What do these settings mean?</summary><p>",
    html_escape(model_spec_settings_help(spec)), "</p></details>",
    model_spec_table(spec$parameters, "Recorded settings, using R parameter names"),
    "<details><summary>Selection and reproducibility</summary>", model_spec_table(selection, "Fit provenance"),
    if (length(tuning$requested_parameters)) model_spec_table(tuning$requested_parameters, "Requested search settings"),
    "</details><details><summary>Preprocessing and encoded inputs</summary>",
    model_spec_table(preprocessing, "Training-derived preprocessing"),
    model_spec_table(recipe$imputations, "Training fill values for missing inputs"),
    if (length(spec$blueprint$center)) model_spec_table(as.list(spec$blueprint$center), "Encoded-column centers"),
    if (length(spec$blueprint$scale)) model_spec_table(as.list(spec$blueprint$scale), "Encoded-column scales"),
    "</details><h3>Inspect in R</h3><pre><code>", html_escape(paste0(
      "model <- result$models[[", deparse(id), "]]\n",
      "extract_model_characteristics(result)[[", deparse(id), "]]\n",
      if (inherits(model, "autoxplain_tuned_nnet")) "model$model$wts" else "str(model, max.level = 1)"
    )), "</code></pre></div></details>"
  )
}
