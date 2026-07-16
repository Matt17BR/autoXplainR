#' Inspect AutoXplainR learner families
#'
#' Returns the learner-family contract used by training-only tuning. Families
#' describe the kind of pattern a model can learn; backends identify the R
#' implementation. Keeping those concepts separate lets reports explain model
#' behavior without exposing beginners to backend-specific syntax.
#'
#' @param task Optional task filter: one of `"regression"`, `"binary"`, or
#'   `"multiclass"`.
#'
#' @return A data frame with learner capabilities, dependencies, and plain-
#'   language behavior notes.
#' @export
#'
#' @examples
#' learner_catalog()
#' learner_catalog("regression")
learner_catalog <- function(task = NULL) {
  if (!is.null(task)) {
    task <- match.arg(task, c("regression", "binary", "multiclass"))
  }
  registry <- autoxplain_learner_registry()
  if (!is.null(task)) {
    registry <- registry[vapply(registry, function(item) {
      task %in% item$tasks
    }, logical(1))]
  }
  rows <- lapply(registry, function(item) {
    dependency <- learner_dependency_status(item)
    data.frame(
      family = item$family,
      backend = item$backend,
      model = learner_model_label(item, task %||% "regression"),
      supported_tasks = paste(item$tasks, collapse = ", "),
      portfolios = paste(item$portfolios, collapse = ", "),
      dependency = item$package %||% "AutoXplainR core",
      minimum_version = item$minimum_version %||% NA_character_,
      available = dependency$available,
      dependency_status = dependency$status,
      installed_version = dependency$installed_version,
      status_note = dependency$reason,
      one_se_priority = item$simplicity_rank,
      complexity_proxy = item$complexity_label,
      nonlinearity = item$nonlinearity,
      interactions = item$interactions,
      scaling = item$scaling,
      strengths = item$strengths,
      cautions = item$cautions,
      stringsAsFactors = FALSE
    )
  })
  output <- do.call(rbind, rows)
  rownames(output) <- NULL
  structure(output, class = c("autoxplain_learner_catalog", "data.frame"))
}

#' @export
print.autoxplain_learner_catalog <- function(x, ...) {
  cat("<AutoXplainR learner catalog>\n")
  cat("  families:  ", nrow(x), "\n", sep = "")
  if ("available" %in% names(x)) {
    cat("  available: ", sum(x$available), "\n", sep = "")
  }
  columns <- intersect(
    c("family", "backend", "supported_tasks", "available", "nonlinearity", "interactions"),
    names(x)
  )
  print.data.frame(
    as.data.frame(x)[columns],
    row.names = FALSE,
    ...
  )
  invisible(x)
}

autoxplain_learner_registry <- function() {
  list(
    linear = list(
      family = "linear",
      backend = "stats/nnet",
      package = NULL,
      minimum_version = NULL,
      tasks = c("regression", "binary", "multiclass"),
      portfolios = c("core", "recommended", "extended"),
      labels = c(
        regression = "linear regression",
        binary = "logistic regression",
        multiclass = "multinomial logistic regression"
      ),
      model_id = "linear_model",
      simplicity_rank = 1L,
      complexity_label = "potential coefficient count",
      nonlinearity = "none unless encoded in features",
      interactions = "none unless specified in features",
      scaling = "usually helpful, not required here",
      strengths = "Transparent reference model with directly inspectable coefficients.",
      cautions = "Can miss curved effects and interactions; coefficients are associational.",
      grid = linear_learner_grid,
      fit = fit_linear_learner,
      describe = function(parameters) "default statistical fit",
      complexity = linear_learner_complexity
    ),
    regularized = list(
      family = "regularized",
      backend = "glmnet",
      package = "glmnet",
      minimum_version = "4.1",
      tasks = c("regression", "binary", "multiclass"),
      portfolios = c("recommended", "extended"),
      labels = c(
        regression = "regularized linear model",
        binary = "regularized logistic model",
        multiclass = "regularized multinomial model"
      ),
      model_id = "regularized_model",
      simplicity_rank = 2L,
      complexity_label = "within-family regularization-path flexibility",
      nonlinearity = "none unless encoded in features",
      interactions = "none unless specified in features",
      scaling = "standardized internally from training rows",
      strengths = "Shrinks unstable coefficients and can select a sparse set of predictors.",
      cautions = "Still additive and linear on the encoded scale; lambda and alpha affect conclusions.",
      grid = regularized_learner_grid,
      fit = fit_regularized_learner,
      describe = describe_regularized_parameters,
      complexity = regularized_learner_complexity
    ),
    additive = list(
      family = "additive",
      backend = "mgcv",
      package = "mgcv",
      minimum_version = "1.8",
      tasks = c("regression", "binary"),
      portfolios = c("recommended", "extended"),
      labels = c(
        regression = "generalized additive model",
        binary = "generalized additive logistic model"
      ),
      model_id = "additive_model",
      simplicity_rank = 3L,
      complexity_label = "within-family smooth-basis capacity",
      nonlinearity = "smooth, one predictor at a time",
      interactions = "not in the automatic default formula",
      scaling = "not required",
      strengths = "Shows smooth nonlinear effects while preserving an additive structure.",
      cautions = "Automatic smooths need enough distinct values and can miss interactions.",
      grid = additive_learner_grid,
      fit = fit_additive_learner,
      describe = describe_additive_parameters,
      complexity = additive_learner_complexity
    ),
    tree = list(
      family = "tree",
      backend = "rpart",
      package = "rpart",
      minimum_version = NULL,
      tasks = c("regression", "binary", "multiclass"),
      portfolios = c("core", "recommended", "extended"),
      labels = c(
        regression = "decision tree",
        binary = "decision tree",
        multiclass = "decision tree"
      ),
      model_id = "tree_model",
      simplicity_rank = 4L,
      complexity_label = "within-family potential leaf count",
      nonlinearity = "stepwise",
      interactions = "automatic along tree paths",
      scaling = "not required",
      strengths = "Readable rules and automatic nonlinear splits and interactions.",
      cautions = "Single trees can be unstable and less accurate than ensembles.",
      grid = tree_learner_grid,
      fit = fit_tree_learner,
      describe = describe_tree_parameters,
      complexity = tree_learner_complexity
    ),
    forest = list(
      family = "forest",
      backend = "ranger",
      package = "ranger",
      minimum_version = "0.16",
      tasks = c("regression", "binary", "multiclass"),
      portfolios = c("recommended", "extended"),
      labels = c(
        regression = "random forest",
        binary = "probability random forest",
        multiclass = "probability random forest"
      ),
      model_id = "forest_model",
      simplicity_rank = 7L,
      complexity_label = "within-family tree and node-capacity proxy",
      nonlinearity = "flexible threshold ensembles",
      interactions = "automatic across tree paths",
      scaling = "not required",
      strengths = "Averages many decorrelated trees for strong, stable tabular predictions.",
      cautions = "Individual rules are obscured; correlated predictors can share importance.",
      grid = forest_learner_grid,
      fit = fit_forest_learner,
      describe = describe_forest_parameters,
      complexity = forest_learner_complexity
    ),
    boosting = list(
      family = "boosting",
      backend = "xgboost",
      package = "xgboost",
      minimum_version = "2.0",
      current_cran_r_minimum = "4.3.0",
      tasks = c("regression", "binary", "multiclass"),
      portfolios = c("recommended", "extended"),
      labels = c(
        regression = "XGBoost gradient-boosted trees",
        binary = "XGBoost gradient-boosted trees",
        multiclass = "XGBoost gradient-boosted trees"
      ),
      model_id = "boosting_model",
      simplicity_rank = 10L,
      complexity_label = "within-family boosting-round and tree-capacity proxy",
      nonlinearity = "sequential threshold ensemble",
      interactions = "automatic across boosted tree paths",
      scaling = "not required after stable matrix encoding",
      strengths = "Corrects residual errors sequentially and often excels on structured data.",
      cautions = "Tuning-sensitive, less transparent, and costly to compile on some systems.",
      grid = boosting_learner_grid,
      fit = fit_boosting_learner,
      describe = describe_boosting_parameters,
      complexity = boosting_learner_complexity
    ),
    neural = list(
      family = "neural",
      backend = "nnet",
      package = "nnet",
      minimum_version = NULL,
      tasks = c("regression", "binary", "multiclass"),
      portfolios = c("core", "extended"),
      labels = c(
        regression = "neural network",
        binary = "neural network",
        multiclass = "neural network"
      ),
      model_id = "neural_model",
      simplicity_rank = 9L,
      complexity_label = "within-family connection count",
      nonlinearity = "smooth and flexible",
      interactions = "automatic through hidden units",
      scaling = "performed inside the adapter",
      strengths = "Can learn smooth nonlinear combinations of many predictors.",
      cautions = "Less transparent, seed-sensitive, and easy to overfit on small data.",
      grid = neural_learner_grid,
      fit = fit_neural_learner,
      describe = describe_neural_parameters,
      complexity = neural_learner_complexity
    ),
    kernel = list(
      family = "kernel",
      backend = "e1071",
      package = "e1071",
      minimum_version = "1.7",
      tasks = c("regression", "binary", "multiclass"),
      portfolios = "extended",
      labels = c(
        regression = "radial support vector regression",
        binary = "radial support vector machine",
        multiclass = "radial support vector machine"
      ),
      model_id = "kernel_model",
      simplicity_rank = 8L,
      complexity_label = "within-family kernel-flexibility proxy",
      nonlinearity = "smooth similarity surface",
      interactions = "implicit through distances in encoded feature space",
      scaling = "performed from training rows",
      strengths = "Can separate curved boundaries using similarities to support vectors.",
      cautions = "Scaling and tuning matter; calibrated probabilities add another fitted layer.",
      grid = kernel_learner_grid,
      fit = fit_kernel_learner,
      describe = describe_kernel_parameters,
      complexity = kernel_learner_complexity
    ),
    neighbors = list(
      family = "neighbors",
      backend = "kknn",
      package = "kknn",
      minimum_version = "1.4",
      tasks = c("regression", "binary", "multiclass"),
      portfolios = "extended",
      labels = c(
        regression = "weighted nearest-neighbor regression",
        binary = "weighted nearest-neighbor classifier",
        multiclass = "weighted nearest-neighbor classifier"
      ),
      model_id = "neighbors_model",
      simplicity_rank = 6L,
      complexity_label = "within-family local-region flexibility proxy",
      nonlinearity = "local neighborhoods",
      interactions = "implicit through multivariable distance",
      scaling = "performed from training rows",
      strengths = "Explains a prediction through similar observed training examples.",
      cautions = "Stores training rows and degrades with irrelevant dimensions or large data.",
      grid = neighbors_learner_grid,
      fit = fit_neighbors_learner,
      describe = describe_neighbors_parameters,
      complexity = neighbors_learner_complexity
    ),
    mars = list(
      family = "mars",
      backend = "earth",
      package = "earth",
      minimum_version = "5.3",
      tasks = c("regression", "binary"),
      portfolios = "extended",
      labels = c(
        regression = "MARS piecewise-linear model",
        binary = "MARS logistic model"
      ),
      model_id = "mars_model",
      simplicity_rank = 5L,
      complexity_label = "within-family hinge-term capacity",
      nonlinearity = "piecewise-linear hinge functions",
      interactions = "optional and bounded by degree",
      scaling = "not required after stable matrix encoding",
      strengths = "Exposes hinge locations and compact nonlinear, piecewise-linear effects.",
      cautions = "Term selection invalidates ordinary post-selection coefficient inference.",
      grid = mars_learner_grid,
      fit = fit_mars_learner,
      describe = describe_mars_parameters,
      complexity = mars_learner_complexity
    )
  )
}

learner_definition <- function(family) {
  registry <- autoxplain_learner_registry()
  if (!is.character(family) || length(family) != 1L || is.na(family) ||
        !family %in% names(registry)) {
    stop(
      "Unknown learner family `", paste(family, collapse = ""), "`. Available families: ",
      paste(names(registry), collapse = ", "), ".",
      call. = FALSE
    )
  }
  registry[[family]]
}

learner_is_available <- function(definition) {
  learner_dependency_status(definition)$available
}

learner_dependency_status <- function(definition) {
  package <- definition$package %||% NULL
  if (is.null(package)) {
    return(list(
      available = TRUE,
      installed = TRUE,
      status = "core",
      installed_version = NA_character_,
      reason = "Included with R or AutoXplainR.",
      current_r_blocked = FALSE
    ))
  }
  installed <- requireNamespace(package, quietly = TRUE)
  installed_version <- if (installed) {
    as.character(utils::packageVersion(package))
  } else {
    NA_character_
  }
  minimum <- definition$minimum_version %||% NULL
  current_r_minimum <- definition$current_cran_r_minimum %||% NULL
  current_r_blocked <- !installed && !is.null(current_r_minimum) &&
    getRversion() < numeric_version(current_r_minimum)
  if (!installed) {
    reason <- paste0("Package `", package, "` is not installed.")
    if (current_r_blocked) {
      reason <- paste0(
        reason, " Its current CRAN release requires R >= ", current_r_minimum,
        "; this session uses R ", as.character(getRversion()), "."
      )
    }
    return(list(
      available = FALSE,
      installed = FALSE,
      status = if (current_r_blocked) "incompatible_r" else "missing",
      installed_version = installed_version,
      reason = reason,
      current_r_blocked = current_r_blocked
    ))
  }
  if (!is.null(minimum) && utils::packageVersion(package) < numeric_version(minimum)) {
    return(list(
      available = FALSE,
      installed = TRUE,
      status = "outdated",
      installed_version = installed_version,
      reason = paste0(
        "Package `", package, "` ", installed_version,
        " is installed; AutoXplainR requires >= ", minimum, "."
      ),
      current_r_blocked = FALSE
    ))
  }
  list(
    available = TRUE,
    installed = TRUE,
    status = "ready",
    installed_version = installed_version,
    reason = paste0("Package `", package, "` ", installed_version, " is available."),
    current_r_blocked = FALSE
  )
}

learner_model_label <- function(definition, task) {
  label <- unname(definition$labels[task])
  if (length(label) != 1L || is.na(label) || !nzchar(label)) definition$family else label
}

portfolio_learner_families <- function(portfolio, task) {
  portfolio <- match.arg(portfolio, c("recommended", "core", "extended"))
  registry <- autoxplain_learner_registry()
  names(registry)[vapply(registry, function(item) {
    portfolio %in% item$portfolios && task %in% item$tasks
  }, logical(1))]
}

resolve_tuning_learners <- function(portfolio, learners, task) {
  explicit <- !is.null(learners)
  requested <- if (explicit) learners else portfolio_learner_families(portfolio, task)
  if (!is.character(requested) || !length(requested) || anyNA(requested) ||
        any(!nzchar(requested)) || anyDuplicated(requested)) {
    stop("`learners` must be NULL or a unique, non-empty character vector.", call. = FALSE)
  }
  registry <- autoxplain_learner_registry()
  unknown <- setdiff(requested, names(registry))
  if (length(unknown)) {
    stop(
      "Unknown learner families: ", paste(unknown, collapse = ", "),
      ". Use `learner_catalog()` to inspect supported families.",
      call. = FALSE
    )
  }
  unsupported <- requested[!vapply(requested, function(family) {
    task %in% registry[[family]]$tasks
  }, logical(1))]
  if (length(unsupported)) {
    stop(
      "Learner families do not support the `", task, "` task: ",
      paste(unsupported, collapse = ", "), ".",
      call. = FALSE
    )
  }
  unavailable <- requested[!vapply(requested, function(family) {
    learner_is_available(registry[[family]])
  }, logical(1))]
  if (length(unavailable)) {
    statuses <- lapply(unavailable, function(family) {
      learner_dependency_status(registry[[family]])
    })
    names(statuses) <- unavailable
    packages <- unique(vapply(unavailable, function(family) {
      registry[[family]]$package %||% family
    }, character(1)))
    command <- paste0(
      "install.packages(c(",
      paste(sprintf("\"%s\"", packages), collapse = ", "),
      "))"
    )
    fallback <- if (!explicit) {
      " Or use `portfolio = \"core\"` for the dependency-light tournament."
    } else {
      ""
    }
    details <- paste(vapply(names(statuses), function(family) {
      paste0(family, ": ", statuses[[family]]$reason)
    }, character(1)), collapse = " ")
    blocked <- any(vapply(statuses, `[[`, logical(1), "current_r_blocked"))
    action <- if (blocked) {
      " Upgrade R before installing the current CRAN backends."
    } else {
      paste0(" Run `", command, "`.")
    }
    stop(
      "The requested model portfolio is unavailable. ", details, action, fallback,
      call. = FALSE
    )
  }
  requested
}

#' Install optional model backends
#'
#' Installs the CRAN packages required by an AutoXplainR model portfolio. This
#' helper only changes the user's library when called explicitly; use
#' `dry_run = TRUE` to inspect the missing packages without installing them.
#'
#' @param portfolio One of `"recommended"` or `"extended"`.
#' @param task Task used to omit inapplicable learner backends. Defaults to
#'   regression, whose recommended portfolio also covers binary classification.
#' @param dry_run Print and visibly return the missing package plan without
#'   installing anything.
#' @param ... Additional arguments passed to [utils::install.packages()].
#'
#' @return With `dry_run = TRUE`, the missing package names visibly. With
#'   `dry_run = FALSE`, the package names whose installation was requested,
#'   invisibly.
#' @export
#'
#' @examples
#' install_model_engines("recommended", task = "regression", dry_run = TRUE)
install_model_engines <- function(portfolio = c("recommended", "extended"),
                                  task = c("regression", "binary", "multiclass"),
                                  dry_run = FALSE,
                                  ...) {
  portfolio <- match.arg(portfolio)
  task <- match.arg(task)
  if (!is.logical(dry_run) || length(dry_run) != 1L || is.na(dry_run)) {
    stop("`dry_run` must be TRUE or FALSE.", call. = FALSE)
  }
  registry <- autoxplain_learner_registry()
  families <- portfolio_learner_families(portfolio, task)
  statuses <- lapply(families, function(family) {
    learner_dependency_status(registry[[family]])
  })
  names(statuses) <- families
  unavailable <- families[!vapply(statuses, `[[`, logical(1), "available")]
  missing <- unique(vapply(unavailable, function(family) {
    registry[[family]]$package
  }, character(1)))
  missing <- missing[nzchar(missing)]
  blocked <- unavailable[vapply(statuses[unavailable], `[[`, logical(1), "current_r_blocked")]
  if (dry_run) {
    if (length(missing)) {
      message(
        "Dry run only; no packages were installed. Missing backends: ",
        paste(missing, collapse = ", "), ".\nWould run: install.packages(c(",
        paste(sprintf("\"%s\"", missing), collapse = ", "), "))"
      )
    } else {
      message(
        "Dry run only; no packages were installed. All `", portfolio,
        "` portfolio backends for `", task, "` are available."
      )
    }
    return(missing)
  }
  if (!dry_run && length(blocked)) {
    requirements <- unique(vapply(blocked, function(family) {
      registry[[family]]$current_cran_r_minimum
    }, character(1)))
    stop(
      "The current CRAN backend requires R >= ", paste(requirements, collapse = ", "),
      ", but this session uses R ", as.character(getRversion()),
      ". Upgrade R or use `portfolio = \"core\"`.",
      call. = FALSE
    )
  }
  if (!dry_run && length(missing)) {
    message("Installing model backends: ", paste(missing, collapse = ", "), ".")
    utils::install.packages(missing, ...)
  }
  if (!length(missing)) message("All `", portfolio, "` portfolio backends are available.")
  invisible(missing)
}

linear_learner_grid <- function(n, p, task, n_classes) {
  list(list())
}

tree_learner_grid <- function(n, p, task, n_classes) {
  template <- data.frame(
    maxdepth = c(2L, 4L, 6L, 10L, 3L, 5L, 8L, 10L, 2L, 4L, 6L, 10L),
    cp = c(0.03, 0.01, 0.003, 0.0005, 0.01, 0.003, 0.001, 0, 0.01, 0.003, 0.001, 0),
    minsplit_fraction = c(
      0.20, 0.12, 0.08, 0.05, 0.20, 0.08, 0.05, 0.03,
      0.08, 0.05, 0.03, 0.02
    )
  )
  lapply(seq_len(nrow(template)), function(index) {
    list(
      maxdepth = template$maxdepth[[index]],
      cp = template$cp[[index]],
      minsplit = max(4L, as.integer(round(n * template$minsplit_fraction[[index]])))
    )
  })
}

neural_learner_grid <- function(n, p, task, n_classes) {
  template <- data.frame(
    size = c(1L, 2L, 4L, 6L, 8L, 2L, 4L, 6L, 8L, 12L, 12L, 16L),
    decay = c(0.1, 0.03, 0.01, 0.01, 0.001, 0.001, 0.001, 0, 0, 0.001, 0, 0.0001)
  )
  lapply(seq_len(nrow(template)), function(index) {
    list(size = template$size[[index]], decay = template$decay[[index]])
  })
}

fit_linear_learner <- function(data, target, task, parameters, seed) {
  formula <- safe_reformulate(setdiff(names(data), target), response = target)
  if (task == "regression") return(stats::lm(formula, data = data))
  if (task == "binary") {
    return(stats::glm(formula, data = data, family = stats::binomial()))
  }
  nnet::multinom(formula, data = data, trace = FALSE)
}

fit_tree_learner <- function(data, target, task, parameters, seed) {
  formula <- safe_reformulate(setdiff(names(data), target), response = target)
  rpart::rpart(
    formula,
    data = data,
    method = if (task == "regression") "anova" else "class",
    control = rpart::rpart.control(
      cp = parameters$cp,
      maxdepth = parameters$maxdepth,
      minsplit = parameters$minsplit,
      xval = 0L
    )
  )
}

fit_neural_learner <- function(data, target, task, parameters, seed) {
  fit_tuned_neural_network(
    data = data,
    target = target,
    task = task,
    size = parameters$size,
    decay = parameters$decay
  )
}

describe_tree_parameters <- function(parameters) {
  paste0(
    "max depth = ", parameters$maxdepth,
    ", pruning cp = ", format(parameters$cp, trim = TRUE),
    ", minimum split = ", parameters$minsplit
  )
}

describe_neural_parameters <- function(parameters) {
  paste0(
    "hidden units = ", parameters$size,
    ", weight decay = ", format(parameters$decay, trim = TRUE)
  )
}

linear_learner_complexity <- function(parameters, n, p, task, n_classes) {
  if (task == "multiclass") (p + 1L) * (n_classes - 1L) else p + 1L
}

tree_learner_complexity <- function(parameters, n, p, task, n_classes) {
  max(2, min(2^parameters$maxdepth, ceiling(n / parameters$minsplit)))
}

neural_learner_complexity <- function(parameters, n, p, task, n_classes) {
  outputs <- if (task == "multiclass") n_classes else 1L
  (p + 1L) * parameters$size + (parameters$size + 1L) * outputs
}
