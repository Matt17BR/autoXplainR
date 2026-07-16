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
  rows <- lapply(registry, function(item) {
    data.frame(
      family = item$family,
      backend = item$backend,
      model = learner_model_label(item, task %||% "regression"),
      supported_tasks = paste(item$tasks, collapse = ", "),
      dependency = item$package %||% "AutoXplainR core",
      available = learner_is_available(item),
      nonlinearity = item$nonlinearity,
      interactions = item$interactions,
      scaling = item$scaling,
      strengths = item$strengths,
      cautions = item$cautions,
      stringsAsFactors = FALSE
    )
  })
  output <- do.call(rbind, rows)
  if (!is.null(task)) {
    output <- output[vapply(registry, function(x) task %in% x$tasks, logical(1)), , drop = FALSE]
  }
  rownames(output) <- NULL
  structure(output, class = c("autoxplain_learner_catalog", "data.frame"))
}

#' @export
print.autoxplain_learner_catalog <- function(x, ...) {
  cat("<AutoXplainR learner catalog>\n")
  cat("  families:  ", nrow(x), "\n", sep = "")
  cat("  available: ", sum(x$available), "\n", sep = "")
  print.data.frame(
    x[c("family", "backend", "supported_tasks", "available", "nonlinearity", "interactions")],
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
      tasks = c("regression", "binary", "multiclass"),
      labels = c(
        regression = "linear regression",
        binary = "logistic regression",
        multiclass = "multinomial logistic regression"
      ),
      model_id = "linear_model",
      simplicity_rank = 1L,
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
    tree = list(
      family = "tree",
      backend = "rpart",
      package = "rpart",
      tasks = c("regression", "binary", "multiclass"),
      labels = c(
        regression = "decision tree",
        binary = "decision tree",
        multiclass = "decision tree"
      ),
      model_id = "tree_model",
      simplicity_rank = 2L,
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
    neural = list(
      family = "neural",
      backend = "nnet",
      package = "nnet",
      tasks = c("regression", "binary", "multiclass"),
      labels = c(
        regression = "neural network",
        binary = "neural network",
        multiclass = "neural network"
      ),
      model_id = "neural_model",
      simplicity_rank = 5L,
      nonlinearity = "smooth and flexible",
      interactions = "automatic through hidden units",
      scaling = "performed inside the adapter",
      strengths = "Can learn smooth nonlinear combinations of many predictors.",
      cautions = "Less transparent, seed-sensitive, and easy to overfit on small data.",
      grid = neural_learner_grid,
      fit = fit_neural_learner,
      describe = describe_neural_parameters,
      complexity = neural_learner_complexity
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
  is.null(definition$package) || requireNamespace(definition$package, quietly = TRUE)
}

learner_model_label <- function(definition, task) {
  unname(definition$labels[[task]] %||% definition$family)
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
  formula <- stats::reformulate(setdiff(names(data), target), response = target)
  if (task == "regression") return(stats::lm(formula, data = data))
  if (task == "binary") {
    return(stats::glm(formula, data = data, family = stats::binomial()))
  }
  nnet::multinom(formula, data = data, trace = FALSE)
}

fit_tree_learner <- function(data, target, task, parameters, seed) {
  formula <- stats::reformulate(setdiff(names(data), target), response = target)
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
