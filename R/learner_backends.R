regularized_learner_grid <- function(n, p, task, n_classes) {
  template <- data.frame(
    alpha = c(1, 0, 0.5, 1, 0.5, 0, 0.25, 0.75, 1, 0.5),
    path_fraction = c(0.55, 0.55, 0.55, 0.75, 0.75, 0.75, 0.4, 0.4, 0.9, 0.9)
  )
  lapply(seq_len(nrow(template)), function(index) {
    list(
      alpha = template$alpha[[index]],
      path_fraction = template$path_fraction[[index]]
    )
  })
}

fit_regularized_learner <- function(data, target, task, parameters, seed) {
  require_optional("glmnet", "fitting regularized models")
  features <- setdiff(names(data), target)
  blueprint <- fit_matrix_blueprint(data, predictors = features)
  x <- bake_matrix_blueprint(blueprint, data)
  needs_dummy <- ncol(x) == 1L
  if (needs_dummy) {
    dummy_name <- make.unique(c(colnames(x), ".autoxplain_glmnet_dummy"))[[2L]]
    dummy <- matrix(0, nrow = nrow(x), ncol = 1L, dimnames = list(NULL, dummy_name))
    x <- cbind(x, dummy)
  } else {
    dummy_name <- NULL
  }
  y <- if (task == "regression") {
    data[[target]]
  } else if (task == "binary") {
    as.numeric(data[[target]] == levels(data[[target]])[[2L]])
  } else {
    data[[target]]
  }
  family <- switch(
    task,
    regression = "gaussian",
    binary = "binomial",
    multiclass = "multinomial"
  )
  arguments <- list(
    x = x,
    y = y,
    family = family,
    alpha = parameters$alpha,
    nlambda = 80L,
    lambda.min.ratio = 0.001,
    standardize = TRUE,
    intercept = TRUE
  )
  if (needs_dummy) arguments$exclude <- ncol(x)
  fit <- do.call(glmnet::glmnet, arguments)
  index <- max(1L, min(
    length(fit$lambda),
    as.integer(round(1 + parameters$path_fraction * (length(fit$lambda) - 1L)))
  ))
  new_autoxplain_fitted_model(
    family = "regularized",
    backend = "glmnet",
    fit = fit,
    task = task,
    features = features,
    parameters = parameters,
    class_levels = if (task == "regression") NULL else levels(data[[target]]),
    blueprint = blueprint,
    fit_details = list(
      lambda = fit$lambda[[index]],
      lambda_index = index,
      dummy_column = dummy_name
    ),
    seed = seed
  )
}

describe_regularized_parameters <- function(parameters) {
  mixture <- if (parameters$alpha == 0) {
    "ridge"
  } else if (parameters$alpha == 1) {
    "lasso"
  } else {
    paste0("elastic net (alpha = ", format(parameters$alpha, trim = TRUE), ")")
  }
  paste0(
    mixture,
    ", lambda-path position = ",
    format(round(parameters$path_fraction, 2L), trim = TRUE)
  )
}

regularized_learner_complexity <- function(parameters, n, p, task, n_classes) {
  outputs <- if (task == "multiclass") n_classes else 1L
  max(1, (p + 1L) * outputs * parameters$path_fraction)
}

additive_learner_grid <- function(n, p, task, n_classes) {
  list(
    list(k = 5L, gamma = 1, select = TRUE),
    list(k = 8L, gamma = 1, select = TRUE),
    list(k = 5L, gamma = 1.4, select = TRUE),
    list(k = 8L, gamma = 1, select = FALSE),
    list(k = 10L, gamma = 1.4, select = TRUE)
  )
}

effective_learner_parameters <- function(family, parameters, data, target) {
  features <- setdiff(names(data), target)
  switch(
    family,
    additive = {
      smooth <- features[vapply(data[features], function(x) {
        is.numeric(x) && !is.logical(x) && length(unique(x)) >= 4L
      }, logical(1))]
      smooth_k <- vapply(smooth, function(feature) {
        unique_values <- length(unique(data[[feature]]))
        max(3L, min(parameters$k, unique_values - 1L))
      }, integer(1))
      list(
        smooth_k = smooth_k,
        gamma = parameters$gamma,
        select = parameters$select
      )
    },
    forest = {
      effective <- parameters
      effective$mtry <- as.integer(min(parameters$mtry, length(features)))
      effective
    },
    neighbors = {
      effective <- parameters
      effective$k <- as.integer(min(parameters$k, max(0L, nrow(data) - 1L)))
      effective
    },
    mars = {
      effective <- parameters
      effective$nprune <- as.integer(min(
        parameters$nprune,
        max(3L, nrow(data) - 2L)
      ))
      effective
    },
    parameters
  )
}

fit_additive_learner <- function(data, target, task, parameters, seed) {
  require_optional("mgcv", "fitting generalized additive models")
  features <- setdiff(names(data), target)
  safe_features <- paste0(".ax", seq_along(features))
  safe_data <- data[c(features, target)]
  names(safe_data) <- c(safe_features, ".outcome")
  effective <- effective_learner_parameters("additive", parameters, data, target)
  smooth_features <- names(effective$smooth_k)
  smooth <- safe_features[match(smooth_features, features)]
  if (!length(smooth)) {
    stop("The additive learner needs a numeric predictor with at least four values.",
         call. = FALSE)
  }
  parametric <- setdiff(safe_features, smooth)
  smooth_terms <- vapply(seq_along(smooth), function(index) {
    paste0(
      "s(", smooth[[index]], ", k = ", effective$smooth_k[[index]],
      ", bs = 'tp')"
    )
  }, character(1))
  rhs <- c(smooth_terms, parametric)
  formula <- stats::as.formula(
    paste(".outcome ~", paste(rhs, collapse = " + ")),
    env = asNamespace("mgcv")
  )
  family <- if (task == "regression") stats::gaussian() else stats::binomial()
  fit <- mgcv::gam(
    formula,
    data = safe_data,
    family = family,
    method = "REML",
    select = parameters$select,
    gamma = parameters$gamma
  )
  new_autoxplain_fitted_model(
    family = "additive",
    backend = "mgcv",
    fit = fit,
    task = task,
    features = features,
    parameters = parameters,
    class_levels = if (task == "regression") NULL else levels(data[[target]]),
    fit_details = list(
      feature_map = stats::setNames(safe_features, features),
      requested_parameters = parameters,
      effective_parameters = effective
    ),
    seed = seed
  )
}

describe_additive_parameters <- function(parameters) {
  paste0(
    "smooth basis limit = ", parameters$k,
    ", gamma = ", format(parameters$gamma, trim = TRUE),
    ", shrinkage selection = ", if (parameters$select) "on" else "off"
  )
}

additive_learner_complexity <- function(parameters, n, p, task, n_classes) {
  max(1, p * parameters$k)
}

forest_learner_grid <- function(n, p, task, n_classes) {
  base_mtry <- unique(pmax(1L, pmin(p, c(
    as.integer(round(sqrt(p))),
    as.integer(round(p / 3)),
    as.integer(round(2 * p / 3)),
    p
  ))))
  node <- if (task == "regression") c(5L, 10L, 20L) else c(1L, 5L, 10L)
  template <- expand.grid(
    mtry = base_mtry,
    min_node_size = node,
    sample_fraction = c(0.632, 0.8),
    splitrule = "default",
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  extra <- data.frame(
    mtry = head(base_mtry, 2L),
    min_node_size = head(node, 2L),
    sample_fraction = 0.8,
    splitrule = "extratrees",
    stringsAsFactors = FALSE
  )
  # Put space-filling anchors first so even a modest shared portfolio budget
  # explores every important forest control instead of only walking `mtry`.
  anchors <- data.frame(
    mtry = c(base_mtry[[1L]], utils::tail(base_mtry, 1L),
             base_mtry[[ceiling(length(base_mtry) / 2)]], base_mtry[[1L]]),
    min_node_size = c(node[[2L]], node[[1L]], node[[3L]], node[[1L]]),
    sample_fraction = c(0.632, 0.8, 0.632, 0.8),
    splitrule = c("default", "default", "default", "extratrees"),
    stringsAsFactors = FALSE
  )
  template <- unique(rbind(anchors, template, extra))
  lapply(seq_len(nrow(template)), function(index) {
    list(
      num.trees = 500L,
      mtry = template$mtry[[index]],
      min.node.size = template$min_node_size[[index]],
      sample.fraction = template$sample_fraction[[index]],
      splitrule = template$splitrule[[index]]
    )
  })
}

fit_forest_learner <- function(data, target, task, parameters, seed) {
  require_optional("ranger", "fitting random forests")
  features <- setdiff(names(data), target)
  effective <- effective_learner_parameters("forest", parameters, data, target)
  effective_mtry <- effective$mtry
  arguments <- list(
    x = data[features],
    y = data[[target]],
    num.trees = parameters$num.trees,
    mtry = effective_mtry,
    min.node.size = parameters$min.node.size,
    sample.fraction = parameters$sample.fraction,
    probability = task != "regression",
    respect.unordered.factors = "order",
    num.threads = 1L,
    seed = seed,
    write.forest = TRUE
  )
  if (!identical(parameters$splitrule, "default")) {
    arguments$splitrule <- parameters$splitrule
    if (identical(parameters$splitrule, "extratrees")) {
      arguments$num.random.splits <- 5L
    }
  }
  fit <- do.call(ranger::ranger, arguments)
  new_autoxplain_fitted_model(
    family = "forest",
    backend = "ranger",
    fit = fit,
    task = task,
    features = features,
    parameters = parameters,
    class_levels = if (task == "regression") NULL else levels(data[[target]]),
    fit_details = list(
      requested_mtry = parameters$mtry,
      effective_mtry = effective_mtry,
      training_predictors = length(features),
      requested_parameters = parameters,
      effective_parameters = effective
    ),
    seed = seed
  )
}

describe_forest_parameters <- function(parameters) {
  paste0(
    "trees = ", parameters$num.trees,
    ", mtry = ", parameters$mtry,
    ", minimum node = ", parameters$min.node.size,
    ", sample fraction = ", format(parameters$sample.fraction, trim = TRUE),
    ", split rule = ", parameters$splitrule
  )
}

forest_learner_complexity <- function(parameters, n, p, task, n_classes) {
  parameters$num.trees * max(2, ceiling(n / parameters$min.node.size))
}

boosting_learner_grid <- function(n, p, task, n_classes) {
  template <- data.frame(
    nrounds = c(100L, 200L, 300L, 500L, 150L, 300L, 500L, 800L, 250L, 500L, 800L, 1000L),
    eta = c(0.1, 0.05, 0.03, 0.02, 0.1, 0.05, 0.03, 0.02, 0.05, 0.03, 0.02, 0.01),
    max_depth = c(2L, 3L, 4L, 5L, 5L, 2L, 3L, 4L, 6L, 6L, 3L, 4L),
    min_child_weight = c(1, 1, 1, 2, 1, 3, 3, 3, 5, 5, 7, 7),
    subsample = c(1, 0.8, 0.8, 0.8, 1, 0.7, 0.7, 0.7, 0.8, 0.8, 0.7, 0.7),
    colsample_bytree = c(1, 1, 0.8, 0.8, 0.8, 1, 0.8, 0.7, 0.7, 0.6, 0.8, 0.6)
  )
  lapply(seq_len(nrow(template)), function(index) {
    list(
      nrounds = template$nrounds[[index]],
      eta = template$eta[[index]],
      max_depth = template$max_depth[[index]],
      min_child_weight = template$min_child_weight[[index]],
      subsample = template$subsample[[index]],
      colsample_bytree = template$colsample_bytree[[index]],
      reg_alpha = 0,
      reg_lambda = 1
    )
  })
}

fit_boosting_learner <- function(data, target, task, parameters, seed) {
  require_optional("xgboost", "fitting gradient-boosted trees")
  features <- setdiff(names(data), target)
  blueprint <- fit_matrix_blueprint(data, predictors = features)
  x <- bake_matrix_blueprint(blueprint, data)
  class_levels <- if (task == "regression") NULL else levels(data[[target]])
  label <- if (task == "regression") {
    data[[target]]
  } else {
    as.numeric(data[[target]]) - 1L
  }
  objective <- switch(
    task,
    regression = "reg:squarederror",
    binary = "binary:logistic",
    multiclass = "multi:softprob"
  )
  metric <- switch(task, regression = "rmse", binary = "logloss", multiclass = "mlogloss")
  xgb_parameters <- list(
    objective = objective,
    eval_metric = metric,
    eta = parameters$eta,
    max_depth = parameters$max_depth,
    min_child_weight = parameters$min_child_weight,
    subsample = parameters$subsample,
    colsample_bytree = parameters$colsample_bytree,
    alpha = parameters$reg_alpha,
    lambda = parameters$reg_lambda,
    nthread = 1L,
    seed = seed,
    verbosity = 0L
  )
  if (task == "multiclass") xgb_parameters$num_class <- length(class_levels)
  matrix <- xgboost::xgb.DMatrix(data = x, label = label)
  fit <- xgboost::xgb.train(
    params = xgb_parameters,
    data = matrix,
    nrounds = parameters$nrounds,
    verbose = 0L
  )
  new_autoxplain_fitted_model(
    family = "boosting",
    backend = "xgboost",
    fit = fit,
    task = task,
    features = features,
    parameters = parameters,
    class_levels = class_levels,
    blueprint = blueprint,
    seed = seed
  )
}

describe_boosting_parameters <- function(parameters) {
  paste0(
    "rounds = ", parameters$nrounds,
    ", learning rate = ", format(parameters$eta, trim = TRUE),
    ", depth = ", parameters$max_depth,
    ", child weight = ", parameters$min_child_weight,
    ", row/column sample = ", format(parameters$subsample, trim = TRUE),
    "/", format(parameters$colsample_bytree, trim = TRUE)
  )
}

boosting_learner_complexity <- function(parameters, n, p, task, n_classes) {
  parameters$nrounds * max(2, 2^parameters$max_depth)
}

kernel_learner_grid <- function(n, p, task, n_classes) {
  # The prefix is deliberately space-filling because round-robin portfolio
  # budgets commonly expose only the first few configurations per family.
  template <- data.frame(
    cost = c(1, 4, 0.5, 16, 1, 1, 4, 16, 0.5, 4, 16, 0.5),
    gamma_multiplier = c(1, 0.5, 2, 1, 0.5, 2, 2, 0.5, 1, 1, 2, 0.5),
    epsilon = c(0.1, 0.05, 0.2, 0.02, 0.2, 0.05, 0.1, 0.1, 0.05, 0.2, 0.05, 0.1)
  )
  if (!identical(task, "regression")) template$epsilon <- 0.1
  lapply(seq_len(nrow(template)), function(index) {
    list(
      cost = template$cost[[index]],
      gamma_multiplier = template$gamma_multiplier[[index]],
      epsilon = template$epsilon[[index]]
    )
  })
}

fit_kernel_learner <- function(data, target, task, parameters, seed) {
  require_optional("e1071", "fitting radial support vector machines")
  features <- setdiff(names(data), target)
  blueprint <- fit_matrix_blueprint(
    data,
    predictors = features,
    center = TRUE,
    scale = TRUE,
    categorical_encoding = "one_hot"
  )
  x <- bake_matrix_blueprint(blueprint, data)
  y <- data[[target]]
  outcome_center <- 0
  outcome_scale <- 1
  if (task == "regression") {
    outcome_center <- mean(y)
    outcome_scale <- stats::sd(y)
    if (!is.finite(outcome_center) || !is.finite(outcome_scale) || outcome_scale <= 0) {
      stop(
        "Radial support-vector regression needs a finite outcome with non-zero variance.",
        call. = FALSE
      )
    }
    y <- (y - outcome_center) / outcome_scale
  }
  fit <- e1071::svm(
    x = x,
    y = y,
    type = if (task == "regression") "eps-regression" else "C-classification",
    kernel = "radial",
    cost = parameters$cost,
    gamma = parameters$gamma_multiplier / ncol(x),
    epsilon = parameters$epsilon,
    probability = task != "regression",
    scale = FALSE,
    cachesize = 100,
    tolerance = 1e-7,
    shrinking = TRUE
  )
  new_autoxplain_fitted_model(
    family = "kernel",
    backend = "e1071",
    fit = fit,
    task = task,
    features = features,
    parameters = parameters,
    class_levels = if (task == "regression") NULL else levels(y),
    blueprint = blueprint,
    fit_details = list(
      outcome_center = outcome_center,
      outcome_scale = outcome_scale,
      outcome_standardized = task == "regression"
    ),
    seed = seed
  )
}

describe_kernel_parameters <- function(parameters) {
  paste0(
    "radial SVM cost = ", format(parameters$cost, trim = TRUE),
    ", gamma multiplier = ", format(parameters$gamma_multiplier, trim = TRUE),
    ", dimensionless epsilon = ", format(parameters$epsilon, trim = TRUE)
  )
}

kernel_learner_complexity <- function(parameters, n, p, task, n_classes) {
  max(1, n * sqrt(parameters$cost * parameters$gamma_multiplier))
}

mars_learner_grid <- function(n, p, task, n_classes) {
  template <- expand.grid(
    degree = c(1L, 2L),
    nprune = unique(pmax(3L, pmin(c(6L, 12L, 20L), 2L * p + 1L))),
    KEEP.OUT.ATTRS = FALSE
  )
  lapply(seq_len(nrow(template)), function(index) {
    list(degree = template$degree[[index]], nprune = template$nprune[[index]])
  })
}

fit_mars_learner <- function(data, target, task, parameters, seed) {
  require_optional("earth", "fitting MARS models")
  features <- setdiff(names(data), target)
  blueprint <- fit_matrix_blueprint(data, predictors = features)
  x <- bake_matrix_blueprint(blueprint, data)
  y <- if (task == "regression") {
    data[[target]]
  } else {
    as.numeric(data[[target]] == levels(data[[target]])[[2L]])
  }
  effective <- effective_learner_parameters("mars", parameters, data, target)
  arguments <- list(
    x = x,
    y = y,
    degree = parameters$degree,
    nprune = effective$nprune,
    nk = max(21L, 2L * effective$nprune + 1L),
    pmethod = "backward",
    trace = 0
  )
  if (task == "binary") arguments$glm <- list(family = stats::binomial())
  fit <- do.call(earth::earth, arguments)
  new_autoxplain_fitted_model(
    family = "mars",
    backend = "earth",
    fit = fit,
    task = task,
    features = features,
    parameters = parameters,
    class_levels = if (task == "regression") NULL else levels(data[[target]]),
    blueprint = blueprint,
    fit_details = list(
      requested_parameters = parameters,
      effective_parameters = effective,
      nk = arguments$nk
    ),
    seed = seed
  )
}

describe_mars_parameters <- function(parameters) {
  paste0(
    "maximum interaction degree = ", parameters$degree,
    ", retained terms = ", parameters$nprune
  )
}

mars_learner_complexity <- function(parameters, n, p, task, n_classes) {
  parameters$nprune * parameters$degree
}

neighbors_learner_grid <- function(n, p, task, n_classes) {
  # Keep early candidates diverse: low portfolio budgets should compare local
  # versus smoother neighborhoods and both distance/weighting geometries.
  template <- data.frame(
    k = c(5L, 15L, 3L, 25L, 9L, 35L, 3L, 15L, 25L, 5L, 35L, 9L),
    distance = c(2, 1, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1),
    kernel = c(
      "optimal", "triangular", "optimal", "triangular",
      "triangular", "optimal", "triangular", "optimal",
      "optimal", "triangular", "triangular", "optimal"
    ),
    stringsAsFactors = FALSE
  )
  template$k <- pmax(1L, pmin(max(1L, n - 1L), template$k))
  template <- unique(template)
  lapply(seq_len(nrow(template)), function(index) {
    list(
      k = template$k[[index]],
      distance = template$distance[[index]],
      kernel = template$kernel[[index]]
    )
  })
}

fit_neighbors_learner <- function(data, target, task, parameters, seed) {
  require_optional("kknn", "fitting nearest-neighbor models")
  features <- setdiff(names(data), target)
  training_rows <- nrow(data)
  if (training_rows < 2L) {
    stop("The nearest-neighbor learner needs at least two training rows.", call. = FALSE)
  }
  effective <- effective_learner_parameters("neighbors", parameters, data, target)
  requested_k <- as.integer(parameters$k)
  effective_k <- effective$k
  fitted_parameters <- parameters
  fitted_parameters$k <- effective_k
  blueprint <- fit_matrix_blueprint(
    data,
    predictors = features,
    center = TRUE,
    scale = TRUE,
    categorical_encoding = "one_hot"
  )
  x <- bake_matrix_blueprint(blueprint, data)
  safe_names <- paste0(".ax", seq_len(ncol(x)))
  encoded <- as.data.frame(x, check.names = FALSE)
  names(encoded) <- safe_names
  encoded$.outcome <- data[[target]]
  fit <- kknn::train.kknn(
    .outcome ~ .,
    data = encoded,
    kmax = effective_k,
    ks = effective_k,
    distance = parameters$distance,
    kernel = parameters$kernel,
    scale = FALSE
  )
  new_autoxplain_fitted_model(
    family = "neighbors",
    backend = "kknn",
    fit = fit,
    task = task,
    features = features,
    parameters = fitted_parameters,
    class_levels = if (task == "regression") NULL else levels(data[[target]]),
    blueprint = blueprint,
    fit_details = list(
      encoded_names = safe_names,
      training_rows = training_rows,
      requested_k = requested_k,
      effective_k = effective_k,
      requested_parameters = parameters,
      effective_parameters = effective
    ),
    seed = seed
  )
}

describe_neighbors_parameters <- function(parameters) {
  paste0(
    "neighbors = ", parameters$k,
    ", distance power = ", format(parameters$distance, trim = TRUE),
    ", weighting kernel = ", parameters$kernel
  )
}

neighbors_learner_complexity <- function(parameters, n, p, task, n_classes) {
  max(1, n / parameters$k) * p
}
