new_autoxplain_fitted_model <- function(family,
                                        backend,
                                        fit,
                                        task,
                                        features,
                                        parameters,
                                        class_levels = NULL,
                                        blueprint = NULL,
                                        fit_details = list(),
                                        seed = NULL) {
  structure(
    list(
      family = family,
      backend = backend,
      fit = fit,
      task = task,
      features = features,
      parameters = parameters,
      class_levels = class_levels,
      blueprint = blueprint,
      fit_details = fit_details,
      seed = seed,
      package_version = backend_package_version(backend)
    ),
    class = "autoxplain_fitted_model"
  )
}

#' @export
print.autoxplain_fitted_model <- function(x, ...) {
  cat("<AutoXplainR fitted model>\n")
  cat("  family:  ", x$family, "\n", sep = "")
  cat("  backend: ", x$backend, " ", x$package_version, "\n", sep = "")
  cat("  task:    ", x$task, "\n", sep = "")
  cat("  features:", length(x$features), "\n", sep = "")
  invisible(x)
}

#' @export
predict.autoxplain_fitted_model <- function(object, newdata, ...) {
  assert_data_frame(newdata, "newdata")
  missing <- setdiff(object$features, names(newdata))
  if (length(missing)) {
    stop("`newdata` is missing model features: ", paste(missing, collapse = ", "), ".",
         call. = FALSE)
  }
  data <- newdata[object$features]
  prediction <- switch(
    object$backend,
    ranger = predict_ranger_backend(object, data),
    mgcv = predict_mgcv_backend(object, data),
    glmnet = predict_glmnet_backend(object, data),
    xgboost = predict_xgboost_backend(object, data),
    e1071 = predict_e1071_backend(object, data),
    earth = predict_earth_backend(object, data),
    kknn = predict_kknn_backend(object, data),
    stop("Unsupported fitted-model backend `", object$backend, "`.", call. = FALSE)
  )
  normalize_fitted_model_prediction(prediction, object, nrow(data))
}

predict_ranger_backend <- function(object, newdata) {
  require_optional("ranger", "predicting with a random forest")
  stats::predict(object$fit, data = newdata)$predictions
}

predict_mgcv_backend <- function(object, newdata) {
  require_optional("mgcv", "predicting with an additive model")
  map <- object$fit_details$feature_map
  names(newdata)[match(names(map), names(newdata))] <- unname(map)
  stats::predict(object$fit, newdata = newdata, type = "response")
}

predict_glmnet_backend <- function(object, newdata) {
  require_optional("glmnet", "predicting with a regularized model")
  matrix <- bake_matrix_blueprint(object$blueprint, newdata)
  dummy <- object$fit_details$dummy_column %||% NULL
  if (!is.null(dummy)) {
    dummy_matrix <- base::matrix(
      0,
      nrow = nrow(matrix),
      ncol = 1L,
      dimnames = list(NULL, dummy)
    )
    matrix <- cbind(matrix, dummy_matrix)
  }
  stats::predict(
    object$fit,
    newx = matrix,
    s = object$fit_details$lambda,
    type = "response"
  )
}

predict_xgboost_backend <- function(object, newdata) {
  require_optional("xgboost", "predicting with boosted trees")
  matrix <- bake_matrix_blueprint(object$blueprint, newdata)
  stats::predict(object$fit, newdata = matrix)
}

predict_e1071_backend <- function(object, newdata) {
  require_optional("e1071", "predicting with a radial support vector machine")
  matrix <- bake_matrix_blueprint(object$blueprint, newdata)
  if (object$task == "regression") {
    prediction <- as.numeric(stats::predict(object$fit, matrix))
    outcome_center <- object$fit_details$outcome_center %||% 0
    outcome_scale <- object$fit_details$outcome_scale %||% 1
    return(prediction * outcome_scale + outcome_center)
  }
  hard <- stats::predict(object$fit, matrix, probability = TRUE)
  probability <- attr(hard, "probabilities")
  if (is.null(probability)) {
    stop("The SVM backend did not return calibrated class probabilities.", call. = FALSE)
  }
  probability
}

predict_earth_backend <- function(object, newdata) {
  require_optional("earth", "predicting with a MARS model")
  matrix <- bake_matrix_blueprint(object$blueprint, newdata)
  stats::predict(object$fit, newdata = matrix, type = "response")
}

predict_kknn_backend <- function(object, newdata) {
  require_optional("kknn", "predicting with a nearest-neighbor model")
  matrix <- bake_matrix_blueprint(object$blueprint, newdata)
  data <- as.data.frame(matrix, check.names = FALSE)
  names(data) <- object$fit_details$encoded_names
  stats::predict(
    object$fit,
    newdata = data,
    type = if (object$task == "regression") "raw" else "prob"
  )
}

normalize_fitted_model_prediction <- function(prediction, object, n) {
  if (object$task == "regression") return(as.numeric(prediction))
  levels <- object$class_levels
  if (object$task == "binary") {
    if (is.array(prediction) && length(dim(prediction)) > 2L) {
      prediction <- drop(prediction)
    }
    if (is.matrix(prediction) || is.data.frame(prediction)) {
      prediction <- as.matrix(prediction)
      if (ncol(prediction) == 1L) return(as.numeric(prediction[, 1L]))
      index <- match(levels[[2L]], colnames(prediction))
      if (is.na(index)) index <- ncol(prediction)
      return(as.numeric(prediction[, index]))
    }
    return(as.numeric(prediction))
  }
  if (is.array(prediction) && length(dim(prediction)) == 3L) {
    prediction <- prediction[, , 1L, drop = TRUE]
  }
  if (is.vector(prediction) && length(prediction) == n * length(levels)) {
    prediction <- matrix(prediction, nrow = n, ncol = length(levels), byrow = TRUE)
  }
  prediction <- as.matrix(prediction)
  if (!identical(dim(prediction), c(n, length(levels)))) {
    stop("The backend returned an invalid multiclass probability shape.", call. = FALSE)
  }
  if (is.null(colnames(prediction))) {
    colnames(prediction) <- levels
  } else {
    missing <- setdiff(levels, colnames(prediction))
    if (length(missing)) {
      stop("The backend omitted multiclass probabilities for: ",
           paste(missing, collapse = ", "), ".", call. = FALSE)
    }
    prediction <- prediction[, levels, drop = FALSE]
  }
  prediction
}

backend_package_version <- function(backend) {
  package <- switch(
    backend,
    ranger = "ranger",
    mgcv = "mgcv",
    glmnet = "glmnet",
    xgboost = "xgboost",
    e1071 = "e1071",
    earth = "earth",
    kknn = "kknn",
    NA_character_
  )
  if (is.na(package) || !requireNamespace(package, quietly = TRUE)) return(NA_character_)
  as.character(utils::packageVersion(package))
}

fitted_model_complexity <- function(model) {
  switch(
    model$backend,
    ranger = as.numeric(model$fit$num.trees %||% model$parameters$num.trees),
    mgcv = sum(model$fit$edf, na.rm = TRUE),
    glmnet = glmnet_nonzero_coefficients(model),
    xgboost = as.numeric(model$parameters$nrounds),
    e1071 = as.numeric(model$fit$tot.nSV %||% sum(model$fit$nSV)),
    earth = as.numeric(length(model$fit$selected.terms %||% numeric())),
    kknn = as.numeric(model$fit_details$training_rows / model$parameters$k),
    NA_real_
  )
}

glmnet_nonzero_coefficients <- function(model) {
  require_optional("glmnet", "extracting regularized-model complexity")
  coefficients <- stats::coef(model$fit, s = model$fit_details$lambda)
  if (is.list(coefficients)) {
    return(sum(vapply(coefficients, function(x) sum(as.numeric(x) != 0), numeric(1))))
  }
  sum(as.numeric(coefficients) != 0)
}
