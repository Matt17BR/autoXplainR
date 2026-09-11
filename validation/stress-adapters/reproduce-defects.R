# Run with AXR_ADAPTER_LIBRARY for the unmodified release or source otherwise.
library_path <- Sys.getenv("AXR_ADAPTER_LIBRARY")
if (nzchar(library_path)) {
  .libPaths(c(library_path, .libPaths()))
  library(AutoXplainR)
} else {
  pkgload::load_all(quiet = TRUE)
}
output <- Sys.getenv("AXR_ADAPTER_OUTPUT")
if (!nzchar(output)) stop("Set AXR_ADAPTER_OUTPUT to a JSON destination.")
set.seed(12)
collision <- data.frame(x = factor(rep(c("a", "b"), 60L)), xb = rnorm(120L))
collision$y <- as.numeric(collision$x) + collision$xb + rnorm(120L, sd = 0.1)
start <- proc.time()[["elapsed"]]
result <- autoxplain(
  collision, "y", learners = c("linear", "mars"),
  max_models = 2L, nfolds = 3L, explain = FALSE
)
collision_record <- list(
  elapsed_seconds = proc.time()[["elapsed"]] - start,
  candidates = result$tuning$candidates,
  folds = result$tuning$fold_scores,
  predictions = lapply(names(result$models), function(model) {
    list(model = model, predictions = predict(result, collision[1:10, ], model = model))
  })
)
base <- data.frame(x = seq(-2, 2, length.out = 80L))
base$y <- sin(2 * base$x) + base$x / 5
scaled <- base
scaled$x <- scaled$x * 1e160
blueprint <- AutoXplainR:::fit_matrix_blueprint(scaled["x"], center = TRUE, scale = TRUE)
matrix <- AutoXplainR:::bake_matrix_blueprint(blueprint, scaled)
scaled_result <- tryCatch(autoxplain(
  scaled, "y", learners = c("tree", "kernel"),
  max_models = 2L, nfolds = 3L, explain = FALSE
), error = identity)
base_result <- autoxplain(
  base, "y", learners = c("tree", "kernel"),
  max_models = 2L, nfolds = 3L, explain = FALSE
)
scale_record <- list(
  scale = blueprint$scale, zero_variance = blueprint$zero_variance_columns,
  standardization_max_error = max(abs(as.numeric(matrix) - as.numeric(scale(base$x)))),
  success = !inherits(scaled_result, "error"),
  error = if (inherits(scaled_result, "error")) conditionMessage(scaled_result) else NULL,
  base_candidates = base_result$tuning$candidates,
  scaled_candidates = if (inherits(scaled_result, "error")) NULL else scaled_result$tuning$candidates,
  scaled_folds = if (inherits(scaled_result, "error")) NULL else scaled_result$tuning$fold_scores
)
jsonlite::write_json(
  list(collision = collision_record, scale = scale_record), output,
  pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null", digits = 16
)
