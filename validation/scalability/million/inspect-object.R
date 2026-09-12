# Diagnose retained payloads without mutating the saved result. object.size()
# counts shared ordinary vectors repeatedly, so it is not physical live memory.
args <- commandArgs(TRUE)
stopifnot(length(args) == 1L)
directory <- normalizePath(args[[1L]])
result <- readRDS(file.path(directory, "result.rds"))
bytes <- function(x) as.numeric(object.size(x))
components <- function(x) sort(vapply(x, bytes, numeric(1L)), decreasing = TRUE)
models <- lapply(result$models, function(model) {
  fit <- if (inherits(model, "autoxplain_fitted_model")) model$fit else model
  native_call <- if (is.list(fit)) fit$call else NULL
  list(components = if (is.list(model)) as.list(components(model)) else NULL,
    native_components = if (is.list(fit)) as.list(components(fit)) else NULL,
    native_call_arguments = if (!is.null(native_call)) {
      values <- as.list(native_call)
      labels <- names(values)
      labels[[1L]] <- "function"
      lapply(seq_along(values), function(i) list(name = labels[[i]],
        type = typeof(values[[i]]), bytes = bytes(values[[i]])))
    } else NULL)
})
summary <- list(
  package_artifact = jsonlite::read_json(file.path(directory, "input.json"))$library,
  training_rows = nrow(result$training_data),
  object_bytes = bytes(result), saved_bytes = file.info(file.path(directory, "result.rds"))$size,
  size_scope = "R object.size counts shared vectors repeatedly; this is not resident-memory attribution.",
  components = as.list(components(result)),
  data_context = lapply(result$data_context[c("raw", "row_map")], function(x) as.list(components(x))),
  preprocessing = as.list(components(result$preprocessing_metadata)),
  duplicated_preprocessed_values = list(
    training = identical(result$training_data, result$preprocessing_metadata$training_data$data),
    evaluation = identical(result$test_data, result$preprocessing_metadata$test_data$data)),
  models = models,
  script_md5 = unname(tools::md5sum(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))))
)
jsonlite::write_json(summary, file.path(directory, "object-profile.json"), pretty = TRUE,
  auto_unbox = TRUE, null = "null", digits = 16)
cat("Object profile saved for", nrow(result$training_data), "training rows\n")
