# Run from the repository root. See README.md for the recorded scope.
arguments <- commandArgs(trailingOnly = TRUE)
flags <- arguments[-1L]
if (!length(arguments) || any(!flags %in% c("--events", "--shared-context")) || anyDuplicated(flags)) {
  stop(paste("Usage: Rscript validation/competitive-tabular/explanation-work/count-work.R",
    "OUTPUT_DIRECTORY [--shared-context] [--events]"))
}
if (!file.exists("DESCRIPTION") || !dir.exists("R") ||
    !identical(unname(read.dcf("DESCRIPTION")[1L, "Package"]), "AutoXplainR")) {
  stop("Run this script from the AutoXplainR repository root.")
}
audit_directory <- arguments[[1L]]
dir.create(audit_directory, recursive = TRUE, showWarnings = FALSE)
audit_directory <- normalizePath(audit_directory, mustWork = TRUE)
save_events <- "--events" %in% flags
shared_context <- "--shared-context" %in% flags
source_paths <- c("DESCRIPTION", "NAMESPACE", sort(list.files("R", full.names = TRUE)),
  sort(list.files("inst", recursive = TRUE, full.names = TRUE)),
  sort(list.files("data", recursive = TRUE, full.names = TRUE)),
  "validation/competitive-tabular/explanation-work/count-work.R")
source_hashes <- function(root = ".") {
  vapply(source_paths, function(path) digest::digest(file = file.path(root, path), algo = "sha256"), character(1))
}
started_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
source_at_start <- source_hashes()
# Other development can continue while this probe runs. Load an immutable copy
# and record the bytes actually loaded, rather than hashing a changing checkout.
snapshot_directory <- tempfile("autoxplain-explanation-source-")
for (path in source_paths) {
  destination <- file.path(snapshot_directory, path)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  stopifnot(file.copy(path, destination))
}
source_loaded <- source_hashes(snapshot_directory)
stopifnot(identical(source_at_start, source_loaded))
pkgload::load_all(snapshot_directory, quiet = TRUE)
namespace <- asNamespace("AutoXplainR")
audit_log <- new.env(parent = emptyenv())
audit_log$rows <- list()
audit_log$fingerprints <- list()
options(axr.explanation.work.audit = audit_log)

# Observe validations, which occur once after every actual prediction in this
# path. The supplied predictor stays pure; logging is outside its identity.
trace("validate_predictions", where = namespace, print = FALSE, tracer = quote({
  active_calls <- sys.calls()
  calls <- vapply(active_calls, function(call) paste(deparse(call[[1L]]), collapse = " "), character(1))
  fingerprint_active <- any(vapply(seq_len(sys.nframe()), function(index) {
    identical(sys.function(index), AutoXplainR:::current_explainer_fingerprint)
  }, logical(1)))
  kind <- if (fingerprint_active) {
    "full_fingerprint"
  } else if (any(grepl("calculate_ale_impl", calls, fixed = TRUE))) {
    "sampled_ale"
  } else if (any(grepl("calculate_pdp_impl", calls, fixed = TRUE))) {
    "sampled_pdp"
  } else if (any(grepl("calculate_permutation_importance", calls, fixed = TRUE))) {
    is_permutation <- any(vapply(active_calls, function(call) {
      identical(call[[1L]], as.name("predict")) && length(call) >= 3L &&
        identical(call[[3L]], as.name("permuted"))
    }, logical(1)))
    if (is_permutation) "sampled_permutation" else "full_importance_baseline"
  } else if (any(grepl("prediction_agreement", calls, fixed = TRUE))) {
    "full_agreement"
  } else if (any(grepl("explain_model", calls, fixed = TRUE))) {
    if (n <= 3L) "context_probe" else "context_reference"
  } else {
    "other"
  }
  holder <- getOption("axr.explanation.work.audit")
  holder$rows[[length(holder$rows) + 1L]] <- data.frame(
    kind = kind, task = task, rows = n, stringsAsFactors = FALSE
  )
}))
trace("explainer_content_fingerprint", where = namespace, print = FALSE, tracer = quote({
  holder <- getOption("axr.explanation.work.audit")
  holder$fingerprints[[length(holder$fingerprints) + 1L]] <- data.frame(
    task = task, rows = nrow(data), model_bytes = as.numeric(object.size(model)), stringsAsFactors = FALSE
  )
}))

predict_fixture <- function(model, newdata) {
  value <- as.numeric(as.matrix(newdata) %*% model$weights)
  if (identical(model$task, "regression")) return(value)
  logits <- cbind(a = value / 8, b = -value / 8, c = 0.25 + value / 32)
  logits <- exp(logits - apply(logits, 1L, max))
  logits / rowSums(logits)
}

run_case <- function(task) {
  set.seed(321)
  data <- as.data.frame(matrix(rnorm(180L * 90L), ncol = 90L))
  names(data) <- paste0("x", seq_len(90L))
  data$y <- if (identical(task, "regression")) {
    rowSums(data[seq_len(40L)]) + rnorm(nrow(data), sd = 20)
  } else {
    factor(rep(c("a", "b", "c"), length.out = nrow(data)), levels = c("a", "b", "c"))
  }
  models <- lapply(seq_len(5L), function(index) {
    weights <- numeric(90L)
    weights[seq_len(8L) + (index - 1L) * 8L] <- 1
    list(weights = weights, task = task)
  })
  names(models) <- paste0("model", seq_along(models))
  result <- evaluate_models(models, data, "y", task = task,
    predict_functions = setNames(rep(list(predict_fixture), length(models)), names(models)),
    primary = "model1", seed = 123L
  )
  audit_log$rows <- list()
  audit_log$fingerprints <- list()
  elapsed <- system.time(prepared <- AutoXplainR:::prepare_model_report_data(result, explanation_rows = 50L))[["elapsed"]]
  rows <- do.call(rbind, audit_log$rows)
  fingerprints <- do.call(rbind, audit_log$fingerprints)
  summary <- aggregate(rows ~ kind + task, data = rows, FUN = sum)
  counts <- as.data.frame(table(rows$kind), stringsAsFactors = FALSE)
  summary$calls <- counts$Freq[match(summary$kind, counts$Var1)]
  summary <- summary[c("task", "kind", "calls", "rows")]
  M <- length(prepared$audit$importance_objects)
  P <- ncol(data) - 1L
  U <- length(prepared$audit$config$features)
  C <- if (task == "multiclass") 3L else 1L
  K <- sum(prepared$audit$performance$near_optimal)
  formula <- data.frame(task = task, models = M, features = P, union_features = U,
    classes = C, near_optimal = K, evaluation_rows = nrow(data), sampled_rows = 50L,
    predicted_permutation_calls = M * (5L * P + 20L * U),
    predicted_effect_calls = M * 8L * (if (shared_context) 1L else C) * 2L,
    predicted_full_fingerprint_predictions = if (shared_context) M else M * 3L + M * 8L * C,
    actual_full_fingerprint_predictions = sum(rows$kind == "full_fingerprint"),
    predicted_total_full_predictions = if (shared_context) 2L * M else {
      6L * M + 8L * M * C + if (K >= 2L) K else 0L
    },
    actual_total_full_predictions = sum(rows$rows == nrow(data)),
    supplied_explainer_identity_hashes = nrow(fingerprints),
    expected_supplied_explainer_identity_hashes = if (shared_context) 3L * M else M * 5L + M * 8L * C,
    inferred_native_explainer_identity_hashes = if (shared_context) 2L * M else M * 4L + M * 8L * C,
    elapsed_instrumented_seconds = elapsed)
  write.csv(summary, file.path(audit_directory, paste0(task, "-counts.csv")), row.names = FALSE)
  write.csv(formula, file.path(audit_directory, paste0(task, "-formula.csv")), row.names = FALSE)
  print(formula)
  print(summary)
  stopifnot(
    sum(rows$kind == "sampled_permutation") == formula$predicted_permutation_calls,
    sum(rows$kind == "sampled_ale") == formula$predicted_effect_calls,
    formula$actual_full_fingerprint_predictions == formula$predicted_full_fingerprint_predictions,
    formula$actual_total_full_predictions == formula$predicted_total_full_predictions,
    # Supplied-model reconstruction rebinds each original custom-instance ID,
    # adding one identity hash per model beyond the initial context hash.
    nrow(fingerprints) == formula$expected_supplied_explainer_identity_hashes,
    !any(rows$kind == "other")
  )
  if (save_events) {
    saveRDS(list(rows = rows, fingerprints = fingerprints, formula = formula,
      source_sha256 = source_at_start,
      metadata = list(scope = "Synthetic pure supplied predictors; operation counts only, not runtime benchmark"),
      effects = if (!is.null(prepared$effects_by_class)) prepared$effects_by_class else prepared$effects_by_model),
      file.path(audit_directory, paste0(task, "-evidence.rds")))
  }
}
run_case("regression")
run_case("multiclass")
untrace("validate_predictions", where = namespace)
untrace("explainer_content_fingerprint", where = namespace)
options(axr.explanation.work.audit = NULL)

# This supported deterministic predictor demonstrates why concatenating
# perturbation batches is not generally equivalent to separate prediction.
centered_predictor <- function(newdata) newdata$x - mean(newdata$x)
lower <- data.frame(x = c(0, 1))
upper <- data.frame(x = c(1, 2))
separate_difference <- centered_predictor(upper) - centered_predictor(lower)
combined <- centered_predictor(rbind(lower, upper))
combined_difference <- combined[3:4] - combined[1:2]
stopifnot(identical(separate_difference, c(0, 0)), identical(combined_difference, c(1, 1)))
write.csv(data.frame(row = 1:2, separate_difference = separate_difference,
  concatenated_difference = combined_difference),
  file.path(audit_directory, "batching-counterexample.csv"), row.names = FALSE)
source_at_end <- source_hashes()
manifest <- list(
  schema_version = "1",
  scope = "Synthetic pure supplied predictors; operation counts only, not native model or elapsed-time benchmark",
  computation_protocol = if (shared_context) "shared-context-and-class-batches" else "separate-public-computations",
  started_at = started_at, completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  r_version = R.version.string,
  packages = vapply(c("AutoXplainR", "pkgload", "digest", "jsonlite"),
    function(package) as.character(utils::packageVersion(package)), character(1)),
  source_sha256_at_start = as.list(source_at_start),
  source_sha256_loaded = as.list(source_loaded),
  source_sha256_at_end = as.list(source_at_end),
  loaded_snapshot_matches_start = identical(source_at_start, source_loaded),
  checkout_changes_during_run = names(source_at_end)[source_at_start != source_at_end],
  fixture = list(models = 5L, numeric_predictors = 90L, evaluation_rows = 180L,
    explanation_rows = 50L, screening_repeats = 5L, audit_repeats = 20L,
    effect_inputs_per_model_class = 8L, fixture_seed = 321L, explanation_seed = 123L),
  checks = list(regression_work_counts = "passed", multiclass_work_counts = "passed",
    batch_composition_counterexample = "passed"),
  event_records_written = save_events,
  outputs_sha256 = as.list(vapply(c("regression-counts.csv", "regression-formula.csv",
    "multiclass-counts.csv", "multiclass-formula.csv", "batching-counterexample.csv"),
    function(name) digest::digest(file = file.path(audit_directory, name), algo = "sha256"), character(1)))
)
jsonlite::write_json(manifest, file.path(audit_directory, "manifest.json"),
  pretty = TRUE, auto_unbox = TRUE)
stopifnot(manifest$loaded_snapshot_matches_start,
  identical(source_loaded, source_hashes(snapshot_directory)))
unlink(snapshot_directory, recursive = TRUE)
