script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script)), "common.R"))
output <- stress_directory()
run <- Sys.getenv("AXR_STRESS_RUN", "baseline")
summaries <- list.files(file.path(output, run), "^summary\\.json$", recursive = TRUE, full.names = TRUE)
process_path <- file.path(output, run, "processes.json")
processes <- if (file.exists(process_path)) jsonlite::read_json(process_path) else list()
rows <- list()
operational <- list()
for (path in summaries) {
  record <- jsonlite::read_json(path, simplifyVector = TRUE)
  operational[[length(operational) + 1L]] <- list(scenario = record$scenario,
    variant = record$variant, status = record$status, error = record$error,
    elapsed_seconds = record$elapsed_seconds, configuration_count = record$configuration_count,
    failed_configurations = if (!is.null(record$configurations)) {
      record$configurations$configuration_id[record$configurations$status != "ok"]
    } else character(), warnings = record$warnings)
  if (record$status != "ok") next
  metrics <- record$metrics
  if (record$variant == "references") {
    metrics <- lapply(record$references, `[[`, "metrics")
    metrics <- Filter(Negate(is.null), metrics)
    if (!is.null(record$oracle)) metrics$known_function <- record$oracle
  }
  for (model in names(metrics)) {
    score <- vapply(metrics[[model]], function(value) {
      if (is.null(value)) NA_real_ else as.numeric(value)
    }, numeric(1))
    # The earliest harness JSON used arrays; raw predictions remain authoritative.
    case <- readRDS(file.path(output, "cases", paste0(record$scenario, ".rds")))
    prediction_path <- file.path(dirname(path), "predictions.rds")
    prediction <- if (model == "known_function") case$oracle else readRDS(prediction_path)[[model]]
    independent <- stress_metrics(case$evaluation$y, prediction, case$task)
    stopifnot(isTRUE(all.equal(unname(score), unname(independent), tolerance = 1e-12)))
    row <- data.frame(scenario = record$scenario, variant = record$variant, model = model,
      primary = identical(model, record$primary),
      primary_family = if (is.null(record$primary_family)) NA_character_ else record$primary_family,
      task = record$task, training_rows = nrow(case$training), evaluation_rows = nrow(case$evaluation),
      predictors = ncol(case$training) - 1L, elapsed_seconds = record$elapsed_seconds)
    for (name in names(independent)) row[[name]] <- independent[[name]]
    rows[[length(rows) + 1L]] <- row
  }
}

# A timed-out process may never write a result. Keep it in the operational
# evidence rather than accidentally reporting only conditions that finished.
for (process in processes) {
  matched <- which(vapply(operational, function(record) {
    identical(record$scenario, process$scenario) && identical(record$variant, process$variant)
  }, logical(1)))
  if (length(matched)) {
    operational[[matched[[1L]]]]$process_status <- process$status
    operational[[matched[[1L]]]]$process_exit_code <- process$exit_code
  } else {
    operational[[length(operational) + 1L]] <- list(scenario = process$scenario,
      variant = process$variant, status = process$status,
      process_exit_code = process$exit_code, elapsed_seconds = process$elapsed_seconds,
      error = "Process ended without a saved result; see its process.log and processes.json.")
  }
}
write_json(operational, file.path(output, run, "operational-summary.json"))
if (!length(rows)) stop("No completed predictions are available; operational failures were retained.")

all_columns <- unique(unlist(lapply(rows, names)))
table <- do.call(rbind, lapply(rows, function(row) {
  for (missing in setdiff(all_columns, names(row))) row[[missing]] <- NA
  row[all_columns]
}))
write.csv(table, file.path(output, run, "scores.csv"), row.names = FALSE, na = "")

paired <- list()
for (scenario in unique(table$scenario)) {
  path <- file.path(output, run, scenario)
  if (!all(file.exists(file.path(path, c("core", "stronger"), "predictions.rds")))) next
  case <- readRDS(file.path(output, "cases", paste0(scenario, ".rds")))
  core <- readRDS(file.path(path, "core", "predictions.rds"))$main_model
  stronger <- readRDS(file.path(path, "stronger", "predictions.rds"))$main_model
  loss <- function(prediction) {
    if (case$task == "regression") return((case$evaluation$y - prediction)^2)
    event <- as.integer(case$evaluation$y == "yes")
    bounded <- pmax(1e-15, pmin(1 - 1e-15, prediction))
    -(event * log(bounded) + (1 - event) * log1p(-bounded))
  }
  a <- loss(core)
  b <- loss(stronger)
  difference <- function(rows) {
    if (case$task == "regression") sqrt(mean(b[rows])) - sqrt(mean(a[rows])) else mean(b[rows] - a[rows])
  }
  set.seed(4242L)
  draws <- replicate(1000L, difference(sample.int(length(a), length(a), replace = TRUE)))
  paired[[scenario]] <- list(metric = if (case$task == "regression") "rmse" else "log_loss",
    stronger_minus_core = difference(seq_along(a)),
    percentile_interval_95 = as.list(setNames(unname(quantile(draws, c(0.025, 0.975))), c("lower", "upper"))),
    draws = 1000L,
    scope = "Paired evaluation-row bootstrap conditional on these fixed fits; excludes training and selection variability. The bank contact rows may include repeated customers.")
}
write_json(paired, file.path(output, run, "paired-comparisons.json"))
print(table[table$primary | table$variant == "references",
  intersect(c("scenario", "variant", "model", "primary_family", "rmse", "log_loss", "average_precision"), names(table))],
  row.names = FALSE)
