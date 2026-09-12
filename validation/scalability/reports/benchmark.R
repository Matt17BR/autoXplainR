# Bounded report benchmarks. Fitting performance is measured separately.
args <- commandArgs(trailingOnly = TRUE)
variant <- if (length(args)) args[[1L]] else "candidate"
case <- if (length(args) > 1L) args[[2L]] else "wide-500"
base <- Sys.getenv("AXR_SCALE_OUTPUT", path.expand("~/.cache/autoxplain-scale-0.7.0/reports"))
library_path <- Sys.getenv("AXR_BASELINE_LIBRARY",
  path.expand("~/.cache/autoxplain-stress-0.6.2/published-release/library"))
if (variant == "baseline") {
  .libPaths(c(library_path, .libPaths()))
  library(AutoXplainR)
  stopifnot(as.character(packageVersion("AutoXplainR")) == "0.6.2")
} else {
  stopifnot(variant == "candidate")
  pkgload::load_all(Sys.getenv("AXR_REPORT_SOURCE", "."), quiet = TRUE)
}
directory <- file.path(base, variant, case)
dir.create(directory, recursive = TRUE, showWarnings = FALSE)
if (case == "wide-500") {
  stored <- readRDS(Sys.getenv("AXR_WIDE_RESULT",
    path.expand("~/.cache/autoxplain-stress-0.6.2/reports/wide-500/result.rds")))
  result <- stored$result
  arguments <- list(audit = stored$audit)
  control <- report_data_control("rows", max_rows = nrow(result$training_data) + nrow(result$test_data))
  title <- "500 predictors across 80 sites"
  partitions <- list(training = result$training_data, evaluation = result$test_data)
  mapping <- result$data_context$row_map
  last <- mapping[mapping$partition == "evaluation" &
                    mapping$processed_position == nrow(result$test_data), , drop = FALSE]
  stopifnot(nrow(last) == 1L)
  jsonlite::write_json(list(
    pair = lapply(partitions, function(data) list(n = nrow(data),
      rho = cor(data$sensor_499, data$sensor_498, method = "spearman"))),
    record = list(key = last$row_key, values = as.list(result$test_data[nrow(result$test_data),
      c("sensor_499", "sensor_498", "response")]))
  ), file.path(directory, "source.json"), auto_unbox = TRUE, pretty = TRUE, digits = 16)
} else {
  n <- as.integer(case)
  stopifnot(!is.na(n), n >= 1000L, n <= 1000000L, variant != "baseline" || n <= 200000L)
  set.seed(7403L)
  training <- data.frame(x = rnorm(100L), z = runif(100L))
  training$response <- 3 * training$x - .5 * training$z + rnorm(100L, sd = .1)
  fitted <- lm(response ~ x + z, training)
  evaluation <- data.frame(x = rnorm(n), z = runif(n))
  evaluation$response <- 3 * evaluation$x - .5 * evaluation$z + rnorm(n, sd = .1)
  result <- evaluate_models(list(linear = fitted), evaluation, "response", training_data = training)
  arguments <- list(top_features = 1L, n_repeats = 2L, max_models = 1L)
  control <- report_data_control("rows", max_rows = n + nrow(training))
  title <- paste(format(n, big.mark = ",", scientific = FALSE), "evaluation rows, all records exported")
  selected <- unique(c(1L, 2L, as.integer(n / 2), n - 1L, n))
  source <- list(evaluation_rows = n, training_rows = 100L,
    rmse = sqrt(mean((predict(fitted, evaluation) - evaluation$response)^2)),
    selected = lapply(selected, function(i) list(source_row = i, values = as.list(evaluation[i, ]))),
    filtered = list(column = "x", op = "ge", value = 2,
      n_evaluation = sum(evaluation$x >= 2), n_training = sum(training$x >= 2)),
    all_extents = list(x = range(c(training$x, evaluation$x)), z = range(c(training$z, evaluation$z))))
  jsonlite::write_json(source, file.path(directory, "source.json"), auto_unbox = TRUE, pretty = TRUE, digits = 16)
}
source_root <- Sys.getenv("AXR_REPORT_SOURCE", ".")
source_files <- c(list.files(file.path(source_root, "R"), "\\.R$", full.names = TRUE),
  list.files(file.path(source_root, "inst/report"), full.names = TRUE))
fingerprints <- tools::md5sum(source_files)
prepare <- getFromNamespace("prepare_data_explorer", "AutoXplainR")
prepare_args <- list(result = result, report_data = control)
if (variant == "candidate") prepare_args$row_layout <- "columns"
prepare_seconds <- system.time(export <- do.call(prepare, prepare_args))[["elapsed"]]
prepared_bytes <- as.numeric(object.size(export))
payload_function <- getFromNamespace("report_data_payload", "AutoXplainR")
payload_args <- list(export = export)
if (variant == "candidate") payload_args$compact <- TRUE
serialize_seconds <- system.time(wire_script <- getFromNamespace("report_json_script", "AutoXplainR")(
  do.call(payload_function, payload_args), "axr-data-payload"
))[["elapsed"]]
wire_bytes <- nchar(wire_script, type = "bytes")
rm(export, wire_script, payload_args)
gc()
elapsed <- system.time(path <- do.call(render_model_report, c(list(
  result = result, output_file = file.path(directory, "report.html"), report_data = control,
  uncertainty = FALSE, title = title
), arguments)))[["elapsed"]]
if (variant == "candidate") stopifnot(identical(fingerprints, tools::md5sum(source_files)))
measurement <- list(variant = variant, case = case, render_seconds = elapsed,
  html_bytes = file.info(path)$size, prepared_data_bytes = prepared_bytes, data_wire_script_bytes = wire_bytes,
  prepare_data_seconds = prepare_seconds, serialize_data_seconds = serialize_seconds,
  data_manifest = attr(path, "data_manifest"),
  package_version = as.character(packageVersion("AutoXplainR")),
  source_md5 = if (variant == "candidate") as.list(fingerprints) else NULL)
jsonlite::write_json(measurement, file.path(directory, "measurement.json"), pretty = TRUE, auto_unbox = TRUE)
cat(jsonlite::toJSON(measurement[c("variant", "case", "render_seconds", "html_bytes")], auto_unbox = TRUE), "\n")
