# Run from the repository root. Examples are in README.md.
args <- commandArgs(trailingOnly = TRUE)
variant <- if (length(args)) args[1L] else "candidate"
n <- if (length(args) > 1L) as.integer(args[2L]) else 100000L
high_cardinality <- length(args) > 2L && args[3L] == "ids"
layout <- if (length(args) > 3L) args[4L] else "summary"
output <- Sys.getenv("AXR_SCALE_OUTPUT", path.expand("~/.cache/autoxplain-scale-0.7.0/data"))
baseline_library <- Sys.getenv("AXR_BASELINE_LIBRARY",
  path.expand("~/.cache/autoxplain-stress-0.6.2/published-release/library"))
dir.create(output, recursive = TRUE, showWarnings = FALSE)
loadNamespace("AutoXplainR", lib.loc = baseline_library)
baseline <- asNamespace("AutoXplainR")
stopifnot(as.character(getNamespaceVersion(baseline)) == "0.6.2")
implementation <- if (variant == "baseline") baseline else new.env(parent = baseline)
source_paths <- c("R/data_profile.R", "R/data_context.R")
if (variant == "candidate") for (path in source_paths) sys.source(path, implementation)
stopifnot(variant %in% c("baseline", "candidate"))
source_hashes <- as.list(tools::md5sum(source_paths))
set.seed(740L)
data <- data.frame(
  x = rnorm(n), skew = rexp(n), noise = rnorm(n),
  periodic = runif(n, -pi, pi),
  category = rep(c("a", "b", "c", "d"), length.out = n),
  rare = ifelse(seq_len(n) %% 10000L == 0L, "rare", "common")
)
data$y <- data$x + sin(data$periodic) + .2 * data$noise
if (high_cardinality) data$id <- sprintf("record_%09d", seq_len(n))
training_n <- as.integer(n * .8)
training <- data[seq_len(training_n), , drop = FALSE]
evaluation <- data[seq.int(training_n + 1L, n), , drop = FALSE]
features <- setdiff(names(data), c("y", "id"))
processed_training <- training[c(features, "y")]
processed_evaluation <- evaluation[c(features, "y")]
capture_elapsed <- system.time(context <- baseline$capture_data_context(
  training, evaluation, "y", features,
  list(data = processed_training, row_indices = seq_len(nrow(training))),
  list(data = processed_evaluation, row_indices = seq_len(nrow(evaluation)))
))["elapsed"]
result <- structure(list(training_data = processed_training, test_data = processed_evaluation,
  features = features, target_column = "y", data_context = context,
  provenance = list(seed = 740L), task = "regression"), class = "autoxplain_result")
shared_numeric_columns <- if (isTRUE(capabilities("profmem"))) {
  vapply(features[vapply(training[features], is.numeric, logical(1))], function(name) {
    raw_address <- tracemem(context$raw$training[[name]])
    processed_address <- tracemem(result$training_data[[name]])
    untracemem(context$raw$training[[name]])
    untracemem(result$training_data[[name]])
    identical(raw_address, processed_address)
  }, logical(1))
} else NULL
control <- implementation$report_data_control(if (layout == "summary") "summary" else "rows",
  context_columns = if (high_cardinality) "id" else character(),
  max_rows = if (layout %in% c("columns-full", "records-full")) n else 5000L)
arguments <- list(result = result, report_data = control)
if (layout %in% c("columns", "columns-full")) arguments$row_layout <- "columns"
gc()
elapsed <- system.time(export <- do.call(implementation$prepare_data_explorer, arguments))["elapsed"]
stopifnot(identical(source_hashes, as.list(tools::md5sum(source_paths))))
# Whole-column totals and exact rare-level counts are checked independently of
# the pair sampler, including when the single rare group is absent from a sample.
for (stage in c("raw", "processed")) {
  for (partition in c("training", "evaluation")) {
    expected <- if (partition == "training") training else evaluation
    for (name in c(features, "y")) {
      distribution <- export$profile$stages[[stage]]$columns[[name]][[partition]]
      stopifnot(distribution$n_total == nrow(expected), sum(distribution$counts) == nrow(expected))
      if (is.numeric(expected[[name]])) {
        stopifnot(identical(distribution$mean, mean(expected[[name]])))
        stopifnot(isTRUE(all.equal(unlist(distribution$quantiles, use.names = FALSE),
          unname(quantile(expected[[name]], c(0, .25, .5, .75, 1))), tolerance = 0)))
      }
    }
    rare_profile <- export$profile$stages[[stage]]$columns$rare
    rare_bin <- match("rare", rare_profile$axis$levels)
    stopifnot(rare_profile[[partition]]$counts[rare_bin] == sum(expected$rare == "rare"))
  }
}
if (high_cardinality) {
  id_profile <- export$profile$stages$raw$columns$id
  stopifnot(id_profile$training$n_unique == training_n,
    identical(id_profile$training$counts, c(rep(1L, 20L), training_n - 20L, 0L)),
    identical(id_profile$evaluation$counts, c(rep(0L, 21L), n - training_n)))
  if (variant == "candidate") {
    id_pairs <- Filter(function(pair) pair$x == "id" || pair$y == "id", export$profile$stages$raw$pairs)
    stopifnot(all(vapply(id_pairs, function(pair) pair$training$association$status == "unavailable", logical(1))))
  }
}
size_shared <- if (requireNamespace("lobstr", quietly = TRUE)) {
  list(result = as.numeric(lobstr::obj_size(result)),
       context = as.numeric(lobstr::obj_size(context)),
       raw_and_processed = as.numeric(lobstr::obj_size(context$raw, result$training_data, result$test_data)),
       row_map = as.numeric(lobstr::obj_size(context$row_map)), export = as.numeric(lobstr::obj_size(export)))
} else NULL
measurement <- list(variant = variant, total_rows = n, training_rows = training_n,
  columns = ncol(data), high_cardinality = high_cardinality, layout = layout,
  capture_elapsed_seconds = unname(capture_elapsed), prepare_elapsed_seconds = unname(elapsed),
  retained_export_size_bytes = as.numeric(object.size(export)),
  shared_object_sizes_bytes = size_shared,
  raw_processed_training_share_numeric_columns = as.list(shared_numeric_columns),
  pair_sample = export$profile$stages$raw$pair_sampling,
  rows_exported = export$manifest$individual_records,
  exact_distributions_verified = TRUE,
  baseline_package_version = "0.6.2", source_hashes = if (variant == "candidate") source_hashes else NULL,
  r_version = R.version.string)
name <- paste(variant, n, if (high_cardinality) "ids" else "mixed", layout, sep = "-")
jsonlite::write_json(measurement, file.path(output, paste0(name, ".json")), auto_unbox = TRUE,
  pretty = TRUE, null = "null", digits = 16)
cat(jsonlite::toJSON(measurement, auto_unbox = TRUE, pretty = TRUE, null = "null"), "\n")
