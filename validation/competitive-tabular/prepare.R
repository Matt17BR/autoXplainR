script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script)), "common.R"))
cache <- tabular_cache()
manifest_file <- file.path(cache, "partitions.json")
if (file.exists(manifest_file)) stop("Partitions already exist. Do not regenerate after inspecting outcomes.")
raw <- file.path(cache, "raw")
sources <- jsonlite::read_json(file.path(raw, "sources.json"), simplifyVector = FALSE)
# Bind the manifest to the exact source files consumed below.
expected_source_members <- c(yearprediction = "YearPredictionMSD.txt",
  covertype = "covtype.data.gz", bank = "bank-additional.zip")
source_repair <- "Rerun download.py or use a new cache; do not regenerate existing partitions."
if (!is.list(sources) || !identical(sort(names(sources)), sort(names(expected_source_members)))) {
  stop("Source metadata must contain exactly yearprediction, covertype and bank. ", source_repair)
}
for (name in names(expected_source_members)) {
  if (!is.list(sources[[name]]) || !identical(sources[[name]]$member, unname(expected_source_members[[name]]))) {
    stop("Source metadata has the wrong consumed member for ", name, ". ", source_repair)
  }
}
for (name in names(sources)) {
  source_file <- file.path(raw, sources[[name]]$member)
  stopifnot(identical(tabular_hash(source_file), sources[[name]]$member_sha256))
}
# Verify extracted Bank files before any data reads or partition writes.
verify_bank_nested_sources <- function(raw, bank_source) {
  expected <- c("bank-additional-full.csv" = "bank-additional/bank-additional-full.csv",
    "bank-additional-names.txt" = "bank-additional/bank-additional-names.txt")
  repair <- paste("Rerun download.py to record and verify nested members, or use a new cache;",
    "do not regenerate existing partitions.")
  if (!is.list(bank_source) || !is.list(bank_source$nested_members)) {
    stop("Bank nested-member metadata is missing or invalid. ", repair)
  }
  nested <- bank_source$nested_members
  for (filename in names(expected)) {
    metadata <- nested[[filename]]
    valid <- is.list(metadata) && identical(metadata$member, unname(expected[[filename]])) &&
      is.character(metadata$sha256) && length(metadata$sha256) == 1L &&
      !is.na(metadata$sha256) && grepl("^[0-9a-f]{64}$", metadata$sha256) &&
      is.numeric(metadata$bytes) && length(metadata$bytes) == 1L &&
      is.finite(metadata$bytes) && metadata$bytes > 0 && metadata$bytes == floor(metadata$bytes)
    if (!valid) stop("Bank nested-member metadata is missing or invalid for ", filename, ". ", repair)
    path <- file.path(raw, filename)
    info <- file.info(path)
    if (!file.exists(path) || isTRUE(info$isdir) || !identical(unname(info$size), as.numeric(metadata$bytes)) ||
        !identical(tabular_hash(path), metadata$sha256)) {
      stop("Bank extracted source is missing or changed: ", filename, ". ", repair)
    }
  }
  invisible(TRUE)
}
verify_bank_nested_sources(raw, sources$bank)

data.table::setDTthreads(1L)
cases <- list()
save_partition <- function(name, task, data, training_rows, evaluation_rows, phase, source) {
  stopifnot(!anyDuplicated(training_rows), !anyDuplicated(evaluation_rows),
    !length(intersect(training_rows, evaluation_rows)))
  destination <- file.path(cache, "cases", name, phase)
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  training <- data[training_rows, , drop = FALSE]
  calibration_rows <- tabular_sample(seq_len(nrow(training)), training$y,
    floor(nrow(training) * .2), seed = 80703L)
  metadata <- list(name = name, task = task, phase = phase, fit_seed = 80711L,
    n_training = length(training_rows), n_evaluation = length(evaluation_rows),
    predictors = ncol(data) - 1L, source = source,
    class_levels = if (is.factor(data$y)) levels(data$y) else NULL)
  saveRDS(list(data = training, folds = tabular_folds(training$y),
    calibration_rows = calibration_rows, source_rows = training_rows,
    metadata = metadata), file.path(destination, "training.rds"), compress = FALSE, version = 3L)
  saveRDS(list(data = data[evaluation_rows, setdiff(names(data), "y"), drop = FALSE],
    source_rows = evaluation_rows), file.path(destination, "evaluation-features.rds"),
    compress = FALSE, version = 3L)
  saveRDS(data$y[evaluation_rows], file.path(destination, "evaluation-targets.rds"),
    compress = FALSE, version = 3L)
  files <- list.files(destination, pattern = "\\.rds$", full.names = TRUE)
  metadata$files <- lapply(setNames(files, basename(files)), function(path) {
    list(sha256 = tabular_hash(path), bytes = file.info(path)$size)
  })
  cases[[paste(name, phase, sep = "/")]] <<- metadata
  cat("Prepared", name, phase, length(training_rows), "/", length(evaluation_rows), "rows\n")
}

save_real <- function(name, task, data, full_training, full_evaluation, n_development, n_evaluation, source) {
  development <- tabular_sample(full_training, data$y, n_development, seed = 80717L)
  remaining <- setdiff(full_training, development)
  evaluation <- tabular_sample(remaining, data$y, n_evaluation, seed = 80719L)
  save_partition(name, task, data, development, evaluation, "development", source)
  save_partition(name, task, data, full_training, full_evaluation, "acceptance", source)
}

year <- as.data.frame(data.table::fread(file.path(raw, "YearPredictionMSD.txt"), header = FALSE,
  showProgress = FALSE, nThread = 1L))
stopifnot(nrow(year) == 515345L, ncol(year) == 91L, !anyNA(year))
names(year) <- c("y", sprintf("timbre_%02d", 1:90))
save_real("yearprediction", "regression", year, seq_len(463715L), 463716:515345,
  50000L, 20000L, "Official artist-separated outer split; internal artist IDs unavailable.")
rm(year)
gc()

cover <- as.data.frame(data.table::fread(file.path(raw, "covtype.data.gz"), header = FALSE,
  showProgress = FALSE, nThread = 1L))
stopifnot(nrow(cover) == 581012L, ncol(cover) == 55L, !anyNA(cover))
names(cover) <- c("elevation", "aspect", "slope", "horizontal_hydrology", "vertical_hydrology",
  "horizontal_roadways", "hillshade_9am", "hillshade_noon", "hillshade_3pm", "horizontal_fire",
  paste0("wilderness_", 1:4), paste0("soil_", 1:40), "y")
cover$y <- factor(cover$y, levels = 1:7,
  labels = c("spruce_fir", "lodgepole_pine", "ponderosa_pine", "cottonwood_willow", "aspen", "douglas_fir", "krummholz"))
outer_eval <- tabular_sample(seq_len(nrow(cover)), cover$y, floor(nrow(cover) * .2), seed = 80723L)
save_real("covertype", "multiclass", cover, setdiff(seq_len(nrow(cover)), outer_eval), outer_eval,
  50000L, 20000L, "Stratified same-area row split, not geographic transfer; spatial dependence possible.")
rm(cover)
gc()

bank <- as.data.frame(data.table::fread(file.path(raw, "bank-additional-full.csv"), sep = ";",
  showProgress = FALSE, nThread = 1L))
stopifnot(nrow(bank) == 41188L, ncol(bank) == 21L, !anyNA(bank))
bank$duration <- NULL
bank$y <- factor(bank$y, levels = c("no", "yes"))
outer_eval <- tabular_sample(seq_len(nrow(bank)), bank$y, floor(nrow(bank) * .2), seed = 80729L)
save_real("bank", "binary", bank, setdiff(seq_len(nrow(bank)), outer_eval), outer_eval,
  20000L, 5000L, "Contact-row split; duration removed; customer overlap and temporal dependence cannot be excluded.")

source(file.path(dirname(normalizePath(script)), "controlled-cases.R"))
controlled <- tabular_controlled_cases()
for (name in names(controlled)) {
  case <- controlled[[name]]
  save_partition(name, case$task, case$data, seq_len(case$training_rows),
    seq.int(case$training_rows + 1L, nrow(case$data)), "development",
    paste("Previously inspected development case; original SHA-256", case$original_case_sha256))
}

tabular_json(list(prepared_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  protocol_sha256 = tabular_hash(file.path(dirname(normalizePath(script)), "README.md")),
  sources_sha256 = tabular_hash(file.path(raw, "sources.json")),
  cases = cases), manifest_file)
