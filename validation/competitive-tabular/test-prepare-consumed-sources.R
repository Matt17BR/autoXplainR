# Exercise the actual preparation guards with synthetic source bytes only.
# A valid fixture stops before the first data read; no partitions are prepared.
arguments <- commandArgs(trailingOnly = TRUE)
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
directory <- dirname(normalizePath(script))
prepare_file <- if (length(arguments)) normalizePath(arguments[[1L]]) else file.path(directory, "prepare.R")
expressions <- as.list(parse(prepare_file))
text <- vapply(expressions, function(expr) paste(deparse(expr), collapse = "\n"), character(1))
first_read <- which(grepl("data.table::fread", text, fixed = TRUE))[[1L]]
guard_call <- which(vapply(expressions, function(expr) {
  is.call(expr) && identical(expr[[1L]], as.name("verify_bank_nested_sources"))
}, logical(1)))
partition_definition <- which(grepl("^save_partition <- function", text))
stopifnot(length(guard_call) == 1L, length(partition_definition) == 1L,
  guard_call < partition_definition, guard_call < first_read,
  grepl("tabular_hash(source_file)", paste(text[seq_len(guard_call - 1L)], collapse = "\n"), fixed = TRUE))
# Execute the actual script's setup and guard. Intercept its first data read so
# even a valid fixture cannot start a dataset read or partition preparation.
expressions[[first_read]] <- quote(stop("TEST_FIRST_DATA_READ_REACHED"))
expressions <- as.expression(expressions[-1L]) # script path is supplied below
hash <- function(path) digest::digest(file = path, algo = "sha256")
write_bytes <- function(text, path) writeBin(charToRaw(text), path)
results <- list()
run_case <- function(name, mutate = function(raw, source) source, expected_message = "TEST_FIRST_DATA_READ_REACHED",
    mutate_sources = function(raw, sources) sources) {
  cache <- tempfile(paste0("bank-source-", name, "-"))
  dir.create(cache); on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  raw <- file.path(cache, "raw"); dir.create(raw)
  write_bytes("synthetic archive bytes\n", file.path(raw, "bank-additional.zip"))
  write_bytes("x;y\n1;yes\n", file.path(raw, "bank-additional-full.csv"))
  write_bytes("Synthetic names documentation.\n", file.path(raw, "bank-additional-names.txt"))
  expected <- c("bank-additional-full.csv" = "bank-additional/bank-additional-full.csv",
    "bank-additional-names.txt" = "bank-additional/bank-additional-names.txt")
  nested <- lapply(names(expected), function(filename) list(member = unname(expected[[filename]]),
    sha256 = hash(file.path(raw, filename)), bytes = file.info(file.path(raw, filename))$size))
  names(nested) <- names(expected)
  source <- list(member = "bank-additional.zip", member_sha256 = hash(file.path(raw, "bank-additional.zip")),
    nested_members = nested)
  source <- mutate(raw, source)
  outer <- c(yearprediction = "YearPredictionMSD.txt", covertype = "covtype.data.gz")
  sources <- lapply(outer, function(filename) {
    write_bytes(paste0("synthetic ", filename, " bytes\n"), file.path(raw, filename))
    list(member = filename, member_sha256 = hash(file.path(raw, filename)))
  })
  sources$bank <- source
  sources <- mutate_sources(raw, sources)
  jsonlite::write_json(sources, file.path(raw, "sources.json"), auto_unbox = TRUE, pretty = TRUE)
  files_before <- list.files(raw, full.names = TRUE)
  hashes_before <- setNames(vapply(files_before, hash, character(1)), basename(files_before))
  previous_cache <- Sys.getenv("AXR_TABULAR_DIR", unset = NA_character_)
  on.exit(if (is.na(previous_cache)) Sys.unsetenv("AXR_TABULAR_DIR") else Sys.setenv(AXR_TABULAR_DIR = previous_cache), add = TRUE)
  Sys.setenv(AXR_TABULAR_DIR = cache)
  environment <- new.env(parent = globalenv()); environment$script <- prepare_file
  observed <- tryCatch({ eval(expressions, envir = environment); "NO_ERROR" }, error = conditionMessage)
  if (!grepl(expected_message, observed, fixed = TRUE)) {
    stop(sprintf("%s: expected %s; observed %s", name, expected_message, observed))
  }
  stopifnot(
    !file.exists(file.path(cache, "cases")), !file.exists(file.path(cache, "partitions.json")),
    identical(files_before, list.files(raw, full.names = TRUE)),
    identical(hashes_before, setNames(vapply(files_before, hash, character(1)), basename(files_before))))
  results[[name]] <<- list(passed = TRUE, observed = observed,
    reached_first_read_boundary = identical(observed, "TEST_FIRST_DATA_READ_REACHED"),
    no_dataset_read_or_partition_write = TRUE, raw_bytes_unchanged = TRUE)
}
run_case("correct_fixture")
run_case("same_size_changed_csv", function(raw, source) {
  path <- file.path(raw, "bank-additional-full.csv"); before <- file.info(path)$size
  write_bytes("x;y\n2;yes\n", path)
  stopifnot(identical(before, file.info(path)$size))
  source
}, "Bank extracted source is missing or changed: bank-additional-full.csv")
run_case("old_metadata", function(raw, source) { source$nested_members <- NULL; source },
  "Bank nested-member metadata is missing or invalid")
run_case("incomplete_metadata", function(raw, source) {
  source$nested_members[["bank-additional-full.csv"]]$bytes <- NULL; source
}, "Bank nested-member metadata is missing or invalid for bank-additional-full.csv")
run_case("swapped_filename_binding", function(raw, source) {
  source$nested_members <- source$nested_members[rev(names(source$nested_members))]
  names(source$nested_members) <- rev(names(source$nested_members))
  source
}, "Bank nested-member metadata is missing or invalid for bank-additional-full.csv")
run_case("missing_names_metadata", function(raw, source) {
  source$nested_members[["bank-additional-names.txt"]] <- NULL; source
}, "Bank nested-member metadata is missing or invalid for bank-additional-names.txt")
run_case("wrong_nested_container", function(raw, source) { source$nested_members <- "invalid"; source },
  "Bank nested-member metadata is missing or invalid")
run_case("changed_names_file", function(raw, source) {
  write_bytes("Changed names documentation.\n", file.path(raw, "bank-additional-names.txt")); source
}, "Bank extracted source is missing or changed: bank-additional-names.txt")
run_case("changed_existing_archive", function(raw, source) {
  write_bytes("changed synthetic archive bytes\n", file.path(raw, "bank-additional.zip")); source
}, "is not TRUE")
run_case("missing_year_case", expected_message = "Source metadata must contain exactly", mutate_sources = function(raw, sources) {
  sources$yearprediction <- NULL; sources
})
run_case("missing_cover_case", expected_message = "Source metadata must contain exactly", mutate_sources = function(raw, sources) {
  sources$covertype <- NULL; sources
})
run_case("year_member_substitution", expected_message = "Source metadata has the wrong consumed member for yearprediction", mutate_sources = function(raw, sources) {
  sources$yearprediction <- sources$covertype; sources
})
run_case("cover_member_substitution", expected_message = "Source metadata has the wrong consumed member for covertype", mutate_sources = function(raw, sources) {
  sources$covertype <- sources$yearprediction; sources
})
run_case("unexpected_case", expected_message = "Source metadata must contain exactly", mutate_sources = function(raw, sources) {
  sources$unexpected <- sources$yearprediction; sources
})
run_case("same_size_changed_year", expected_message = "is not TRUE", mutate_sources = function(raw, sources) {
  path <- file.path(raw, "YearPredictionMSD.txt"); before <- file.info(path)$size
  write_bytes("Synthetic YearPredictionMSD.txt bytes\n", path)
  stopifnot(identical(before, file.info(path)$size)); sources
})
run_case("same_size_changed_cover", expected_message = "is not TRUE", mutate_sources = function(raw, sources) {
  path <- file.path(raw, "covtype.data.gz"); before <- file.info(path)$size
  write_bytes("Synthetic covtype.data.gz bytes\n", path)
  stopifnot(identical(before, file.info(path)$size)); sources
})
for (name in setdiff(names(results), c("correct_fixture", "changed_existing_archive",
    "same_size_changed_year", "same_size_changed_cover"))) {
  stopifnot(grepl("Rerun download.py", results[[name]]$observed, fixed = TRUE),
    grepl("new cache", results[[name]]$observed, fixed = TRUE))
}
record <- list(scope = "Synthetic source preflight only; the first real data read is intercepted. No full partition preparation.",
  prepare_sha256 = hash(prepare_file), checks = results, passed = length(results))
if (length(arguments) >= 2L) jsonlite::write_json(record, arguments[[2L]], auto_unbox = TRUE, pretty = TRUE)
cat("Passed", length(results), "consumed-source preflight checks.\n")
