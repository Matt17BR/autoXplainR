# Compare complete saved results after both timed processes have exited.
arguments <- commandArgs(trailingOnly = TRUE)
stopifnot(length(arguments) == 3L)
before_directory <- normalizePath(arguments[[1L]], mustWork = TRUE)
after_directory <- normalizePath(arguments[[2L]], mustWork = TRUE)
before <- jsonlite::read_json(file.path(before_directory, "measurement.json"), simplifyVector = TRUE)
after <- jsonlite::read_json(file.path(after_directory, "measurement.json"), simplifyVector = TRUE)
stopifnot(before$variant == "before", after$variant == "after", before$rows == after$rows,
          before$problem == after$problem, before$package_path == after$package_path,
          before$reference_md5 == after$reference_md5, before$draws == 1000L, after$draws == 1000L)
before_file <- file.path(before_directory, "interval.rds")
after_file <- file.path(after_directory, "interval.rds")
old <- readRDS(before_file)
current <- readRDS(after_file)
stopifnot(identical(old, current), nrow(current$draws) == 1000L, current$units == after$rows)
before_sha <- digest::digest(file = before_file, algo = "sha256", serialize = FALSE)
after_sha <- digest::digest(file = after_file, algo = "sha256", serialize = FALSE)
stopifnot(identical(before_sha, after_sha))
record <- list(problem = after$problem, rows = after$rows, draws = 1000L,
  whole_interval_object_identical = TRUE, serialized_interval_identical = TRUE,
  interval_sha256 = after_sha, before_seconds = before$public_call_seconds,
  after_seconds = after$public_call_seconds, package_path = after$package_path,
  R_version = R.version.string)
jsonlite::write_json(record, arguments[[3L]], pretty = TRUE, auto_unbox = TRUE, digits = NA)
print(record)
