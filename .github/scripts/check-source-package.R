arguments <- commandArgs(trailingOnly = TRUE)

if (length(arguments) != 2L) {
  stop(
    "Usage: check-source-package.R <source-archive> <check-directory>",
    call. = FALSE
  )
}

archive <- normalizePath(arguments[[1L]], mustWork = TRUE)
check_directory <- normalizePath(arguments[[2L]], mustWork = TRUE)

result <- rcmdcheck::rcmdcheck(
  path = archive,
  args = "--as-cran",
  check_dir = check_directory,
  error_on = "warning"
)

if (!identical(result$status, 0L)) {
  stop("R CMD check did not return a successful status.", call. = FALSE)
}
