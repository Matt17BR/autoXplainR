arguments <- commandArgs(trailingOnly = TRUE)

if (length(arguments) < 2L) {
  stop(
    paste(
      "Usage: check-source-package.R <source-archive> <check-directory>",
      "[R-CMD-check arguments]"
    ),
    call. = FALSE
  )
}

archive <- normalizePath(arguments[[1L]], mustWork = TRUE)
check_directory <- normalizePath(arguments[[2L]], mustWork = TRUE)
check_arguments <- arguments[-c(1L, 2L)]
if (!length(check_arguments)) {
  check_arguments <- "--as-cran"
}

result <- rcmdcheck::rcmdcheck(
  path = archive,
  args = check_arguments,
  check_dir = check_directory,
  error_on = "warning"
)

if (!identical(result$status, 0L)) {
  stop("R CMD check did not return a successful status.", call. = FALSE)
}
