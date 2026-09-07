arguments <- commandArgs(trailingOnly = TRUE)
known <- arguments %in% c("--install", "--tests") |
  grepl("^--(library|report)=.+$", arguments)
if (any(!known) || anyDuplicated(sub("=.*$", "", arguments))) {
  stop("Usage: check-engine-support.R [--install] [--tests] [--library=PATH] [--report=PATH]",
       call. = FALSE)
}
option_value <- function(name) {
  supplied <- arguments[startsWith(arguments, paste0("--", name, "="))]
  if (!length(supplied)) return(NULL)
  sub(paste0("^--", name, "="), "", supplied)
}
install <- "--install" %in% arguments
run_tests <- "--tests" %in% arguments
library_path <- option_value("library")
report_path <- option_value("report")
original_libraries <- normalizePath(.libPaths(), mustWork = TRUE)
if (install && is.null(library_path)) {
  stop("Installation requires a dedicated --library path; the default library is never modified.",
       call. = FALSE)
}
if (!is.null(library_path)) {
  dir.create(library_path, recursive = TRUE, showWarnings = FALSE)
  library_path <- normalizePath(library_path, mustWork = TRUE)
  if (install && library_path %in% original_libraries) {
    stop("--install must use a dedicated library outside the current library paths.", call. = FALSE)
  }
  .libPaths(c(library_path, .libPaths()))
}

# DESCRIPTION is the single source for pins. Versions never enter shell code.
description <- read.dcf("DESCRIPTION")
entries <- trimws(strsplit(description[1L, "Suggests"], ",", fixed = TRUE)[[1L]])
engines <- c("e1071", "earth", "glmnet", "h2o", "kknn", "mgcv", "ranger", "xgboost")
pins <- vapply(engines, function(package) {
  entry <- entries[grepl(paste0("^", package, "[[:space:]]*\\("), entries)]
  if (length(entry) != 1L) stop("Expected one engine minimum in DESCRIPTION for ", package, ".")
  matched <- regmatches(entry, regexec(
    "^[A-Za-z][A-Za-z0-9.]*[[:space:]]*\\([[:space:]]*>=[[:space:]]*([0-9][0-9.-]*)[[:space:]]*\\)$",
    entry
  ))[[1L]]
  if (length(matched) != 2L) stop("Expected an explicit >= version for engine ", package, ".")
  matched[[2L]]
}, character(1))

if (install) {
  if (!requireNamespace("remotes", quietly = TRUE)) stop("Install the CI bootstrap dependency `remotes`.")
  for (package in engines) {
    remotes::install_version(
      package, version = pins[[package]], lib = library_path,
      repos = c(CRAN = "https://cloud.r-project.org"), dependencies = NA,
      upgrade = "never", force = TRUE, build_vignettes = FALSE
    )
  }
}

versions <- do.call(rbind, lapply(engines, function(package) {
  if (!requireNamespace(package, quietly = TRUE)) stop("Engine is unavailable: ", package, ".")
  installed <- utils::packageDescription(package)
  data.frame(
    package = package, declared_minimum = pins[[package]], installed = installed$Version,
    exact_minimum = utils::packageVersion(package) == numeric_version(pins[[package]]),
    library = normalizePath(dirname(find.package(package)), mustWork = TRUE),
    direct_depends = if (is.null(installed$Depends)) "" else installed$Depends,
    stringsAsFactors = FALSE
  )
}))
print(versions[c("package", "declared_minimum", "installed", "exact_minimum")], row.names = FALSE)
if (!all(versions$exact_minimum)) {
  stop("The exact minimum-version gate requires every engine to match its DESCRIPTION pin.", call. = FALSE)
}
if (!is.null(library_path) && any(versions$library != library_path)) {
  stop("Every pinned engine must resolve from the dedicated library.", call. = FALSE)
}

if (!requireNamespace("pkgload", quietly = TRUE)) stop("Install the CI bootstrap dependency `pkgload`.")
pkgload::load_all(".", quiet = TRUE)
registry <- get("autoxplain_learner_registry", asNamespace("AutoXplainR"))()
for (definition in registry) {
  if (is.null(definition$minimum_version)) next
  package <- definition$package
  if (!package %in% names(pins) ||
        numeric_version(definition$minimum_version) != numeric_version(pins[[package]])) {
    stop("Learner registry and DESCRIPTION minima disagree for ", package, ".", call. = FALSE)
  }
}

scope <- "Exact engine versions and registry declarations checked; no fitting tests requested."
if (run_tests) {
  testthat::test_local(
    ".", filter = "native-engines|kernel-geometry|matrix-blueprint|audit-data-contracts",
    stop_on_failure = TRUE
  )
  scope <- paste(
    "Native adapters, geometry, matrix encoding, and audit data contracts passed.",
    "H2O package loading/version verified; no Java cluster or live H2O fit was requested."
  )
}
message(scope)
if (!is.null(report_path)) {
  dir.create(report_path, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(versions, file.path(report_path, "engine-versions.csv"), row.names = FALSE)
  writeLines(c(scope, "", capture.output(utils::sessionInfo())),
             file.path(report_path, "session-info.txt"))
  saveRDS(utils::installed.packages()[, c("Package", "Version", "LibPath", "Built"), drop = FALSE],
          file.path(report_path, "installed-packages.rds"))
}
