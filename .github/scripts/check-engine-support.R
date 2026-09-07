arguments <- commandArgs(trailingOnly = TRUE)
known <- arguments %in% c("--install", "--tests", "--live-h2o") |
  grepl("^--(library|report|engines)=.+$", arguments)
if (any(!known) || anyDuplicated(sub("=.*$", "", arguments))) {
  stop(paste("Usage: check-engine-support.R [--install] [--tests] [--live-h2o]",
             "[--engines=h2o,...] [--library=PATH] [--report=PATH]"),
       call. = FALSE)
}
option_value <- function(name) {
  supplied <- arguments[startsWith(arguments, paste0("--", name, "="))]
  if (!length(supplied)) return(NULL)
  sub(paste0("^--", name, "="), "", supplied)
}
install <- "--install" %in% arguments
run_tests <- "--tests" %in% arguments
run_live_h2o <- "--live-h2o" %in% arguments
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
all_engines <- c("e1071", "earth", "glmnet", "h2o", "kknn", "mgcv", "ranger", "xgboost")
selected <- option_value("engines")
engines <- if (is.null(selected)) all_engines else strsplit(selected, ",", fixed = TRUE)[[1L]]
if (!length(engines) || anyDuplicated(engines) || any(!engines %in% all_engines)) {
  stop("--engines must name unique supported engines separated by commas.", call. = FALSE)
}
if (run_tests && !setequal(engines, all_engines)) {
  stop("--tests requires all eight engines; use --live-h2o for the H2O-only gate.", call. = FALSE)
}
if (run_live_h2o && !"h2o" %in% engines) {
  stop("--live-h2o requires h2o in the selected engines.", call. = FALSE)
}
pins <- vapply(all_engines, function(package) {
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
  stop("Every selected engine must match its exact DESCRIPTION minimum.", call. = FALSE)
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

scope <- "Selected engine versions and all registry declarations checked; fitting tests not yet run."
write_engine_report <- function(scope) {
  if (is.null(report_path)) return(invisible(NULL))
  dir.create(report_path, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(versions, file.path(report_path, "engine-versions.csv"), row.names = FALSE)
  writeLines(c(scope, "", capture.output(utils::sessionInfo())),
             file.path(report_path, "session-info.txt"))
  saveRDS(utils::installed.packages()[, c("Package", "Version", "LibPath", "Built"), drop = FALSE],
          file.path(report_path, "installed-packages.rds"))
}
# Retain successful version provenance even when a following fitting test fails.
write_engine_report(scope)
if (run_tests) {
  testthat::test_local(
    ".", filter = "native-engines|kernel-geometry|matrix-blueprint|audit-data-contracts|gam-model-settings",
    stop_on_failure = TRUE
  )
  scope <- paste(
    "Native adapters, geometry, matrix encoding, and audit data contracts passed.",
    "H2O package loading/version verified; no Java cluster or live H2O fit was requested."
  )
}
if (run_live_h2o) {
  Sys.setenv(AUTOXPLAIN_RUN_H2O = "true")
  java <- if (nzchar(Sys.getenv("JAVA_HOME"))) {
    file.path(Sys.getenv("JAVA_HOME"), "bin", "java")
  } else {
    Sys.which("java")
  }
  if (!nzchar(java) || !file.exists(java)) stop("A supported Java runtime is required for --live-h2o.")
  java_version <- system2(java, "-version", stdout = TRUE, stderr = TRUE)
  if (!is.null(report_path)) writeLines(java_version, file.path(report_path, "java-version.txt"))
  h2o::h2o.init(nthreads = 2, max_mem_size = "2G")
  if (!is.null(report_path)) {
    writeLines(capture.output(h2o::h2o.clusterInfo()), file.path(report_path, "h2o-cluster-info.txt"))
  }
  results <- tryCatch(
    testthat::test_local(".", filter = "h2o", stop_on_failure = TRUE),
    finally = try(h2o::h2o.shutdown(prompt = FALSE), silent = TRUE)
  )
  if (!is.null(report_path)) saveRDS(results, file.path(report_path, "h2o-test-results.rds"))
  scope <- paste("Exact selected engine versions and registry declarations checked.",
                 "H2O preparation and live binary, regression, and multiclass integration passed.")
}
if (!run_tests && !run_live_h2o) {
  scope <- "Exact selected engine versions and registry declarations checked; no fitting tests requested."
}
message(scope)
write_engine_report(scope)
