# Shared setup for the installed-package workflow; no package source is loaded.
output <- Sys.getenv("AXR_EXTENDED_DIR")
if (!nzchar(output)) stop("Set AXR_EXTENDED_DIR to an output directory outside the checkout.")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
output <- normalizePath(output, mustWork = TRUE)

library_path <- Sys.getenv("AXR_EXTENDED_LIBRARY")
if (nzchar(library_path)) {
  library_path <- normalizePath(library_path, mustWork = TRUE)
  .libPaths(c(library_path, .libPaths()))
}
library(AutoXplainR)
stopifnot(as.character(packageVersion("AutoXplainR")) == "0.6.0")
if (nzchar(library_path)) {
  stopifnot(normalizePath(find.package("AutoXplainR")) ==
    file.path(library_path, "AutoXplainR"))
}
