# Compare in fresh R processes, with AXR_ADAPTER_LIBRARY for the released baseline.
library_path <- Sys.getenv("AXR_ADAPTER_LIBRARY")
if (nzchar(library_path)) {
  .libPaths(c(library_path, .libPaths()))
  library(AutoXplainR)
} else {
  pkgload::load_all(quiet = TRUE)
}
mode <- Sys.getenv("AXR_MATRIX_OUTPUT", "dense")
output <- Sys.getenv("AXR_ADAPTER_OUTPUT")
if (!nzchar(output)) stop("Set AXR_ADAPTER_OUTPUT to a JSON destination.")
set.seed(423)
data <- data.frame(
  customer_group = factor(rep(sprintf("level_%04d", 1:1200), length.out = 4800L)),
  amount = rnorm(4800L), lag = rnorm(4800L), active = rep(c(TRUE, FALSE), 2400L)
)
arguments <- list(data = data)
if (mode != "dense") arguments$output <- mode
invisible(gc(reset = TRUE))
elapsed <- system.time({
  blueprint <- do.call(AutoXplainR:::fit_matrix_blueprint, arguments)
  encoded <- AutoXplainR:::bake_matrix_blueprint(blueprint, data)
})[["elapsed"]]
memory <- gc()
record <- list(
  mode = mode, rows = nrow(encoded), columns = ncol(encoded),
  matrix_class = class(encoded), matrix_bytes = as.numeric(object.size(encoded)),
  vector_heap_peak_bytes = unname(memory["Vcells", "max used"] * 8),
  elapsed_seconds = elapsed,
  package_version = as.character(packageVersion("AutoXplainR")),
  library = find.package("AutoXplainR")
)
jsonlite::write_json(record, output, pretty = TRUE, auto_unbox = TRUE, digits = 16)
print(record)
