# Inspect recorded search decisions without refitting or computing explanations.
# This isolated UI fixture is not a complete report acceptance run.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: Rscript render-saved-selection.R model.rds output-directory")
pkgload::load_all(quiet = TRUE)
result <- readRDS(args[[1L]])
stopifnot(inherits(result, "autoxplain_result"))
output <- args[[2L]]
dir.create(output, recursive = TRUE, showWarnings = FALSE)
asset <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
html <- paste0(
  '<!doctype html><html lang="en"><meta charset="utf-8">',
  '<meta name="viewport" content="width=device-width,initial-scale=1">',
  '<title>Recorded development search</title><style>', report_css(),
  asset("inst/report/explorer.css"), asset("inst/report/selection.css"),
  'body{padding:16px}#selection,.fixture-note{max-width:1100px;margin:auto}</style>',
  '<body class="explorer"><p class="fixture-note">Recorded development search: ',
  nrow(result$training_data), ' training rows. This page tests the search interface; ',
  'it is not a complete model report.</p>', render_model_selection(result),
  '<script>', asset("inst/report/explorer.js"), '</script><script>',
  asset("inst/report/selection.js"), '</script></body></html>'
)
report <- file.path(output, "saved-selection.html")
writeLines(html, report)
evidence_file <- file.path(output, "saved-selection-source.json")
jsonlite::write_json(tuning_evidence(result), evidence_file,
  dataframe = "rows", auto_unbox = TRUE, na = "null", digits = 16, pretty = TRUE
)
source <- sort(c(list.files("R", full.names = TRUE), list.files("inst/report", full.names = TRUE)))
hash_file <- function(path) digest::digest(file = path, algo = "sha256")
jsonlite::write_json(list(
  created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  scope = "Recorded development search UI only; no fitting, scoring or explanation computation.",
  input_sha256 = hash_file(args[[1L]]),
  source_sha256 = as.list(stats::setNames(vapply(source, hash_file, character(1)), source)),
  artifacts_sha256 = as.list(stats::setNames(
    vapply(c(report, evidence_file), hash_file, character(1)), basename(c(report, evidence_file))
  ))
), file.path(output, "saved-selection-manifest.json"), auto_unbox = TRUE, pretty = TRUE)
cat(normalizePath(report), "\n")
