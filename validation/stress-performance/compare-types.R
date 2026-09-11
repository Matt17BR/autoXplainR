# Check row-export compatibility, including supported classes and fallback values.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: Rscript compare-types.R BASELINE_LIBRARY OUTPUT_JSON")
baseline <- loadNamespace("AutoXplainR", lib.loc = normalizePath(args[1L], mustWork = TRUE))
candidate <- new.env(parent = baseline)
files <- c("R/data_profile.R", "R/report_payload.R")
for (file in files) sys.source(file, envir = candidate)
data <- data.frame(
  number = c(1, Inf, -Inf, NA, NaN, 3), integer = c(1L, NA_integer_, 3:6),
  flag = c(TRUE, FALSE, NA, TRUE, FALSE, TRUE), category = factor(c("a", NA, "c", "b", "b", "a")),
  date = as.Date(c(1, Inf, -Inf, NA, NaN, 3), origin = "1970-01-01"),
  time = as.POSIXct(c(1, Inf, -Inf, NA, NaN, 3), origin = "1970-01-01", tz = "UTC")
)
data$wrapped <- I(c(1, Inf, -Inf, NA, NaN, 3))
data$unsupported <- I(as.list(1:6))
data$complex <- c(1 + 1i, complex(real = 1, imaginary = Inf),
                  complex(real = Inf, imaginary = 1), NA_complex_,
                  complex(real = NaN, imaginary = 1), 3 + 1i)
variables <- c(names(data), "absent")
indices <- c(3L, NA_integer_, 1L, 6L, 2L, 4L, 5L)
# The legacy numeric helper warns when discarding imaginary parts for its
# nonfinite marker. This comparison preserves that behavior, not model support
# for complex predictors. The value export itself retains finite complex values.
expected <- suppressWarnings(list(
  values = lapply(indices, function(i) {
    setNames(lapply(variables, function(name) baseline$data_export_value(data[[name]], i)), variables)
  }),
  nonfinite = lapply(indices, function(i) baseline$data_nonfinite_columns(data, i, variables))
))
actual <- suppressWarnings(candidate$data_export_partition(data, indices, variables))
stopifnot(identical(actual, expected))
records <- function(value) {
  Map(function(raw, nonfinite) {
    list(raw = raw, nonfinite = list(raw = nonfinite, processed = character()))
  }, value$values, value$nonfinite)
}
old_export <- list(mode = "rows", rows = records(expected))
new_export <- list(mode = "rows", rows = records(actual))
old_html <- baseline$report_json_script(old_export, "fixture")
new_html <- candidate$report_json_script(candidate$report_data_payload(new_export), "fixture")
stopifnot(identical(old_html, new_html))
decode <- function(html) {
  jsonlite::fromJSON(sub("^<script[^>]*>", "", sub("</script>$", "", html)), simplifyVector = FALSE)
}
stopifnot(identical(decode(old_html), decode(new_html)))
record <- list(
  baseline_version = as.character(utils::packageVersion("AutoXplainR", lib.loc = args[1L])),
  source_md5 = as.list(tools::md5sum(files)), columns = variables, selected_positions = indices,
  complete_R_export_identical = TRUE, escaped_script_byte_identical = TRUE,
  decoded_full_payload_identical = TRUE,
  date_time_nonfinite = "Original numeric infinities remain in R; JSON nulls and separate markers are preserved."
)
jsonlite::write_json(record, args[2L], pretty = TRUE, auto_unbox = TRUE, na = "null")
cat("Mixed-type R exports, JSON bytes and decoded payloads match installed baseline.\n")
