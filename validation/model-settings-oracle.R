# Preserve numeric controls independently of the report's display formatter.
# Hexadecimal floats retain every bit, including neighboring doubles and thirds.
model_settings_numeric_oracle <- function(parameters) {
  numeric <- parameters[vapply(parameters, function(value) is.numeric(value) && length(value) > 0L, logical(1))]
  lapply(numeric, function(value) {
    list(
      values = as.list(sprintf("%a", as.double(value))),
      labels = if (is.null(names(value))) NULL else as.list(names(value))
    )
  })
}
