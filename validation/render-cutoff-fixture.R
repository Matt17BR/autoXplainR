# Literal 0.57 probability tests the exact displayed decimal threshold boundary.
# All presentation functions are real; no fitted-model performance claim is made.
pkgload::load_all(quiet = TRUE)
output <- Sys.getenv("EXPLORER_CASES", "/tmp/autoxplain-explorer-cases")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
y <- factor(c("yes", "no"), levels = c("yes", "no"))
p <- c(.57, .57)
model <- list(
  model_id = "constant", label = "Literal probability 0.57", task = "binary", n = 2L,
  positive = "yes", labels = levels(y), accuracy = .5,
  confusion = prediction_confusion(y, c("yes", "yes"), levels(y)),
  calibration = prediction_calibration_records(y, p, levels(y), "yes"),
  cutoffs = prediction_cutoff_records(y, p, "yes")
)
result <- list(models = list(constant = list()), provenance = list(primary_model_id = "constant"))
html <- paste0(
  '<!doctype html><html lang="en"><meta charset="utf-8">',
  '<meta name="viewport" content="width=device-width,initial-scale=1">',
  "<title>Literal decimal cutoff fixture</title><style>", report_css(), "</style><body>",
  '<section class="prediction-workspace"><div data-prediction-model="constant">',
  prediction_classification_html(model, result), "</div></section>",
  report_json_script(list(models = list(model)), "axr-predictions-payload"),
  "<script>", paste(readLines("inst/report/predictions.js"), collapse = "\n"), "</script></body></html>"
)
writeLines(html, file.path(output, "cutoff-decimal.html"))
