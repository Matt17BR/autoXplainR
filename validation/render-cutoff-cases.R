# Exercise exact decision boundaries through the real existing-model report path.
# The Python oracle independently declares these scores and observed classes.
pkgload::load_all(quiet = TRUE)
cutoff_case_output <- Sys.getenv("EXPLORER_CASES", "/tmp/autoxplain-explorer-cases")
dir.create(cutoff_case_output, recursive = TRUE, showWarnings = FALSE)
cutoff_case_data <- data.frame(
  score = rep(c(0, .01, .49, .5, .57, .58, .99, 1), 2),
  outcome = factor(rep(c("yes", "no", "yes", "no"), 4), levels = c("yes", "no"))
)
cutoff_case_adapter <- function(model, newdata) {
  if (model$flip) 1 - newdata$score else newdata$score
}
cutoff_case_result <- evaluate_models(
  list('direct "score"' = list(flip = FALSE), inverse = list(flip = TRUE)),
  cutoff_case_data, "outcome", positive = "yes",
  predict_functions = list('direct "score"' = cutoff_case_adapter, inverse = cutoff_case_adapter)
)
for (cutoff_case_mode in c("rows", "summary", "none")) {
  render_model_report(
    cutoff_case_result, file.path(cutoff_case_output, paste0("cutoff-cases-", cutoff_case_mode, ".html")),
    report_data = cutoff_case_mode, top_features = 1L, n_repeats = 2L, uncertainty = FALSE
  )
}
