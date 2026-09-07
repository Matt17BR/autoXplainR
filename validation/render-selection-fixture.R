# Deliberately synthetic evidence, not claims about fitted model performance.
# Independent oracle: neural_01 has fold RMSE 1,3 with validation sizes10,30.
# Its pooled RMSE=sqrt(7); delta SE=sqrt(20)/(2*sqrt(7)); threshold is their sum.
# tree_01 (score3, smaller capacity proxy) is the policy choice but its refit fails;
# tree_02 (score3.2) becomes the final primary. Linear3.6 is outside the threshold.
# Run from repository root. No training, external engine or stochastic fit is used.
pkgload::load_all(quiet = TRUE)
output <- Sys.getenv("SELECTION_CASES", "/tmp/autoxplain-selection-cases")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
plan <- local_tuning_plan(5, n = 40, p = 2, task = "regression", seed = 314)
ids <- plan$configuration_id
scores <- c(linear_01 = 3.6, tree_01 = 3, neural_01 = sqrt(7), tree_02 = 3.2, neural_02 = NA_real_)
se <- c(linear_01 = 0, tree_01 = 0, neural_01 = sqrt(20) / (2 * sqrt(7)), tree_02 = 0, neural_02 = NA_real_)
candidates <- plan[c(
  "configuration_id", "family", "backend", "model", "hyperparameters",
  "simplicity_rank", "complexity_definition", "complexity_proxy"
)]
candidates$cv_score <- unname(scores[ids])
candidates$cv_se <- unname(se[ids])
candidates$status <- ifelse(ids == "neural_02", "failed", "ok")
candidates$selected <- ids == "tree_01"
candidates$retained_model_id <- c(
  linear_01 = "linear_alternative", tree_01 = NA_character_,
  tree_02 = "main_model", neural_01 = "neural_alternative", neural_02 = NA_character_
)[ids]
folds <- do.call(rbind, lapply(seq_along(ids), function(index) {
  id <- ids[[index]]
  values <- if (id == "neural_01") c(1, 3) else rep(scores[[id]], 2)
  row <- data.frame(
    configuration_id = id, fold = 1:2, score = values,
    training_rows = c(30L, 10L), validation_rows = c(10L, 30L),
    fit_seed = 314L, optimization_status = if (id == "neural_02") "not_converged" else "converged",
    warning = "", error = if (id == "neural_02") "Fixture iteration limit" else ""
  )
  row$requested_parameters <- I(rep(plan$parameters[index], 2))
  row$effective_parameters <- row$requested_parameters
  row$learned <- I(rep(list(list(source = "synthetic browser acceptance fixture")), 2))
  row
}))
refit <- list(fallback_used = TRUE, status = "partial", attempts = data.frame(
  configuration_id = c("tree_01", "tree_02"), status = c("failed", "ok"),
  optimization_status = c("failed", "not_applicable"), error = c("Fixture refit failure", "")
))
tuning <- structure(list(
  schema_version = 5L, metric = "rmse", folds_used = 2L,
  scope_note = "Synthetic arithmetic and rendering fixture; not fitted performance evidence.",
  plan = plan, candidates = candidates, fold_scores = folds,
  search_space = attr(plan, "search_space"),
  selection = tuning_selection_record(candidates, "tree_01", "one_se", "rmse"),
  selected_configuration = "tree_01", final_configuration = "tree_02", selection_rule = "one_se",
  refit = refit, control = list(optimization_policy = "exclude")
), class = "autoxplain_tuning")
result <- structure(list(task = "regression", tuning = tuning, provenance = list(seed = 314L)),
  class = "autoxplain_result"
)
asset <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
html <- paste0(
  '<!doctype html><html lang="en"><meta charset="utf-8">',
  '<meta name="viewport" content="width=device-width,initial-scale=1">',
  "<title>Selection arithmetic fixture</title><style>",
  report_css(), asset("inst/report/selection.css"),
  "body{padding:14px}#selection{max-width:1100px;margin:auto}</style><body>",
  "<p>Synthetic acceptance fixture: these are hand-specified losses, not fitted-model performance claims.</p>",
  render_model_selection(result), "<script>", asset("inst/report/selection.js"), "</script></body></html>"
)
writeLines(html, file.path(output, "selection-oracle.html"))
writeLines(
  as.character(jsonlite::toJSON(list(candidates = candidates, folds = folds),
    dataframe = "rows", auto_unbox = TRUE, na = "null", digits = 16
  )),
  file.path(output, "selection-source.json")
)
cat(file.path(output, "selection-oracle.html"), "\n")
