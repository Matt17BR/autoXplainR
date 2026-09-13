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
fixture_html <- function(result) paste0(
  '<!doctype html><html lang="en"><meta charset="utf-8">',
  '<meta name="viewport" content="width=device-width,initial-scale=1">',
  "<title>Selection arithmetic fixture</title><style>",
  report_css(), asset("inst/report/explorer.css"), asset("inst/report/selection.css"),
  'body{padding:14px}#selection{max-width:1100px;margin:auto}</style><body class="explorer">',
  "<p>Synthetic acceptance fixture: these are hand-specified scores, not fitted-model performance claims.</p>",
  render_model_selection(result),
  '<section class="workspace-page" id="validation"><h2>Checks</h2>',
  '<a id="failed-cv-link" href="#selection-detail-6e657572616c5f3032" data-navigate>',
  'Neural network neural_02: inspect failed CV folds</a></section>',
  "<script>", asset("inst/report/explorer.js"),
  "</script><script>", asset("inst/report/selection.js"), "</script></body></html>"
)
writeLines(fixture_html(result), file.path(output, "selection-oracle.html"))
writeLines(
  as.character(jsonlite::toJSON(list(candidates = candidates, folds = folds),
    dataframe = "rows", auto_unbox = TRUE, na = "null", digits = 16
  )),
  file.path(output, "selection-source.json")
)
# A second declared decision uses the known lowest-loss neural configuration for
# both policy and final fit. It exercises the compact exact-agreement case; the
# original fixture must continue to show its three genuinely different choices.
agreed <- result
agreed$tuning$candidates$selected <- agreed$tuning$candidates$configuration_id == "neural_01"
agreed$tuning$selection <- tuning_selection_record(agreed$tuning$candidates, "neural_01", "min", "rmse")
agreed$tuning$selected_configuration <- "neural_01"
agreed$tuning$final_configuration <- "neural_01"
agreed$tuning$selection_rule <- "min"
agreed$tuning$refit <- list(fallback_used = FALSE, status = "ok", attempts = data.frame(
  configuration_id = "neural_01", status = "ok", optimization_status = "converged", error = ""
))
writeLines(fixture_html(agreed), file.path(output, "selection-agreement.html"))
cat(file.path(output, "selection-oracle.html"), "\n")

# Adaptive fixtures below are declared records, without any engine calls.
# Screening ordering, failure states, close penalty values and monitoring
# curves have independent Python constants in check-selection.py.
adaptive_fixture <- function(metric) {
  ids <- c("regularized_01", paste0("boosting_0", 1:7))
  parameters <- c(list(list(alpha = .5, lambda_fraction = .01)), lapply(1:7, function(index) {
    list(nrounds = if (index == 7L) 4096L else 2000L, eta = .05, max_depth = 6L,
      min_child_weight = 1, subsample = .8, colsample_bytree = .8,
      reg_alpha = if (index == 2L) .1 else if (index == 7L) .10000000000000002 else 0,
      reg_lambda = if (index == 2L) 10 else if (index == 7L) 1 + 2^-52 else 1,
      encoding = "matrix")
  }))
  plan <- data.frame(
    configuration_id = ids, family = c("regularized", rep("boosting", 7L)),
    backend = c("glmnet", rep("xgboost", 7L)), model = ids,
    hyperparameters = c("alpha .5; lambda fraction .01", rep("2000-round synthetic cap", 6L), "4096-round synthetic cap"),
    simplicity_rank = c(1L, rep(2L, 7L)),
    complexity_definition = c("Synthetic penalty preference", rep("Synthetic depth preference", 7L)),
    complexity_proxy = c(1, rep(6, 7L)), stringsAsFactors = FALSE
  )
  plan$parameters <- I(parameters)
  candidates <- plan[setdiff(names(plan), "parameters")]
  auc <- identical(metric, "roc_auc")
  candidates$cv_score <- c(if (auc) .95 else .2, if (auc) .85 else .4, rep(NA_real_, 6L))
  candidates$cv_se <- c(.01, .05, rep(NA_real_, 6L))
  candidates$status <- c("ok", "ok", "screened_out", "screening_failed", "not_screened_time_limit",
    "failed", "screening_failed", "screened_out")
  candidates$selected <- ids == "regularized_01"
  candidates$retained_model_id <- c("main_model", "boosting_alternative", rep(NA_character_, 6L))
  monitoring <- function(scores, chosen, cap) list(
    status = "calibrated", selected_rounds = chosen, attempted_rounds = length(scores),
    maximum_rounds = cap, patience = length(scores) - chosen, stop_reason = "patience_reached",
    training_rows = 60L, validation_rows = 20L, metric = metric,
    curve = data.frame(round = seq_along(scores), score = if (auc) 1 - scores else scores)
  )
  screen_rounds <- monitoring(c(.9, .6, .4, .5), 3L, 128L)
  fold_rounds <- list(
    monitoring(c(.8, .55, .3, .35, .4), 3L, 2000L),
    monitoring(c(.9, .7, .5, .4, .45, .5), 4L, 2000L)
  )
  folds <- do.call(rbind, lapply(c(1L, 2L, 6L), function(index) {
    value <- if (index == 1L) c(.18, .22) else if (index == 2L) c(.35, .45) else c(NA_real_, NA_real_)
    if (auc && index == 1L) value <- c(.94, .96)
    if (auc && index == 2L) value <- c(.80, .90)
    row <- data.frame(configuration_id = ids[[index]], fold = 1:2, score = value,
      training_rows = 80L, validation_rows = 80L, fit_seed = c(2147483646L, 2147483647L),
      optimization_status = if (index == 6L) "failed" else "not_applicable", warning = "",
      error = if (index == 6L) "Synthetic CV failure after successful screening promotion" else "")
    row$requested_parameters <- I(rep(parameters[index], 2L))
    row$effective_parameters <- I(lapply(1:2, function(fold) {
      value <- parameters[[index]]
      if (index == 2L) value$nrounds <- c(3L, 4L)[[fold]]
      value
    }))
    row$learned <- I(if (index == 2L) lapply(fold_rounds, function(record) list(round_selection = record)) else rep(list(list()), 2L))
    row
  }))
  # Intentionally store screen scores in neither candidate nor rank order.
  attempted <- c(8L, 4L, 1L, 6L, 2L, 7L, 3L)
  screen_values <- c(.9, .8, .6, if (auc) .999 else .001, NA, .7, NA, .6)
  screen <- data.frame(configuration_id = ids[attempted], score = screen_values[attempted],
    status = ifelse(attempted == 4L, "failed", "ok"),
    error = ifelse(attempted == 4L, "Synthetic native screening failure", ""), warning = "",
    training_rows = 80L, validation_rows = 20L, fit_seed = 2147483647L)
  screen$requested_parameters <- I(lapply(attempted, function(index) {
    value <- parameters[[index]]
    if (index != 1L) value$nrounds <- 128L
    value
  }))
  screen$effective_parameters <- screen$requested_parameters
  screen$effective_parameters[[which(attempted == 2L)]]$nrounds <- 3L
  screen$learned <- I(lapply(attempted, function(index) {
    if (index == 2L) list(round_selection = screen_rounds) else list()
  }))
  promotion <- data.frame(configuration_id = ids, promoted = ids %in% c("regularized_01", "boosting_01", "boosting_05"),
    reason = paste("Synthetic declared promotion record for", ids))
  attempts <- data.frame(configuration_id = ids[1:2], status = "ok", optimization_status = "not_applicable", error = "")
  attempts$effective_parameters <- I(parameters[1:2])
  attempts$effective_parameters[[2L]]$nrounds <- 4L
  attempts$learned <- I(list(list(), list(round_selection = list(status = "fold_aggregate",
    selected_rounds = 4L, fold_rounds = c(`1` = 3L, `2` = 4L), calibrated_folds = 2L))))
  tuning <- structure(list(schema_version = 6L, metric = metric, folds_used = 2L, folds_requested = 2L,
    configurations_requested = 8L,
    scope_note = "Synthetic adaptive browser fixture; no models were fitted and these are not performance claims.",
    plan = plan, candidates = candidates, fold_scores = folds,
    selection = tuning_selection_record(candidates, "regularized_01", "best", metric),
    selected_configuration = "regularized_01", final_configuration = "regularized_01", selection_rule = "best",
    screening = list(metric = metric, scores = screen, promotion = promotion,
      partition = list(method = "Synthetic common training-only sample", training_rows = 80L, validation_rows = 20L)),
    refit = list(fallback_used = FALSE, status = "ok", attempts = attempts),
    control = list(search = "adaptive", screening_rows = 100L, finalists_per_family = 2L, threads = 1L,
      early_stopping = TRUE, patience = 2L, optimization_policy = "exclude")
  ), class = "autoxplain_tuning")
  structure(list(task = "binary", tuning = tuning, provenance = list(seed = 314L)), class = "autoxplain_result")
}
for (metric in c("log_loss", "roc_auc")) {
  writeLines(fixture_html(adaptive_fixture(metric)), file.path(output, paste0("selection-adaptive-", metric, ".html")))
}
