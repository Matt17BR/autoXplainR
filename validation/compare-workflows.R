# Run from the repository root. Optional reference packages may live in an
# isolated library selected by AUTOXPLAIN_COMPARISON_LIBRARY. No installation,
# telemetry, live service, tuning or human performance measurement occurs here.
comparison_library <- Sys.getenv("AUTOXPLAIN_COMPARISON_LIBRARY",
                                 "/tmp/autoxplain-workflow-library")
if (dir.exists(comparison_library)) .libPaths(c(comparison_library, .libPaths()))
required <- c("DALEX", "modelStudio", "r2d3", "iml", "digest", "jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install reference packages in an isolated library: ",
                          paste(missing, collapse = ", "))
if (dir.exists("R")) pkgload::load_all(quiet = TRUE) else library(AutoXplainR)
artifact_dir <- Sys.getenv("AUTOXPLAIN_COMPARISON_ARTIFACTS",
                           "/tmp/autoxplain-workflow-comparison")
result_dir <- "validation/results"
dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
seed <- 20260907L
set.seed(seed)
n <- 400L
data <- data.frame(
  distance_km = round(stats::rgamma(n, shape = 2, rate = 0.15), 2),
  package_count = as.numeric(sample.int(12L, n, replace = TRUE)),
  traffic_index = round(stats::runif(n, 0, 1), 3),
  dispatch_hour = as.numeric(sample(8:20, n, replace = TRUE)),
  rain = factor(sample(c("dry", "rain"), n, replace = TRUE, prob = c(.7, .3)))
)
data$delivery_minutes <- 12 + 1.8 * data$distance_km + 0.9 * data$package_count +
  16 * data$traffic_index + 0.3 * (data$dispatch_hour - 8) +
  4 * (data$rain == "rain") + stats::rnorm(n, sd = 5)
rownames(data) <- sprintf("synthetic_delivery_%03d", seq_len(n))
# The explicit split is fixed before fitting or explanation. Both branches use
# these exact predictors/outcomes and the very same fitted R model objects.
evaluation_rows <- sort(sample.int(n, 100L))
training_rows <- setdiff(seq_len(n), evaluation_rows)
training <- data[training_rows, , drop = FALSE]
evaluation <- data[evaluation_rows, , drop = FALSE]
result <- autoxplain(training, "delivery_minutes", test_data = evaluation,
                     evaluation_role = "test", task = "regression",
                     model_set = "quick", seed = seed, explain = FALSE)
stopifnot(identical(result$test_data, evaluation))
model_ids <- c("main_model", "simple_baseline")
x <- result$test_data[result$features]
y <- result$test_data[[result$target_column]]
models <- result$models[model_ids]
ours <- as_explainers(result, models = model_ids)
dalex <- lapply(model_ids, function(id) {
  DALEX::explain(models[[id]], data = x, y = y, label = id,
                 type = "regression", verbose = FALSE)
})
names(dalex) <- model_ids
for (id in model_ids) {
  stopifnot(identical(dalex[[id]]$model, models[[id]]),
            identical(dalex[[id]]$data, ours[[id]]$data),
            identical(dalex[[id]]$y, ours[[id]]$y),
            isTRUE(all.equal(as.numeric(dalex[[id]]$y_hat),
                             as.numeric(predict(ours[[id]], x)), tolerance = 0)))
}
performance <- lapply(dalex, DALEX::model_performance)
metrics <- do.call(rbind, lapply(model_ids, function(id) {
  auto <- result$evaluation$metrics[[id]]
  # DALEX's mad is median absolute error, not MAE. Compare common native
  # RMSE/R2, and calculate MAE explicitly from DALEX's retained residuals.
  reference <- c(rmse = performance[[id]]$measures$rmse,
                 mae = mean(abs(dalex[[id]]$residuals)),
                 r_squared = performance[[id]]$measures$r2)
  data.frame(model = id, metric = names(reference), autoxplain = unname(auto[names(reference)]),
             dalex = unname(reference), absolute_error = abs(unname(auto[names(reference)] - reference)),
             reference_method = c("DALEX model_performance rmse", "mean absolute DALEX residual",
                                  "DALEX model_performance r2"))
}))
stopifnot(all(metrics$absolute_error < 1e-10))
utils::write.csv(metrics, file.path(result_dir, "workflow-comparison-metrics.csv"), row.names = FALSE)

# Same RMSE increase target, full evaluation sample, 20 shuffles in each tool.
# Distinct internal RNG consumption means individual shuffles are not paired.
importance <- lapply(seq_along(model_ids), function(index) {
  id <- model_ids[[index]]
  auto <- calculate_permutation_importance(ours[[id]], metric = "rmse", n_repeats = 20, seed = seed)
  set.seed(seed)
  reference <- DALEX::model_parts(dalex[[id]], type = "difference", N = NULL, B = 20,
                                  loss_function = DALEX::loss_root_mean_square)
  reference <- reference[reference$variable %in% result$features, , drop = FALSE]
  means <- tapply(reference$dropout_loss, reference$variable, mean)
  data.frame(model = id, feature = auto$feature, autoxplain_mean = auto$importance,
             dalex_mean = unname(means[auto$feature]), repeats = 20L,
             interpretation = "Independent Monte Carlo draws; differences are not a superiority test")
})
utils::write.csv(do.call(rbind, importance),
                 file.path(result_dir, "workflow-comparison-importance.csv"), row.names = FALSE)

# iml supplies independent numerical implementations. The same requested grid
# and same observations are used; ALE shape is compared after anchoring since
# finite-bin centering conventions differ between packages.
effect_metrics <- do.call(rbind, lapply(model_ids, function(id) {
  predictor <- iml::Predictor$new(models[[id]], data = x, y = y)
  pdp <- explain_effect(ours[[id]], feature = "distance_km", method = "pdp", n_points = 16)
  pdp_reference <- iml::FeatureEffect$new(predictor, "distance_km", method = "pdp",
                                          grid.points = pdp$distance_km)$results
  pdp_reference <- pdp_reference[match(pdp$distance_km, pdp_reference$distance_km), ]
  ale <- explain_effect(ours[[id]], feature = "distance_km", method = "ale", n_points = 16)
  ale_reference <- iml::FeatureEffect$new(predictor, "distance_km", method = "ale",
                                          grid.points = ale$distance_km)$results
  ale_reference <- ale_reference[match(ale$distance_km, ale_reference$distance_km), ]
  pdp_error <- max(abs(pdp$partial_dependence - pdp_reference$.value))
  ale_error <- max(abs((ale$accumulated_effect - ale$accumulated_effect[[1L]]) -
                         (ale_reference$.value - ale_reference$.value[[1L]])))
  stopifnot(is.finite(pdp_error), pdp_error < 1e-10,
            is.finite(ale_error), ale_error < 1e-10)
  data.frame(model = id, method = c("PDP values", "ALE shape anchored at minimum"),
             reference = "iml", rows = nrow(x),
             grid_points = c(nrow(pdp), nrow(ale)), max_absolute_error = c(pdp_error, ale_error))
}))
utils::write.csv(effect_metrics, file.path(result_dir, "workflow-comparison-effects.csv"), row.names = FALSE)

# Native artifacts. modelStudio works with one explainer per widget, so the
# established branch emits one studio for each of the identical retained fits.
auto_path <- file.path(artifact_dir, "autoxplain-delivery.html")
render_model_report(result, auto_path, title = "Synthetic delivery-time prediction",
                    target_units = "minutes", top_features = length(result$features),
                    n_repeats = 20, max_models = 2)
studios <- lapply(model_ids, function(id) {
  set.seed(seed)
  widget <- modelStudio::modelStudio(
    dalex[[id]], new_observation = x[1:2, , drop = FALSE], new_observation_y = y[1:2],
    N = nrow(x), N_fi = nrow(x), N_sv = nrow(x), B = 20, B_fi = 20,
    max_features = length(result$features), eda = TRUE,
    show_info = FALSE, telemetry = FALSE, widget_id = paste0("studio_", id)
  )
  path <- file.path(artifact_dir, paste0("dalex-modelstudio-", id, ".html"))
  r2d3::save_d3_html(widget, path, selfcontained = TRUE,
                     title = paste("Synthetic delivery-time prediction:", id))
  widget
})
names(studios) <- model_ids
# An explicit hand-written index exposes DALEX's two-model metric evidence;
# it is labelled as script glue and is not credited as a native modelStudio UI.
metric_rows <- apply(metrics, 1, function(row) {
  paste0("<tr><td>", row[["model"]], "</td><td>", row[["metric"]], "</td><td>",
         row[["dalex"]], "</td></tr>")
})
index_path <- file.path(artifact_dir, "dalex-comparison-index.html")
index_html <- c(
  '<!doctype html><html lang="en"><meta charset="utf-8"><title>DALEX comparison</title>',
  "<body><h1>DALEX / modelStudio: synthetic delivery-time prediction</h1>",
  "<p>This index is comparison-script glue. Both studios use the same held-out rows and fits as AutoXplainR.</p>",
  "<p>Target unit: minutes. Training rows: 300. Evaluation rows: 100. Model selection: pre-specified linear model.</p>",
  "<table><caption>DALEX evaluation measures; MAE calculated from retained residuals</caption>",
  "<tr><th>Model</th><th>Metric</th><th>Value</th></tr>", metric_rows, "</table>",
  '<ul><li><a href="dalex-modelstudio-main_model.html">Primary model studio</a></li>',
  '<li><a href="dalex-modelstudio-simple_baseline.html">Intercept baseline studio</a></li></ul>',
  "<p>These fitted explanations are not causal estimates. Human comprehension and task success were not measured.</p>",
  "</body></html>"
)
writeLines(index_html, index_path)
paths <- c(auto_path, index_path,
           file.path(artifact_dir, paste0("dalex-modelstudio-", model_ids, ".html")))
manifest <- data.frame(
  artifact = basename(paths), workflow = c("AutoXplainR", rep("DALEX + modelStudio", 3)),
  implementation = c("native report", "comparison-script index", "native modelStudio", "native modelStudio"),
  exists = file.exists(paths), bytes = as.numeric(file.info(paths)$size),
  sha256 = unname(vapply(paths, digest::digest, character(1), file = TRUE, algo = "sha256")),
  interactive_browser_check = "Not performed by this R script"
)
stopifnot(all(manifest$exists), all(manifest$bytes > 1000L))
utils::write.csv(manifest, file.path(result_dir, "workflow-comparison-artifacts.csv"), row.names = FALSE)
coverage <- data.frame(
  task = c("Identify training/evaluation boundary", "Compare primary and baseline RMSE",
           "Inspect repeated permutation importance", "Inspect fitted feature effects",
           "Open shareable HTML without running an R server", "Inspect local case explanations",
           "Interpret uncertainty and dependence scope", "Demonstrate better novice task success"),
  autoxplain = c("native report provenance", "native report comparison", "native report and retained draws",
                 "native report ALE/PDP", "native standalone HTML", "not generated in this workflow",
                 "native scoped diagnostics", "not measured"),
  dalex_modelstudio = c("script index; split managed explicitly", "DALEX measures plus script index",
                        "native modelStudio feature importance and DALEX draws",
                        "native modelStudio profiles", "native self-contained HTML",
                        "native modelStudio break-down / Shapley panels",
                        "requires analyst interpretation; no matched AutoXplainR audit added",
                        "not measured"),
  evidence_kind = c(rep("Generated artifact / retained object", 7), "Unexecuted human-study question")
)
utils::write.csv(coverage, file.path(result_dir, "workflow-comparison-coverage.csv"), row.names = FALSE)
versions <- vapply(c("AutoXplainR", required), function(package) {
  as.character(utils::packageVersion(package))
}, character(1))
writeLines(c(
  "Synthetic post-fit workflow comparison; no human participants or superiority claims.",
  paste("Seed:", seed), paste("Shared data SHA256:", digest::digest(data, algo = "sha256")),
  paste("Training rows:", paste(training_rows, collapse = ",")),
  paste("Evaluation rows:", paste(evaluation_rows, collapse = ",")),
  paste("Model objects and ordered evaluation content asserted identical:", paste(model_ids, collapse = ",")),
  "No timing comparison: work performed by the report generators differs.",
  "modelStudio telemetry explicitly disabled. Packages loaded from isolated library where configured.",
  paste(names(versions), versions), trimws(capture.output(sessionInfo()), which = "right")
), file.path(result_dir, "workflow-comparison-session.txt"))
saveRDS(list(result = result, dalex = dalex, studios = studios), file.path(artifact_dir, "comparison-objects.rds"))
# Optional replayable offline browser smoke; no timing or user-performance claim.
browser_python <- Sys.getenv("AUTOXPLAIN_COMPARISON_PYTHON", "")
if (nzchar(browser_python)) {
  browser_script <- file.path(artifact_dir, "browser-check.py")
  browser_csv <- file.path(result_dir, "workflow-comparison-browser.csv")
  writeLines(c(
    "from pathlib import Path",
    "from playwright.sync_api import sync_playwright",
    "import csv",
    "import hashlib",
    "import sys",
    "",
    "root = Path(sys.argv[1])",
    "output = Path(sys.argv[2])",
    "rows = []",
    "with sync_playwright() as playwright:",
    "    browser = playwright.chromium.launch(headless=True)",
    "    for file in sorted(root.glob('*.html')):",
    "        page = browser.new_page(viewport={'width': 1100, 'height': 900})",
    "        errors = []",
    "        page.on('pageerror', lambda error: errors.append(str(error)))",
    "        # All artifacts are checked with network requests blocked.",
    "        page.route('https://**/*', lambda route: route.abort())",
    "        page.route('http://**/*', lambda route: route.abort())",
    "        page.goto(file.as_uri(), wait_until='load')",
    "        if 'modelstudio' in file.name:",
    "            page.wait_for_selector('select#inputVar')",
    "            page.select_option('select#inputVar', 'traffic_index')",
    "            assert page.locator('select#inputVar').input_value() == 'traffic_index'",
    "            interaction = 'feature selector changed to traffic_index'",
    "        elif file.name.startswith('autoxplain'):",
    "            summary = page.locator('details > summary').first",
    "            summary.click()",
    "            assert summary.locator('..').get_attribute('open') is not None",
    "            interaction = 'details disclosure opened'",
    "        else:",
    "            assert page.locator('a[href$=\".html\"]').count() == 2",
    "            interaction = 'both local studio links present'",
    "        page.wait_for_timeout(700)",
    "        assert len(page.locator('body').inner_text()) > 200",
    "        assert not errors, errors",
    "        if 'index' not in file.name:",
    "            assert page.locator('svg').count() >= 1",
    "        rows.append({",
    "            'artifact': file.name,",
    "            'sha256': hashlib.sha256(file.read_bytes()).hexdigest(),",
    "            'browser': 'Chromium ' + browser.version,",
    "            'network': 'HTTP and HTTPS blocked',",
    "            'status': 'passed',",
    "            'javascript_errors': len(errors),",
    "            'svg_count': page.locator('svg').count(),",
    "            'interaction': interaction,",
    "            'scope': 'render/control smoke check only; no human comprehension or usability result'",
    "        })",
    "        page.close()",
    "    browser.close()",
    "with output.open('w', newline='') as handle:",
    "    writer = csv.DictWriter(handle, fieldnames=rows[0].keys())",
    "    writer.writeheader()",
    "    writer.writerows(rows)",
    "print('Offline browser smoke passed for', len(rows), 'artifacts')"
  ), browser_script)
  status <- system2(browser_python, c(shQuote(browser_script), shQuote(artifact_dir), shQuote(browser_csv)))
  stopifnot(status == 0L)
  browser_results <- utils::read.csv(browser_csv, stringsAsFactors = FALSE)
  selected <- match(manifest$artifact, browser_results$artifact)
  stopifnot(!anyNA(selected), all(browser_results$sha256[selected] == manifest$sha256))
  manifest$interactive_browser_check <- browser_results$status[selected]
  utils::write.csv(manifest, file.path(result_dir, "workflow-comparison-artifacts.csv"), row.names = FALSE)
}
print(metrics)
print(effect_metrics)
print(manifest)
