# Deliberately known values cover near ties, tiny effects, uneven bins and long labels.
# Run from the repository root; the normal fixture generator sources this file.
if (!exists("report_chart_frame", mode = "function")) pkgload::load_all(quiet = TRUE)
chart_output <- Sys.getenv("EXPLORER_CASES", "/tmp/autoxplain-explorer-cases")
dir.create(chart_output, recursive = TRUE, showWarnings = FALSE)
trade <- data.frame(model_id = c("linear", "neural", "tree", "baseline"), model = c("Linear regression", "Neural network", "Decision tree", "Intercept-only baseline"), role = c("primary", "alternative", "alternative", "baseline"), rmse = c(2.997, 3.007, 3.952, 7.565), model_size_kb = c(116.1, 106.4, 85.08, 79.7), pareto_optimal = c(TRUE, TRUE, TRUE, TRUE))
attr(trade, "performance_metric") <- "rmse"
attr(trade, "complexity_metric") <- "model_size_kb"
attr(trade, "higher_is_better") <- FALSE
one <- structure(data.frame(distance_km = c(0, 2, 10, 30, 100, 400), accumulated_effect = c(-.01, -.008, -.003, .003, .007, .015), conf_low = c(-.012, -.01, -.005, .001, .005, .013), conf_high = c(-.008, -.006, -.001, .005, .009, .017), n = c(NA, 2, 8, 40, 15, 2), support = c(.05, .05, .2, 1, .375, .05)), method = "ale", prediction_target = "predicted delivery hours")
one[c("accumulated_effect", "conf_low", "conf_high")] <- one[c("accumulated_effect", "conf_low", "conf_high")] * 1e-5
two <- one
two$accumulated_effect <- two$accumulated_effect * 1.5
two$conf_low <- two$accumulated_effect - .00000002
two$conf_high <- two$accumulated_effect + .00000002
category <- structure(data.frame(service = c("Economy service", "Priority dispatch", "Oversized parcel with scheduled freight"), partial_dependence = c(25, 21, 28), conf_low = c(23, 20, 25), conf_high = c(27, 22, 31), support = c(1, .8, .15)), method = "pdp", prediction_target = "predicted delivery hours")
res <- list(task = "regression", provenance = list(target_units = "hours"))
frames <- c(tradeoff_chart(trade), effect_chart(one, "distance_km", res, model_id = "Linear regression", comparison = list("Neural network" = two)), effect_chart(category, "service", res, model_id = "Linear regression"))
nav <- '<aside class="sidebar"><a class="wordmark" href="#overview">AutoXplainR</a><nav class="explorer-nav"><a href="#overview" data-page-link="overview">Compare models</a><a href="#patterns" data-page-link="patterns">Explore features</a></nav></aside>'
header <- '<div class="report-body"><header class="workspace-header"><h1>Adversarial chart fixtures</h1></header><main>'
controls <- '<select id="feature-model-select" class="model-select"><option>Linear regression</option><option>Neural network</option></select><select id="comparison-model-select"><option value="">No comparator</option><option>Linear regression</option><option>Neural network</option></select>'
html <- paste0('<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Chart review</title><style>', report_css(), readChar("inst/report/explorer.css", file.info("inst/report/explorer.css")$size), '</style><body class="explorer">', nav, header, '<section class="workspace-page" id="overview"><h2>Model comparison</h2>', frames[1], '</section><section class="workspace-page" id="patterns"><h2>Feature comparison</h2>', controls, frames[2], frames[3], "</section></main></div><script>", readChar("inst/report/explorer.js", file.info("inst/report/explorer.js")$size), "</script><script>", readChar("inst/report/charts.js", file.info("inst/report/charts.js")$size), "</script></body></html>")
writeLines(html, file.path(chart_output, "chart-oracle.html"))

# A wider portfolio puts ten inexpensive fits beside one large forest. This
# independently specified cluster exposed overlapping labels that four fits did
# not exercise. Keep exact numeric answers separate from the generated SVG.
dense <- data.frame(
  model_id = c("gam", "neural", "boosting", "forest", "svm", "mars", "knn", "tree", "linear", "regularized", "baseline"),
  model = c("Generalized additive model", "Neural network", "XGBoost gradient-boosted trees", "Random forest",
    "Radial support vector regression", "MARS piecewise-linear model", "Weighted nearest-neighbor regression",
    "Decision tree", "Linear regression", "Regularized linear model", "Intercept-only baseline"),
  rmse = c(.7406, .8111, .8251, .9487, 1.032, 1.037, 1.070, 1.525, 1.529, 1.534, 3.037),
  model_size_kb = c(198.7, 102.6, 26.98, 5117, 123.1, 86.84, 72.62, 72.92, 111.1, 1108, 79.7),
  pareto_optimal = c(TRUE, TRUE, TRUE, rep(FALSE, 8))
)
attr(dense, "performance_metric") <- "rmse"
attr(dense, "complexity_metric") <- "model_size_kb"
attr(dense, "higher_is_better") <- FALSE
near <- dense[c(1, 3, 5, 6, 7), ]
near$rmse <- c(1, 1.001, 1.002, 1.003, 3)
near$model_size_kb <- c(100, 100.1, 100.2, 100.3, 5000)
near$pareto_optimal <- c(TRUE, rep(FALSE, 4))
timing <- dense
timing$prediction_time_ms <- c(3, 1, 2, 7, 2, 1, 4, 1, 0, 3, 1)
timing$pareto_optimal <- c(TRUE, TRUE, rep(FALSE, 6), TRUE, FALSE, FALSE)
attr(timing, "complexity_metric") <- "prediction_time_ms"
fragments <- strsplit(html, frames[1], fixed = TRUE)[[1L]]
writeLines(paste0(fragments[1L],
  '<div class="comparison-chart" style="max-width:520px">',
  '<h3>Eleven models with a large size outlier</h3>', tradeoff_chart(dense),
  '<h3>Five nearly coincident fits</h3>', tradeoff_chart(near),
  '<h3>Short batch times with tied measurements</h3>', tradeoff_chart(timing),
  '</div>', fragments[2L]
), file.path(chart_output, "dense-chart-oracle.html"))
jsonlite::write_json(list(dense = dense, near = near, timing = timing),
  file.path(chart_output, "dense-chart-source.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA
)
