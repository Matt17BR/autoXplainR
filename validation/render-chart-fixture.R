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
