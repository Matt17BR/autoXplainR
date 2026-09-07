# Exercise the production cost controls with literal, clock-independent values.
local({
  if (!exists("explorer_tradeoffs", mode = "function")) pkgload::load_all(quiet = TRUE)
  out <- Sys.getenv("EXPLORER_CASES", "/tmp/autoxplain-explorer-cases")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  board <- data.frame(
    model_id = c("compact", "middle", "large"),
    model = c("Compact model", "Middle model", "Large model"),
    role = c("baseline", "candidate", "candidate"),
    rmse = c(10, 6, 2), r_squared = c(-.5, .2, .9),
    model_size_kb = c(1, 10, 100), training_time_ms = c(0, 10, 100),
    prediction_time_ms = c(-1, 10, 100), repeated_prediction_ms_per_row = rep(NA_real_, 3)
  )
  result <- structure(list(
    leaderboard = board, task = "regression", engine = "native", evaluation = list(primary_metric = "rmse")
  ), class = "autoxplain_result")
  models <- list(
    table = board, metrics = c("rmse", "r_squared"),
    resources = c("model_size_kb", "training_time_ms", "prediction_time_ms", "repeated_prediction_ms_per_row")
  )
  html <- paste0(
    '<!doctype html><html lang="en"><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width,initial-scale=1">',
    "<title>Cost scale controls</title><style>", report_css(), report_asset("explorer.css"),
    "body.explorer{display:block;padding:12px;margin:0;background:white}.workspace-page{max-width:560px}",
    '</style><body class="explorer"><main><section class="workspace-page" id="overview"><h2>Compare costs</h2>',
    '<label class="control">Score <select id="metric-select"><option value="rmse">RMSE</option>',
    '<option value="r_squared">R-squared</option></select></label>', explorer_tradeoffs(result, models),
    "</section></main><script>", report_asset("explorer.js"), "</script><script>", report_asset("charts.js"),
    "</script></body></html>"
  )
  writeLines(html, file.path(out, "cost-scale-oracle.html"))
})
