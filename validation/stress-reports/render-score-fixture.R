# Literal values isolate axis behavior from fitting and elapsed clocks.
pkgload::load_all(quiet = TRUE)
out <- file.path(Sys.getenv("AXR_STRESS_REPORTS", path.expand("~/.cache/autoxplain-stress-0.6.2/reports")), "score-scale")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
board <- data.frame(model_id = c("compact", "middle", "large"),
  model = c("Compact model", "Middle model", "Large model"),
  role = c("baseline", "candidate", "candidate"), rmse = c(100, 10, 1),
  log_loss = c(.1, .01, .001), mae = c(2, 1, 0), r_squared = c(-682, .2, .9),
  accuracy = c(.5, .8, .9), model_size_kb = c(1, 10, 100))
result <- structure(list(leaderboard = board, task = "regression", engine = "native",
  evaluation = list(primary_metric = "rmse")), class = "autoxplain_result")
metrics <- c("rmse", "log_loss", "mae", "r_squared", "accuracy")
models <- list(table = board, metrics = metrics, resources = "model_size_kb")
html <- paste0('<!doctype html><html lang="en"><meta charset="utf-8">',
  '<meta name="viewport" content="width=device-width,initial-scale=1">',
  '<title>Score scale controls</title><style>', AutoXplainR:::report_css(),
  AutoXplainR:::report_asset("explorer.css"),
  'body.explorer{display:block;padding:12px;margin:0;background:white}.workspace-page{max-width:560px}',
  '</style><body class="explorer"><main><section class="workspace-page" id="overview"><h2>Compare scores</h2>',
  '<label class="control">Score <select id="metric-select">',
  AutoXplainR:::explorer_options(metrics, metrics), '</select></label>',
  AutoXplainR:::explorer_tradeoffs(result, models), '</section></main><script>',
  AutoXplainR:::report_asset("explorer.js"), '</script><script>',
  AutoXplainR:::report_asset("charts.js"), '</script></body></html>')
writeLines(html, file.path(out, "scores.html"))
