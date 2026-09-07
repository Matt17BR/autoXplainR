# Literal measurements and expected frontier vertices, independent of chart helpers.
# Run from the repository root; render-chart-fixture.R includes this fixture in CI.
local({
  if (!exists("tradeoff_chart", mode = "function")) pkgload::load_all(quiet = TRUE)
  out <- Sys.getenv("EXPLORER_CASES", "/tmp/autoxplain-explorer-cases")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  checks <- list()
  assert <- function(label, condition) {
    checks[[length(checks) + 1L]] <<- list(check = label, passed = isTRUE(condition))
    if (!isTRUE(condition)) stop(label)
  }
  fake <- function(board) structure(list(
    leaderboard = board, task = "regression",
    evaluation = list(primary_metric = "rmse")
  ), class = "autoxplain_result")
  board <- data.frame(
    model_id = LETTERS[1:11],
    model = c("Baseline", "Compact", "Dominated", "Accurate", "Accurate twin", "Same cost worse",
      "Same score dearer", "No cost", "Infinite loss", "Not a number", "Infinite cost"),
    role = c("baseline", rep("candidate", 10)),
    rmse = c(8, 5, 6, 3, 3, 7, 3, 0, Inf, NaN, 0),
    r_squared = c(-.2, .2, .1, .7, .7, -.1, .7, 1, Inf, NaN, 1),
    model_size_kb = c(1, 2, 3, 4, 4, 2, 6, NA, 0, 0, Inf),
    training_time_ms = c(10, 2, 1, 5, 5, 3, 6, NA, 0, 0, Inf)
  )
  cases <- list()
  add_case <- function(id, source, metric, resource, expected, path, include_baseline = TRUE) {
    trade <- model_tradeoffs(fake(source), metric, resource, include_baseline)
    got <- sort(trade$model_id[trade$pareto_optimal])
    assert(paste(id, "literal nondominated IDs"), identical(got, sort(expected)))
    finite <- is.finite(source[[metric]]) & is.finite(source[[resource]])
    if (!include_baseline) finite <- finite & source$role != "baseline"
    assert(paste(id, "finite rows retained exactly"), setequal(trade$model_id, source$model_id[finite]))
    cases[[id]] <<- list(
      id = id, metric = metric, resource = resource, higher = identical(metric, "r_squared"),
      rows = source[finite, ], expected = expected, path = path, chart = tradeoff_chart(trade)
    )
  }
  add_case("loss_ties", board, "rmse", "model_size_kb", c("A", "B", "D", "E"),
    rbind(c(1, 8), c(2, 8), c(2, 5), c(4, 5), c(4, 3)))
  add_case("r2_negative", board, "r_squared", "model_size_kb", c("A", "B", "D", "E"),
    rbind(c(1, -.2), c(2, -.2), c(2, .2), c(4, .2), c(4, .7)))
  add_case("time_switch", board, "rmse", "training_time_ms", c("B", "C", "D", "E"),
    rbind(c(1, 6), c(2, 6), c(2, 5), c(5, 5), c(5, 3)))
  add_case("without_baseline", board, "rmse", "model_size_kb", c("B", "D", "E"),
    rbind(c(2, 5), c(4, 5), c(4, 3)), FALSE)
  single <- board[1:3, ]
  single$rmse <- c(1, 2, 3)
  add_case("single_frontier", single, "rmse", "model_size_kb", "A", rbind(c(1, 1)))
  duplicates <- board[1:3, ]
  duplicates$model_size_kb <- c(1, 1, 2)
  duplicates$rmse <- c(2, 2, 3)
  add_case("duplicate_frontier", duplicates, "rmse", "model_size_kb", c("A", "B"), rbind(c(1, 2)))
  equal_cost <- board[1:3, ]
  equal_cost$model_size_kb <- c(2, 2, 2)
  equal_cost$rmse <- c(3, 1, 2)
  add_case("equal_cost", equal_cost, "rmse", "model_size_kb", "B", rbind(c(2, 1)))
  equal_score <- board[1:3, ]
  equal_score$rmse <- c(2, 2, 2)
  add_case("equal_score", equal_score, "rmse", "model_size_kb", "A", rbind(c(1, 2)))
  tiny <- board[1:3, ]
  tiny$model_size_kb <- c(1, 2, 3)
  tiny$rmse <- c(1 + 1e-12, 1, 1 + 2e-12)
  add_case("near_tie", tiny, "rmse", "model_size_kb", c("A", "B"),
    rbind(c(1, 1 + 1e-12), c(2, 1 + 1e-12), c(2, 1)))
  switch_metric <- board[1:4, ]
  switch_metric$mae <- c(2, 3, 4, 1.5)
  add_case("metric_switch", switch_metric, "mae", "model_size_kb", c("A", "D"),
    rbind(c(1, 2), c(4, 2), c(4, 1.5)))
  nonconvex <- board[1:3, ]
  nonconvex$rmse <- c(10, 9, 2)
  add_case("nonconvex", nonconvex, "rmse", "model_size_kb", c("A", "B", "C"),
    rbind(c(1, 10), c(2, 10), c(2, 9), c(3, 9), c(3, 2)))
  zero_time <- board[1:3, ]
  zero_time$training_time_ms <- c(0, 0, 2)
  zero_time$rmse <- c(8, 5, 4)
  add_case("zero_time", zero_time, "rmse", "training_time_ms", c("B", "C"),
    rbind(c(0, 5), c(2, 5), c(2, 4)))
  html <- paste0(
    '<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
    '<title>Independent Pareto counterexamples</title><style>', report_css(),
    readChar("inst/report/explorer.css", file.info("inst/report/explorer.css")$size),
    'body {margin:0;padding:12px;background:white}.review-case{max-width:560px;margin:0 0 30px}',
    '</style><body class="explorer">',
    paste(vapply(cases, function(x) paste0('<section class="review-case" data-review-case="', x$id,
      '"><h2>', x$id, '</h2>', x$chart, '</section>'), character(1)), collapse = ""),
    '<script>', readChar("inst/report/charts.js", file.info("inst/report/charts.js")$size), '</script></body></html>'
  )
  writeLines(html, file.path(out, "frontier-oracle.html"))
  jsonlite::write_json(lapply(cases, function(x) {x$chart <- NULL; x}), file.path(out, "frontier-source.json"),
    auto_unbox = TRUE, pretty = TRUE, digits = NA)
  jsonlite::write_json(checks, file.path(out, "frontier-r-checks.json"), auto_unbox = TRUE, pretty = TRUE)
  cat(length(checks), "independent literal R checks passed\n")
})
