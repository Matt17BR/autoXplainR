test_that("disagreement matches hand calculations and never exports unrequested records", {
  data <- data.frame(x = 1:4, y = c(1, 2, 3, 4))
  rownames(data) <- paste0("PRIVATE-ROW-", 1:4)
  rule <- function(model, newdata) model$offset + newdata$x
  result <- evaluate_models(
    list(a = list(offset = 0), b = list(offset = 2), c = list(offset = -1)),
    data, "y", predict_functions = list(a = rule, b = rule, c = rule), features = "x"
  )
  original <- result
  for (mode in c("summary", "none")) {
    result$.report_export <- AutoXplainR:::prepare_data_explorer(result, report_data_control(mode))
    view <- AutoXplainR:::report_disagreement_view(result)
    pairs <- view$pairs
    expect_equal(pairs$mean_prediction_distance, c(3, 2, 1))
    expect_equal(pairs$p90_prediction_distance, c(3, 2, 1))
    expect_null(view$cases)
    html <- AutoXplainR:::explorer_disagreement(result)
    expect_false(grepl("PRIVATE-ROW-|data-select-row", html))
  }
  result$.report_export <- AutoXplainR:::prepare_data_explorer(result, report_data_control("rows", max_rows = 2L))
  view <- AutoXplainR:::report_disagreement_view(result)
  expect_length(view$cases, 2L)
  expect_true(all(vapply(view$cases, `[[`, numeric(1), "gap") == 3))
  keys <- vapply(result$.report_export$rows, `[[`, character(1), "row_key")
  expect_true(all(vapply(view$cases, `[[`, character(1), "row_key") %in% keys))
  result$.report_export <- NULL
  expect_identical(result, original)
})

test_that("disagreement uses the recorded event, reference and original source positions", {
  data <- data.frame(x = 1:4, event = factor(c("yes", "no", "yes", "no"), levels = c("yes", "no")))
  rule <- function(model, newdata) model$p[newdata$x]
  result <- evaluate_models(
    list(a = list(p = c(.5, .2, .8, .1)), b = list(p = c(.4, .3, .9, .2)), ref = list(p = rep(.5, 4))),
    data, "event", positive = "yes", reference = "ref", features = "x",
    predict_functions = list(a = rule, b = rule, ref = rule)
  )
  view <- AutoXplainR:::report_disagreement_view(result)
  expect_equal(view$pairs$class_disagreement_rate, .25)
  expect_equal(view$pairs$mean_prediction_distance, .1)
  expect_setequal(view$performance$model_id, c("a", "b"))
  result$models$b <- NULL
  expect_null(AutoXplainR:::report_disagreement_view(result))
})

test_that("public three-model row reports link the largest gaps to original retained positions", {
  for (task in c("regression", "binary")) {
    raw <- data.frame(x = seq_len(10L))
    raw$y <- if (task == "regression") raw$x else factor(rep(c("no", "yes"), 5L))
    data <- raw
    rule <- if (task == "regression") {
      function(model, newdata) newdata$x * model$scale
    } else {
      function(model, newdata) .1 + newdata$x * model$scale / 40
    }
    result <- evaluate_models(
      list(a = list(scale = 1), b = list(scale = 2), c = list(scale = 3)),
      data, "y", task = task, predict_functions = list(a = rule, b = rule, c = rule),
      features = "x", positive = if (task == "binary") "yes" else NULL
    )
    expected_sources <- 10:6
    expected_gaps <- expected_sources * if (task == "regression") 2 else 1 / 20
    original <- result
    views <- lapply(c("records", "columns"), function(layout) {
      exported <- result
      exported$.report_export <- AutoXplainR:::prepare_data_explorer(
        result, report_data_control("rows", max_rows = 10L), row_layout = layout
      )
      AutoXplainR:::report_disagreement_view(exported)
    })
    expect_equal(views[[1L]], views[[2L]])
    for (view in views) {
      expect_equal(view$n, nrow(data))
      expect_equal(vapply(view$cases, `[[`, numeric(1), "source_row"), expected_sources)
      expect_equal(vapply(view$cases, `[[`, numeric(1), "gap"), expected_gaps)
      expect_identical(vapply(view$cases, `[[`, character(1), "row_key"), paste0("data:", expected_sources))
    }
    path <- tempfile(fileext = ".html")
    expect_no_error(render_model_report(result, path,
      report_data = report_data_control("rows", max_rows = 10L),
      uncertainty = FALSE, n_repeats = 2L, top_features = 1L
    ))
    html <- paste(readLines(path, warn = FALSE), collapse = "\n")
    section <- regmatches(html, regexpr('<ul class="disagreement-records">.*?</ul>', html, perl = TRUE))
    links <- regmatches(section, gregexpr('data-select-row="[^"]+"', section))[[1L]]
    expect_identical(links, paste0('data-select-row="data:', expected_sources, '"'))
    expect_identical(result, original)
    unlink(path)
  }
})

test_that("disagreement ranks only exported evaluation rows and checks every mapped position", {
  ambiguity <- data.frame(evaluation_row = 1:8, prediction_range = c(100, 2, 10, 1, 4, 5, 6, 7))
  meta <- data.frame(
    row_key = paste0("data:", 21:28), source = "data", source_row = 21:28,
    partition = c("training", rep("evaluation", 7L)), retained = c(TRUE, FALSE, rep(TRUE, 6L)),
    processed_position = c(1L, NA_integer_, 3:8)
  )
  export <- list(mode = "rows", rows = list(layout = "columns-v1", length = 8L, meta = meta))
  cases <- AutoXplainR:::report_disagreement_cases(export, ambiguity, "regression")
  expect_identical(vapply(cases, `[[`, character(1), "row_key"), paste0("data:", c(23L, 28L, 27L, 26L, 25L)))
  export$rows$meta$processed_position[4L] <- 999L
  expect_error(AutoXplainR:::report_disagreement_cases(export, ambiguity, "regression"), "evaluation positions")
  export$rows$meta <- meta[FALSE, ]
  export$rows$length <- 0L
  expect_length(AutoXplainR:::report_disagreement_cases(export, ambiguity, "regression"), 0L)
})
