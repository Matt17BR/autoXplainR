read_public_report_payload <- function(html, id) {
  marker <- paste0('<script type="application/json" id="', id, '"')
  start <- regexpr(marker, html, fixed = TRUE)
  stopifnot(start > 0L)
  tail <- substring(html, start + attr(start, "match.length"))
  header_end <- regexpr(">", tail, fixed = TRUE)
  stopifnot(header_end > 0L)
  header <- substring(tail, 1L, header_end - 1L)
  body <- substring(tail, header_end + 1L)
  end <- regexpr("</script>", body, fixed = TRUE)
  stopifnot(end > 0L)
  json <- substring(body, 1L, end - 1L)
  if (grepl('data-json-chunks="', header, fixed = TRUE)) {
    count <- as.integer(sub('.*data-json-chunks="([0-9]+)".*', "\\1", header))
    stopifnot(is.finite(count), count > 0L, identical(json, ""))
    prefix <- paste0('<script type="application/octet-stream" data-json-owner="', id, '" data-json-chunk="')
    positions <- gregexpr(prefix, html, fixed = TRUE)[[1L]]
    stopifnot(length(positions) == count, all(positions > start))
    pieces <- vapply(seq_len(count), function(index) {
      tag <- paste0(prefix, index, '">')
      position <- regexpr(tag, html, fixed = TRUE)
      stopifnot(position == positions[[index]])
      remaining <- substring(html, position + attr(position, "match.length"))
      close <- regexpr("</script>", remaining, fixed = TRUE)
      stopifnot(close > 0L)
      substring(remaining, 1L, close - 1L)
    }, character(1))
    json <- paste0(pieces, collapse = "")
  }
  jsonlite::fromJSON(json, simplifyVector = FALSE)
}

read_public_report_block <- function(block) {
  if (identical(block$encoding, "json")) return(block$value)
  stopifnot(identical(block$encoding, "zlib-json-v1"))
  bytes <- memDecompress(jsonlite::base64_dec(block$data), "gzip")
  stopifnot(length(bytes) == block$bytes)
  jsonlite::fromJSON(rawToChar(bytes), simplifyVector = FALSE)
}

public_payload_fixture <- function() {
  training <- data.frame(x = 1:40, y = (1:40)^2 / 16)
  evaluation <- data.frame(x = (1:10) + .5, y = ((1:10) + .5)^2 / 16)
  training$x[3L] <- NA_real_
  evaluation$x[c(2L, 7L)] <- NA_real_
  result <- autoxplain(training, "y", test_data = evaluation,
    learners = "tree", nfolds = 2L, explain = FALSE, seed = 8L,
    preprocessing_config = list(missing_value_strategy = "drop_rows"),
    tuning_control = tuning_control(
      grids = list(tree = list(list(cp = .01, maxdepth = 3L, minsplit = 8L))),
      family_budgets = c(tree = 1L), retain_oof = FALSE
    )
  )
  list(result = result, training = training, evaluation = evaluation)
}

render_public_payload <- function(result, control) {
  path <- tempfile(fileext = ".html")
  on.exit(unlink(path))
  render_model_report(result, path,
    report_data = control, uncertainty = FALSE, n_repeats = 2L,
    top_features = 1L, max_models = 1L, explanation_rows = 6L
  )
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

test_that("public multiclass reports retain original class order and prediction probabilities", {
  training <- data.frame(x = seq_len(45L))
  training$y <- factor(rep(c("third", "_row", "first"), each = 15L), c("third", "_row", "first"))
  evaluation <- data.frame(x = c(5, 20, 35, 8, 23, 38))
  evaluation$y <- factor(c("_row", "first", "third", "third", "_row", "first"), levels(training$y))
  rownames(evaluation) <- paste0("private-row-", seq_len(nrow(evaluation)))
  model <- rpart::rpart(y ~ x, training, method = "class", control = rpart::rpart.control(xval = 0L))
  # A real class named _row must survive; incidental data-frame row names must not.
  rule <- function(model, newdata) predict(model, newdata, type = "prob")[, c("first", "third", "_row")]
  result <- evaluate_models(list(tree = model), evaluation, "y",
    training_data = training, predict_functions = list(tree = rule), features = "x"
  )
  expected <- predict(model, evaluation, type = "prob")[, levels(evaluation$y), drop = FALSE]
  # Exercise fragmented public HTML without making every unit run export a
  # megabyte fixture. The browser gate separately crosses the real threshold.
  chunk <- AutoXplainR:::report_json_chunks
  local_mocked_bindings(report_json_chunks = function(json, id, chunk_size = 1048576L) {
    chunk(json, id, chunk_size = 127L)
  })
  html <- render_public_payload(result, report_data_control("rows", max_rows = 51L))
  expect_match(html, 'id="axr-predictions-payload" data-json-chunks="', fixed = TRUE)
  payload <- read_public_report_payload(html, "axr-predictions-payload")
  expect_length(payload$models, 1L)
  cases <- payload$models[[1L]]$cases
  expect_identical(cases$layout, "case-columns-v1")
  expect_equal(cases$length, nrow(evaluation))
  columns <- lapply(cases$columns, read_public_report_block)
  actual <- do.call(rbind, lapply(columns$probability, unlist, use.names = TRUE))
  expect_identical(colnames(actual), levels(evaluation$y))
  expect_equal(unname(actual), unname(expected), tolerance = 1e-14)
  expect_identical(unlist(columns$observed, use.names = FALSE), as.character(evaluation$y))
  expect_identical(unlist(columns$row_key, use.names = FALSE), paste0("data:", seq_len(nrow(evaluation))))
  expect_equal(unlist(columns$observed_probability), expected[cbind(seq_len(nrow(evaluation)), evaluation$y)])
})

test_that("public prediction records follow source positions after preprocessing omissions", {
  fixture <- public_payload_fixture()
  result <- fixture$result
  keep <- which(!is.na(fixture$evaluation$x))
  html <- render_public_payload(result, report_data_control("rows", max_rows = 50L, max_pair_rows = 3L))
  payload <- read_public_report_payload(html, "axr-predictions-payload")
  expect_setequal(vapply(payload$models, `[[`, character(1), "model_id"), names(result$models))
  for (model in payload$models) {
    columns <- lapply(model$cases$columns, read_public_report_block)
    expected <- predict(result, fixture$evaluation[keep, ], model = model$model_id)
    expect_equal(model$n, length(keep))
    expect_equal(model$cases$length, length(keep))
    expect_identical(unlist(columns$row_key, use.names = FALSE), paste0("test_data:", keep))
    expect_identical(unlist(columns$source_row, use.names = FALSE), keep)
    expect_identical(unlist(columns$processed_position, use.names = FALSE), seq_along(keep))
    expect_equal(unlist(columns$observed), fixture$evaluation$y[keep])
    expect_equal(unlist(columns$predicted), unname(expected), tolerance = 1e-14)
    expect_equal(unlist(columns$residual), fixture$evaluation$y[keep] - unname(expected), tolerance = 1e-14)
  }
  data <- read_public_report_payload(html, "axr-data-payload")
  expect_equal(data$rows$length, nrow(fixture$training) + nrow(fixture$evaluation))
  keys <- unlist(read_public_report_block(data$rows$meta$row_key), use.names = FALSE)
  retained <- unlist(read_public_report_block(data$rows$meta$retained), use.names = FALSE)
  expect_setequal(keys[!retained], c("data:3", "test_data:2", "test_data:7"))
})

test_that("public reports keep omitted source rows without inventing evaluation predictions", {
  fixture <- public_payload_fixture()
  html <- render_public_payload(fixture$result, report_data_control("rows", max_rows = 2L, seed = 1L))
  data <- read_public_report_payload(html, "axr-data-payload")
  meta <- lapply(data$rows$meta, read_public_report_block)
  evaluation <- unlist(meta$partition, use.names = FALSE) == "evaluation"
  expect_equal(sum(evaluation), 1L)
  source_row <- unlist(meta$source_row, use.names = FALSE)[evaluation]
  expect_true(is.na(fixture$evaluation$x[source_row]))
  expect_false(unlist(meta$retained, use.names = FALSE)[evaluation])
  predictions <- read_public_report_payload(html, "axr-predictions-payload")
  expect_length(predictions$models, length(fixture$result$models))
  for (model in predictions$models) {
    expect_equal(model$n, sum(!is.na(fixture$evaluation$x)))
    expect_identical(model$cases$layout, "case-columns-v1")
    expect_equal(model$cases$length, 0L)
    expect_true(all(lengths(lapply(model$cases$columns, read_public_report_block)) == 0L))
  }
  expect_match(html, "No retained evaluation records are present in the exported row sample.", fixed = TRUE)
})
