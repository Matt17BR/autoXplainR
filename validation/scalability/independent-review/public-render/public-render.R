pkgload::load_all(".", quiet = TRUE)
out <- Sys.getenv("AXR_PUBLIC_RENDER_OUTPUT", file.path(path.expand("~/.cache"), "autoxplain-public-render-review"))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
ns <- asNamespace("AutoXplainR")
trace("model_report_html",
  tracer = quote(assign(".captured_report", result, envir = .GlobalEnv)), print = FALSE, where = ns
)
extract <- function(html, id) {
  marker <- paste0('<script type="application/json" id="', id, '"')
  start <- regexpr(marker, html, fixed = TRUE)
  if (start < 0L) return(NULL)
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
block <- function(value) {
  if (identical(value$encoding, "json")) {
    return(value$value)
  }
  stopifnot(identical(value$encoding, "zlib-json-v1"))
  jsonlite::fromJSON(rawToChar(memDecompress(jsonlite::base64_dec(value$data), "gzip")), simplifyVector = FALSE)
}
checks <- list()
set.seed(815)
for (task in c("regression", "binary", "multiclass")) {
  train <- data.frame(x = rnorm(84), z = rep(c("b", "a", "c"), 28))
  test <- data.frame(x = rnorm(21), z = rep(c("c", "b", "a"), 7))
  response <- function(d) {
    switch(task,
      regression = 2 * d$x + as.integer(factor(d$z)),
      binary = factor(rep(c("yes", "no", "no"), length.out = nrow(d)), c("yes", "no")),
      multiclass = factor(rep(c("third", "first", "second"), length.out = nrow(d)), c("third", "first", "second"))
    )
  }
  train$y <- response(train)
  test$y <- response(test)
  train$x[c(3, 11)] <- NA
  test$x[c(2, 9)] <- NA
  result <- autoxplain(train, "y",
    test_data = test, learners = "tree", nfolds = 2L, explain = FALSE,
    seed = 815L, preprocessing_config = list(missing_value_strategy = "drop_rows"),
    tuning_control = tuning_control(
      grids = list(tree = list(list(cp = .01, maxdepth = 3L, minsplit = 8L))),
      family_budgets = c(tree = 1L), retain_oof = FALSE
    )
  )
  immutable <- function(x) x[c("training_data", "test_data", "evaluation", "explanations", "data_context")]
  original <- serialize(immutable(result), NULL)
  for (mode in c("summary", "none", "rows")) {
    name <- paste(task, mode, sep = "-")
    file <- file.path(out, paste0(name, ".html"))
    value <- tryCatch(
      {
        path <- render_model_report(result, file,
          uncertainty = FALSE, explanation_rows = 7L, n_repeats = 2L, top_features = 1L, max_models = 1L,
          report_data = report_data_control(mode, max_rows = 105L, max_pair_rows = 5L, seed = 14L)
        )
        stopifnot(identical(serialize(immutable(result), NULL), original))
        report <- .captured_report
        stopifnot(
          identical(report$evaluation$metrics, result$evaluation$metrics),
          report$explanations$audit$config$sampling$rows_available == 19L,
          report$explanations$audit$config$sampling$rows_used == 7L
        )
        html <- paste(readLines(file, warn = FALSE), collapse = "\n")
        predictions <- extract(html, "axr-predictions-payload")
        stopifnot(length(predictions$models) > 0L)
        for (model in predictions$models) {
          stopifnot(model$n == 19L)
          if (mode != "rows") {
            stopifnot(is.null(model$cases))
          } else {
            stopifnot(model$cases$length == 19L)
            meta <- lapply(model$cases$columns, block)
            stopifnot(identical(unlist(meta$row_key, use.names = FALSE), paste0("test_data:", setdiff(1:21, c(2, 9)))))
            stopifnot(identical(unlist(meta$processed_position, use.names = FALSE), 1:19))
            p <- predict(result, result$test_data, model = model$model_id)
            if (task == "regression") stopifnot(isTRUE(all.equal(
              unlist(meta$predicted, use.names = FALSE), unname(p), tolerance = 1e-14
            )))
            if (task == "binary") stopifnot(isTRUE(all.equal(
              unlist(meta$probability, use.names = FALSE), unname(p), tolerance = 1e-14
            )))
            if (task == "multiclass") {
              q <- do.call(rbind, lapply(meta$probability, function(row) unlist(row, use.names = TRUE)))
              stopifnot(identical(colnames(q), colnames(p)), isTRUE(all.equal(unname(q), unname(p), tolerance = 1e-14)))
            }
          }
        }
        data <- extract(html, "axr-data-payload")
        if (mode == "none") {
          stopifnot(is.null(data))
        } else {
          profile <- block(data$profile)
          stopifnot(
            profile$stages$raw$columns$x$evaluation$n_total == 21L,
            profile$stages$processed$columns$x$evaluation$n_total == 19L
          )
          if (mode == "rows") {
            stopifnot(data$rows$length == 105L, sum(unlist(block(data$rows$meta$retained), use.names = FALSE)) == 101L)
            # Omitted raw rows remain inspectable but have no model prediction.
            stopifnot(identical(
              unlist(block(data$rows$meta$row_key), use.names = FALSE),
              c(paste0("data:", 1:84), paste0("test_data:", 1:21))
            ))
          } else {
            stopifnot(is.null(data$rows))
          }
        }
        list(
          status = "passed", evaluation_rows = 19L, explanation_rows = 7L,
          individual_rows = attr(path, "data_manifest")$individual_records
        )
      },
      error = function(e) list(status = "failed", reason = conditionMessage(e))
    )
    checks[[name]] <- value
    print(list(name = name, result = value))
  }
  saveRDS(result, file.path(out, paste0(task, ".rds")))
}
untrace("model_report_html", where = ns)
jsonlite::write_json(checks, file.path(out, "verdict.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")
stopifnot(all(vapply(checks, function(check) identical(check$status, "passed"), logical(1))))
