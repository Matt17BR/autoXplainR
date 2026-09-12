# Executed only by check-installed-artifact.R in a new Rscript --vanilla process.
# Read saved results and independent expected outputs; do not fit models or
# attach their engine packages to make prediction dispatch work.
args <- commandArgs(TRUE)
stopifnot(length(args) == 4L)
.libPaths(c(normalizePath(args[1]), .libPaths()))
initial_namespaces <- loadedNamespaces()
cat("Namespaces before loading AutoXplainR:", paste(initial_namespaces, collapse = ", "), "\n")
stopifnot(!any(c("nnet", "rpart", "xgboost", "mgcv") %in% initial_namespaces))
stopifnot(!any(c("package:nnet", "package:rpart", "package:xgboost", "package:mgcv") %in% search()))
library(AutoXplainR)
stopifnot(as.character(packageVersion("AutoXplainR")) == args[3])
stopifnot(normalizePath(find.package("AutoXplainR")) == file.path(normalizePath(args[1]), "AutoXplainR"))
cat("Fresh-session installed library:", find.package("AutoXplainR"), "\n")
cat("Fresh-session AutoXplainR version:", as.character(packageVersion("AutoXplainR")), "\n")

read_report_payload <- function(html, id) {
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

decode_report_block <- function(value) {
  if (identical(value$encoding, "json")) return(value$value)
  stopifnot(identical(value$encoding, "zlib-json-v1"))
  bytes <- memDecompress(jsonlite::base64_dec(value$data), "gzip")
  stopifnot(length(bytes) == value$bytes)
  jsonlite::fromJSON(rawToChar(bytes), simplifyVector = FALSE)
}

check_native_report <- function(path, expected) {
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  payload <- read_report_payload(html, "axr-predictions-payload")
  main <- Filter(function(model) identical(model$model_id, "main_model"), payload$models)
  stopifnot(length(main) == 1L)
  cases <- main[[1L]]$cases
  n <- nrow(expected$evaluation)
  stopifnot(main[[1L]]$n == n, cases$length == n, identical(cases$layout, "case-columns-v1"))
  columns <- lapply(cases$columns, decode_report_block)
  probabilities <- do.call(rbind, lapply(columns$probability, unlist, use.names = TRUE))
  stopifnot(
    identical(colnames(probabilities), expected$class_levels),
    isTRUE(all.equal(unname(probabilities), unname(expected$probabilities), tolerance = 1e-14)),
    identical(unlist(columns$observed, use.names = FALSE), as.character(expected$evaluation$outcome)),
    identical(unlist(columns$row_key, use.names = FALSE), paste0("test_data:", seq_len(n))),
    identical(unlist(columns$source_row, use.names = FALSE), seq_len(n)),
    identical(unlist(columns$processed_position, use.names = FALSE), seq_len(n))
  )
  cat("Installed native report: decoded class probabilities and original evaluation identities match\n")
}

check_compact_report <- function(path, expected) {
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  for (name in c("fflate-0.8.3.js", "payload.js", "data-explorer.js")) {
    asset <- system.file("report", name, package = "AutoXplainR")
    stopifnot(nzchar(asset), file.info(asset)$size > 0)
    stopifnot(grepl(paste(readLines(asset, warn = FALSE), collapse = "\n"), html, fixed = TRUE))
  }
  stopifnot(nzchar(system.file("report", "fflate-LICENSE.txt", package = "AutoXplainR")))
  payload <- read_report_payload(html, "axr-data-payload")
  compressed <- 0L
  block <- function(value) {
    if (identical(value$encoding, "zlib-json-v1")) compressed <<- compressed + 1L
    decode_report_block(value)
  }
  profile <- block(payload$profile)
  rows <- payload$rows
  n_training <- nrow(expected$training)
  n_evaluation <- nrow(expected$evaluation)
  total <- n_training + n_evaluation
  stopifnot(
    payload$schema_version == 2L, identical(rows$layout, "columns-v1"),
    rows$length == total, payload$manifest$individual_records == total
  )
  identities <- c(paste0("data:", seq_len(n_training)), paste0("test_data:", seq_len(n_evaluation)))
  stopifnot(identical(unlist(block(rows$meta$row_key), use.names = FALSE), identities))
  stopifnot(identical(
    unlist(block(rows$meta$source_row), use.names = FALSE), c(seq_len(n_training), seq_len(n_evaluation))
  ))
  stopifnot(identical(
    unlist(block(rows$meta$partition), use.names = FALSE),
    c(rep("training", n_training), rep("evaluation", n_evaluation))
  ))
  stopifnot(all(unlist(block(rows$meta$retained), use.names = FALSE)))
  column <- function(stage, name) {
    value <- rows[[stage]][[name]]
    if (identical(value$encoding, "reference")) {
      stopifnot(stage == "processed", value$stage == "raw", value$column == name)
      return(column("raw", name))
    }
    unlist(block(value), use.names = FALSE)
  }
  for (stage in c("raw", "processed")) {
    stopifnot(setequal(names(rows[[stage]]), names(expected$training)))
    stopifnot(identical(names(profile$stages[[stage]]$pairs), "1_2"))
    for (name in names(expected$training)) {
      # These dyadic fixture values have exact finite JSON representations.
      wanted <- c(expected$training[[name]], expected$evaluation[[name]])
      stopifnot(identical(as.numeric(column(stage, name)), as.numeric(wanted)))
      summaries <- profile$stages[[stage]]$columns[[name]]
      stopifnot(summaries$training$n_total == n_training, summaries$evaluation$n_total == n_evaluation)
    }
    for (pair in profile$stages[[stage]]$pairs) {
      stopifnot(
        pair$training$n_population == n_training, pair$evaluation$n_population == n_evaluation,
        pair$training$n_sample == expected$pair_rows, pair$evaluation$n_sample == expected$pair_rows
      )
    }
  }
  stopifnot(compressed > 0L)
  cat(
    "Installed compact report: decoded source values, row identities and full/pair denominators match;",
    compressed, "compressed blocks checked\n"
  )
}

cases <- readRDS(args[4])
model_classes <- character()
for (name in names(cases)) {
  item <- cases[[name]]
  result <- readRDS(item$saved)
  stopifnot(identical(names(result$models), names(item$expected)))
  model_classes <- union(model_classes, unlist(lapply(result$models, class), use.names = FALSE))
  for (id in names(item$expected)) {
    expected <- item$expected[[id]]
    actual <- predict(result, item$newdata, model = id)
    stopifnot(isTRUE(all.equal(actual, expected$response, tolerance = 1e-12)))
    if (!is.null(expected$class)) {
      actual_class <- predict(result, item$newdata, model = id, type = "class")
      stopifnot(identical(actual_class, expected$class))
    }
  }
  if (identical(item$checks$kind, "native_boosting")) {
    model <- result$models$main_model
    stopifnot(
      inherits(model$blueprint, "autoxplain_boosting_blueprint"),
      identical(model$parameters$encoding, "native"),
      identical(model$class_levels, item$checks$class_levels),
      model$blueprint$training_rows == item$checks$training_rows
    )
    changed <- item$newdata
    changed$category <- factor(as.character(changed$category), levels = model$blueprint$factor_levels$category)
    stopifnot(isTRUE(all.equal(predict(result, changed), item$expected$main_model$response, tolerance = 0)))
    stopifnot(isTRUE(all.equal(predict(result, item$checks$evaluation), item$checks$probabilities, tolerance = 0)))
  }
  if (identical(item$checks$kind, "auto_bam_compact")) {
    checks <- item$checks
    computation <- result$models$main_model$fit_details$computation
    stopifnot(
      inherits(result$models$main_model$fit, "bam"), computation$solver == "bam",
      computation$requested_solver == "auto", computation$planning_rows == nrow(checks$training)
    )
    prediction <- predict(result, checks$evaluation)
    actual_score <- sqrt(mean((checks$evaluation$outcome - prediction)^2))
    stopifnot(isTRUE(all.equal(actual_score, checks$score, tolerance = 1e-14)))
    sampling <- result$explanations$audit$config$sampling
    stopifnot(sampling$rows_available == nrow(checks$evaluation), sampling$rows_used == checks$explanation_rows)
    stopifnot(isTRUE(all.equal(
      unname(result$evaluation$metrics$main_model[["rmse"]]), actual_score, tolerance = 1e-14
    )))
    performance <- result$explanations$audit$performance
    stopifnot(isTRUE(all.equal(performance$score[performance$model == "main_model"], actual_score, tolerance = 1e-14)))
  }
  if (item$report) {
    report_path <- file.path(args[2], paste0(name, "-fresh-session.html"))
    options <- item$report_options
    if (is.null(options)) options <- list(report_data = report_data_control("rows", max_rows = 300L))
    do.call(render_model_report, c(
      list(result = result, output_file = report_path, benchmark = item$benchmark), options
    ))
    stopifnot(file.info(report_path)$size > 10000)
    if (identical(item$checks$kind, "native_boosting")) check_native_report(report_path, item$checks)
    if (identical(item$checks$kind, "auto_bam_compact")) check_compact_report(report_path, item$checks)
  }
  cat(
    name, ": restored", length(item$expected), "models; raw responses/classes match saved outputs;",
    if (item$report) "report rendered" else "saved recipe reapplied", "\n"
  )
}
stopifnot(all(c("autoxplain_tuned_nnet", "rpart", "multinom") %in% model_classes))
stopifnot(!any(c("package:nnet", "package:rpart", "package:xgboost", "package:mgcv") %in% search()))
writeLines(capture.output(sessionInfo()), file.path(args[2], "fresh-session-info.txt"))
cat("Fresh-session installed artifact replay passed.\n")
