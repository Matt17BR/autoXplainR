# Executed only by check-installed-artifact.R in a new Rscript --vanilla process.
# Read saved results and independent expected outputs; do not fit models or
# attach their engine packages to make prediction dispatch work.
args <- commandArgs(TRUE)
stopifnot(length(args) == 4L)
.libPaths(c(normalizePath(args[1]), .libPaths()))
initial_namespaces <- loadedNamespaces()
cat("Namespaces before loading AutoXplainR:", paste(initial_namespaces, collapse = ", "), "\n")
stopifnot(!any(c("nnet", "rpart") %in% initial_namespaces))
stopifnot(!any(c("package:nnet", "package:rpart") %in% search()))
library(AutoXplainR)
stopifnot(as.character(packageVersion("AutoXplainR")) == args[3])
stopifnot(normalizePath(find.package("AutoXplainR")) == file.path(normalizePath(args[1]), "AutoXplainR"))
cat("Fresh-session installed library:", find.package("AutoXplainR"), "\n")
cat("Fresh-session AutoXplainR version:", as.character(packageVersion("AutoXplainR")), "\n")
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
  if (item$report) {
    report_path <- file.path(args[2], paste0(name, "-fresh-session.html"))
    render_model_report(result, report_path, benchmark = item$benchmark,
      report_data = report_data_control("rows", max_rows = 300L))
    stopifnot(file.info(report_path)$size > 10000)
  }
  cat(name, ": restored", length(item$expected), "models; raw responses/classes match saved outputs;",
    if (item$report) "report rendered" else "saved recipe reapplied", "\n")
}
stopifnot(all(c("autoxplain_tuned_nnet", "rpart", "multinom") %in% model_classes))
stopifnot(!any(c("package:nnet", "package:rpart") %in% search()))
writeLines(capture.output(sessionInfo()), file.path(args[2], "fresh-session-info.txt"))
cat("Fresh-session installed artifact replay passed.\n")
