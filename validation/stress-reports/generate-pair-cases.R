# The source rows are separate from HTML so browser calculations can be checked
# against observations, not copied report aggregates.
library_path <- Sys.getenv("AXR_STRESS_LIBRARY")
if (nzchar(library_path)) {
  .libPaths(c(library_path, .libPaths()))
  library(AutoXplainR)
} else {
  pkgload::load_all(quiet = TRUE)
}
output <- Sys.getenv("AXR_STRESS_REPORTS", path.expand("~/.cache/autoxplain-stress-0.6.2/reports"))
folder <- file.path(output, Sys.getenv("AXR_STRESS_PAIR_RUN", "pair-cases"))
dir.create(folder, recursive = TRUE, showWarnings = FALSE)
set.seed(260913L)
n <- 120L
data <- as.data.frame(matrix(rnorm(n * 31L), nrow = n))
names(data) <- sprintf("noise_%02d", seq_len(31L))
data$pair_a <- rep(c(-3, -2, -1, 0, 1, 2, 3, 4), length.out = n)
data$pair_b <- data$pair_a * 2 + sample(c(-1, 0, 1), n, TRUE)
data$category_a <- factor(rep(c("West", "North", "South"), length.out = n))
data$category_b <- factor(ifelse(data$pair_a > 0, "high", "low"))
data$response <- data$pair_a^2 + data$noise_01 + rnorm(n, sd = 0.2)
data$pair_a[c(7, 90)] <- NA_real_
data$pair_b[c(10, 100)] <- NA_real_
data$category_a[c(11, 101)] <- NA
train <- data[1:80, ]
test <- data[81:120, ]
result <- autoxplain(train, "response", test_data = test, learners = "tree",
  max_models = 1L, nfolds = 3L, explain = FALSE, seed = 260913L)
# A single shuffle deliberately leaves intervals unavailable for the tree's two
# used predictors. The grouped diagnostic must retain both records and links.
audit <- audit_explanations(as_explainers(result), features = c("pair_a", "pair_b"), n_repeats = 1L, seed = 260913L)
for (case in c("all", "sampled", "summary", "limited-columns")) {
  control <- switch(case,
    all = report_data_control("rows"), sampled = report_data_control("rows", max_rows = 30L),
    summary = report_data_control("summary"),
    `limited-columns` = report_data_control("rows", columns = c("pair_a", "category_a")))
  render_model_report(result, file.path(folder, paste0(case, ".html")), audit = audit,
    uncertainty = FALSE, report_data = control, title = "Relationships beyond the aggregate budget")
}
source <- lapply(c("training", "evaluation"), function(split) {
  raw <- if (split == "training") train else test
  processed <- if (split == "training") result$training_data else result$evaluation_data
  lapply(seq_len(nrow(raw)), function(i) list(partition = split, source_row = i,
    raw = as.list(raw[i, ]), processed = as.list(processed[i, ])))
})
jsonlite::write_json(unlist(source, recursive = FALSE), file.path(folder, "source.json"),
  auto_unbox = TRUE, pretty = TRUE, digits = 16, na = "null")
saveRDS(list(result = result, audit = audit), file.path(folder, "result.rds"))
jsonlite::write_json(audit$findings[audit$findings$code == "shuffle_interval_unresolved", c("model", "feature", "evidence")],
  file.path(folder, "shuffle-findings.json"), auto_unbox = TRUE, pretty = TRUE)
# Non-finite context values are legal in a supplied-model report: the fitted
# model uses a different column. These must not become zero or valid ranks.
train$pair_a[c(2, 4)] <- c(Inf, -Inf)
test$pair_b[c(2, 4)] <- c(Inf, -Inf)
context_result <- evaluate_models(list(linear = lm(response ~ noise_01, data = train)),
  test, "response", training_data = train, features = names(data)[1:31])
context_audit <- audit_explanations(as_explainers(context_result), features = "noise_01", n_repeats = 2L)
render_model_report(context_result, file.path(folder, "nonfinite-context.html"),
  audit = context_audit, uncertainty = FALSE,
  report_data = report_data_control("rows", context_columns = c("pair_a", "pair_b", "category_a", "category_b")))
context_source <- lapply(c("training", "evaluation"), function(split) {
  raw <- if (split == "training") train else test
  lapply(seq_len(nrow(raw)), function(i) list(partition = split, source_row = i, raw = as.list(raw[i, ])))
})
jsonlite::write_json(unlist(context_source, recursive = FALSE), file.path(folder, "source-nonfinite.json"),
  auto_unbox = TRUE, pretty = TRUE, digits = 16, na = "null")
cat("Saved pair-selection fixtures and independent source rows in", folder, "\n")
