# Usage: Rscript validation/stress-grouped/run.R installed-library output-directory
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
.libPaths(c(normalizePath(args[[1L]]), .libPaths()))
library(AutoXplainR)
dir.create(args[[2L]], recursive = TRUE, showWarnings = FALSE)

set.seed(20260912)
data <- data.frame(site = rep(sprintf("site_%02d", 1:32), each = 20), feature = rnorm(640))
data$outcome <- factor(rep(c(rep("yes", 10), rep("no", 22)), each = 20), levels = c("no", "yes"))
records <- lapply(1:30, function(seed) {
  design <- AutoXplainR:::prepare_validation_design(
    data, "outcome", NULL, validation_split(group = "site"),
    0.2, seed, "base", "tuned", "binary", 5L, NULL
  )
  groups <- design$provenance$training_groups
  group_class <- vapply(split(as.character(design$training$outcome), groups), `[`, character(1), 1L)
  folds <- design$provenance$fold_ids
  data.frame(
    seed, positive_groups = sum(group_class == "yes"), negative_groups = sum(group_class == "no"),
    feasible = all(table(group_class) >= 5),
    class_coverage = all(table(folds, design$training$outcome) > 0),
    whole_groups = all(vapply(split(folds, groups), function(x) length(unique(x)) == 1L, logical(1)))
  )
})
records <- do.call(rbind, records)
public_error <- tryCatch({
  fit <- autoxplain(data, "outcome", validation = validation_split(group = "site"),
    learners = "tree", max_models = 1, nfolds = 5, seed = 1, explain = FALSE
  )
  stopifnot(all(is.finite(predict(fit, data[fit$validation$evaluation_rows, ]))))
  ""
}, error = conditionMessage)
write.csv(records, file.path(args[[2L]], "coverage.csv"), row.names = FALSE)
writeLines(c(
  paste("Installed version:", packageVersion("AutoXplainR")),
  paste("Installed path:", find.package("AutoXplainR")),
  paste("Feasible seeds:", sum(records$feasible)),
  paste("Seeds with complete class coverage:", sum(records$class_coverage)),
  paste("Public seed 1 call error:", if (nzchar(public_error)) public_error else "none")
), file.path(args[[2L]], "result.txt"))
writeLines(capture.output(sessionInfo()), file.path(args[[2L]], "session.txt"))
