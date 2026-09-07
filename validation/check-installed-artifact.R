# Run against an installed release, never pkgload source. Arguments are:
# <fresh library path> <output directory> <expected package version>.
args <- commandArgs(TRUE)
stopifnot(length(args) == 3L)
.libPaths(c(normalizePath(args[1]), .libPaths()))
library(AutoXplainR)
stopifnot(as.character(packageVersion("AutoXplainR")) == args[3])
stopifnot(normalizePath(find.package("AutoXplainR")) == file.path(normalizePath(args[1]), "AutoXplainR"))
dir.create(args[2], recursive = TRUE, showWarnings = FALSE)
set.seed(220)
binary <- data.frame(x = rnorm(200), group = factor(rep(c("a", "b"), 100)))
binary$outcome <- factor(ifelse(runif(200) < plogis(binary$x), "yes", "no"), levels = c("no", "yes"))
cases <- list(
  regression = list(mtcars, "mpg"), binary = list(binary, "outcome"),
  multiclass = list(iris, "Species")
)
for (name in names(cases)) {
  item <- cases[[name]]
  path <- file.path(args[2], paste0(name, ".html"))
  result <- autoxplain(item[[1]], item[[2]], report = path)
  stopifnot(
    inherits(result, "autoxplain_result"), result$schema_version == "2.0",
    file.info(path)$size > 10000
  )
  predictions <- predict(result, head(item[[1]], 3))
  stopifnot(all(is.finite(predictions)))
  saved <- file.path(args[2], paste0(name, ".rds"))
  saveRDS(result, saved)
  restored <- readRDS(saved)
  stopifnot(identical(predictions, predict(restored, head(item[[1]], 3))))
  stopifnot(inherits(restored$explanations$audit, "autoxplain_audit"))
  memo <- generate_natural_language_report(restored)
  render_model_report(restored, file.path(args[2], paste0(name, "-restored.html")), narrative = memo)
  evidence <- evidence_summary(restored)
  json <- file.path(args[2], paste0(name, ".json"))
  jsonlite::write_json(evidence, json, auto_unbox = TRUE, pretty = TRUE, null = "null")
  stopifnot(jsonlite::fromJSON(json)$schema_version == "2.0")
  capture.output(print(result), file = file.path(args[2], paste0(name, "-console.txt")))
  if (name == "binary") {
    stopifnot(
      all(predictions >= 0 & predictions <= 1),
      identical(levels(predict(result, binary[1:3, ], type = "class")), c("no", "yes"))
    )
  }
  if (name == "multiclass") {
    stopifnot(
      identical(dim(predictions), c(3L, 3L)),
      max(abs(rowSums(predictions) - 1)) < 1e-10
    )
  }
  cat(name, ": fit, raw predictions, report, RDS reuse, narrative, JSON export passed\n")
}
train <- data.frame(x = 1:50, category = rep(c("a", "b"), 25), y = sin(1:50))
result <- autoxplain(train, "y", explain = FALSE)
raw <- data.frame(x = c(NA, 4, 10), category = c("a", "new", "b"))
stopifnot(length(predict(result, raw)) == 3L, all(is.finite(predict(result, raw))))
cat("Missing values and novel categories: saved recipe prediction passed\n")
writeLines(capture.output(sessionInfo()), file.path(args[2], "session-info.txt"))
cat("Installed artifact smoke passed from", find.package("AutoXplainR"), "\n")
