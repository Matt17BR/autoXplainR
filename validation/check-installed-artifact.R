# Run against an installed release, never pkgload source. Arguments are:
# <fresh library path> <output directory> <expected package version>.
args <- commandArgs(TRUE)
stopifnot(length(args) == 3L)
.libPaths(c(normalizePath(args[1]), .libPaths()))
library(AutoXplainR)
stopifnot(as.character(packageVersion("AutoXplainR")) == args[3])
stopifnot(normalizePath(find.package("AutoXplainR")) == file.path(normalizePath(args[1]), "AutoXplainR"))
dir.create(args[2], recursive = TRUE, showWarnings = FALSE)
replay_cases <- list()
replay_case <- function(result, saved, newdata, benchmark = NULL, report = TRUE,
                        report_options = NULL, checks = NULL) {
  expected <- lapply(names(result$models), function(id) {
    list(
      response = predict(result, newdata, model = id),
      class = if (result$task != "regression") predict(result, newdata, model = id, type = "class")
    )
  })
  names(expected) <- names(result$models)
  list(
    saved = normalizePath(saved), newdata = newdata, expected = expected,
    benchmark = benchmark, report = report, report_options = report_options, checks = checks
  )
}
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
  replay_cases[[name]] <- replay_case(restored, saved, head(item[[1]], 3))
  cat(name, ": fit, raw predictions, report, RDS reuse, narrative, JSON export passed\n")
}
train <- data.frame(x = 1:50, category = rep(c("a", "b"), 25), y = sin(1:50))
result <- autoxplain(train, "y", explain = FALSE)
raw <- data.frame(x = c(NA, 4, 10), category = c("a", "new", "b"))
stopifnot(length(predict(result, raw)) == 3L, all(is.finite(predict(result, raw))))
recipe_saved <- file.path(args[2], "recipe.rds")
saveRDS(result, recipe_saved)
replay_cases$recipe <- replay_case(result, recipe_saved, raw, report = FALSE)
cat("Missing values and novel categories: saved recipe prediction passed\n")

# Exercise the new bridge and report assets from the installed archive. The
# event is deliberately the first factor level, unlike the guided default.
binary$outcome <- factor(binary$outcome, levels = c("yes", "no"))
training <- binary[1:140, ]
evaluation <- binary[141:200, ]
fit <- glm(outcome ~ x + group, data = training, family = binomial())
reference <- glm(outcome ~ 1, data = training, family = binomial())
existing <- evaluate_models(
  list(current = fit, reference = reference), evaluation, "outcome",
  training_data = training, positive = "yes", primary = "current", reference = "reference"
)
expected <- 1 - predict(fit, evaluation, type = "response")
stopifnot(max(abs(predict(existing, evaluation) - expected)) < 1e-12)
bench <- benchmark_predictions(existing, batch_size = 20L, n_repeats = 3L,
  min_duration = .01, max_seconds = 2
)
stopifnot(inherits(bench, "autoxplain_prediction_benchmark"))
existing_path <- file.path(args[2], "existing-binary.html")
render_model_report(existing, existing_path, benchmark = bench,
  report_data = report_data_control("rows", max_rows = 200L)
)
saveRDS(existing, file.path(args[2], "existing-binary.rds"))
restored <- readRDS(file.path(args[2], "existing-binary.rds"))
stopifnot(identical(predict(existing, evaluation), predict(restored, evaluation)))
render_model_report(restored, file.path(args[2], "existing-binary-restored.html"),
  benchmark = bench, report_data = "summary"
)
replay_cases$existing_binary <- replay_case(
  restored, file.path(args[2], "existing-binary.rds"), evaluation, benchmark = bench
)
cat("Existing models: first-level event, benchmark, row/summary reports and RDS reuse passed\n")

# Small optional-engine cases must survive a new process from this archive.
# Native categories use a deliberately nonalphabetic class order and raw zeros.
set.seed(732)
native_data <- data.frame(
  x = c(rep(0, 30), rnorm(210)), category = factor(rep(c("c", "a", "b"), 80)),
  outcome = factor(rep(c("gamma", "alpha", "beta"), 80), c("gamma", "alpha", "beta"))
)
native_parameters <- list(
  nrounds = 5L, eta = .1, max_depth = 2L, min_child_weight = 1,
  subsample = 1, colsample_bytree = 1, reg_alpha = 0, reg_lambda = 1, encoding = "native"
)
native <- autoxplain(
  native_data[1:180, ], "outcome", test_data = native_data[181:240, ],
  learners = "boosting", nfolds = 2L, explain = FALSE, seed = 732L,
  tuning_control = tuning_control(
    grids = list(boosting = native_parameters), family_budgets = c(boosting = 1L), retain_oof = FALSE
  )
)
stopifnot(
  nrow(native$tuning$fold_scores) == 2L,
  identical(sort(native$tuning$fold_scores$fold), 1:2),
  sum(native$tuning$fold_scores$validation_rows) == 180L,
  inherits(native$models$main_model$blueprint, "autoxplain_boosting_blueprint"),
  identical(native$models$main_model$parameters$encoding, "native"),
  all(vapply(
    native$tuning$fold_scores$effective_parameters,
    function(value) identical(value$encoding, "native"), logical(1)
  ))
)
native_new <- native_data[181:188, ]
native_new$category <- factor(native_new$category, levels = rev(levels(native_new$category)))
native_prediction <- predict(native, native_new)
stopifnot(
  identical(colnames(native_prediction), levels(native_data$outcome)),
  max(abs(rowSums(native_prediction) - 1)) < 1e-6
)
native_saved <- file.path(args[2], "native-boosting.rds")
saveRDS(native, native_saved)
replay_cases$native_boosting <- replay_case(
  native, native_saved, native_new,
  report_options = list(
    explanation_rows = 11L, uncertainty = FALSE, report_data = report_data_control("rows", max_rows = 240L)
  ),
  checks = list(
    kind = "native_boosting", class_levels = levels(native_data$outcome), training_rows = 180L,
    evaluation = native_data[181:240, ], probabilities = predict(native, native_data[181:240, ])
  )
)
cat("Native multiclass boosting: categorical blueprint, ordered probabilities and fold encoding passed\n")

# Ten thousand rows trigger automatic BAM, with only one smooth and one setting.
# Dyadic source values allow exact decoded-data comparisons without rounding.
large_training <- data.frame(x = rep(0:99, 100))
large_training$outcome <- (large_training$x - 50)^2 / 128 + rep(-3:3, length.out = 10000) / 16
large_evaluation <- data.frame(x = rep((0:99) + .5, 20))
large_evaluation$outcome <- (large_evaluation$x - 50)^2 / 128 + rep(-3:3, length.out = 2000) / 16
large <- autoxplain(
  large_training, "outcome", test_data = large_evaluation,
  learners = "additive", nfolds = 2L, explanation_rows = 37L, seed = 917L,
  tuning_control = tuning_control(
    grids = list(additive = list(k = 5L, gamma = 1, select = TRUE, solver = "auto")),
    family_budgets = c(additive = 1L), retain_oof = FALSE
  )
)
computation <- large$models$main_model$fit_details$computation
stopifnot(
  nrow(large$tuning$fold_scores) == 2L,
  identical(sort(large$tuning$fold_scores$fold), 1:2),
  sum(large$tuning$fold_scores$validation_rows) == 10000L,
  inherits(large$models$main_model$fit, "bam"), identical(computation$solver, "bam"),
  identical(computation$requested_solver, "auto"), computation$planning_rows == 10000L,
  computation$fitting_rows == 10000L,
  all(vapply(
    large$tuning$fold_scores$learned,
    function(value) identical(value$computation$solver, "bam") && value$computation$fitting_rows == 5000L,
    logical(1)
  ))
)
full_score <- sqrt(mean((large_evaluation$outcome - predict(large, large_evaluation))^2))
stopifnot(isTRUE(all.equal(unname(large$evaluation$metrics$main_model[["rmse"]]), full_score, tolerance = 1e-14)))
sampling <- large$explanations$audit$config$sampling
stopifnot(sampling$rows_available == 2000L, sampling$rows_used == 37L, sampling$sampled)
large_options <- list(
  report_data = report_data_control("rows", max_rows = 12000L, max_pair_rows = 53L, seed = 917L),
  explanation_rows = 37L, uncertainty = FALSE
)
do.call(render_model_report, c(
  list(result = large, output_file = file.path(args[2], "auto-bam-compact.html")), large_options
))
large_saved <- file.path(args[2], "auto-bam-compact.rds")
saveRDS(large, large_saved)
replay_cases$auto_bam_compact <- replay_case(
  large, large_saved, large_evaluation,
  report_options = large_options,
  checks = list(
    kind = "auto_bam_compact", score = full_score, explanation_rows = 37L, pair_rows = 53L,
    training = large_training, evaluation = large_evaluation
  )
)
cat("Automatic BAM: fixed solver, full evaluation score and bounded explanations passed\n")
writeLines(capture.output(sessionInfo()), file.path(args[2], "session-info.txt"))
replay_manifest <- file.path(args[2], "replay-inputs.rds")
saveRDS(replay_cases, replay_manifest)
script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
stopifnot(length(script_argument) == 1L)
script_directory <- dirname(normalizePath(sub("^--file=", "", script_argument)))
replay_script <- file.path(script_directory, "check-reloaded-artifact.R")
replay_log <- file.path(args[2], "fresh-session.log")
rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
status <- system2(
  rscript, c(
    "--vanilla", shQuote(replay_script),
    shQuote(normalizePath(args[1])), shQuote(normalizePath(args[2])), shQuote(args[3]),
    shQuote(normalizePath(replay_manifest))
  ), stdout = replay_log, stderr = replay_log
)
cat(readLines(replay_log, warn = FALSE), sep = "\n")
if (!identical(status, 0L)) stop("Fresh-session installed artifact replay failed; see ", replay_log, call. = FALSE)
cat("Installed artifact smoke passed from", find.package("AutoXplainR"), "\n")
