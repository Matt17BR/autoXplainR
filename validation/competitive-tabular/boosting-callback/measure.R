arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 1L) stop("Usage: Rscript measure.R OUTPUT", call. = FALSE)
pkgload::load_all(".", quiet = TRUE)
destination <- arguments[[1L]]
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
stopifnot(!file.exists(file.path(destination, "timings.csv")))
script_path <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())])
set.seed(258)
data <- as.data.frame(matrix(rnorm(210000), nrow = 21000L, ncol = 10L))
data$y <- data$V1^2 + sin(data$V2) + rnorm(nrow(data), sd = .4)
training <- data[1:1000, ]
validation <- data[1001:21000, ]
records <- list()
curves <- list()
index <- 0L
warm <- AutoXplainR:::boosting_learner_grid(1000, 10, 'regression', 1)[[1]]
warm$nrounds <- 2L
invisible(AutoXplainR:::fit_boosting_core(training, 'y', 'regression', warm, 238L,
  validation = validation, early_stopping_rounds = 3L))
for (repetition in 1:2) {
for (rounds in c(200L, 800L, 2000L)) {
  parameters <- AutoXplainR:::boosting_learner_grid(1000, 10, 'regression', 1)[[1]]
  parameters$nrounds <- rounds
  parameters$max_depth <- 4L
  parameters$eta <- .03
  for (mode in if (repetition == 1L) c('callback', 'native_rmse') else c('native_rmse', 'callback')) {
    gc()
    started <- proc.time()[['elapsed']]
    if (mode == 'callback') {
      fit <- AutoXplainR:::fit_boosting_core(
        training, 'y', 'regression', parameters, 238L,
        validation = validation, early_stopping_rounds = rounds + 1L
      )
      curve <- fit$calibration$curve$score
    } else {
      train_matrix <- xgboost::xgb.DMatrix(as.matrix(training[1:10]), label = training$y, nthread = 1L)
      validation_matrix <- xgboost::xgb.DMatrix(as.matrix(validation[1:10]), label = validation$y, nthread = 1L)
      fit <- xgboost::xgb.train(
        list(objective = 'reg:squarederror', eval_metric = 'rmse', eta = .03, max_depth = 4L,
          min_child_weight = 1, subsample = 1, colsample_bytree = 1, alpha = 0, lambda = 1,
          nthread = 1L, seed = 238L, verbosity = 0L),
        train_matrix, nrounds = rounds, evals = list(inner = validation_matrix),
        early_stopping_rounds = rounds + 1L, verbose = 0L
      )
      curve <- as.data.frame(attr(fit, 'evaluation_log'))$inner_rmse
    }
    elapsed <- proc.time()[['elapsed']] - started
    index <- index + 1L
    records[[index]] <- data.frame(mode = mode, rounds = rounds, repetition = repetition, seconds = elapsed)
    curves[[paste(mode, rounds, repetition, sep = '_')]] <- curve
    cat(mode, rounds, elapsed, '\n')
  }
}
}
timings <- do.call(rbind, records)
for (repetition in 1:2) {
  for (rounds in c(200L, 800L, 2000L)) {
    stopifnot(max(abs(curves[[paste('callback', rounds, repetition, sep = '_')]] -
      curves[[paste('native_rmse', rounds, repetition, sep = '_')]])) < 1e-7)
  }
  stopifnot(identical(curves[[paste0('callback_200_', repetition)]],
    curves[[paste0('callback_2000_', repetition)]][1:200]))
}
saveRDS(list(timings = timings, curves = curves, source_md5 = tools::md5sum(
  'R/boosting_training.R')), file.path(destination, 'result.rds'))
write.csv(timings, file.path(destination, 'timings.csv'), row.names = FALSE)
print(timings)

checks <- do.call(rbind, lapply(1:2, function(repetition) {
  do.call(rbind, lapply(c(200L, 800L, 2000L), function(rounds) {
    data.frame(repetition = repetition, rounds = rounds,
      max_curve_difference = max(abs(curves[[paste('callback', rounds, repetition, sep = '_')]] -
        curves[[paste('native_rmse', rounds, repetition, sep = '_')]])),
      prefix_identical = identical(curves[[paste0('callback_200_', repetition)]],
        curves[[paste0('callback_2000_', repetition)]][1:200]))
  }))
}))
write.csv(checks, file.path(destination, "checks.csv"), row.names = FALSE)
source_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
source_hashes <- stats::setNames(lapply(source_files, function(path) {
  digest::digest(file = path, algo = "sha256")
}), source_files)
manifest <- list(
  phase = "synthetic development diagnostic", generated_at = format(Sys.time(), tz = "UTC"),
  R = R.version.string, xgboost = as.character(packageVersion("xgboost")),
  package_version = as.character(packageVersion("AutoXplainR")),
  threads = 1L, training_rows = 1000L, calibration_rows = 20000L, predictors = 10L,
  data_seed = 258L, fit_seed = 238L, depth = 4L, eta = .03,
  runner_sha256 = digest::digest(file = script_path, algo = "sha256"),
  source_sha256 = source_hashes,
  scope = "Fit plus calibration scoring time. Matrix construction differs across paths. These timings are not a pure callback overhead estimate.")
jsonlite::write_json(manifest, file.path(destination, "manifest.json"), pretty = TRUE, auto_unbox = TRUE)
