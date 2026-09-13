arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) < 2L) {
  stop("Usage: Rscript run-probes.R CACHE OUTPUT [CASE] [TREES] [no_oob]", call. = FALSE)
}
source_path <- file.path(arguments[[1L]], "cases/yearprediction/development/training.rds")
destination <- arguments[[2L]]
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
arguments <- arguments[-c(1L, 2L)]
script_path <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())])
source <- readRDS(source_path)
validation_rows <- source$calibration_rows
training_rows <- setdiff(seq_len(nrow(source$data)), validation_rows)
training <- source$data[training_rows, , drop = FALSE]
validation <- source$data[validation_rows, , drop = FALSE]
definitions <- data.frame(
  id = c('sqrt_node5', 'third_node5', 'sqrt_node20', 'third_node20', 'sqrt_node50',
    'third_node50', 'extra_sqrt_node5', 'legacy_full_p'),
  mtry = c(9L, 30L, 9L, 30L, 9L, 30L, 9L, 90L),
  min.node.size = c(5L, 5L, 20L, 20L, 50L, 50L, 5L, 5L),
  splitrule = c(rep('variance', 6), 'extratrees', 'variance'),
  num.trees = c(rep(128L, 7), 32L)
)
if (length(arguments)) {
  definitions <- definitions[match(arguments[[1]], definitions$id), , drop = FALSE]
  if (length(arguments) > 1L) definitions$num.trees <- as.integer(arguments[[2]])
}
stopifnot(nrow(training) == 40000L, nrow(validation) == 10000L, !anyNA(definitions))
for (index in seq_len(nrow(definitions))) {
  definition <- definitions[index, ]
  no_oob <- length(arguments) > 2L && arguments[[3]] == 'no_oob'
  id <- paste0(definition$id, '_', definition$num.trees, 'trees', if (no_oob) '_no_oob' else '')
  output <- file.path(destination, id)
  if (file.exists(paste0(output, '.json'))) stop('Output exists: ', output)
  parameters <- list(
    x = training[setdiff(names(training), 'y')], y = training$y,
    num.trees = definition$num.trees, mtry = definition$mtry,
    min.node.size = definition$min.node.size, splitrule = definition$splitrule,
    sample.fraction = .8, respect.unordered.factors = 'order',
    num.threads = 2L, seed = 80711L, write.forest = TRUE, verbose = FALSE, oob.error = !no_oob
  )
  if (definition$splitrule == 'extratrees') parameters$num.random.splits <- 5L
  cat('Starting', id, format(Sys.time(), tz = 'UTC'), '\n')
  gc()
  started <- proc.time()[['elapsed']]
  fit <- do.call(ranger::ranger, parameters)
  seconds <- proc.time()[['elapsed']] - started
  prediction <- predict(fit, data = validation[setdiff(names(training), 'y')], num.threads = 2L)$predictions
  residual <- validation$y - prediction
  parameters$x <- NULL
  parameters$y <- NULL
  record <- list(
    id = id, phase = 'development',
    runner_sha256 = digest::digest(file = script_path, algo = 'sha256'),
    source_sha256 = digest::digest(file = source_path, algo = 'sha256'),
    fit_seed = 80711L, training_rows = length(training_rows), validation_rows = length(validation_rows),
    native_samples = fit$num.samples, native_trees = fit$num.trees,
    native_predictors = fit$num.independent.variables, parameters = parameters,
    fit_seconds = seconds, rmse = sqrt(mean(residual^2)), mae = mean(abs(residual)),
    ranger_version = as.character(packageVersion('ranger')),
    R_version = R.version.string, completed_at = format(Sys.time(), tz = 'UTC')
  )
  fit$call <- quote(ranger::ranger(x = training_predictors, y = training_outcome))
  saveRDS(list(model = fit, predictions = prediction, validation_rows = validation_rows,
    training_rows = training_rows, record = record), paste0(output, '.rds'), compress = FALSE)
  jsonlite::write_json(record, paste0(output, '.json'), auto_unbox = TRUE, pretty = TRUE, digits = 16)
  cat('Finished', id, 'seconds', seconds, 'RMSE', record$rmse, '\n')
  rm(fit)
}
