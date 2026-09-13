arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 2L) stop("Usage: Rscript screen-probe.R CACHE OUTPUT", call. = FALSE)
pkgload::load_all(".", quiet = TRUE)
destination <- arguments[[2L]]
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
source_path <- file.path(arguments[[1L]], "cases/yearprediction/development/training.rds")
script_path <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())])
case <- readRDS(source_path)
set.seed(80761L)
training_rows <- sort(sample(setdiff(seq_len(nrow(case$data)), case$calibration_rows), 16000L))
validation_rows <- sort(sample(case$calibration_rows, 4000L))
training <- case$data[training_rows, , drop = FALSE]
validation <- case$data[validation_rows, , drop = FALSE]
planning_rows <- 463715L
policy <- AutoXplainR:::forest_search_policy(planning_rows, 90L, 'regression')
for (mtry in c(9L, 30L)) {
  full <- list(num.trees = 500L, mtry = mtry, min.node.size = policy$node_values[[2L]],
    sample.fraction = .8, splitrule = 'default')
  for (mode in c('unscaled', 'rescaled')) {
    screen <- if (mode == 'rescaled') {
      AutoXplainR:::adaptive_screen_parameters(full, 'forest', training_rows = nrow(training),
        planned_rows = planning_rows, planned_predictors = 90L)
    } else {
      AutoXplainR:::adaptive_screen_parameters(full, 'forest')
    }
    id <- paste0('screen_mtry', mtry, '_', mode)
    output <- file.path(destination, id)
    stopifnot(!file.exists(paste0(output, '.json')))
    arguments <- list(x = training[setdiff(names(training), 'y')], y = training$y,
      num.trees = screen$num.trees, mtry = screen$mtry, min.node.size = screen$min.node.size,
      sample.fraction = screen$sample.fraction, splitrule = 'variance',
      num.threads = 2L, seed = 80711L, respect.unordered.factors = 'order',
      write.forest = TRUE, oob.error = FALSE, verbose = FALSE)
    cat('Starting', id, format(Sys.time(), tz = 'UTC'), '\n')
    gc()
    started <- proc.time()[['elapsed']]
    model <- do.call(ranger::ranger, arguments)
    elapsed <- proc.time()[['elapsed']] - started
    predictions <- predict(model, data = validation[setdiff(names(training), 'y')], num.threads = 2L)$predictions
    record <- list(id = id, phase = 'development', seed = 80711L, sampling_seed = 80761L,
      rows_actually_fitted = nrow(training), rows_scored = nrow(validation),
      planning_only_rows = planning_rows, full_parameters = full, screening_parameters = screen,
      threads = 2L, oob_computed = FALSE, native_samples = model$num.samples,
      seconds = elapsed, rmse = sqrt(mean((validation$y - predictions)^2)),
      source_sha256 = digest::digest(file = source_path, algo = 'sha256'),
      policy_source_sha256 = digest::digest(file = 'R/learner_backends.R',
        algo = 'sha256'),
      runner_sha256 = digest::digest(file = script_path, algo = 'sha256'),
      adaptive_source_sha256 = digest::digest(file = 'R/adaptive_search.R', algo = 'sha256'),
      ranger_version = as.character(packageVersion('ranger')), R_version = R.version.string,
      scope = 'Only development rows were fitted. The larger count sets a hypothetical screening policy, not a claim of a large fit.')
    model$call <- quote(ranger::ranger(x = training_predictors, y = training_outcome))
    saveRDS(list(model = model, predictions = predictions, training_rows = training_rows,
      validation_rows = validation_rows, record = record), paste0(output, '.rds'), compress = FALSE)
    jsonlite::write_json(record, paste0(output, '.json'), auto_unbox = TRUE, pretty = TRUE, digits = 16)
    cat('Finished', id, 'seconds', elapsed, 'RMSE', record$rmse, '\n')
  }
}
