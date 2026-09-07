# Run with Rscript; common.R selects an installed package and an explicit output directory.
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script)), "common.R"))
original_path <- Sys.getenv("AXR_EXTENDED_SOURCE")
if (!nzchar(original_path)) stop("Set AXR_EXTENDED_SOURCE to the original source-and-result.rds.")
original_path <- normalizePath(original_path, mustWork = TRUE)
target_path <- file.path(output, "source-and-result.rds")
if (file.exists(target_path)) {
  stopifnot(identical(readBin(original_path, "raw", file.info(original_path)$size),
    readBin(target_path, "raw", file.info(target_path)$size)))
} else {
  stopifnot(file.copy(original_path, target_path))
}
source <- readRDS(original_path)
# Exercise the original fresh-session failure before any report work.
fresh_neural <- predict(source$result, source$original[1:5, ], model='neural_model')
stopifnot(length(fresh_neural)==5L, all(is.finite(fresh_neural)))
writeLines(capture.output(fresh_neural),file.path(output,'fresh-neural-predictions.txt'))
render_model_report(source$result,file.path(output,'extended.html'),
  title='Dispatch delay: challenge the wider portfolio',target_units='hours',
  report_data=report_data_control('rows',max_rows=400L),
  max_models=4L,top_features=2L,n_repeats=2L)
writeLines(capture.output(sessionInfo()),file.path(output,'session-info.txt'))
stopifnot(identical(readBin(original_path,'raw',file.info(original_path)$size),
  readBin(file.path(output,'source-and-result.rds'),'raw',file.info(original_path)$size)))
cat('Replayed unchanged original data and fits through the final installed package.\n')
