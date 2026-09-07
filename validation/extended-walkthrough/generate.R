# Run with Rscript; common.R selects an installed package and an explicit output directory.
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script)), "common.R"))
if (file.exists(file.path(output, "source-and-result.rds"))) {
  stop("Use a new AXR_EXTENDED_DIR for fitting; use replay.R to preserve an existing result.")
}
set.seed(61041)
shipments <- data.frame(
  distance_km = round(runif(360, 10, 400), 1),
  load_tonnes = round(runif(360, 1, 12), 2),
  depot_queue = rpois(360, 6),
  service = factor(sample(c('standard', 'express'), 360, replace = TRUE))
)
shipments$delay_hours <- 6 + 0.012 * shipments$distance_km +
  3.5 * sin(shipments$load_tonnes / 2) + .15 * shipments$depot_queue -
  1.2 * (shipments$service == 'express') + rnorm(360, sd=.65)
result <- autoxplain(shipments, 'delay_hours', portfolio='extended',
  model_set='tuned', max_models=20L, nfolds=3L, tuning_rule='one_se',
  seed=61042L, explain=FALSE)
render_model_report(result, file.path(output, 'extended.html'),
  title='Dispatch delay: challenge the wider portfolio', target_units='hours',
  report_data=report_data_control('rows', max_rows=400L),
  max_models=4L, top_features=2L, n_repeats=2L)
saveRDS(list(original=shipments, result=result), file.path(output, 'source-and-result.rds'))
writeLines(capture.output(sessionInfo()), file.path(output, 'session-info.txt'))
cat('Generated installed-archive extended report and retained source. Read the report before inspecting R objects.\n')
