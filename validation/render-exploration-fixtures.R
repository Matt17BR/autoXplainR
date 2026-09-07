# Source-row and preprocessing examples for the report acceptance tasks.
# Data are simulated; the two supplied partitions deliberately differ.
pkgload::load_all(quiet = TRUE)
output <- Sys.getenv("EXPLORER_CASES", "/tmp/autoxplain-explorer-cases")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
set.seed(607)
parcels <- data.frame(
  shipment_id = sprintf("parcel-%03d", seq_len(320)),
  distance_km = round(runif(320, 5, 700), 1),
  weight_kg = round(rlnorm(320, .5, .8), 2),
  dispatch_backlog = rpois(320, 12),
  service = sample(c("Economy delivery", "Priority delivery with signature"), 320, TRUE),
  duplicate_constant = "same for all rows"
)
parcels$planned_route_hours <- parcels$distance_km / 40 + runif(320, 1, 3)
parcels$delivery_hours <- 9 + .034 * parcels$distance_km +
  1.4 * log1p(parcels$weight_kg) + .3 * parcels$dispatch_backlog -
  5 * (parcels$service == "Priority delivery with signature") + rnorm(320, sd = 3)
parcels$weight_kg[c(8, 28, 80, 199, 241:260)] <- NA
parcels$service[c(5, 29, 88, 261, 282)] <- NA
parcels$service[270:278] <- "New same-day service"
train <- parcels[1:240, ]
evaluation <- parcels[241:320, ]
result <- autoxplain(train, "delivery_hours", test_data = evaluation, seed = 607,
  preprocessing_config = list(remove_id_columns = TRUE,
    missing_value_strategy = "impute", novel_level_strategy = "mode"))
saveRDS(result, file.path(output, "messy-regression.rds"))
for (mode in c("summary", "rows", "none")) {
  render_model_report(result, file.path(output, paste0("messy-regression-", mode, ".html")),
    title = "Delivery estimates across two supplied batches", target_units = "hours", report_data = mode)
}
jsonlite::write_json(list(
  training_rows = nrow(train), evaluation_rows = nrow(evaluation),
  training_weight_missing = sum(is.na(train$weight_kg)),
  evaluation_weight_missing = sum(is.na(evaluation$weight_kg)),
  training_weight_median = median(train$weight_kg, na.rm = TRUE),
  evaluation_new_service_rows = which(evaluation$service == "New same-day service"),
  expected_features = setdiff(names(train), c("shipment_id", "duplicate_constant", "delivery_hours")),
  raw = list(training = train, evaluation = evaluation)
), file.path(output, "messy-regression-oracle.json"), auto_unbox = TRUE, digits = 16, na = "null", null = "null")
cat("Prepared the missingness, novel-category, raw-value and source-row tasks in", output, "\n")
