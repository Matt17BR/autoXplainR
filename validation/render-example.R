# Synthetic delivery times: no private observations or trained service model.
# Every predictor is available when a parcel is dispatched. The primary model
# is selected by training-only cross-validation; test scores remain separate.
if (dir.exists("R")) pkgload::load_all(quiet = TRUE) else library(AutoXplainR)
set.seed(82)
parcels <- data.frame(
  distance_km = round(runif(360, 5, 700), 1),
  parcel_kg = round(rlnorm(360, 0.5, 0.8), 1),
  dispatch_backlog = rpois(360, 12),
  service = factor(sample(c("economy", "priority"), 360, replace = TRUE))
)
parcels$planned_route_hours <- parcels$distance_km / 40 + runif(360, 1, 3)
# A route estimate overlaps with distance; the report should flag that context.
parcels$delivery_hours <- 9 + 0.034 * parcels$distance_km +
  1.4 * log1p(parcels$parcel_kg) + 0.3 * parcels$dispatch_backlog -
  5 * (parcels$service == "priority") + rnorm(360, sd = 3)
# A few weights were not recorded at dispatch; their outcomes are still observed.
# The report should show these supplied gaps and the training-derived fill value.
parcels$parcel_kg[c(17, 39, 112, 206, 283)] <- NA_real_
result <- autoxplain(parcels, "delivery_hours", seed = 2026)
dir.create("pkgdown/assets", recursive = TRUE, showWarnings = FALSE)
render_model_report(
  result, "pkgdown/assets/model-report.html", uncertainty = TRUE,
  title = "Delivery time at dispatch", target_units = "hours", report_data = "rows"
)
