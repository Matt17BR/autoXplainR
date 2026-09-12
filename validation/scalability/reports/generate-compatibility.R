# A modest fixture catches category controls that accidentally scale with every
# row, and preserves AsIs numeric columns used by existing R workflows.
pkgload::load_all(quiet = TRUE)
base <- Sys.getenv("AXR_SCALE_OUTPUT", path.expand("~/.cache/autoxplain-scale-0.7.0/reports"))
folder <- file.path(base, "compatibility")
dir.create(folder, recursive = TRUE, showWarnings = FALSE)
set.seed(7411L)
data <- data.frame(x = rnorm(2700), z = runif(2700))
data$response <- 2 * data$x + data$z + rnorm(2700, sd = .1)
data$record_id <- sprintf("person_%06d", seq_len(nrow(data)))
data$rare_group <- c(rep("common", nrow(data) - 1L), "rare")
training <- data[1:200, ]
evaluation <- data[201:2700, ]
fit <- lm(response ~ x + z, training)
training$x <- I(training$x)
evaluation$x <- I(evaluation$x)
result <- evaluate_models(list(linear = fit), evaluation, "response", training_data = training,
  features = c("x", "z"))
render_model_report(result, file.path(folder, "report.html"), n_repeats = 2L, top_features = 1L,
  uncertainty = FALSE, report_data = report_data_control("rows", max_rows = 2700L,
    context_columns = c("record_id", "rare_group"), max_pair_rows = 300L, seed = 7L))
render_model_report(result, file.path(folder, "all-pairs.html"), n_repeats = 2L, top_features = 1L,
  uncertainty = FALSE, report_data = report_data_control("rows", max_rows = 2700L,
    context_columns = c("record_id", "rare_group"), max_pair_rows = NULL, seed = 7L))
selected <- c("person_000017", "person_002699")
jsonlite::write_json(list(
  rows = 2700L, ids = data$record_id, selected = selected,
  selected_x = as.list(setNames(data$x[match(selected, data$record_id)], selected)),
  filtered_count = sum(data$x >= 2), training_rows = 200L, evaluation_rows = 2500L,
  x = data$x, z = data$z, pair_limit = 300L, pair_seed = 7L,
  full_spearman = list(training = cor(training$x, training$z, method = "spearman"),
    evaluation = cor(evaluation$x, evaluation$z, method = "spearman"))
), file.path(folder, "source.json"), auto_unbox = TRUE, pretty = TRUE, digits = 16)
