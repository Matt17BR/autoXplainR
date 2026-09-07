# Expert workflow fixtures: AutoXplainR evaluates already fitted models without
# inventing training, tuning or timer history. The oracle uses backend predictions.
pkgload::load_all(quiet = TRUE)
output <- Sys.getenv("EXPLORER_CASES", "/tmp/autoxplain-explorer-cases")
dir.create(output, recursive = TRUE, showWarnings = FALSE)

linear <- lm(mpg ~ wt + hp, mtcars[1:20, ])
regression <- evaluate_models(
  list(approved = linear), mtcars[21:32, ], "mpg",
  features = c("wt", "hp"),
  labels = c(approved = "Approved fuel model"), evaluation_role = "test"
)
render_model_report(regression, file.path(output, "supplied-regression.html"),
  report_data = "rows", top_features = 2L, n_repeats = 3L
)
saveRDS(regression, file.path(output, "supplied-regression.rds"))

set.seed(6102)
source_data <- data.frame(tenure = runif(260, 0, 10), monthly_bill = runif(260, 15, 100))
event <- rbinom(260, 1, plogis(-.8 - .3 * source_data$tenure + .035 * source_data$monthly_bill))
source_data$cancelled <- factor(ifelse(event, "yes", "no"), levels = c("yes", "no"))
training <- source_data[1:180, ]
evaluation <- source_data[181:260, ]
logistic <- glm(cancelled ~ tenure + monthly_bill, training, family = binomial())
tree <- rpart::rpart(cancelled ~ tenure + monthly_bill, training,
  method = "class",
  control = rpart::rpart.control(cp = .01, maxdepth = 3, minsplit = 15)
)
reference <- glm(cancelled ~ 1, training, family = binomial())
binary <- evaluate_models(
  list(current = logistic, challenger = tree, historical = reference), evaluation, "cancelled",
  labels = c(current = "Current logistic model", challenger = "Shallow tree", historical = "Historical rate"),
  primary = "current", reference = "historical", positive = "yes", training_data = training,
  features = c("tenure", "monthly_bill"), evaluation_role = "test"
)
for (mode in c("summary", "rows", "none")) {
  render_model_report(binary, file.path(output, paste0("supplied-binary-", mode, ".html")),
    report_data = mode, top_features = 2L, n_repeats = 3L
  )
}
saveRDS(binary, file.path(output, "supplied-binary.rds"))
benchmark <- benchmark_predictions(binary,
  batch_size = 32L, n_repeats = 3L, min_duration = .01, max_seconds = 2, seed = 6102
)
render_model_report(binary, file.path(output, "supplied-binary-benchmark.html"),
  benchmark = benchmark, top_features = 2L, n_repeats = 3L
)
saveRDS(benchmark, file.path(output, "supplied-benchmark.rds"))
jsonlite::write_json(
  benchmark[c("summary", "measurements", "protocol")],
  file.path(output, "supplied-benchmark-oracle.json"), dataframe = "rows",
  auto_unbox = TRUE, null = "null", na = "null", digits = 16, pretty = TRUE
)
oracle <- list(
  regression = list(source = mtcars[21:32, c("wt", "hp", "mpg")], predicted = unname(predict(linear, mtcars[21:32, ]))),
  binary = list(
    source = evaluation, classes = levels(training$cancelled), positive = "yes",
    primary = "current", reference = "historical", predictions = list(
      current = unname(1 - predict(logistic, evaluation, type = "response")),
      challenger = unname(predict(tree, evaluation, type = "prob")[, "yes"]),
      historical = unname(1 - predict(reference, evaluation, type = "response"))
    )
  )
)
jsonlite::write_json(oracle, file.path(output, "supplied-models-oracle.json"),
  dataframe = "rows", auto_unbox = TRUE, null = "null", na = "null", digits = 16, pretty = TRUE
)
