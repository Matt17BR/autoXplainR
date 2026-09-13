script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script)), "common.R"))
library_path <- path.expand("~/.cache/autoxplain-scale-0.7.0/release-verification/published-release-0.7.0/installed-library")
.libPaths(c(library_path, .libPaths()))
library(AutoXplainR)
stopifnot(as.character(packageVersion("AutoXplainR")) == "0.7.0")
namespace <- asNamespace("AutoXplainR")
output <- list()
for (name in c("yearprediction", "covertype", "bank", "friedman_noise", "rare_interaction")) {
  training <- readRDS(file.path(tabular_cache(), "cases", name, "development", "training.rds"))
  data <- training$data
  plan <- get("local_tuning_plan", namespace)(max_models = 10L, n = nrow(data),
    p = ncol(data) - 1L, task = training$metadata$task,
    n_classes = if (is.factor(data$y)) nlevels(data$y) else 0L,
    learners = c("forest", "boosting"), seed = training$metadata$fit_seed,
    boosting_encoding = "matrix")
  output[[name]] <- plan
}
record <- list(package_version = "0.7.0", package_library = find.package("AutoXplainR"),
  scope = "Reconstructed from the unchanged published installation and frozen development training dimensions. This is not a record of completed fits.",
  forest_grid_function_sha256 = digest::digest(paste(deparse(get("forest_learner_grid", namespace)), collapse = "\n"),
    algo = "sha256", serialize = FALSE),
  plans = output)
tabular_json(record, file.path(dirname(normalizePath(script)), "baseline-plans.json"))
