# Generate the fixed challenges once, before inspecting model scores.
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script)), "common.R"))
output <- stress_directory()
if (file.exists(file.path(output, "benchmark-plan.json"))) {
  stop("Cases already exist here. Reuse them, or choose a fresh AXR_STRESS_DIR for new data.")
}
seed_offset <- as.integer(Sys.getenv("AXR_STRESS_SEED_OFFSET", "0"))
stopifnot(length(seed_offset) == 1L, !is.na(seed_offset), seed_offset >= 0L)
cases <- file.path(output, "cases")
dir.create(cases, recursive = TRUE, showWarnings = FALSE)

save_case <- function(name, data, n_train, task, truth = NULL, metadata = list()) {
  training <- data[seq_len(n_train), , drop = FALSE]
  evaluation <- data[seq.int(n_train + 1L, nrow(data)), , drop = FALSE]
  object <- list(name = name, training = training, evaluation = evaluation,
    task = task, folds = stress_folds(training$y, task, 90210L), fit_seed = 20260912L,
    oracle = if (is.null(truth)) NULL else tail(truth, nrow(evaluation)), metadata = metadata)
  saveRDS(object, file.path(cases, paste0(name, ".rds")), version = 3L)
  list(name = name, training_rows = nrow(training), evaluation_rows = nrow(evaluation),
    predictors = ncol(data) - 1L, task = task,
    training_prevalence = if (task == "binary") mean(training$y == "yes") else NULL,
    evaluation_prevalence = if (task == "binary") mean(evaluation$y == "yes") else NULL,
    sha256 = digest::digest(file = file.path(cases, paste0(name, ".rds")), algo = "sha256"),
    metadata = metadata)
}

set.seed(1001L + seed_offset)
x <- as.data.frame(matrix(runif(2000L * 30L), 2000L, 30L))
names(x) <- paste0("x", seq_len(ncol(x)))
mu <- 10 * sin(pi * x$x1 * x$x2) + 20 * (x$x3 - 0.5)^2 + 10 * x$x4 + 5 * x$x5
x$y <- mu + rnorm(nrow(x))
manifest <- list(save_case("friedman_noise", x, 1200L, "regression", mu,
  list(description = "Friedman 1 response, 5 useful and 25 irrelevant uniform inputs; Gaussian noise SD 1.",
    seed = 1001L + seed_offset, source = "https://doi.org/10.1214/aos/1176347963")))

set.seed(1002L + seed_offset)
x <- as.data.frame(matrix(rnorm(780L * 240L), 780L, 240L))
names(x) <- paste0("x", seq_len(ncol(x)))
mu <- 3 * x$x1 - 2 * x$x2 + 1.5 * x$x3 + x$x4 - x$x5
x$y <- mu + rnorm(nrow(x))
manifest[[2L]] <- save_case("sparse_wide", x, 180L, "regression", mu,
  list(description = "240 numeric inputs, 180 training rows, five nonzero linear coefficients; Gaussian noise SD 1.",
    seed = 1002L + seed_offset, source = "Synthetic case defined in prepare.R"))

set.seed(1003L + seed_offset)
x <- as.data.frame(matrix(runif(4000L * 15L, -1, 1), 4000L, 15L))
names(x) <- paste0("x", seq_len(ncol(x)))
probability <- plogis(-4 + 5 * (x$x1 > 0.35 & x$x2 > 0.35) + 1.5 * x$x3)
x$y <- factor(ifelse(runif(nrow(x)) < probability, "yes", "no"), levels = c("no", "yes"))
manifest[[3L]] <- save_case("rare_interaction", x, 2500L, "binary", probability,
  list(description = "Rare event with a two-input conjunction and a continuous effect, plus 12 irrelevant inputs.",
    seed = 1003L + seed_offset, source = "Synthetic case defined in prepare.R"))

download_dir <- file.path(output, "uci-bank")
dir.create(download_dir, showWarnings = FALSE)
archive <- file.path(download_dir, "bank-additional.zip")
if (!file.exists(archive)) {
  download.file("https://archive.ics.uci.edu/static/public/222/bank+marketing.zip",
    file.path(download_dir, "bank-marketing.zip"), mode = "wb", quiet = TRUE)
  unzip(file.path(download_dir, "bank-marketing.zip"), files = "bank-additional.zip", exdir = download_dir)
}
unzip(archive, exdir = download_dir)
bank <- read.csv(file.path(download_dir, "bank-additional", "bank-additional-full.csv"), sep = ";")
stopifnot(nrow(bank) == 41188L, ncol(bank) == 21L)
bank$duration <- NULL # Not available before the call; the donor warns against predictive use.
bank$y <- factor(bank$y, levels = c("no", "yes"))
set.seed(1004L + seed_offset)
training <- evaluation <- integer()
for (level in levels(bank$y)) {
  rows <- sample(which(bank$y == level))
  proportion <- length(rows) / nrow(bank)
  n_train <- round(5000L * proportion)
  n_test <- round(3000L * proportion)
  training <- c(training, head(rows, n_train))
  evaluation <- c(evaluation, rows[seq.int(n_train + 1L, n_train + n_test)])
}
training <- sample(training)
evaluation <- sample(evaluation)
stopifnot(!length(intersect(training, evaluation)))
bank_rows <- c(training, evaluation)
write.csv(data.frame(source_row = bank_rows,
  split = c(rep("training", length(training)), rep("evaluation", length(evaluation)))),
  file.path(download_dir, "selected-source-rows.csv"), row.names = FALSE)
manifest[[4L]] <- save_case("bank_marketing", bank[bank_rows, ], length(training), "binary",
  metadata = list(description = "UCI bank-additional-full, stratified 5,000/3,000 contact-row subsamples, duration removed.",
    seed = 1004L + seed_offset, source = "https://archive.ics.uci.edu/dataset/222/bank+marketing",
    citation = "Moro, S., Rita, P., and Cortez, P. (2014). Bank Marketing. DOI 10.24432/C5K306.",
    license = "CC BY 4.0", omitted_predictors = "duration",
    sampling_scope = "Random contact-row discrimination, not a future-period or independent-customer estimate. Customer IDs are unavailable; repeated customers cannot be excluded.",
    unknown_values = "The donor's literal unknown category is retained.",
    source_zip_sha256 = digest::digest(file = archive, algo = "sha256")))

write_json(list(prepared_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  folds = "Five fixed training-only folds, identical across portfolios and the native regularized reference.",
  package_variants = list(core = "Default 15-configuration linear/tree/neural portfolio, one_se rule.",
    stronger = "15 configurations shared by explicit regularized/forest/boosting learners, one_se rule."),
  reference_models = list(xgboost = "400 rounds, eta .05, depth 3, min_child_weight 1, row/column fraction .8, lambda 1, alpha 0.",
    ranger = "500 trees, default mtry and node size, probability forests for classification.",
    glmnet = "Alpha 1, full default lambda path, fixed training folds, lambda.1se."),
  exclusions = "No test-based hyperparameters, score thresholds, feature selection, resampling, or case filtering.",
  cases = manifest), file.path(output, "benchmark-plan.json"))
cat("Prepared four challenges and the declared comparison plan in", output, "\n")
