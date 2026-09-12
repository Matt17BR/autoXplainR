# Pair sampling is descriptive, not a promise to preserve rare-group structure.
output <- Sys.getenv("AXR_SCALE_OUTPUT", path.expand("~/.cache/autoxplain-scale-0.7.0/data"))
dir.create(output, recursive = TRUE, showWarnings = FALSE)
pkgload::load_all(quiet = TRUE)
set.seed(141L)
n <- 100000L
x <- rnorm(n)
cases <- list(
  monotone = list(x = x, y = 2 * x + rnorm(n, sd = .3)),
  curved = list(x = x, y = x^2 + rnorm(n, sd = .05)),
  skew = list(x = rexp(n), y = rexp(n)),
  rare_cluster = list(x = as.numeric(seq_len(n) %% 10000L == 0),
                      y = as.numeric(seq_len(n) %% 10000L == 0) * 100 + rnorm(n))
)
results <- lapply(names(cases), function(name) {
  values <- cases[[name]]
  full <- AutoXplainR:::data_pair_association(values$x, values$y)
  seeds <- seq_len(30L)
  samples <- lapply(seeds, function(seed) withr::with_seed(seed, sort(sample.int(n, 10000L, useHash = TRUE))))
  estimates <- vapply(samples, function(index) {
    AutoXplainR:::data_pair_association(values$x[index], values$y[index])$value
  }, numeric(1))
  rare_counts <- if (name == "rare_cluster") vapply(samples, function(index) sum(values$x[index] == 1), integer(1)) else NULL
  list(case = name, population_rows = n, sampled_rows = 10000L, seeds = seeds,
    full_association = full$value, sampled_associations = estimates,
    maximum_absolute_error = if (any(is.finite(estimates))) max(abs(estimates - full$value), na.rm = TRUE) else NULL,
    unavailable_samples = sum(!is.finite(estimates)), sampled_rare_counts = rare_counts,
    population_rare_count = if (name == "rare_cluster") sum(values$x == 1) else NULL)
})
jsonlite::write_json(results, file.path(output, "pair-quality.json"), pretty = TRUE,
  auto_unbox = TRUE, null = "null", na = "null", digits = 16)
cat("Wrote", file.path(output, "pair-quality.json"), "\n")
