# Run from the repository root after generate-oracles.py. No model is fitted.
output <- Sys.getenv("AXR_NUMERICAL_OUTPUT", path.expand("~/.cache/autoxplain-numerical-review"))
source_file <- "R/matrix_blueprint.R"
baseline_ref <- "813d9b0a18397028ff1c6caaa951b8e46ffa03bb"
stopifnot(file.exists(source_file), file.exists(file.path(output, "oracles.json")))
pkgload::load_all(quiet = TRUE)
source_hash <- digest::digest(file = source_file, algo = "sha256")
baseline_file <- tempfile(fileext = ".R")
error_file <- tempfile()
status <- system2("git", c("show", paste0(baseline_ref, ":", source_file)),
                  stdout = baseline_file, stderr = error_file)
if (status != 0L) stop("Cannot read pinned baseline: ", paste(readLines(error_file), collapse = "\n"))
namespace <- asNamespace("AutoXplainR")
before <- new.env(parent = namespace)
after <- new.env(parent = namespace)
sys.source(baseline_file, before)
sys.source(source_file, after)
unlink(c(baseline_file, error_file))

RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(9351)
ordinary_cases <- 0L
for (n in c(3L, 10L, 41L, 100L)) {
  for (units in c(1e-100, 1, 1e100)) {
    data <- data.frame(
      x = rnorm(n) * units, z = seq_len(n), constant = 7,
      group = factor(rep(c("a", "b"), length.out = n))
    )
    for (center in c(FALSE, TRUE)) {
      for (scale in c(FALSE, TRUE)) {
        old <- before$fit_matrix_blueprint(data, center = center, scale = scale)
        new <- after$fit_matrix_blueprint(data, center = center, scale = scale)
        stopifnot(identical(old, new))
        for (input in list(data, data[rev(seq_len(n)), , drop = FALSE])) {
          stopifnot(identical(before$bake_matrix_blueprint(old, input),
                              after$bake_matrix_blueprint(new, input)))
        }
        ordinary_cases <- ordinary_cases + 1L
      }
    }
  }
}
stopifnot(ordinary_cases == 48L)

oracles <- jsonlite::fromJSON(file.path(output, "oracles.json"), simplifyVector = FALSE)
tolerance <- 1e-12
results <- lapply(oracles, function(case) {
  actual <- after$matrix_blueprint_sd(unlist(case$values, use.names = FALSE))
  error <- if (isTRUE(case$unrepresentable)) {
    stopifnot(is.infinite(actual), actual > 0)
    NA_real_
  } else {
    value <- abs(actual / case$expected_sd - 1)
    stopifnot(is.finite(value), value < tolerance)
    value
  }
  list(
    case = case$case, expected_sd = case$expected_sd,
    actual_sd = if (is.finite(actual)) actual else NULL,
    unrepresentable = case$unrepresentable, relative_error = error, passed = TRUE
  )
})
errors <- vapply(results, function(result) result$relative_error, numeric(1))
stopifnot(length(results) == 35L, identical(source_hash, digest::digest(file = source_file, algo = "sha256")))
verdict <- list(
  baseline_ref = baseline_ref, source_file = source_file, source_sha256 = source_hash,
  ordinary_blueprint_configurations = ordinary_cases,
  ordinary_blueprints_identical = TRUE, baked_training_and_reordered_inputs_identical = TRUE,
  sd_cases = length(results), finite_sd_cases = sum(is.finite(errors)),
  unrepresentable_sd_cases = sum(!is.finite(errors)),
  max_relative_sd_error = max(errors, na.rm = TRUE), relative_tolerance = tolerance,
  R_version = R.version.string, platform = R.version$platform,
  scope = "Blueprints and baked model-input matrices, not fitted models or full model predictions. No ARM binary run."
)
jsonlite::write_json(results, file.path(output, "results.json"), pretty = TRUE,
                     auto_unbox = TRUE, digits = 16, na = "null", null = "null")
jsonlite::write_json(verdict, file.path(output, "verdict.json"), pretty = TRUE,
                     auto_unbox = TRUE, digits = 16)
print(verdict)
