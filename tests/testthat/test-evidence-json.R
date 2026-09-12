test_that("classification evidence exports labelled counts without changing the audit", {
  binary <- data.frame(
    x = seq_len(100),
    outcome = factor(c(rep("zeta", 99), "alpha"), levels = c("zeta", "alpha"))
  )
  multiclass <- data.frame(
    x = seq_len(100),
    outcome = factor(c(rep("gamma", 70), rep("alpha", 30)), levels = c("gamma", "alpha", "beta"))
  )
  probability_model <- list(probabilities = c(gamma = .6, alpha = .3, beta = .1))
  results <- list(
    binary = evaluate_models(
      list(intercept = glm(outcome ~ 1, binary, family = binomial())), binary, "outcome"
    ),
    multiclass = evaluate_models(
      list(fixed = probability_model), multiclass, "outcome",
      predict_functions = list(fixed = function(model, newdata) {
        matrix(rep(model$probabilities, each = nrow(newdata)), nrow = nrow(newdata),
          dimnames = list(NULL, names(model$probabilities))
        )
      })
    )
  )
  full_counts <- list(
    binary = list(zeta = 99L, alpha = 1L),
    multiclass = list(gamma = 70L, alpha = 30L, beta = 0L)
  )
  sampled_counts <- list(binary = list(zeta = 2L, alpha = 0L), multiclass = full_counts$multiclass)
  for (name in names(results)) {
    result <- results[[name]]
    result$explanations$audit <- audit_explanations(as_explainers(result),
      n_repeats = 2L, max_rows = if (name == "binary") 2L else NULL, seed = 2L
    )
    before <- unserialize(serialize(result$explanations$audit, NULL))
    summary <- evidence_summary(result)
    locations <- list(
      summary$explanations$config$sampling,
      summary$explanations$diagnostic_status$permutation$evidence$sampling
    )
    for (sampling in locations) {
      expect_identical(sampling$full_class_counts, full_counts[[name]])
      expect_identical(sampling$class_counts, sampled_counts[[name]])
    }
    path <- withr::local_tempfile(fileext = ".json")
    jsonlite::write_json(summary, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
    decoded <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    decoded_locations <- list(
      decoded$explanations$config$sampling,
      decoded$explanations$diagnostic_status$permutation$evidence$sampling
    )
    for (sampling in decoded_locations) {
      expect_identical(names(sampling$full_class_counts), names(full_counts[[name]]))
      expect_identical(names(sampling$class_counts), names(sampled_counts[[name]]))
      expect_equal(sampling$full_class_counts, full_counts[[name]])
      expect_equal(sampling$class_counts, sampled_counts[[name]])
    }
    expect_identical(decoded$schema_version, "2.0")
    expect_identical(result$explanations$audit, before)
    expect_identical(
      serialize(result$explanations$audit, NULL, version = 2), serialize(before, NULL, version = 2)
    )
    expect_s3_class(result$explanations$audit$config$sampling$full_class_counts, "table")
    expect_s3_class(result$explanations$audit$config$sampling$class_counts, "table")
  }
})
