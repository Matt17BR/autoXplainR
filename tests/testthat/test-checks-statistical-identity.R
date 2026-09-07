test_that("Checks names the recorded metric and actual supplied reference without relabelling estimates", {
  result <- list(
    model_labels = c(plain = "Linear <fit>", custom = "Tree & forest"),
    provenance = list(target_units = "hours"), evaluation = list(primary_metric = "rmse")
  )
  interval <- list(
    metric = "mae", primary_model_id = "plain", reference_model_id = "custom",
    confidence = .9, n_boot = 200L, units = 24L, unit = "observation",
    estimates = data.frame(
      quantity = c("difference", "baseline", "primary"), estimate = c(-2, 5, 3),
      lower = c(-3, 4, 2), upper = c(-1, 6, 4)
    ),
    notes = c(
      "Paired percentile intervals conditional on the fitted models; negative differences favor the primary model.",
      "These intervals omit fitting and selection uncertainty and assume independent sampling units."
    )
  )
  html <- AutoXplainR:::render_performance_uncertainty(interval, result = result)
  text <- gsub("<[^>]+>", "|", html)
  expect_match(text, "MAE (hours)", fixed = TRUE)
  expect_false(grepl("RMSE", text, fixed = TRUE))
  expect_match(text, "Linear &lt;fit&gt; (primary)", fixed = TRUE)
  expect_match(text, "Tree &amp; forest (reference)", fixed = TRUE)
  expect_false(grepl("Intercept-only baseline", text, fixed = TRUE))
  expect_match(text, "90.0% interval", fixed = TRUE)
  rows <- regmatches(html, gregexpr("<tr>.*?</tr>", html))[[1L]][-1L]
  values <- lapply(rows, function(row) {
    gsub("<[^>]+>", "", regmatches(row, gregexpr("<td[^>]*>.*?</td>", row))[[1L]])
  })
  expect_identical(values, list(
    c("Primary minus reference", "-2.00", "-3.00 to -1.00"),
    c("Tree &amp; forest (reference)", "5.00", "4.00 to 6.00"),
    c("Linear &lt;fit&gt; (primary)", "3.00", "2.00 to 4.00")
  ))
  expect_match(text, "independent sampling units", fixed = TRUE)
  expect_equal(lengths(regmatches(text, gregexpr("Negative differences", text, fixed = TRUE))), 1L)

  interval$metric <- "logloss"
  interval$unit <- "group"
  interval$units <- 4L
  interval$notes <- c(interval$notes, "Fewer than 20 sampling units: interval endpoints may be very unstable.")
  html <- AutoXplainR:::render_performance_uncertainty(interval, result = result)
  expect_match(html, "log loss", fixed = TRUE)
  expect_match(html, "4 evaluation groups", fixed = TRUE)
  expect_false(grepl("hours", html, fixed = TRUE))
  visible <- sub('<details class="uncertainty-method">.*', "", html)
  expect_match(visible, "Fewer than 20 sampling units", fixed = TRUE)
})

test_that("missingness changes distinguish percentage points from relative percentages", {
  shift <- list(
    n_with_missing = 1L, n_flagged_model_features = 1L, threshold = .05,
    largest_shift = .10, preprocessing_strategy = "impute", scope_note = "Recorded raw missingness.",
    features = data.frame(
      feature = "x", used_by_model = TRUE, training_missing_rate = .20,
      evaluation_missing_rate = .30, rate_change = .10, flagged = TRUE
    )
  )
  html <- AutoXplainR:::render_missingness_shift(shift)
  expect_match(html, "Change (pp)", fixed = TRUE)
  expect_match(html, "percentage points (pp)", fixed = TRUE)
  expect_match(html, "At least 5.0 percentage points", fixed = TRUE)
  expect_match(html, "10.0 pp", fixed = TRUE)
  row <- regmatches(html, regexpr("<tbody>.*?</tbody>", html))
  values <- gsub("<[^>]+>", "", regmatches(row, gregexpr("<td[^>]*>.*?</td>", row))[[1L]])
  expect_identical(values, c("x", "yes", "20.0%", "30.0%", "+10.0", "yes"))
})

test_that("importance disagreement links to the retained candidate ranges with their scope", {
  audit <- list(
    config = list(metric = "mae"),
    performance = data.frame(model = c("a", "b", "unused"), near_optimal = c(TRUE, TRUE, FALSE)),
    explanation_agreement = list(importance_ranges = data.frame(
      feature = c("distance", "weight"), min_importance = c(1, -.5),
      max_importance = c(3, .5), mean_importance = c(2, 0)
    )),
    findings = data.frame(
      code = "rashomon_disagreement", severity = "critical", message = "Candidates rank inputs differently.",
      evidence = "Mean Spearman agreement: -1", recommendation = "Report candidate ranges."
    )
  )
  result <- list(model_labels = c(a = "Linear fit", b = "Tree fit", unused = "Unused model"))
  evidence <- AutoXplainR:::render_candidate_importance_ranges(audit, result)
  finding <- AutoXplainR:::render_findings(audit$findings, audit, result)
  expect_match(finding, 'href="#candidate-importance-ranges"', fixed = TRUE)
  expect_match(evidence, 'id="candidate-importance-ranges"', fixed = TRUE)
  expect_match(evidence, "Linear fit; Tree fit", fixed = TRUE)
  expect_false(grepl("Unused model", evidence, fixed = TRUE))
  expect_match(evidence, "Increase in MAE", fixed = TRUE)
  expect_match(evidence, "not confidence intervals", fixed = TRUE)
  expect_match(evidence, '>1.00</td><td class="number">3.00</td><td class="number">2.00</td>', fixed = TRUE)
  expect_match(evidence, '>-0.50</td><td class="number">0.50</td><td class="number">0.00</td>', fixed = TRUE)

  audit$performance$near_optimal <- c(TRUE, FALSE, FALSE)
  expect_identical(AutoXplainR:::render_candidate_importance_ranges(audit, result), "")
  expect_false(grepl(
    'href="#candidate-importance-ranges"',
    AutoXplainR:::render_findings(audit$findings, audit, result), fixed = TRUE
  ))
  expect_false(grepl(
    'href="#candidate-importance-ranges"',
    AutoXplainR:::render_findings(audit$findings, audit), fixed = TRUE
  ))
})

test_that("candidate importance labels describe score decreases for accuracy and AUC", {
  data <- data.frame(x = rep(c(0, 1), each = 10))
  data$y <- factor(ifelse(data$x == 1, "yes", "no"))
  probability <- function(newdata) .1 + .8 * newdata$x
  a <- explain_model(list(), data, "y", task = "binary", label = "A",
                     predict_function = probability)
  b <- explain_model(list(), data, "y", task = "binary", label = "B",
                     predict_function = probability)
  for (metric in c("accuracy", "auc")) {
    audit <- audit_explanations(list(a, b), metric = metric, n_repeats = 4, seed = 22)
    # Both unshuffled predictions classify and rank these rows perfectly.
    # A positive importance is the recorded score drop, not a score increase.
    drops <- vapply(audit$importance_objects, function(item) {
      expect_equal(attr(item, "baseline_score"), 1)
      expected <- 1 - mean(attr(item, "permuted_metric"))
      expect_equal(item$importance, expected)
      expected
    }, numeric(1))
    expect_true(all(drops > 0))
    expect_equal(audit$explanation_agreement$importance_ranges$min_importance, min(drops))
    expect_equal(audit$explanation_agreement$importance_ranges$max_importance, max(drops))
    caption <- if (metric == "auc") "Decrease in ROC AUC" else "Decrease in accuracy"
    expect_match(AutoXplainR:::render_candidate_importance_ranges(audit), caption, fixed = TRUE)
  }
})

test_that("a real failed family links from Checks to its recorded fold errors", {
  skip_if_package_unavailable("mgcv")
  set.seed(55)
  data <- data.frame(group = factor(rep(c("a", "b", "c"), 24)))
  data$y <- as.numeric(data$group) + rnorm(nrow(data), sd = .2)
  result <- autoxplain(
    data, "y", test_data = make_disjoint_evaluation(data, "y", 1:18),
    model_set = "tuned", learners = c("linear", "additive"), max_models = 4, nfolds = 3, seed = 71
  )
  expect_identical(result$tuning$families_resampling_failed, "additive")
  notes <- result$evaluation$notes
  note <- notes[notes$code == "tuning_family_resampling_failed", , drop = FALSE]
  html <- AutoXplainR:::render_guided_notes(note, result)
  expect_match(html, "Additive model fold failures", fixed = TRUE)
  expect_match(html, "for: Additive model.", fixed = TRUE)
  destination <- sub('.*href="#([^"]+)".*', "\\1", html)
  selection <- AutoXplainR:::render_model_selection(result)
  expect_match(selection, paste0('id="', destination, '"'), fixed = TRUE)
  failed <- result$tuning$candidates[result$tuning$candidates$family == "additive", , drop = FALSE]
  first <- failed$configuration_id[[1L]]
  expect_identical(destination, paste0("selection-detail-", AutoXplainR:::report_anchor(first)))
  errors <- result$tuning$fold_scores$error[result$tuning$fold_scores$configuration_id == first]
  expect_true(any(nzchar(errors)))
  expect_match(selection, AutoXplainR:::html_escape(errors[nzchar(errors)][[1L]]), fixed = TRUE)
  expect_match(html, "<summary>Inspect in R</summary>", fixed = TRUE)
  expect_match(html, "<code>tuning_results(result)$fold_scores$error</code>", fixed = TRUE)
  expect_false(grepl("tuning_results", sub("<details.*", "", html), fixed = TRUE))

  # Older saved runs cannot expose a destination their Selection tab omits.
  result$tuning$selection <- NULL
  legacy <- AutoXplainR:::render_guided_notes(note, result)
  expect_false(grepl("href=", legacy, fixed = TRUE))
  expect_match(legacy, AutoXplainR:::html_escape(note$recommendation), fixed = TRUE)
  expect_identical(AutoXplainR:::render_guided_notes(note), legacy)
})
