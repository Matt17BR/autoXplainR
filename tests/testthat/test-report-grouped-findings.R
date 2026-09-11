test_that("grouped shuffle findings preserve each evidence link and distinct claim", {
  findings <- data.frame(
    severity = c("note", "warning", "note"),
    code = c("shuffle_interval_unresolved", "other_warning", "shuffle_interval_unresolved"),
    message = c("first", "Unrelated diagnostic", "second"),
    evidence = c("The interval includes zero.", "Other evidence.", "One repeat cannot estimate an interval."),
    recommendation = c("Inspect the repeats.", "Review the fit.", "Increase repeat count."),
    model = c("model_a", NA, "model_b"), feature = c("x<1", NA, "y&2")
  )
  html <- render_findings(findings)
  expect_match(html, 'data-evidence-model="model_a" data-evidence-feature="x&lt;1"', fixed = TRUE)
  expect_match(html, 'data-evidence-model="model_b" data-evidence-feature="y&amp;2"', fixed = TRUE)
  expect_match(html, "The interval includes zero.", fixed = TRUE)
  expect_match(html, "One repeat cannot estimate an interval.", fixed = TRUE)
  expect_match(html, "Inspect the repeats. Increase repeat count.", fixed = TRUE)
  expect_match(html, "Unrelated diagnostic", fixed = TRUE)
  expect_match(html, "Other evidence.", fixed = TRUE)
  expect_match(html, 'href="#evidence-', fixed = TRUE)
  expect_match(render_findings(findings[1L, , drop = FALSE]), "first", fixed = TRUE)
})
