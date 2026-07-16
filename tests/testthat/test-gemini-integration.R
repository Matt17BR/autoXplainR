test_that("Gemini returns a validated evidence narrative", {
  skip_if_not(
    identical(tolower(Sys.getenv("AUTOXPLAIN_RUN_GEMINI")), "true"),
    "Set AUTOXPLAIN_RUN_GEMINI=true for the opt-in live API test."
  )
  skip_if_not(
    nzchar(Sys.getenv("GEMINI_API_KEY")),
    "GEMINI_API_KEY is required for the opt-in live API test."
  )

  result <- autoxplain(
    iris,
    "Species",
    model_set = "tuned",
    portfolio = "core",
    max_models = 5,
    nfolds = 3,
    test_fraction = 0.25,
    seed = 2026
  )
  memo <- generate_natural_language_report(
    result,
    provider = "gemini",
    fallback = FALSE,
    timeout = 90
  )
  provenance <- attr(memo, "narrative_provenance")

  expect_equal(provenance$provider_requested, "gemini")
  expect_equal(provenance$provider_used, "gemini")
  expect_equal(provenance$model, "gemini-3.5-flash")
  expect_true(provenance$remote)
  expect_true(provenance$structured_requested)
  expect_true(provenance$structured_used)
  expect_false(provenance$fallback)
  expect_match(memo, "Held-out performance", fixed = TRUE)
  expect_match(memo, "Required interpretation boundaries", fixed = TRUE)
  expect_match(memo, "No raw rows", fixed = TRUE)
})
