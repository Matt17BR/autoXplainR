# AutoXplainR release checklist

Releases are cut from a clean `main` branch after all required GitHub Actions
checks pass. The maintainer owns the final statistical-language and CRAN-policy
review; automation is supporting evidence, not a substitute for that review.

## Prepare

- Confirm the version and release date in `DESCRIPTION`, `NEWS.md`,
  `CITATION.cff`, and `cran-comments.md`.
- Confirm that README and vignette examples still express the beginner-first
  product contract in `PRODUCT.md`.
- Review user-facing metrics, warnings, and narrative prompts for unsupported
  causal, fairness, or certainty claims.
- Run `devtools::document()` and ensure the generated files are committed.
- Run `spelling::spell_check_package()` and `urlchecker::url_check()`.
- Run `lintr::lint_package()`, `testthat::test_local()`, and
  `covr::package_coverage()`; statement coverage must remain at least 80%.
- Run `devtools::check(args = "--as-cran")` and explain every remaining note in
  `cran-comments.md`.
- Run the isolated real-H2O test with `AUTOXPLAIN_RUN_H2O=true`.

## Validate remotely

- Require successful R CMD checks on Ubuntu (devel, release, and oldrel),
  Windows, and macOS.
- Require successful lint, spelling, link, coverage, pkgdown, and H2O jobs.
- Open the deployed pkgdown site and smoke-test the beginner workflow on a
  narrow mobile viewport as well as desktop.
- Verify that no workflow log, fixture, or report contains an API key or raw
  private data.

## Publish

- Create and push the signed version tag `v<DESCRIPTION version>`.
- Inspect the workflow-built source package and its SHA-256 checksum.
- Review and publish the draft GitHub release created by the release workflow.
- Submit the exact checked source archive to CRAN and record any reviewer
  feedback in `cran-comments.md`.
- After acceptance, verify CRAN installation, the pkgdown site, citations, and
  release links from a clean R library.

Do not reuse or move a published version tag. Corrections require a new package
version and a new tag.
