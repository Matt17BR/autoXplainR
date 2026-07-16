# Source and intellectual-property provenance

This record covers the source tree prepared for AutoXplainR 0.2.0. It is
kept outside the CRAN source archive because it documents repository
governance rather than installed package behavior.

## Repository audit

- The Git history through 2026-07-16 attributes the project source,
  documentation, tests, and generated website inputs to Matteo
  Mazzarelli, the author and maintainer named in `DESCRIPTION`.
- The package contains no vendored third-party source tree, compiled
  binary, embedded model, or bundled external data set. Examples and
  tests refer to standard R data such as `mtcars` and `iris` or
  construct data at runtime.
- Optional model engines, reporting tools, and hosted narrative
  providers are integrations reached through declared R dependencies or
  user-supplied API endpoints. Their source code and credentials are not
  redistributed.
- Statistical methods are implemented for this project from their
  published descriptions. Central method sources are cited in
  `DESCRIPTION`, the statistical-methods vignette, and the package
  citation material; source code from those publications or from other R
  packages is not bundled here.
- AutoXplainR’s project-specific source and documentation are released
  under the MIT license. `DESCRIPTION`, `LICENSE`, and `LICENSE.md`
  record the author, copyright holder, years, and distribution terms.

The repository history is useful audit evidence, but it does not replace
the maintainer’s legal responsibility. Before every release, the
maintainer reviews new files and contributors, confirms that this record
is still accurate, and adds contributor (`ctb`) or copyright-holder
(`cph`) roles and retained notices whenever outside material is
accepted.

## Contribution control

`CONTRIBUTING.md` and the pull-request template require contributors to
confirm that submitted work can be distributed under the project
license, to identify derived material, and to document the origin and
redistribution terms of data and fixtures. A contribution with unclear
provenance is not release-ready.
