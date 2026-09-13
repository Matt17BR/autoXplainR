# Source and intellectual-property provenance

This record accompanies the unreleased AutoXplainR 0.8.0 candidate. It is kept
outside the CRAN source archive because it documents repository governance
rather than installed package behavior.

## Repository audit

- The initial 2026-09-13 review covered six commits from `v0.7.0` through
  `23d541d1e2889da710c9fbe09252aa617f5dbf89`. A follow-up review covered the next
  five commits through `c9a3fb0b66b54cac7bfe2d01eb75b4868833ecda`, comprising
  68 changed files and 12 additions. The initial commits record Matteo
  Mazzarelli's account and email as author and committer; the next five record
  `Matt17BR <matteo.mazzarelli@gmail.com>` in both roles. No additional contributor
  or newly bundled third-party implementation was identified. Git attribution
  records authorship claims; it does not independently establish ownership.
- The package includes the unmodified UMD distribution of fflate 0.8.3, with an
  added attribution header, for offline report decompression. Its MIT license
  and 2026 Arjun Barrett copyright notice are retained in
  `inst/report/fflate-LICENSE.txt` and embedded in generated reports.
  `inst/report/fflate-source.json` identifies the upstream archive, original
  file and SHA256 checksums. `validation/scalability/reports/check-vendor.py`
  verifies the distributed file and attribution. `DESCRIPTION` identifies the
  third-party copyright holder separately from the package author.
- The package contains no compiled binary. Examples and tests use standard R
  data such as `mtcars` and `iris` or construct
  synthetic data at runtime. Public website reports deliberately embed their
  example records and fitted summaries: synthetic delivery and churn data, and
  R's `iris` data. Their generators are in `validation/`; website HTML is excluded
  from the R source archive. Screenshots show those same public examples.
- The 0.8.0 modeling benchmarks also use UCI's YearPredictionMSD, Covertype and
  Bank Marketing datasets. Their authors, DOIs, source links and CC BY 4.0 terms
  are recorded in `validation/competitive-tabular/README.md`; `sources.json`
  records the downloaded archive and member hashes. Raw observations, complete
  predictions and fitted models remain outside Git. Committed results contain
  measurements, feature summaries, file hashes, benchmark command references
  and host/resource metadata, including local cache paths. The published
  host-wide process snapshot contains counts and a digest rather than process
  arguments; raw diagnostic originals remain in the private cache. The
  benchmark directory is excluded from the R source archive.
- Optional model engines, reporting tools, and hosted narrative providers are
  integrations reached through declared R dependencies or user-supplied API
  endpoints. Their source code and credentials are not redistributed.
- Statistical methods are implemented for this project from their published
  descriptions. Central method sources are cited in `DESCRIPTION`, the
  statistical-methods vignette, and the package citation material; source code
  from those publications or from other R packages is not bundled here.
- AutoXplainR's project-specific source and documentation are released under
  the MIT license. `DESCRIPTION`, `LICENSE`, and `LICENSE.md` record the author,
  copyright holder, years, and distribution terms.

The repository history is useful audit evidence, but it does not replace the
maintainer's legal responsibility. Before every release, the maintainer reviews
new files and contributors, confirms that this record is still accurate, and
adds contributor (`ctb`) or copyright-holder (`cph`) roles and retained notices
whenever outside material is accepted.

## Contribution control

`CONTRIBUTING.md` and the pull-request template require contributors to confirm
that submitted work can be distributed under the project license, to identify
derived material, and to document the origin and redistribution terms of data
and fixtures. A contribution with unclear provenance is not release-ready.
