# Optional engine support

AutoXplainR 0.4.0 narrows its optional engine minimum versions to the versions in
the local validation environment. Older engine versions are no longer declared
supported merely because the package could load them. The minimum R version of
4.1 applies to the core workflow; optional engines and their dependencies can
require newer R versions.

| Engine | Declared minimum and CI pin |
| --- | --- |
| e1071 | 1.7-17 |
| earth | 5.3.6 |
| glmnet | 5.0 |
| h2o | 3.46.0.9 |
| kknn | 1.4.1 |
| mgcv | 1.9-4 |
| ranger | 0.18.0 |
| xgboost | 3.2.1.1 |

The pinned mgcv release directly requires R >= 4.4.0, and the pinned xgboost
release directly requires R >= 4.3.0. These are not complete environment floors:
transitive dependencies can impose additional requirements. The complete engine
set is checked on current R release in CI, not on R 4.1.

## What is checked

The `native-model-engines` workflow keeps its current-dependency Ubuntu and
Windows jobs and adds an Ubuntu/current-R job for the declared minimum versions.
The minimum job reads every pin from DESCRIPTION using R's DCF parser, installs
the pins into a dedicated library, then checks them again in a fresh R process.
It fails if an engine resolves from another library, its version differs from
the declared minimum, or the learner registry has a different minimum.

The same job runs the native adapter, kernel geometry, matrix blueprint, and
audit data contract tests. H2O is installed at its minimum and its package load
is verified, but this job does not start Java or exercise a live H2O model. The
separate H2O integration/release gate provides that evidence.

This tests one set of direct engine minima with the transitive dependencies
available at the time of the run. It does not test every older transitive
dependency, every possible dependency combination, or every R/OS combination.
Newer engine releases remain subject to the separate current-dependency jobs.
An engine pin is a compatibility floor, not a guarantee about every future
release. CI uploads the exact engine table, `sessionInfo()`, and the installed
package inventory so failures can be attributed to the environment actually
tested.

## Local verification and release evidence

From the repository root, check the current library without installing or
modifying packages:

```sh
Rscript .github/scripts/check-engine-support.R --report=/tmp/axr-engine-support
```

The local check for the 0.4.0 repair used R 4.5.2 on Linux and the eight versions
in the table above. Running the checker with `--tests` passed 694 assertions
across the four targeted test files, with no failures, warnings, or skips.
Environment artifacts were written to `/tmp/axr-engine-support` during the
repair. A version-only invocation establishes exact version/registry agreement;
it does not by itself establish model correctness. The successful pinned CI
job on the release commit and the separate live H2O gate still need review
before release.

The installer is intended for CI or a separate test environment and requires
`--install --library=/path/to/dedicated-library`. It refuses to install into any
existing active library. It never downgrades the shared default R library.
Support evidence for a release requires successful jobs on the release commit;
the presence of this workflow alone is not evidence that its tests passed.
The release workflow also calls the exact-minimum job as a publication gate.
