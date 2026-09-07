# Optional engine support

AutoXplainR 0.4.0 narrows its optional engine minimum versions to tested releases
available from CRAN. Older engine versions are no longer declared supported
merely because the package could load them. The minimum R version of 4.1 applies
to the core workflow; optional engines and their dependencies can require newer
R versions.

| Engine | Declared minimum and CI pin |
| --- | --- |
| e1071 | 1.7-17 |
| earth | 5.3.6 |
| glmnet | 5.0 |
| h2o | 3.44.0.3 |
| kknn | 1.4.1 |
| mgcv | 1.9-4 |
| ranger | 0.18.0 |
| xgboost | 3.2.1.1 |

The pinned mgcv release directly requires R >= 4.4.0, and the pinned xgboost
release directly requires R >= 4.3.0. These are not complete environment floors:
transitive dependencies can impose additional requirements. The complete engine
set is checked on current R release in CI, not on R 4.1.

The [official CRAN H2O metadata](https://cran.r-project.org/package=h2o), checked
on 2026-09-07, identifies version 3.44.0.3 and requires Java >= 8 and <= 17. The
live CI gate explicitly selects Temurin 17. The initial local H2O 3.46.0.9 was
newer than CRAN's published release; using it as the minimum prevented CRAN-based
dependency resolution, including a core report CI job. It remains part of the
earlier measured environment, but it is not the CRAN compatibility floor.

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
separate H2O integration/release gate reads the same DESCRIPTION pin, installs
only that engine in an isolated library, verifies its exact version, and runs
H2O preparation plus live binary, regression and multiclass tests. It records
the Java version, package library and server version. Release publication calls
this same reusable live workflow.

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

The initial check for the 0.4.0 repair used R 4.5.2 on Linux and the seven native
versions above, with local H2O 3.46.0.9. Its `--tests` run passed 694 assertions
across four targeted files; `/tmp/axr-engine-support` records that original
environment. Those results do not establish support for an older H2O release.

The corrected CRAN H2O 3.44.0.3 was installed into
`/tmp/axr-h2o-cran/library`, with the shared H2O 3.46.0.9 left untouched. The exact
installer and live-check CLI used by CI passed all 93 H2O assertions with no
test failures, warnings or skips in 40.1 seconds using an isolated Temurin
17.0.20.1+1 JRE. Startup and cluster-info calls emitted H2O's advisory notices
about the age of the CRAN release. The live server reported 3.44.0.3 with two
allowed cores and a 2 GB heap. The all-eight version checker also confirmed that
every installed engine matched its corrected DESCRIPTION minimum. A
version-only invocation establishes version/registry agreement, not model
correctness. Successful pinned CI and live H2O jobs on the exact release source
still need review before publication.

With isolated CRAN H2O first on the library path, the all-eight `--tests` gate
also passed all 694 assertions in 17.3 seconds with no failures, warnings or
skips. Its package and session records are in
`/tmp/axr-h2o-cran/all-engine-tests/`. Invalid engine names, incomplete native-test
engine sets, live tests without H2O, and installation without a dedicated library
were rejected. R parsing/lint, H2O/native-workflow YAML lint and diff checks
passed.

The tested [CRAN source archive](https://cran.r-project.org/src/contrib/h2o_3.44.0.3.tar.gz)
had SHA-256
`61a85f6c2f15e8e96839f8a4fd3a45eaa6bca90517bb20a4dd36e951d6fd0c82`.
The package downloaded its matching server JAR from
`https://s3.amazonaws.com/h2o-release/h2o/rel-3.44.0/3/Rjar/h2o.jar` and verified
its embedded MD5; the JAR's measured SHA-256 was
`f15727dc56bc4fa507c1492e1452b900c12a55b3a201228f10ee8f91b6f5f179`.
The [Temurin 17.0.20.1+1 release](https://github.com/adoptium/temurin17-binaries/releases/tag/jdk-17.0.20.1%2B1)
and its official API metadata provided the JRE archive checksum, verified as
`0b2b640e3046b64c8ec504de0ab9d91bb5610182bda21fad454681ce54d45a62`.
Source metadata, runtime provenance, cluster information, test results and the
all-engine version report were retained under `/tmp/axr-h2o-cran` locally. The
exact CLI runs wrote `checker-install/` and `checker-live/` there, including
`h2o-test-results.rds`, `java-version.txt` and `h2o-cluster-info.txt`.

The installer is intended for CI or a separate test environment and requires
`--install --library=/path/to/dedicated-library`. It refuses to install into any
existing active library. It never downgrades the shared default R library.
Support evidence for a release requires successful jobs on the release commit;
the presence of this workflow alone is not evidence that its tests passed.
The release workflow also calls the exact-minimum job as a publication gate.

For an isolated H2O-only reproduction, use a supported Java runtime and run:

```sh
Rscript .github/scripts/check-engine-support.R --engines=h2o --install \
  --library=/tmp/h2o-minimum-library --report=/tmp/h2o-support-install
Rscript .github/scripts/check-engine-support.R --engines=h2o --live-h2o \
  --library=/tmp/h2o-minimum-library --report=/tmp/h2o-support-tests
```

`--engines` accepts only unique names from the eight supported engines. The
native `--tests` gate requires all eight; `--live-h2o` explicitly starts the
H2O integration gate and requires H2O in the selected set. Version evidence is
written before fitting so a later test failure does not erase provenance.
