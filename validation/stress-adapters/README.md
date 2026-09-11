# Adapter stress tests

These probes exercise the public `autoxplain()` workflow, including fold-specific
preprocessing, candidate failures, refitting and prediction from raw data. They
use deliberately small searches to diagnose adapters. They do not establish that
the default search finds the best model on a difficult real dataset.

The before runs use the installed, published 0.6.1 package. The after runs use the
working source. Both use R 4.5.2, the same data and seeds, three folds and one
configuration per requested learner family. Native threads are limited to one.

## Reproduced defects and repairs

| Case | Published 0.6.1 | Repaired behavior |
| --- | --- | --- |
| 75 rows and 100 numeric predictors | The public call had not finished after 381.82 seconds and was stopped. A separate profile of the published package located the cost in GAM REML fitting on 40 rows with 100 smooths. | Completes in 3.42 seconds. Six other learner families and the baseline remain available; the GAM capacity exclusion appears in the failed fold records. |
| A factor named `x` with level `b`, plus a numeric column named `xb` | Both became an encoded `xb` column. MARS failed every fold with `Duplicate colname in x`. | Stable unique encoded names preserve both inputs and their original feature mapping. MARS completes CV and returns finite predictions. |
| Rescale a predictor by `1e160` | Standard deviation overflow was mistaken for zero variance. The supposedly standardized values remained enormous, and the public SVM candidate failed with `Model is empty!`. | Standardization matches the original units within `2.3e-16`; the SVM candidate completes with CV RMSE 0.0666. Very small scales and subtraction overflow have separate tests. |
| 4,800 rows with a 1,200-level factor | glmnet preprocessing allocated a dense 4,800 by 1,202 matrix occupying 46.57 MB. | The sparse representation occupies 0.621 MB, about 75 times less. An independent dense glmnet fit gives the same predictions within `1e-8`. |

The GAM restriction is an automatic-search policy: the number of predictor terms
must be smaller than the number of rows in each fitting fold. It is not a claim
that all penalized GAMs require this. Externally fitted GAMs can still be evaluated
with `evaluate_models()`. The catalog and recorded learner manifest disclose the
policy. Boundary tests check equality, one row above the limit, and a 12-term,
40-row case without invoking an expensive native fit at the excluded boundary.

Only glmnet uses sparse encoding. XGBoost retains dense encoding because its
treatment of implicit sparse zeros differs from ordinary numeric zeros.

## Broader results

The nine cases in `probe.R` completed after the repairs. Every retained model
returned finite predictions for raw input rows and identical predictions after
an RDS round trip.

| Case | After runtime | Families with no valid CV configuration |
| --- | ---: | --- |
| 75 rows, 100 predictors | 3.42 s | Additive, by the disclosed capacity policy |
| Correlated, duplicate and constant predictors | 2.90 s | None |
| 420 rows, 180 possible factor levels | 9.98 s | None |
| Binary outcome with 4 rare cases among 150 rows | 12.08 s | Additive, with recorded optimizer nonconvergence |
| Three classes, including 10 rare cases among 180 rows | 0.81 s | None among the five requested supported families |
| Numeric predictors with few distinct values | 1.15 s | None |
| Predictor units multiplied by `1e160` | 0.61 s | Additive and boosting |
| Predictor units multiplied by `1e-160` | 0.61 s | Additive and MARS |
| Outcome units multiplied by `1e30` | 1.27 s | None |

Times describe these small, single-configuration searches on this machine. They
are not speed guarantees. Extreme-unit failures remain real limitations of some
adapters; other families continue and the failures remain visible. The rare-class
exercise checks failure handling, not the reliability of predictions from four
positive observations. Repeated or external validation needs substantially more
information than this fixture provides.

The initial sparse implementation broke Matrix's handling of spaces and other
punctuation in predictor names. Existing native-engine tests caught this. The
repair uses safe internal names while preserving the dense encoder's column
names and source mapping. Tests compare numeric values, column order, logical
inputs, ordered factors, one-hot encoding, unseen-level errors, literal colons and
backticks, and serialization. The existing native-engine tests were retained.

## Run the probes

Run these scripts from the package root. Set `AXR_ADAPTER_LIBRARY` to an installed
package library to use a released baseline; omit it to load the working source.
Set `TMPDIR` if the system temporary directory has limited space.

```sh
AXR_ADAPTER_LIBRARY=/path/to/baseline-library \
  AXR_ADAPTER_OUTPUT=/path/to/evidence/before \
  Rscript validation/stress-adapters/probe.R

AXR_ADAPTER_OUTPUT=/path/to/evidence/after \
  Rscript validation/stress-adapters/probe.R

AXR_ADAPTER_LIBRARY=/path/to/baseline-library \
  AXR_ADAPTER_OUTPUT=/path/to/evidence/defects-before.json \
  Rscript validation/stress-adapters/reproduce-defects.R

AXR_ADAPTER_OUTPUT=/path/to/evidence/defects-after.json \
  Rscript validation/stress-adapters/reproduce-defects.R

AXR_MATRIX_OUTPUT=sparse AXR_ADAPTER_OUTPUT=/path/to/evidence/sparse.json \
  Rscript validation/stress-adapters/matrix-memory.R
```

`probe.R` records progress for every candidate fit and saves each result, complete
fold evidence, warnings, prediction checks and runtime in `summary.json`.
`AXR_ADAPTER_CASES` accepts a comma-separated subset. Set
`AXR_ADAPTER_PROFILE=true` to save an R profile. The high-dimensional case on the
unmodified release can take several minutes; the recorded before run was stopped
after establishing the cause rather than pretending it had passed.

`matrix-memory.R` measures stored matrix size and the process's maximum R vector
heap usage during encoding in a fresh R process. Run dense and sparse modes in
separate processes. Package loading is outside the timer, but the first use of
the optional Matrix namespace can occur inside it.

Local evidence for this run is under
`~/.cache/autoxplain-stress-0.6.2/adapters/`, including the stopped-run record,
before and after JSON, R profiles and test logs. The regression tests live in
`tests/testthat/test-adapter-robustness.R`.
