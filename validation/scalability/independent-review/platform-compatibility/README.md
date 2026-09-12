# Platform checks before 0.7.0

Two failures in the first CI run required different corrections. Neither test
tolerance was relaxed. The results below are local evidence; the updated
macOS ARM and R 4.1 CI jobs must still pass before release.

## Categorical association on macOS ARM

The sparse statistic was correct. Its dense test reference summed 1.32 million
floating-point Pearson residual terms. On macOS ARM, that sum lost enough
precision to fail the existing `1e-13` tolerance.

The fixture has 3,600 observations, 1,200 row categories with three observations
each, and column margins of either three or four. Therefore an expected cell
count is `C / 1200`. Its Pearson contribution is
`(1200 * O - C)^2 / (1200 * C)`. Multiplying every contribution by 14,400 gives
an integer because `C` is three or four. All terms and their positive total,
19,005,120,000, are below `2^53`, so even ordinary double accumulation is exact.

This independent dense calculation gives chi-square 1,319,800 and Cramer's V
0.57756911998752014. Both orientations of the production sparse calculation
match it. An explicit sequential double sum reproduces the old reference's
error of about `-8.11e-12`; the test now uses the integer-scaled reference.
Production association code is unchanged.

Run from the repository root:

```sh
Rscript validation/scalability/independent-review/platform-compatibility/association.R
```

The probe writes to `~/.cache/autoxplain-platform-review`, or to
`AXR_PLATFORM_REVIEW_OUTPUT`. It records actual values, not only a pass flag.
The committed verdict also preserves the original rounded CI values and the
reviewed source hashes.

## Exact overlap with byte-marked text on R 4.1

R 4.1's `match()` rejects some mixtures of byte-marked and encoded strings.
The faster overlap path now defers such candidate text columns to the existing
exact serialized-row comparison. Other columns still discard impossible
matches first. Ordinary text keeps the fast columnwise path.

The original mixed-encoding fixture remains unchanged. Additional checks cover
candidate filtering with a byte column first or last and prevent ordinary text
from silently falling back to row serialization. All 96 focused assertions
passed locally. A separate 307-case comparison against the actual published
0.6.2 namespace also passed, including signed zeros, NA/NaN distinctions,
encoding flags, type normalization and shared references in unusual columns.

The independent harness is
[`million/check-overlap.R`](../../million/check-overlap.R). It takes the
published package library and an existing output directory as arguments.
This correction preserves the previous equality semantics; it does not define
new text equivalence rules.

## Warnings in the first R 4.1 job

The job reported 698 warnings: 692 from `rpart` 4.1.16's internal partial
`fit$split` match, and six from `predict.lm()` on the intentionally aliased
linear-model fixture. Both underlying prediction paths and the aliased fixture
already existed in 0.6.2. No warning has been suppressed for this review.

The published 0.6.2 release's
[R 4.1 job](https://github.com/Matt17BR/autoXplainR/actions/runs/34659828765/job/103459820923)
used the same R 4.1.3, rpart 4.1.16, withr 3.0.3 and testthat 3.3.2 versions.
It reported 692 warnings: 686 of the same internal rpart partial matches and
the same six linear-model warnings. Every pre-existing test has the same
warning count. The six additional rpart warnings come from the new streaming
tuning test exercising that existing dependency path. This is inherited
dependency behavior, not evidence of a new tuning or option-cleanup defect.
The strict partial-match test and its option scope remain unchanged.

## Which installed package a replay checks

The main million-row runner already rejected a silently substituted package
library. The cold-replay and before/after comparison scripts now enforce the
same exact loaded-path check. The comparison verdict also records that path
and package version. Existing replay evidence recorded the expected library;
this guard prevents a future missing installation from falling through to a
different package on `.libPaths()`.

Each actual script prefix, through its new assertion, was run in a fresh R
process against an existing empty library and the installed published 0.6.2
library. Both empty-library attempts rejected the fallback installation, and
both installed-library attempts passed. These four checks did not recompute
model fits or large prediction arrays. The committed guard verdict records
their scope and the reviewed script hashes.
