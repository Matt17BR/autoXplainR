# Streaming tuning and out-of-fold evidence review

The independent replay found no streaming regression in failed-fit handling,
omitted validation rows, probability ordering, pooled fold weights or optional
OOF retention. It did find two older inconsistencies in the stored binary
prediction evidence. Those are corrected separately from the optimization.

The fixture uses 48 training rows, folds of 12, 16 and 20 rows, and six missing
predictor values removed by explicit preprocessing. One tree configuration is
made to fail only its second fold. Its partial predictions must be excluded;
the two valid configurations must each retain every eligible original row
exactly once. Binary and multiclass factors use deliberately nonalphabetic
class orders. Regression, binary and multiclass runs each compare OOF retention
on and off.

Before the evidence fixes, all six outputs matched the immutable published
0.6.2 outputs exactly. Afterward, fold scores, uncertainty summaries, selection,
failures, omissions, source identities, probabilities and final predictions
still matched exactly. The [verdict](verdict.json) identifies the numerical
changes explicitly. It does not hide them with a tolerance or round them away.

## Two corrected inconsistencies

Binary fold scoring clips the positive probability to `[1e-15, 1 - 1e-15]`.
Stored OOF case losses previously clipped the probability assigned to the
observed class instead. At an extreme prediction these differ: the finite
double representing `1 - 1e-15` has a complement slightly below `1e-15`.
The tree's mean stored log loss was 4.0966942940754612, while its CV score was
4.0967133321095135. OOF case losses now use the same convention as fold scoring.
Native probabilities remain unchanged. The same correction applies to Brier
case losses at the clipping boundaries.

Across the 252 retained records in this replay, 17 binary case losses changed;
the largest correction was 0.000799597430194865. Means of the corrected records
now reproduce the CV scores. The independent boundary test specifies expected
losses directly and an actual pure-leaf tree with omitted rows checks pooling.

Binary OOF labels also used the first class when both probabilities equaled
0.5, while public prediction selected the positive class. A separate balanced
48-row stump reproduces this inconsistency exactly. OOF labels now follow the
same `p >= 0.5` rule. Multiclass ties retain their previous rule.

These are the expected exceptions in old/new evidence comparisons: binary
`case_loss`, and binary `predicted_class` at exactly 0.5. Model fitting,
probabilities and model selection are unaffected. Existing tests that merely
repeat the old case-loss formula cannot establish cross-validation consistency;
the new tests check independent boundary values and actual pooled losses.

## Reproduce

From the repository root, with a separately installed published 0.6.2 library:

```sh
Rscript validation/scalability/independent-review/streaming/replay.R /path/to/published-library /tmp/baseline.rds
Rscript validation/scalability/independent-review/streaming/replay.R source /tmp/candidate.rds
Rscript validation/scalability/independent-review/streaming/check.R /tmp/baseline.rds /tmp/candidate.rds /tmp/verdict.json
Rscript validation/scalability/independent-review/streaming/tie.R
```

The failure injection changes a fitter binding only within the replay process.
It does not edit package source. The two workflow losses used here are RMSE and
log loss; focused package tests additionally check Brier clipping. These small
correctness cases are not performance measurements or a claim that all model
families have been exercised.
