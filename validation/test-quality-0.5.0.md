# What the tests actually prove

The maintainer asked us to challenge the tests before trusting their totals.
This review concentrated on the changed report, model details, model selection
and preprocessing, and inspected the existing analytic statistical checks.
It is not an exhaustive mutation score for the whole package.

## Defects found in the checks

| Check | Why it was insufficient | Change |
|---|---|---|
| Report curves and cost charts | The browser checked selected panels and table values. Flattening 16 curves and moving all 18 cost points in the quick fixture still passed all 215 checks. | Read plotted positions through the visible axis ticks, compare them with retained values, and independently check frontier membership. Check displayed cost cells too. |
| Fold preprocessing | A test with no missing numeric values checked row counts and `novel_levels_mapped >= 0`. Deliberately imputing from the full training set still passed its three assertions. | Replace it with a missing-value fixture whose fold medians differ substantially. Compare every out-of-fold prediction and fold RMSE with explicit base-R imputation and `lm()` fits. |
| Class-code invariance | The test name promised invariance, but it did not change class labels. Its expected value repeated the production calculation. | Use three prescribed prediction patterns with hand-counted agreement of 7/12; rename classes and reorder probability columns and factor levels. |
| Fitted tree depth | The expected result copied the exact node-number formula used by the implementation. Both could share the same error. | Fit a three-region example that has three leaves and depth two, then check those known answers. |
| Report markup | Presence of control IDs and specific CSS text was counted alongside actual interaction checks. Some assertions froze a width of 600px. | Remove 14 duplicated or incidental assertions. Keep escaping checks and the browser's actual navigation, sizing and print tasks. |

The tree, class-code and fold tests replace weaker tests. More assertions are not
the objective. The browser's numerical answer data still come from R: these are
checks of faithful presentation, not independent verification of every estimator.
Likewise, model-specification answer data check identity and wiring; comparisons
with native fitted controls and the known tree fixture establish separate facts.

## Deliberate faults now required to fail

`check-report-mutations.py` makes disposable copies of the quick report. One
disconnects model selection. Another preserves the tables but falsifies the
graphics. Both must produce the intended assertion failures, without execution
errors. A timeout or parser error does not count as detecting the fault.

`check-tuning-mutation.R` first runs the new fold test unchanged: all three
assertions pass. It then replaces fold-local imputation with full-training
imputation in that R process: the prediction and score assertions fail. The
fixture separately checks that those two calculations differ, so the mutation
cannot pass unnoticed because the chosen data happen to give identical answers.

Both checks are release gates. The browser mutations also run on pull requests;
the imputation mutation runs with statistical validation. No production source
is rewritten by these scripts.

## Useful checks retained

Perturbing held-out outcomes while requiring unchanged training selection tests
an actual boundary. The analytic ALE examples use known linear and nonlinear
functions, and the confusion-metric checks enumerate small truth/prediction
patterns with independently counted answers. Those provide stronger numerical
evidence than object classes or matching prose. Invalid-input, serialization,
optional-engine and schema tests remain useful for their stated contracts;
they should not be described as statistical validation or evidence of usability.

Run the required [product walkthrough](product-walkthrough.md) as well. Tests
can reject known wrong behavior; they cannot establish that a report answers
the questions a reader needs to ask.
