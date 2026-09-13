# Forest validation budget amendment, version 2

Declared on 12 September 2026 after the frozen candidate-v2 Year development
run timed out and before any locked acceptance outcomes were opened. This
amendment changes automatic search approximation, not final forest size, fold
coverage, evaluation partitions, scoring gates or resource ceilings.

## Operational evidence and decision

Candidate v2 used five complete CV folds with 256-tree forests and an explicit
four native threads. On the fixed 50,000-row Year development pool it completed
all CV and the primary boosting refit, then exceeded the 1,200-second process
limit during the mandatory 500-tree forest refit. The native call log records
all 50,000 final training rows, `mtry=30`, node size 20 and sample fraction 0.8.
The frozen failure remains under
`runs/candidate-paired-4t-v2/yearprediction/package` in the benchmark cache;
no quality result is substituted for its incomplete final forest.

Automatic adaptive forest CV now uses this deterministic outer-training input
work proxy, `number of rows * number of raw predictors`:

| Input work | CV trees | Screening trees | Final all-training trees |
|---|---:|---:|---:|
| Below 1,000,000 | 500 | 128 | 500 |
| 1,000,000 to below 4,000,000 | 256 | 128 | 500 |
| At least 4,000,000 | 128 | 128 | 500 |

The policy identifier is `forest-validation-budget-v2`. All requested CV folds
and every row in their training partitions remain included. Grid search,
explicit grids and exact-budget search retain requested tree counts. Effective
tree counts change the existing parameter key and deterministic fitting seed;
requested settings remain 500. Successful and failed native fit evidence
records the actual count and policy identifier.

Four million is a coarse computational tier, not a measured optimal threshold.
It is four times the existing large-input boundary, applies to Year development
work of 4.5 million and both full acceptance pools, and leaves Covertype
development work of 2.7 million at 256. The work proxy omits task-specific split
cost, `mtry`, node size and hardware. For five equal folds, changing CV from 256
to 128 reduces CV-plus-final normalized row-tree work from 1524 to 1012,
or 33.60%. It leaves final fitting, boosting and report costs intact and does
not establish completion within either resource gate.

## Existing approximation evidence and its limits

The fixed development probes already compared saved native forest prefixes of
128, 256 and 500 trees. Independent per-tree aggregation checked numerical
agreement with native prediction. Relevant observed loss differences from 500:

| Development model | 128-tree difference | 256-tree difference |
|---|---:|---:|
| Year `mtry=30`, node size 20, RMSE | +0.008514 | -0.000718 |
| Bank native forest, log loss | +0.001682 | +0.000199 |
| Covertype native forest, log loss | +0.001944 | +0.000095 |

Sources: [Year prefixes](forest-policy/prefix-results.json) and
[classification prefixes](forest-policy/classification-prefix-results.json).
These development outcomes were already used for iterative diagnosis. They
are not locked acceptance evidence. Prefixes overlap; their agreement does not
prove equivalence of separately fitted forests with different parameter-derived
seeds, preserve all class decisions, or establish identical model selection.
The smaller CV forests approximate the final model and must be labeled as such.
The medium work tier retains 256 because the approximation gap is smaller and
the observed Year failure does not justify widening it to all large inputs.

The benchmark agent will freeze a new candidate and rerun the same Year
development workload at four threads. The already-running full native Year
reference and frozen Covertype v3 run remain unchanged. The final 500-tree
forest must independently pass the full-row fit, held-out quality and saved
replay requirements in [forest-family-acceptance.md](forest-family-acceptance.md).
The complete one-call explanation/report gate remains necessary.

## Implementation checks

Small native regression, binary and multiclass fixtures independently match
ranger's 128-tree probabilities or predictions, tree structures and expected
effective seeds. Final native fits retain 500 trees and the original final seed
formula. Failure injection checks actual 128-tree attempted settings and counts.
Separate mock orchestration at the real 50,000-row planning size checks all
five 40,000-row CV training partitions and the full 50,000-row final fit for both
256 and 128 tiers. Mock fits do not supply timing or predictive evidence.
