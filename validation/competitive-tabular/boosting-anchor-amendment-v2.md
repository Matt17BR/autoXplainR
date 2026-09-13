# Matched boosting depth anchors, version 2

Declared on 12 September 2026 after the frozen Covertype candidate-v3 development
run and before any locked acceptance outcomes were opened. The adaptive search
policy becomes `adaptive-screening-v2`. Existing cohorts remain unchanged.

## Development evidence

The completed Covertype v3 paired run took 837.524 seconds with four native
threads and peaked at 2,229,952 KiB RSS. Its primary model's development log loss
was 0.287632 versus 0.256900 for the matched native boosting reference. The
original relative-loss formula would give 0.284590 on these development scores;
this comparison diagnoses a gap and is not a locked acceptance result. Its
retained forest scored 0.332314 versus the native forest's 0.335199, so a useful
forest does not resolve the boosted-model coverage gap.

The five allocated boosting screening settings were:

| Depth | Eta | Child weight | Lambda | Alpha | Selected/attempted rounds | Screening log loss |
|---|---:|---:|---:|---:|---:|---:|
| 3 | 0.10 | 1 | 1 | 0 | 600/600 | 0.490215 |
| 6 | 0.05 | 3 | 3 | 0 | 595/600 | 0.430182 |
| 10 | 0.03 | 10 | 10 | 0.1 | 600/600 | 0.437678 |
| 7 | 0.086 | 1.32 | 5.87 | 0 | 434/464 | 0.415990 |
| 3 | 0.0488 | 13.9 | 1.2 | 0 | 600/600 | 0.527659 |

The source is the frozen `candidate-paired-4t-v3/covertype/package` process and
summary records. The depth-seven proposal advanced. The native reference's
depth-ten, eta 0.05, child-weight one, lambda one setting was absent. The three
original anchors confounded increased depth with slower learning, stronger
child constraints, stronger penalties and changed row/column sampling. All
three reached the common screening round cap. This does not prove that missing
coverage caused the final score gap, but it substantiates a general search
design defect.

## Bounded correction

Keep three anchor slots at depths 3, 6 and 10. Hold their other controls fixed:
eta 0.05, child weight one, lambda one, alpha zero, row sampling 0.8 and column
sampling 0.8. This includes the predeclared native medium/deep reference shapes
and makes depth the only changing anchor control. The seeded spread continues
to vary learning rate, regularization and sampling after the three anchors.
No dataset identifiers, target values or held-out scores enter generation.

The rationale follows the official [XGBoost tree-parameter reference](https://xgboost.readthedocs.io/en/stable/parameter.html#parameters-for-tree-booster):
depth increases potential complexity; smaller learning rates shrink updates;
larger child weights constrain further splits; and larger L1/L2 penalties make
fitting more conservative. Holding these controls constant permits an
interpretable capacity comparison. Eta 0.05 is an existing search/reference
value, and the child-weight and regularization values are native defaults;
their combination is a practical anchor, not a universal optimum.

Configuration budgets, three anchor slots, seeded proposal streams, 600-round
screening cap, 2,000-round full calibration cap, patience, finalist count, all
CV folds and full-training refits remain unchanged. This is not an equal-time
comparison: weakly constrained deep trees can cost more per round, and the
slower shallow anchor can need more rounds. There is no promised speedup or
guaranteed score improvement. No scheduling or resource ceiling is expanded.

## Checks and next run

Focused tests check actual allocated five-setting paired and six-setting
tabular family plans, matched non-depth controls, exact budgets and continued
seeded diversity. Anchors are identical across regression, binary and multiclass
tasks with different input dimensions. Tiny native regression and classification
fits at depths six and ten reproduce independently specified XGBoost models,
including reordered class probabilities and native parameter inspection. These
are coverage and adapter checks, not predictive quality benchmarks.

The next frozen candidate includes this amendment and the separately declared
[forest CV tier](forest-validation-budget-v2.md). It must rerun the fixed
development workloads before any final acceptance scoring. Earlier failures,
the successful Covertype v3 record, the running full native references,
partitions, resource limits and final forest/selected-model acceptance gates
remain intact. Complete explanation/report acceptance is still required.
