# Actual 256-tree development comparison

Recorded 13 September 2026 under the predeclared
[diagnostic protocol](final-tree-protocol.md). All twenty supervised stages
completed successfully. The largest observed process RSS was 0.944 GiB for
Year and 1.917 GiB for Covertype, within the separate stage limits.

The diagnostic uses the recovered v4 development models and two newly fitted
256-tree forests. Each forest retains all 50,000 processed development training
rows, four native threads, and final OOB diagnostics. Predictions use all 20,000
previously used development assessment rows. No acceptance outcomes were opened.

Only the tree count changes in the requested configuration. That change also
changes the production parameter-derived fitting seed, so the comparison measures
tree count and its actual production seed together. The 500-tree forests are
complete saved models, not prefixes recreated from the new fits. Their original
fits and evidence remain untouched.

| Case | mtry / node size / sample fraction | Seed: 500 trees | Seed: 256 trees |
| --- | --- | ---: | ---: |
| Year | 30 / 20 / 0.8 | 1385187677 | 253049781 |
| Covertype | 18 / 5 / 0.8 | 1221041465 | 671430028 |

Year has 90 effective inputs. Covertype has 54 raw inputs and 53 effective
inputs because the original development recipe removed the constant `soil_15`
column. No additional feature removal occurs here. Covertype's 50,000-row
development pool is below the four-million input-work threshold; its 256-tree
fit is an explicit diagnostic, not its ordinary automatic default.

## Predictive quality and saved prediction checks

| Measure | 500 trees | 256 trees | Change, 256 minus 500 |
| --- | ---: | ---: | ---: |
| Year RMSE | 9.499291060 | 9.514588402 | +0.015297343 (+0.161%) |
| Year MAE | 6.838313755 | 6.850043137 | +0.011729382 |
| Year R-squared | 0.260337666 | 0.257953492 | -0.002384173 |
| Covertype log loss | 0.332314379 | 0.332324903 | +0.000010524 (+0.00317%) |
| Covertype Brier score | 0.183476947 | 0.183613085 | +0.000136138 |
| Covertype accuracy | 0.88630 | 0.88615 | -0.00015 |

Near-equal aggregate Covertype loss does not establish class-level equivalence.
Correct counts over the same assessment rows are:

| Class | Assessment rows | Correct: 500 | Correct: 256 | Recall change, percentage points |
| --- | ---: | ---: | ---: | ---: |
| spruce fir | 7,292 | 6,301 | 6,320 | +0.261 |
| lodgepole pine | 9,752 | 9,048 | 9,031 | -0.174 |
| ponderosa pine | 1,231 | 1,130 | 1,129 | -0.081 |
| cottonwood / willow | 94 | 76 | 73 | -3.191 |
| aspen | 327 | 133 | 140 | +2.141 |
| Douglas fir | 598 | 426 | 422 | -0.669 |
| krummholz | 706 | 612 | 608 | -0.567 |

Neither model assigns zero probability to the true class on these 20,000 rows;
unbounded log loss is finite. Full confusion matrices and per-class probability
zero counts are retained in the machine-readable evidence. The known earlier
prefix warning about fewer correct cottonwood predictions remains relevant.

All four saved forests reproduce the original saved predictions in fresh R
processes over all 20,000 rows. Wrapper predictions and direct native ranger
predictions also match exactly: maximum absolute difference is zero in every
comparison. Native tree counts, sample counts, fitting seeds, requested/effective
parameters, and OOB evidence are checked and retained.

## Resource observations

| Measure | Year: 500 | Year: 256 | Covertype: 500 | Covertype: 256 |
| --- | ---: | ---: | ---: | ---: |
| Fit seconds | 193.232 | 211.273 | 38.879 | 33.211 |
| Saved model bytes | 135,540,786 | 69,643,487 | 281,003,985 | 145,059,446 |
| Native nodes | 4,222,234 | 2,163,286 | 4,090,552 | 2,091,658 |
| Median prediction seconds, 5,000 rows | 0.742 | 0.415 | 0.883 | 0.474 |

The fits occurred at different times and under different host load. In
particular, the observed Year 256-tree fit took longer than the earlier
500-tree fit. These measurements do not demonstrate a fitting speedup. The
smaller saved objects and faster sampled predictions are observations from
these particular models; full-workflow savings remain to be measured.
Saved sizes refer to standalone forest objects serialized as uncompressed,
version-3 RDS files in both cases, not complete public-workflow results.

One independently supervised, four-thread full native reference could share
the host with one diagnostic stage; integration checks also ran during part
of the comparison. Each diagnostic process has its own
1,200-second and 24-GiB limit. The process records preserve overlap, elapsed time,
peak RSS, commands, and status. These separate stage limits are not a successful
single-call time budget.

## Explanation fidelity and cost

All importance computations use the same 5,000 uniformly sampled assessment
rows (sample seed 80711), with five screening repeats over every effective
input. Screening permutation seed is 80711. Both forests receive twenty
detailed repeats on the union of both ordinary report feature unions, plus
up to two predeclared borderline inputs per forest. Detailed permutation seed
is 80712. Full repeat arrays and hashes are retained.

Each ordinary union combines the leading eight inputs from the unchanged
boosted model, the corresponding forest, and the simple baseline. The common
detailed comparison keeps all features selected by either model set. A public
portfolio may additionally retain a regularized model, which this recovered
paired three-model diagnostic does not include; its complete union and report
cost remain part of the public-workflow gate.

Year screening top-eight overlap is 7/8 and the top-sixteen feature sets are
identical. The forest's eighth screening input changes from `timbre_23` to
`timbre_22`. The boosted model retains `timbre_23` in both ordinary report unions;
the new union adds `timbre_22`, growing from 13 to 14 inputs. All fourteen union
inputs have positive importance and positive shuffle-interval lower bounds in
both twenty-repeat detailed passes.

The detailed comparison also includes four borderline inputs, for 18 inputs
in total. Their instability remains visible. For example, `timbre_65` changes
from +0.002375 (shuffle interval [0.000876, 0.003874]) to -0.000107
([-0.001735, 0.001520]); `timbre_84` changes from negative to positive but its
256-tree interval crosses zero. Shuffle intervals condition on these rows and
fixed models. They exclude fitting-seed, row-sampling, and training-sample
uncertainty and cannot establish predictive equivalence.

Year all-input screening takes 265.158 seconds with 500 trees and 188.776
seconds with 256 trees. The common 18-input detailed pass takes 271.560 and
155.371 seconds respectively. Combined observed forest importance time falls
from 536.718 to 344.147 seconds (35.9%). This includes the additional diagnostic
borderline features and is not a complete default-report timing.
These figures time the importance function; stage startup, saved-model loading,
and explainer construction are outside that timer. Complete process elapsed
times are recorded separately.

The ordinary Year forest permutation counts are `5*90 + 20*13 = 710` and
`5*90 + 20*14 = 730`, excluding baseline predictions. At 5,000 rows, their
row-times-tree work is 1.775 billion and 0.9344 billion, respectively (47.4%
less), despite the larger feature union. Tree traversal depth, R/native
overhead, and other report work prevent interpreting that ratio as wall-time
savings.

Covertype screening top-eight and top-sixteen sets are identical between the
two forests. Both ordinary unions retain the same eleven inputs. The common
detailed set adds `soil_21`, `soil_25`, and `soil_26`, for fourteen inputs in
total. All eleven report inputs remain positive with positive shuffle-interval
lower bounds, but effect magnitudes change: elevation increases from 1.12545
to 1.18903 with non-overlapping shuffle intervals, while `wilderness_1` and
`wilderness_4` decrease beyond their shuffle intervals. Among borderline inputs,
`soil_26` changes from +0.000033814 to -0.000005728, with both shuffle intervals
excluding zero. Stable leading-feature sets therefore do not mean identical
explanations.

Covertype all-input screening takes 410.468 versus 188.663 seconds, and the
common fourteen-input detailed pass takes 394.820 versus 204.042 seconds.
Combined forest importance time is 805.288 versus 392.705 seconds, a 51.2%
observed reduction under the shared-host conditions. Each ordinary forest
report uses `5*53 + 20*11 = 485` permutation predictions, excluding baseline
predictions. Its row-times-tree work declines from 1.2125 billion to 0.6208
billion (48.8% less). Neither figure includes complete report generation.

## Reproduction and evidence

The frozen run is
`~/.cache/autoxplain-tabular-0.8.0/forest-final256-development-v1`.
Its source manifest SHA-256 is
`3d392b19dabb437d5cf0ece657d2cc5e0aec683198f013d9ec06defb820cdd30`.
The source models are preserved under
`runs/candidate-paired-4t-v4-recovery-20260913/{yearprediction,covertype}/package`.
Every stage verifies the frozen source and protocol before running; source
models, development partition files, saved predictions, and process records
have recorded hashes.

Use [final-tree-probe.py](final-tree-probe.py) and its
[R stage implementation](final-tree-probe.R) with a new `--run` directory to
repeat the protocol. Existing stage directories cannot be overwritten. Run
`prepare`, `fit`, and `replay` for each case; then run `screen` separately for
`main_model`, `simple_baseline`, `forest500`, and `forest256`, followed by
`detail` for each forest and `summarize`. Alternate forest order between the
cases as recorded in this run; this is still not an isolated timing experiment.

After all stages finish, [collect-final-tree.py](collect-final-tree.py) verifies
completion, frozen/source hashes, native structure, prediction parity, seed
and parameter bindings, repeat counts, and reference-row hashes. It writes
the compact JSON and screening/detailed comparison CSV files alongside this
note. The independent [saved-array checker](check-final-tree-importance.R)
read all twelve importance arrays, verified their complete repeat scores and
reference-row identities against the recorded hashes, independently recomputed
means, standard errors and shuffle intervals, and wrote paired repeat
differences. It uses no model fitting, prediction, or evaluation-target reads.
All checks passed at a tolerance of `1e-12`.

The compact evidence is [final-tree-results.json](final-tree-results.json) and
[final-tree-importance-check.json](final-tree-importance-check.json). Detailed
effect comparisons are in the [Year CSV](final-tree-yearprediction-detail-comparison.csv)
and [Covertype CSV](final-tree-covertype-detail-comparison.csv). Full screening
comparisons and paired repeat-difference CSVs are alongside them; complete
models, predictions, repeat arrays and process logs remain in the frozen run.

These development results support keeping the 256-tree policy as a candidate
for full-scale evaluation. They show reductions in saved-model size and
importance work alongside small aggregate loss changes, rare-class changes,
and non-identical explanations. The full-data quality, full public report,
and cold saved-result replay acceptance gates remain required.
