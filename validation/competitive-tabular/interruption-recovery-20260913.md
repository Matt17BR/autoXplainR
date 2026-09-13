# Recovery of interrupted development processes

Recorded on 13 September 2026 before restarting any missing process. No locked
acceptance outcomes were opened. The evaluation partitions, algorithms and
resource ceilings are unchanged.

Independent inspection found the original supervised PIDs absent from `/proc`
and no native R process, `time` wrapper or benchmark `run.py` supervisor alive.
The original records still said `running`. None had a completed summary,
saved final model or complete evaluation predictions. This establishes an
interruption; it does not establish a timeout, stop time, exit code or cause.

| Original cohort and case | Supervised PID | Last observed elapsed seconds | Observed RSS high-water mark, KiB |
| --- | ---: | ---: | ---: |
| candidate-paired-4t-v4, Year | 3425391 | 700.686 | 1184796 |
| candidate-paired-4t-v4, Covertype | 3434679 | 560.526 | 1070124 |
| native-full-fit-4t-v1, Year ranger | 3358120 | 1831.604 | 11393296 |

`record-interruption.py` preserved every original top-level evidence file,
copied them byte for byte into `interruption-originals`, and wrote a separate
`interruption.json` with the inspection evidence and SHA-256 hashes. Original
process records and logs remain unchanged, including trailing NUL bytes in
the Year package and native-forest logs. `collect.py` verifies those hashes
before reporting the separate interrupted state and keeps actual duration
unknown. The sampled time and memory remain checkpoint evidence.

The full native Year forest completed its first training-calibration setting
and stopped during the second. It has no full-training final model and supplies
no forest-readiness or acceptance-quality result. Recovery does not launch a
replacement native full-training fit.

Only the missing Year and Covertype paired development comparisons repeat, in
the new `candidate-paired-4t-v4-recovery-20260913` cohort. Both use the original
run's frozen scripts and the candidate-v4 private installation, retain four
native threads, the 1,200-second process ceiling and 24-GiB address-space limit,
and train on the same 50,000 development rows with the same known development
evaluation rows. These runs overlap on the shared host; their wall times are
observations rather than isolated speed benchmarks.

Preflight verified every one of the 248 candidate source files and 45 installed
package files against the original snapshot, whose source inventory SHA-256 is
`c5ea5be2aeaf7cdfbfcda42d0be7fa72082a9e1f484ae75de6bbb5bd13cd98ab`.
It also verified both frozen script inventories and development partition
hashes. R 4.5.2 and recorded backend versions match the original runs: XGBoost
3.2.1.1, ranger 0.18.0, Matrix 1.7.5 and data.table 1.18.2.1. Current installed
dependency files receive SHA-256 inventories. Installed MD5 checks returned
unavailable for all 24 packages because no MD5 manifests were supplied.
Historic records contain backend versions but no
dependency-file inventories; byte identity of those historic dependencies
cannot be asserted. The full preflight remains under cache
`recovery/2026-09-13/restart-preflight.json`, bound by each repeat's
`recovery-provenance.json`.

The completed `public-default-development-v4/rare_interaction` evidence is
preserved and verified independently. Its public call resolved one automatic
native thread under the four-thread supervisor ceiling. Collection displays
the actual resolved count and keeps the supervisor ceiling in its JSON record.
This small controlled case establishes no full-data acceptance result.

Both repeated paired runs completed within their limits and all three retained
models reproduced all 20,000 development predictions exactly in fresh R
sessions. Independent held-out metrics and all 100,000 OOF records per case
(two complete configurations, five folds, 50,000 training rows) agree with the
recorded scores. Original native-fit parameter traces match the recovery
prefixes exactly. Candidate and dependency files remained unchanged from the
new preflight through verification.

| Case | Seconds | Peak RSS, KiB | Primary development loss | Forest development loss |
| --- | ---: | ---: | ---: | ---: |
| YearPredictionMSD | 495.091 | 1633992 | RMSE 9.155860 | RMSE 9.499291 |
| Covertype | 391.213 | 2065072 | Log loss 0.261613 | Log loss 0.332314 |

These times belong to the recovered host: Intel Core Ultra 9 185H, 16 physical
cores and 22 logical CPUs, affinity 0–21, about 61 GiB physical RAM, and no
visible CPU or memory quota in the active cgroup ancestry. Four native backend
threads appear throughout both adapter traces. The complete hardware/cgroup
snapshot is hash-linked to each run. Historic cohorts have no comparable host
allocation record; the timing difference establishes no speedup.

The Year workflow warns that development evaluation rows 4771 and 5121 exactly
match training values. Source-row inspection confirms different original rows:
134690 versus 134689, and 448780 versus 448774. Training/evaluation source-row
sets are disjoint. Equal feature vectors and targets therefore do not arise
from reusing the same source row, but artist independence cannot be established
without artist identifiers. Preserve this warning when interpreting the
development comparison; the original official acceptance boundary is unchanged.
