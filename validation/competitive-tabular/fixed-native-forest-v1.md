# Fixed 500-tree native full-training reference

Declared on 13 September 2026 after the recorded development calibration
comparisons and interrupted full-Year native attempt, before any fit under
this policy or any locked acceptance outcomes were opened. The original
attempts and protocol remain recorded as their own cohorts.

The separate `fixed-native-forest-v1` reference fits one 500-tree ranger forest
on every row of each complete training pool. Its settings are the winners of
the already recorded training-only native calibration comparisons. It does
not rerun the two candidate configurations on a new full-training calibration
split. The selection rule is minimum training-calibration RMSE for regression
or log loss for classification; held-out development metrics do not select
these settings. The exact source process and summary hashes, both competing
calibration losses and chosen parameters are recorded in
[fixed-native-forest-v1.json](fixed-native-forest-v1.json).

| Case | Prior development cohort | Prior native threads | Chosen mtry | Minimum node size | Trees | Full training rows |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| YearPredictionMSD | native-development-4t-v1 | 4 | 30 | 5 | 500 | 463715 |
| Covertype | native-development-4t-v1 | 4 | 18 | 5 | 500 | 464810 |
| Bank | native-development-v2 | 1 | 4 | 5 | 500 | 32951 |

Every new reference uses four native threads, seed 80711, sample fraction 0.8,
the variance split rule for regression and Gini for classification. All feature
and factor handling stays with the native harness. The one-thread Bank source
provides a prior parameter choice; it is not described as a completed matched
four-thread full-training comparison.

This changes the reference workflow and its cost: it removes two new
500-tree full-pool calibration fits and retains one full-pool 500-tree fit.
It is a fixed, modest native comparison selected before locked evaluation,
not an exhaustive tuning oracle or a claim that calibration scales unchanged
from development. A successful fit satisfies the separately declared
full-row native-forest completion requirement under this amended protocol.
It cannot turn the original interrupted two-setting workflow into a success.
There is no equal-work or algorithmic-speedup claim.

Use a separate cohort and fit-only stage. The runner verifies the source
process/summary hashes, successful development status, calibration choice,
partition-manifest identity and exact fixed parameters before launch. The
saved model and summary retain this source lineage. The forest must contain
exactly 500 trees and the complete declared number of training rows.

The combined fit/scoring ceiling remains 7200 seconds and the per-process
address-space limit remains 24 GiB. All later scoring attempts for a saved fit
consume the same remaining budget. Fit-only reads only the training partition;
it must not open either locked evaluation file. Native fits run sequentially
after the candidate-v4 development recovery and its verification finish, with
actual current CPU, memory, cgroup and backend thread controls recorded.

Scoring still requires the coordinator's final candidate freeze and the exact
saved fit's process, summary and model hashes. Include this amendment, its JSON
mapping and the final candidate policy amendment in the freeze. Keep every
interruption, failure and timeout visible. A resource failure remains a failed
reference and cannot pass forest readiness.

For each case, the forest-quality reference is the lowest held-out primary
loss among **all successfully completed, predeclared 500-tree native forest
cohorts** on the same full training/evaluation partitions and four threads.
This includes the original two-setting workflow if it later completes and
this fixed reference; an inferior completed reference cannot replace a better
one. The selected-model gate likewise uses the best completed predeclared
native reference across families. The previously declared loss tolerances,
complete-prediction replay, default report workflow and full-data row gates
remain unchanged. Selection of the best reference happens only after the
candidate and reference set are frozen; none of those scores may guide a
candidate change while retaining a claim of untouched acceptance.
