# Development measurements

Generated from the frozen process records by `collect.py`. These are development
comparisons, not locked acceptance results or leaderboard claims. The full
training sets, report workflow and large saved-model usability still require
their separate acceptance checks. Raw predictions and models stay in the cache.

Times include the declared workflow and saving its result. Only the
`public-tabular` request includes default explanations and HTML generation;
the paired and tabular comparisons disable them. Jobs shared the host, so times are
observations rather than isolated speed benchmarks. Compare thread counts
explicitly. The public-default row shows its resolved automatic native threads;
its four-thread supervisor ceiling is recorded separately in the JSON evidence.
An unfinished, interrupted or timed-out run has no invented quality score.

| Case | Cohort | Learner | Threads | Status | Seconds | Selected-model loss | Forest loss | Cold replay |
| --- | --- | --- | ---: | --- | ---: | ---: | ---: | --- |
| bank | baseline-development-v1 | package | 1 | ok | 155.4 | 0.265358 | 0.272072 | all exact |
| covertype | baseline-development-v1 | package | 1 | timeout | 1200.0 | no result | no result | no saved result |
| friedman_noise | baseline-development-v1 | package | 1 | ok | 37.3 | 1.474922 | 2.151428 | all exact |
| rare_interaction | baseline-development-v1 | package | 1 | ok | 37.8 | 0.152992 | 0.167031 | all exact |
| yearprediction | baseline-development-v1 | package | 1 | timeout | 1200.0 | no result | no result | no saved result |
| bank | candidate-paired-1t-v1 | package | 1 | ok | 77.9 | 0.264579 | 0.272072 | all exact |
| covertype | candidate-paired-1t-v1 | package | 1 | timeout | 1200.0 | no result | no result | no saved result |
| friedman_noise | candidate-paired-1t-v1 | package | 1 | ok | 38.8 | 1.418555 | 2.151428 | all exact |
| rare_interaction | candidate-paired-1t-v1 | package | 1 | ok | 11.0 | 0.149186 | 0.186624 | all exact |
| yearprediction | candidate-paired-4t-v1 | package | 4 | timeout | 1200.0 | no result | no result | no saved result |
| covertype | candidate-paired-4t-v2 | package | 4 | failed | 615.6 | no result | no result | no saved result |
| yearprediction | candidate-paired-4t-v2 | package | 4 | timeout | 1200.0 | no result | no result | no saved result |
| covertype | candidate-paired-4t-v3 | package | 4 | ok | 837.5 | 0.287632 | 0.332314 | all exact |
| covertype | candidate-paired-4t-v4 | package | 4 | interrupted | unknown (last observed 560.5) | no result | no result | no saved result |
| yearprediction | candidate-paired-4t-v4 | package | 4 | interrupted | unknown (last observed 700.7) | no result | no result | no saved result |
| covertype | candidate-paired-4t-v4-recovery-20260913 | package | 4 | ok | 391.2 | 0.261613 | 0.332314 | all exact |
| yearprediction | candidate-paired-4t-v4-recovery-20260913 | package | 4 | ok | 495.1 | 9.155860 | 9.499291 | all exact |
| bank | candidate-tabular-4t-v1 | package | 4 | ok | 76.6 | 0.264579 | 0.272072 | all exact |
| covertype | native-development-4t-v1 | ranger | 4 | ok | 134.7 | 0.335199 | 0.335199 | all exact |
| covertype | native-development-4t-v1 | xgboost | 4 | ok | 224.4 | 0.256900 | n/a | all exact |
| yearprediction | native-development-4t-v1 | ranger | 4 | ok | 685.2 | 9.493825 | 9.493825 | all exact |
| friedman_noise | native-development-v1 | ranger | 1 | ok | 6.3 | 2.228817 | 2.228817 | all exact |
| bank | native-development-v2 | ranger | 1 | ok | 14.8 | 0.272769 | 0.272769 | all exact |
| bank | native-development-v2 | xgboost | 1 | ok | 2.7 | 0.267292 | n/a | all exact |
| covertype | native-development-v2 | ranger | 1 | ok | 328.5 | 0.335199 | 0.335199 | all exact |
| covertype | native-development-v2 | xgboost | 1 | ok | 211.0 | 0.256900 | n/a | all exact |
| friedman_noise | native-development-v2 | xgboost | 1 | ok | 7.5 | 1.647168 | n/a | all exact |
| rare_interaction | native-development-v2 | ranger | 1 | ok | 5.4 | 0.170820 | 0.170820 | all exact |
| rare_interaction | native-development-v2 | xgboost | 1 | ok | 2.6 | 0.165470 | n/a | all exact |
| yearprediction | native-development-v2 | ranger | 1 | timeout | 1200.0 | no result | no result | no saved result |
| yearprediction | native-development-v2 | xgboost | 1 | ok | 83.1 | 9.120693 | n/a | all exact |
| rare_interaction | public-default-development-v4 | package | 1 | ok | 97.8 | 0.166152 | 0.164932 | all exact |

Loss is RMSE for YearPredictionMSD and Friedman, and log loss for the
classification cases. Lower is better. Selected means training-resampling
selection for the package and training-calibration selection for native
references. See `development-results.json` for secondary metrics, class recall,
failures, exact controls, parameter settings, source hashes and memory records.

Covertype tests same-area row discrimination, not geographic transfer. Bank
excludes call duration but does not establish independent-customer or future-period
performance. The controlled cases were previously inspected. Native references
use two fixed settings and are bounded comparisons, not optimized oracles.

The initial native-v1 XGBoost smoke is retained in the JSON record but omitted
from this table: v2 corrected its R prediction interval to one-based BEGIN.
