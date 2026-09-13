# Native boosting callback cost and round-count checks

Early stopping must score probabilities for classification and the exact chosen
selection metric. The package's native XGBoost callback therefore predicts on
the inner validation DMatrix after each round. This probe checks a specific
performance concern: whether predicting all accumulated trees on every round
causes quadratic work, or whether XGBoost's prediction cache is effective.

The synthetic problem has 1,000 fitting rows, 20,000 inner validation rows and
10 numeric inputs. The target is `V1^2 + sin(V2)` plus Gaussian noise with standard
deviation 0.4. Data seed is 258; native fitting seed is 238. Both paths use depth
4, learning rate 0.03, one native thread, and the same remaining learner settings.
The callback and native RMSE paths each run 200, 800 and 2,000 rounds twice.
Execution order reverses for the second repetition after a warm-up. Patience
exceeds each cap so every requested round is scored.

All callback scores must match native RMSE within `1e-7`. The first 200 callback
scores must also be byte-identical when the maximum changes from 200 to 2,000
rounds with the same seed. Tests elsewhere independently compare selected
iterations and classification/Brier/AUC curves against sliced native predictions.
This probe checks the regression prediction-cache path, not all statistical
contracts of early stopping.

In the first measured cohort, callback time grew from 0.32 to 0.47 seconds at
200 rounds to 2.07 to 2.18 seconds at 2,000 rounds. That supports useful caching
on this path and workload. It does not establish a universal runtime bound.
The native reference constructs standard DMatrices while the helper can use
quantile matrices, so the timing difference is **not** a pure callback-overhead
estimate. The small measurements also share a host with other development work.

Run from the repository root with the package's development dependencies:

```sh
Rscript validation/competitive-tabular/boosting-callback/measure.R /tmp/boosting-callback
```

The script refuses an existing timing output. It saves full curves in a local
RDS file, compact checks and timings in CSV, and source hashes plus native
versions in JSON. Committed results include the original timing cohort and a
portable-script rerun, identified separately. Neither uses external datasets,
evaluation labels or acceptance data.
