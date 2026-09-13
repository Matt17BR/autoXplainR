# Full-training Bank native references with one thread

Declared on 13 September 2026, before either fit in the separate
`native-bank-original-full-fit-1t-v1` cohort. No locked evaluation outcomes
have been opened to prepare this cohort.

The public Bank workflow may resolve to one native thread: the declared
32,951 training rows and 19 inputs give 626,069 row-input combinations.
Matching is established from the eventual public workflow's recorded actual
thread count. The existing four-thread references remain separate evidence;
their timing or quality cannot be labeled a matched one-thread comparison.

This cohort retains the original two-setting training-calibration and
full-row refit protocol in [README.md](README.md). The frozen training-only
calibration rows choose between ranger forests with mtry 4 and 6, 500 trees,
minimum node size 5, sample fraction 0.8 and gini splits. The selected setting
is refitted on all 32,951 training rows with seed 80711, probability output
and ordered treatment of unordered factors. The separately declared fixed
forest mapping is not supplied to these fits.

XGBoost retains histogram trees at depths 6 and 10, eta 0.05, minimum child
weight 1, lambda 1, alpha 0, row and column subsampling 0.8, max-bin 256,
binary logistic objective and log loss, seed 80711, at most 1,500 rounds and
50-round early stopping. Training-calibration log loss chooses the setting
and its best round count; the chosen model is refitted on every training row.
Both backends explicitly use one thread for calibration, refit and future
scoring. Each family receives its own bounded process.

The [staging amendment](native-staging.md) originally described four-thread
full fits. This separate one-thread cohort uses the same fit/scoring
separation and original 7,200-second combined fit-plus-score budget, with a
24-GiB address-space limit. This is a declared thread-count cohort, not a
change to either model-selection protocol. Fitting uses `--phase acceptance
--stage fit-only --threads 1 --request paired`, without an unlock manifest
or `--native-reference-plan`. No evaluation file or outcome is read, and
completion establishes no held-out quality result.

The finite controller waits for successful completion and structural
verification of all three cases in the existing immutable
`native-xgboost-full-fit-4t-v1` queue. It does not use temporary gaps between
that queue's native jobs as completion. It independently checks the live R
process inventory, permits at most one other R process before each launch,
and requires at least 24 GiB of available host memory. Ranger then XGBoost
run sequentially; the existing Year forest may occupy the other native slot.
The controller stops on a prior-queue failure, its own failed fit or failed
verification, a stop request, or an eight-hour dependency/resource wait limit.
Every attempt and interruption remains recorded; existing runs are never
overwritten or silently retried.

Only after the harness hardening handoff does the controller snapshot its
scripts and their hashes. It verifies the snapshot and partition manifest
before launching each fit, captures physical hardware, cgroup allocation,
available memory and backend thread controls, and records overlapping work.
Times are shared-host observations. Fresh R sessions verify saved native
structure, complete ranger row/tree counts, XGBoost boosted rounds and
configuration, calibration provenance, and all fitting script/model hashes.
XGBoost training-row counts remain explicitly described as runner and
manifest metadata because the native booster does not retain them. These
checks never substitute for later complete held-out prediction replay.

The exact completed process, summary and model hashes must be bound in the
final acceptance manifest before any later scoring. The best completed native
reference at the actual matched thread count remains the comparison target;
the new cohort does not remove or reclassify any four-thread result or failure.
