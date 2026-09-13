# Full-training native XGBoost scheduling record

Recorded on 13 September 2026 before launching these full-training fits.
This records execution of the existing native XGBoost protocol; it changes
neither the reference algorithm nor the acceptance criteria. No locked
evaluation outcomes have been opened.

The new `native-xgboost-full-fit-4t-v1` cohort runs Bank, YearPredictionMSD and
Covertype sequentially with `--phase acceptance --stage fit-only --threads 4`.
Each process retains the original 7200-second combined fit/scoring budget and
24-GiB address-space limit. The fixed native-forest mapping is not used for
these XGBoost fits.

Each case retains both original calibration settings: histogram boosting at
depths 6 and 10, eta 0.05, minimum child weight 1, lambda 1, alpha 0, row and
column subsampling 0.8, max-bin 256, seed 80711, at most 1500 rounds, and
50-round early stopping. The already frozen training-calibration partition
selects the setting and round count by RMSE or log loss. The selected setting
and round count are then refitted on every declared training row: 32951 Bank,
463715 Year and 464810 Covertype rows. Calibration predictions and round logs
remain available as training-only evidence.

The coordinator permits at most two concurrent four-thread native jobs. The
already running full Year forest uses one slot. The XGBoost sequence waits
until the entire forest-diagnostic pipeline has released the other slot;
gaps between diagnostic R stages do not count as completion. Each XGBoost
launch independently checks the current native process inventory and available
memory. Overlapping work, physical hardware, cgroup allocation and thread
controls are recorded. Times are shared-host observations, not speedup claims.

Each successful saved reference receives fresh-session structural and hash
checks, including its actual boosted round count, calibration-selected setting
and round evidence. XGBoost does not retain a native training-row count in the
saved booster: complete-row fitting is supported by the frozen runner's native
call, complete input construction, trace and metadata, and is labeled as such.
These checks do not open evaluation files or establish held-out prediction
replay. Later scoring and complete evaluation replay require the final frozen
candidate/reference manifest and the remaining combined budget, as specified
in [native-staging.md](native-staging.md).
