# Additional development resource cohorts

Declared on 12 September 2026 before running these cohorts.

The initial YearPredictionMSD package baseline spends several minutes in its
first 40,000-row forest fit, then enters an all-90-input, 500-tree fit whose
progress estimate exceeds 40 minutes. The one-thread, 1,200-second attempt is
retained under its original boundary. It is not replaced with a faster run.

A separate `native-development-4t-v1` Year random-forest reference uses four
native threads. Its dataset partitions, seed, 500-tree settings, training-only
calibration, two-setting selection and complete-training refit are identical
to the declared one-thread native reference. It retains the same 1,200-second
development ceiling and 24-GiB address-space limit. The one-thread native
attempt is also retained, including a timeout if it cannot finish.

This cohort establishes a practical multi-core reference. Its elapsed time
cannot establish an algorithmic speedup over a one-thread package run. Final
acceptance uses matched four-thread native and candidate runs, alongside
one-thread development comparisons where they complete. A candidate with
four threads must record that explicit resource control; published 0.7.0
does not expose it for these native learners.
