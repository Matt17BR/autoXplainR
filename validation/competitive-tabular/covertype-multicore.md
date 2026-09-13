# Covertype matched-thread development references

The coordinating agent authorized four-thread native Covertype references on
12 September 2026 before their processes started. This written record was
added immediately after launch. It extends the earlier Year-only development
resource cohort and leaves its recorded attempts intact.

Run `native-development-4t-v1/covertype` for both `ranger` and `xgboost`. Keep the
same data partitions, two fixed native settings, calibration split, seeds,
complete 50,000-row final fit, 1,200-second ceiling and 24-GiB address-space
limit as the one-thread native runs. Only the explicit native thread count
changes. These provide context for prospective four-thread package candidates.

Preserve both thread cohorts, including any failures. Compare predictions and
losses directly rather than assuming identical results across thread counts.
Wall times still reflect concurrent work on the shared host. The one-thread
Covertype outcomes were already known when these runs were authorized, so this
is a development resource comparison, not new acceptance evidence.
