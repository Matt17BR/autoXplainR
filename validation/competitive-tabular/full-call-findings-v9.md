# First full default calls for the 0.8 candidate

These are results from the unchanged candidate-v9 freeze. Later repairs do not
replace these attempts or turn them into results from a newer package snapshot.

## YearPredictionMSD

The public `autoxplain()` call completed on 463,715 training rows and 51,630
test rows, with 90 predictors. All four retained models passed independent
numerical verification and reproduced their complete test predictions in a
fresh R session. The quality thresholds declared before scoring passed.

The public call took 5,566.898 seconds. The complete supervised process,
including saving and additional scoring, took 5,582.139 seconds and peaked at
7,519,532 KiB RSS. These are shared-host measurements with four native threads.
Explanation preparation took 364.702 seconds and HTML rendering took 56.699
seconds. The default summary report contains 5,865,734 bytes.

The first direct browser inspection found the selected model, parameter
summaries, separate screening/CV results, thread and tree budgets, and per-fold
boosting rounds without consulting the detailed numerical audit. It also
identified two presentation problems: large durations appear as unformatted
millisecond counts, and the default cost comparison uses R object size despite
excluding native engine allocations. The size caveat exists but is collapsed.
These initial findings are retained below alongside their later follow-ups.

Original report SHA256:
`20b461f2ad8d058f1723123969e6f51bc157d04ae8ca9994f8970b1d8cfc8d23`.
Numerical verification SHA256:
`9351cfea6e7f0c520a60d3413a41da02b1b94fcbfab6f88a38529f18c1603639`.
Process record SHA256:
`160622de885950354bffb2067167bda578e6a20066fbca81caf732736bbb415f`.

## Covertype

The next call failed before screening any candidate. On 464,810 training rows
and seven classes, screening emitted `NAs produced by integer overflow`, then
stopped with `missing value where TRUE/FALSE needed`. The R attempt took 4.699
seconds; the supervised process took 5.870 seconds and peaked at 927,172 KiB RSS.
The controller stopped, so the following Bank call was not attempted.

The failure was reproduced using synthetic class labels, without fitting any
model. Class quotas multiplied integer sample size by integer class counts
before division. The product can exceed R's integer range even though the
desired sample is only 20,000 rows. Inspection also found this arithmetic in
report-row allocation: a 5,000-row export can overflow with large source splits.
The quota arithmetic repairs passed 165 focused assertions, including large
class counts, rare classes, exact quotas, original-fold separation, ordinary
sample identity and caller RNG preservation. This has not yet established a
successful full Covertype call. The original failed attempt remains in the
evidence and has not been restarted or overwritten.

Failed process SHA256:
`16ed7b30f6c95fbd3bc2e3bb7f01ef97f6a83889911caa1072aaf8539d4bd4fa`.
Failed summary SHA256:
`fe715c759da7dcfa267ade163f2f846aa4991cd1663d4d5a358d2f21455d6496`.

## Follow-up scope

Fix the quota arithmetic, verify ordinary-case sampling and RNG parity, and
exercise genuinely large synthetic class/split counts. Restore informative
native forest progress without altering statistical settings. Make cost
comparisons readable and honest while retaining native baseline diagnostics.
Then freeze the revised candidate and explicitly record any later full-data
attempts as follow-ups. Complete report, package and release checks on the
resulting implementation before publishing.

The progress repair passed a combined 678-assertion group. Separate native
forest fits with 20,000 synthetic rows and 2,500 trees produced identical trees,
OOB predictions and errors in information and quiet modes. The information run
printed a native progress update 30.34 seconds before fitting finished; the
quiet run printed none. These are console checks, not public-call benchmarks.

The unchanged Year HTML passed all 12 independent offline browser task groups:
all-model scores/settings, screening and CV, explanation scope, aggregate
errors, export help, mobile navigation and PDF. Its first observed offline load
took 0.384 seconds, with no browser errors or external requests. Direct product
inspection agreed with the numerical audit: primary RMSE 8.890530 and retained
forest RMSE 9.227795, against native references 8.917096 and 9.232230. Every
retained model uses all 463,715 training rows and replays all test predictions
exactly. The direct walkthrough also found redundant no-op preprocessing
tables and an unclear input-policy presentation; revised HTML must be reviewed
separately.

The separate saved-v9/v12 rows derivative later passed all 206,520 prediction
replays, 910,000 raw/processed cell checks and 2,004 exported model prediction
cases. It contains exactly 5,000 proportional sampled records and occupies
9,502,169 bytes. The original failed checker allocation assertion is preserved;
a versioned double-precision allocation check passed on the unchanged report.
Direct browser inspection followed test_data row 51495 from the booster error
table into its source values, then compared the forest's reordered error table.
This establishes saved-model rendering compatibility, not a new full public call.
