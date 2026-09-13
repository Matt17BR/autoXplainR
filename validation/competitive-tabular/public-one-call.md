# Test the actual default tabular workflow

Declared on 12 September 2026, after the candidate-v3 development comparison
and before any `public-tabular` run or locked acceptance scoring.

The earlier `paired` and `tabular` comparisons deliberately fix the folds,
seed and native threads, and disable explanations. They isolate fitting
behavior but cannot establish that a new user can produce a useful report
with one ordinary call.

The separate `public-tabular` cohort calls:

```r
result <- autoxplain(
  training_data, "y",
  portfolio = "tabular",
  test_data = untouched_test_data,
  evaluation_role = "test",
  report = "report.html"
)
```

No tuning control, custom grid, setting budget, thread count, fold assignment,
fold count, seed, task, explanation control or report-data control is supplied.
This exercises the package's 18-setting tabular portfolio, automatic threads,
generated five folds, seed 123, automatic task detection, default explanations
and default shareable report. Record the resolved values from the result.
The prepared test data retain their original locked rows and outcomes.

The supervisor has the same four-thread environment and 24-GiB address-space
ceiling used for the practical native references. It does not pass a thread
override to `autoxplain`. Record both this ceiling and the package's actual
automatic choice, so an unexpected lower choice cannot be described as a
matched four-thread run.

The full acceptance process still has 7,200 seconds total. That includes model
search, final refits, default explanations, HTML generation, prediction for
every retained model and saving the complete result. There is no additional
report-time allowance and no smaller explanation sample introduced by the
benchmark. Development harness checks retain the 1,200-second limit. A timeout
is a failed workflow even if some fits or report sections already finished.

The package's default report may sample rows for explanations and display.
Record those counts and compare them with the complete training and evaluation
counts. Independently score every evaluation row, check every retained saved
model in a fresh R session, and perform the tasks in
[report-acceptance-tasks.md](report-acceptance-tasks.md). The existing predictive
quality and separate forest-family gates still apply. This cohort does not
replace the controlled paired comparisons or reuse their scores as its own.
