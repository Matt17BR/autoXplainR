# Public report integration review

This review found one additional defect after the initial rendering matrix
passed. Supplied multiclass prediction matrices could carry custom row names
into their JSON records as an extra `_row` field. The probability records then
contained more than the declared classes. The fix gives the nested data frame
sequential row names. It preserves actual class columns, including a class
literally named `_row`.

Three small tests now run in the normal package suite. They call
`render_model_report()` and decode its actual HTML, then check:

- Multiclass probabilities and class order against native predictions on the
  original evaluation rows. The fixture uses custom row names, deliberately
  reordered prediction columns and a real `_row` class.
- Original source positions after `drop_rows`, including observed values,
  predictions and residuals computed from the supplied evaluation frame.
  Removed observations remain in the data export but have no prediction record.
- An exported sample whose only evaluation observation was removed during
  preprocessing. The report must expose that source observation without
  inventing a prediction or crashing on an empty prediction table.

Run these from the repository root:

```sh
Rscript -e 'devtools::test(filter="report-public-payload", stop_on_failure=TRUE)'
```

All 40 assertions passed after the fix, with no warnings or skipped tests. Before
the fix, the first fixture failed its class-key and numeric-matrix comparisons.
The tests do not derive expected values from report-preparation helpers.

The larger exploratory matrix is retained here, outside routine unit testing.
It covers three outcome types in summary, none and full rows modes, followed by
retained explanations, supplied audit/effects, all-row curve support, and an
empty retained evaluation export. Run both scripts in order:

```sh
Rscript validation/scalability/independent-review/public-render/public-render.R
Rscript validation/scalability/independent-review/public-render/attached-render.R
```

Outputs default to `~/.cache/autoxplain-public-render-review`. Set
`AXR_PUBLIC_RENDER_OUTPUT` to use another directory. The scripts share that
directory because the second reuses the first script's saved fitted models.
All 24 renders passed in the original review. This matrix alone missed the
custom-row-name case, which is why the maintained test uses a different supplied
model fixture. A trace observes the final report argument without replacing it.

`verdict.json` records the results, defect, limits and SHA256 values of the source
and test files after verification. These identify the checked working files;
they are not an immutable archive captured before the runs. The review covers
public R rendering and decoded HTML, not browser interaction, timing, or final
archive acceptance. The separate model-disagreement defect belongs to the
report owner's review.

The installed/cold-process acceptance scripts also now decode the native
multiclass report's complete probability matrix and source identities against
predictions saved from the original inputs. That added smoke passed against the
preserved preliminary installed snapshot. The complete release gate must still
run against the final checked archive.
