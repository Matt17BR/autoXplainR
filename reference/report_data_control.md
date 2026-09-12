# Control the data included in a standalone report

The default includes aggregate distributions and relationships. Row
export is explicit: anyone receiving the HTML receives every embedded
record, even when browser filters hide it. Aggregate output is not
anonymization; category labels, small groups and fitted model details
can still disclose information.

## Usage

``` r
report_data_control(
  mode = c("summary", "rows", "none"),
  columns = NULL,
  context_columns = character(),
  max_rows = 5000L,
  seed = NULL,
  max_pair_rows = 10000L
)
```

## Arguments

- mode:

  `"summary"`, `"rows"`, or `"none"`. Strings can also be passed
  directly as `report_data` when fitting or rendering a report.

- columns:

  Predictor names to include. `NULL` includes all model inputs. The
  target is included automatically. These columns must be model inputs;
  use `context_columns` to explicitly include other retained columns.
  This selects data-explorer values; it does not redact feature names,
  fitted-model details or explanations elsewhere in the report.

- context_columns:

  Additional retained raw columns to export explicitly, such as a site
  or time column excluded from fitting. Empty by default.

- max_rows:

  Maximum exported records across training and evaluation. Sampling
  allocates rows proportionally between splits and samples uniformly
  within each split. Browser row filters describe only this exported
  sample and leave model scores unchanged.

- seed:

  Optional sampling seed. `NULL` uses the analysis seed, or 2026.

- max_pair_rows:

  Maximum rows per partition used for relationship plots, conditional
  summaries and pairwise associations. The default is 10,000; `NULL`
  uses all rows. One uniform sample is shared by all pairs. Raw and
  processed samples retain the same source rows where possible. Counts
  and associations describe that sample, with its size shown in the
  report. Rare groups can be missed; increase this limit or use `NULL`
  to inspect them. Unfiltered individual-column distributions and
  missing counts use all available rows. Filtered charts and their
  counts describe only matching exported records; sidebar counts keep
  the full population. Model scores always use the complete evaluation
  partition and are unchanged by filters. This limit is independent of
  `max_rows`.

## Value

An `autoxplain_report_data_control` object.
