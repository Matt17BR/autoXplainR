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
  seed = NULL
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
  within each split. Unfiltered aggregate profiles use the full
  available data for each stage; browser row filters describe only the
  exported sample and leave model scores unchanged.

- seed:

  Optional sampling seed. `NULL` uses the analysis seed, or 2026.

## Value

An `autoxplain_report_data_control` object.
