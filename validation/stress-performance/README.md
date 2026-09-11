# Report generation with wide data

The 0.6.1 report took about two minutes to generate when row export included
1,200 observations and 500 predictors. Profiling identified repeated work in
R before the browser opened: building a data frame for each occupied bin,
classifying each column again for each exported row, and serializing every
numeric cell separately.

The changes build each conditional table once, convert only the selected rows
of each column once, and temporarily use nested data frames when serializing
row records. The regular R result retains its original list of records. The
browser receives the same JSON objects, field names, values and missing-value
markers. Existing HTML escaping still handles every exported value and name.

## Observed generation times

These are complete `render_model_report()` calls using the same saved fitted
result and audit on the same host. Fitting and explanation computation are
excluded. The report audit intentionally covers three specified inputs; this
experiment measures report scaling rather than the quality of a 500-feature
importance ranking.

| Predictors | Export | Original seconds | Updated seconds |
| --- | --- | ---: | ---: |
| 100 | Summary | 19.803 | 8.754 |
| 100 | Rows | 39.331 | 9.526 |
| 500 | Summary | 26.748 | 11.395 |
| 500 | Rows | 121.136 | 20.724 |

The complete measurements and browser follow-up are in
[the report stress test](../stress-reports/README.md). These observations come
from one machine also running other checks. They establish a substantial
improvement for these inputs, not a latency guarantee for other data or hardware.

## What the isolated measurements show

- Building the 100-predictor aggregate export fell from a median 17.604 seconds
  to 3.582 seconds over three repetitions. Both returned the identical R object.
- In the 500-predictor row case, serialization alone took 67.899 seconds.
  Transposing the temporary serialization copy took 3.295 seconds and encoding
  it took 6.632 seconds. Both JSON strings were byte-identical.
- Caching selected column conversions reduced the remaining row preparation
  from 29.296 to 5.694 seconds. The complete R export remained identical.
- The final escaped data payload matched the original report byte for byte:
  56,405,308 bytes, SHA-256
  `5d572d45882284a7f9d0f9467b7374cf20d1c5728e5819046413729ba33703cb`.
  Independent JSON decoding also produced identical complete payloads.

The full 500-predictor row report is still approximately 60 MB. These changes
reduce computation without claiming a storage improvement. The separate
100-predictor `Rprofmem` record changed only from 506,685,472 to 503,786,320
recorded allocation bytes. Allocation totals and R object sizes do not measure
peak process memory; the sampled `Rprof` memory column is not an RSS measurement.

The focused tests cover hand-counted conditional statistics, selected source
positions, excluded rows, nonfinite values, date and time values, raw values
with different types, empty and single-row exports, numeric precision, and
hostile text in values and column names. The latter still passes through the
same JSON and HTML escaping functions. These checks passed along with the data
capture integration tests: 189 assertions in total.

The final compatibility comparison also checks Date and POSIX infinities,
attributes on scalar values, unsupported columns, and missing processed
positions against the installed baseline. Date and time infinities keep their
original internal numeric value; serialization still emits JSON null alongside
the separate nonfinite marker. See [the compatibility record](type-compatibility.json).

## Reproduce the comparison

Install the published 0.6.1 source package into a separate R library. Generate
the wide fixture from the repository root if the saved fit is unavailable:

```sh
AXR_STRESS_WIDTHS=500 Rscript validation/stress-reports/generate-wide.R
```

Then compare that installed version with this checkout. Use an output directory
outside the repository:

```sh
Rscript validation/stress-performance/compare.R \
  /path/to/installed-0.6.1-library \
  ~/.cache/autoxplain-stress-0.6.2/reports/wide-500/result.rds \
  /path/to/performance-results rows 1
```

The final argument is the repetition count; use `summary` instead of `rows` to
measure aggregate export. The script loads the installed baseline and sources
only the preparation and serialization files into a separate environment. It
never refits the saved model. It fails if any R export value, escaped JSON byte,
or decoded payload changes, and records source and fixture fingerprints beside
the timings. The comparison covers data preparation and embedded JSON; use
`validation/stress-reports/profile-render.R` for complete report generation.

Run the broader type compatibility comparison separately:

```sh
Rscript validation/stress-performance/compare-types.R \
  /path/to/installed-0.6.1-library /path/to/type-compatibility.json
```

[The isolated measurements](isolated-measurements.json) record the successive
optimizations. They are separate runs, so their component times should not be
added together.

The [final paired comparison](comparison-500.json), after the type compatibility
check, measured preparation at 37.206 versus 6.272 seconds and serialization
including HTML escaping at 77.578 versus 12.219 seconds. This fresh process
compared installed 0.6.1 with the final source fingerprints recorded in the
file. Complete R exports, escaped JSON bytes and decoded payloads were all
identical.
