# Reports with many columns or many exported rows

These checks cover a different question from model-fitting benchmarks: after a
fit succeeds, can someone open its report, inspect the original records, apply a
filter, and understand the scope of a chart?

The reference package is the installed, published **0.6.2** source archive. The
wide case reuses the same saved fitted result in both versions. The large-row
cases fit a linear model on 100 training rows, then score 200,000 or 1,000,000
evaluation rows. This isolates reporting costs; it is not evidence about fitting
a model on a million training rows.

## What changed

- Report generation builds column vectors directly. Identical raw and processed
  value columns share one encoded vector; their missing/non-finite flags remain
  separate. Mixed numeric and character values retain their types.
- Large blocks use the pinned, locally embedded fflate 0.8.3 decoder. Small
  blocks remain ordinary JSON. Decompression checks both the byte count and the
  zlib checksum. There is no network requirement. If decoding fails, the report
  shows an error and the existing distribution tables; model scores remain
  available.
- Columns decode when they are needed. Closed plots do no work. Paging and
  repeated sorts reuse their previous result. All explicitly exported rows stay
  available to filters, tables and source-record selection.
- Scatter plots display at most 1,500 evenly spaced records, state the displayed
  and complete-pair counts, and retain the full extent of the exported data.
  Source-record lookup reaches rows outside that visual sample.
- Category filters offer a searchable list with at most 200 new choices at a
  time. Selected categories stay selected while searching for another value.
- Relationships derived from row filters honor `max_pair_rows`. A uniform,
  seeded reservoir sample is shared across pairs within each split. The chart
  states its sample size; row filters and individual-column counts still include
  every matching exported row. `max_pair_rows = NULL` requests all pairs.

The compact format corrects one old representation error: an `AsIs` numeric
column now contains scalar numbers instead of one-element arrays. That makes
numeric filtering and plotting work for those columns. The internal record-based
preparation path remains available, and the browser decoder accepts its older
payload shape.

## Measurements

Final report measurements used an immutable source snapshot. The subsequent
BAM convergence changes do not affect these supplied or previously saved fits.
Exact byte counts, process memory and source hashes are in
[measurements.json](measurements.json).

| Explicit export | 0.6.2 HTML | Candidate HTML | 0.6.2 full render | Candidate full render |
|---|---:|---:|---:|---:|
| 500 predictors, 1,200 records | 59.88 MB | 15.84 MB | 16.277 s | 13.168 s |
| 200,000 evaluation + 100 training rows | 121.66 MB | 17.56 MB | 85.319 s | 4.855 s |
| 1,000,000 evaluation + 100 training rows | Not run | 83.34 MB | Not run | 21.126 s |

MB means 1,000,000 bytes. Prepared R data for the 200,100-row case fell from
650.11 MB to 29.82 MB. The million-row export retained 148.22 MB of prepared R
data; the whole report process reached about 1.63 GiB RSS. The 200,100-row
process peak fell from 1.32 GiB to 478 MiB. These are explicit all-row exports.
Ordinary row exports still default to 5,000 records. An 83 MB file is still a
large report; full export remains an explicit choice, not a promise that a
million-row dashboard is inexpensive on every device.

Full rendering uses each version's default explanation scope. The candidate
bounds explanation reference rows; predictive scores still use all evaluation
rows. Separate preparation and serialization timings in the measurement JSON
help distinguish those changes from the compact data format itself. Source
snapshots and hashes identify the implementation used for each timed run.

In Chromium, the final million-row report loaded in about 1.4 seconds and
opened its Records view in 1.5 seconds. The 74 desktop/mobile checks passed,
including exact source lookup, full-row filtering, paging and the full extent
of the displayed scatter sample. Observed JavaScript heap readings were
183 MB on desktop and 208 MB at mobile width; these are not browser peak RSS.

Opening an all-matching relationship fell from 1.919 seconds before the browser
pair cap to 0.115 seconds, and revisiting it took 0.070 seconds. All 1,000,100
matching records remain available to filters and tables; the relationship
explicitly describes its 10,000-row evaluation sample. The earlier operation
completed without errors too. [Browser evidence](million-browser-results.json)
records the measured tasks and scope.

The same 83 MB file did not reach its initial load event within 30 seconds in
the local WebKit WPE runtime. No row-task pass is claimed for that attempt, and
no browser crash was observed. The smaller cross-engine gates passed. This
large-file limit remains under investigation and must not be presented as
universal browser support for million-record exports.

## Reproduce the checks

Run from the repository root with package dependencies and Playwright 1.58.0
installed. The CI workflow installs Chromium, Firefox and WebKit on a supported
Ubuntu runner. Large reports stay outside the repository.

```sh
export AXR_SCALE_OUTPUT="$HOME/.cache/autoxplain-report-checks"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1
Rscript validation/scalability/reports/benchmark.R candidate 2500
Rscript validation/scalability/reports/generate-compatibility.R
python validation/scalability/reports/check-vendor.py
python validation/scalability/reports/check-browser.py --folder "$AXR_SCALE_OUTPUT/candidate/2500" --browsers chromium firefox webkit
python validation/scalability/reports/check-codec.py --folder "$AXR_SCALE_OUTPUT/candidate/2500" --browsers chromium firefox webkit
python validation/scalability/reports/check-categories.py --folder "$AXR_SCALE_OUTPUT/compatibility" --browsers chromium firefox webkit
python validation/scalability/reports/check-sampled-relationships.py --folder "$AXR_SCALE_OUTPUT/compatibility" --browsers chromium firefox webkit
```

Use `benchmark.R candidate 200000` or `candidate 1000000` for the large explicit
exports. Set `AXR_BASELINE_LIBRARY` to the library containing published 0.6.2
when running a `baseline` case. The baseline driver limits row cases to 200,000.
`AXR_REPORT_SOURCE` can point to an immutable package source snapshot for a
candidate measurement while development continues elsewhere. For the wide case,
create the saved fit once with `validation/stress-reports/generate-wide.R` and
`AXR_STRESS_WIDTHS=500`, then reuse its `wide-500/result.rds` for both versions.
`AXR_WIDE_RESULT` can point to that file in another directory. The
[wide payload comparison](wide-payload-parity.json) covers every original row
value, source identity, whole-column statistic and complete prediction payload.

The checks compare complete source identities and values, official prediction
totals, the full extent of a plotted sample, numeric filtering, rare categories,
and independent rank correlations. Corrupt-block cases test the actual static
fallback rather than checking only whether an exception occurs. The separate
[independent review](independent-review/README.md) verifies mixed types, context
columns, preprocessing exclusions, category classification and exact raw versus
processed non-finite flags.

Local Chromium 145 and WebKit 26 passed the compact row tasks, category search,
independent relationship oracles and codec failure cases at desktop and mobile
widths. [Recorded results](local-browser-results.json) keep each gate separate.
The pinned Firefox binary did not launch on this Ubuntu 26.04 host; a traced
launch showed its GL probe child crashing. This does not establish report
incompatibility. The normal Firefox gate remains required on the supported CI
runner. WebKit used isolated Ubuntu 24.04 compatibility libraries without
replacing any system library.

The existing data-explorer, supplied-model, cutoff and prediction browser gates continue to run. Their
oracles decode compact blocks with Python's zlib rather than borrowing the
production JavaScript decoder.
