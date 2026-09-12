# Independent row-export review

The [recorded verdict](verdict.json) compares eight small prepared-report cases
with 1,162 exported raw and processed cells. It checks the old row representation,
compact export from records and compact export directly from columns.

Cases include excluded rows, stage-specific nonfinite flags, absent raw data,
absent and empty training partitions, bounded and complete export, AsIs numeric
columns, Date and POSIXct values, factor and text values, hostile Unicode,
pooled categories and novel identifiers. Every exported categorical value must
retain its displayed, other or novel classification after dictionary pruning.
The entire decoded payload must otherwise match, except for the documented
AsIs scalar correction and removal of unused wire-only known levels.

This checks values and their interpretation, beyond compression round trips.
It also decodes a compressed hostile-text block with Python's zlib, independently
of the browser library. It does not measure browser memory or prove that every
possible R column object is supported.

Run from the repository root:

```sh
Rscript validation/scalability/reports/independent-review/generate.R
python3 validation/scalability/reports/independent-review/check.py
```

Set `AXR_COMPACT_REVIEW_OUTPUT` to use another intermediate output folder.
