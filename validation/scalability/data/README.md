# Data exploration at a million rows

The report keeps individual-column distributions, missing counts, means and
quantiles exact. By default, relationships use at most 10,000 rows per partition.
The same sample is used for every pair. This changes the meaning of relationship
counts: they describe the sample, not an estimate of the population count. The
report carries both sample and population sizes.

Use `report_data_control(max_pair_rows = NULL)` when every relationship must use
all rows. `max_rows` still controls the separate, explicit export of individual
records. Neither control changes fitted models or validation scores.

## Measurements

These are data-preparation measurements, not complete report rendering or model
fitting benchmarks. The fixed fixture has four numeric predictors, two categorical
predictors and a numeric outcome. Training contains 80% of rows. Both raw and
processed views are present. The candidate adds pair sampling and reuses exact
univariate summaries when the raw and processed values are identical.

| Workload | Published 0.6.2 | Candidate | Peak process RSS, before / after |
|---|---:|---:|---:|
| 100,000 rows, aggregate summaries | 1.459 s | 0.538 s | 144 / 155 MiB |
| 1,000,000 rows, aggregate summaries | 20.243 s | 2.384 s | 708 / 716 MiB |
| 100,000 explicitly exported records | 4.601 s | 1.059 s | 503 / 164 MiB |

The record export comparison uses the old list of row objects versus the new
direct column store. At 100,000 rows, `object.size()` of the prepared export falls
from 452 MB to 22 MB. A full million-row column export completes in 2.952 seconds,
with a 214 MB prepared R object and 717 MiB peak process RSS. Exporting that many
rows remains an explicit choice; the default record limit is 5,000.

The million-row summary does **not** show a meaningful reduction in peak memory.
The process still holds the input, retained data context and row map, and computes
full-data integrity fingerprints. `tracemem()` verifies that raw and processed
numeric columns share their underlying vectors when preprocessing leaves them
unchanged in this fixture. A second list reference is not a second deep copy.
`object.size()` counts shared references more than once, so its values above are
R object size estimates, not physical memory measurements.

A million-row fixture with an additional unique ID column completes in 6.519
seconds at 854 MiB peak RSS. Its exact counts distinguish 800,000 training IDs
from 200,000 novel evaluation IDs. The internal profile still retains the training
category vocabulary for classification, making this R object 71 MB. Report wire
pruning and compression are separate from these measurements.

All cases independently check whole-data counts and quantiles. The final harness
also checks means and the exact high-cardinality overflow counts. Results and
source fingerprints are in [measurements.json](measurements.json). Each row is a
single fresh-process run on Linux with R 4.5.2. Elapsed time measures only data
preparation. `/usr/bin/time` peak RSS covers the whole process, including fixture
construction, data capture, preparation and checks. It is not a function-specific
allocation count.

## Sampling limits and statistical meaning

[pair-quality.json](pair-quality.json) compares full-data Spearman associations
with 30 fixed samples of 10,000 from 100,000 observations. The largest absolute
error is 0.00095 for the strong monotone relationship, 0.02537 for a curved
relationship and 0.02370 for independent skewed variables. The curved relationship
has nearly zero full-data Spearman correlation despite its strong nonlinear
structure. Sampling cannot fix that limitation of the statistic.

The rare-cluster fixture has only 10 affected observations. Seven of the 30
samples miss the cluster completely, correctly leaving its sampled association
unavailable because the sampled indicator has no variation. The full distribution
still shows all 10 observations. Use the exact pair mode or a larger limit when
rare groups matter. These four constructed problems are checks on interpretation,
not evidence of uniform statistical accuracy across datasets.

Raw and processed samples share selected retained source rows. If preprocessing
removes some selected rows, the processed sample is filled uniformly from its
remaining rows. This preserves a uniform marginal sample of retained rows: the
construction treats every retained source row symmetrically. The seed is preserved
without changing the caller's RNG state. Samples are independent across views only
when no source mapping is available.

Unique categorical IDs no longer receive an automatically perfect association.
Their association is unavailable because there is no category replication. For
partially repeated categories, the result retains the statistic and reports the
number of categories, singleton rows, singleton fraction and replicated rows.
Counts and distributions remain available in both cases.

## Reproduction

Run from the repository root with the published 0.6.2 package installed separately.
The default library location is the release verification cache; set
`AXR_BASELINE_LIBRARY` to use another installation. It must contain version 0.6.2.
`AXR_SCALE_OUTPUT` optionally changes the output directory.

```sh
export AXR_BASELINE_LIBRARY=/path/to/installed-0.6.2-library
export AXR_SCALE_OUTPUT=/tmp/autoxplain-data-scale
mkdir -p "$AXR_SCALE_OUTPUT"
/usr/bin/time -v -o "$AXR_SCALE_OUTPUT/baseline.time" \
  Rscript validation/scalability/data/benchmark.R baseline 1000000 mixed summary
/usr/bin/time -v -o "$AXR_SCALE_OUTPUT/candidate.time" \
  Rscript validation/scalability/data/benchmark.R candidate 1000000 mixed summary
Rscript validation/scalability/data/benchmark.R candidate 1000000 ids summary
Rscript validation/scalability/data/benchmark.R baseline 100000 mixed records-full
Rscript validation/scalability/data/benchmark.R candidate 100000 mixed columns-full
Rscript validation/scalability/data/benchmark.R candidate 1000000 mixed columns-full
Rscript validation/scalability/data/pair-quality.R
```

Run timing commands sequentially on an otherwise idle machine. The comparison
loads the frozen published namespace and sources only the two changed data files
into a child environment. It does not mix in changes to model fitting, audit
sampling or browser serialization. Focused package tests additionally check
source-position alignment after exclusions, legacy control objects, unchanged
full-data summaries, record/column equality, mixed types, nonfinite markers and
categorical replication diagnostics.
