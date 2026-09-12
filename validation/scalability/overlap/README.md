# Exact overlap checks at larger row counts

The supplied-evaluation workflow checks whether any evaluation row also occurs
in training. Version 0.6.2 serialized every complete row into a long hexadecimal
key. The replacement compares exact column values, discards candidates as soon
as a column differs, and checks the remaining combinations. It does not accept a
hash match as proof that two rows are equal.

Integer and numeric columns still compare alike, as do factors and their text
labels. The existing distinctions between signed zeros, NA and NaN, NaN bit
patterns, and string encoding flags remain. Unusual column objects use the
previous whole-row comparison after ordinary columns have narrowed the search.
That fallback also preserves shared references across list columns.

## Measurements

These fresh-process runs used 21 columns and 20,000 evaluation rows from the
controlled regression fixture. Each process checked disjoint data, then five
injected matches. One additional row shared its first value with training but
differed elsewhere. Both the number of matches and their original evaluation
positions were checked independently.

| Implementation | Training rows | Disjoint, seconds | Five matches, seconds | Peak process RSS, MiB |
| --- | ---: | ---: | ---: | ---: |
| Published 0.6.2 | 100,000 | 11.311 | 11.207 | 298.1 |
| Candidate | 100,000 | 0.023 | 0.027 | 131.0 |
| Candidate | 1,000,000 | 0.236 | 0.181 | 430.2 |

Elapsed times cover the overlap check. Peak RSS covers the whole process,
including fixture creation, both scenarios and the candidate's independent
position assertions. These are single runs, not estimates of typical latency.
The continuous first predictor quickly rules out most candidates in this
fixture. Repeated categorical columns can require scanning all columns, and
unusual object columns can still need the slower fallback.

The separate published million-row workflow spent 97.343 seconds in overlap
checking, but that trace is not a matched isolated benchmark and is not used to
calculate a speedup here. Full workflow results live in the
[million-row study](../million/README.md).

The [recorded measurements](measurements.json) include function-body hashes and
R versions. The focused package tests check binary64 distinctions, normalization,
encoding, mixed columns and original row positions. A separate reviewer compared
307 cases against the immutable 0.6.2 namespace, including 300 randomized mixed
frames and unsupported list, complex, raw and shared-environment values; all
matched. Its replay is [check-overlap.R](../million/check-overlap.R).

## Reproduce

Run from the repository root with R and the package dependencies installed. Set
`AXR_BASELINE_LIBRARY` to an installed published 0.6.2 library if it is not in the
default cache location. `AXR_SCALE_OUTPUT` optionally selects the output folder.

```sh
mkdir -p /tmp/autoxplain-overlap
export AXR_SCALE_OUTPUT=/tmp/autoxplain-overlap
/usr/bin/time -v -o "$AXR_SCALE_OUTPUT/baseline-100000.time" Rscript validation/scalability/overlap/run.R baseline 100000
/usr/bin/time -v -o "$AXR_SCALE_OUTPUT/candidate-100000.time" Rscript validation/scalability/overlap/run.R candidate 100000
/usr/bin/time -v -o "$AXR_SCALE_OUTPUT/candidate-1000000.time" Rscript validation/scalability/overlap/run.R candidate 1000000
Rscript validation/scalability/million/check-overlap.R
```

The baseline loads the published namespace. The candidate sources only the
current workflow file into an environment whose parent is that namespace, so
unrelated fitting or reporting changes cannot explain these timings.
