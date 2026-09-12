# Paired-bootstrap computation

The optimization preserves the ordinary 1,000 full-evaluation bootstrap
draws, paired models, whole-group sampling and percentile intervals. It caches
fixed row contributions instead of recalculating them for every draw. Ordinary
IID draws directly produce source-row indices, avoiding a list with one member
per observation. Grouped draws retain their original row expansion and order.

The first functional gate passed 293 assertions with no failures, warnings or
skips, including the existing uncertainty and validation-design tests. The new
unit tests independently resample literal outcomes and predictions and
recalculate all six supported task/metric combinations. They cover unequal
groups, first-level binary events, boundary probabilities, reordered multiclass
columns, an absent evaluation class, signed zero and caller RNG preservation.
The existing grouped test also covers omitted rows and surviving-group alignment.
The [reference replay](reference-verdict.json) preserves every output field and
sampled-unit sequence exactly across twelve task, metric and grouping cases,
and checks all 444 draws against the literal-row oracle. It also verifies caller
RNG preservation. Runtime measurements and cross-platform acceptance remain
separate gates.

## Initial measurements

All four 20,000-row calls completed using the same frozen installation. The
saved interval objects, including every draw, endpoint and note, were exactly
identical between variants; their serialized bytes also match.

| Evaluation problem | Before public call | After public call | Before peak RSS | After peak RSS |
| --- | ---: | ---: | ---: | ---: |
| Regression RMSE, 20,000 rows | 1.039 s | 0.671 s | 141,804 KiB | 138,980 KiB |
| Eight-class Brier, 20,000 rows | 5.183 s | 0.838 s | 162,548 KiB | 163,112 KiB |

These are individual observations on one host. The Brier case has a substantial
time reduction, while its process peak memory is slightly higher. Every call
still uses 1,000 paired full-evaluation resamples. Larger cases and final
cross-platform acceptance remain pending. Exact measurements, limits, hashes
and session details are in [measurements.json](measurements.json).

## Reproduce the comparison

The frozen pre-optimization function is available from
`afefdbe:R/performance_uncertainty.R`; its SHA-256 is
`b0d94d406c57781221428c289f668438b4aa2ada702f64986251dec250fa10eb`.
The local reference is under
`~/.cache/autoxplain-scale-0.7.0/uncertainty/performance_uncertainty-before.R`.
Both variants use the same installed package, prediction functions and inputs;
the before variant substitutes only that frozen public function. This isolates
the bootstrap change, rather than comparing two complete package releases.

`check-reference.R SOURCE_DIRECTORY FROZEN_REFERENCE OUTPUT.json` compares every
output field and sampled unit against that old function in all twelve task,
metric and grouping cases. It also checks every draw against the independent
literal-row oracle and confirms that prediction preparation occurs once per
call. Run the focused unit tests before this check.

First run both variants on 20,000 evaluation rows and require exact equality of
the saved `interval.rds` objects. Only after that small case shows a material
improvement should the same protocol progress to 200,000 and 1,000,000 rows:

```sh
python3 validation/scalability/uncertainty/supervise.py \
  --library /path/to/installed-candidate-library \
  --reference /path/to/performance_uncertainty-before.R \
  --output /path/to/evidence/regression-20000-before \
  --variant before --problem regression --rows 20000
```

Repeat with `--variant after` and a new output directory. The second problem,
`multiclass_brier`, uses eight probability columns. These deterministic supplied
prediction fixtures isolate evaluation and interval computation; they say
nothing about model fitting quality. Each process has one native thread and
external 180-second/12-GiB limits, and shares the scaling harness lock. Schedule
runs apart from other workloads. Timeouts remain timeouts, not completed timings.

The public-call timer includes ordinary prediction/context validation but
excludes creating the evaluation result and saving the interval. Both preceding
preparation time and whole-process peak RSS are recorded separately. Every run
uses the default 1,000 draws and every evaluation row. The frozen source, executed
runner, exact command, interval object and session details are retained.
