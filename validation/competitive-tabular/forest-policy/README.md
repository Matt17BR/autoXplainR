# Automatic forest search: development evidence

An automatic search that tries every input at every split can spend most of
its budget on a single forest. This probe compares useful alternatives before
the separate full-workflow acceptance run. Initial cost probes use
YearPredictionMSD's fixed development partition; later tree-count checks add
Bank and Covertype. These are development diagnostics, not acceptance results
or claims of broad dataset performance.

The subsequent [version 3 tree-budget amendment](../forest-tree-budget-v3.md)
declares 256-tree automatic final models at four million input work or above.
The [actual production-seed development comparison](final-tree-README.md)
records new 256-tree fits and full 5,000-row importance comparisons. The
500-tree observations and earlier policy history below remain unchanged.

## Original policy and version 2 limits

For automatic searches with at least one million rows times predictors, the
largest `mtry` is `max(round(sqrt(p)), ceiling(p / 3))`. Square-root, intermediate
and one-third anchors remain available. Final candidates still use 500
trees. Complete CV uses 256 trees at one million to below four million input
work, and 128 at four million or above, under the subsequent
[version 2 budget amendment](../forest-validation-budget-v2.md). Explicit custom
grids retain access to every input at each split and
smaller nodes.

Regression split-node alternatives are 5, 20 and 50; classification alternatives
are 1, 5 and 10. Above 50,000 planned training rows these grow by
`sqrt(n / 50000)`, rounded up. In ranger, `min.node.size` is the minimum node size
eligible for splitting. It does **not** guarantee a minimum terminal leaf size.
Larger nodes can miss fine structure, so this is a disclosed computation policy,
not a claim that those settings are optimal. Small-table anchors remain unchanged.

Adaptive screening uses up to 128 trees and a smaller training sample. It removes
the part of node-size growth attributable to the larger planned population,
then rounds to the nearest positive integer. For example, a full-data node size
of 60 planned for 400,000 rows becomes 21 on a 20,000-row screening fit. Complete
validation and final refits retain their declared full-data split parameters;
the separate CV tree budget is recorded alongside the final 500-tree setting.
Both sets of settings are recorded in search evidence.

Disposable screening and cross-validation forests omit native OOB error
calculation because their external assessment rows supply the search score.
Saved final forests retain OOB diagnostics. Structural and prediction parity
checks cover regression, binary and multiclass learners. The measured Year pair
does **not** establish an elapsed-time improvement from omitting OOB calculation.

See the [ranger parameter reference](https://imbs-hl.github.io/ranger/reference/ranger.html)
for the native parameter meanings. The package tests also check that explicit
grids are unaffected and that screening does not overwrite final settings.

## Probe design

The initial plan was written before fitting on 12 September 2026. Each main
probe fits all 40,000 development fitting rows, then scores the 10,000 fixed
training-only calibration rows. There are 90 numeric inputs. Settings are seed
80711, sample fraction 0.8, two native threads, standard variance splits unless
marked Extra Trees, and five random split points for Extra Trees. OOB is enabled
unless the record explicitly disables it. Timing covers fitting, excluding
calibration prediction and saving. Other benchmark processes shared the host,
so elapsed-time comparisons are descriptive and can be noisy.

The first sweep compares 128 trees at `mtry` 9 and 30 with node sizes 5, 20 and
50, plus an Extra Trees option. The full-input legacy probe has only 32 trees:
its loss must not be treated as a matched 128-tree quality comparison. Separate
500-tree fits check whether promising choices remain useful with the full
number of trees. JSON records retain exact parameters and precision; native
models and complete calibration predictions are cached rather than committed.

The screening probe fits 16,000 development rows and scores 4,000 calibration
rows, using sampling seed 80761 inside the existing partitions. It compares
planned node size 61 with screening size 20 at `mtry` 9 and 30. The count 463,715
sets a hypothetical full-population policy only. These are actual 16,000-row
fits, not acceptance fits or evidence of training on 463,715 rows.

The independent [forest-family acceptance gate](../forest-family-acceptance.md)
requires acceptable retained forest quality even when boosting wins overall.

## What the calibration showed

These are individual two-thread runs, not repeated timing benchmarks. Full
precision, native counts and settings are in [results.json](results.json).

| Split rule | Trees | Inputs per split | Split-node size | Fit seconds | Calibration RMSE |
|---|---:|---:|---:|---:|---:|
| Variance | 128 | 9 | 5 | 40.124 | 9.686888 |
| Variance | 128 | 9 | 20 | 31.993 | 9.697669 |
| Variance | 128 | 30 | 5 | 121.568 | 9.497077 |
| Variance | 128 | 30 | 20 | 88.379 | 9.481114 |
| Variance, full-input cost probe | 32 | 90 | 5 | 93.539 | 9.648439 |
| Variance | 500 | 9 | 5 | 155.089 | 9.643309 |
| Variance | 500 | 30 | 20 | 443.796 | 9.472599 |
| Extra Trees | 500 | 9 | 5 | 111.024 | 9.672467 |

The 128-tree comparisons support keeping larger-node alternatives: node size 20
preserved similar loss while taking less time than node size 5 in both matched
`mtry` comparisons. The full-input probe took longer with just 32 trees than
the 128-tree forest trying 30 inputs with split-node size 20. Its smaller tree
count prevents a matched quality comparison. The 500-tree runs show that the alternatives still
make useful predictions after increasing tree count. They also show that a
full forest can remain expensive, so these policy changes alone do not establish
that a complete search meets the resource gate.

The screening rescale changes calibration RMSE from 9.9677 to 9.9889 at `mtry=9`
and from 9.8213 to 9.8117 at `mtry=30`. It preserves comparable quality in this
probe; it does not consistently improve it. The OOB-disabled timing was 37.159
seconds versus 31.993 with OOB enabled, despite identical trees and calibration
predictions. No OOB timing benefit is claimed.

## Tree-count fidelity diagnostic

A further probe was declared after the first full 500-tree candidate workflow
showed that repeated forest fitting remained expensive. It uses the official
[`num.trees` prediction argument](https://imbs-hl.github.io/ranger/reference/predict.ranger.html)
to compare the first 128, 256 and 500 trees of each saved 500-tree calibration forest.
Independent row means of all per-tree predictions check native aggregation.
The 10,000-by-500 numeric array is approximately 40 MB per forest.

Paired resampling of 300 calibration-row samples describes variation in the loss
difference from 500 trees. Thirty random tree subsets describe variation within
each saved forest. The subsets overlap; they are not independent forest fits.
Neither procedure refits models or establishes training-sample uncertainty.
Parameter-derived fitting seeds may change when a search uses fewer trees, so
prefix agreement alone does not prove that a separate 256-tree fit is identical.
This diagnostic can support a later declared search approximation; the initial
candidate workflow and its results remain intact.

```sh
Rscript validation/competitive-tabular/forest-policy/prefix-probe.R CACHE /tmp/forest-probes /tmp/forest-prefix
```

In these three regression forests, using 256 rather than 500 trees changed RMSE
by +0.01516, +0.01221 and -0.00072, all below 0.2% of the 500-tree loss. Their ordering
was unchanged. Native predictions agreed with independent tree averages within
`3.64e-12`. See [prefix-results.json](prefix-results.json) for paired resampling
and within-forest subset variation.

The predeclared classification extension uses the already assessed native Bank
forest (20,000 fitting rows, 5,000 development assessment rows) and Covertype
forest (50,000 fitting rows, 20,000 assessment rows). It aggregates per-tree
probabilities in chunks of 1,000 rows, with at most 28 MB for the seven-class
prediction array. It never reads acceptance data.

| Development case | Trees | Log loss, floor `1e-15` | Brier score |
|---|---:|---:|---:|
| Bank |256|0.27296794|0.07669055|
| Bank |500|0.27276904|0.07662812|
| Covertype |256|0.33529420|0.18463115|
| Covertype |500|0.33519948|0.18433041|

Bank had no zero probabilities for the observed class. Covertype had one at
128, 256 and 500 trees, so unbounded log loss is infinite for all three. The table
uses the benchmark's declared probability floor and the records retain zero
counts. Covertype's rare cottonwood class had 71 of 94 correctly classified at 256
trees versus 74 at 500. Close aggregate loss therefore does not imply identical
rare-class decisions. The other class recalls and paired loss variation are in
[classification-prefix-results.json](classification-prefix-results.json).

```sh
Rscript validation/competitive-tabular/forest-policy/classification-prefix-probe.R CACHE NATIVE_RUN_DIRECTORY /tmp/classification-prefix
```

Together these are supporting diagnostics for a 256-tree validation
approximation with 500-tree final models. They do not measure its full-workflow
cost, prove identical candidate selection or replace the forest-family
acceptance gate.

## Explanation cost

A separate inference probe replays three saved 500-tree forests through the
package's prediction wrapper. Each was fitted on the same 40,000 development
rows. Predictions use two native threads. Two repetitions reverse the batch
order; these are shared-host observations, not deployment latency guarantees.

| Forest | Predict 5,000 rows, seconds | Full 10,000-row identity check, seconds | Three inputs, three permutation repeats, seconds |
|---|---:|---:|---:|
| Variance, `mtry=9`, node size 5 |1.55 to 1.75|4.17|20.08|
| Variance, `mtry=30`, node size 20 |0.73 to 0.79|2.25|11.32|
| Extra Trees, `mtry=9`, node size 5 |1.45 to 1.54|5.49|23.54|

The identity check includes a complete evaluation prediction and model hashing.
The importance call includes nine 5,000-row perturbation predictions and two
10,000-row predictions for baseline and identity. That is 65,000 predicted rows
for just three inputs and three repeats.

Five separate 5,000-row perturbation batches were also concatenated into one
25,000-row native forest prediction. Every prediction was byte-identical; the
observed time reduction ranged from 2% to 38%, with smaller benefits for the
forest using node size 20. General custom prediction functions can depend on
batch composition, so this is evidence for a native forest optimization only.
It does not justify concatenating arbitrary custom-model batches.

The same probe verified that all separate 128-tree fitted-model predictions
matched the first 128 trees of their saved 500-tree counterparts at the same
seed. This native prefix fact does not override the package's separate
parameter-derived seed rules.

The [operation-count audit](../explanation-work/README.md) identifies why even
bounded row samples can be expensive. Full raw measurements and source hashes
are in [inference-results.json](inference-results.json).

```sh
Rscript validation/competitive-tabular/forest-policy/inference-probe.R CACHE /tmp/forest-probes /tmp/forest-inference.json
```

## Reference-row precision

The [predeclared precision probe](precision-protocol.md) uses a frozen source
snapshot and compares nested samples of 1,000 and 5,000 rows from the separate
20,000-row Year development assessment set. Both screen all 90 inputs with five
repeats on the retained 500-tree forest with `mtry=30` and split-node size 20.

The smaller sample took 158.371 seconds versus 374.850 seconds. All eight leading
inputs appeared in both sets; 14 of the leading 16 overlapped. Effect magnitudes
were less stable: `timbre_03` changed from 0.4417 on 5,000 rows to 0.3165 on 1,000,
with non-overlapping shuffle intervals. The strongest input, `timbre_01`, changed
from 1.3544 to 1.3772. This illustrates why shuffle intervals do not describe
uncertainty from selecting reference rows.

These results support further testing of cheaper screening. They do not justify
silently treating a smaller sample as equally precise detailed evidence across
tasks. No default changed as part of this probe. The complete 90-input means,
intervals, signs and ranks are in [importance-comparison.csv](importance-comparison.csv);
timing, overlap and frozen source hashes are in [precision-results.json](precision-results.json).

```sh
Rscript validation/competitive-tabular/forest-policy/precision-probe.R CACHE /tmp/forest-probes /tmp/forest-precision
```

The [classification extension](classification-precision-protocol.md) keeps the
same nested sample and shuffle seeds and uses saved native probability forests.
It also records all class counts and three repeated prediction timings at each
row limit. The complete summaries and source hashes are in
[classification-precision-results.json](classification-precision-results.json).

| Development forest | 1,000 rows | 5,000 rows | Shared top 8 | Shared top 16 |
| --- | ---: | ---: | ---: | ---: |
| Bank, 20,000 training rows | 38.134 s | 77.066 s | 5 | 14 |
| Covertype, 50,000 training rows | 307.868 s | 493.131 s | 8 | 16 |

Bank fails the declared feature-retention checks. `poutcome` changes from a
positive importance of 0.00242 on 5,000 rows to -0.00043 on 1,000 rows. This is a
smaller-work control outside a possible large-forest trigger, but it rules out
treating the smaller pilot as a general substitute. See the
[complete Bank feature comparison](bank-importance-comparison.csv).

Covertype retains both leading feature sets, but some magnitudes shift beyond
their shuffle intervals: `horizontal_roadways` changes from 0.1751 to 0.1593 and
`hillshade_noon` from 0.06235 to 0.05313. The rarer cottonwood/willow class has only
five observations in the smaller sample versus 20 in the larger one. Passing
the selection checks on this one sample pair does not establish stable feature
selection. See the [complete Covertype comparison](covertype-importance-comparison.csv).
Detailed importance and screening retain their existing row budgets.

```sh
Rscript validation/competitive-tabular/forest-policy/classification-precision-probe.R \
  CACHE /path/to/native-development-v2 /path/to/frozen-source /tmp/classification-precision
```

The [report cost assessment](../explanation-work/report-cost-assessment.md)
connects these measured prediction times to the unchanged permutation work.

## Reproduce

First prepare the development data using the parent benchmark instructions.
Run from the repository root with R, ranger, digest, jsonlite and pkgload
installed. Replace `CACHE` with the benchmark cache directory. The scripts
construct the development path explicitly and never open acceptance data.

```sh
Rscript validation/competitive-tabular/forest-policy/run-probes.R CACHE /tmp/forest-probes
Rscript validation/competitive-tabular/forest-policy/run-probes.R CACHE /tmp/forest-probes sqrt_node5 500
Rscript validation/competitive-tabular/forest-policy/run-probes.R CACHE /tmp/forest-probes third_node20 500
Rscript validation/competitive-tabular/forest-policy/run-probes.R CACHE /tmp/forest-probes extra_sqrt_node5 500
Rscript validation/competitive-tabular/forest-policy/run-probes.R CACHE /tmp/forest-probes sqrt_node20 128 no_oob
Rscript validation/competitive-tabular/forest-policy/screen-probe.R CACHE /tmp/forest-probes
Rscript validation/competitive-tabular/forest-policy/replay.R CACHE /tmp/forest-probes /tmp/forest-replay.json
```

Outputs are never overwritten. The default sweep can take several minutes.
Fresh replay verifies every saved prediction, native row/tree/input counts,
partition boundaries and OOB parity. It does not refit the models.

The original eight-fit sweep wrote all eight native models, prediction arrays
and JSON records, then its encompassing R process exited with a parse error.
The script had been edited while R was executing an already parsed loop, causing
a later source-file read at an obsolete offset. This was a harness failure after
fitting; the whole script did not complete successfully. The exact pre-edit
whole-script bytes were not retained. Later runs used an unchanged frozen copy.
The independent replay covers the eight retained individual outputs as well as
later fits. This limitation is recorded rather than relabeling the failed run.

The portable scripts adapt cache paths and add source manifests to that frozen
runner. The committed manifest distinguishes original run provenance from
these reproducible scripts. No native model binaries or outcome rows are
included in this directory.
