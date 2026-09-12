# Scaling to a million training rows

These probes distinguish fitting, training-only model selection, evaluation,
explanation and report preparation. A completed sampled explanation is not
evidence that a model was fitted or evaluated on a million rows.

`fixtures.R` generates four deterministic problems:

| Problem | Inputs | Outcome |
| --- | --- | --- |
| `regression` | 20 numeric inputs, including correlated inputs | Curvature, a squared effect and an interaction, with Gaussian noise SD 0.7 |
| `rare_binary` | 20 numeric inputs and categorical inputs with 32 and 5 levels | Nonlinear event probabilities; 430 events in the fixed 20,000-row evaluation set |
| `multiclass` | The same mixed input dimensions | Four classes from nonlinear softmax probabilities |
| `wide` | 128 numeric inputs and categorical inputs with 1,000 and 5 levels | Nonlinear regression with category effects and Gaussian noise SD 0.7 |

Two numeric inputs have 2% missingness. Categorical examples also have 1%
missingness in the larger factor. Missingness is independent of outcome noise.
The package must learn imputation from the training portion of each fold.

Each input and outcome draw uses its own recorded random stream. The training
sets at 10,000, 100,000 and 1,000,000 rows are nested prefixes. Each problem has
a fixed, separately generated 20,000-row evaluation set. The known generating
mean or probability gives a useful reference loss before missing inputs. It is
not an achievable-model guarantee, because fitted models do not observe the
missing values or the generating formula.

## Running a bounded process

The baseline is the downloaded, published `AutoXplainR_0.6.2.tar.gz` artifact,
SHA256 `e15ee291f447414a16e10acce0f69cca826d7e58b424f5597c41ea9f56114624`.
The download was checked against both its attached `SHA256SUMS` and GitHub asset
digest. Candidate source must be installed into a different library.

Install the exact package artifact into a separate library first. For example:

```sh
python3 validation/scalability/million/supervise.py \
  --library /path/to/published-library \
  --output /path/to/evidence/regression-10000 \
  --problem regression --rows 10000 --mode controlled_fit \
  --seconds 180 --memory-gib 12
```

The supervisor freezes its R runner and fixture in each new evidence directory,
then starts a fresh R process with one native thread, a 12 GiB
address-space limit and an explicit wall-time limit. It preserves partial stage
events even if the process times out. These limits protect the machine; they are
not package-native budgets. Runs use a shared harness lock and are scheduled
apart from other agents' timed workloads. Address space and resident memory are
different quantities; `time.txt` supplies the process peak resident memory.
If termination prevents that final record, the index labels the last observed
high-water mark as a lower bound rather than an exact peak.

The modes make the user-facing choices explicit:

- `controlled_fit` uses `autoxplain()` with regularized and boosting learners,
  two folds, two configurations, and `tuning_control(retain_oof = FALSE)`.
  It disables explanation preparation and omits an HTML report.
- `controlled_full` uses the same fitting controls and requests explanations
  and an HTML report in that one public call.
- `default_fit` and `default_full` preserve the ordinary core search defaults.
- `quick_fit` and `quick_full` select the public quick workflow.
- `stronger_fit` and `stronger_full` are a separate capacity experiment: one
  regularized configuration and two boosting configurations (300 rounds at
  depth 4, and 600 rounds at depth 6, both at learning rate 0.05), with two
  folds and no retained OOF predictions. These fixed larger grids are declared
  before running their evaluation; their results do not replace the controls.

All cases pass the full training frame and fixed evaluation frame directly to
`autoxplain()`. The checks verify retained training and evaluation row counts,
native training counts where available, successful candidates' complete
cross-validation row coverage, finite predictions and independently calculated
losses on every evaluation row. Complete results and tuning tables are saved.
The runner records explanation sample sizes when that API is available, and
checks that reported full-data scores still use all evaluation rows.

For a separate-process reload check:

```sh
Rscript --vanilla validation/scalability/million/replay-case.R \
  /path/to/published-library /path/to/evidence/regression-10000
```

Append `report` to request a report after reload. That explicit report uses four
features, three models and five permutation repeats; these reduced display and
Monte Carlo settings are recorded. The original one-call `controlled_full`
benchmark uses the package's default report settings instead. Schedule and bound
large replay/report processes just as you would fitting runs.

## Reading evidence

Each run records source hashes, package and engine versions, data provenance,
the exact command, external limits, stage timings and memory snapshots. A
`public-call.json` checkpoint distinguishes a completed public call from a later
verification or serialization timeout. `result.json` contains independent
holdout losses and the complete-run verdict. `tuning-evidence.rds` preserves
exact numeric values for old/new comparisons; JSON is a readable index.
The comparison re-predicts every held-out row with every retained model. For
legacy matrix fits it recognizes one intentional metadata change: the new
version explicitly records `encoding = "matrix"`. Any native encoding, changed
numerical setting, prediction, score, row count or fit seed still fails parity.
`inspect-object.R` inventories retained components and native call arguments.
Its `object.size()` results count shared vectors repeatedly and should not be
mistaken for measured resident-memory attribution. `summarize.py` produces both
a run index and inclusive stage durations; nested stage times must not be summed.

Initial harness mistakes are preserved in their original cache directories:
one attempt could not serialize an R `table` before calling the package, and
another imposed a stricter multiclass normalization tolerance than the public
prediction validator. Neither is counted as a package failure. The runner now
uses the validator's `1e-6` tolerance for native float32 probabilities and
records the actual maximum error.

The first completed million-row comparison uses the fixed two-configuration
regression control. All three models were fitted on 1,000,000 rows and evaluated
on the same 20,000 independent rows. The public call fell from 145.331 to 33.596
seconds, process peak RSS from 4,911,800 to 2,814,432 KiB, and uncompressed saved
result size from 903,015,420 to 654,947,875 bytes. Every holdout prediction for
every retained model, CV score, selected configuration and fitting seed agreed
within `1e-10`. Both versions passed separate-process reload checks.

This control still underfits: primary RMSE is 1.6835, compared with 0.6993 for
the generating mean before missingness. The faster unchanged result establishes
less overhead, not stronger predictive performance. The separate capacity cases
are needed to assess that question. These are single-process observations on
one machine, not guaranteed runtime or memory budgets.

The ordinary 10,000-row default exposed a different problem. Its 500-iteration
neural limit excluded four of seven neural configurations for nonconvergence,
including the useful six-unit model. A separate training-only probe reused the
same folds and initialization seeds and established that a 2,000-iteration limit
recovered all those fits. The public workflow then repeated its normal search
with the new limit. It selected six hidden units and weight decay 0.01 using
five-fold CV, whose pooled RMSE was 0.772083. On the unchanged independent
20,000-row evaluation set, RMSE improved from 1.6988 to 0.7475. All 15 default
configurations completed; convergence remains a requirement for selection.

That quality improvement has a cost: the public call increased from 49.104 to
82.588 seconds. Peak RSS was 277,316 versus 257,500 KiB. Retaining OOF evidence
for the recovered configurations increased the uncompressed result from
39,357,524 to 41,768,184 bytes. Both versions passed fresh-process prediction
checks for every retained model. Actual native neural fitted-value arrays and
the tree root independently confirm all 10,000 training rows were used.
These observations support the new configurable iteration budget; they do not
establish that 2,000 iterations will converge on every dataset.

The raw evidence is kept outside the source package at
`~/.cache/autoxplain-scale-0.7.0/`. Findings and final before/after measurements
will be added after the staged runs finish. Until then, these scripts establish
a measurement protocol rather than a claim of million-row readiness.

## Retained native call regression

The published regularized adapter called `glmnet()` through `do.call()`, which
left the complete input matrix, outcome and function body inside the native
model's recorded call. The new call keeps literal fitting settings and symbolic
`x` and `y` arguments. Its input requirements are recorded, and it can be
reconstructed from the saved blueprint and supplied training outcome.

On the fixed 10,000-row regression case, the native serialized model shrank from
2,565,520 to 11,565 bytes. Every native field except the call and every prediction
on the 20,000-row holdout remained identical. This removes a redundant payload;
it does not remove the full raw or processed data retained by the public result.
The focused test also reconstructs regression, binary and multiclass fits,
including the one-input dummy-column workaround. Original failing assertions
and exact parity evidence remain in the cache's `native-call/` directory.

The forest and MARS adapters had the same retained-input problem. On a fixed
180-row input, the forest call shrank from roughly 202 KB to 494 bytes. Native
fields other than the call and every prediction stayed identical for regression,
binary and multiclass fits. Rebuilding each model from the compact call gives
the same native forest and out-of-bag predictions.

For MARS, the recorded call shrank from 9,485 to 306 bytes for regression and
from 41,153 to 436 bytes for binary classification. Both supported tasks preserve
native values and predictions; the native comparison ignores environment
identity. Binary reconstruction explicitly supplies `stats::binomial()`. The
tests also reload complete public results and check their raw-data predictions.
These call changes leave legitimate training diagnostics intact. Before-failure,
after-test and native parity records are in `forest-call/` and `mars-call/`.
