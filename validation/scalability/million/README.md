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
The final full-workflow runner also traces the paired uncertainty result and
checks all 1,000 bootstrap draws use the full 20,000 evaluation sampling units.

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

For the final `controlled_full` case, decode the HTML after the cold check:

```sh
python3 validation/scalability/million/check-final-report.py /path/to/evidence/case
```

This compares the actual report's full-row counts and scores with the original
predictions verified by the cold process. Browser interaction remains a separate
check of that same HTML file.

`render-saved.R` provides a separate review report with six top features, up to
four models, five permutation repeats, 5,000 explanation rows and 1,000 exported
data rows. It records the chosen limits beside the HTML. That report helps
inspect actual prediction errors and feature effects in a completed fit, while
the one-call full-workflow measurements retain their own default settings.

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
The original cold replays compare 64 saved raw prediction probes per model and
recompute each model's loss over the full 20,000-row holdout. The final wide and
one-call full runner additionally saves every model's complete holdout prediction
vector before serialization, then checks all values after a cold reload. Its
JSON result states whether those full saved vectors were available; an older
64-probe artifact does not acquire stronger claims retrospectively.

Eleven historical cases also passed a stronger check against columns retained
by their original public call, without generating a new warm reference. Cold
predictions matched every original primary and baseline prediction or binary
probability on all 20,000 rows. For multiclass results, the original columns
contain class labels and primary maximum probability; all those values matched,
but the complete original class-probability matrices were not retained. Alternate
models still have the stated 64 raw probes and full-loss checks. The portable
index records the exact verified original fields for each model.

A separate 200-row multiclass harness control checked the new full-vector
contract: all three original 20,000-row probability matrices survived cold
reload. Altering one alternate-model probability at row 500, outside the 64
probes, correctly failed the complete-vector comparison. This tests the harness,
not model performance; its [verdict](replay-contract.json) records the scope.
`inspect-object.R` inventories retained components and native call arguments.
Its `object.size()` results count shared vectors repeatedly and should not be
mistaken for measured resident-memory attribution. `summarize.py` produces both
a run index and inclusive stage durations; nested stage times must not be summed.
Run it on each `baseline` or `candidate-v*` directory, then use
`collect-results.py CACHE OUTPUT.json` to produce a portable combined index.
That index retains failed attempts, distinguishes the two established harness
mistakes below, and links the immutable candidate source manifests by hash.

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

For this regression generator, `conditional-reference.R` also calculates the
known mean given the inputs the model can actually see. A hidden, independent,
mean-zero `x3` contributes zero to that mean; the irrelevant missing `x8` changes
nothing. This reference has RMSE 0.727177 on the fixed evaluation set, compared
with 0.699287 when the hidden values are supplied. Its population RMSE is
`sqrt(0.7^2 + 0.02 * 1.5^2) = 0.731437`. The new default's 0.7475 is therefore
close to the reference that respects missing inputs. This calculation is a
diagnostic reference only and was never used for fitting or selecting a model.

That quality improvement has a cost: the public call increased from 49.104 to
82.588 seconds. Peak RSS was 277,316 versus 257,500 KiB. Retaining OOF evidence
for the recovered configurations increased the uncompressed result from
39,357,524 to 41,768,184 bytes. Both versions passed fresh-process checks of
64 saved raw predictions per model and complete 20,000-row holdout losses.
Actual native neural fitted-value arrays and
the tree root independently confirm all 10,000 training rows were used.
These observations support the new configurable iteration budget; they do not
establish that 2,000 iterations will converge on every dataset.

The two million-row classification controls also completed with every training
row and the same 20,000 independent evaluation rows. The candidate planned native
categorical boosting once from outer-training dimensions, then kept that
representation fixed through both folds and the final fit. Tracing `xgb.train()`
confirmed the actual final matrix contained 1,000,000 rows and 22 columns;
the published matrix encoding had 55 columns. Category levels and imputation
remained local to each training fold.

| Million-row control | Published public call | Candidate public call | Published peak RSS | Candidate peak RSS | Published boosting log loss | Candidate boosting log loss |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Rare binary | 188.882 s | 70.777 s | 6,125,976 KiB | 3,503,424 KiB | 0.0699033 | 0.0686940 |
| Four classes | 328.787 s | 200.551 s | 5,717,652 KiB | 3,257,168 KiB | 0.9219975 | 0.9050906 |

These boosting losses are deliberately not presented as prediction parity:
native category partitions are a different model representation from numeric
contrasts. Both versions selected using CV only; the regularized and intercept
baseline holdout losses remained identical. Fresh processes reloaded all three
models, matched 64 saved raw prediction probes per model and recomputed losses
on every holdout row. Reversing factor levels on those probes preserved
predictions; the tested missing values and previously unseen category gave
finite predictions. The candidate saved results still occupied
871,494,857 and 705,542,148 bytes respectively because they retain the full data
and evaluation evidence. These fit-only controls do not establish full-report
latency or the cost of the unrestricted default search on a million rows.

At 100,000 rows, the wider problem completed in 29.549 seconds in the candidate
versus 126.832 seconds in the published package. Peak RSS fell from 6,688,820 to
2,368,584 KiB. Boosting used 130 native inputs instead of 1,131 matrix columns,
and its holdout RMSE was 1.7454 versus 1.8239. Both configurations succeeded.

The published million-row wide workflow reached its 420-second guard without
returning a result. Row-overlap serialization took 370.742 seconds. Tuning then
began and entered the first regularized fit attempt; no boosting training matrix
was reached. The last recorded process high-water mark was 12,274,016 KiB,
a lower bound because termination prevented the final GNU `time` record.
This is a failure to finish the stated whole workflow within the external
limits, not proof that a native engine cannot fit those inputs on another budget.

The intermediate candidate reached both tuning folds and completed the actual
million-row boosting fit. It attempted the regularized refit and advanced
through evaluation, then failed while allocating the final fingerprint
serialization buffer. Final per-family statuses were not saved.
The process ended after 271.114 seconds with peak RSS 12,257,676 KiB.
No complete result or accepted holdout score exists for that failed run.
The failed attempt remains evidence; its status is not overwritten by a rerun.

After the exact fingerprint repair, the complete wide million-row workflow
finished under the same 420-second and 12 GiB limits. The public call took
303.841 seconds and the process 310.883 seconds, retaining all three models with
no failed configurations or refits. The actual final boosting matrix had
1,000,000 rows and 130 columns, and native regularized `nobs` was 1,000,000.
Boosting RMSE was 1.732706, regularized RMSE 2.143834 and baseline RMSE 2.618056.
The cold process compared every original 20,000-row prediction vector, including
the alternate model, and passed the reordered-factor and tested new/missing
input checks. Peak RSS was still 12,261,144 KiB and the uncompressed result
3,341,727,020 bytes. Completion within this stated budget is useful evidence;
it does not make the wide workflow inexpensive on a smaller machine.

The declared larger regression grid completed at both training sizes. At
10,000 rows, the existing one-standard-error selection rule preferred 300
boosting rounds at depth 4; holdout RMSE was 0.910107 and the public call took
4.129 seconds. At one million rows, CV chose 600 rounds at depth 6, whose
holdout RMSE was 0.749065, compared with 1.683529 in the shallow control.
All three configurations completed, and traces verified the actual full
million-row refit. That public call took 109.012 seconds, process peak RSS
was 2,895,452 KiB, and the saved result occupied 657,678,928 bytes.
Both larger-grid results passed cold checks of 64 saved raw probes per model
and full 20,000-row losses. Their grid was fixed before observing either
holdout result; it is a capacity experiment separate from the timing control.

The final one-call regression control fitted all 1,000,000 training rows,
scored the fixed 20,000-row holdout and produced its default report in 81.055
seconds under the unchanged 420-second / 12 GiB bounds. Peak RSS was 2,455,048
KiB; the retained result occupied 656,589,190 bytes and HTML 3,891,782 bytes.
All three models had importance results, with eight primary effects and no
failed effects. The normal 20 permutation repeats and 8-feature/5-model display
limits were preserved. Explanations used 5,000 recorded evaluation rows;
scores and all 1,000 paired bootstrap draws used the complete evaluation set.

All original prediction values survived a fresh-process reload. The portable
`check-final-report.py` then decoded that exact HTML independently in Python:
99 checks passed, including all 84 raw/processed column-partition totals,
complete 20,000-row diagnostic summaries and exact model RMSEs. Individual
column distributions use every row; data relationships use the disclosed
10,000-row limit per partition. The default summary report exports no individual
records. Its shallow model still has RMSE 1.683529; the larger-grid quality
result is a separate case. This verifies a complete bounded workflow, not a
claim that every optional learner or default search is practical on a million
rows. Desktop/mobile interaction is checked separately on the generated HTML.

The raw evidence remains outside the source package at
`~/.cache/autoxplain-scale-0.7.0/`. The [combined index](completed-results.json)
preserves every attempt, including the original failures. Final source binding,
whole-workflow results and cold scope are in [final-workflows.json](final-workflows.json);
the exact HTML decoding checks are in [final-report-payload.json](final-report-payload.json).

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
