# Scalability findings for 0.7.0

Release verification is still in progress. These measurements identify the
tested workload and candidate snapshot; they do not establish that every
million-row problem or every optional learner is practical.

## What has been measured

| Workload | Published 0.6.2 | Candidate | Scope |
| --- | ---: | ---: | --- |
| Million-row nonlinear regression, two configurations and two folds | 145.331 s | 33.596 s | Fitting, tuning and scoring; explanations disabled |
| Peak process memory for that workflow | 4,911,800 KiB | 2,814,432 KiB | Whole fresh R process, including verification and serialization |
| Uncompressed saved result for that workflow | 903,015,420 bytes | 654,947,875 bytes | Complete retained result, including raw and processed data |
| Million-row rare-event classification, two configurations and two folds | 188.882 s | 70.777 s | Same search budget; automatic native categories change the boosting fit |
| Peak process memory for that classification workflow | 6,125,976 KiB | 3,503,424 KiB | Whole fresh R process |
| Held-out log loss for its primary model | 0.0699033 | 0.0686940 | All 20,000 fixed independent evaluation rows; lower is better |
| Million-row four-class classification, two configurations and two folds | 328.787 s | 200.551 s | Same search budget; native categories change the boosting fit |
| Peak process memory for that multiclass workflow | 5,717,652 KiB | 3,257,168 KiB | Whole fresh R process |
| Held-out log loss for its primary model | 0.9220 | 0.9051 | All 20,000 fixed independent evaluation rows |
| Wide 100,000-row workflow, two configurations and two folds | 126.832 s | 29.549 s | 128 numeric inputs plus factors with 1,000 and 5 levels; native category representation changes boosting |
| Peak process memory for that wide workflow | 6,688,820 KiB | 2,368,584 KiB | Whole fresh R process |
| Held-out RMSE for its primary model | 1.8239 | 1.7454 | All 20,000 fixed independent evaluation rows |
| Wide million-row workflow under 420 s and 12 GiB address space | Timed out before returning a result | Failed during final fingerprint serialization | Intermediate candidate; complete acceptance awaits the fingerprint repair and full rerun |
| Larger fixed grid on 10,000 nonlinear training rows | Not run | RMSE 0.9101; 4.129 s | Three configurations, two folds; CV chose 300 boosting rounds at depth 4 |
| Same larger grid on one million nonlinear training rows | Not run | RMSE 0.7491; 109.012 s | CV chose 600 rounds at depth 6; full fitting and all 20,000 evaluation rows |
| Ordinary core search on 10,000 nonlinear training rows | RMSE 1.6988; 49.104 s | RMSE 0.7475; 82.588 s | Fit-only, explanations disabled; same 20,000-row holdout; larger neural iteration budget |
| 200,100 explicitly exported records, three numeric columns | 121.66 MB, 85.319 s | 17.56 MB, 5.064 s | Full HTML rendering from the same supplied model |
| Wide report, 500 inputs and all 1,200 records | 59.88 MB, 16.277 s | 15.85 MB, 15.094 s | Full HTML rendering from the same saved fit |

The million-row fitting comparison uses the immutable `candidate-source-v1`
installation. It fits all one million training rows and scores a separately
generated 20,000-row holdout. A trace of the actual XGBoost training calls records
500,000 rows in each fold and 1,000,000 at refit. Every holdout prediction from
all three retained models agrees with 0.6.2, as do fold losses, fit seeds,
preprocessing and selection. New computation metadata is compared separately.
A fresh R session also compared 64 original saved prediction probes per model
and reproduced each model's loss over all 20,000 holdout rows.

The rare-event comparison also fits all one million training rows. The new
categorical policy uses 22 native input columns in both 500,000-row folds and
the million-row refit; the previous numeric expansion used 55 columns. Its
boosting predictions change, while the regularized and intercept-only losses
remain identical. Both versions passed fresh-session checks of 64 saved raw
prediction probes per model and complete holdout losses. The probes also cover
reversed factor-level order and tested new/missing categories.

The four-class workflow uses `candidate-source-v3`. It also fits every training
row, using 22 native columns instead of 55 expanded columns for boosting. Its
regularized and intercept-only losses are unchanged, every configuration
completed, and both versions passed fresh-process checks of 64 saved probability
matrix rows per model and all 20,000-row holdout losses. Its uncompressed saved result fell from 970,841,120 to
705,542,148 bytes. The generating-probability reference has log loss 0.8550;
the small search still leaves room for better fitting.

The wide 100,000-row control uses `candidate-source-v4`. Actual XGBoost calls
used 50,000 rows in each fold and 100,000 at refit, with 130 native input columns
instead of 1,131 numeric matrix columns. Both configurations succeeded, and the
regularized and intercept-only holdout losses are unchanged. The saved result
fell from 568,919,930 to 412,320,401 bytes. Its cold check compared 64 saved raw
prediction probes per model and all 20,000-row losses, including the tested
factor reordering and new/missing input cases.

The first wide million-row pair did not complete. The published package spent
370.742 seconds in exact row-overlap serialization, then entered its first
regularized fit attempt before the 420-second guard terminated it. No boosting
training call was reached. Its last recorded high-water mark, 12,274,016 KiB,
is a lower bound because GNU `time` could not write a final record. The candidate
completed the tuning stage and an actual million-row boosting fit, attempted
the regularized refit, and advanced through evaluation, but failed allocating
the final serialized fingerprint buffer. Final per-family statuses were not saved.
Its process ended after 271.114 seconds at a measured peak of 12,257,676 KiB.
Neither run returned a public result, so neither has an accepted holdout score
or cold replay. The exact fingerprint allocation is being repaired; the full
workflow must be repeated under the same bounds before claiming success.

The larger regression grid was declared before either evaluation. It adds
300-round/depth-4 and 600-round/depth-6 boosting configurations at learning rate
0.05, alongside one regularized configuration. All three completed at both
training sizes. At 10,000 rows, the simpler 300-round/depth-4 configuration was
within one standard error of the lowest CV loss, so the existing selection rule
preferred it. At one million rows, CV selected 600 rounds at depth 6.
The selected million-row booster's RMSE of 0.749065
is close to the observed-input conditional-mean reference of 0.727177, versus
1.683529 for the shallow timing control. The larger-grid process peaked at
2,895,452 KiB and saved 657,678,928 bytes. Actual native traces show both
500,000-row folds for each boosting configuration and the million-row refit.
Both larger-grid results passed cold checks of 64 saved prediction probes per
model and complete holdout losses. This demonstrates useful capacity on the
specified nonlinear problem; it is a separate search from the shallow control,
and no result from the independent holdout was used to choose its grid.

An extended cold check passed for all 11 previously replayed cases. It compared
the original public result's complete primary and baseline numeric predictions
or binary probabilities on all 20,000 rows. For multiclass, every retained
class label and primary confidence also matched. Historical alternate models
still have only 64 saved raw probes plus complete holdout loss checks; their
original full probability matrices were not saved. The revised runner saves all
models' complete holdout vectors before serialization for the final wide and
one-call runs. Its small independent harness control passed a cold comparison
and rejected one changed alternate-model probability outside the saved probes.

The report comparisons isolate export and browser work. Their large numeric
fixture uses a supplied linear model fitted to 100 rows. It is separate from
the million-training-row experiment. A complete export of 1,000,100 records
rendered in 22.400 seconds and produced an 83.35 MB HTML file. Compression makes
large exports cheaper; it does not make their information or browser-memory
cost disappear. The default report exports aggregate summaries, and explicit
record export defaults to a 5,000-row limit.

These are individual runs on one machine, not estimates of a universal speedup.
The report measurements use the frozen reporting source dated 2026-09-12;
its manifest is recorded with the measurements.
Full-render timings also include the new default explanation row cap. They do
not isolate compression or serialization alone; preparation and payload sizes
are recorded separately in the [report measurements](reports/README.md).
Both versions omit paired score intervals in these report benchmarks. The tall
fixture requests one explained feature and two permutation repeats; the wide
fixture reuses its saved audit. These are measured export workloads, not default
end-to-end reporting times.
Final release acceptance must repeat functional checks on the released files.

## Why it became cheaper

The published overlap check serialized every complete training and evaluation
row. On the million-row fitting control it consumed 97.343 seconds. The new
check refines exact shared groups one column at a time and removes impossible
matches early. It agrees with the published equality behavior on 307 independent
mixed-type cases. Isolated candidate million-row checks took 0.236 seconds for
disjoint partitions and 0.181 seconds with five injected overlaps. Those short
component timings are distinct from the whole-workflow comparison above.

Tuning now keeps one prepared fold at a time and requests one validated
prediction batch per fit. It avoids computing unrelated metrics and avoids
constructing case records when `retain_oof = FALSE`. Redundant native fitting
calls previously embedded complete inputs and outcomes in saved models.
Removing that payload preserves the native fits and predictions; on the
million-row regularized fixture the old call contained a 304 MB sparse matrix
and an 8 MB outcome vector. Forest and MARS calls had the same avoidable payload
and now retain executable settings with symbolic input arguments. The neural
adapter removes its duplicated encoded inputs and outcomes in the same way.

Reports store columns once, share exactly identical raw and processed values,
compress large blocks and decode columns locally as needed. The wide report's
largest gain is file size and allocation; its rendering time improved modestly.
The tall, full-record report previously paid much more to construct and serialize
one R object per row, so its rendering improvement is larger.

The first compressed million-record export still stalled in WebKit's HTML
parser, before report code ran. Keeping large embedded text in smaller inert
script blocks avoids that stall and reconstructs the exact JSON before use.
The browser removes those temporary blocks after assembly. The original timeout
and the successful boundary, data-fidelity and browser checks remain in the
[report evidence](reports/README.md).

The review also found repeated work in paired score intervals. Each bootstrap
draw rebuilt row lists and recalculated the same individual losses. Caching
those losses and drawing observation indices directly preserves the original
resampling and arithmetic. Independent literal-row and frozen-function checks
agree exactly. On 20,000 evaluation rows, the unchanged 1,000-draw eight-class
Brier calculation fell from 5.183 to 0.838 seconds; regression fell from 1.039 to
0.671 seconds. The Brier process used slightly more peak memory. Larger cases
and the final one-call report are still pending; see the
[interval computation evidence](uncertainty/README.md).

## Choices that can change results

Exact implementation changes are separate from new computation policies.
Automatic additive fitting can choose continuous BAM instead of GAM, with its
solver fixed for each candidate throughout cross-validation and refitting.
The coefficient-work shortcut applies to Gaussian regression. Binary problems
below 10,000 training rows retain GAM after training-fold checks exposed
nonconvergence in BAM's iteratively weighted procedure. The large-data row
threshold remains a computation policy, not a guarantee of convergence.
Automatic boosting can use native categorical splits for large contrast
expansions. Both choices are recorded, can change predictions, and have explicit
overrides. The [solver comparison](search/README.md) retains counterexamples,
including slower BAM fits and materially different discrete-BAM predictions.

Default report explanations use a bounded uniform evaluation sample. Pairwise
data summaries have a separate sample limit. Model scores and univariate
summaries still use all available rows. A constructed ten-row rare cluster was
missed by seven of thirty 10,000-row samples; exact counts still showed it.
The [data study](data/README.md) records this limitation and the all-row control.
PDP retains a separate curve-row limit, independently of its support sample.

The two-configuration million-row control underfits the nonlinear generating
process: test RMSE is about 1.68, with generating noise SD 0.7 before missingness.
More training rows alone did not repair insufficient model capacity. Larger
boosting configurations, rare outcomes, multiclass outcomes, width and category
count are separate acceptance cases under the [million-row protocol](million/README.md).

The ordinary core search had another, distinct problem. In a fixed five-fold
training-only check, only six of 25 neural fits converged within the old 500
iterations. All 25 converged with 2,000, including wider networks with much
lower validation error. Individual fold losses could still worsen. The new
default exposes that larger allowance as `maxit`, stops early on convergence,
and continues to exclude unsuccessful fits. Changing this budget can change
the selected model. A separate 500-iteration replay preserves native weights
and predictions exactly; the [neural evidence](search/README.md) records both
the modeling change and that compatibility control.

The ordinary core `autoxplain()` search on the same 10,000 training rows, with
explanations disabled, then selected six hidden units with weight decay 0.01.
All 15 configurations completed across every fold, compared with four neural configurations failing
under the former limit. Independent scoring on the untouched 20,000-row holdout
gave RMSE 0.7475 instead of 1.6988. The known conditional mean after respecting
missing inputs scored 0.7272; the generating mean with complete, unobservable
inputs scored 0.6993. The [conditional reference](million/conditional-reference.R)
uses the fixture's independent, mean-zero interaction input and is never used
for model selection. Runtime increased from 49.104 to 82.588 seconds. This is evidence
of better fitting on this problem at a higher cost, not a neural speedup or a
claim that the core search is practical at one million rows.
Both versions passed cold-session checks of 64 saved prediction probes per model
and complete 20,000-row holdout losses. The uncompressed
result grew from 39.36 to 41.77 MB because more configurations now have valid
retained out-of-fold predictions, despite the smaller native fitting calls.

## Independent review and remaining acceptance

The [independent review](independent-review/README.md) challenged sampling,
prediction types, search/refit consistency and retained explanation scope.
It found defects that the initial passing tests missed. Repairs are tied to
reproducing examples rather than only implementation-shaped assertions.
The [streaming replay](streaming-verdict.json) covers 4,320 out-of-fold records
across six task/metric workflows. Settings, fold scores, selections, probabilities
and final predictions agree exactly with the published version when both use
the former 500-iteration neural budget. Its new explicit setting is checked
before comparing the remaining metadata. Binary case
losses have an explicitly verified correction to match CV clipping and
arithmetic; that intended repair is separate from the execution optimization.

Outstanding acceptance includes the final recommended-search timings,
remaining large fitting cases, the complete three-browser CI gate, and the
complete source/archive and publication gates. The ordinary result deliberately
retains training and evaluation evidence; it is larger than an inference-only
model. Its memory and artifact sizes remain part of the practical limit.
