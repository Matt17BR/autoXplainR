# Final 256-tree development diagnostic

Declared on 13 September 2026 before fitting either diagnostic forest. This
is development evidence for `forest-tree-budget-v3`, not acceptance scoring or
a complete public workflow benchmark. No acceptance partition may be opened.

Use the completed frozen `candidate-paired-4t-v4-recovery-20260913` Year and
Covertype package results. Preserve these original results. Their retained
500-tree forests were fitted on all 50,000 development training rows, with
training-selected settings: Year mtry 30/node size 20; Covertype mtry 18/node
size 5; sample fraction .8 and standard splits. The source search seed is
80711. Verify the recorded parameter-derived final seeds, native tree and
sample counts, requested/effective settings and saved prediction identities.

For each case, change only the planned tree count to 256, derive the native
seed with the unchanged production `stable_configuration_seed()` function,
and fit through the production native forest path using all 50,000 processed
training rows. Do not select a seed or parameter after observing this probe.
Both fits use four native threads and final OOB diagnostics. Freeze source,
protocol, scripts, source-model hashes and development partition hashes before
the first fit. Preserve the original preprocessing and document its effective
predictor count; Covertype's constant development `soil_15` column was already
removed by that training recipe. No further feature removal is introduced.

Covertype's 50,000-by-54 raw development pool is below the four-million final
budget threshold. Its 256-tree fit is an explicit diagnostic of tree count,
not the ordinary default on that development pool or a full-Covertype fit.
The 500-tree models are reused only after verifying exact settings and seeds.
Older native importance probes used different models or seeds and therefore
cannot substitute for these comparisons.

Assess complete predictions on the already used 20,000 development assessment
rows. Record RMSE/MAE/R-squared or floored log loss/Brier/accuracy, complete
confusion matrices, class denominators and recalls, zero-probability counts,
true-class zero counts, and unbounded log loss where defined. Save complete
predictions and model objects. In a fresh R process compare saved wrapper
predictions and native ranger predictions over every assessment row, with a
maximum absolute difference of 1e-12. Record fit time, saved model bytes, native
node/tree/sample counts and repeated 5,000-row prediction times. Timings from
shared-host processes are observations, not isolated algorithmic speedups.

Use exactly the same 5,000 uniformly sampled assessment rows for every
importance computation: sample seed 80711. Screen every effective input of
each retained model with five repeats and permutation seed 80711. This includes
the unchanged boosted model and simple baseline, plus both forest versions.
Record all screening repeat arrays, feature scores and sampled class counts.

Form the ordinary report feature union separately for the 500-tree and
256-tree model sets, using the eight leading inputs of each retained model.
Keep both complete unions. The common detailed comparison set is their union,
plus up to two borderline inputs per forest outside that set, chosen by the
smallest absolute screening importance divided by its shuffle standard error
(finite positive standard error required; deterministic feature-name ties).
This rule is fixed before screening. It gives influential and borderline
inputs twenty repeats without discarding any input selected by either report.
Use permutation seed 80712 for both forest detailed passes, matching the second
model's audit position, and the same sample seed 80711 and 5,000 rows. The added
borderline inputs are a diagnostic extension, not a modified package default.

Compare complete detailed repeat arrays, effect magnitudes, signs, shuffle
standard errors/intervals, top-eight/top-sixteen overlap and inputs lost from
either ordinary report union. Shuffle intervals condition on these sampled
rows and fixed forests; they do not include row-sampling, fitting-seed or
training-sample uncertainty. Retain the known prefix warning that near-equal
Covertype aggregate loss accompanied fewer correct cottonwood classifications.
No result automatically establishes forest readiness or final equivalence.

Each fitting process has a 1,200-second wall limit and 24-GiB address-space
limit. Each replay, individual-model screening or individual-forest detailed
stage is also independently bounded at 1,200 seconds and 24 GiB. These stage
limits are diagnostic resource controls, not extra allowances for acceptance:
their sum must not be presented as a successful 1,200-second or 7,200-second
one-call run. Preserve failed or interrupted attempts; never overwrite them.
Only one diagnostic stage runs at a time. A separately supervised four-thread
native reference may share the host; record overlapping processes and do not
claim isolated timings. The unchanged full-scale quality, complete-report and
saved-replay acceptance gates remain required after this diagnostic.
