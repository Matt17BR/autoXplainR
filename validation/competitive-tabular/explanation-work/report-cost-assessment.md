# Remaining report cost

This assessment uses the completed development probes and static code review.
It adds no large model fit and opens no acceptance targets. All timings below
refer to the saved 500-tree Covertype forest trained on 50,000 rows, using two
prediction threads. They do not measure a forest fitted on the full 464,810-row
training partition.

## What the measured time includes

Screening all 54 inputs with five permutations on 5,000 rows took 493.131 seconds.
Three subsequent ordinary prediction calls on the same 5,000 reference rows
took 1.791, 1.838 and 1.830 seconds. The median multiplied by the 270 required
screening predictions is 494.1 seconds. These are different calls under shared
host load, so this is an attribution check rather than an additive profile. It
strongly indicates that repeated prediction dominates the screen.

The timing includes `predict.autoxplain_explainer()`, input selection, the native
ranger call, probability normalization and validation. It does not isolate
native tree traversal from native forest reconstruction, R-to-C++ conversion or
adapter work. Static review shows no fingerprint calculation inside the
permutation loop. The diagnostic used the earlier public worker with two full
prediction/identity calls outside that loop; the private report path now reuses
those values.

The 1,000-row prediction median was 0.997 seconds, versus 1.830 seconds for five
times as many rows. This suggests meaningful fixed prediction cost, but these
six calls cannot identify its cause or give a reliable linear cost model.
Ranger passes the stored forest representation to native prediction on each
call. There is no reusable compiled forest handle in the package's current
adapter. Input conversion and validation also recur, but the measured timings
do not justify claiming a particular percentage for any one component.

## Default work after exact reuse

With p inputs and U inputs in the union selected across reported models, each
model needs `5*p + 20*U` permutation predictions. For this forest, p is 54.
Using the observed 1.830-second median gives these conditional estimates:

| Union inputs U | Prediction calls for this forest | Estimated prediction time |
| --- | ---: | ---: |
| 8 | 430 | 13.1 min |
| 16 | 590 | 18.0 min |
| 32 | 910 | 27.8 min |

These are prediction costs for one forest, not complete report or one-call
times. The completed package benchmark cohorts had explanations disabled and
therefore do not provide actual multi-model union counts. The eight leading
inputs of one native forest cannot stand in for that union.

For eight successful numeric effects, shared class predictions add 16 ALE
batches for the forest, about 29 seconds at the same prediction median. The
seven classes no longer multiply these batches by seven. Two full-reference
context predictions per model, model identities, support/dependence diagnostics,
other reported models and HTML preparation add work. Categorical effects have a
different grid size and may use fewer reference rows.

The much larger full-training forest can have many more nodes and greater fixed
prediction cost. Its report time cannot be inferred from training row count or
the 50,000-row forest's prediction median. Consequently these measurements do
not establish the complete workflow's 7,200-second acceptance limit.

## Remaining exact opportunities and boundaries

The main exact savings are already implemented: full prediction and identity
reuse within one calculation, and sharing probability columns for identical
effect row batches. Permutation probabilities already include all classes in
one prediction, so there is no further per-class duplication in that loop.

Screening and detailed importance generally do not generate the same
perturbations. Their feature orders differ, their repeat counts differ, and
audit seeds vary by model. Reusing screening scores would change the existing
statistical calculation. Even caching an accidentally repeated row permutation
would have to preserve the native predictor's RNG side effects.

[Native perturbation batching](native-forest-batching-proposal.md) remains
deferred because ranger's per-call random seed draw affects subsequent shuffles.
The measured 2% to 38% batching opportunity does not justify imitating private
engine RNG behavior or silently changing retained repeat arrays.

A smaller remaining candidate is sharing deterministic support, bin geometry
and pairwise input-association calculations across identical reference data,
features and effect classes. That needs a profile and exact attribute checks;
it cannot remove the dominant 270 screening predictions. No such additional
cache was implemented in this assessment. No row or repeat budget was reduced.
