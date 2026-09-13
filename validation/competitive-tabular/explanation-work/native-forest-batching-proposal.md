# Native forest batching assessment

Status: deferred. No batching optimization was implemented.

The existing native inference probe found that predicting five separate
5,000-row perturbation batches in one 25,000-row call reduced elapsed time by
2% to 38% across three saved Year Prediction forests. All resulting predictions
were identical. These are descriptive two-thread measurements under concurrent
host work, not a promise of the same gain on other forests.

The narrow candidate design would combine at most five consecutive repeats of
one feature group, with a 25,000-row ceiling and a separate memory ceiling.
Predictions would then be split into their original ordered row blocks and
scored separately. Detailed importance would retain 20 repeats and 5,000 rows;
screening would retain five repeats over every input.

Only package ranger regression or probability forests using the exact native
adapter could qualify. A backend label alone is insufficient. Custom prediction
functions, overridden S3 methods, class-label forests with random tie-breaking,
and custom data-frame or column behavior would keep the scalar path. The valid
callback `function(newdata) newdata$x - mean(newdata$x)` already demonstrates why
combining arbitrary batches changes the result.

The native RNG boundary prevents treating this as an exact optimization today.
In ranger 0.18.0, `predict.ranger.forest()` draws one `runif()` seed on every call
when no explicit seed is supplied, including regression and probability forests.
The current importance loop alternates a permutation with a prediction call.
Generating five permutations followed by one prediction would therefore change
later permutations even when each forest probability is row independent.

Reproducing the engine's internal RNG consumption would couple the package to
its implementation. Changing the prediction seed or permutation schedule would
change retained repeat arrays. Neither is justified by the modest timing probe.
RMSLE adds another boundary: a failed repeat stops that feature, so speculative
permutations must not consume random numbers that the scalar path never used.
Batch failure handling would also need to preserve the first failing block and
its original error record.

The implemented report optimization instead reuses existing validated full
predictions and class columns on identical row batches. It leaves every
permutation call intact. Its complete native evidence comparison is recorded in
[the parity results](parity-README.md), and its measured operation counts are in
[the work audit](README.md).

Any future batching experiment should first define its RNG contract openly,
then compare complete repeat matrices, sampled rows, feature groups, blocked
permutations, all three tasks, reversed binary event classes, failure records
and caller RNG state. It must also prove that custom or overridden adapters
never enter the native path. Timing evidence alone is insufficient.
