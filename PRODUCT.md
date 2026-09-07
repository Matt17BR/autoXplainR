# Product contract

AutoXplainR turns a data frame and an outcome column into fitted models,
evaluation against a baseline, explanation diagnostics and an optional HTML
report. `autoxplain()` is the entry point. The default requires R and the
package's ordinary dependencies, with no account or service setup.

A first-time modeler should be able to compare the fitted models, see which
inputs they use, inspect related inputs and find prediction errors. Scores and
plots belong beside the controls that change them. Short help belongs on hover,
focus or tap; full method details belong in their own tab or disclosure.
A control earns its place by changing useful evidence. Decorative grades,
repeated generic conclusions and dead controls do not belong in the report.

The default compares linear, tree and neural families using training-only
cross-validation, then evaluates the retained fits and baseline on held-out
rows. Quick mode provides the previous fast reference workflow explicitly.

An experienced user should be able to inspect the models, evaluation rows,
training recipe, tuning folds, losses and explanation draws. Presets are a
convenience, not a restriction on inspecting evidence. `evaluate_models()` gives
existing fitted models the same report through explicit prediction contracts,
without claiming that AutoXplainR trained, tuned or timed them. A training table
may be unavailable; the report must say so.

Model selection must expose why the search contained particular settings, which
fold results support its choice, which configurations failed and what was
successfully refitted. General parameter advice cannot substitute for these
recorded facts. Repeated benchmarks use common batches and retain raw timings;
single millisecond readings cannot establish a speed ranking.

The result retains supplied and processed values, partition membership and
original input-table positions. The Data tab exports aggregate profiles by
default. Individual records require explicit export and retain their source
identity after filtering, splitting or row removal. Row filters operate on the
exported sample; full-data summaries and model scores must remain distinguishable.
Hiding a record in a view never removes it from HTML. Aggregate output can still
reveal sensitive labels or small groups, and export controls do not redact fitted
model details or feature explanations.

The primary model is pre-specified in quick/comparison mode and selected using
training-only resampling in tuned mode. Evaluation ranks never silently replace
it. Grouped validation keeps units intact; temporal evaluation respects time and
ties. Unsupported validation designs must fail rather than silently use random
rows. User-supplied evaluation data are not automatically described as independent.

The package explains fitted prediction functions. It does not identify causal
effects, certify deployment, or establish that every competitive model agrees.
Uncertainty from permutations, evaluation sampling and fitting are different and
must have different labels. Diagnostics report their scope, status and affected
models or features; they are not collapsed into an overall quality grade.
Automatically selected feature summaries remain descriptive. Optional generated
prose is displayed separately from numerical results.
Hosted responses are checked for format and length, not factual or numerical
grounding; users must review their claims against the retained evidence.

The contribution we aim to demonstrate is a simple, inspectable workflow that
joins these pieces. We do not claim a unique estimator or demonstrated superiority
to established R packages. See [the development plan](ROADMAP.md) for remaining
work and the evidence needed to assess reader comprehension and usefulness.
