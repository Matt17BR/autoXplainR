# Product contract

AutoXplainR turns a data frame and an outcome column into fitted models,
evaluation against a baseline, explanation diagnostics and an optional
HTML report.
[`autoxplain()`](https://matt17br.github.io/autoXplainR/reference/autoxplain.md)
is the entry point. The default requires R and the package’s ordinary
dependencies, with no account or service setup.

A first-time modeler should be able to answer four questions from the
result: what was predicted, whether the primary model beat the baseline,
which fitted patterns were inspected, and what limits the evidence.
Every metric needs a plain definition. Every diagnostic failure needs a
visible reason.

An experienced user should be able to inspect the models, evaluation
rows, training recipe, tuning folds, losses and explanation draws.
Presets are a convenience, not a restriction on inspecting evidence.
Lower-level functions accept existing models through a documented
prediction contract.

The primary model is pre-specified in quick/comparison mode and selected
using training-only resampling in tuned mode. Evaluation ranks never
silently replace it. Grouped validation keeps units intact; temporal
evaluation respects time and ties. Unsupported validation designs must
fail rather than silently use random rows. User-supplied evaluation data
are not automatically described as independent.

The package explains fitted prediction functions. It does not identify
causal effects, certify deployment, or establish that every competitive
model agrees. Uncertainty from permutations, evaluation sampling and
fitting are different and must have different labels. Diagnostics report
their scope, status and affected models or features; they are not
collapsed into an overall quality grade. Automatically selected feature
summaries remain descriptive. Optional generated prose is displayed
separately from numerical results. Hosted responses are checked for
format and length, not factual or numerical grounding; users must review
their claims against the retained evidence.

The contribution we aim to demonstrate is a simple, inspectable workflow
that joins these pieces. We do not claim a unique estimator or
demonstrated superiority to established R packages. See [the development
plan](https://matt17br.github.io/autoXplainR/ROADMAP.md) for
architecture, feature proposals, acceptance tests and the evidence
required for that claim.
