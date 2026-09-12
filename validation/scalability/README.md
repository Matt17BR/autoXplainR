# Scaling work after 0.6.2

This work addresses three measured limits: the recommended search took 533
seconds on the retained nonlinear regression problem, a wide report reached
59.9 MB, and the release had no million-row acceptance evidence.

The work is in progress. Measurements belong to the exact source and runtime
recorded alongside them; this file is not a claim that a release has passed.

## Acceptance criteria

- Repeat the same recommended searches with the same partitions, candidate
  budgets and selection rules. Separate exact implementation improvements
  from solver approximations. Keep adverse predictions, losses and failures.
- Stream prepared folds and remove redundant prediction/scoring work. Compare
  every retained out-of-fold prediction, fold loss, selected configuration and
  final prediction against 0.6.2. Check memory in an isolated process.
- Reduce the actual wide report's size substantially while retaining every
  explicitly exported value and source-row identity. Verify offline loading,
  filtering, model selection, records, plots, print output and keyboard use
  across browser engines. A smaller file alone is insufficient.
- Keep model scores and univariate data summaries on all available rows.
  Bound expensive exploration and explanations with reproducible row samples,
  visible counts, an explicit all-row option, and tests of sample provenance.
  Sampling uncertainty must not be presented as shuffle uncertainty.
- Fit and score genuinely million-row training sets, rather than calling a
  sampled fit a million-row fit. Cover nonlinear regression, rare-event binary
  classification, multiclass classification, and width/cardinality stress.
  Record hardware, peak process memory, wall time, search settings, holdout
  metrics, failures, artifact size and browser behavior. Confirm predictions
  after a fresh-session reload.
- Run an independent review of the statistical and resource boundaries, then
  the package, browser, documentation and release-artifact gates. Publish only
  after those gates pass. State the measured workload and remaining limits.

## Architecture under test

Tuning prepares one fold at a time and uses one validated prediction batch for
each fit. Optional additive solvers expose their actual computation method.
Reports use compact column storage, share identical raw and processed values,
and decode data locally. Default report explanations and pairwise data views
use separate, disclosed row limits. Fitting and held-out scoring keep their
complete partitions.

The `million`, `search`, and subsequent evidence directories contain the
reproduction scripts and measurements. Large generated datasets, fitted
objects, reports and raw logs are stored outside the source package.
