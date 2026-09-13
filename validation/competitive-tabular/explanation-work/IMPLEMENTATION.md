# Sharing computed report evidence

The implemented optimization preserves report rows, seeds, screened inputs,
repeats, model comparisons and effect estimators. The native timing measurements
in the sibling forest-policy evidence motivated removing duplicated work while
retaining explanation coverage.

## 1. Pass the validated context to private workers

`prepare_report_context()` in `R/report_preparation.R` already constructs fresh
explainers, reference predictions and fingerprints, and validates stored
evaluation evidence. `render_model_report()` rebuilds this context at each public
render boundary. Use that context through explicit internal worker arguments:

```r
calculate_permutation_importance_impl(..., prediction_context = NULL)
audit_explanations_impl(..., prediction_context = NULL)
explain_effect_impl(..., prediction_context = NULL)
```

Exported signatures retain their independent freshness behavior. A public
function calls its private worker without permission to reuse an old snapshot.
Only `prepare_model_report_data()` and `prepare_report_effects()` pass the current
validated context. Do not expose a reusable authorization object or consult a
global cache. The scope is one active computation, never saved evidence.

The worker can reuse these exact values:

- Importance: full baseline predictions, subset baseline predictions using the
  existing row sample, and the identity attached to the resulting object.
- Audit: full model-agreement predictions and provenance fingerprints.
- Effects: explainer fingerprint and ordered-reference fingerprint.

Validate context/model IDs and reference scope before reuse. A supplied data
replacement must not inherit a context for different rows or outcomes. Context
creation remains a fresh check even when evidence has been serialized. One
completion check per model compares fresh state against the initial snapshot
and rejects changed evidence. This bounds
freshness work without silently accepting mutation during a long computation.

Measured reduction: the fixture's 55/135 full fingerprint-prediction batches
become five completion checks, while context creation supplies five initial full
predictions. The completion guard remains part of the accounting.

## 2. Share class probabilities on identical effect rows

`explain_effect_bundle()` in `R/effect_computation.R` shares full probability
arrays across class-specific calls to the private effect worker. The existing
scalar `calculate_ale_impl()` and `calculate_pdp_impl()` arithmetic remains
unchanged, including uncertainty and support calculations.

- ALE predicts its existing upper and lower data separately, retaining the full
  probability matrices. Each class then summarizes its own local differences
  using the same bins, centering coefficients and standard-error calculations.
- PDP predicts each existing category/grid batch once. Summarize every requested
  class column with the existing mean, standard error and interval calculation.
- Do not concatenate upper and lower data, combine different grid values, alter
  row order or change samples. This preserves batch-dependent custom predictors.

The private effect-bundle helper returns a named list of class-specific
effects. Within one bundle, a prediction is reused only after an exact comparison
of the complete ordered modified data frame. A mismatched batch is independently
predicted. Outside a bundle, cache only completed small curve objects within the current preparation,
keyed by model, feature, method, seed, sample scope and grid settings. Share this
local cache between initial primary curves in `prepare_model_report_data()` and
missing curves in `prepare_report_effects()`. Discard large probability matrices
after deriving the requested class summaries.

Retain explicit primary overrides, explicit empty collections, already retained
secondary curves, wrong-class validation and model/class/feature failure records.
Generate only missing requested curves. A failure in one aggregation must not
erase independently available siblings. The existing collection-management tests
remain useful even if their injection point moves from a public wrapper to the
new aggregation helper.

Measured reduction: five models with three classes need 80 ALE prediction
batches rather than 240, using exactly the same requested row batches and
class-specific estimators. Regression and binary ALE already use two batches per
curve; they benefit mainly from avoiding repeated full-data identities.

## 3. Lexical state reached through partial assignment

The completion check exposed an existing identity-analysis gap:
`codetools::findGlobals()` can treat `state$field <- value` as a local binding and
omit the outer `state` object that it reads. When `state` is an environment, the
assignment also changes shared state. If its influence cancels on observed rows,
baseline predictions alone cannot identify the change.

`prediction_partial_assignment_bindings()` supplements the existing dependency
analysis for roots used in partial assignments. It distinguishes formal inputs,
previously established local bindings and bindings established by both branches
of a conditional. Lazy default arguments are inspected separately from the body,
including defaults on locally defined helpers. Their assignments do not become
assumed body bindings. Removing a local binding with `rm()` or `remove()` can
expose an outer object again. Superassignment skips the current frame but can
resolve a known local binding in an enclosing callback frame.

Non-local removal has an explicit boundary. `rm()` or `remove()` with explicit
`envir`/`pos`, inherited or dynamically chosen search, forwarded arguments, or a
function alias is rejected when binding custom prediction evidence. The scanner
does not claim to follow arbitrary environment removal. Ordinary local removal,
including literal `inherits = FALSE`, remains supported. A nested-helper test
demonstrates why this matters: removing a callback-local binding with inherited
search can expose and mutate outer state while baseline predictions stay equal.

The dedicated tests cover `$`, `[[` and `[` assignment, missing indices, local
shadowing, conditional bindings, zero-iteration loops, default arguments,
binding removal and nested superassignment. Actual report calls reject state
mutations invisible on baseline rows. Serialization tests verify that reloaded
unchanged callbacks keep their identities and retain independent lexical state.
This improves the existing static contract; it is not a general proof of purity
for arbitrary R programs.

## 4. Verification before changing defaults or resource claims

The [native parity harness](parity-README.md) compares complete report evidence
against a frozen copy of the implementation from before this optimization in
separate R sessions. It reuses the same saved native fixtures and excludes only
creation timestamps. Dedicated tests also use independent public computations
and analytic references for focused semantic checks:

1. Small native ranger and XGBoost fixtures covering regression, binary event
   orientation and multiclass column order. Compare every permutation repeat,
   baseline metric, sampled row identity, curve value, standard error, interval,
   support field and class attribute at existing numerical tolerances.
2. An analytic numeric effect with varying local differences, such as
   `x*z + 0.25*x^2`, with tied x and correlated z. Independently calculate local
   differences and bin uncertainties. Include categorical PDP grids, missing
   values, singleton bins and class-specific aggregation failure.
3. The batch-dependent custom callback and counterexample in this directory.
   Shared class computation must keep separate prediction batches; it cannot
   reinterpret deterministic as row-independent.
4. A lexical-state sentinel `d$x + state$scale*(d$z - d$x^2)` evaluated where
   `z=x^2`. Changing `scale` preserves baseline predictions while changing
   perturbed predictions. A fresh render must still reject its stale evidence.
5. Mutated native coefficients, changed outcome, reordered rows, a replaced
   prediction callback, serialization/reload, a stale secondary curve hidden
   beside an explicit primary override and an explicit empty effect collection.
   Existing tests cover several of these and should remain behavioral checks.
6. Sampling/seed restoration, RMSLE domain failures, feature-specific error
   attribution and classification probability validation. Reuse cannot conceal
   an invalid sibling prediction or change the uncertainty scope.

Instrument native and custom prediction entry points to verify fewer calls only
after numerical and freshness checks pass. Count assertions should express
bounded duplicate work, not lock every wrapper call forever. Ordinary tests
should not assert noisy elapsed times. Repeat the native timing probe afterward
with matched rows, settings, thread counts and source hashes.

Sharing data-only feature associations on the same ordered sample is a separate
exact optimization. Pairwise missingness matters: globally precomputed ranks can
change pairwise-complete Spearman correlations. Reducing permutation coverage or
reusing screening shuffles belongs to a later statistical protocol change.
