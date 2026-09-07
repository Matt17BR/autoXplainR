# Development priorities

AutoXplainR's purpose is a short, inspectable path from a tabular prediction
question to a baseline comparison, fitted explanations and a report. Existing
packages already provide the underlying estimators and much of the reporting
infrastructure. Integration and reader comprehension are hypotheses to test.

## 0.4.0: correct and explain the current workflow

This release follows the [0.3.0 audit](validation/audit-0.3.0.md). The release
record must identify which checks actually ran; implementation alone is not
acceptance evidence.

- Use the numerical feature grid in report graphics, label axes and prediction
  targets, and retain categorical tables and unavailable effects.
- Replace overall explanation grades with scoped diagnostics. Keep shuffle
  behavior, limited association screening, evaluation uncertainty and model
  disagreement distinct. A missing check must not look successful.
- Render HTML and local narratives from the same retained evidence. Explicit
  narrative overrides replace components without discarding evaluation scores.
  Hosted text has format checks, not guaranteed numerical grounding.
- Preserve prediction identity, validation boundaries and recorded preprocessing
  across adapters. Show when supplied models or rows cannot be compared.
- Teach a complete real-data workflow: choose predictors, exclude future
  information, set outcome levels, choose a split, inspect the recipe, evaluate,
  predict and share. Move specialized tasks into focused articles.
- Deprecate duplicate dashboard entry points with replacements and warnings in
  0.4.0; keep compatibility until at least 0.6.0. Separate GitHub publication from
  optional CRAN submission and record manual checks explicitly.

Acceptance requires meaningful regression tests for the reported failures,
executed tutorials, numerical reference checks, rendered report review and
package checks of the exact release archive. See the
[release checklist](.github/RELEASE_CHECKLIST.md).

## Next: interoperability and demonstrated usefulness

1. **Existing-model adapters.** Document tested prediction contracts for common
   tidymodels/DALEX/mlr3 workflows, including preprocessing, probability-column
   order and raw-row prediction. Prefer adapters over additional learner families.
2. **User comprehension.** Compare the complete workflow with equivalent
   DALEX/modelStudio and tidymodels workflows on identical data and partitions.
   Ask users to choose a split, identify the positive class, interpret an
   importance interval and locate a failed check. Publish task completion,
   interpretation errors and time, not only code length.
3. **Report accessibility.** Exercise keyboard order, contrast, table semantics,
   mobile layouts, print output and screen readers. Automated checks complement
   manual review; attractive screenshots do not establish accessibility.
4. **Compatibility and performance.** Publish result-schema migration rules and
   aggregate JSON fixtures. Measure prediction-call counts, runtime and memory
   on wide/tall data before adding batching or parallel execution.

## Deferred until the current workflow meets those criteria

Rolling-origin tuning, nested evaluation, conditional-importance samplers,
refit bootstraps, conformal prediction and model-class coverage are separate
statistical projects. They require explicit estimands, independent reference
implementations, simulation plans and review. They are not commitments for the
next release. Additional learner families and hosted-provider catalogs are also
deferred unless a demonstrated user task requires them.

A methods manuscript or a 1.0 claim needs broader simulation coverage and
independent statistical review. CRAN acceptance, a passing test suite and a
GitHub release do not establish comparative usefulness or universal validity.
