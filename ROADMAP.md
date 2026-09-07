# Development priorities

AutoXplainR's purpose is a short, inspectable path from a tabular prediction
question to a baseline comparison, fitted explanations and a report. Existing
packages already provide the underlying estimators and much of the reporting
infrastructure. Integration and reader comprehension are hypotheses to test.

## 0.5.0: restore model exploration

The [review of 0.4.0](validation/product-review-0.4.0.md) found that statistical
repairs had displaced the package's main purpose: comparing models and
understanding their behaviour. The current work restores that purpose.

- Make a small training-CV search across linear, tree and neural models the
  one-command default. Keep a fast reference model available explicitly.
- Replace the long report with focused tabs. Compare scores and costs first;
  let the reader switch models, choose an input, inspect relationships and
  examine prediction errors. Put background explanations in accessible help.
- Audit the inputs used by each retained model and connect the displayed
  importance bars to its fitted curves. Keep failures explicit.
- Gate publication on executed task walkthroughs, numerical answers checked
  against R, keyboard/mobile checks, actual screenshots and printed views.
  Record concrete failures and repairs, not just a test total.
- Preserve the corrected statistical contracts from 0.4.0. Test ranking is
  descriptive; training CV chooses the default. Associations remain distinct
  from causal effects and sampling uncertainty.

See the [release checklist](.github/RELEASE_CHECKLIST.md). A participant study
can add independent evidence, but ordinary task acceptance must happen now.

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
