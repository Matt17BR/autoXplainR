# Development priorities

AutoXplainR’s purpose is a short, inspectable path from a tabular
prediction question to a baseline comparison, fitted explanations and a
report. Existing packages already provide the underlying estimators and
much of the reporting infrastructure. Integration and reader
comprehension are hypotheses to test.

## 0.6.0: make exploration useful

The [review of
0.4.0](https://matt17br.github.io/autoXplainR/validation/product-review-0.4.0.md)
found that statistical repairs had displaced the package’s main purpose:
comparing models and understanding their behaviour. Version 0.5.0
restored tabs and model controls, but the next review found sparse
charts, missing search rationale and little access to the supplied data.
The [0.6.0 work
record](https://matt17br.github.io/autoXplainR/validation/product-overhaul-0.6.0.md)
tracks the repairs and rejected intermediate designs.

- Make selection inspectable: parameter rationale, candidate and fold
  losses, convergence, the numerical selection rule and successful final
  fits.
- Preserve raw partitions and source positions separately from processed
  model inputs. Let readers inspect distributions, missingness,
  associations and explicitly exported records without losing their
  place.
- Use readable responsive charts, shared comparison axes and direct
  labels. Check plot coordinates, text size and print reflow against
  independent values.
- Add the same workflow for existing fitted models, with explicit
  prediction contracts and no invented training or selection history.
- Compute report predictions once and validate every attached artifact
  against the current model, outcomes, row order and recorded prediction
  context. A result must not mix old scores or explanations with changed
  predictions. Such checks do not prove equivalence on unseen inputs or
  validate hidden external state.
- Retain repeated prediction measurements on common batches. Expose
  timer and budget limitations, raw timings and variation before
  offering a cost comparison.
- Gate publication on executed task walkthroughs, numerical answers
  checked against R, keyboard/mobile checks, actual screenshots and
  printed views. Record concrete failures and repairs, not just a test
  total.
- Preserve the corrected statistical contracts from 0.4.0. Test ranking
  is descriptive; training CV chooses the default. Associations remain
  distinct from causal effects and sampling uncertainty.

See the [release
checklist](https://matt17br.github.io/autoXplainR/RELEASE_CHECKLIST.md).
A participant study can add independent evidence, but ordinary task
acceptance must happen now.

## Next: interoperability and demonstrated usefulness

1.  **Framework-specific adapters.**
    [`evaluate_models()`](https://matt17br.github.io/autoXplainR/reference/evaluate_models.md)
    accepts native models and explicit prediction functions. Dedicated
    tidymodels and mlr3 adapters still need examples covering embedded
    recipes, probability-column order, new factor levels and
    serialization. Prefer these over more learner families.
2.  **User comprehension.** Compare the complete workflow with
    equivalent DALEX/modelStudio and tidymodels workflows on identical
    data and partitions. Ask users to choose a split, identify the
    positive class, interpret an importance interval and locate a failed
    check. Publish task completion, interpretation errors and time, not
    only code length.
3.  **Report accessibility.** Exercise keyboard order, contrast, table
    semantics, mobile layouts, print output and screen readers.
    Automated checks complement manual review; attractive screenshots do
    not establish accessibility.
4.  **Compatibility and performance.** Extend the retained schema and
    raw-data tests with wide/tall memory profiles and old-result
    migration fixtures. Report preparation and benchmarking have
    different scopes; test their call counts and resource costs
    separately before adding parallel execution.

## Deferred until the current workflow meets those criteria

Rolling-origin tuning, nested evaluation, conditional-importance
samplers, refit bootstraps, conformal prediction and model-class
coverage are separate statistical projects. They require explicit
estimands, independent reference implementations, simulation plans and
review. They are not commitments for the next release. Additional
learner families and hosted-provider catalogs are also deferred unless a
demonstrated user task requires them.

A methods manuscript or a 1.0 claim needs broader simulation coverage
and independent statistical review. CRAN acceptance, a passing test
suite and a GitHub release do not establish comparative usefulness or
universal validity.
