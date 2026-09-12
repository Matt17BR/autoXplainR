# Adversarial review of explanation limits and large-data model inputs

The review found eight concrete sampling or model-input defects during the
0.7.0 work, then an adjacent evaluation task-inference failure. All nine were
corrected in the reviewed source. The [versioned verdict](verdict.json) records
their status and reviewed source hashes. This is a bounded review, separate from
the complete package checks and large-data measurements.

| Original failure | Correction checked |
| --- | --- |
| Supplying an audit bypassed an explicit cap for newly generated curves. | New curves use the explicit cap; supplied evidence remains supplied evidence. |
| A sample missing one binary class aborted an AUC audit. | Full-evaluation AUC remains available; sampled permutation AUC is marked unavailable. |
| A second draw with the same seed from a sorted first sample biased PDP row selection. | The final curve sample comes directly from original rows; support uses its separately recorded sample. |
| Boosting could use matrix contrasts in smaller CV folds and native category partitions at full refit. | An outer-training resource plan fixes representation across the search; learned levels and preprocessing remain inside each fold. |
| Native boosting silently coerced changed predictor kinds. | Prediction rejects incompatible numeric, logical and categorical inputs. |
| A PDP caption gave the curve's smaller row count as the support denominator. | The two counts are recorded and displayed separately. |
| A class absent from the entire evaluation set prompted advice to raise the sample cap. | The report distinguishes missing sampled observations from missing evaluation observations. |
| Retained implicit curves survived a changed explicit cap when an audit was supplied. | Implicit curves are regenerated; explicitly supplied effects remain unchanged. |
| Automatic task detection treated a binary factor with one observed class as multiclass. | Evaluation uses declared factor levels; training continues to use observed classes. |

The independent PDP replay checks 100 seeds against actual original row
identities, analytic means and descriptive standard errors, independently
calculated support, unchanged full-reference fingerprints and caller RNG state.
Its observed sample frequencies are descriptive, without a stochastic passing
threshold. The rare-class replay checks full AUC, unavailable sampled AUC and
the two distinct missing-class explanations through public functions. The final
task-inference tests also verify multiclass probabilities, preserved training
behavior with unused factor levels, explicit task overrides and rejection of
full-evaluation AUC when one class is absent.

The fixed encoding plan was reviewed through production data flow and the
separately authored workflow test that crosses the resource threshold between
folds and refit. That workflow was not repeated as part of this small review.
No claim of predictive equivalence between contrast encoding and native category
partitions is made. They are different model choices, now kept consistent within
each search.

Original counterexamples and before/after logs remain under
`~/.cache/autoxplain-scale-0.7.0/independent-review/`:

- `explanations.R` and `explanations-after.log` cover supplied audits, PDP
  support and rare-class AUC. Its final helper-level encoding comparison still
  differs by row count, deliberately: the fix freezes the search plan rather
  than changing the resource estimator itself.
- `boosting-types.R` and `boosting-types-after.log` reproduce changed-kind inputs.
- `retained-cap.R` and `retained-cap-after.log` show a retained 50-row curve
  being replaced by a new three-row curve under an explicit cap.
- `final-explanations.R` and `final-explanations.log` contain the final analytic
  and rare-class replay.

These R scripts run from the repository root with the package dependencies
installed. The relevant regression tests ship in `test-explanation-sampling.R`,
`test-boosting-native.R` and `test-task-inference.R`. The separate
[compact-export review](../reports/independent-review/README.md) includes its
portable R and Python replay scripts.

The review does not make the fixed-model bands population confidence intervals.
They remain descriptive and do not include row sampling, fitting or model
selection uncertainty. A bounded sample can still miss rare groups.
