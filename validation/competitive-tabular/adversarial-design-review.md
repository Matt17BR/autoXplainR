# Review of the tabular automation design

Reviewed against the published 0.7.0 architecture on 12 September 2026. This
document records design requirements, not completed product acceptance.

The principal weakness is search allocation. Native XGBoost and ranger are
already present, but a fixed prefix of short boosting fits and 500-tree forest
fits does not adapt to a difficult problem. Changing chart styles or adding
more preset tuples would leave that limitation largely intact.

## Decisions worth implementing

* Keep the dependency-light core workflow stable. Add a clearly named tabular
  portfolio containing regularized models, forests and boosting. Make the same
  search available for explicitly selected native families.
* Screen a broad, reproducible parameter pool on a common sample of training
  rows. Spend complete cross-validation on the successful finalists from each
  family. Keep custom grids and exact family budgets in an explicit grid mode.
* Describe this as two-stage screening and allocation. It is not Bayesian
  optimization, and it does not refine parameter values after observing scores.
* Give boosting meaningful tree-capacity and regularization choices. A
  600-round screen should not compare very small learning rates that only become
  useful after several thousand rounds. The initial pool therefore keeps the
  learning rate at or above 0.03 and uses a separate 2,000-round final cap.
* Expose native thread counts and a cooperative search deadline. A deadline
  checked between fits cannot promise a hard elapsed-time limit. State which
  phases it covers, what remains after search and any actual overrun.

## Statistical boundaries

1. Final evaluation rows cannot choose the screening sample, settings, stopping
   round, family, refit fallback or resource policy.
2. Early stopping needs a monitoring partition inside each CV training fold.
   Scoring a model on the same assessment rows that selected its stopping round
   would contaminate the CV score. After choosing the round, refit on all that
   fold's training rows before its assessment.
3. Grouped validation must preserve group separation in screening and stopping,
   as well as in the final holdout. A random split inside an otherwise grouped
   workflow is not sufficient. Original fold labels can define a safe screening
   boundary, even when sampling rows within each side.
4. Rare-class preservation changes sampling fractions. Retain population and
   sampled class counts, and weight decomposable screening losses by inverse
   sampling probability. This restores the original assessment class mixture.
   Screening fits can still see a changed training mixture; disclose it. Final
   CV must use the complete folds at their natural class frequencies.
5. Screening and later CV use the same outer-training universe. Their scores
   are evidence for selection, not independent estimates of final performance.
   Do not use adaptively proposed OOF predictions to claim an unbiased stacking
   evaluation without an additional nesting design.
6. AUC is a pairwise ranking statistic. Do not invent a per-row AUC loss or call
   an average of fold AUCs pooled AUC. Preserve its direction in selection,
   reports, importance and uncertainty. RMSLE requires an explicit policy for
   negative targets and predictions; silent clipping changes the metric.

## Scheduling and evidence

The old fold-major loop can exhaust a deadline after evaluating every
configuration on one fold, leaving no complete candidate. Finite-budget search
must complete a candidate or explicitly bounded cohort before proceeding.
Incomplete folds must never compete with complete CV scores.

The record should distinguish screened out, screening failed, promoted,
skipped for time, CV failed, CV complete and refit failed. A failed family must
remain visible. Requested final settings and the smaller screening settings
must be separate fields. The report should show a compact progression from
screened configurations to completed candidates to the selected fit, with exact
numbers available on demand.

Count actual work: screening fits, full CV fits, stopping pilots, stopping
refits and retained final fits. A configuration count alone conceals substantial
extra work introduced by early stopping. Keep timing and hardware/thread
settings alongside those counts.

## Acceptance that can reject the change

* Compare the frozen 0.7.0 release, the new one-call workflow and competent native
  RF/XGBoost references on matching train/evaluation boundaries and resources.
  Include real classification and regression data and at least one genuine
  forest fit on 100,000 or more training rows.
* Separate development benchmarks from a prespecified final assessment. Repeated
  engineering choices informed by a holdout make it development evidence.
* Require a material quality or resource improvement on the targeted hard
  cases without a material unexplained regression elsewhere. Record all tested
  configurations and failed runs, including negative results.
* Measure the complete call, peak resident memory, native training time and
  report size. Reload saved models in a fresh process and compare predictions
  for every retained family, not only the primary model.
* Test the default call, a small explicit grid, a finite deadline, insufficient
  time, a failed finalist, rare classes, whole-group validation and missing-row
  preprocessing. Make the tests fail under deliberate assessment leakage,
  reversed AUC ordering, skipped-candidate selection and wrong weight alignment.
* Inspect actual reports for regression, binary classification and multiclass
  fitting. A reader must be able to find what was tried, why a candidate was
  promoted, how many rounds were used and which boundary supplied every score.

## Tests and claims to avoid

Existing hand-reconstructed loss arithmetic, real optimizer-failure fixtures
and the imputation mutation test protect useful contracts. Preserve them.
Exact preset-count assertions are appropriate for explicit grid mode, but they
should not force the adaptive mode to mimic the previous implementation.
Assertions for an exact friendly sentence or merely finite output do not prove
correct selection, native execution or a usable report.

Do not promise universal Kaggle competitiveness, automatic knowledge of
leakage boundaries, GPU support without native verification, arbitrary memory
limits, or million-row forest readiness from boosting-only measurements.
Neither these helpers nor a synthetic success case establish those claims.
