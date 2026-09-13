# Acceptance harness review

Reviewed on 13 September 2026. This record covers harness hardening and
synthetic/development verification only. Full acceptance remains pending.
No locked evaluation targets were read, full fits launched, or manifest
capable of unlocking the actual benchmark partitions created by this review.
Existing interrupted/completed records and active frozen scripts were preserved.

## Corrections

- Both runners require an exact stage and cohort in each authorization.
  A native score-only entry cannot authorize refitting. Separate cohorts bind
  their own saved process, model and summary hashes.
- Prediction, retained-model, leaderboard and evaluation-metric IDs must have
  complete, nonempty, duplicate-free coverage. A missing forest cannot disappear
  from score, row-count or replay verification.
- Full-row counts must be finite scalar values matching the declared partition.
  Ranger, glmnet and baseline counts are checked from native models; XGBoost
  row counts remain explicitly identified as input-encoding metadata. Native
  boosted rounds and forest tree counts are checked directly.
- Independent arithmetic checks summary, result-evaluation and leaderboard
  metrics, complete OOF row/fold identity and final-model identity. Binary
  benchmark loss and reported/CV loss retain their documented clipping
  operations; endpoint floating-point differences are recorded separately.
- Native accounting includes fitting and every scoring attempt, including
  failures, with the maximum observed RSS across all attempts. Missing or
  invalid durations cannot reset the budget. Verified failure records receive
  no quality credit. Replay and verification time remain separate.
- `check-evidence.R` retains its development-only guard. The separate
  `check-acceptance.R` requires the consumed final freeze, pinned executable
  helpers, immutable artifacts, complete replay and matched actual threads.
  Failed references are identity-checked before exclusion. Public selected-model
  and retained-forest quality gates remain separate.
- A declared full published 0.7 baseline is checked on the same acceptance
  partition before applying the 2% non-regression gate. Public Bank requires
  native references matching its actual automatic thread count. Development
  losses or timeouts cannot substitute for either full-data comparison.

## Verification actually performed

| Check | Count or result |
|---|---|
| Isolated Python/R authorization fixtures | 21 scenarios passed: absent/wrong stage or cohort, cross-stage reuse, distinct references, duplicate authorization and changed source hashes |
| Synthetic resource helper fixtures | 32 scenarios passed: cumulative attempt accounting, invalid/missing scalar fields, source tampering, active attempts, limits and historical RSS |
| Synthetic Python supervisor fixtures | 17 scenarios passed: invalid source/prior durations reject and earlier RSS is retained |
| Failed-attempt accounting fixtures | 15 scenarios passed: bounded and over-budget failures remain failures; stub records, missing evidence and wrong identities reject |
| Reusable development evidence guard suite | 30 tamper cases rejected, including missing/duplicate predictions, altered reported scores, missing/incomplete rows, absent forest, changed primary/final identity, OOF omissions and overridden public defaults |
| Existing public development model |All 4 retained models and 1,500 evaluation rows verified; fresh-session replay was exact |
| Existing native development models |Ranger/XGBoost 2,500-row fits and 1,500-row saved predictions verified; missing row metadata rejected |
| Published 0.7 development compatibility |All 3 retained models, 20,000 training rows, 5,000 evaluation rows and five frozen folds verified |
| Actual development resource metadata |Public process and native fit plus two scoring attempts passed independent accounting |
| Static checks |All changed R entry points/helpers parsed; Python runner compiled; no trailing whitespace |

Hand-calculated regression, multiclass and binary endpoint cases also checked
metric definitions and primary/forest threshold arithmetic. Repeated test runs
are not added to these counts.

The disposable public fixture repaired a historical null summary fold count
from its saved five-fold assignment and added the missing historical summary
hash. Its provenance records both original hashes. Original benchmark evidence
was unchanged; acceptance rejects missing fold counts. An older native XGBoost
fixture's missing round-count field was populated only in memory from its
existing booster to test the new verifier.

## Reproduction

Use a disposable copy of a completed development run as
`AXR_DEVELOPMENT_RUN`, its matching private package library as `AXR_LIBRARY`,
and a new output path as `AXR_CHECK_RECORD`. `AXR_TABULAR_DIR` identifies the
existing declared partition cache. No particular user directory is required.
These commands perform verification and prediction replay, not fitting:

```bash
Rscript --vanilla validation/competitive-tabular/check-evidence-guards.R "$AXR_DEVELOPMENT_RUN" "$AXR_CHECK_RECORD"
Rscript --vanilla validation/competitive-tabular/check-evidence.R "$AXR_DEVELOPMENT_RUN"
Rscript --vanilla validation/competitive-tabular/replay.R "$AXR_DEVELOPMENT_RUN" "$AXR_LIBRARY"
```

The one-off synthetic fixtures used isolated caches, empty case declarations
and non-RDS sentinels. They did not open actual acceptance files. Detailed
fixture records and the manifest/command handoff are retained separately from
this review.

After root authorizes the final freeze, use the frozen harness and matching
library (`unused` for native references):

```bash
Rscript --vanilla "$AXR_HARNESS/replay.R" "$AXR_RUN" "$AXR_RUN_LIBRARY" "$AXR_FREEZE"
Rscript --vanilla "$AXR_HARNESS/check-acceptance.R" "$AXR_RUN" "$AXR_RUN_LIBRARY" "$AXR_FREEZE"
```

Replay native references and the published baseline before checking the public
candidate. The per-case verifier reports half-excess closure; the original
requirement that at least one applicable challenging case closes half the old
excess still needs a suite-level decision. Full-workflow completion, actual
held-out quality, offline report tasks and release approval remain open.
