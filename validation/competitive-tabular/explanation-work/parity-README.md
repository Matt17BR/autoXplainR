# Complete report evidence parity

The report now reuses validated predictions and model identities within one
report calculation. To check that this saves work without changing evidence,
`parity-reports.R` loads immutable source snapshots in separate R processes and
compares their complete output using `identical()`.

The fixtures contain one ranger model and one XGBoost model for each of
regression, binary classification and three-class classification. Each model is
fitted once on 140 rows and assessed on the same 70 rows. Inputs include a
categorical variable and a numeric variable with ties. The binary fixture uses
the reversed event class. Forests have 32 trees and boosting models have 24
rounds, with one native thread. Reports use a 31-row explanation sample, three
leading inputs per model and four detailed permutations.

The comparison includes screening, every importance repeat, audit evidence,
effect curves, all class-specific effects, statuses, attributes and list order.
Only fields named `created_at` are removed. The underlying evaluated results are
serialized once, so all other timestamps and native model bytes are shared.

All three complete objects pass. Source hashes, native versions, fixture hash,
runner hash and per-field results are in `parity-results.json`. This is a
reproducible behavior check on six small real native fits, not a large-data speed
claim or a substitute for the custom-predictor boundary tests.

The first complete comparison found that multiclass `effects_by_class` had moved
before `config` in the output list. Every named value was identical, but the
whole object failed. Production order was restored and the same serialized
fixtures were rerun successfully. The whole-object assertion was retained.

Run from the repository root, supplying a pre-reuse source checkout or snapshot:

```sh
Rscript validation/competitive-tabular/explanation-work/parity-reports.R \
  /path/to/pre-reuse-source /path/to/new-output-directory
```

An optional third argument reuses an existing `fixtures.rds` without refitting.
Each run copies both source trees before loading either. It checks source hashes
before and after copying and refuses to overwrite existing snapshots. Full
fixtures and complete evidence objects stay in the output directory; only the
compact manifest is tracked in the repository.

## Separate public RNG contract repair

The native investigation also found an older public API bug: ranger predictions
draw a random seed even for regression and probability forests, and some full
prediction calls sat outside the helpers that restore RNG state. Public
importance and effect functions therefore violated their documented promise to
leave the caller's random state unchanged.

The public importance, audit and effect boundaries now preserve the complete
caller's RNG state with `withr::with_preserve_seed()`. They do not reset the seed
at entry, reorder predictions or change internal permutation streams. PDP and
ALE aliases inherit the same boundary. This is a contract repair, separate from
the exact reuse of report evidence.

Complete report preparation and HTML rendering also preserve the caller's RNG
state at function entry. This uses a local exit handler, preserving the existing
`missing()` checks that select retained report evidence when optional arguments
are omitted.

`test-explanation-native-rng.R` checks native regression, reversed-event binary
and multiclass forests, both existing and absent `.Random.seed`, successful
calls and errors after a real native prediction. Complete importance, audit and
curve objects still match the unchanged private scalar workers; only the audit
creation timestamp is excluded. Each test also verifies that the scalar worker
actually consumed RNG, so a wrapper that omitted preservation would fail.
Native report tests cover successful preparation and HTML output, prediction
errors, existing and absent RNG state, and retained evidence without recomputing
its permutations.
