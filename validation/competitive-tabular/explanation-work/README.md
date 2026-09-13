# Explanation computation: operation counts

Reports now reuse validated predictions and identities during one computation,
and share class probabilities when the ordered prediction rows are identical.
The synthetic probe below compares operation counts before and after that change. It uses five
inexpensive, deterministic supplied predictors, 90 numeric inputs, 180 evaluation
rows and a 50-row explanation sample. It fits no native models and reads no
external datasets or acceptance labels. Report defaults retain five screening
repeats, twenty audit repeats and eight effect inputs per model and class.

Run from the repository root with the development dependencies installed:

```sh
Rscript validation/competitive-tabular/explanation-work/count-work.R /tmp/explanation-work --shared-context
```

The explicit output directory receives compact CSV files and a source/version
manifest. Append `--events` to retain complete prediction-call records and effect
objects as local RDS files. Those detailed records are not committed. The script
loads an immutable temporary copy of package sources and records the actual
loaded hashes, so simultaneous checkout changes cannot silently alter the run.
The manifest also records any checkout changes observed during execution.

Omit `--shared-context` only when running against the pre-reuse source recorded
in the original manifest. The flag selects the expected accounting, not package
behavior. Both modes assert actual observed counts.

## Measured counts before reuse

| Operation | Regression | Three classes |
| --- | ---: | ---: |
| Union of each model's leading eight inputs | 26 | 27 |
| Screening and audit permutation batches | 4,850 | 4,950 |
| ALE prediction batches | 80 | 240 |
| Full evaluation predictions for fresh fingerprints | 55 | 135 |
| Full evaluation importance baselines | 10 | 10 |
| Full evaluation context references | 5 | 5 |
| Full evaluation agreement batches | 5 | 4 |
| Small context probes | 5 | 5 |
| Supplied-explainer identity hashes | 65 | 145 |

Every operation-count assertion passed. CSV `rows` sums actual rows presented to
prediction, not distinct observations. Instrumented elapsed times are retained
for traceability, but include logging overhead and concurrent host work. They
are not model performance benchmarks. Separate [native forest measurements](../forest-policy/README.md)
show the cost of representative prediction and identity operations.

## Measured counts after reuse

| Operation | Regression before → after | Three classes before → after |
| --- | ---: | ---: |
| Full evaluation prediction batches | 75 → 10 | 154 → 10 |
| Full predictions for identity checks | 55 → 5 | 135 → 5 |
| ALE prediction batches | 80 → 80 | 240 → 80 |
| Supplied-explainer identity hashes | 65 → 15 | 145 → 15 |
| Permutation batches | 4,850 → 4,850 | 4,950 → 4,950 |

The full evaluation predictions now consist of one fresh context prediction and
one completion check per model. The latter still detects changed model or lexical
state during computation. Numeric ALE retains two separate prediction batches
per feature, while each class uses the corresponding probability column. These
counts do not imply a proportional total runtime improvement: permutation work
is unchanged. [Updated results and source manifest](shared/manifest.json) identify
the measured implementation. Detailed native report parity is recorded separately
by the `parity-*` harness and results in this directory.

Supplied-model reconstruction computes an initial explainer identity, then
restores the original custom-instance identifier and hashes again. Native-result
reconstruction needs one initial identity per model. Thus the inferred native
identity counts for otherwise identical work are 60 and 140. These are
**explainer-content hashes**, excluding separate evaluation/data-only hashes.
The native counts are inferred from that code-path difference, not observed
native-model timings or operation counts in this fixture.

## Previous work model and unchanged statistical scope

Let M be retained models, p inputs, U union-selected inputs, C classes (one for
regression or binary classification), N evaluation rows and S=min(N,5000).
With eight successful numeric effect curves per model/class:

- Permutations: M*(5*p + 20*U) batches of S rows.
- ALE: 16*M*C batches of S rows, two batches per curve, not per bin.
- Fresh-fingerprint predictions: 3*M + 8*M*C batches of all N rows.
- Total full-evaluation predictions: 6*M + 8*M*C + K batches. K is the number
  of near-optimal models when at least two qualify, otherwise zero.
- Native explainer-content hashes: 4*M + 8*M*C.
- Pairwise association calls: (U + 8*M*C)*(p-1), repeatedly evaluating the same
  data-only relationships across model/class curves.

The formulas assume no diagnostic failures and at least eight numeric inputs.
Categorical PDP instead predicts once per displayed category, up to sixteen,
on at most min(N,5000,1000) curve rows. Re-rendering retained evidence does not
rerun every permutation and effect, but still refreshes its public report context.
Five models and seven classes permit 315 full-evaluation batches in this work
model. That is an illustration, not a measured Covertype result.

Screening still examines every input using five permutations. Each model's
leading eight inputs contribute to the union audited twenty times on every
selected model. Numeric effects use two separate ALE prediction batches on the
sample; categorical effects use separate PDP grids. Full evaluation predictions
still provide performance, agreement and evidence identities. Reusing screening
shuffles in the audit would change its present seed streams and selection
dependence; reducing repeats, rows or covered models would reduce evidence.

## Exact reuse and its boundaries

The [implementation notes](IMPLEMENTATION.md) describe the private reuse path,
public-boundary freshness checks and verification. They also explain an identity
analysis fix: partial assignment into an outer environment must retain that
environment as lexical state, even when baseline predictions stay unchanged.

Concatenating arbitrary prediction batches is not equivalent. The valid custom
callback `function(newdata) newdata$x - mean(newdata$x)` gives zero differences
between separately predicted lower x=(0,1) and upper x=(1,2), but differences of
one after concatenation. The executable counterexample and its CSV protect this
distinction. Known row-independent native adapters can be evaluated separately;
shared probability columns on the **same** rows do not require concatenation.

Relevant code is in `R/dashboard_generation.R` (`prepare_model_report_data`),
`R/report_preparation.R` (`prepare_report_context`, `prepare_report_effects`),
`R/evidence_contract.R` (`current_explainer_fingerprint`), `R/audit.R`,
`R/permutation_importance.R` and `R/partial_dependence.R`. The manifest binds the
recorded results to source hashes rather than line numbers that can drift.
