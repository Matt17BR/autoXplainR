# Software architecture and test audit before 0.6.0

Baseline: `12e9ba1` (published 0.5.0). All source line references below refer to
that commit, not the changing implementation working tree. The audit used
implementation reads and targeted counterexamples, rather than test totals or
README claims. No dependency installs or live H2O runs were needed.

## 1. Secondary and class-specific effect failures disappear from aggregate status

**Verified defect, high priority.** `R/report_explorer.R:259–305` catches secondary
model effect errors and stores `effect_failure` strings in nested model/class
collections. `R/dashboard_generation.R:157–169` only builds the primary model's
flat failure table. `R/report_view_model.R:133–150` and
`R/evidence_summary.R:59` only consult that flat representation.

On a four-model regression fixture with two displayed inputs, an injected
failure for every secondary curve produced:

```
Nested failures: main_model 0; simple_baseline 2; small_tree 2; flexible_tree 2
Public primary failure rows: 0
Exported failure rows: 0
Failed aggregate diagnostic statuses: 0
```

A reader could see a successful overall status while six displayed-model
explanations were unavailable. The same dimension mismatch applies to added
multiclass curves. Initial reproduction: `/tmp/axr-050-software-audit.R` and its
`.log`; failure fixture `/tmp/axr-050-secondary-failure.rds`.

**Repair:** one model/feature/prediction-class/method/status/reason table, with
the old primary failure table retained only as a compatibility view. Derive the
console, export and report status from the same complete evidence. Validate all
retained model/class curves when applying a primary effect override.

**Acceptance:** inject exactly one secondary-class failure; identify its model,
class and feature on every aggregate surface; preserve successful siblings.
Also reject a foreign secondary curve despite an otherwise valid primary
override, and make an explicit empty override actually empty. New
`test-report-preparation.R` exercises these boundaries.

## 2. Rendering retained evidence repeatedly predicts the same evaluation rows

**Verified performance and lifecycle defect, high priority.** A retained
four-model, 20-evaluation-row report invoked prediction **66 times**, processing
**946 prediction rows**, despite not refitting or recomputing retained curves.
The small fixture took 0.408 seconds locally; that time is descriptive, not a
portable benchmark. Construction and explanation used 273 calls in the same
instrumented process, measured separately from rendering.

Sources include the constructor's probe and full prediction
(`R/explainer.R:130–136`), repeated reconstruction in
`R/autoxplain.R:468–536`, full prediction during each fingerprint validation
(`R/evidence_contract.R:71–80`), and independent prediction preparation in
report and diagnostic views. The initial reproduction is in
`/tmp/axr-050-software-audit.R` and `.log`.

Existing reuse tests mocked `prepare_model_report_data` or `explain_effect` to
fail if called; they proved those specific functions were not invoked, but did
not measure prediction work or engine dependence. A remote or expensive
predictor magnifies this avoidable cost.

**Repair:** create one explicit, short-lived context at each render boundary,
containing explainers, their already-computed reference predictions and content
fingerprints. All downstream views and optional checks consume it. Rebuild at
the next render; never install a global or persistent stale cache.

**Acceptance:** instrument actual adapters and count full batches, not helper
invocations. One full batch plus the existing small schema probe per model is
sufficient; later views add no calls. Changed coefficients, outcomes and row
order must invalidate a fresh context and reject retained evidence. New
`test-report-preparation.R` covers the context and optional APIs; the integrated
render count is recorded separately after UI integration.

## 3. Public Plotly class agreement depends on arbitrary class coding

**Verified numerical defect, high priority.**
`R/plotting_functions.R:195–215` reduces multiclass probabilities to integer
column indices and takes Spearman correlation. The HTML report and legacy
narrative had already switched to predicted-class agreement.

For four observations with winners A=(a,a,b,c), B=(a,b,b,a), and
C=(a,c,b,c), reordering probability columns from (a,b,c) to (c,a,b) preserved
all predicted labels. Nevertheless, A/B's exported heatmap changed from
**−0.2357023 to +0.7071068**, and A/C from **0.3888889 to 0.8333333**.
Actual class agreement is respectively 0.5 and 0.75, independent of coding.
Controlled reproduction: `/tmp/axr-050-plotly-controlled.R` and `.log`.

**Repair:** classification uses label agreement with a 0–1 scale and matching
legend. Regression can retain explicitly labeled rank correlation and undefined
constant-prediction cases. Share the numerical definition across export paths.

**Acceptance:** inspect built Plotly trace `z`, independently count agreement,
and verify invariance to class renaming and column permutation. Merely asserting
`plotly`/`htmlwidget` class, as baseline `test-plotting.R` did, cannot detect this.
Root implemented this repair with trace-value tests during 0.6.0 work.

## 4. Public categorical effect plots imply continuity that the HTML avoids

**Verified presentation semantics defect, medium priority.**
`R/plotting_functions.R:111–146` unconditionally adds a ribbon and connected
line. A categorical PDP with levels east/south/west produced a filled polygon
and `mode="lines"` trace joining those unordered categories.
Reproduction: `/tmp/axr-050-plotly-audit.R` and `.log`.

**Repair:** separate categorical markers and intervals; retain numeric curves
only for numeric axes. **Acceptance:** inspect trace modes, fill and category
labels for nominal and ordered factors, not just widget class. Root implemented
categorical marker and trace-data tests during 0.6.0 work.

## 5. Supported-platform checks were not prerequisites of tagged publication

**Verified workflow policy gap, medium priority.**
`.github/workflows/release.yaml:94–95` depended on Linux quality, browser,
H2O and pinned-engine jobs. The macOS, Windows, R 4.1 and old-release matrix in
`.github/workflows/R-CMD-check.yaml:1–25` ran for main/PR/manual events but was
not a callable or required release dependency. A tag on another commit could
therefore publish without those checks on that exact commit. This is not a
claim that the published 0.5.0 artifact failed: its recorded checks passed.

**Repair:** reuse the supported-platform matrix as a blocking release
prerequisite, or verify required successful checks on the exact tagged SHA.
**Acceptance:** exercise the reusable job and prove a failed platform blocks
publication; inspection of workflow existence alone is insufficient. Root added
the reusable matrix and release dependency during 0.6.0 work. CI execution is a
separate verification obligation.

## 6. Existing fitted models cannot use the full new report workflow

**Verified architectural limitation, medium priority.** `explain_model()` and
`audit_explanations()` support existing/custom fitted models, but
`render_model_report()` (`R/reporting.R:52–54`) accepts only an
`autoxplain_result`. `as_explainers()` converts that result to explainers, with
no public inverse evaluation constructor. `render_explanation_report()` uses
the older static report path. Users with an established training pipeline miss
the model explorer, prediction diagnostics, training/evaluation context and
model specifications without manually assembling internal result fields.

**Proposed repair:** a public fitted-model evaluation constructor with named
models, explicit data/outcome/adapters, shared row identity and honest missing
training-selection provenance. It should reuse the same report contract
without fitting anything. **Acceptance:** custom/native fitted models reach
the same report, predictions match direct model calls, no training is invoked,
and unavailable tuning information is clearly distinguished from failure.
At baseline this was a capability proposal; its implementation evidence is
recorded below.

## Follow-up implementation evidence

The baseline observations above are preserved separately from these repairs.
The per-render preparation layer now shares each model's full evaluation
prediction vector among report modules. Replaying the four-model, 20-row
instrumented fixture used 8 adapter calls and 92 predicted rows (one three-row
constructor probe and one full evaluation per model), versus 66 calls and 946
rows at baseline. This is a prediction-work comparison, not a claim that the
larger report has lower total wall-clock time. A later render rebuilds the
context; changed models or data cannot reuse the previous render's cache.

`evaluate_models()` now connects explicitly supplied fitted models and adapters
to the report without fitting, inventing a training set, running model
selection, or synthesizing a reference model. Controlled tests cover a first-level
binary event, cutoff ties, multiclass column permutations, RDS round trips,
changed-model rejection of attached evidence, optional training context, and
evaluation-only source row identity. The three new preparation, prediction and
existing-model test files passed 210 assertions after the final calibration and
binary disagreement corrections.

The persistent `validation/check-predictions.py` uses separate full-precision
`prediction_source` records written by `validation/render-explorer-cases.R`.
These contain observed outcomes and public prediction results; they do not
copy report aggregates. The Python oracle independently counts all 101 binary
cutoffs for every retained model, checks multiclass labels and confusion
rates, reconstructs rank groups and their weighted calibration gap, and checks
rendered calibration positions and circle areas. It also checks source links,
exported-row restrictions, summary/none modes, no-JavaScript confusion tables,
and automated WCAG A/AA rules at the Predictions module.

On the calibration-corrected fixture, before the later lifecycle changes, it
passed 1,853 assertions in Python 3.14.4,
Playwright 1.58.0, Chromium 145.0.7632.6 and axe-core 4.13.0. Its JSON artifact
records runtime versions, report/oracle SHA-256 digests, accessibility
incomplete results, and deliberately corrupted-report evidence. Replacing the
primary model's cutoff table with the neural model's table left JavaScript
valid but failed the numeric assertion at cutoff 0.02: the report showed
TN=0/FP=35 where original predictions require TN=1/FP=34. This demonstrates
that the gate detects a wrong model's valid-looking counts. The browser checks
are implementer acceptance checks, not a human usability study; axe incomplete
results require review and are not represented as accessibility passes.

The strict optimizer policy exposed an assumption in native-engine tests:
their multiclass comparison required a linear multinomial fit to be retained
even when its optimizer reported nonconvergence. Production rejection was
preserved. The one-row linear prediction test now uses deliberately overlapping
classes and asserts observed convergence; optional-engine explanation tests
use a tree comparator. The focused native-engine suite then passed 607
assertions across the seven locally available optional native engines. The separate iteration-limit fixture continues to
verify exclusion and the explicit warning-policy alternative.

## Follow-up adversarial lifecycle review

Independent UI review found that a new report could combine constructor-time
RMSE with a subsequently edited model when no explanations had yet been made.
The stronger counterexample kept every evaluation prediction unchanged: with
evaluation `x == z`, a closure using `x + state * (x - z)` could change its
permutation/effect behavior after the audit while its old prediction-based
identity still matched. Native formula transforms and environment-valued model
attributes had the same gap. A schema-only event edit could separately make the
report's declared event disagree with the adapter's event.

`R/evaluation_snapshot.R` now records the constructor's assessment identity and
checks it before reuse. It binds official model/data/schema/recipe/validation
and selection state, while allowing explicitly derived benchmark columns and
cosmetic units. `prediction_function_context()` traverses referenced lexical
inputs and model attributes, and native formula identity distinguishes data
column names from unrelated ambient variables. Recognized reflective lookup,
external I/O, stochastic calls, active bindings and external pointers are
rejected for reusable custom evidence. This is a bounded compatibility check,
not proof of purity for arbitrary dynamic R programs or namespace internals.

Every new report also compares its already-cached current predictions and
recomputed metrics with the recorded assessment. This adds no prediction calls
and catches mismatches not visible in serialized model state. Unsealed legacy
results receive the same comparison of available ordered prediction fields and
official metrics; their former off-grid state cannot be recovered. Actual
0.5.0 regression, binary and multiclass saved results all rendered successfully
after explicit explanation recomputation. Their older retained audits were
rejected rather than silently mixed with the newer identity contract.

The focused constructor/snapshot/evidence/preparation tests passed 234
assertions after these changes. They include changed fits before the first
report, unchanged observed predictions with changed off-grid behavior, native
formula state, attributes on list/atomic/function/environment models, irrelevant
ambient bindings, RDS round trips, event/recipe/group/selection edits, declared
but unobserved classes, selected-model subsets and explicit references. The
first full run under the new guard exposed six tests that mutated sealed result
metadata directly. Those fixtures were moved to supported inputs or isolated
renderer tests; a subsequent run passed 3,817 assertions with no failures,
errors or warnings and two opt-in integration skips.

The final bounded native-callback review additionally covered registered custom
S3 prediction methods, GLM inverse-link closures, separately supplied LM/GLM
offset expressions, and callable or named contrast functions. Controlled
examples preserve every evaluation prediction while changing predictions on
other feature combinations. The snapshot test file passed 96 assertions after
these additions. Eight canonical report results retained exactly the same
fingerprints before and after the narrow native-callback repair, so ordinary
stats methods were not invalidated. The final full suite and archive checks
are recorded separately from these earlier focused results.

The final source suite completed 397 tests with 3,845 passing assertions,
zero failures, errors or warnings, and two opt-in integration skips. The final
regenerated 0.6.0 fixtures passed 1,923 Predictions browser assertions with no
runtime errors. This includes dynamic exported-case decisions, probabilities,
ordering and source links at cutoffs 0, 0.50, 0.57 and 1 across model switches,
plus keyboard access to the chart's rank-grouping explanation. Each of the
four fixture scans had zero axe WCAG A/AA violations and one incomplete rule
record; incomplete results remain review items. The wrong-model cutoff-table
mutation still fails the independent numeric count oracle. Detailed local
records are `~/.cache/autoxplain-overhaul-0.6/full-suite-compact/` and
`~/.cache/autoxplain-overhaul-0.6/final-predictions/prediction-checks.json`.

## What survives scrutiny

The exact-archive release process checks a built source archive and its digest,
checks the same archive under R-devel, and records a downloaded-artifact
installation proof. Existing numerical tests include analytic effect fixtures,
independent reference methods, held-out-outcome perturbation checks, and a
training-imputation mutation gate. The 0.5.0 browser gate gained independent
curve/cost oracles after deliberately corrupted graphs exposed weak assertions.
Those are meaningful strengths. The defects above show why additional model,
class, export and lifecycle boundaries still matter more than increasing the
number of superficial expected-output assertions.
