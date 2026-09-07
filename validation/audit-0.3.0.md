# Critical audit of AutoXplainR 0.3.0

Date: 2026-09-07. Source: `2e6920354fe7ec7550f9c2688eff5f54034c28bd`.

## Verdict

The project has a useful core: fit a model, preserve the evaluation boundary,
compare with a baseline, and retain evidence that a person can inspect. Its
current release also contains silent calculation and evidence-identity bugs.
The report gives some weakly justified summaries more visual authority than
they deserve. Passing release checks did not establish correctness broadly
enough to justify treating the whole workflow as robust.

The next work should repair the numerical contracts and make the report a clear
record of the analysis. Adding more estimators or model families should wait.
The most defensible differentiation is fewer decisions and less glue code
between raw data, appropriate evaluation and an understandable report. That
advantage remains a hypothesis requiring comparison with existing workflows.

## How this review was conducted

Four separate reviewers examined statistics, software, UI, and product/docs in
fresh contexts without the earlier implementation rationale. A coordinating
reviewer checked source references, reran the numerical and adapter
counterexamples, and measured the actual report in a browser. Independence here
means separate review contexts; this is not external human peer review.

The audit used R 4.5.2, the current repository, public built-in/synthetic data,
and the committed generated report. Eight targeted existing test files passed:
inner-CV integrity, validation design, bootstrap uncertainty, statistical
oracles, calibration, thresholds, importance, and effects. The coordinator's
rerun passed 201 assertions with zero failures, warnings or skips; its log is
[retained here](audit-evidence/existing-targeted-tests.txt). The new counterexamples
still reproduced. The full engine matrix was not rerun during
this audit; recent CI results and prior coverage totals were treated as evidence
of those checks, not proof against the failures below. No live H2O or hosted
language-model call was made.

The package implementation was left unchanged. Diagnostic scripts and observed
outputs are in [audit-evidence](audit-evidence/README.md). Some scripts deliberately
print wrong behavior; a successful script exit does not mean that behavior is
correct. Source links below refer to line numbers in the audited commit.

Priority meanings: **P1** can silently corrupt a result or its interpretation;
**P2** is a material reliability, usability or maintenance issue. A source-backed
gap or design judgment is explicitly distinguished from a reproduced failure.
The numbering identifies findings, not a claim that their effort or severity is equal.

## Calculation and evidence contracts

### 1. P1 — A selected binary event can have the opposite probability

**Reproduced.** [R/explainer.R:206](../R/explainer.R#L206) takes binomial GLM
response probabilities and [line 288](../R/explainer.R#L288) returns a numeric
vector unchanged. Explaining the same model with `positive="no"` and
`positive="yes"` produced identical predictions, rather than complements; the
maximum complement error was 1. This affects the lower-level explainer when
the requested event differs from the model's native event. It does not mean
the ordinary guided workflow's default second-level convention always fails.

**Repair and acceptance:** record the adapter's native probability event and
convert deliberately. Reversing the positive event must complement predictions,
preserve predictive quality under the corresponding truth recoding, and reverse
probability effects. Test nonalphabetical class order and native fitted wrappers.
Ambiguous custom numeric vectors need an explicit event contract.

### 2. P1 — Invalid predictions become plausible numbers before validation

**Reproduced.** [R/explainer.R:259](../R/explainer.R#L259) converts a regression
factor to its integer storage codes: a factor containing `"100"` became 2.
[Line 263](../R/explainer.R#L263) turns every unrecognized binary label into zero;
an adapter returning `"typo"` for every row was accepted. Hard class labels also
become probability evidence without a separate capability declaration.

**Repair and acceptance:** validate original types, dimensions and class
membership before conversion. Separate label predictions from probabilities.
Invalid labels and factor regression output must fail specifically; valid
probability vectors and named matrices must continue to work. Probability losses
must not be silently synthesized from an adapter that only supplies labels.

### 3. P1 — Singleton permutation strata borrow observations from other strata

**Reproduced.** [R/permutation_importance.R:286](../R/permutation_importance.R#L286)
uses `sample(indices)`. For a singleton numeric index `k`, R samples from `1:k`.
With twenty singleton strata, one draw moved sixteen rows across strata and had
only eleven distinct donors. Public within-stratum importance was 6.519 with
interval [6.153, 6.886]; every permitted permutation should instead be the
identity, giving zero. Warnings also occurred. This is the documented singleton
trap in [R's sampling API](https://stat.ethz.ch/R-manual/R-devel/library/base/html/sample.html).

**Repair and acceptance:** sample positions, for example
`indices[sample.int(length(indices))]`. All-singleton and mixed-size cases must
preserve stratum membership and donor bijection, emit no warnings, and recover
zero when no observation can move. Check every index-sampling path for this trap.

### 4. P1 — Returned ALE coordinates and values are inconsistent

**Reproduced.** [R/partial_dependence.R:497](../R/partial_dependence.R#L497)
accumulates full-bin changes, while [line 503](../R/partial_dependence.R#L503)
attaches them to bin midpoints. For the exact function `f(x)=3*x` on irregular
input, the returned secant slopes were 3, 5.944 and 2.783. Empty bins receive a
zero increment at [line 490](../R/partial_dependence.R#L490): tied numeric inputs
for `f(x)=x` produced an effect span of 2 across reported coordinates spanning
3.2. This is a numerical defect before any plot is drawn.

**Repair and acceptance:** make the evaluation coordinates agree with the
accumulated estimator; handle unsupported bins explicitly. Verify analytic
linear/additive recovery on skewed, irregular and tied data, including correct
slope at every reported coordinate. Compare against the estimator described by
[Apley and Zhu](https://arxiv.org/html/1612.08468v2), with conventions made explicit.

### 5. P1 — Multi-model audits do not establish common observations

**Reproduced.** [R/audit.R:54](../R/audit.R#L54) checks task and row count, but
not row identity, ordering, outcomes or positive-event semantics. The same exact
regression function on forward and reversed evaluation rows received zero RMSE
in both explainers, prediction agreement -1, and a material-disagreement warning.
Equivalent opposite-positive binary encodings also need normalization.

**Repair and acceptance:** define shared evaluation rows and event semantics
before paired comparison. Either align explicit row IDs or reject mismatches.
Reordering identical cases must produce agreement 1 and ambiguity 0 after
alignment, or a clear error. Different data of equal size must never silently pair.

### 6. P1 — Foreign evidence passes the model-identity guard

**Reproduced through the public API.** The fingerprint at
[R/explainer.R:125](../R/explainer.R#L125) excludes model state, predictor and
outcome contents, row order, positive class and the custom prediction function.
[R/model_behavior.R:557](../R/model_behavior.R#L557) nevertheless uses it to
authorize attaching an explanation audit. Runs with seeds 10 and 20 had different
models and test rows but identical corresponding fingerprints; an audit from the
first was accepted by `compare_model_behavior()` on the second. The report's
explicit audit argument also lacks a comparable content-identity check.

**Repair and acceptance:** establish run/model identities and content checks for
ordered evaluation inputs, outcomes and prediction semantics. Validate every
evidence attachment. Reject cross-run, altered-data, changed-model and
opposite-event evidence; accept an unchanged save/load round trip. A stronger
hash function cannot compensate for omitted inputs.

### 7. P2 — Reporting fewer features hides dependence

**Reproduced.** [R/audit.R:114](../R/audit.R#L114) checks dependence only within
the selected feature subset. With `proxy=x` present, auditing both gives
association 1 and grade C for x; reporting only x gives association 0 and grade A.
The default workflow screens to eight features before this check, so the issue
can occur automatically.

**Repair and acceptance:** separate reported features from the predictor context
used to assess dependence. Removing a proxy from the displayed list must not
improve the unchanged feature's dependence result.

### 8. P2 — Subgroup analysis can erase the groups needing attention

**Reproduced.** [R/subgroup_performance.R:37](../R/subgroup_performance.R#L37)
uses processed evaluation data. Three rows each of `a`, `b`, unseen `new`, and
missing were reported as only `a=9, b=3` after default preprocessing. The
missing/unseen groups disappeared into the mode group.

**Repair and acceptance:** preserve aligned raw context separately from model
inputs, or accept explicit raw grouping values. Keep all four groups distinct
while predictions use the saved recipe. Row dropping must preserve alignment;
processed-feature grouping should be an explicit, labeled choice.

### 9. P2 — Threshold metrics contradict the ordinary evaluator

**Reproduced.** [R/threshold_diagnostics.R:162](../R/threshold_diagnostics.R#L162)
returns balanced accuracy 1 on an all-negative evaluation set because it drops
undefined sensitivity. The ordinary evaluator correctly leaves this unavailable.
[Line 148](../R/threshold_diagnostics.R#L148) returns F1 missing for TP=0, FP=2,
FN=2, although the count formula gives zero.

**Repair and acceptance:** share metric implementations and explicit undefined
policies. Test one-class samples and completely wrong predictions against
independent count calculations. The
[standard F-measure definition](https://scikit-learn.org/stable/modules/model_evaluation.html#precision-recall-and-f-measures)
also provides a direct oracle for this edge case.

### 10. P2 — A letter grade conflates different questions and model scopes

**Reproduced, plus design judgment.** [R/audit.R:307](../R/audit.R#L307) assigns
grades from shuffle sign stability, Monte Carlo limits and pairwise association;
[line 496](../R/audit.R#L496) takes an all-model median. The same primary model
and evaluation rows score A/100% alone and B/50% after adding only its intercept
baseline. In the published comparison report, the three qualified/unsupported
claims are baseline x, baseline z and small-tree z, all with zero importance.
The report nonetheless presents a broad warning beside a large A badge.

**Repair and acceptance:** remove the report-wide grade. Show separately scoped
shuffle stability, dependence limitations, evaluation adequacy and candidate
agreement. The baseline's expected absence of feature reliance should not become
an alarming feature claim. Adding a baseline must not change the primary model's
diagnostic interpretation. An absent comparison should say why it was not made.

### 11. P2 — A narrow association screen is treated as broader reassurance

**Reproduced limitation, not a bug in Spearman correlation.**
[R/audit.R:285](../R/audit.R#L285) uses absolute Spearman correlation for numeric
pairs. Symmetric x and its deterministic proxy x² produced association 0.0041
and grade A for the reliant x² model. Low monotone association is not independence.

**Repair and acceptance:** label the screen's scope accurately and stop deriving
broad reliability from a small value. Add nonlinear diagnostics only with a
defined target and validation. Deterministic nonlinear counterexamples must
produce a suitable warning or an explicit unassessed-dependence qualification.

### 12. P2 — Learned external H2O preprocessing precedes internal CV

**Source-backed limitation; preprocessing influence reproduced, live ranking
effect unmeasured.** [R/h2o_workflow.R:24](../R/h2o_workflow.R#L24) prepares the
whole outer-training frame before H2O cross-validation. Opting into learned
imputation therefore allows inner-assessment values to affect an analysis-row
imputation. The reviewer changed a hypothetical inner-assessment partition and
observed an analysis imputation change from 101 to 10001. H2O's default numeric
missing-value `keep` avoids this particular imputation example. The outer
holdout is still separate.

**Repair and acceptance:** document/restrict this combination or orchestrate
fold-local preprocessing externally. Inner-assessment-only perturbations must
not affect that fold's learned recipe. H2O operates on the supplied frame, as its
[CV documentation](https://h2o-release.s3.amazonaws.com/h2o/rel-3.46.0/7/docs-website/h2o-docs/cross-validation.html)
describes. Do not advertise all preprocessing paths as fold-local until verified.

## Architecture and operational behavior

### 13. P2 — Ordered categorical predictors break neural tuning

**Reproduced.** [R/tuning.R:1192](../R/tuning.R#L1192) builds ordered-factor
contrasts for training; prediction reconstructs ordinary factors at
[line 1255](../R/tuning.R#L1255). The matrix columns then disagree. A neural-only
public tuning call failed every configuration. Ordered status is also lost in
parts of preprocessing.

**Repair and acceptance:** use one saved matrix blueprint for neural models,
including categorical kind, levels and contrasts. Test ordered/unordered factors
for regression, binary and multiclass; one-row prediction; save/load; and changed
session contrast options after fitting.

### 14. P2 — Formula environments retain unrelated data and models

**Reproduced.** [R/matrix_blueprint.R:316](../R/matrix_blueprint.R#L316) attaches
the caller's environment to internally generated formulas. The intercept
baseline retains sibling models and raw tuning tables from
[R/guided_workflow.R:522](../R/guided_workflow.R#L522). For 3,000 rows and ten
inputs, the baseline alone serialized to 7,657,086 bytes. Minimal formula/terms
environments reduced that to 155,654 with unchanged predictions. `object.size()`
reported 596,912 bytes and missed much of the retained state.

**Repair and acceptance:** use minimal environments where column-only formulas
allow them; inspect model frames and terms as well as top-level formulas.
Model-only export must not capture siblings or unrelated raw columns. Verify
prediction equivalence and artifact-size behavior. Clarify which resource
measurement is actually being reported before using it in a frontier.

### 15. P2 — Custom prediction dispatch counts `...` as an input

**Reproduced.** [R/explainer.R:221](../R/explainer.R#L221) mistakes
`function(newdata, ...)` for a two-input model/data adapter. It passes the model
as data and fails with an irrelevant `invalid 'times' argument` in the example.

**Repair and acceptance:** define and validate supported signatures explicitly.
Test one/two inputs with and without ellipsis, ambiguous signatures, and
preservation of the prediction function's own errors.

### 16. P2 — Some failed diagnostics still disappear from reports

**Confirmed in source; no additional natural-input trigger established in this
audit.** [R/reporting.R:346](../R/reporting.R#L346),
[452](../R/reporting.R#L452), [497](../R/reporting.R#L497), and
[896](../R/reporting.R#L896) catch errors in comparisons, ambiguity and thresholds
and omit the section. The new explicit effect-failure handling is a better
contract, but it has not been applied consistently.

**Repair and acceptance:** every diagnostic returns a status: computed,
inapplicable, not requested, insufficient evidence, or failed with a reason.
Inject an adapter failure and verify it reaches the result, HTML and console;
normal inapplicability should remain quiet and understandable.

### 17. P2 — Test coverage is broad but weak at contract intersections

**Evidence gap.** Existing tests passed while the counterexamples above failed.
The initial ALE test at [tests/testthat/test-effects.R:10](../tests/testthat/test-effects.R#L10)
accepts correlation above 0.95, which is too weak to establish correct geometry.
The analytic additive test uses equally spaced inputs. The cross-package script
at [validation/run-reference.R:20](run-reference.R#L20) checks PDP, not ALE.
The optional-engine CI matrix tests current engines on current R; it does not
establish every older minimum version declared in DESCRIPTION.

**Repair and acceptance:** prioritize mathematical invariants and combinations:
irregular/tied inputs, singleton groups, changed class event, reordered cases,
stale evidence, recipe changes and missing diagnostics. Add representative
minimum-version compatibility jobs or narrow declared support. Keep the existing
full suite, but make these independent oracles release gates; a percentage target
is not a substitute for them.

## Report design and user experience

### 18. P1 — The renderer further distorts effects and removes their scale

**Reproduced/source-confirmed, independent of finding 4.**
[R/reporting.R:1188](../R/reporting.R#L1188) assigns equal pixel spacing by row
index, ignoring numeric coordinates. Each effect fills its own vertical range.
The published x and z curves span 10.5633 and 2.9417 units but look similarly
sized. The SVGs contain no text nodes: no input labels, output ticks, zero line
or support. Categorical effects also need actual category labels rather than an
unlabeled connecting line.

**Repair and acceptance:** plot real coordinates; label target/event and units;
show ALE zero and empirical support; state shared versus independent scales.
Provide a text/table equivalent with essential values. A user should be able to
locate a change and quantify its size without opening an R object. W3C's
[complex-image guidance](https://www.w3.org/WAI/tutorials/images/complex/)
supports providing the essential information beyond a short image label.

### 19. P2 — The comparison chart's arrow contradicts its encoding

**Reproduced/source-confirmed.** Larger objects appear farther right at
[R/reporting.R:576](../R/reporting.R#L576), while
[line 623](../R/reporting.R#L623) labels that direction `lower ... →`. The side
text says left is lower. There are no numeric axis ticks. The example also gives
47–67 KB model sizes substantial prominence without any stated resource need.

**Repair and acceptance:** fix direction and quantitative axes. Make performance
and material prediction disagreements the ordinary comparison; make resource
analysis explicit and relevant to a supplied constraint. A reader must be able
to obtain the same quantitative comparison from graph and table.

### 20. P2 — The reading order delays the explanations and their caveats

**Measured, plus design judgment.** At 390px the example is 13,420px high;
Patterns starts at y=8,279 and Reliability at y=9,803. It has nineteen metric
cards, twelve tables, and two disclosures. Comparison alone occupies 4,610px.
Navigation is confined to the header; uncertainty has no dedicated section ID.
At desktop width the page is roughly 7,800px tall.

The large `Understanding outcome` heading and generic introduction displace
model identity, study boundary and caveats. The mobile opening emphasizes the
improvement statement before the 48-row caution is visible. Screenshot additions
make the output visible, but cannot fix this hierarchy.

**Repair and acceptance:** begin with analysis identity, primary/baseline result,
and the leading caveat together. Keep navigation available. Move exhaustive
metrics and family-capacity teaching into details. Test whether users can find
the validation design, positive class and first material limitation without
traversing the entire report.

### 21. P2 — Findings do not lead to their evidence, including in the console

**Observed.** [R/reporting.R:1413](../R/reporting.R#L1413) renders generic next
actions without naming affected models/features or linking their evidence. The
example has only seven links, all skip/header navigation. Disagreement extrema
lack the candidate identities that produced them. The console at
[R/autoxplain.R:550](../R/autoxplain.R#L550) prints scores and a tuning suggestion,
but omits evaluation notes and leading explanation findings.

**Repair and acceptance:** findings should identify an entity, evidence and a
destination. Give the corresponding R command in a detail block. The console
should name the primary model, evaluation role, leading caveat and a relevant
next step. HTML and R should agree about what needs attention.

### 22. P2 — Mobile and print checks miss information loss

**Measured.** At 390px, comparison labels scale to about 4.4 screen pixels.
Five tables overflow their containers. Axe-core 4.13.0 flagged their missing
explicit keyboard-focus treatment, but manual Chrome 152 testing successfully
tabbed to and scrolled them. This is an unresolved cross-browser portability
issue, not a demonstrated Chrome keyboard failure. Automated contrast checks
also had incomplete results requiring review; no full accessibility claim is made.

The default A4 browser PDF had eight pages and omitted the contents of both
closed disclosures while retaining `Open the technical evidence audit`.
[R/reporting.R:1526](../R/reporting.R#L1526) does not resolve disclosure state for print.

**Repair and acceptance:** set a readable minimum at actual rendered chart size;
use a mobile-specific table/plot presentation where needed. Name scroll regions
and verify keyboard operation across supported browsers. Print must include
required evidence or state its durable location. Open/closed UI state must not
silently change the scientific record.

## Product, documentation and writing

### 23. P2 — The documented narrative call ignores retained explanations

**Reproduced.** `autoxplain(iris, "Species", seed=2026)` retained eight importance
rows and three effects. `generate_natural_language_report(result)` nevertheless
omitted that evidence and the audit findings.
[R/llm_integration.R:152](../R/llm_integration.R#L152) chooses audit context or
analysis context rather than merging them; [line 714](../R/llm_integration.R#L714)
reads only separately supplied importance. The local memo substitutes generic
model-family prose, including the broken phrase `can represent none unless
encoded in features`. No hosted output was needed to reproduce this.

**Repair and acceptance:** make one typed narrative context from evaluation,
retained explanations and provenance. Explicit arguments can override matching
components. The documented call must describe actual available evidence without
recomputing it; missing explanations must be identified as not run. Prompt
instructions should not be advertised as verified factual grounding.

### 24. P2 — The entry point is simple, but the surrounding choices are not

**Observed facts plus product judgment.** There are forty exported functions,
thirty `autoxplain()` arguments, and five reporting/dashboard routes. Some legacy
arguments are unused or ignored. The getting-started vignette is 610 source lines
and combines beginner setup, advanced tuning, provider setup and many diagnostics.
Yet it does not give a complete realistic workflow for selecting predictors,
excluding a future-only field, and inspecting imputation/novel-level choices.

**Repair and acceptance:** keep the public routes obvious: `autoxplain()`,
`predict()`, `render_model_report()`, and `explain_model()` for existing models.
Mark legacy routes with replacements and a measured retirement policy. Split
the introduction into one complete first analysis and focused task articles.
Include a realistic example with identifiers, missingness, categories and a
known future-only field; readers must be able to explain which columns entered
the fit and what preprocessing changed.

### 25. P2 — Documentation drift and feature accumulation weaken the product

**Confirmed inconsistencies plus strategic judgment.**
[The introduction:408](../vignettes/autoxplainr-introduction.Rmd#L408) promises a
dated provider comparison that no longer exists; line 389 still advertises free
allowances. [The release checklist:43](../.github/RELEASE_CHECKLIST.md#L43) requires
fresh Win-builder evidence and later CRAN submission, while the release record
treats CRAN as a separate future action. This is a lifecycle-documentation
inconsistency, not evidence that the GitHub CI gates failed.

The proposed roadmap spans resampling infrastructure, conditional importance,
refit inference, conformal prediction and model-class coverage. Each needs
substantial statistical validation. Together they risk turning the short-path
package into another general modeling framework before its core is dependable.
Existing [modelStudio](https://modelstudio.drwhy.ai/) already offers automated
explanations, descriptions and shareable HTML;
[modelDown](https://modeloriented.github.io/modelDown/) combines performance,
importance, response and model comparison. [DALEX](https://jmlr.org/papers/v19/18-416.html)
and [xplainfi](https://mlr-org.github.io/xplainfi/articles/inference.html) provide
established explainer and importance-inference foundations.

**Repair and acceptance:** separate GitHub and CRAN checklists; remove stale
commercial promises. Defer new estimator families until existing semantics and
onboarding are sound. Publish an identical-data/split end-to-end comparison and
task-based usability results before claiming a practical advantage. Keep the
README's current honesty that superiority has not been demonstrated.

## Writing changes: remove abstract authority, keep useful explanation

This review does not infer authorship from prose. The problem called “AI slop”
here is vague authority language, repeated caveats, generic filler and promises
unconnected to observable behavior. Much of the rewritten README is already clear.

| Current wording | Why it should change | Better direction |
|---|---|---|
| `Safe local defaults` — DESCRIPTION | Local execution and default splitting do not establish statistical safety | Name the model, baseline and holdout behavior |
| `provenance-rich evidence audits` — DESCRIPTION | Abstract stacked nouns | `diagnostics retaining settings, predictions and permutation draws` |
| `the default serious tabular comparison` — introduction:137 | Status claim without a criterion | `a six-family preset requiring optional engines` |
| `reviewed prior knowledge` — introduction:275 | Unspecified reviewer/standard | Describe the nonlinear effects and interactions each family can represent |
| `advanced reliability layer` / `robust descriptive evidence` — audit.R:3 | Oversells a collection of limited diagnostics | Name shuffle variability, association and supplied-model disagreement |
| `stable marginal evidence` under “How to communicate it” — audit.R:318 | A label is not a usable explanation | Give a scoped sentence about this fitted model and these rows |
| `Report model-class importance ranges` — audit.R:449 | Sounds broader than the supplied fitted models | `Report importance ranges across these supplied fits` |
| Repeated `not a certification` — reporting.R:1219 | Repetition does not resolve the grade's meaning | Remove the aggregate grade and explain each diagnostic once |
| `can represent none unless encoded in features` — llm_integration.R:1238 | Broken template grammar | Explain additive linear terms and whether nonlinear/interaction terms were supplied |
| `Inspect repeat distributions and dependence` — audit findings | No affected entity or useful destination | Name the model/feature and link its draws and dependence evidence |

## A less generic report: concrete design brief

The proposed identity is an **analysis brief with inspectable evidence**. A new
palette alone will not accomplish this. The forest-green color can remain; the
repeated eyebrow, oversized question, paragraph and four-card layout should not
determine every section.

| Surface | Shown immediately | Available on demand |
|---|---|---|
| Analysis identity | Target and supplied units, primary model, selection basis, evaluation boundary | Recipe, provenance and run details |
| Result | Primary/baseline comparison and leading caveat together | Metric definitions and all secondary scores |
| Evidence needing attention | Named, prioritized findings linked to the affected evidence | Diagnostic derivation and thresholds |
| Feature evidence | Faithful quantitative effects, support, model/event scope and qualifications | Permutation draws, methods, other candidates |
| Candidate sensitivity | Compact performance comparison and material prediction disagreements | Family descriptions and requested resource analysis |
| Reproduce/share | Relevant R commands, export scope, version and data identity | Complete audit appendix |

Implementation should start with a report view model built from typed evidence
objects. Each diagnostic needs scope, status, units, affected entities, evidence
references and a concise interpretation. Render HTML, console summaries and
local/remote narrative contexts from that common representation. Keep formatting
out of numerical functions and calculation out of HTML construction.

Use aligned numeric tables, real axes, compact identity fields and section
numbering. Reserve color for model identity and evidence status. Use whitespace
and rules instead of boxing every statistic. Technical identifiers belong in
details; display names should remain consistent. Domain labels must be supplied
by the user or explicit example data, never invented by a narrative model.

Test the design through tasks, not only screenshots:

1. A novice identifies the target, primary model, positive class, split and main
   caveat from the summary, and explains them correctly.
2. A reader traces each finding to named model-feature evidence in one action.
3. An experienced user obtains the underlying values and corresponding R call
   without reverse-engineering the report or recomputing an expensive audit.
4. A reader interprets an irregular ALE curve's coordinates, effect units and
   support correctly; a scale change is never invisible.
5. At 390px and 200% text zoom, important labels remain readable and keyboard
   access is verified. The print appendix preserves necessary evidence.
6. In a small study, beginners and experienced R users complete the same analysis
   tasks using AutoXplainR and an established workflow. Record time, errors and
   mistaken interpretations, including failures.

## Recommended work sequence

1. **Correctness repair release:** findings 1–9 and 13–16; a shared evaluation and
   prediction contract; adversarial regression tests. Fix returned ALE values
   and plotted geometry together. Document which previously produced outputs
   may differ. Preserve the published 0.3.0 tag.
2. **Evidence interpretation:** remove the overall grade, preserve raw context,
   label diagnostic limitations, and unify narrative/report evidence. Explicitly
   resolve the H2O preprocessing scope.
3. **Report redesign:** implement the view model and analysis-brief layout,
   quantitative plots, linked findings, print appendix and responsive reading
   path. Validate with the tasks above before taking new screenshots.
4. **Product proof:** short real-data tutorial, lifecycle cleanup, dependency
   support evidence and end-to-end competitor/usability comparison. Reprioritize
   rolling-origin or other new methods only after the core passes these gates.

## What should survive the overhaul

The local tuner relearns preprocessing inside folds from raw outer-training
rows. It records failed configurations and refits. Holdout ranks do not silently
replace the primary model. Paired bootstrap resampling uses common units for
model and baseline, retains whole groups, rejects temporal IID inference and
discloses omitted fitting uncertainty. Ordinary metric definitions and several
edge-case implementations are thoughtful. The exact-source release checks and
checksums are useful. The candid undercoverage result, local default, preserved
R objects and clear absence of CRAN acceptance should remain.

Those strengths make this worth repairing. They do not neutralize the numerical
and interpretation failures identified here.
