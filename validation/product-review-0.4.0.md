# Product review: what was lost after 0.1

This review was prompted by the maintainer's rejection of the 0.4.0 report.
The maintainer described it as generic, flat and unhelpful for comparing models.
That is actual user feedback. The walkthroughs and decisions below are the
implementer's evaluation, not results from recruited participants.

## The failure

The package moved from a model-comparison tool to a document about the validity
of a mostly pre-specified model. Correctness improved, but the main user tasks
became harder or disappeared from the default path. Releasing on numerical and
browser checks did not establish that the product was worth using.

The source comparison uses tags v0.1.0, v0.1.2 and v0.4.0. The 0.1.2 dashboard
opened with performance versus training time, feature-importance plots and model
navigation. It calculated importance for multiple models and included model
prediction correlations. Some old calculations and fallbacks were unsound; the
useful interaction model should have survived their repair.

| User task | Regression in 0.4.0 | Required repair |
|---|---|---|
| Fit alternatives with one call | Default fits a reference model and a baseline; search requires discovering another mode and its dependencies | Make the dependency-light, training-CV model search the default; keep the fast reference workflow explicit |
| Decide which models are worth considering | One primary score dominates; candidate scores occur after long sections about checks and effects | Open with all retained models, their test scores and the separate training-selection result |
| Compare performance and cost | Resource plot is collapsed late in the report; default axis is object size | Show a live metric/resource comparison beside the model table; expose training time, prediction time and size |
| See which inputs matter | Importance is a repetitive table of identical interpretation sentences | Use ranked, signed bars with values and a model switch; keep intervals in detail |
| Understand fitted patterns | Three effects describe only the primary model and use generic range summaries | Link model and feature selection to actual stored curves, support and named endpoint changes |
| See relationships between inputs | A strongest-pair threshold warning replaces exploration; reciprocal warnings repeat | Show signed numeric correlations and explicitly named mixed-type associations, with pair inspection |
| Compare what models predict | An abstract range summary replaces an immediately readable comparison | Show prediction agreement by task, and inspect selected-model errors with clear row/sample scope |
| Learn without reading a methods paper | Repeated non-causality, independence and uncertainty qualifications dominate | Put one short explanation beside each chart; place derivation and full diagnostic evidence in Methods/Checks |
| Trust a control | Report has almost no useful controls; no test asks whether changing a model changes the explanation | Test linked state against exact model/feature IDs, scores, predictions and curves |
| Understand the package's promise | Claims of usability coexist with an entirely deferred study protocol | Make ordinary task walkthroughs a release gate now; publish observed limitations and never equate them with a participant study |

## Boundaries for the replacement

A report earns its space by helping answer a modeling question. No decorative
score, generic praise, fabricated timing or repeated disclaimer belongs on the
opening screen. No best-model badge may silently turn a test ranking into a
training-selection decision. A model switch must change every model-specific
view, including its label, metric, feature importance, effect and prediction
plot. Missing results must stay missing. Correlation must retain its sign;
mixed-type association must never masquerade as signed correlation.

The first screen must expose model count, evaluation size, scores and costs.
A reader must reach feature importance, a fitted effect and input relationships
in at most two deliberate actions. Core evidence must work offline. No-JavaScript output must retain all evidence. Print must preserve the
selected view, model and report identity. Keyboard and narrow-screen users
must have a usable route through the same tasks.

## Acceptance walkthrough

Run this on a regression report, a binary report and a multiclass report, plus
single-model and deliberately unavailable-evidence cases. Record the generated
artifact, observed answers, failed interactions and revisions before release.

1. Identify the lowest evaluation loss, the model selected without those rows,
   and the reason they can differ.
2. Change the cost axis; read the actual cost and performance for two models.
3. Switch to another model, identify its top input and inspect its fitted curve.
4. Find a strongly related pair of inputs and explain the sign/type of association.
5. Find an observed prediction error or classification mistake for the selected model.
6. Obtain the R command to predict with the inspected model.
7. Repeat navigation and model selection with a keyboard and at a narrow width.
8. Open offline and inspect printed output; confirm evidence was not lost.

Failures in these tasks block publication. Passing unit tests or an axe scan
alone cannot satisfy this checklist. Formal comparative participant research is
separate from this immediate product acceptance work.

## Walkthrough performed during the repair

The fixtures are generated by `render-explorer-cases.R`; browser actions and
numerical assertions are in `check-explorer.py`. The developer inspected actual
screenshots of the comparison, feature, relationship and prediction tabs on
desktop and phone, and rendered the selected-feature PDF. The following are
observed answers from the fixed datasets, not invented example conclusions.

| Task | Observed answer |
|---|---|
| Compare delivery models | Linear regression RMSE 2.997 hours; neural network 3.007; tree 3.952; baseline 7.565. The training-CV choice is linear regression. |
| Compare a relevant cost | The tree occupies about 84.91 KiB versus 115.97 KiB for the linear model. Changing the cost selector exposes that tradeoff. Timings are measured per run and can change. |
| Inspect a delivery input | Linear importance ranks distance first, followed by the overlapping planned-route estimate. Selecting distance shows a centered ALE increase of 11.97 hours across the displayed range. |
| Inspect related inputs | Distance and planned-route estimate have signed Spearman correlation about 0.99 on 288 preprocessed training rows. Selecting the cell supplies the exact method and count. |
| Find delivery prediction errors | The linear model has mean absolute error 2.481 hours and maximum absolute error 9.190 hours on 72 evaluation rows. The largest-error table exposes the affected row positions. |
| Inspect binary predictions | The churn example's selected logistic model has log loss 0.4920 and 11 mistakes on 48 rows. Probability refers to `yes`; the report states the 0.5 classification threshold. |
| Inspect multiclass predictions | The flower example's selected neural model has log loss 0.1662 and 2 mistakes on 30 rows. The class selector exposes all three probability curves without changing the all-class importance measure. |
| Use the inspected model | Switching models updates the displayed `predict(result, new_data, model = "...")` command and the prediction and feature panels. |

The review caused concrete revisions after the first implementation:

- The first comparison layout still scrolled unnecessarily. Scores and the cost
  plot now sit beside each other at desktop width.
- Primary-only screening hid inputs used by other models. The audit now covers
  the union of their leading inputs, with a nonlinear regression counterexample.
- Three curves were insufficient when eight inputs appeared selectable. Every
  displayed top input now has a retained curve or a stated calculation failure.
- Mobile charts initially required unnoticed horizontal scrolling and navigation
  could obscure the report heading. Numeric plots now fit the available width,
  with larger labels; tab changes preserve the heading.
- The first mobile and no-JavaScript variants overflowed. Layout, keyboard focus
  and tooltip positioning were repaired and added to the browser gate.
- The initial feature PDF wasted a page and a focused native selector could lose
  its label. Print now uses explicit selection text and a compact two-column
  layout, with checks for the report title and selected model.
- Floating-point clock subtraction gave apparently tied fits a spurious cost
  advantage. Elapsed times now discard noise below a microsecond, with a
  counterexample checking that equal costs have the expected Pareto status.
- A live H2O report exposed missing optional characteristic fields. Cost
  enrichment now preserves its existing measurements and represents absent
  values as unavailable.

A deliberately disconnected model selector was tested outside the repository.
The task checker failed the model-switch assertion, exited 1 and recorded no
execution errors. This establishes that the check can reject a plausible-looking
but broken interaction. It is not a substitute for the numerical reference tests.

Remaining scope is explicit: the matrix shows at most 12 selected inputs; explanation budgets
can exclude models; temporal tuning is unsupported; Chromium automation does
not establish screen-reader or Safari behaviour. These limits are recorded in
the report or documentation rather than represented as successful checks.

### Model identity review

The next review exposed another regression: family names had replaced useful
fit specifications. The comparison now keeps a concise setting line per model,
with a keyboard-accessible detail view containing actual controls, fitted
structure, formulas, coefficients or tree rules, training selection and encoded
inputs. Model identity follows the feature and prediction selectors. The detail
view uses retained fitted objects; a requested maximum depth is labeled
separately from the resulting tree depth. Optional-engine effective settings
are checked as well as the core fits. The static HTML retains the specifications
when JavaScript is disabled.

The multiclass walkthrough also found indistinguishable rounded probability
axis ticks and first-class-only exploration. Tick precision now preserves small
signed changes, and all outcome classes have selectable, cached fitted curves.

The repeated in-browser journey used the model links, then the ordinary controls
rather than navigating directly to internal panels. It recovered these answers,
subsequently checked against retained R fits and predictions:

| Question | Answer recovered from the report |
|---|---|
| Is the tree actually ten levels deep? | No: `maxdepth = 10` is the cap; the fitted tree has depth 6 and 33 terminal leaves. |
| What is the linear service coefficient? | `servicepriority = -5.000612`; the formula and factor encoding can be inspected alongside it. |
| Does the neural model show the same service association? | Its PDP averages fall by 4.958273 hours from economy to priority; this is a fitted association. |
| Does changing tabs preserve the model? | Predictions keeps the neural model: MAE 2.485880, maximum absolute error 9.035591, and `predict(result, new_data, model = "neural_model")`. |
| Is the flower curve really class-specific? | For the same neural model and Petal.Width range, the ALE endpoint change is -0.02295566 for setosa and +0.5033818 for virginica. Overall log-loss importance stays unchanged. |

This pass removed the duplicated formula from the controls table, put the main
tree settings first, kept the close button available while details scroll, and
replaced irrelevant cross-family help with help for the inspected engine. The
ALE help now explains that a negative centered effect is not a negative
probability. A deliberately restricted model budget now offers a report command
instead of an empty feature selector. Regression, binary and multiclass examples
are published together so readers can inspect the supported task types before
installation.

The classification follow-up asked why similar accuracies can coexist with very
different log loss. The original mistake list could not answer that: it contained
only labels. It now shows probabilities for both the predicted and observed
classes and sorts by the probability assigned to the observed class, lowest
first. Unit checks include shuffled probability-column order and binary 0.5
ties; the browser checks every displayed mistake against independently assembled
R probability rows.

In the flower fixture, the tree and neural model both have accuracy 28/30, while
log loss is 1.213843 and 0.166199. The tree assigns probability zero to the true
class on evaluation row 15; the neural model assigns 0.165180 there. This now
provides a concrete reason for the score difference. Phone inspection initially
found the probability columns off-screen. They now sit below their class labels
in a three-column table, and browser checks reject horizontal overflow. The
active mobile tab is also brought into view on navigation and deep links.

The final CI browser pass caught a further 320px overflow in the multiclass
feature view. Repeating the task locally with DejaVu Sans reproduced it: the
model selector's flex item sized itself to its longest option. Its containing
label now respects the available width; the same wider-font case is checked for
every model. Failed layout checks save the affected report, screenshot and
element dimensions. Cost labels also no longer claim that a zero timer reading
means less than one millisecond: R documents platform-dependent resolution, so
the table shows `~0` with an explanation in the cost-measurement details.
