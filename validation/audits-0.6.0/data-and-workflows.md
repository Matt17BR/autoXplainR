# Data exploration and workflow audit

This read-only audit examined baseline commit `12e9ba1` (0.5.0) before the 0.6.0
data module was implemented. It distinguishes observed behavior from proposed
product choices. Later implementation tests below are replacement acceptance
checks, not retrospective evidence that the baseline was correct. No participant
usability study was performed.

## Verified baseline gaps

1. **The relationship matrix does not support investigation.** In baseline
   `R/report_explorer.R:444–472`, the matrix uses at most 12 processed training
   predictors. Its cell handler (`inst/report/explorer.js:129–131`) changes only
   a sentence containing the association method and sample count. There is no
   paired observation plot, outcome relationship, variable search, split
   comparison or way to inspect predictors outside that subset.
2. **Raw training and excluded observations cannot be reconstructed.**
   `R/guided_workflow.R:69–70` holds raw training temporarily for tuning, while
   the returned object (`:154–159`) stores processed training/evaluation tables
   and raw context only for retained evaluation rows. `data_info()` in
   `R/data_preprocessing.R:398–407` records types and missingness counts, not
   distributions. A new UI cannot honestly call those processed values raw.
3. **Row identity is insufficient for linked inspection.** Error tables in
   `R/report_explorer.R:582–585` use evaluation positions; the ordinary split
   helper (`R/guided_workflow.R:336–345`) returns partition tables without
   explicit original-source position vectors. Arbitrary row names are neither
   a stable cross-source key nor suitable default browser identifiers.
4. **Default HTML already contains individual values.** Regression prediction
   tables (`R/report_explorer.R:573–617`) and SVG coordinates/tooltips
   (`:679–694`) embed outcomes and predictions. Classification mistake tables
   embed individual class labels and probabilities. Rendering has no export-mode
   argument in the baseline (`R/reporting.R:39–51`). Sharing guidance in
   `README.md:264–266` and the introduction `:130–131` mentions names and
   diagnostics but does not describe these HTML records adequately.
5. **Displayed cases have narrow, different selection rules.** The prediction
   scatter samples at most 400 evenly spaced positions (`R/report_explorer.R:665`),
   while error tables show ten worst cases. Neither supplies linked input rows.
   A plot sample and an error-ranked case selection answer different questions.
6. **The documented workflow is model-first throughout.** Baseline PRODUCT,
   ROADMAP and introduction describe model comparison and fitted effects, but
   provide no report task for understanding an outcome distribution, missingness
   by group, raw category mapping, or a nonlinear data relationship. This is a
   product gap, not a claim that all users must follow one reading order.

A small synthetic reproduction confirmed: raw training had no complete retained
snapshot; one missing training input became non-missing after preprocessing;
the raw evaluation category `new` survived in `evaluation_context` while the
model table contained `a`; default HTML contained a distinct individual outcome
`7000.001` and a `Row 1: observed` tooltip. An unrelated evaluation-only marker
was retained locally but did not appear in that HTML. This last result does not
establish a general privacy guarantee.

## Proposed product and data contracts

Replace the limited **Input relationships** page with **Explore data**, keeping
one searchable column selector and useful full-data views. The initial selected
column is the outcome; a reader can inspect every exported model input without
being restricted by importance ranking.

- **Distributions:** comparable training/evaluation bins or category bars,
  missing/usable counts, distinct counts, numeric summaries, and explicit raw
  versus processed populations.
- **Relationships:** selected input versus outcome or another input; sparse
  joint count/density plots, categorical count tables, and observed target
  summaries by bin. A correlation matrix can index these views, but cannot
  replace them.
- **Rows and preprocessing:** a supplied/retained/excluded ledger by default;
  explicit row export unlocks source positions, raw/processed values, linked
  scatter and prediction cases, filtering and sorting.

Use `report_data = "summary"` by default, `"rows"` as a one-argument opt-in,
and `"none"` to omit dataset and case-level displays. A compact control object
can specify selected predictors, explicitly permitted context columns, row
budget and sampling seed. Apply the mode to the entire report, including older
prediction tables, SVG tooltips, hidden panels and serialized payloads.

Summary mode should remain useful: full-data variable profiles, missingness,
target/split comparisons and aggregate pair densities. It must not pretend to
support arbitrary exact multi-column filters without row data. Row-mode filters
describe exported observations and never refit models or alter official scores.
Repeated exploration of held-out outcomes creates exploratory subgroup evidence,
not a fresh unbiased final-test estimate.

Retain a versioned local context with raw training/evaluation partitions, optional
validation exclusions, a typed column/role registry, original-source positions,
processed row positions, exclusion reasons and split/recipe provenance. Input
row names should not be browser keys. Keep this local retention distinct from
HTML export. Older results must show a specific unavailable-raw-data state rather
than reconstructing original categories or missing values from transformed data.

Numeric comparisons need common bins with evaluation overflow; category grouping
must preserve missing versus literal text and distinguish previously observed
from novel categories. Every chart needs its population and denominator. Small
pairwise association is not independence; changed supplied distributions do not
identify a cause or establish future distribution shift. No data-quality or
drift grade is proposed.

Aggregate export is not anonymization. Rare category labels, small-group values
and fitted model details may disclose information. Row mode should explicitly
state that recipients receive all embedded records, including records hidden by
filters. Do not describe offline operation or hidden controls as privacy measures.

## Acceptance tasks and implementation checks

Use a reproducible messy fixture with missing values, a novel evaluation category,
time/group context, a removed column, nonlinear structure and known prediction
errors. Require a reader or test to:

1. Find the outcome distribution and understand its class/event meaning.
2. Quantify a known split difference on comparable axes with correct counts.
3. Identify missing/novel raw values and the exact preprocessing change.
4. See nonlinear joint structure that a single correlation would miss.
5. Link a prediction error back to the correct original observation after split
   reordering and row removal.
6. Apply and clear filters; verify the resulting counts independently while
   official model scores and selection remain unchanged.
7. Confirm summary HTML lacks a row payload and individual prediction displays;
   unrequested context values must be absent from every serialized channel.
8. Confirm row manifests and reproducible samples match actual embedded records.
9. Handle constants, all-missing values, dates, ordered categories, high-cardinality
   columns and unsupported types truthfully.
10. Complete the tasks offline, by keyboard and on mobile; preserve meaningful
    state and scope in print.

The replacement module's numerical/export checks are in
[`test-data-profile.R`](../../tests/testthat/test-data-profile.R), covering bin
boundaries and overflow, independent pair counts, category/missing distinctions,
row mappings, changed attachment rejection, sampling, old results and serialized
export boundaries. Browser task execution and release-source acceptance must be
recorded separately. The durable task runner is
[`check-data-explorer.py`](../check-data-explorer.py), using the original-row
oracles from [`render-exploration-fixtures.R`](../render-exploration-fixtures.R)
and [`render-explorer-cases.R`](../render-explorer-cases.R). It also deliberately
corrupts missing counts and source-row keys to test whether those task checks
detect the wrong answers. Passing a script or screenshot inspection is not evidence
of participant usability, general privacy protection or accessibility conformance.

## Implementer walkthrough after the repair — 2026-09-07

The regenerated delivery and churn previews were read in Chromium at 1440 and
390 pixels using screenshots, accessible control names and visible tables.
Answers were recorded before reconstructing the seeded source data or computing
native `lm`/`glm` predictions. This was an implementer's task walkthrough, not a
study of novice participants; author familiarity remains a limitation.

| Reader task | Answer obtained from the report and then independently verified |
|---|---|
| Put delivery accuracy in context | Linear regression is the training-selected primary; 72 test rows, RMSE 3.00 hours versus baseline 7.565. Native predictions give 3.000101 and 7.564512. |
| Locate and inspect missing inputs | The parcel-weight badge leads to five missing training values and none in evaluation; processed values have zero in both. Reconstructed source rows 17, 39, 112, 206 and 283 are all in training. |
| Investigate overlapping inputs | Raw distance and planned route time have signed Spearman association 0.9931 in training (n = 288) and 0.9900 in evaluation (n = 72), matching direct unbinned calculations. The report does not describe this as causal. |
| Trace a large error to its source | Delivery record `data:79` has observed 24.2611 hours and predicted 15.0714. Its link opens Records, the evaluation split and the correct page, with distance 35.8, parcel weight 5.1, backlog 10 and economy service. These match the original source row. |
| Understand churn balance and the event | The header names event `yes`; the distribution and exact table show training 138 no / 54 yes and evaluation 35 no / 13 yes. Both real classes are visible. |
| Explore a decision cutoff | Keyboard movement from 0.50 to 0.80 changes false positives from 3 to 0 and false negatives from 8 to 13. Precision becomes undefined; calibration and the official log-loss comparison stay unchanged. Direct native probabilities reproduce every count. |
| Inspect and filter cases | Churn mistake `data:50` links to yes, 49 months, one support call and monthly plan; native probability of yes is 0.117948. Filtering evaluation rows to monthly plans gives 27 of 48, independently counted from original source rows. The screen names this an exploratory filtered sample. |

At 390 pixels, native Tab then Enter moves from Distribution to Relationships
with a visible focus ring. The event-rate chart remains readable and distinguishes
observed rates from fitted probabilities. Controls and column choices require
vertical scrolling before the plot; this walkthrough does not establish that the
layout is optimal for a new reader. The R prediction example follows the selected
cutoff, and the export disclosure explains that sharing includes hidden records.

The walkthrough exposed a misleading fixed missing-count badge and a repeated
sharing sentence. The badge now follows the chosen raw/processed values, split
and filters, with an accessible scope description; the duplicate sentence was
removed. The categorical note now says categories are *ordered* by training
counts, avoiding confusion with evaluation-only bars. The excessive precision in
regression case tables was also repaired: the refreshed preview displays
`data:79` as 24.26 observed, 15.07 predicted and 9.19 residual. Exact values remain
in the cell titles and exported payload; a fresh screenshot and accessibility
snapshot confirmed the readable values.

The [data browser gate](../check-data-explorer.py) passed all 209 checks on the
final regenerated canonical reports, including seven checks of badge scope
against original-data oracles. The fresh public delivery preview was also
reopened to confirm the processed/evaluation-only badges and sharing text.
Other release gates are recorded separately. Local screenshots, accessibility snapshots and
the browser-first answer log are in `autoxplain-overhaul-0.6/novice-journey` under
the maintainer's cache. The source generators remain
[render-example.R](../render-example.R) and
[render-explorer-cases.R](../render-explorer-cases.R). The introduction vignette's
three training / one evaluation missing distances belong to its separate
240-row chronological example, not this 360-row parcel-weight demonstration.
