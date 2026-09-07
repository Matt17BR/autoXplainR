# Structured validation journeys, 0.6.0

Executed on 2026-09-07 using the current development source loaded with
`pkgload::load_all()`: AutoXplainR 0.6.0, R 4.5.2, Linux x86_64. This is a
synthetic acceptance walkthrough, not a participant study. Source-unit checks
ran against the original data, followed by actual browser navigation and
screenshots of the generated reports. No hosted service or H2O was used.

## Grouped tuning: previously unseen dispatch sites

The synthetic input contains 133 shipments from 30 sites, with distance, load,
shift and a continuous delay outcome. Site sizes vary from three to six rows.
Generation and fitting seed: 61031. The public call used `model_set = "tuned"`,
`portfolio = "core"`, six scheduled configurations, three folds,
`validation_split(group = "site")`, and `missing_value_strategy = "drop_rows"`.
Two original loads were missing.

- The outer split has 24 training sites and six evaluation sites, with no site
  overlap. Every original training site belongs to exactly one inner fold.
- Training contains 101 raw and 100 processed rows; evaluation contains 32 raw
  and 31 processed rows. The row ledger identifies exactly the original missing
  rows as preprocessing exclusions, retains unique source keys, and maps all
  processed positions back to the original row names.
- The `site` column is absent from model inputs and retained in raw partitions
  and aligned evaluation context. The Data view reports 24 versus six distinct
  sites when explicitly exported as context. Switching load from raw to
  processed values changes the missing count from one per partition to zero
  and the displayed row counts accordingly.
- The automatic paired interval uses **six groups**, not 31 observations. The
  overview reports primary-minus-reference RMSE interval **-1.112 to -0.728 hours**,
  states that independent groups are assumed, and visibly warns that fewer than
  20 sampling units may give unstable endpoints. It excludes fitting and
  selection uncertainty. This small group count does not establish reliable
  interval coverage or transport to other sites.

## Chronological comparison: later dispatch dates

The synthetic input contains two shipments on each of 60 dates from 2025-01-01
through 2025-03-01. Generation and fitting seed: 61032. The public call used
`model_set = "comparison"`, `validation_split(time = "day", gap = 2)`, and
`missing_value_strategy = "drop_rows"`. Two original loads were missing.

- Training ends on **2025-02-15**: 46 distinct dates and 92 raw rows.
- The gap is **2025-02-16 and 2025-02-17**, containing four original rows. Those
  rows are retained as excluded context, not assigned to training or evaluation.
- Evaluation comprises **2025-02-18 through 2025-03-01**: the latest twelve
  distinct dates and 24 raw rows. Date ties remain together, and the training,
  gap and evaluation dates are strictly ordered.
- Dropping the original missing rows leaves 91 training and 23 evaluation rows.
  Raw values, processed positions and evaluation date context agree with the
  original source. The date column remains excluded from the predictors.
- The overview and Checks view both state: “Temporal evaluation requires a
  dependence-aware uncertainty method, not an IID bootstrap.” No automatic
  paired interval is supplied. The comparison uses predefined models; this
  walkthrough does not claim rolling-origin temporal tuning.

## Deliberately limited explanation budget and copied predictions

Both reports were rendered with `max_models = 1`, `top_features = 1` and
`n_repeats = 1`, using the complete fitted evaluation results. Row export was
explicit; split context was explicitly included with
`report_data_control("rows", context_columns = "site")` or `"day"`. Context
columns are intentionally omitted by default, so a context-export request was
needed to inspect them in the Data view.

Selecting an unaudited model in Feature effects displays that its explanations
were not computed, together with the command to increase the model budget.
Its predictions and evaluation scores remain available. Checks state that one
shuffle does not provide a Monte Carlo interval and that more shuffles address
Monte Carlo error only; they do not add evaluation observations. This was a
deliberate budget restriction, not an explanation failure.

For every retained model, the reviewer opened **Use this fitted model in R**,
copied its actual displayed call, and executed it on five retained evaluation
rows from the original raw data. The raw frame included the site/date context.
All **eight copied calls** matched direct predictions from the corresponding
explainer on the aligned processed inputs, with maximum absolute error zero.
The calls selected the intended model IDs, including both alternatives and the
reference; none silently used a different model.

## Acceptance and evidence

**41 original-unit checks passed**, followed by browser verification of raw and
processed counts, split-column roles, uncertainty messages, limited explanation
coverage and the eight copied prediction calls. No material production issue
was found in these two supported journeys. The walkthrough exercises regression;
it does not establish every grouped classification or missingness combination.

Local evidence is under
`/home/mmazzarelli/.cache/autoxplain-overhaul-0.6/structured-journeys/`:

- `generate.R`, `browser.py`, `copied-predictions.R`: exact executed workflow and
  verification scripts;
- `grouped.rds`, `temporal.rds`: original data plus fitted results;
- `original-unit-checks.json`: all membership and provenance assertions;
- `browser-observations.json`: displayed counts, roles, interval messages,
  omitted explanations and the actual model-specific prediction commands;
- `copied-prediction-checks.csv`: all eight calls and independent comparisons;
- HTML reports, overview/Data/limited-budget screenshots, and `session-info.txt`.

The test harness initially tried to inspect an unexported context column before
requesting `context_columns`; it was corrected to use the documented explicit
export. This was not a package failure and did not require refitting or changing
the source data or seeds.
