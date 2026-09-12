# AutoXplainR 0.7.0

- Tuning prepares one fold at a time and scores each fit from one prediction
  batch. Disabling retained out-of-fold predictions also avoids constructing
  their case records. Fold scores, selection and final evaluation remain
  available.
- Paired score intervals reuse fixed row contributions and draw observation
  indices directly. The full-evaluation resamples, paired comparisons and
  interval results remain unchanged.
- Large evidence fingerprints stream serialized values through a temporary file
  instead of allocating another complete raw copy. Hashes still cover every
  value and retain their previous format. Temporary files are removed after
  hashing, including when an operation fails.
- `evidence_summary()` exports explanation class counts as ordinary named values,
  so its documented JSON export also works with sampled classification audits.
  The audit retains its original class-count tables.
- Binary out-of-fold case losses now use the same probability clipping as fold
  scoring, so their pooled loss reproduces the reported CV score. A probability
  of 0.5 now selects the positive class in both out-of-fold records and public
  predictions. These repair older evidence inconsistencies; fitted probabilities,
  CV scores and model selection are unchanged.
- Additive grids expose GAM, BAM and discrete BAM fitting, including the actual
  solver, smoothing method and discretization resolution. Automatic computation
  choices are recorded separately from model selection. Smaller binary problems
  retain GAM; Gaussian regression can use BAM earlier when its estimated
  coefficient work is large. Final BAM fitting failures override a successful
  smoothing-optimizer status; the original
  warning and fitting stage are retained.
- Neural grids expose `maxit`, with a default allowance of 2,000 iterations
  instead of 500. Fits still stop earlier when they converge. The larger budget
  can recover previously excluded configurations and change model selection;
  unsuccessful fits remain excluded by default. Use `maxit = 500` for the former
  budget. Changing the budget preserves initialization for the same architecture
  and weight penalty.
- Boosting can use native categorical inputs without constructing a large
  contrast matrix. Automatic encoding is planned from the outer training inputs
  and fixed across cross-validation and refitting. Category levels and all
  preprocessing are still learned within each fold. Explicit encoding controls
  remain available because the representations can produce different models.
- Training/evaluation overlap checks use exact column grouping instead of
  serializing every full row, with an exact fallback for byte-marked text on
  older R versions. Regularized, forest, neural and MARS model calls
  no longer embed redundant training inputs and outcomes in saved fits. Their
  recorded settings and input reconstruction notes remain available.
- Reports store exported values in compressed columns and decode them locally
  as needed. Source-row identity, filtering and record lookup retain the complete
  exported sample. Large embedded scripts are assembled from smaller text blocks
  to avoid a reproduced WebKit parsing stall. Dense scatter plots show a
  disclosed subset of points.
- Cost-chart help explains what R object sizes count: retained diagnostics can
  dominate the estimate, shared data can be counted repeatedly, and native
  engine allocations can be absent. These values are not deployment-memory or
  saved-file sizes.
- Default report explanations use at most 5,000 evaluation rows; pairwise data
  summaries use at most 10,000 rows per partition. Model scores, univariate
  distributions and missing-value counts still use all available rows. Reports
  identify the samples, and both limits accept `NULL` to remove those caps.
  PDP curves keep their separate, recorded reference-row budget.
- Explanation sampling preserves original row identities and distinguishes
  PDP curve rows from support rows. Sampled AUC importance is unavailable when
  the sample lacks an outcome class, while the full evaluation score remains
  visible. Unique identifiers no longer receive a misleading perfect
  categorical association.

# AutoXplainR 0.6.2

- Grouped classification tuning balances outcome coverage as well as group
  sizes. It preserves whole groups and uses only training outcomes. Bounded
  randomized retries handle mixed-class groups; failures explain whether a
  class occurs in too few groups or the search could not find an allocation.
- Regularized models use sparse input encoding. Large categorical inputs no
  longer require a dense one-hot matrix. Native prediction parity is tested.
- Numeric scaling handles extreme finite predictor units without mistaking
  overflow or underflow for zero variance. Training centers remain finite on
  platforms without extended-precision sums, and scales preserve small
  differences between very large values. Encoded name collisions no longer
  remove otherwise usable MARS candidates.
- Automatic additive searches exclude folds with at least as many predictor
  terms as fitting rows. This disclosed resource policy avoids a reproduced
  expensive fit; externally fitted penalized GAMs remain supported.
- Rank-deficient linear fits record a fitting warning. Reports show rank
  deficiency beside settings and retain the actual scores, coefficients and
  design dimensions.
- Positive loss metrics have an explicit logarithmic score axis, so an unstable
  model need not flatten the other comparisons. Linear remains the default;
  exact scores and all models remain available on either scale.
- Wide reports prepare pair summaries and serialize row records faster without
  changing exported values. When an aggregate pair was not retained, explicitly
  exported rows can supply its plot, with the sample scope identified.
- Repeated unresolved permutation-interval findings share a compact evidence
  table while preserving every model, feature, claim and diagnostic link.
- Reproducible stress comparisons cover nonlinear interactions, more predictors
  than rows, rare events and Bank Marketing. Documentation separates the core
  defaults from optional learners and explains their measured strengths and
  remaining limits.

# AutoXplainR 0.6.1

- Cost charts draw the Pareto frontier: the best observed score available at
  each measured cost budget. The line does not imply intermediate fits. An explicit logarithmic
  cost axis separates positive costs spanning several orders of magnitude.
- Small cost ranges retain readable ticks. Almost equal scores no longer fill
  a chart with floating-point noise or lose their differences in point inspection.
- Data exploration reaches the plot sooner, uses a compact column selector on
  phones, and keeps binning explanations in keyboard- and touch-accessible help.
  Supporting details take less space and remain available in print.
- Model selection keeps parameters, scores and fold variation in one comparison.
  Select a configuration to inspect its folds, or expand the exact settings table.
  Fitting warnings open the failed configuration and its recorded errors.
- Regression reports place the three prediction diagnostics together. Feature
  charts remove repeated labels while retaining the prediction target and class.
- Evaluation intervals name their models, metric and units. A disagreement
  finding links to the recorded feature-importance ranges. Missingness changes
  explicitly use percentage points.
- GitHub screenshots show complete, focused report tasks instead of full-page
  posters. Browser checks now challenge vertical layout, print composition and
  the numeric geometry of the frontier, as well as the underlying values.
- Printed views keep model summaries, assumptions and importance values readable.
  Screen links and the selected view are restored after printing.

# AutoXplainR 0.6.0

- Reports add model-selection evidence: the exact selection threshold, searched
  settings and their rationale, fold scores, effective controls, optimizer
  status and final refit attempts. The minimum CV score, policy choice and final
  primary fit are separate recorded decisions.
- Explicit local optimizer nonconvergence excludes a candidate by default.
  `tuning_control(optimization_policy = "warn")` opts into retaining such fits
  with their status. Unknown convergence is reported as unknown.
- Data exploration retains original partitions and source-row positions. Raw
  and processed distributions expose missingness, imputation and category mapping.
  `report_data` controls aggregate, explicit sampled-row, or no-data HTML exports.
- Charts use responsive geometry, direct model labels and stable model colors.
  Model-effect comparisons share axes, categorical effects use separate named
  points, and support counts remain distinct from relative support measures.
- Prediction diagnostics expose aggregate errors and calibration. Binary cutoff
  exploration updates counts, rates, mistake ordering and matching R code for
  the selected model, including exact decimal boundaries. Explicit row
  exports link mistakes and their probabilities to original source records.
- Reports prepare predictions once per model per render and validate retained
  evidence against the current fit. Effect failures cover every model and class.
- Evaluation identity checks cover fitted state, captured prediction dependencies,
  data, class contracts, scores and substantive provenance. Changed predictions
  cannot be combined with saved evaluation evidence. Unchanged native and supported
  custom fits survive save/reload; dynamic external prediction state is unsupported.
- `evaluate_models()` brings already fitted models into the same comparison,
  data, effect and prediction workflow. Users declare the primary model,
  reference, event class and evaluation role; missing training history stays
  explicitly unavailable.
- `benchmark_predictions()` measures repeated predictions on a common batch,
  retaining raw timings, repeat variation and timer limitations. Attach it with
  `render_model_report(..., benchmark = bench)` to compare normalized costs.
- Model disagreement is available beside the compared models' evaluation scores.
  Individual case links respect the report's explicit row-export settings.
- Primary-versus-baseline paired intervals are included automatically when
  supported, with sampling assumptions and unavailable reasons recorded.
- `subgroup_performance(metric = ...)` inspects another supported loss without
  changing the result's official primary metric or model selection.
- Classification correlation plots now compare predicted-class agreement;
  relabeling classes cannot change the comparison. Public categorical PDP plots
  no longer connect unordered categories with a line or ribbon.
- Release archives require supported-platform checks as well as current and
  minimum optional-engine checks from the release commit.

Saved 0.5.0 results require recomputation of incompatible retained explanations.
For example, `render_model_report(result, "updated.html", top_features = 2,
n_repeats = 3)` recomputes a small audit without refitting. Older results cannot
recover raw source context that was never saved. Re-evaluate changed fitted
models through `evaluate_models()` instead of editing their saved metrics.

# AutoXplainR 0.5.0

- Reports show effective settings beside each model. Model details expose fitted
  coefficients or tree rules, exact controls, selection and preprocessing.
  Native tree metadata now includes its full controls; neural metadata includes
  the retained fitting controls and network size.
- Classification mistakes include probabilities for the predicted and observed
  classes, ordered by the lowest probability assigned to the observed class.
- Multiclass reports offer a class selector for fitted probability curves and
  retain all class-specific curves for reuse.

## Model exploration is the main workflow again

- `autoxplain()` now searches 15 settings across the core linear, tree and neural
  families using five training-only folds. One representative per successful
  family and a baseline are retained. Use `model_set = "quick"` for the previous
  reference-model default. Temporal splits require explicit quick/comparison
  mode until rolling-origin tuning is implemented.
- Reports use focused tabs for model comparison, features, input relationships,
  prediction errors, checks and methods. Scores and measured costs share the
  opening view. Short help is available on hover, focus and tap.
- Model and feature controls select the corresponding importance, fitted curve,
  prediction diagnostics and R command. The training-CV choice remains distinct
  from held-out score ordering. Metric and cost controls use retained R values.
- Feature screening now covers each audited model. The audit uses the union of
  their leading inputs, and up to eight curves per model are retained in
  `result$explanations$effects_by_model`. Primary-model curves remain available
  at `result$explanations$effects`.
- Input relationships show signed numeric correlations and explicitly named
  unsigned associations for categories. Classification prediction agreement
  compares class labels, including through the legacy dashboard helper.
- Print exports the selected view; no-JavaScript output exposes all evidence.
  Keyboard, phone layouts and actual report screenshots are covered by task
  checks against R-generated answer data. The README shows the working tabs.
- Cost enrichment preserves available leaderboard measurements when an engine
  omits optional model-characteristic fields. Missing measurements stay missing.
- Local elapsed times discard floating-point subtraction noise below a
  microsecond so that equal measured costs remain equal in Pareto comparisons.
  A zero timer reading is shown as `~0`; its resolution depends on the platform.
- Small probability effects retain distinct signed axis labels instead of
  rounding several different ticks to zero. Tiny table values use scientific notation.
- Report checks now read plotted values through visible axes. Deliberately
  false graphics, disconnected model selection and leaked fold imputation must
  fail before release; duplicated markup assertions were removed.

# AutoXplainR 0.4.0

## Correct calculations and evidence identity

- Binary explainers now track the probability event. Reversing `positive`
  complements native probabilities and corresponding effects. Custom numeric
  probabilities can declare their event with `probability_class`. Numeric and
  logical binomial GLMs retain their native success event under reordered
  evaluation factors; factor GLMs without retained response levels require an
  explicit event declaration.
- Invalid labels and factor-valued regression predictions fail before coercion.
  Hard classification labels remain labels; probability losses and effects
  require probability-capable adapters. Custom functions with `...` dispatch
  correctly and preserve their own error messages.
- Within-stratum shuffles preserve membership even for singleton strata. ALE
  values now use consistent endpoint coordinates and observed quantile
  boundaries that avoid unsupported empty bins. Irregular and tied inputs have analytic
  regression tests and an independent reference comparison.
- Paired model audits require identical ordered evaluation data, outcomes and
  event semantics. Content identities include fitted model state, prediction
  adapter code, data and predictions. Foreign or stale attached evidence fails
  with instructions to recompute; unchanged serialized evidence can be reused.
- Dependence screening checks the complete predictor context, including inputs
  outside the displayed feature subset. Low rank association is explicitly a
  limited screen, not evidence of independence.
- F1 is zero for an all-wrong classification with a nonzero denominator;
  balanced accuracy is unavailable when an outcome class is absent. Threshold,
  subgroup and overall metrics use the same definition.

## Preserve the analysis boundary

- Subgroup diagnostics retain raw evaluation context and row alignment through
  imputation, recoding, structured splits and row removal. Missing and novel
  categories no longer disappear into model preprocessing values.
- H2O internal cross-validation rejects external learned imputation, missingness
  column selection and ordinal conversion that would otherwise learn across its
  internal folds. This restriction is checked before starting Java.
- Neural learners retain and reuse their model-matrix blueprint, including
  ordered-factor contrasts. Fitted formulas no longer retain the calling
  session and sibling models through their environments.
- Optional engine minimum versions now match exercised versions. A dedicated
  CI job installs those exact versions in an isolated library and checks their
  adapters. The core still supports R 4.1; optional engines can require newer R.

## Reports that explain the analysis

- Replaced the card-heavy dashboard with an analysis brief: named primary model,
  selection rule, evaluation design, baseline comparison and leading caveat.
  Candidate scores remain descriptive when they did not select the model.
- Effects have quantitative axes, real numeric spacing, bin support and table
  equivalents. Resource comparisons move into an expandable section with
  correctly directed, labeled axes. Mobile tables remain readable; printing
  includes evidence inside closed disclosures without requiring JavaScript.
- Findings identify affected models and features and link to the relevant
  evidence and R inspection commands. Failed and unavailable checks are shown
  explicitly. Console output surfaces evaluation caveats and useful next steps.
- Local narratives use the retained feature evidence, effects and failed checks.
  Hosted narratives remain drafts for review; a format check does not establish
  numerical grounding.
- Gemini's shipped text model uses a tested low-thinking setting within the
  existing token budget. Requested settings are retained on success and local
  fallback; incomplete outputs remain rejected. Generated content is capped
  before fixed interpretation notes are appended.
- The README includes fresh screenshots of a reproducible synthetic delivery
  analysis. A shorter first-report tutorial covers real predictor choices,
  missingness, novel categories, chronological evaluation and saved recipes.
  Separate articles cover selection, diagnostics and existing fitted models.
- Validation now includes a disposition for every independent audit finding
  and an executable comparison with a DALEX/modelStudio workflow. No comparative
  human-usability or general superiority claim is made.

## Compatibility

Result and aggregate-export schemas are now **2.0**. Aggregate audit grades,
`stable_claim_rate`, and per-feature `evidence_grade` have been removed. Use
`shuffle_status`, `dependence_status`, per-model diagnostic counts and scoped
findings instead. Regenerate explanations and reports created with 0.3.0;
old calculation results are not repaired merely by installing this version.

`generate_dashboard()`, `create_simple_dashboard()`,
`create_model_comparison_report()` and `calculate_weighted_efficiency()` are
deprecated and issue a classed warning. Use `render_model_report()` and inspect
performance and resource measurements separately. Removal will occur no earlier
than 0.6.0. The package remains distributed on GitHub; this release is not a
claim of CRAN submission or acceptance.

# AutoXplainR 0.3.0

## One call, inspectable evidence

- `autoxplain()` now computes and retains explanation screening, a repeated
  permutation audit and up to three fitted effects. `explain = FALSE` keeps the
  fitting-only path. `report = "report.html"` writes HTML in the same call.
- `predict()` on a result applies its saved training recipe to raw rows and uses
  the primary model unless another model is explicitly requested. Regression,
  binary probabilities, multiclass probability matrices and class labels are
  supported. Dropped incomplete rows retain their positions as missing predictions.
- Reports reuse retained evidence unless explicit calculation budgets are passed.
  Failed effect calculations are recorded and displayed with their reasons.
- `evidence_summary()` exports aggregate evidence under schema 1.0 without raw
  rows, per-case predictions, group identifiers or fitted model objects.

## Validation and uncertainty

- `validation_split(group = ...)` keeps whole units out of fitting and inner
  tuning folds. `validation_split(time = ..., gap = ...)` reserves later times
  with tied-time and gap accounting. Split columns are excluded from predictors.
  Temporal tuning and structured H2O validation are explicitly unsupported.
- `performance_uncertainty()` provides a paired percentile bootstrap of primary
  and baseline losses, conditional on the fitted models. Grouped designs sample
  whole evaluation groups. Temporal IID bootstrapping is rejected.
- Reports can include these intervals with `uncertainty = TRUE` and now describe
  structured validation designs. Keyboard focus and print styles were added.

## Correctness and documentation

- Imputation learns values for all training predictors, including columns that
  become missing only in evaluation. Missing factor values can use a training
  level absent from the raw evaluation factor vocabulary.
- Evaluation accepts samples missing one or more declared training classes;
  undefined AUC, balanced accuracy and macro recall remain unavailable.
- Numeric inputs with incompatible evaluation types fail explicitly.
- Added independent statistical oracles, iml PDP comparisons, a reproducible
  bootstrap coverage experiment and an installed statistical-methods vignette.
- Rewrote the README, product description, provider guide and development plan
  around implemented behavior and explicit limitations.

### Compatibility

Existing fitting calls still return an `autoxplain_result`, with additional
components. Default calls now spend time computing explanations; use
`explain = FALSE` to retain the previous fitting-only workload. The package does
not claim stable cross-version RDS compatibility or CRAN acceptance.

# AutoXplainR 0.2.0 (2026-07-16)

## Beginner-first workflow

- Added a dependency-light `autoxplain(data, target)` path that creates a
  reproducible holdout, fits a simple baseline and an understandable primary
  model, and evaluates both with task-appropriate metrics and definitions.
- Added local linear, logistic, and multinomial logistic workflows for
  regression, binary classification, and multiclass classification. H2O
  remains available explicitly through `engine = "h2o"`.
- Added `model_set = "tuned"`, `tuning_results()`, and a ten-family learner
  registry. The recommended portfolio compares linear, regularized, additive,
  tree, forest, and boosting models; extended mode adds neural, radial-kernel,
  nearest-neighbor, and MARS models. Every backend shares one serializable
  prediction contract and is tuned with fold-specific preprocessing inside the
  outer training set.
- Added portfolio-aware automatic search budgets, space-filling low-budget
  grids, stable per-configuration seeds, explicit dependency/version status,
  retained family winners, paired out-of-fold predictions, and deterministic
  fallback when a full-data refit fails. Explicit model budgets are not silently
  capped.
- Added the optional `tuning_control()` escape hatch with validated custom
  family grids and exact budgets, RMSE/MAE or log-loss/Brier selection,
  ordinary supplied V-fold IDs, out-of-fold retention control, and configurable
  failure handling. Beginner portfolio defaults remain unchanged.
- The one-standard-error rule now uses a reviewed family priority followed by a
  family-specific flexibility proxy; raw proxy values are never compared across
  unrelated model families. The final holdout remains untouched until selection
  and full-training refit are complete.
- Added `compare_model_behavior()` to separate reviewed model-capacity cards
  from computed same-row prediction disagreement and optional permutation-
  importance evidence across retained families.
- Added `compare_model_effects()` and its base-R plot method for aligned,
  support-aware ALE/PDP comparisons of one named feature across retained
  families. Curves retain their prediction target and are explicitly scoped as
  fitted-function descriptions rather than causal effects.
- Added `render_model_report()`, a standalone beginner-first report that leads
  with the modeling question, held-out baseline comparison, and metric
  definitions before revealing feature patterns and a collapsed evidence audit.
- Added evaluation-row predictions, regression error diagnostics,
  classification confusion matrices, Brier scores, held-out probability
  calibration groups, and actionable warnings for sample-size and
  baseline-performance failure modes.
- Added `calibration_diagnostics()` and an adjacent report explanation that
  compares grouped probabilities with observed frequencies while making its
  sample- and grouping-dependence explicit.
- Added `threshold_diagnostics()` and a default binary-report view showing how
  sensitivity, specificity, precision, accuracy, false positives, and false
  negatives change when the 0.5 decision convention moves. The report never
  presents a cutoff optimized on its held-out rows as validated.
- Added opt-in `subgroup_performance()` and `subgroup =` report support for
  comparing held-out metrics across one explicitly chosen column. Small groups
  remain visible and flagged, and the output rejects fairness-certification
  language.
- Added `missingness_shift()` using pre-imputation, per-predictor rates retained
  in preprocessing provenance. Guided results flag practical training-versus-
  evaluation differences without presenting them as a statistical test or a
  general drift check.
- The guided workflow now rejects unsupported or non-finite inputs, removes and
  records constant predictors, and reapplies its fitted preprocessing recipe to
  raw evaluation data supplied later.
- Added `model_set = "comparison"`, a dependency-light candidate set with the
  pre-specified statistical model, two decision-tree complexities, and the
  simple baseline. Held-out candidate ranks remain descriptive rather than
  silently replacing the primary model.
- Added candidate-set-relative Pareto analysis through `model_tradeoffs()` and
  rebuilt `plot_model_comparison()` around explicit performance-versus-size
  trade-offs rather than a subjective composite score.
- Guided reports now include a plain-language multi-model trade-off section and
  dependency-free Pareto SVG whenever more than two candidates were fitted.
- Added `prediction_ambiguity()` and a comparison-report section that finds
  held-out rows where supplied non-baseline candidates predict different values,
  classes, or probability distributions. Candidate performance remains beside
  disagreement so weak-model variation is not mislabeled as uncertainty.
- Numerical fit warnings are captured in model diagnostics and translated into
  an actionable report warning instead of being lost or printed without context.
- Repositioned the model-agnostic explanation evidence audit as an advanced
  reliability layer behind the beginner workflow.
- Added `explain_model()` with a validated prediction contract for regression,
  binary classification, and multiclass classification. Explicit tasks now
  reject incompatible outcomes; multi-column probabilities must identify their
  class semantics rather than relying on column position.
- Added `audit_explanations()` to combine repeated-importance stability,
  feature dependence, near-optimal-model importance ranges, prediction
  agreement, prioritized findings, and explicit permitted-claim labels.
- Added `render_explanation_report()`, a standalone accessible HTML artifact
  with no runtime dependency on H2O, Plotly, Flexdashboard, or an LLM.

## Statistical methods

- Rebuilt permutation importance to retain repeat-level values, sign stability,
  grouped-feature permutations, optional blocked permutations, and Monte Carlo
  intervals whose interpretation is stated explicitly.
- Added accumulated local effects (ALE) as the default feature-effect method.
- Rebuilt PDP estimation with relative support and predictor-dependence
  diagnostics.
- Added supplied-model Rashomon diagnostics: empirical near-optimal membership,
  feature-importance ranges, rank agreement, and prediction ambiguity.
- Numeric two-level outcomes are now detected as binary classification instead
  of silently becoming regression targets.

## Architecture and reliability

- Replaced the hard-coded three-column tuning dispatcher with a learner-family
  registry and generic list-column search plan. `learner_catalog()` now exposes
  the distinction between a model family, its backend, supported tasks, and a
  reviewed plain-language behavior card.
- Tuning records both requested and fold-effective hyperparameters. Seeds are
  derived from the effective configuration, so adding or reordering a learner
  cannot silently change another initialization and configurations collapsed by
  fold-specific bounds truly fit identically.
- Supplying new rows to `as_explainers()` now always reapplies the fitted
  outcome-level and schema contract, even when predictor preprocessing was
  disabled; reversed factor levels cannot silently invert binary probability
  semantics.
- Balanced accuracy and macro recall are now unavailable when the evaluation
  rows omit a trained class, rather than averaging only over classes that happen
  to be present.
- Held-out and fold-validation rows are never moved into training to repair
  categorical levels. Training recipes instead learn a modal fallback for
  novel predictor levels, record mapping counts, and retain a strict
  `novel_level_strategy = "error"` option.
- H2O AutoML is now an optional fitting adapter; core audits use a lightweight
  runtime dependency set.
- The H2O adapter now creates its automatic outer holdout before preprocessing,
  learns its recipe on training rows only, and never reuses that holdout as an
  AutoML validation frame. `nfolds = 0` is rejected unless an explicit
  validation data set is supplied, preventing ranking by training error alone.
- H2O's engine leaderboard is now retained separately from an engine-neutral
  outer-evaluation leaderboard. The model selected by H2O cross-validation, or
  by an explicitly supplied validation frame when `nfolds = 0`, remains primary;
  common-row ranks are descriptive and include an intercept-only baseline.
- Explicit evaluation data remain outside H2O validation by default. Exact
  duplicate-valued records across supplied training and evaluation data trigger
  a configurable possible-leakage diagnostic rather than being treated as proof
  that the observational units overlap.
- H2O provenance now distinguishes time-limited best-effort seeded searches
  from the more reproducible fixed-model-budget mode and records the Deep
  Learning caveat.
- Live H2O smoke tests now cover regression, binary, and multiclass contracts.
  They also guard the probability-column normalization that keeps hard labels
  from coercing valid multiclass probabilities to character data.
- Preprocessing is conservative, returns a reusable recipe, and applies fitted
  levels/imputation values to evaluation data. Identifier removal and guessed
  ordinal conversion are opt-in.
- Added the engine-neutral `preprocess_data()` name; `preprocess_for_h2o()` is
  retained as a compatibility alias.
- Replaced the generated 2,300-line Flexdashboard implementation with the same
  evidence-report pipeline used by model-agnostic callers.
- Replaced fixed, ranking-first LLM prompts with local deterministic reports and
  explicit aggregated-data-only Gemini, Groq, Cloudflare Workers AI, Ollama,
  OpenRouter, and custom OpenAI-compatible adapters. Local deterministic
  reporting is always the default, even when an API key is present, and
  provider/model/fallback details are attached as provenance.
- Added a dated provider decision guide covering current free access, privacy,
  reproducibility, setup, and structured-output capabilities. The Groq default
  now uses schema-capable `openai/gpt-oss-120b`.
- Schema-capable narrative providers now return a validated five-field object
  that AutoXplainR renders locally with fixed causal, fairness, safety, and
  external-validation boundaries. Invalid JSON falls back transparently and
  structured-output use is retained in provenance.
- Narrative list fields and total length are bounded, and non-loopback custom
  endpoints must use HTTPS so credentials are not sent over plaintext networks.
- Migrated Gemini to its current Interactions API with `store = false` and its
  provider-recommended default temperature. OpenRouter schema requests require
  a compatible routed provider; unsupported adapters remain explicitly
  unstructured.
- Added complete, reproducible local and live Gemini output snapshots. A live
  Gemini check exposed reasoning-token exhaustion at the previous response
  budget, so the default is now 4,000 tokens and incomplete interactions return
  targeted recovery guidance.
- Removed bundled third-party datasets whose provenance and redistribution
  terms were not documented.

## Package engineering

- Added CRAN-oriented metadata, automated checks, coverage, pkgdown deployment,
  issue forms, contribution and security policies, citation metadata, and a
  release checklist.
- Enforced an 80% statement-coverage floor for the ordinary package suite;
  Plotly/native-engine and live H2O paths run in separate integration jobs.
- Added an optional-engine matrix covering every advertised supported task for
  glmnet, mgcv, ranger, XGBoost, e1071, kknn, and earth. Every case exercises
  fitting, permutation importance, ALE, explanation audit, and aligned
  cross-model effects.
- Split fast model-agnostic unit tests from opt-in H2O integration tests and
  made version-tag releases wait for native, coverage, lint, spelling, H2O,
  source-package, and permanent-link gates.
- Responsive browser QA now guards long diagnostic codes from widening the
  standalone report beyond a mobile viewport while retaining table-local
  horizontal scrolling.
- Rebuilt examples and the vignette around held-out evaluation and explicit
  interpretation boundaries.

# AutoXplainR 0.1.2

- Added a min-max weighted model score.
- Added `performance_weight` to the legacy dashboard.

# AutoXplainR 0.1.1

- Expanded the legacy dashboard and Gemini integration.

# AutoXplainR 0.1.0

- Initial experimental H2O AutoML release.
