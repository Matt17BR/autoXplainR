# Extended-portfolio expert walkthrough, 0.6.0

This review closes the extended-portfolio task in `validation/product-walkthrough.md`.
It used a private installation of the locally checked source archive, not
`pkgload::load_all()`. The initial archive SHA256 was
`34595ac9b3e9a27e98beb099a18bdd0add364a6716a7e47aa4dec99ce4315756`.
Later corrections make that archive an inspection checkpoint, not the final
release artifact. Final installed-archive replay is pending below.

The reviewer read the rendered report, recorded visible answers and inspected
screenshots before calculating native prediction and selection answers. This
was an implementer walkthrough, not a participant study. It found two interface defects
that the four-model browser fixtures had not exposed; the subsequent fresh-process
archive replay also found a saved-model prediction failure.

## Scenario and tasks

The synthetic dispatch data have 360 observations, four inputs and a continuous
delay outcome. Load has a deliberately nonmonotone relationship with delay;
distance, queue length and service also contribute. Data seed: 61041. The public
call used `portfolio = "extended"`, `model_set = "tuned"`, 20 scheduled settings,
three folds, `tuning_rule = "one_se"`, seed 61042 and `explain = FALSE`.

The report retained eleven models across ten families plus the baseline, with
288 training and 72 evaluation rows. It explicitly exported all 360 source
records and limited explanations to four models, two inputs per model and two
permutation repeats. The installed package used R 4.5.2; optional versions were
mgcv 1.9.4, glmnet 5.0, ranger 0.18.0, xgboost 3.2.1.1, e1071 1.7.17, earth 5.3.6
and kknn 1.4.1. Browser inspection used Chrome 152.0.7977.82 and Playwright 1.58.0
at 1440 and 390 CSS pixels.

| Expert task | Report answer and independent verification |
|---|---|
| Explain why the chosen GAM is smaller than the lowest-loss setting. | The selected/final setting has requested basis limit `k = 5`, gamma 1 and shrinkage selection on. Its CV RMSE is 0.649747, versus 0.644231 for `k = 8`. Recomputing squared errors from original OOF predictions gives those scores and the stated one-SE limit 0.683299. Both GAM settings qualify; the report explains the within-family capacity preference. The wider setting is a searched boundary, not proof that extending it improves performance. |
| Inspect the actual optional models. | The GAM formula has three smooths at basis dimension 5, fourteen coefficients and total effective degrees of freedom 7.964503. Native mgcv objects reproduce those values. XGBoost records 200 rounds, learning rate 0.05, depth 3 and row/column fractions 0.8/1; the native booster contains 200 rounds. The forest has 500 trees, four inputs considered per split and minimum node size 5, matching the native ranger object. |
| Compare a fitted pattern without inventing a trend. | The GAM load curve rises to its highest displayed value near 3.17 tonnes, falls to its minimum near 8.97 tonnes and then partly recovers. Its differences across the displayed grid agree with native GAM prediction differences within 9.44e-6, consistent with table rounding. XGBoost shows the same broad nonlinear pattern with its own values and support. The report does not reduce either curve to a monotone description. |
| Diagnose another optional model and inspect the original error. | XGBoost shows RMSE 0.8251, MAE 0.6862 and mean residual -0.06059; direct native predictions give 0.8251476, 0.6861994 and -0.0605942. Its largest-error link opens source row 257: 121.4 km, 2.5 tonnes, queue 6, express service; observed delay 11.899806 versus predicted 9.980374 hours. The linked raw and processed values match the original observation. |
| Use the inspected model outside the report. | The reviewer copied the displayed `predict(result, new_data, model = "...")` call for the GAM, booster and forest. Each call was executed on five original evaluation rows and matched the corresponding native engine prediction exactly. Save/reload preserves the source and fitted objects used by these checks. |
| Recognize the explanation budget. | The forest remains in the comparison and prediction selectors but its Feature effects panel says explanations were not computed and gives the command to raise the model budget. GAM, neural, boosting and MARS effects are available. Missing effects were not represented as zero importance or successful checks. |

The opened GAM fold records show three 192-row training / 96-row validation
partitions, all converged and no omitted validation rows. The selected setting's
fold RMSEs are 0.701605, 0.571415 and 0.669139. The alternative's are 0.691134,
0.561655 and 0.672292. The reader can inspect requested values, effective values,
learned smoothing penalties and the separate full-training refit.

## Defects found and repair acceptance

1. The primary GAM's compact identity displayed `smooth basis limit = , gamma
   = 1, shrinkage selection = on`. Selection and the full settings table retained
   the correct values, but the main comparison and Feature effects names were
   incomplete. The repaired identity reads `smooth basis k = 5`; model details
   show each native smooth's basis dimension and distinguish those values from
   the requested limit and learned effective degrees of freedom.
2. The eleven-model cost plot placed long labels over each other for models
   clustered between roughly 70 and 200 KiB. Its exact table remained usable,
   but the plot itself failed the comparison task at desktop width. The repaired
   dense layout gives names separate vertical positions and connects each name
   to its actual point. Independent inspection at 1440 and 390 pixels found all
   eleven names legible. Dense, clustered and tied-cost fixtures now challenge
   this case rather than inferring it from four well-spaced models.

3. The first final-archive replay failed in a new R process after only
   `library(AutoXplainR)` and `readRDS()`: the tuned-neural adapter called
   `stats::predict()` without loading the nnet S3 method. Same-process
   save/reload checks had already loaded that namespace during fitting and
   therefore missed the failure. The failing archive was
   `c585530574482161b144e586af71f24407c780b5ca1e15d7be2653ca6220df4a`;
   its fresh installation and `final-replay/replay.log` preserve the counterexample.
   Release acceptance requires a fresh-process replay without attaching the
   missing engine by hand.

At 390 pixels the inspected XGBoost effect and support chart remained readable,
with no whole-page overflow. Detailed comparisons of eleven models require
vertical scrolling and advanced tables retain their local scroll regions.
The XGBoost model-details view honestly states that a separate learned-structure
summary is not recorded; its actual training controls and R object are available.
These are observed limits, not claims that every extended-engine combination or
mobile task has been tested.

## Evidence and final replay

Local evidence is under
`/home/mmazzarelli/.cache/autoxplain-overhaul-0.6/extended-journey/`:

- `generate.R`, `install.log`, `generate.log`, `session-info.txt` and
  `source-and-result.rds` identify the installed package, synthetic source and
  untouched fitted models.
- `observed-before-oracles.json` and
  `detail-observations-before-oracles.json` retain visible report answers,
  source links and the three copied R calls. Desktop and phone PNGs record the
  actual charts, model dialogs and error investigation.
- `selection-detail-observations.json` records the opened fold evidence.
- `check-native.R`, `check-native.log` and `independent-native-answers.json`
  contain native mgcv/xgboost/ranger predictions, original-row comparisons,
  independently reconstructed CV arithmetic and copied-code execution.

The repaired current-source preview was visually inspected; replay from the
final installed archive remains pending. The original source, fits, fold records, timing measurements and seeds
must be preserved when rerendering; the initial defects are not accepted merely
because native numerical checks pass.
