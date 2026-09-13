# Demanding tabular modeling

This protocol was written before the first 0.8 development fit. Its purpose is
to test whether a short `autoxplain()` call can fit useful forests and boosted
trees on substantial tabular datasets. It does not test Kaggle leaderboard
parity, geographic transfer, or arbitrary data cleaning.

## Data and isolation

Raw data, row partitions, fitted models and complete predictions stay under
`~/.cache/autoxplain-tabular-0.8.0`. The repository contains scripts, compact
measurements and source attribution. Downloaded bytes and prepared partitions
receive SHA-256 hashes. Existing files are reused only after verification.

| Case | Complete source | Development | Locked acceptance | Scope |
| --- | --- | --- | --- | --- |
| YearPredictionMSD | 515,345 songs; 90 audio features | 50,000 training and 20,000 evaluation rows, drawn only from official training rows | All 463,715 official training rows and the last 51,630 official test rows | Release-year regression. Respect the donor's artist-separated test boundary. Development folds do not have artist identifiers. |
| Covertype | 581,012 forest cells; 54 inputs; 7 classes | 50,000 training and 20,000 evaluation rows, drawn only from the acceptance training pool | Approximately 80% training and 20% stratified held-out rows | Same-area row discrimination. Nearby cells can be dependent. This is not a new-region estimate. |
| Bank Marketing | All 41,188 rows in `bank-additional-full.csv`; 19 inputs after removing duration | 20,000 training and 5,000 evaluation rows, drawn only from the acceptance training pool | Approximately 80% training and 20% stratified held-out rows | Random contact-row discrimination. Repeated customers cannot be identified, and this is not future-period performance. |

The source's already encoded Covertype inputs remain as supplied. Bank's
literal `unknown` categories remain categories. Call duration is removed
because it is unavailable before a call. No target-driven feature engineering
or cleanup uses evaluation rows. Two earlier controlled cases, Friedman noise
and a rare-event interaction, are retained as development regression checks;
their outcomes are already known and cannot establish unseen-data performance.

Each preparation writes training data, evaluation features and evaluation
targets separately. Native tuning sees training rows only. Acceptance targets
are not read by a fit or development-scoring process. Acceptance scoring
requires an explicit frozen candidate manifest. This is a procedural lock,
not cryptographic access control: the raw public data contain the labels.
If a failed acceptance result motivates a model change, that result becomes
development evidence and must not be described as a fresh untouched test.

## Comparisons fixed before running

The package baseline is the actual published 0.7.0 installation, whose release
archive has SHA-256
`bc6ad22bee49a9a3fee2ba7a092d975372815e07629295b3a012871782ac2532`.
It requests `learners = c("forest", "boosting")`, `tuning_rule = "best"`,
the default ten-configuration budget, and five fixed training-only folds.
No custom hyperparameter grid is supplied. Supplying fold IDs makes the
comparison reproducible; it does not expose any evaluation outcomes.
Development timing excludes explanation/report generation, which is tested
separately and then through a complete one-call acceptance workflow.

Competent, modest native references use the same training rows and no package
learner adapters. A fixed 20% training-only calibration split chooses between:

- XGBoost histogram trees at depth 6 or 10, learning rate 0.05, child weight 1,
  row and column subsampling 0.8, lambda 1, alpha 0, at most 1,500 rounds and
  50-round early stopping. The selected setting and its calibration-selected
  round count are refit on all training rows. Calibration uses RMSE or log loss.
- Ranger probability forests for classification and regression forests for
  regression, 500 trees, `mtry` equal to floor(sqrt(p)) or floor(p / 3),
  sample fraction 0.8, minimum node size 5, and the standard split rule.
  Calibration chooses a setting; the final forest uses all training rows.

References are bounded comparisons, not an exhaustive optimization oracle.
All calibrated settings and losses are retained, including unsuccessful fits.
Metric calculations operate on the raw held-out predictions independently of
the package leaderboard. Classification probabilities retain named classes;
log loss, Brier score, accuracy and per-class recall are checked. Binary cases
also record ROC AUC and average precision. Regression records RMSE, MAE and R².

## Acceptance criteria declared before scores

- Every final fit uses the declared complete training pool. Report row counts,
  CPU threads, package/backend versions, search settings, elapsed time, peak
  process RSS, warnings, failures and saved object sizes.
  A measured successful native ranger fit on each full training pool is
  mandatory. A resource-limit failure can motivate a repair but cannot pass
  the forest-readiness gate.
- The selected package model's acceptance RMSE is at most 1.05 times the best
  completed native reference RMSE. Classification log loss is at most 1.10
  times the best completed reference plus 0.002 absolute tolerance.
- Against a completed published-0.7 comparison, the selected model's primary
  loss must not worsen by more than 2%. Where the old loss exceeds the native
  reference by over 10%, close at least half the excess gap on at least one
  challenging case. A timeout is an operational failure, not an invented loss.
- Complete predictions for every retained model must survive saving and a
  fresh R session within an absolute tolerance of 1e-12. Independent scores
  must agree with the stated primary model and explain any selection/evaluation
  difference. A runnable model is not sufficient if its reported evidence lies.
- Development runs have a 1,200-second process wall limit and a 24-GiB address
  space limit. Each native reference family is a separate bounded process.
  Full acceptance runs allow 7,200 seconds and 24 GiB. One native thread is the
  initial baseline; any four-thread measurements are separate cohorts and
  compared with matched-thread references. Do not call added CPU an algorithmic
  speedup. Interrupted runs retain logs and are never silently replaced.
- Final report acceptance includes a complete one-call run, readable search
  and fitted-parameter evidence, faithful sample/full-data counts, and actual
  offline browser tasks. Large-model inference and report export limitations
  must remain visible even when quality criteria pass.

These empirical thresholds are release gates for this declared suite. They
are not confidence bounds or promises for other datasets. Any change to this
protocol after results exist must be dated, justified, and reported separately.

## Sources and licenses

- [Year Prediction MSD, UCI](https://archive.ics.uci.edu/dataset/203/yearpredictionmsd),
  T. Bertin-Mahieux (2011), DOI 10.24432/C50K61, CC BY 4.0. The donor requires
  the first 463,715 / last 51,630 split to avoid the artist producer effect.
- [Covertype, UCI](https://archive.ics.uci.edu/dataset/31/covertype),
  Jock Blackard (1998), DOI 10.24432/C50K5N, CC BY 4.0. The observations are
  30-by-30-metre cells from four Colorado wilderness areas.
- [Bank Marketing, UCI](https://archive.ics.uci.edu/dataset/222/bank+marketing),
  S. Moro, P. Rita and P. Cortez (2014), DOI 10.24432/C5K306, CC BY 4.0.
  Use the 41,188-row additional-full file, not the 45,211-row older variant.
- [XGBoost training interface](https://xgboost.readthedocs.io/en/latest/r_docs/R-package/docs/reference/xgb.train.html)
  and [ranger reference](https://imbs-hl.github.io/ranger/reference/ranger.html)
  describe the independent native fitting controls used here.
