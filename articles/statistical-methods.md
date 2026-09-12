# Statistical methods and interpretation

## What is being estimated?

AutoXplainR fits supervised prediction models for tabular data. It
evaluates predictions against observed outcomes and describes the fitted
prediction function. It does not estimate causal effects or identify an
intervention.

The default selects a configuration using training-only
cross-validation, learns preprocessing again within each fold, refits on
all outer training rows, and evaluates on the outer holdout. This
separation addresses selection bias [Cawley and Talbot,
2010](https://jmlr.org/papers/v11/cawley10a.html). Repeatedly revising a
workflow after viewing the holdout can still overfit it.

## Validation depends on the sampling unit

Random row splits assume rows are suitable independent sampling units.
`validation_split(group = "id")` keeps whole groups out of training and,
in tuned mode, out of each inner analysis fold. This targets performance
on new groups. Group allocation balances row counts in tuning; it does
not guarantee class stratification. Classification folds without all
outcome classes fail explicitly. Group identity is not included as a
predictor.

`validation_split(time = "date", gap = 2)` trains on earlier times and
holds out the latest times, excluding two distinct time values before
the test period. Ties stay together. The gap is not necessarily two days
or two observations. Time order does not by itself prevent leakage in
lagged features or delayed labels. Temporal tuning requires a
rolling-origin design and is not implemented. The [rsample resampling
guide](https://rsample.tidymodels.org/articles/Common_Patterns.html)
describes why these designs answer different prediction questions.

## Performance and the baseline

Regression reports RMSE, MAE and evaluation R-squared. R-squared
compares the model against the evaluation-set mean; it can be negative.
The fitted baseline uses the training-set mean, so beating that baseline
and having positive R-squared are different statements.

Binary classification reports log loss, Brier score, accuracy, balanced
accuracy and ROC AUC. Guided-workflow probabilities refer to the second
training outcome level;
[`evaluate_models()`](https://matt17br.github.io/autoXplainR/reference/evaluate_models.md)
also accepts an explicit `positive` event. Accuracy uses a 0.5
threshold. Undefined AUC or balanced accuracy in a one-class test sample
is returned as missing, not as a favorable value. Multiclass Brier score
is the mean sum of squared class-probability errors (range 0 to 2);
binary Brier score uses only the positive probability (range 0 to 1).

The one-standard-error tuning rule is a heuristic preference among
competitive configurations, not a significance test. Its family priority
and within-family flexibility rules are inspectable through
[`learner_catalog()`](https://matt17br.github.io/autoXplainR/reference/learner_catalog.md)
and the tuning result. Fold scores are dependent because analysis
partitions overlap; their standard error is not an independent-test
confidence interval.

## Three different sources of uncertainty

1.  **Permutation randomness.** Repeated shuffles vary even for the same
    fitted model and evaluation rows. Importance intervals are t
    intervals for the mean loss change over those shuffles. More repeats
    reduce this Monte Carlo error.
2.  **Evaluation sampling.** Different evaluation observations give
    different scores even with the fitted model held fixed.
    [`performance_uncertainty()`](https://matt17br.github.io/autoXplainR/reference/performance_uncertainty.md)
    approximates this using a paired bootstrap of the evaluation units.
3.  **Fitting and selection.** Training on another sample or choosing
    another configuration may change predictions and explanations.
    Neither of the first two intervals includes this variability. It
    requires refitting or a suitable outer evaluation design.

``` r

result <- autoxplain(mtcars, "mpg", explain = FALSE)
uncertainty <- performance_uncertainty(result, n_boot = 100, seed = 2026)
uncertainty$estimates
#>     quantity  estimate      lower     upper
#> 1    primary  2.455942  0.6928441  3.609810
#> 2   baseline  6.621187  3.7056483  8.377275
#> 3 difference -4.165245 -6.0242302 -0.824004
```

The example uses 100 draws for speed; use at least 1000 for analysis. It
samples identical row indices for the primary model and baseline,
calculates each loss, and subtracts baseline loss from primary loss.
Negative differences favor the primary model. Interval endpoints are
empirical quantiles (R’s type 7) at `(1 - confidence) / 2` and
`(1 + confidence) / 2`.

Grouped designs sample whole evaluation groups with replacement and
retain all rows within each selected group. Losses remain
observation-weighted. The assumption is independent representative
groups, not independent rows within a group. Few groups or near-constant
losses can produce unreliable or degenerate intervals. Temporal designs
reject this IID method. These are approximate percentile intervals,
conditional on the fits; they are not simultaneous, selection-adjusted
or a remedy for an unrepresentative test set. See [Davison and Hinkley,
1997](https://doi.org/10.1017/CBO9780511802843).

## Importance and fitted effects

Permutation importance is the change in evaluation loss when a feature
or feature group is shuffled. Negative importance is possible.
Correlated inputs can substitute for each other or make shuffled
combinations implausible. Grouping features preserves their relationship
during joint shuffling, while blocking within strata only permits
shuffles inside those strata. Neither is a general conditional
importance estimator.

Fisher, Rudin and Dominici’s [model reliance
paper](https://jmlr.org/papers/v20/18-760.html) provides motivation for
examining more than one competitive model. AutoXplainR compares only
supplied fitted candidates; it does not implement their full model-class
reliance bounds. Diagnostics report shuffle behavior, association
screening and supplied-model comparison separately; there is no overall
quality grade.

Partial dependence averages predictions after setting one feature to a
grid value for every reference row. With dependent inputs, this can
require extrapolation. Accumulated local effects instead average local
prediction differences within empirical quantile bins, accumulate them
at bin boundaries, and center their linearly interpolated values over
the reference rows, following [Apley and Zhu,
2020](https://doi.org/10.1111/rssb.12377). Tied quantiles reduce the
number of bins. Boundary counts refer to the interval ending at that
boundary; the first boundary has no separate bin count. Sparse bins,
model instability and dependence still require interpretation. PDP bands
describe variation across reference predictions under that calculation;
they are not refit uncertainty bands. Neither curve is causal.
Association screening uses limited pairwise summaries; a low value does
not exclude nonlinear or joint dependence.

The default workflow screens all features for up to five models, audits
the union of their eight highest-ranked inputs, and computes up to eight
fitted curves per model and outcome class. This is data-dependent
selection. The resulting intervals and diagnostics should be read
descriptively; selection-adjusted inference is not supplied. Multiclass
reports offer a class selector for probability curves.
`effects_by_class` retains each class; `effects_by_model` retains the
first class for compatibility. Lower-level calls select a class with
`explain_effect(..., class = "name")` or
[`compare_model_effects()`](https://matt17br.github.io/autoXplainR/reference/compare_model_effects.md).

## Data exploration describes supplied observations

The Data tab compares the retained raw partitions with the values used
by models. Numeric distributions share training-derived bins, with
evaluation values outside the training range counted separately.
Categories distinguish missing, pooled training levels and new
evaluation levels. Without supplied training data, the basis is
evaluation data and no training comparison is shown. Missingness
denominators include all rows in the displayed partition and stage;
relationship plots use complete usable pairs and report exclusions.

For numeric or time pairs, the displayed association is signed Spearman
rank correlation. Mixed pairs use the unsigned correlation ratio;
categorical pairs use unsigned Cramer’s V. Constants and insufficient
pairs are unavailable. These are limited descriptive summaries: zero
does not establish independence, and none supplies a p-value or
identifies a causal relationship. Binned target means and binary event
rates summarize observed outcomes, not model predictions.

Unfiltered univariate profiles use all available rows for the selected
stage. Pairwise summaries use a uniform sample of at most 10,000 rows
per partition by default; `report_data_control(max_pair_rows = NULL)`
uses every row. Their counts and associations describe that sample, not
the full population. A small group can be missed. Categorical
associations are unavailable for unique identifiers; rare categories can
still inflate associations and require inspection. Row exports sample
uniformly within partitions after allocating the row budget between
them. Browser filters then describe only the embedded sample, with its
displayed denominator. They do not recompute official model scores.
Outcomes inspected after adaptive filtering remain exploratory uses of
the same evaluation data. Aggregate export avoids individual record
payloads but can reveal rare category labels or small counts; it is not
an anonymization method.

## Scope and reference implementations

[DALEX](https://jmlr.org/papers/v19/18-416.html), iml and ingredients
provide model-agnostic explanation tools.
[modelStudio](https://doi.org/10.21105/joss.01798) and
[modelDown](https://doi.org/10.21105/joss.01444) provide automated
explanation interfaces.
[xplainfi](https://mlr-org.github.io/xplainfi/articles/inference.html)
addresses formal importance inference with explicit estimation targets.
AutoXplainR combines established methods into a guided analysis; it does
not claim to replace those packages or provide a superior estimator.

The repository’s `validation/` scripts record numerical checks and
environment versions. Unit tests independently calculate permutation
draws, paired bootstrap draws, analytic additive effects and tied-score
AUC. Those checks establish implementation agreement for the tested
cases, not universal statistical validity, coverage or comparative
superiority.
