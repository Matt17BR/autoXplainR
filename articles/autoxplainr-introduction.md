# Your first prediction report

AutoXplainR compares tabular prediction models, evaluates them against a
simple baseline, and retains fitted explanations and an optional HTML
report. The ordinary workflow runs locally. This example predicts a
parcel’s delivery time using information available at dispatch. All data
below are synthetic.

## Choose what can be known when predicting

``` r

set.seed(2026)
parcels <- data.frame(
  parcel_id = sprintf("parcel-%03d", 1:240),
  dispatched = as.Date("2025-01-01") + 0:239,
  distance_km = runif(240, 20, 600),
  service = rep(c("standard", "express"), 120)
)
parcels$delivery_hours <- 12 + parcels$distance_km / 25 -
  5 * (parcels$service == "express") + rnorm(240, sd = 2)
parcels$delivered <- parcels$dispatched + parcels$delivery_hours / 24
parcels$distance_km[c(8, 40, 90, 211)] <- NA_real_

# Exclude the identifier and the delivery timestamp, which is only known later.
inputs <- parcels[c("dispatched", "distance_km", "service", "delivery_hours")]
```

The package cannot decide whether a column leaks future information.
Choose predictors from the intended prediction task before looking at
model scores. Identifiers also require a decision: an arbitrary parcel
ID should not predict delivery time. Repeated customers or sites would
need a grouped validation plan.

## Reserve later observations and inspect preprocessing

``` r

result <- autoxplain(
  inputs, "delivery_hours", model_set = "comparison",
  validation = validation_split(time = "dispatched"),
  preprocessing_config = list(novel_level_strategy = "error"),
  seed = 2026
)
result
#> <AutoXplainR result>
#>   question:   predict `delivery_hours` (regression)
#>   primary:    linear regression [main_model]
#>   engine:     base
#>   data:       192 training + 48 test rows
#>   design:     temporal
#>   selection:  Pre-specified model; candidate evaluation ranks did not select it.
#>   models:     4 (retained comparison set)
#>   score:      rmse = 2.0031 on test rows
#>   baseline:   74.3% improvement in rmse
#>   caution:    Only 48 rows were available for test scoring.
#>   next:       Treat the scores as preliminary and validate on more representative rows.
#>   finding:    The pairwise association screen does not assess every form of dependence.
#>   inspect:    Review nonlinear relationships and joint support before interpreting shuffled inputs or marginal effects.
#>   evidence:   8 model-feature shuffle summaries; 2 fitted effects
#>   inspect:    render_model_report(result, "report.html"), evidence_summary(result)
#>   predict:    predict(result, newdata) uses the saved training recipe
#>   compare:    compare_model_behavior(result) examines the retained models

recipe <- result$preprocessing_metadata$training_data$recipe
recipe$final_columns
#> [1] "distance_km"    "service"        "delivery_hours"
recipe$imputations
#> $distance_km
#> [1] 272.0664
#> 
#> $service
#> [1] "express"
recipe$factor_levels
#> $service
#> [1] "express"  "standard"
recipe$novel_level_strategy
#> [1] "error"
```

This fits a linear regression, two trees and an intercept-only baseline
on the earlier rows. The latest 20% of distinct dispatch dates are
reserved for evaluation. We explicitly use comparison mode because
chronological tuning is not yet implemented. For independently sampled
rows, the ordinary default compares linear, tree and neural models using
training-only cross-validation. `dispatched` determines the split and is
excluded from predictors. Missing predictor values are imputed using
values learned on training rows; character predictors are converted to
factors using their training levels.

The default novel-category strategy is `"mode"`, which maps an unseen
category to the most frequent training category. Here we deliberately
choose `"error"` so that a new service cannot be silently treated as an
existing one.

## Read the result before explaining it

``` r

result$leaderboard
#>   rank        model_id                   model      role   family backend
#> 1    1      main_model       linear regression   primary   linear   stats
#> 2    2   flexible_tree  flexible decision tree candidate     tree   rpart
#> 3    3      small_tree     small decision tree candidate     tree   rpart
#> 4    4 simple_baseline intercept-only baseline  baseline baseline   stats
#>       rmse      mae     r_squared training_time_ms model_size_kb complexity
#> 1 2.003144 1.662172  9.339624e-01                1      67.79688          3
#> 2 2.241207 1.837860  9.173332e-01                2      52.28125         20
#> 3 4.319412 3.508486  6.929447e-01                4      47.83594          4
#> 4 7.795201 6.683058 -5.052385e-05                3      56.07812          1
#>   fit_warning prediction_time_ms
#> 1                              1
#> 2                              1
#> 3                              2
#> 4                              0
result$evaluation$metric_definitions
#>                                                                                                            rmse 
#>                        "Typical prediction error, with larger mistakes weighted more heavily; lower is better." 
#>                                                                                                             mae 
#>                                     "Average absolute prediction error in the target's units; lower is better." 
#>                                                                                                       r_squared 
#> "Share of evaluation-set variation explained relative to predicting the evaluation-set mean; higher is better."
result$evaluation$notes
#>   severity                 code                                       message
#> 1  caution small_evaluation_set Only 48 rows were available for test scoring.
#>                                                              recommendation
#> 1 Treat the scores as preliminary and validate on more representative rows.
```

RMSE summarizes prediction error in hours, giving larger errors more
weight. The baseline predicts the training mean without using distance
or service. Beating it is useful context, but does not establish that
the error is acceptable for a delivery promise. The scores describe
these reserved rows; transfer to a future population needs further
evidence.

``` r

result$explanations$audit$importance
#>             model     feature importance  std_error conf_low conf_high
#> 1      main_model distance_km   8.230674 0.19630872 7.819795  8.641553
#> 2      main_model     service   2.090297 0.05157578 1.982348  2.198246
#> 3 simple_baseline distance_km   0.000000 0.00000000 0.000000  0.000000
#> 4 simple_baseline     service   0.000000 0.00000000 0.000000  0.000000
#> 5      small_tree distance_km   5.624624 0.16915766 5.270573  5.978675
#> 6      small_tree     service   0.000000 0.00000000 0.000000  0.000000
#> 7   flexible_tree distance_km   8.270003 0.16436878 7.925975  8.614031
#> 8   flexible_tree     service   1.840996 0.03933630 1.758664  1.923328
#>   sign_stability baseline  permuted metric n_repeats max_association
#> 1              1 2.003144 10.233818   rmse        20       0.1499773
#> 2              1 2.003144  4.093441   rmse        20       0.1499773
#> 3              1 7.795201  7.795201   rmse        20       0.1499773
#> 4              1 7.795201  7.795201   rmse        20       0.1499773
#> 5              1 4.319412  9.944036   rmse        20       0.1499773
#> 6              1 4.319412  4.319412   rmse        20       0.1499773
#> 7              1 2.241207 10.511210   rmse        20       0.1499773
#> 8              1 2.241207  4.082203   rmse        20       0.1499773
#>   associated_feature unavailable_reason       shuffle_status dependence_status
#> 1            service                    positive_loss_change    limited_screen
#> 2        distance_km                    positive_loss_change    limited_screen
#> 3            service                      no_observed_change    limited_screen
#> 4        distance_km                      no_observed_change    limited_screen
#> 5            service                    positive_loss_change    limited_screen
#> 6        distance_km                      no_observed_change    limited_screen
#> 7            service                    positive_loss_change    limited_screen
#> 8        distance_km                    positive_loss_change    limited_screen
#>                                                                              claim
#> 1   Shuffling increased loss; the fixed-sample Monte Carlo interval excludes zero.
#> 2   Shuffling increased loss; the fixed-sample Monte Carlo interval excludes zero.
#> 3 No loss change in these shuffles; this is not proof of no population importance.
#> 4 No loss change in these shuffles; this is not proof of no population importance.
#> 5   Shuffling increased loss; the fixed-sample Monte Carlo interval excludes zero.
#> 6 No loss change in these shuffles; this is not proof of no population importance.
#> 7   Shuffling increased loss; the fixed-sample Monte Carlo interval excludes zero.
#> 8   Shuffling increased loss; the fixed-sample Monte Carlo interval excludes zero.
result$explanations$audit$findings
#>   severity                     code
#> 1     note association_screen_scope
#>                                                                     message
#> 1 The pairwise association screen does not assess every form of dependence.
#>                                                                                                                                                                                                                                                                                                                                               evidence
#> 1 Limited pairwise screen: absolute Spearman correlation for numeric pairs, correlation ratio for mixed pairs, and Cramer's V for categorical pairs. Small values do not establish independence or exclude nonlinear or joint dependence. Categorical pairs without repeated categories are unavailable; many rare categories can inflate association.
#>                                                                                              recommendation
#> 1 Review nonlinear relationships and joint support before interpreting shuffled inputs or marginal effects.
#>   model feature                            scope     entities
#> 1  <NA>    <NA> Limits of the association screen c("main_....
result$explanations$failures
#> [1] feature reason 
#> <0 rows> (or 0-length row.names)
```

Permutation importance describes how the fitted model’s evaluation loss
changes when an input is shuffled. Its intervals describe shuffle Monte
Carlo error, not uncertainty about a population. ALE and PDP summarize
fitted associations, not the effect of changing a real parcel’s service.
A check that was not run or could not be computed remains visible with
its reason.

## Inspect the supplied data

``` r

context <- result$data_context
vapply(context$raw[c("training", "evaluation")], function(rows) {
  sum(is.na(rows$distance_km))
}, integer(1))
#>   training evaluation 
#>          3          1
sum(is.na(result$training_data$distance_km))
#> [1] 0
sum(is.na(result$evaluation_data$distance_km))
#> [1] 0
```

There are three missing distances in training and one in evaluation;
after the saved recipe is applied, neither partition has missing
distances. The result retains both versions and their source positions.

In the report’s **Explore data** tab, select `distance_km`.
**Distribution** compares the training and evaluation rows. Switch
**Values** from **Raw supplied values** to **Values used by models** to
see the imputation. In **Relationships**, compare distance with
`delivery_hours`: the binned pattern and signed Spearman association
describe the observed rows, not the effect of changing a parcel’s route.
The training and evaluation summaries use common bins.

## Predict new parcels and share the report

``` r

new_parcels <- data.frame(distance_km = c(100, NA), service = c("express", "standard"))
predict(result, new_parcels)
#> [1] 11.32591 22.96807

unknown_service <- data.frame(distance_km = 100, service = "cargo")
tryCatch(predict(result, unknown_service), error = function(error) conditionMessage(error))
#> [1] "Column `service` has unseen levels: cargo"
```

Prediction applies the saved training recipe and preserves input row
order. An outcome column is unnecessary. The unknown service produces an
explicit error under our chosen strategy; decide how that category
should be represented before refitting or changing the recipe.

``` r

path <- tempfile(fileext = ".html")
memo <- generate_natural_language_report(result)
render_model_report(result, path, narrative = memo)
file.exists(path)
#> [1] TRUE
unlink(path)
```

Use a persistent path such as `"delivery-report.html"` to keep the
report. It opens offline in a browser. Start in **Compare models** to
read the scores and costs, then use **Feature effects** for the selected
model’s important inputs and fitted curves. **Predictions** shows error
summaries and the matching R command. Help buttons work on hover,
keyboard focus and tap. **Print this view** exports the active tab and
selections; expand details you want included.

The default report includes aggregates. To inspect individual records,
choose row export explicitly:

``` r

path <- tempfile(fileext = ".html")
render_model_report(result, path, report_data = "rows")
file.exists(path)
#> [1] TRUE
unlink(path)
```

In **Explore data → Records**, keep **Raw supplied values**, set the
filter’s **Column** to `distance_km` and **Condition** to **Is
missing**, then click **Add filter**. Four records match in this
example. Select one to compare its original distance with the imputed
value and identify its source position. Filters use only the exported
sample, which is capped at 5,000 records by default; this example
exports all 240. They do not change model scores.

For a smaller export with a deliberately included context column:

``` r

render_model_report(result, "delivery-report.html", report_data = report_data_control(
  "rows", columns = c("distance_km", "service"),
  context_columns = "dispatched", max_rows = 100, seed = 2026
))
```

The target is included automatically. Anyone receiving the file receives
all embedded records, including rows hidden by filters.
`report_data = "none"` omits the data explorer and individual
predictions. These options do not redact feature names, fitted-model
details or explanation results elsewhere in the report. Review the
export before sharing; aggregates can also reveal small groups.
`saveRDS(result, "analysis.rds")` retains models **and
training/evaluation data**.

For classification, set outcome factor levels deliberately: binary
probabilities refer to the **second** level. For example,
`factor(outcome, levels = c("no", "yes"))` makes predictions
probabilities of `"yes"`. Numeric outcomes with exactly two values
default to classification; set `task = "regression"` for a numeric
target.

Continue with [model
selection](https://matt17br.github.io/autoXplainR/articles/model-selection.md),
[validation and
diagnostics](https://matt17br.github.io/autoXplainR/articles/validation-and-diagnostics.md),
[existing models and
narratives](https://matt17br.github.io/autoXplainR/articles/existing-models-and-narratives.md),
or the [statistical
methods](https://matt17br.github.io/autoXplainR/articles/statistical-methods.md).
