# Choose validation and inspect diagnostics

Choose the sampling unit before interpreting scores. A random row
holdout assumes rows can be sampled independently. Repeated people,
households or sites usually need whole-group partitions; observations
over time need a chronological boundary and features available at
prediction time.

``` r

result <- autoxplain(measurements, "outcome",
                     validation = validation_split(group = "site_id"))
result <- autoxplain(observations, "outcome", model_set = "comparison",
                     validation = validation_split(time = "recorded_at", gap = 2))
```

Split columns are excluded from predictors. Fractions refer to whole
groups or distinct times, so row fractions can differ. A temporal gap of
two excludes two distinct time values, not necessarily two days. Tied
times stay together. Grouped tuning keeps units intact and fails if
classification folds cannot contain every outcome class. Chronological
tuning is unsupported.

For an external evaluation set, supply `test_data`. The default role is
`"evaluation"`; use `evaluation_role = "test"` only when the study
design supports that role. Duplicate-valued rows across training and
evaluation produce a possible leakage warning. The role label and
duplicate check cannot establish independence.

## Separate different uncertainties

``` r

result <- autoxplain(mtcars, "mpg", explain = FALSE)
intervals <- performance_uncertainty(result, n_boot = 100, seed = 2026)
intervals$estimates
#>     quantity  estimate      lower     upper
#> 1    primary  2.455942  0.6928441  3.609810
#> 2   baseline  6.621187  3.7056483  8.377275
#> 3 difference -4.165245 -6.0242302 -0.824004
```

This small teaching example uses 100 bootstrap draws; use at least 1000
for an analysis. The paired bootstrap measures evaluation-sample
variation conditional on the fitted primary model and baseline. Grouped
designs resample whole groups. It does not include uncertainty from
fitting or model selection, and temporal designs reject the IID
bootstrap. See [statistical
methods](https://matt17br.github.io/autoXplainR/articles/statistical-methods.md).

## Probabilities, thresholds and groups

``` r

cars <- transform(mtcars, am = factor(am, labels = c("automatic", "manual")))
fit <- autoxplain(cars, "am")
calibration_diagnostics(fit)
#> <AutoXplainR probability calibration>
#>   model:      main_model
#>   check:      probability of the positive class `manual`
#>   rows:       7 in 1 probability groups
#>   average:    0.423 predicted vs 0.429 observed
#>   binned gap: 0.006 (lower is better)
#>   caution:    sample- and grouping-dependent; not a population guarantee
threshold_diagnostics(fit)
#> <AutoXplainR decision-threshold check>
#>   model:       main_model
#>   positive:    manual
#>   rows:        7 (test)
#>   thresholds:  17 checked
#>   lowest cost: 0 at 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90 (descriptive only)
#>   caution:     validate a chosen threshold on different data
```

Binary probabilities refer to the second training factor level, here
`"manual"`. Calibration compares predicted probabilities with observed
frequencies. The binned calibration gap depends on the sample and chosen
grouping. Threshold sensitivity displays false-positive/false-negative
trade-offs; choosing a threshold after viewing final evaluation labels
would reuse those labels for selection.

``` r

regression <- autoxplain(cars, "mpg")
subgroup_performance(regression, "am")
#> <AutoXplainR subgroup performance>
#>   model:       main_model
#>   compared by: am (2 groups)
#>   metric:      rmse (lower is better)
#>   largest gap: 0.0763
#>   caution:     descriptive holdout check; not fairness certification
```

Subgroup scores describe an explicitly selected column. They are not a
fairness certification, and small groups may supply insufficient
evidence. The function retains group counts and reasons for unavailable
metrics.

## Missingness changes

``` r

set.seed(23)
training <- data.frame(x = rnorm(100), y = rnorm(100))
evaluation <- data.frame(x = rnorm(40), y = rnorm(40))
evaluation$x[1:12] <- NA_real_
missing_fit <- autoxplain(training, "y", test_data = evaluation, explain = FALSE)
missingness_shift(missing_fit)
#> <AutoXplainR missingness shift>
#>   data:        100 training + 40 evaluation rows
#>   predictors:  1 with any missing values
#>   flagged:     1 model inputs at 5 percentage points
#>   caution:     practical flag; not a statistical test or general drift check
```

Missingness rates are recorded before imputation. A difference is a
descriptive review signal, not a distribution-shift test or proof that
imputation preserved predictive validity. The report and result retain
unavailable checks separately from completed checks.

The report’s **Explore data** tab also shows missingness for every
exported input. Use **Raw supplied values** to see it before imputation
and **Values used by models** to inspect the transformed values.
Relationships and filtered subgroup outcomes describe the supplied
sample. Groups discovered while exploring final evaluation labels are
exploratory; they are not a new unbiased test set.
